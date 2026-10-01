use axum::{Router, extract::Request, middleware::Next, routing::get};
use clap::Parser as _;
use listenfd::ListenFd;
use std::{future, sync::Arc, time::Duration};
use tokio::sync::Notify;

const IDLE_TIMEOUT: Duration = Duration::from_secs(60);

#[derive(Debug, clap::Parser)]
struct Args {
    /// Port to listen on when not socket-activated
    #[arg(long, env = "PORT", default_value_t = 3000)]
    port: u16,

    /// Exit after a period with no requests
    #[arg(long, env = "ON_DEMAND")]
    on_demand: bool,

    /// Unique ID for this run, set by systemd
    #[arg(long, env = "INVOCATION_ID")]
    invocation_id: Option<String>,
}

#[tokio::main]
async fn main() {
    let args: &'static Args = Box::leak(Box::new(Args::parse()));

    let activity = Arc::new(Notify::new());

    let app = Router::new()
        .route("/", get(|| async { "Hello, world!" }))
        .route("/args", get(move || async move { format!("{args:#?}") }))
        .layer(axum::middleware::from_fn({
            let activity = Arc::clone(&activity);
            move |request: Request, next: Next| {
                activity.notify_one();
                next.run(request)
            }
        }));

    let shutdown = async move {
        if args.on_demand {
            exit_when_idle(&activity).await;
            println!("Idle for {IDLE_TIMEOUT:?}, exiting");
        } else {
            future::pending::<()>().await;
        }
    };

    if let Some(listener) = ListenFd::from_env().take_unix_listener(0).unwrap() {
        listener.set_nonblocking(true).unwrap();
        let listener = tokio::net::UnixListener::from_std(listener).unwrap();
        println!("Listening on Unix socket");
        axum::serve(listener, app)
            .with_graceful_shutdown(shutdown)
            .await
            .unwrap();
    } else {
        let listener = tokio::net::TcpListener::bind(("127.0.0.1", args.port))
            .await
            .unwrap();
        println!("Listening at http://127.0.0.1:{}", args.port);
        axum::serve(listener, app)
            .with_graceful_shutdown(shutdown)
            .await
            .unwrap();
    }
}

// Resolves once `IDLE_TIMEOUT` passes without `activity` being notified.
async fn exit_when_idle(activity: &Notify) {
    while tokio::time::timeout(IDLE_TIMEOUT, activity.notified())
        .await
        .is_ok()
    {}
}
