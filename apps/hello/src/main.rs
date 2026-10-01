use axum::{Router, routing::get};
use clap::Parser as _;
use listenfd::ListenFd;

#[derive(clap::Parser)]
struct Args {
    /// Port to listen on when not socket-activated
    #[arg(long, env = "PORT", default_value_t = 3000)]
    port: u16,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let app = Router::new().route("/", get(|| async { "Hello, world!" }));
    if let Some(listener) = ListenFd::from_env().take_unix_listener(0).unwrap() {
        listener.set_nonblocking(true).unwrap();
        let listener = tokio::net::UnixListener::from_std(listener).unwrap();
        println!("Listening on Unix socket");
        axum::serve(listener, app).await.unwrap();
    } else {
        let listener = tokio::net::TcpListener::bind(("127.0.0.1", args.port))
            .await
            .unwrap();
        println!("Listening at http://127.0.0.1:{}", args.port);
        axum::serve(listener, app).await.unwrap();
    }
}
