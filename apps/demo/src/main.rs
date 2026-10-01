use axum::{Router, routing::get};
use clap::Parser as _;

#[derive(Debug, clap::Parser)]
struct Args {
    /// Port to listen on
    #[arg(long, env, default_value_t = 3000)]
    port: u16,

    /// Unique ID for this run set by `systemd`
    #[arg(long, env)]
    invocation_id: Option<String>,
}

#[tokio::main]
async fn main() {
    let args: &'static Args = Box::leak(Box::new(Args::parse()));
    let app = Router::new()
        .route("/", get(|| async { "Hello, world!" }))
        .route("/args", get(move || async move { format!("{args:#?}") }));
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", args.port))
        .await
        .unwrap();
    println!("Listening on http://127.0.0.1:{}", args.port);
    axum::serve(listener, app).await.unwrap();
}
