use axum::{Router, routing::get};
use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {
    #[arg(long, env = "PORT", default_value_t = 3000)]
    port: u16,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let app = Router::new().route("/", get(|| async { "Hello, world!" }));
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", args.port))
        .await
        .unwrap();
    println!("Listening at http://127.0.0.1:{}", args.port);
    axum::serve(listener, app).await.unwrap();
}
