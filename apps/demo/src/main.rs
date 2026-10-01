use axum::{Router, extract::State, routing::get};
use clap::Parser as _;
use std::path::PathBuf;
use tokio::fs;

#[derive(Debug, clap::Parser)]
struct Args {
    /// Port to listen on
    #[arg(long, env, default_value_t = 3000)]
    port: u16,

    /// Unique ID for this run set by `systemd`
    #[arg(long, env)]
    invocation_id: Option<String>,

    /// Persistent state directory set by `systemd`
    #[arg(long, env, default_value = ".")]
    state_directory: PathBuf,
}

#[tokio::main]
async fn main() {
    let args: &'static Args = Box::leak(Box::new(Args::parse()));
    let app = Router::new()
        .route("/", get(handle_get_root))
        .with_state(args);
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", args.port))
        .await
        .unwrap();
    println!("Listening on http://127.0.0.1:{}", args.port);
    axum::serve(listener, app).await.unwrap();
}

async fn handle_get_root(State(args): State<&'static Args>) -> String {
    // Written by the `tick` job
    let tick = fs::read_to_string(args.state_directory.join("tick"))
        .await
        .unwrap_or_else(|_| String::from("never\n"));
    format!("Hello, world!\n\n{args:#?}\n\nLast tick: {tick}")
}
