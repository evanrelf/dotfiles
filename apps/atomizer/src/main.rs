mod db;

use axum::{Router, routing::get};
use clap::Parser as _;
use rusqlite::Connection;
use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(disable_help_subcommand = true)]
struct Args {
    /// Unique ID for this run (set by `systemd`)
    #[arg(long, env)]
    invocation_id: Option<String>,

    /// Directory where to store state (set by `systemd`)
    #[arg(long, env = "STATE_DIRECTORY", value_name = "DIRECTORY")]
    state_dir: Option<PathBuf>,

    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Scrape a supported website
    Scrape { website: Website },

    /// Serve scraped websites as Atom feeds
    Syndicate {
        /// Port to listen on (set by `iris.apps`)
        #[arg(long, env, default_value_t = 3000)]
        port: u16,
    },
}

#[derive(Clone, Copy, clap::ValueEnum)]
enum Website {
    /// <https://rfd.shared.oxide.computer>
    OxideRfds,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let db_path = db::path(args.state_dir.as_ref()).await?;
    let mut db = db::open(&db_path)?;

    match args.command {
        Command::Scrape { website } => run_scrape(&mut db, website).await?,
        Command::Syndicate { port } => run_syndicate(&mut db, port).await?,
    }
    Ok(())
}

async fn run_scrape(_db: &mut Connection, _website: Website) -> anyhow::Result<()> {
    todo!()
}

async fn run_syndicate(_db: &mut Connection, port: u16) -> anyhow::Result<()> {
    let app = Router::new().route("/", get(|| async { "Hello, world!" }));
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", port)).await?;
    println!("Listening on http://127.0.0.1:{port}");
    axum::serve(listener, app).await?;
    Ok(())
}
