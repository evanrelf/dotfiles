#![allow(dead_code)] // TODO: Remove

mod db;
mod oxide_rfds;

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
    Scrape {
        #[command(subcommand)]
        command: ScrapeCommand,
    },

    /// Serve scraped websites as Atom feeds
    Syndicate {
        /// Port to listen on (set by `iris.apps`)
        #[arg(long, env, default_value_t = 3000)]
        port: u16,
    },
}

#[derive(clap::Subcommand)]
enum ScrapeCommand {
    /// Oxide Computer Company's Requests for Discussion
    OxideRfds,

    /// An arbitrary URL
    Url { url: String },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let db_path = db::path(args.state_dir.as_ref()).await?;
    let mut db = db::open(&db_path)?;

    match args.command {
        Command::Scrape { command } => match command {
            ScrapeCommand::OxideRfds => oxide_rfds::scrape(&mut db).await?,
            ScrapeCommand::Url { url } => scrape_url(&mut db, &url).await?,
        },
        Command::Syndicate { port } => syndicate(&mut db, port).await?,
    }
    Ok(())
}

async fn scrape_url(_db: &mut Connection, _url: &str) -> anyhow::Result<()> {
    todo!()
}

async fn syndicate(_db: &mut Connection, port: u16) -> anyhow::Result<()> {
    let app = Router::new()
        .route("/", get(|| async { "Hello, world!" }))
        .route(
            "/oxide-rfds",
            get(|| async { "TODO: Oxide RFDs Atom feed here" }),
        )
        .route(
            "/url",
            get(|| async { "TODO: URLs from SQLite `pages` table here" }),
        );

    let listener = tokio::net::TcpListener::bind(("127.0.0.1", port)).await?;

    println!("Listening on http://127.0.0.1:{port}");
    axum::serve(listener, app).await?;

    Ok(())
}
