pub mod api;

use atom_syndication::Feed;
use rusqlite::Connection;

pub async fn scrape(_db: &mut Connection) -> anyhow::Result<()> {
    // TODO: Scrape the RFD API, dump responses into SQLite
    todo!()
}

pub fn syndicate(_db: &mut Connection) -> anyhow::Result<Feed> {
    // TODO: Query SQLite for previously scraped RFDs, build an Atom feed
    todo!()
}
