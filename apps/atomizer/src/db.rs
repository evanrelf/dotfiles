use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
use jiff::Timestamp;
use rusqlite::{Connection, OptionalExtension as _, TransactionBehavior};
use std::path::{Path, PathBuf};
use tokio::fs;

// TODO: Assert that schema version is supported in every query

pub async fn path(state_dir: Option<&PathBuf>) -> anyhow::Result<PathBuf> {
    let state_dir = match state_dir {
        Some(state_dir) => state_dir,
        None => &Xdg::new(AppStrategyArgs {
            top_level_domain: String::from("com"),
            author: String::from("Evan Relf"),
            app_name: String::from("Atomizer"),
        })?
        .state_dir()
        .expect("XDG strategy supports `state_dir`"),
    };
    fs::create_dir_all(&state_dir).await?;
    Ok(state_dir.join("state.db"))
}

pub fn open(path: &Path) -> anyhow::Result<Connection> {
    let mut db = Connection::open(path)?;

    db.execute_batch(
        "
        pragma journal_mode = wal;
        pragma synchronous = normal;
        pragma foreign_keys = on;
        pragma busy_timeout = 5000;
        ",
    )?;

    migrate(&mut db)?;

    Ok(db)
}

fn migrate(db: &mut Connection) -> anyhow::Result<()> {
    // SQLite's 12-step generalized `alter table` procedure:
    // https://www.sqlite.org/lang_altertable.html#otheralter

    const LATEST_VERSION: u16 = 1;

    loop {
        let user_version: u16 = db.query_row("pragma user_version", [], |row| row.get(0))?;

        match user_version {
            0 => migrate_0(db)?,
            LATEST_VERSION => break,
            _ => anyhow::bail!(
                "Database version {user_version} is newer than supported (max: {LATEST_VERSION})"
            ),
        }
    }

    Ok(())
}

/// Initialize database.
fn migrate_0(db: &mut Connection) -> anyhow::Result<()> {
    let tx = db.transaction_with_behavior(TransactionBehavior::Immediate)?;

    let user_version: u16 = tx.query_row("pragma user_version;", [], |row| row.get(0))?;

    // Another process migrated the database while we waited for the write lock.
    if user_version != 0 {
        return Ok(());
    }

    // TODO: Add table(s) representing Oxide RFDs
    tx.execute_batch(
        "
        create table blobs (
            hash blob primary key check (length(hash) = 32),
            content blob not null
        ) strict;

        create table pages (
            id integer primary key,
            url text not null,
            blob_hash blob not null references blobs,
            first_seen_at text not null,
            last_seen_at text not null
        ) strict;
        ",
    )?;

    tx.execute(&format!("pragma user_version = {};", user_version + 1), [])?;

    tx.commit()?;

    Ok(())
}

pub fn get_blob(db: &mut Connection, blob_hash: &blake3::Hash) -> anyhow::Result<Option<Vec<u8>>> {
    let compressed: Option<Vec<u8>> = db
        .query_row(
            "select content from blobs where hash = ?1;",
            (blob_hash.as_bytes(),),
            |row| row.get(0),
        )
        .optional()?;

    let Some(compressed) = compressed else {
        return Ok(None);
    };

    let content = zstd::decode_all(compressed.as_slice())?;

    let actual_hash = blake3::hash(&content);
    anyhow::ensure!(
        actual_hash == *blob_hash,
        "Blob integrity check failed (expected {blob_hash}, got {actual_hash})"
    );

    Ok(Some(content))
}

pub fn insert_blob(db: &mut Connection, content: &[u8]) -> anyhow::Result<blake3::Hash> {
    let hash = blake3::hash(content);

    let compressed = zstd::encode_all(content, zstd::DEFAULT_COMPRESSION_LEVEL)?;

    db.execute(
        "
        insert into blobs (hash, content)
        values (?1, ?2)
        on conflict (hash) do nothing;
        ",
        (hash.as_bytes(), compressed),
    )?;

    Ok(hash)
}

pub struct Page {
    pub id: i64,
    pub url: String,
    pub blob_hash: blake3::Hash,
    pub first_seen_at: Timestamp,
    pub last_seen_at: Timestamp,
}

pub fn get_page(db: &mut Connection, page_id: i64) -> anyhow::Result<Option<Page>> {
    let page = db
        .query_row(
            "
            select id, url, blob_hash, first_seen_at, last_seen_at
            from pages
            where id = ?1;
            ",
            (page_id,),
            |row| {
                Ok(Page {
                    id: row.get(0)?,
                    url: row.get(1)?,
                    blob_hash: blake3::Hash::from_bytes(row.get(2)?),
                    first_seen_at: row.get(3)?,
                    last_seen_at: row.get(4)?,
                })
            },
        )
        .optional()?;

    Ok(page)
}

pub fn insert_page(
    db: &mut Connection,
    url: &str,
    blob_hash: &blake3::Hash,
) -> anyhow::Result<i64> {
    let tx = db.transaction_with_behavior(TransactionBehavior::Immediate)?;

    // Don't trim fractional zeroes, so timestamps sort correctly as text.
    let now = format!("{:.9}", Timestamp::now());

    let latest: Option<(i64, [u8; 32])> = tx
        .query_row(
            "
            select id, blob_hash
            from pages
            where url = ?1
            order by id desc
            limit 1;
            ",
            (url,),
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .optional()?;

    let id = match latest {
        Some((id, latest_hash)) if latest_hash == *blob_hash.as_bytes() => {
            tx.execute(
                "update pages set last_seen_at = ?1 where id = ?2;",
                (&now, id),
            )?;
            id
        }
        _ => {
            tx.execute(
                "
                insert into pages (url, blob_hash, first_seen_at, last_seen_at)
                values (?1, ?2, ?3, ?3);
                ",
                (url, blob_hash.as_bytes(), &now),
            )?;
            tx.last_insert_rowid()
        }
    };

    tx.commit()?;

    Ok(id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pages() -> anyhow::Result<()> {
        let mut db = open(Path::new(":memory:"))?;

        let blob_content = b"<h1>Hello, world!</h1>";
        let blob_hash = insert_blob(&mut db, blob_content)?;
        assert_eq!(get_blob(&mut db, &blob_hash)?.unwrap(), &blob_content[..]);

        let page_url = "https://example.com";
        let page_id = insert_page(&mut db, page_url, &blob_hash)?;
        let page = get_page(&mut db, page_id)?.unwrap();
        assert_eq!(page.url, page_url);
        assert_eq!(page.blob_hash, blob_hash);

        // Same content updates the existing page.
        assert_eq!(insert_page(&mut db, page_url, &blob_hash)?, page_id);
        assert!(get_page(&mut db, page_id)?.unwrap().last_seen_at > page.last_seen_at);

        Ok(())
    }
}
