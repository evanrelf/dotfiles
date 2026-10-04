//! Client for Oxide's (undocumented) public RFD API.
//!
//! <https://rfd-api.shared.oxide.computer>

use jiff::Timestamp;
use serde::{Deserialize, de::DeserializeOwned};

const BASE_URL: &str = "https://rfd-api.shared.oxide.computer";
const USER_AGENT: &str = "atomizer (+https://github.com/evanrelf/dotfiles/tree/main/apps/atomizer)";
const FROM: &str = "evan@evanrelf.com";

pub struct Client(reqwest::Client);

impl Client {
    pub fn new() -> Self {
        let mut headers = reqwest::header::HeaderMap::new();
        headers.insert(
            reqwest::header::FROM,
            reqwest::header::HeaderValue::from_static(FROM),
        );
        let client = reqwest::Client::builder()
            .user_agent(USER_AGENT)
            .default_headers(headers)
            .build()
            .expect("Failed to build HTTP client");
        Self(client)
    }

    /// `GET /rfd`
    pub async fn list_rfds(&self) -> anyhow::Result<Vec<Rfd>> {
        self.get("/rfd").await
    }

    /// `GET /rfd/{number}`
    pub async fn get_rfd(&self, number: u16) -> anyhow::Result<Rfd> {
        self.get(&format!("/rfd/{number}")).await
    }

    /// `GET /rfd/{number}/raw`
    pub async fn get_rfd_raw(&self, number: u16) -> anyhow::Result<RfdRaw> {
        self.get(&format!("/rfd/{number}/raw")).await
    }

    /// `GET /rfd/{number}/revision`
    pub async fn list_rfd_revisions(&self, number: u16) -> anyhow::Result<Vec<RfdRevision>> {
        self.get(&format!("/rfd/{number}/revision")).await
    }

    /// `GET /rfd/{number}/revision/{revision_id}/raw`
    pub async fn get_rfd_revision_raw(
        &self,
        number: u16,
        revision_id: &RevisionId,
    ) -> anyhow::Result<RfdRaw> {
        self.get(&format!("/rfd/{number}/revision/{}/raw", revision_id.0))
            .await
    }

    async fn get<T: DeserializeOwned>(&self, path: &str) -> anyhow::Result<T> {
        let url = format!("{BASE_URL}{path}");
        let response = self.0.get(&url).send().await?;
        let status = response.status();
        if !status.is_success() {
            let message = match response.json::<ApiError>().await {
                Ok(error) => format!("{} (request ID {})", error.message, error.request_id),
                Err(_) => String::from("no error details"),
            };
            anyhow::bail!("GET {url} failed with {status}: {message}");
        }
        Ok(response.json().await?)
    }
}

impl Default for Client {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Deserialize)]
pub struct ApiError {
    pub request_id: String,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
#[serde(transparent)]
pub struct RfdId(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
#[serde(transparent)]
pub struct RevisionId(pub String);

#[derive(Debug, Deserialize)]
pub struct Rfd {
    pub id: RfdId,
    #[serde(rename = "rfd_number")]
    pub number: u16,
    /// GitHub tree URL for the RFD's directory.
    pub link: String,
    /// GitHub pull request URL.
    pub discussion: Option<String>,
    pub title: String,
    pub state: State,
    /// Semicolon-separated, like `Name <email>; Name <email>`.
    pub authors: Option<String>,
    /// Comma-separated, like `security, compliance`.
    pub labels: Option<String>,
    pub format: Format,
    /// Git blob hash of the RFD's content.
    pub sha: String,
    /// Git commit of the latest revision.
    pub commit: String,
    pub committed_at: Timestamp,
    pub latest_major_change_at: Timestamp,
    pub visibility: Visibility,
}

impl Rfd {
    pub fn authors(&self) -> impl Iterator<Item = &str> {
        self.authors
            .iter()
            .flat_map(|authors| authors.split(';'))
            .map(str::trim)
            .filter(|item| !item.is_empty())
    }

    pub fn labels(&self) -> impl Iterator<Item = &str> {
        self.labels
            .iter()
            .flat_map(|labels| labels.split(','))
            .map(str::trim)
            .filter(|item| !item.is_empty())
    }
}

#[derive(Debug, Deserialize)]
pub struct RfdRaw {
    #[serde(flatten)]
    pub rfd: Rfd,
    pub content: String,
}

#[derive(Debug, Deserialize)]
pub struct RfdRevision {
    pub id: RevisionId,
    pub commit_sha: String,
    pub committed_at: Timestamp,
    pub major_change: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum State {
    Discussion,
    Published,
    Committed,
    #[serde(untagged)]
    Unknown(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Format {
    Asciidoc,
    #[serde(untagged)]
    Unknown(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Visibility {
    Public,
    #[serde(untagged)]
    Unknown(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore = "requires network"]
    async fn test_live_api() -> anyhow::Result<()> {
        let client = Client::default();

        let rfds = client.list_rfds().await?;
        assert!(rfds.iter().any(|rfd| rfd.number == 1));

        let rfd = client.get_rfd(1).await?;
        assert_eq!(rfd.title, "Requests for Discussion");

        let raw = client.get_rfd_raw(1).await?;
        assert!(raw.content.contains("= RFD 1"));

        let revisions = client.list_rfd_revisions(1).await?;
        let revision = revisions.last().expect("RFD 1 has revisions");
        let revision_raw = client.get_rfd_revision_raw(1, &revision.id).await?;
        assert!(revision_raw.content.contains("= RFD 1"));

        assert!(client.get_rfd(u16::MAX).await.is_err());

        Ok(())
    }
}
