use clap::Parser as _;
use detect_indent::{self as di, detect_indent};
use serde::Serialize;
use std::{
    fmt::{self, Display},
    fs,
    io::{self, Read as _},
    path::PathBuf,
};

#[derive(clap::Parser)]
struct Args {
    path: Option<PathBuf>,
    #[clap(long, default_value_t)]
    format: OutputFormat,
}

#[derive(clap::ValueEnum, Clone, Default)]
enum OutputFormat {
    #[default]
    Json,
    Vim,
    Kakoune,
}

impl Display for OutputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json => write!(f, "json"),
            Self::Vim => write!(f, "vim"),
            Self::Kakoune => write!(f, "kakoune"),
        }
    }
}

#[derive(Serialize)]
struct Inference {
    indent: Option<Indent>,
}

#[derive(Serialize)]
struct Indent {
    style: IndentStyle,
    size: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum IndentStyle {
    Tabs,
    Spaces,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let bytes = if let Some(path) = &args.path
        && path != "-"
    {
        fs::read(path)?
    } else {
        let mut buffer = Vec::new();
        io::stdin().read_to_end(&mut buffer)?;
        buffer
    };

    let string = str::from_utf8(&bytes)?;

    let inference = infer_from_string(string);

    match args.format {
        OutputFormat::Json => println!("{}", serde_json::to_string_pretty(&inference)?),
        OutputFormat::Kakoune => print!("{}", to_kakoune(&inference)),
        OutputFormat::Vim => print!("{}", to_vim(&inference)),
    }

    Ok(())
}

fn infer_from_string(string: &str) -> Inference {
    let indent = detect_indent(string);
    Inference {
        indent: indent.kind().map(|kind| Indent {
            style: match kind {
                di::IndentKind::Tab => IndentStyle::Tabs,
                di::IndentKind::Space => IndentStyle::Spaces,
            },
            size: indent.amount(),
        }),
    }
}

// TODO: Make tab widths customizable

fn to_vim(inference: &Inference) -> String {
    let mut string = String::new();
    if let Some(Indent { style, size }) = &inference.indent {
        match style {
            IndentStyle::Tabs => {
                string.push_str("set noexpandtab\n");
                string.push_str("set shiftwidth=0\n");
                string.push_str("set tabstop=4\n");
            }
            IndentStyle::Spaces => {
                string.push_str("set expandtab\n");
                string.push_str(&format!("set shiftwidth={}\n", size));
                string.push_str("set tabstop=8\n");
            }
        }
    }
    string
}

fn to_kakoune(inference: &Inference) -> String {
    let mut string = String::new();
    if let Some(Indent { style, size }) = &inference.indent {
        match style {
            IndentStyle::Tabs => {
                string.push_str("set-option buffer indentwidth 0\n");
                string.push_str("set-option buffer tabstop 4\n");
            }
            IndentStyle::Spaces => {
                string.push_str(&format!("set-option buffer indentwidth {}\n", size));
                string.push_str("set-option buffer tabstop 8\n");
            }
        }
    }
    string
}
