use clap::Parser as _;
use std::{io, io::Write as _};

#[derive(clap::Parser)]
struct Args {
    #[command(subcommand)]
    mode: Mode,
}

#[derive(clap::Subcommand)]
enum Mode {
    /// Conversion between km and mi
    Distance,
    /// Conversion between ºC and ºF
    Temperature,
    /// Conversion between 24h and 12h
    Time,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _args = Args::parse();

    let _ = prompt("2 + 2 =")?;
    erase_line();
    let _ = prompt("4 + 4 =")?;
    erase_line();

    Ok(())
}

fn prompt(prompt: &str) -> Result<String, io::Error> {
    print!("{} ", prompt);
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    Ok(input)
}

fn erase_line() {
    print!(
        "{}{}",
        ansi_escapes::CursorPrevLine,
        ansi_escapes::EraseLine
    );
}
