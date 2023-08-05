use clap::Parser as _;
use rand::{distributions::Standard, prelude::*};
use std::{
    fmt::{self, Display},
    io::Write as _,
};
use tokio::io::{self, AsyncBufReadExt as _, AsyncWriteExt as _};

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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _args = Args::parse();

    let _ = prompt("2 + 2 =").await?;
    erase_line();
    let _ = prompt("4 + 4 =").await?;
    erase_line();

    Ok(())
}

async fn prompt(prompt: &str) -> Result<String, io::Error> {
    print!("{} ", prompt);
    tokio::io::stdout().flush().await?;

    let mut input = String::new();
    let mut stdin = io::BufReader::new(io::stdin());
    stdin.read_line(&mut input).await?;

    Ok(input)
}

fn erase_line() {
    print!(
        "{}{}",
        ansi_escapes::CursorPrevLine,
        ansi_escapes::EraseLine
    );
}

enum MeridiemIndicator {
    Am,
    Pm,
}

impl Display for MeridiemIndicator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MeridiemIndicator::Am => "AM",
                MeridiemIndicator::Pm => "PM",
            }
        )
    }
}

impl Distribution<MeridiemIndicator> for Standard {
    fn sample<R>(&self, rng: &mut R) -> MeridiemIndicator
    where
        R: Rng + ?Sized,
    {
        if rng.gen() {
            MeridiemIndicator::Am
        } else {
            MeridiemIndicator::Pm
        }
    }
}

fn convert_km_to_mi(x: f32) -> f32 {
    (x * 0.6213712).round()
}

fn convert_mi_to_km(x: f32) -> f32 {
    (x / 0.6213712).round()
}

fn convert_c_to_f(x: f32) -> f32 {
    ((x * (9.0 / 5.0)) + 32.0).round()
}

fn convert_f_to_c(x: f32) -> f32 {
    ((x - 32.0) * (5.0 / 9.0)).round()
}

fn convert_24h_to_12h(x: u8) -> (u8, MeridiemIndicator) {
    use MeridiemIndicator::*;
    match x {
        0 => (12, Am),
        1..=11 => (x, Am),
        12 => (12, Pm),
        13..=23 => (x - 12, Pm),
        _ => panic!("Invalid 24h time: {}", x),
    }
}

fn convert_12h_to_24h(x: (u8, MeridiemIndicator)) -> u8 {
    use MeridiemIndicator::*;
    match x {
        (12, Am) => 0,
        (1..=11, Am) => x.0,
        (12, Pm) => 12,
        (1..=11, Pm) => x.0 + 12,
        _ => panic!("Invalid 12h time: {} {}", x.0, x.1),
    }
}

fn gen_km() -> f32 {
    thread_rng().gen_range::<u16, _>(1..=800).into()
}

fn gen_mi() -> f32 {
    convert_km_to_mi(gen_km())
}

fn gen_c() -> f32 {
    thread_rng().gen_range::<i8, _>(-20..=50).into()
}

fn gen_f() -> f32 {
    convert_c_to_f(gen_c())
}

fn gen_24h() -> u8 {
    thread_rng().gen_range(0..=23)
}

fn gen_12h() -> (u8, MeridiemIndicator) {
    convert_24h_to_12h(gen_24h())
}
