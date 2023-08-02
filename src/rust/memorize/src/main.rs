use clap::Parser as _;
use rand::{distributions::Standard, prelude::*};
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

enum MeridiemIndicator {
    Am,
    Pm,
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
    todo!()
}

fn convert_c_to_f(x: f32) -> f32 {
    todo!()
}

fn convert_24h_to_12h(x: u8) -> (u8, MeridiemIndicator) {
    todo!()
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
