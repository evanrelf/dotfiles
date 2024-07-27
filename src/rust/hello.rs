#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! clap = { version = "4.5.11", features = ["derive"] }
//! ```

use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {
    #[clap(default_value_t = String::from("Evan"))]
    name: String,
}

fn main() {
    let args = Args::parse();
    println!("Hello, {}!", args.name);
}
