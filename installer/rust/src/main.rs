use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "installer")]
struct Options {
    #[structopt(name = "PACKAGE")]
    packages: Vec<String>,

    /// Run in dry run mode
    #[structopt(long)]
    dry_run: bool,
}

fn main() {
    let options = Options::from_args();
    println!("{:#?}", options);
}
