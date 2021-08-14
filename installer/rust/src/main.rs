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

fn log(message: String) {
    println_colored(ansi_term::Color::Purple, message);
}

fn println_colored(color: ansi_term::Color, message: String) {
    println!("{}", color.paint(message));
}
