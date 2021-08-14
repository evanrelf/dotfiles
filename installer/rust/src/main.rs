#[derive(argh::FromArgs, Debug)]
/// installer
struct Options {
    /// packages
    #[argh(positional)]
    packages: Vec<String>,

    /// run in dry run mode
    #[argh(option)]
    dry_run: bool,
}

fn main() {
    let options: Options = argh::from_env();
    println!("{:#?}", options);
}
