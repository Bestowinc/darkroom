use argh::FromArgs;
use std::error::Error;
use std::path::PathBuf;

pub mod grpc;
pub mod take;

pub type BoxError = Box<dyn Error>;

/// If verbose is turned on then println! the token tree
#[macro_export]
macro_rules! vprintln {
    ($verbose:expr) => {
        if $verbose == true {
            println!();
        }
    };
    ($verbose:expr, $($arg:tt)*) => {
        if $verbose == true {
            println!($($arg)*);
        }
    }
}

/// Top-level command.
#[derive(FromArgs, PartialEq, Debug)]
pub struct Command {
    /// enable verbose output
    #[argh(switch, short = 'v')]
    verbose: bool,

    #[argh(subcommand)]
    pub nested: SubCommand,
}

pub struct Opts {
    verbose: bool,
}

impl Opts {
    pub fn new(cmd: &Command) -> Self {
        Self {
            verbose: cmd.verbose,
        }
    }
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
pub enum SubCommand {
    Take(Take),
}

/// Dark Take
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "take")]
pub struct Take {
    /// frame to process
    #[argh(positional)]
    frame: String,

    /// address passed to grpcurl
    #[argh(positional)]
    addr: String,

    /// filepath of cut file
    #[argh(option, short = 'c')]
    cut: PathBuf,

    /// args passed to grpcurl
    #[argh(option, short = 'H')]
    header: String,

    /// output file
    #[argh(option, short = 'o')]
    output: Option<PathBuf>,
}

/// Dark Record
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "record")]
pub struct Record {
    /// frame to process
    #[argh(positional)]
    frame: PathBuf,

    /// address passed to grpcurl
    #[argh(positional)]
    addr: String,

    /// filepath of cut file, assumed to be in the same directory as the
    /// frame argument
    #[argh(option, short = 'c')]
    cut: Option<PathBuf>,

    /// args passed to grpcurl
    #[argh(option, short = 'H')]
    header: String,

    /// output file
    #[argh(option, short = 'o')]
    output: Option<PathBuf>,
}

// A basic example
