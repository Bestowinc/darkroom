use argh::FromArgs;
use std::error::Error;
use std::path::PathBuf;

pub mod grpc;
pub mod http;
pub mod params;
pub mod record;
pub mod take;

pub type BoxError = Box<dyn Error>;

pub use filmreel::cut::Register;
pub use filmreel::frame::*;
pub use filmreel::reel::{MetaFrame, Reel};

pub struct Logger;

impl log::Log for Logger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Info
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            println!("{}", record.args());
        }
    }

    fn flush(&self) {}
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

/// Additional options such as verbosity
pub struct Opts {
    pub verbose: bool,
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
    Version(Version),
    Take(Take),
    Record(Record),
}

/// returns CARGO_PKG_VERSION
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "version")]
pub struct Version {
    /// returns cargo package version, this is a temporary argh workaround
    #[argh(switch)]
    version: bool,
}

/// argh version workaround
pub fn version() -> String {
    option_env!("CARGO_PKG_VERSION")
        .unwrap_or("unknown")
        .to_string()
}

/// Takes a single frame, emitting the request then validating the returned response
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "take")]
pub struct Take {
    /// path of the frame to process
    #[argh(positional)]
    frame: PathBuf,

    /// enable TLS (not needed for HTTP/S)
    #[argh(switch)]
    tls: bool,

    /// pass proto files used for payload forming
    #[argh(option)]
    proto: Vec<PathBuf>,

    /// fallback address passed to the specified protocol
    #[argh(positional, short = 'a')]
    address: Option<String>,

    /// fallback header passed to the specified protocol
    #[argh(option, short = 'H')]
    header: Option<String>,

    /// filepath of input cut file
    #[argh(option, short = 'c')]
    cut: PathBuf,

    /// output of take file
    #[argh(option, short = 'o')]
    output: Option<PathBuf>,
}

/// Attempts to play through an entire Reel sequence running a take for every frame in the sequence
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "record")]
pub struct Record {
    /// directory path where frames and (if no explicit cut is provided) the cut are to be found
    #[argh(positional)]
    reel_path: PathBuf,

    /// name of the reel, used to find corresponding frames for the path provided
    #[argh(positional)]
    reel_name: String,

    /// enable TLS (not needed for HTTP/S)
    #[argh(switch)]
    tls: bool,

    /// pass proto files used for payload forming
    #[argh(option)]
    proto: Vec<PathBuf>,

    /// fallback address passed to the specified protocol if not provided by the frame itself
    #[argh(option, short = 'a')]
    address: Option<String>,

    /// fallback header passed to the specified protocol if not provided by the frame itself
    #[argh(option, short = 'H')]
    header: Option<String>,

    /// filepath of input cut file
    #[argh(option, short = 'c')]
    cut: Option<PathBuf>,

    /// filepath of component reel files
    #[argh(option, short = 'C')]
    component_reel: Vec<String>,

    /// filepath of merge cuts
    #[argh(positional)]
    merge_cuts: Vec<PathBuf>,

    /// output directory for successful takes
    #[argh(option, short = 'o')]
    output: Option<PathBuf>,

    /// interactive frame sequence transitions
    #[argh(switch, short = 'i')]
    interactive: bool,
}

impl Take {
    pub fn validate(&self) -> Result<(), &str> {
        if !self.frame.is_file() {
            return Err("<frame> must be a valid file");
        }
        if !self.cut.is_file() {
            return Err("<cut> must be a valid file");
        }
        Ok(())
    }
}
impl Record {
    pub fn validate(&self) -> Result<(), &str> {
        if !self.reel_path.is_dir() {
            return Err("<path> must be a valid directory");
        }

        // this permits describable zsh `=(thing)` or basic `<(thing)` FIFO syntax
        // https://superuser.com/questions/1059781/what-exactly-is-in-bash-and-in-zsh
        if let Some(cut) = &self.cut {
            if !cut.is_file() {
                return Err("<cut> must be a valid file");
            }
        } else {
            // check existence of implicit cut file in the same directory
            if !self.get_cut_file().is_file() {
                return Err("unable to find a matching cut file in the given directory");
            }
        }

        if let Some(output) = &self.output {
            if !output.is_dir() {
                return Err("<output> must be a valid directory");
            }
        }
        Ok(())
    }

    /// Returns expected cut filename in the given directory with the provided reel name
    pub fn get_cut_file(&self) -> PathBuf {
        if let Some(cut) = &self.cut {
            return cut.clone();
        }

        self.reel_path.join(format!("{}.cut.json", self.reel_name))
    }

    /// Returns a period  appended path of the current cut file attempting to reduce the likelihood
    /// that the original cut will be overwritten or for the output to be committed to version control
    pub fn get_cut_copy(&self) -> PathBuf {
        self.reel_path.join(format!(".{}.cut.json", self.reel_name))
    }
}
