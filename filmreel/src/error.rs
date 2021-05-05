use crate::utils::Rule;
use colored::*;
use pest::error::Error as PestError;
use serde_hashkey::Error as HashKeyError;
use serde_json::error::{Category, Error as SerdeError};
use std::{error, fmt};

pub type FrError<T> = FilmreelError<T>;

/// An error that occurred during parsing or hydrating a filmReel file
#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum FilmreelError<T>
where
    T: error::Error,
{
    FrameParse(&'static str),
    FrameParsef(&'static str, String),
    ReelParsef(&'static str, String),
    ReadInstruction(&'static str),
    WriteInstruction(&'static str),
    ReadInstructionf(&'static str, String),
    ReelParse(&'static str),
    Parse(String),
    Error(T),
}

impl<T> error::Error for FilmreelError<T> {
    fn description(&self) -> &str {
        "Error related to filmReel"
    }
}

macro_rules! errorf {
    ($fmt: expr, $err_name:expr, $err_msg:expr, $item: expr) => {
        writeln!($fmt, "\n{}", "=======================".red())?;
        writeln!($fmt, "{}: {}", $err_name.yellow(), $err_msg)?;
        writeln!($fmt, "{} {}", "-->".bright_black(), $item)?;
        writeln!($fmt, "{}", "=======================".red())?;
    };
}

impl<T> From<T> for FilmreelError<T>
where
    T: error::Error,
{
    fn from(err: T) -> FilmreelError<T> {
        Self(err)
    }
}

impl<T> From<SerdeError> for FilmreelError<T> {
    fn from(err: SerdeError) -> FilmreelError<T> {
        match err.classify() {
            Category::Io => unreachable!(),
            Category::Syntax | Category::Data | Category::Eof => {
                FilmreelError::Serde(err.to_string())
            }
        }
    }
}

impl<T> From<PestError<Rule>> for FilmreelError<T> {
    fn from(err: PestError<Rule>) -> FilmreelError<T> {
        Self::Pest(err)
    }
}

impl<T> fmt::Display for FilmreelError<T>
where
    T: error::Error,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FilmreelError::FrameParse(msg) => write!(f, "FrameParseError: {}", msg),
            FilmreelError::ReelParse(msg) => write!(f, "ReelParseError: {}", msg),
            FilmreelError::WriteInstruction(msg) => write!(f, "WriteInstructionError: {}", msg),
            FilmreelError::ReadInstruction(msg) => write!(f, "ReadInstructionError: {}", msg),
            FilmreelError::FrameParsef(msg, item) => {
                errorf!(f, "FrameParseError", msg, item);
                Ok(())
            }
            FilmreelError::ReelParsef(msg, item) => {
                errorf!(f, "ReelParseError", msg, item);
                Ok(())
            }
            FilmreelError::ReadInstructionf(msg, item) => {
                errorf!(f, "ReadInstructionError", msg, item);
                Ok(())
            }
            FilmreelError::Parse(msg) => {
                writeln!(f, "ParseError {} {}", "-->".red(), msg)?;
                Ok(())
            }
            FilmreelError::Error(msg) => {
                writeln!(f, "Error {} {}", "-->".red(), msg)?;
                Ok(())
            }
        }
    }
}
