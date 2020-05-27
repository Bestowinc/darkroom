use crate::Command;
use anyhow::{anyhow, Error};
use filmreel::frame::Request;
use serde::Deserialize;
use std::path::PathBuf;

/// Parameters needed for a uri method to be sent.
#[derive(Debug, PartialEq, Clone)]
pub struct Params<'a> {
    pub tls: bool,
    pub header: Option<String>,
    pub address: String,
    pub proto: Option<&'a Vec<PathBuf>>,
    pub attempts: Option<Attempts>,
}

/// BaseParams contains parameter values provided by a Record or Take object
/// before the given values are checked for in the Frame
#[derive(Clone)]
pub struct BaseParams {
    pub tls: bool,
    pub header: Option<String>,
    pub address: Option<String>,
    pub proto: Vec<PathBuf>,
    pub cut_out: Option<PathBuf>,
    pub interactive: bool,
    pub verbose: bool,
}

#[derive(Clone, Deserialize, Default, Debug, PartialEq)]
pub struct Attempts {
    pub times: u32,
    pub ms: u32,
}

impl From<&Command> for BaseParams {
    fn from(cmd: &Command) -> Self {
        Self {
            tls: cmd.tls,
            header: cmd.header.clone(),
            address: cmd.address.clone(),
            proto: cmd.proto.clone(),
            cut_out: cmd.cut_out.clone(),
            interactive: cmd.interactive,
            verbose: cmd.verbose,
        }
    }
}

impl BaseParams {
    /// init provides a frame's request properties to override or populated
    /// parameter fields desired by a specific Frame
    pub fn init(&self, request: Request) -> Result<Params, Error> {
        // let request = frame.get_request();

        let header: Option<String> = match request.get_header() {
            Some(i) => Some(i.to_string()),
            None => self.header.clone(),
        };
        let address = match request.get_entrypoint() {
            Some(i) => i,
            None => self
                .address
                .clone()
                .ok_or_else(|| anyhow!("Params: missing address"))?,
        };

        let attempts: Option<Attempts> = match request.get_etc().get("attempts") {
            Some(v) => serde_json::from_value(v.clone())?,
            None => None,
        };

        let proto = match self.proto.len() {
            0 => None,
            _ => Some(&self.proto),
        };

        Ok(Params {
            tls: self.tls,
            header,
            address,
            proto,
            attempts,
        })
    }
}

/// iter_path_args chains prefixes to every item in an iterable for use with std::Process::Command args
pub fn iter_path_args<'a, S, I>(prefix: S, path_ref: I) -> impl Iterator<Item = S> + 'a
where
    S: Clone + 'a,
    I: IntoIterator<Item = S> + 'a,
{
    path_ref
        .into_iter()
        .flat_map(move |x| vec![prefix.clone(), x])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Command, SubCommand, Version};
    use filmreel::frame::{Frame, Request};
    use std::{ffi::OsStr, path::PathBuf};

    #[test]
    fn test_init() {
        let args = Command {
            tls: false,
            address: Some("www.initial_addr.com".to_string()),
            header: Some("initial_header".to_string()),
            proto: vec![],
            verbose: false,
            cut_out: None,
            interactive: false,
            nested: SubCommand::Version(Version { version: true }),
        };
        let request: Request = serde_json::from_str::<Frame>(
            r#"
{
  "protocol": "HTTP",
  "request": {
    "body": {},
    "header": "Authorization: Bearer BIG_BEAR",
    "entrypoint": "localhost:8000",
    "uri": "POST /it/notes",
    "attempts": {
      "times": 2,
      "ms": 200
    }
  },
  "response": {
    "body": {},
    "status": 200
  }
}
    "#,
        )
        .unwrap()
        .get_request();

        let base_params = args.base_params();
        let params: Params = base_params.init(request).unwrap();
        assert_eq!(
            Params {
                tls: false,
                header: Some("\"Authorization: Bearer BIG_BEAR\"".to_string()),
                address: "localhost:8000".to_string(),
                proto: None,
                attempts: Some(Attempts { times: 2, ms: 200 }),
            },
            params
        )
    }

    #[test]
    fn test_iter_path_args() {
        let path_vec = vec![
            PathBuf::from("./first.file"),
            PathBuf::from("./second_file"),
        ];

        let expected: Vec<&OsStr> = ["prefix", "./first.file", "prefix", "./second_file"]
            .iter()
            .map(|x| OsStr::new(x))
            .collect();
        assert_eq!(
            expected,
            iter_path_args(OsStr::new("prefix"), path_vec.iter().map(|x| x.as_ref()))
                .collect::<Vec<&OsStr>>()
        );
    }
}
