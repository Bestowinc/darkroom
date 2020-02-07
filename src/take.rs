use crate::Take;
use colored::*;
use filmreel as fr;
use filmreel::cut::Register;
use filmreel::frame::{Frame, Response};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::json;
use serde_yaml;
use std::error::Error;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::process::exit;
use std::process::Command;

/// Performs a single frame hydration using a given json file and outputs a Take to either stdout
/// or a designated file
pub fn run_take(cmd: Take) -> Result<(), Box<dyn Error>> {
    if let Err(e) = Command::new("grpcurl").spawn() {
        if let ErrorKind::NotFound = e.kind() {
            return Err("`grpcurl` was not found! Check your PATH!".into());
        } else {
            return Err(e.into());
        }
    }

    let frame_str = fr::file_to_string(cmd.frame).expect("frame error");
    let cut_str = fr::file_to_string(&cmd.cut).expect("cut error");

    let mut frame = Frame::new(&frame_str)?;

    let cut_register = Register::new(&cut_str)?;

    // println!("{}", "Unhydrated frame JSON:".red());
    // println!("{}", frame.to_string_pretty());
    // println!("{}", "Request URI:".yellow());

    // println!("{}", "=======================".magenta());
    // println!("HYDRATING");
    // println!("{}", "=======================".magenta());
    frame.hydrate(&cut_register).unwrap_or_else(|err| {
        eprintln!("Frame {}", err);
        exit(1);
    });

    println!();
    println!();
    println!("{}", "Hydrated frame JSON:".green());
    println!();
    println!("{}", frame.to_string_pretty());
    println!("{}", "Request URI:".yellow());
    let grpc_uri = frame.get_request_uri().expect("grpc uri error");
    let grpc_response = Command::new("grpcurl")
        .arg("-H")
        .arg(cmd.header)
        .arg("-plaintext")
        .arg("-d")
        .arg(frame.get_request())
        .arg(cmd.addr)
        .arg(grpc_uri)
        .output()
        .expect("No grpcurl");

    let mut out_register = cut_register.clone();

    let response: Response = match grpc_response.status.code().unwrap() {
        0 => Response {
            body: serde_json::from_slice(&grpc_response.stdout).expect("invalid UTF-8"),
            status: 0,
            etc: json!({}),
        },
        _ => {
            let g_err: GrpcurlError = serde_yaml::from_slice(&grpc_response.stderr)?;
            // create frame response from deserialized grpcurl error
            Response {
                body: serde_json::Value::String(g_err.message),
                status: g_err.code,
                etc: json!({}),
            }
        }
    };

    dbg!(&response);
    return Ok(());

    let matches = frame.match_payload_response(&response)?;
    std::fs::write(&cmd.cut.clone(), &out_register.to_string_pretty())
        .expect("Unable to write file");
    if let Some(frame_out) = cmd.output {
        std::fs::write(frame_out, frame.to_string_pretty()).expect("Unable to write file");
    }

    dbg!(frame.to_string_pretty());
    Ok(())
}

#[derive(Debug, Serialize, PartialEq)]
struct GrpcurlError {
    code: u32,
    message: String,
}

impl<'de> Deserialize<'de> for GrpcurlError {
    #[allow(non_snake_case)]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        use serde::de::Error;

        // deserialize a nested yaml object by casing it to an inner struct first
        #[derive(Deserialize)]
        struct Outer {
            ERROR: Inner,
        }

        #[derive(Deserialize)]
        struct Inner {
            Code: String,
            Message: String,
        }
        let outer = Outer::deserialize(deserializer)?;
        let code = match outer.ERROR.Code.as_str() {
            "Canceled" => 1,
            "Unknown" => 2,
            "InvalidArgument" => 3,
            "DeadlineExceeded" => 4,
            "NotFound" => 5,
            "AlreadyExists" => 6,
            "PermissionDenied" => 7,
            "ResourceExhausted" => 8,
            "FailedPrecondition" => 9,
            "Aborted" => 10,
            "OutOfRange" => 11,
            "Unimplemented" => 12,
            "Internal" => 13,
            "Unavailable" => 14,
            "DataLoss" => 15,
            "Unauthenticated" => 16,
            _ => return Err(D::Error::custom("unexpected gRPC error code"))?,
        };
        Ok(GrpcurlError {
            code,
            message: outer.ERROR.Message,
        })
    }
}

#[cfg(test)]
mod serde_tests {
    use super::*;
    use serde_yaml;

    const YAML_ERROR: &str = r#"
        ERROR:
            Code: Internal
            Message: input cannot be empty"#;

    #[test]
    fn test_yaml() {
        let yaml_struct: GrpcurlError = serde_yaml::from_str(YAML_ERROR).unwrap();
        assert_eq!(
            GrpcurlError {
                code: 13,
                message: "input cannot be empty".to_owned()
            },
            yaml_struct
        );
    }
}