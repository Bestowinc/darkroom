use crate::grpc::grpcurl;
use crate::http::http_request;
use crate::params::BaseParams;
use crate::Take;
use anyhow::{anyhow, Error};
use colored::*;
use colored_diff::PrettyDifference;
use colored_json::prelude::*;
use filmreel as fr;
use fr::cut::Register;
use fr::frame::{Frame, Protocol, Response};
use log::{debug, error, info};
use prettytable::*;
use std::fs;
use std::io::{self, prelude::*};
use std::path::PathBuf;

/// Performs a single frame hydration using a given json file and outputs a Take to either stdout
/// or a designated file
pub fn run_request<'a>(
    frame: &'a mut Frame,
    register: &'a Register,
    base_params: &BaseParams,
) -> Result<Response, Error> {
    let interactive = base_params.interactive;
    let unhydrated_frame: Option<Frame> = if interactive {
        Some(frame.clone())
    } else {
        info!("[{}] frame:", "Unhydrated".red());
        info!("{}", frame.to_string_pretty().to_colored_json_auto()?);
        info!("{}", "=======================".magenta());
        info!("HYDRATING...");
        info!("{}", "=======================".magenta());
        None
    };

    frame.hydrate(&register, false)?;

    if interactive {
        let mut stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut table = Table::new();
        table.add_row(row![
            format!("[{}] frame", "Unhydrated".red()),
            "Cut Register",
            format!("[{}] frame", "Hydrated".green()),
        ]);
        let mut hidden_frame = unhydrated_frame.clone().expect("None for hidden frame");
        hidden_frame.hydrate(&register, true)?;

        table.add_row(row![
            unhydrated_frame
                .expect("None for unhydrated_frame")
                .to_string_pretty()
                .to_colored_json_auto()?,
            register.to_string_hidden()?.to_colored_json_auto()?,
            hidden_frame.to_string_pretty().to_colored_json_auto()?,
        ]);
        table.printstd();
        write!(
            stdout,
            "{}",
            format!("Press {} to continue...", "ENTER".yellow())
        )
        .expect("write to stdout panic");
        stdout.flush().expect("stdout flush panic");

        // Read a single byte and discard
        let _ = stdin.read(&mut [0u8]).expect("read stdin panic");
    } else {
        info!("[{}] frame:", "Hydrated".green());
        info!("{} {}\n", "Request URI:".yellow(), frame.get_request_uri()?);
        info!("{}", frame.to_string_pretty().to_colored_json_auto()?);
        info!("{}\n", "=======================".magenta());
    }

    let params = base_params.init(frame.get_request())?;
    // Send out the payload here
    match frame.protocol {
        Protocol::HTTP => http_request(params, frame.get_request()),
        Protocol::GRPC => grpcurl(params, frame.get_request()),
    }
}

pub fn process_response<'a>(
    frame: &'a mut Frame,
    cut_register: &'a mut Register,
    payload_response: Response,
    output: Option<PathBuf>,
) -> Result<&'a Register, Error> {
    let payload_matches = match frame
        .response
        .match_payload_response(&frame.cut, &payload_response)
    {
        Err(e) => {
            log_mismatch(
                frame.response.to_string_pretty(),
                payload_response.to_string_pretty(),
            );
            return Err(Error::from(e));
        }
        Ok(r) => r,
    };

    // If there are valid matches for write operations
    if let Some(matches) = payload_matches {
        debug!("writing to cut register...");
        for (k, v) in matches {
            cut_register.write_operation(k, v)?;
        }

        // For now simply run hydrate again to hydrate the newly written cut variables into the
        // Response
        frame.cut.hydrate_writes = true;
        Frame::hydrate_val(&frame.cut, &mut frame.response.body, &cut_register, false)?;
        Frame::hydrate_val(&frame.cut, &mut frame.response.etc, &cut_register, false)?;
    }

    if frame.response != payload_response {
        error!(
            "{}",
            PrettyDifference {
                expected: &frame.response.to_string_pretty(),
                actual: &payload_response.to_string_pretty(),
            }
        );
        error!(
            "{}{}{}",
            "= ".red(),
            "Value Mismatch 🤷‍♀️ ".yellow(),
            "===".red()
        );
        return Err(anyhow!("request/response mismatch"));
    }

    // remove lowercase values
    cut_register.flush_ignored();

    info!(
        "{}{}{}",
        "= ".green(),
        "Match 👍 ".yellow(),
        "============\n".green()
    );

    // If an output was specified create a take file
    if let Some(frame_out) = output {
        debug!("creating take receipt...");
        fs::write(frame_out, frame.to_string_pretty())?;
    }
    if payload_response != frame.response {
        error!("OK");
    }

    Ok(cut_register)
}

/// Run single take using the darkroom::Take struct
pub fn single_take(cmd: Take, base_params: BaseParams) -> Result<(), Error> {
    let frame_str = fr::file_to_string(&cmd.frame)?;
    let cut_str = fr::file_to_string(&cmd.cut)?;

    // Frame to be mutably borrowed
    let frame = Frame::new(&frame_str)?;
    let mut payload_frame = frame.clone();
    let mut cut_register = Register::new(&cut_str)?;
    let response = run_request(&mut payload_frame, &cut_register, &base_params)?;

    process_response(
        &mut payload_frame,
        &mut cut_register,
        response,
        cmd.take_out.clone(),
    )?;

    if let Some(path) = base_params.cut_out {
        debug!("writing cut output to PathBuf...");
        fs::write(path, &cut_register.to_string_hidden()?)
            .expect("unable to write to cmd.get_cut_copy()");
    }
    Ok(())
}

fn log_mismatch(frame_str: String, response_str: String) {
    error!("{}\n", "Expected:".magenta());
    error!(
        "{}\n",
        frame_str
            .to_colored_json_auto()
            .expect("log_mismatch expected panic")
    );
    error!("{}\n", "Actual:".magenta());
    error!(
        "{}\n",
        response_str
            .to_colored_json_auto()
            .expect("log_mismatch actual panic")
    );
    error!(
        "{}{}{}",
        "= ".red(),
        "Form Mismatch 🌋 ".yellow(),
        "====".red()
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use filmreel::cut::Register;
    use filmreel::frame::Response;
    use filmreel::register;
    use serde_json::{self, json};

    #[test]
    fn test_process_response() {
        let mut frame: Frame = serde_json::from_str(
            r#"
{
  "protocol": "HTTP",
  "cut": {
    "to": {
      "USER_ID": "'response'.'body'"
    }
  },
  "request": {
    "body": {},
    "uri": ""
  },
  "response": {
    "body": "created user: ${USER_ID}",
    "status": 200
  }
}
    "#,
        )
        .unwrap();
        let payload_response = Response {
            body: json!("created user: BIG_BEN"),
            etc: json!({}),
            status: 200,
        };
        let mut register = Register::default();
        let processed_register =
            process_response(&mut frame, &mut register, payload_response, None).unwrap();
        assert_eq!(*processed_register, register!({"USER_ID"=>"BIG_BEN"}));
    }
}
