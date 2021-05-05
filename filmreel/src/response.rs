use crate::{
    cut::Register,
    error::FrError,
    frame::*,
    utils::{get_jql_value, new_selector, Selector},
};
use serde::{Deserialize, Serialize};
use serde_hashkey::{to_key_with_ordered_float as to_key, Key};
use serde_json::{json, to_value, Value};
use std::{
    cell::Cell,
    cmp::Reverse,
    collections::{BTreeMap, HashMap, HashSet},
};

const INVALID_INSTRUCTION_TYPE_ERR: &str =
    "Frame write instruction did not correspond to a string object";

///
/// Encapsulates the expected response payload.
///
/// [Request Object](https://github.com/Bestowinc/filmReel/blob/master/frame.md#request)
#[derive(Serialize, Clone, Deserialize, Debug)]
pub struct Response<'a> {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub body:       Option<Value>,
    //
    #[serde(flatten, skip_serializing_if = "Option::is_none")]
    pub etc:        Option<Value>, // https://github.com/serde-rs/serde/issues/1626
    #[serde(borrow, skip_serializing)]
    pub validation: Option<Validation<'a>>,
    pub status:     u32,
}

impl<'a> Response<'a> {
    /// Cast to a serialized Frame as serde_json::Value object for consistency in jql object
    /// traversal: `"response"."body"` should always traverse a serialized Frame struct
    fn to_frame_value(&self) -> Result<Value, FrError> {
        Ok(json!({"response":to_value(self)?}))
    }

    pub(crate) fn validate(&self) -> Result<(), FrError> {
        if self.validation.is_none() {
            return Ok(());
        }
        // for now hardcode checking only response body
        for k in self.validation.as_ref().unwrap().keys() {
            if !k.trim_start_matches('.').starts_with("'response'.'body'") {
                return Err(FrError::ReadInstruction(
                    "validation options currently only support the response body",
                ));
            }
        }
        Ok(())
    }

    /// Using the write instructions found in the frame InstructionSet, look for matches to be
    /// passed to write operations
    pub fn match_payload_response(
        &self,
        set: &'a InstructionSet,
        payload_response: &Response,
    ) -> Result<Option<HashMap<&'a str, Value>>, FrError> {
        let frame_response: Value = self.to_frame_value()?;
        let payload_response: Value = payload_response.to_frame_value()?;

        let mut write_matches: HashMap<&str, Value> = HashMap::new();
        for (k, query) in set.writes.iter() {
            // ensure frame jql query returns a string object
            let frame_str = match get_jql_value(&frame_response, query) {
                Ok(Value::String(v)) => Ok(v),
                Ok(_) => Err(FrError::FrameParsef(
                    INVALID_INSTRUCTION_TYPE_ERR,
                    query.to_string(),
                )),
                Err(e) => Err(e),
            }?;
            let payload_val = get_jql_value(&payload_response, query)?;

            if let Value::String(payload_str) = &payload_val {
                let write_match = Register::write_match(k, &frame_str, payload_str)?;
                if let Some(mat) = write_match {
                    write_matches.insert(k, to_value(mat)?);
                }
                continue;
            }
            // handle non string payload values returned by the jql query
            Register::expect_standalone_var(k, &frame_str)?;
            write_matches.insert(k, payload_val);
        }

        if write_matches.iter().next().is_some() {
            return Ok(Some(write_matches));
        }

        Ok(None)
    }

    /// Applies the validations using the BTree key as the Value selector
    pub fn apply_validation(&mut self, other: &mut Self) -> Result<(), FrError> {
        if self.body.is_none() || other.body.is_none() || self.validation.is_none() {
            return Ok(());
        }
        for (k, v) in self.validation.as_ref().unwrap().iter() {
            // if no validator operations are needed
            if !v.partial && !v.unordered {
                continue;
            }

            let selector = new_selector(strip_query(k))?;
            match (v.partial, v.unordered) {
                (false, false) => {
                    unreachable!();
                }
                (true, false) => {
                    v.apply_partial(
                        selector,
                        self.body.as_mut().unwrap(), // T as Option<&mut Value>.unwrap()
                        other.body.as_mut().unwrap(),
                    )?;
                }
                (false, true) => {
                    v.apply_unordered(
                        selector,
                        self.body.as_mut().unwrap(),
                        other.body.as_mut().unwrap(),
                    )?;
                }
                (true, true) => {
                    unimplemented!();
                }
            }
        }

        // for comparison's sake set validtion to None once applying is finished
        self.validation = None;

        Ok(())
    }
}

// For now selector queries are only used on the reponse body
// selector logic takes the body Value object while mainting a valid
// "whole file" query for reference's sake
// "'response'.'body'" => "."
// "'response'.'body'.'key'" => ".'key'"
fn strip_query(query: &str) -> &str {
    let body_query = query
        .trim_start_matches('.')
        .trim_start_matches("'response'.'body'");

    if body_query.is_empty() {
        return ".";
    }
    body_query
}

impl Default for Response<'_> {
    fn default() -> Self {
        Self {
            body:       None,
            etc:        Some(json!({})),
            validation: None,
            status:     0,
        }
    }
}

/// PartialEq needs to exlcude self.validation to ensure that [Response::aply_validation] can
/// diffentiatiate between the parent `Response` (the one pulled directle from the filmReel file)
/// and the child `Response` (one deserialized from returned data) since the client validations
/// should always be `None`
impl<'a> PartialEq for Response<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.body.eq(&other.body) && self.etc.eq(&other.etc) && self.status.eq(&other.status)
    }
}

impl<'a> Eq for Response<'a> {}

type Validation<'a> = BTreeMap<&'a str, Validator>;

/// Validator represents one validation ruleset applied to a single JSON selection
#[derive(Serialize, Clone, Deserialize, Default, Debug, PartialEq)]
#[serde(default)]
pub struct Validator {
    partial:   bool,
    unordered: bool,
}

impl Validator {
    // partial validation?
    fn apply_partial(
        &self,
        selector: Selector,
        self_body: &mut Value,
        other_body: &mut Value,
    ) -> Result<(), FrError> {
        let selection = selector(self_body).ok_or(FrError::ReadInstruction(
            "selection missing from Frame body",
        ))?;
        match selection {
            Value::Object(o) => {
                let preserve_keys = o.keys().collect::<Vec<&String>>();
                // if the response selection is not an object or selects nothing (None is returned)
                // return early
                let other_selection = match selector(other_body) {
                    Some(Value::Object(o)) => o,
                    _ => return Ok(()),
                };

                let mut has_mutual_keys = false;

                let other_keys: Vec<String> = other_selection
                    .keys()
                    .filter(|k| {
                        let contains = preserve_keys.contains(k);

                        if contains {
                            has_mutual_keys = true;
                        }
                        !contains
                    }) // retain keys that are not found in preserve_keys
                    .cloned()
                    .collect();

                // if there are no mutual keys at all, then do not mutate other_selection
                if !has_mutual_keys {
                    return Ok(());
                }

                for k in other_keys.iter() {
                    other_selection.remove(k);
                }
            }
            Value::Array(self_selection) => {
                let other_selection = match selector(other_body) {
                    Some(Value::Array(o)) => o,
                    _ => return Ok(()),
                };

                let self_len = self_selection.len();
                // do not mutate if self_len is greater that other_selection
                if self_len >= other_selection.len() {
                    return Ok(());
                }

                // do a rolling check seeing if self_selection is a subset of other_selection
                // Self:  [            A, B, C]
                // Other: [A, B, B, C, A, B, C]
                //
                // i=0; [ABB] != [ABC]
                // i=1; [BBC] != [ABC]
                // i=2; [BCA] != [ABC]
                // i=3; [CAB] != [ABC]
                // i=4; [ABC] == [ABC]
                //
                // NOTE: Array partial matches need to be ordered as well as contiguous.
                // The example below would not result in a match:
                // Other: [A, B, B, C, A, B, B, C]
                for (i, _) in other_selection.clone().iter().enumerate() {
                    if i + self_len > other_selection.len() {
                        // other_selection[i..] is already larger than self_selection here
                        // cannot find a partial match at this point
                        return Ok(());
                    }
                    if &other_selection[i..i + self_len] == self_selection.as_slice() {
                        *other_selection = self_selection.clone();
                        return Ok(()); // partial match has been found, no need to iterate further
                    }
                }
            }
            _ => {
                return Err(FrError::ReadInstruction(
                    "validation selectors must point to a JSON object or array",
                ))
            }
        }
        Ok(())
    }
    // What if dupes in self
    // Self:  [            A, A, B, C]
    // for i=0, Self[0] is found: in Other[0]
    // Other: [A, B, B, C, A, B, C]
    // then Search range [1:] for A
    // then Search range [6:] for B
    // then Search range [7:] for C
    // Self:  [            A, A, B, C]
    // unordered then partial
    // Other: [A, B, B, C, A, B, C]
    // Intersections: [0, 4, 5, 6]
    // Other: [A, A, B, C, B, B, C]
    // partial:
    // Other: [A, B, B, C]

    // What if dupes in self
    // Self:  [            A, C, A, B, C]
    // for i=0, Self[0] is found: in Other[0]
    // Other: [C, A, B, B, A, B, C]
    // then Search range [2:] for A
    // then Search range [4:] for C
    // then Search range [6:] for B
    // then Search range [7:] for C
    // Initial Map to be:
    // Self of [0] <HashOf<A>, mut 1>
    // Self of [1] <HashOf<B>, mut 2>
    // Self of [2] <HashOf<C>, mut 0>
    // Self:  [            A, C, A, B, C]
    // Other: [C, A, B, B, A, B, C]
    //
    // HashMap<key: HashMap<A>,key: Vec<usize>>
    // Self[0] <HashOf<A>, Vec<0,4> // now lookup for A is [:]    => [0+1:]
    // Self[1] <HashOf<C>, Vec<0,6> // now lookup for C is [:]    => [1+1:]
    // Self[3] <HashOf<B>, Vec<2,3,5> // now lookup for B is [:]    => [2+1:]
    //
    // Self of [2] <HashOf<A>, mut 4> meaning next A will be in range [4+1:]

    // remove from other_selection starting with the last index of other
    // and inserting it into the index of where it is found in self
    // ----------------
    // Self:  [A, B, C]
    // Other: [B, A, C]
    //
    // Expected intersections/inter sorted by index of Other:
    // Self[2] == Other[2]; (2,2)
    // Self[0] == Other[1]; (0,1)
    // Self[1] == Other[0]; (1,0)
    //
    // Expected iterations:
    // i=2 v=2:
    // Other.remove(2) -> C; Other == [B, A ]; inter[2] <- C; inter == [Null, Null, C]
    // i=0 v=1:
    // Other.remove(1) -> A; Other == [B    ]; inter[0] <- C; inter == [A,    Null, C]
    // i=1 v=0:
    // Other.remove(0) -> B; Other == [     ]; inter[1] <- C; inter == [A, B, C      ]
    //
    // ----------------
    // Self:  [A, B, C]
    // Other: [other_value, false, A, B, C]
    //
    // Expected intersections/inter sorted by index of Other:
    // Self[2] == Other[4]; (2,4)
    // Self[1] == Other[3]; (1,3)
    // Self[0] == Other[2]; (0,2)
    //
    // Expected iterations:
    // i= 2 v=4:
    // Other.remove(4) -> C; Other == [other_value, false, A, B]; inter[2] <- C; intersection == [Null, Null, C]
    // i= 1 v=3:
    // Other.remove(3) -> B; Other == [other_value, false, A   ]; inter[1] <- B; intersection == [A,    Null, C]
    // i= 0 v=2:
    // Other.remove(2) -> A; Other == [other_value, false      ]; inter[0] <- A; intersection == [A, B, C      ]
    // ----------------
    fn apply_unordered(
        &self,
        selector: Selector,
        self_body: &mut Value,
        other_body: &mut Value,
    ) -> Result<(), FrError> {
        let selection = selector(self_body).ok_or(FrError::ReadInstruction(
            "selection missing from Frame body",
        ))?;
        match selection {
            Value::Object(_) => Ok(()),
            Value::Array(self_selection) => {
                let other_selection = match selector(other_body) {
                    Some(Value::Array(o)) => o,
                    _ => return Ok(()),
                };
                // Create a lookup hashmap for self_selection.
                let mut filter_keys: HashSet<usize> = HashSet::new();
                // https://gist.github.com/daboross/976978d8200caf86e02acb6805961195
                let mut other_keys: HashMap<Key<_>, Vec<usize>> = other_selection
                    .iter()
                    .enumerate()
                    // (i: usize, v: &Value)
                    .map(|(i, v)| match to_key(v) {
                        Ok(k) => {
                            // retain reference of key to be removed so we can swap it
                            // with a Value::Null when doing substitution
                            filter_keys.insert(i);
                            Ok((k, vec![i]))
                        }
                        Err(e) => Err(e),
                    })
                    .collect::<Result<HashMap<Key<_>, Vec<usize>>, _>>()
                    .map_err(|e| FrError::Parse(e.to_string()))?;

                let mut match_sink: Vec<Value> = vec![Value::Null; filter_keys.len()];
                for (to_idx, v) in self_selection.iter().enumerate() {
                    let v_hash = to_key(v).map_err(|e| FrError::Parse(e.to_string()))?;

                    if let Some(other_hash_idxs) = other_keys.get_mut(&v_hash) {
                        // remove idx from Vec so that it is not reused
                        let from_idx = other_hash_idxs.remove(0);
                        // Swap places with the Value::Null in the match_sink so that values in
                        // Other maintain a valid index reference
                        std::mem::swap(&mut match_sink[to_idx], &mut other_selection[from_idx]);
                    }
                }

                // we've found no intersections
                // return early
                if match_sink.is_empty() {
                    return Ok(());
                }

                // retain other_selection elements where index is no in filter_keys
                let mut i = 0;
                other_selection.retain(|_| (!filter_keys.contains(&i), i += 1).0);
                other_selection.splice(0..0, match_sink.into_iter());

                Ok(())
            }
            _ => Err(FrError::ReadInstruction(
                "validation selectors must point to a JSON object or array",
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from, to};
    use rstest::*;
    use serde_json::json;

    #[test]
    fn test_match_payload_response() {
        let frame = Frame {
            protocol: Protocol::GRPC,
            cut:      InstructionSet {
                reads:          from![],
                writes:         to! ({
                    "USER_ID"=> "'response'.'body'.'id'",
                    "CREATED"=> "'response'.'body'.'created'",
                    "ignore"=> "'response'.'body'.'array'.[0].'ignore'"
                }),
                hydrate_writes: true,
            },
            request:  Request {
                ..Default::default()
            },
            response: Response {
                body: Some(json!({
                    "id": "${USER_ID}",
                    "created": "${CREATED}",
                    "array": [{"ignore":"${ignore}"}]
                })),
                status: 0,
                ..Default::default()
            },
        };

        let payload_response = Response {
            body: Some(json!({
                "id": "ID_010101",
                "created": 101010,
                "array": [{"ignore": "value"}]
            })),
            status: 0,
            ..Default::default()
        };
        let mat = frame
            .response
            .match_payload_response(&frame.cut, &payload_response)
            .unwrap();
        let mut expected_match = HashMap::new();
        expected_match.insert("USER_ID", to_value("ID_010101").unwrap());
        expected_match.insert("CREATED", to_value(101010).unwrap());
        expected_match.insert("ignore", to_value("value").unwrap());
        assert_eq!(expected_match, mat.unwrap());
    }

    fn partial_case(case: u32) -> (&'static str, &'static str, bool) {
        let frame_obj_response = r#"
{
  "validation": {
    "'response'.'body'": {
      "partial": true
    }
  },
  "body": {
    "A": true,
    "B": true,
    "C": true
  },
  "status": 200
}
    "#;
        let frame_arr_response = r#"
{
  "validation": {
    "'response'.'body'": {
      "partial": true
    }
  },
  "body": [
    "A",
    "B",
    "C"
  ],
  "status": 200
}
    "#;

        match case {
            1 => (
                frame_obj_response,
                r#"{"body":{"A": true,"B": true,"C": true},"status": 200}"#,
                true,
            ),
            2 => (
                frame_obj_response,
                r#"{"body":{"A": true,"B": true,"C": true, "D": true},"status": 200}"#,
                true,
            ),
            3 => (
                frame_obj_response,
                r#"{"body":{"B": true,"C": true, "D": true},"status": 200}"#,
                false,
            ),
            4 => (
                // explicitly declare partial validation as false
                r#"{"validation":{"'response'.'body'":{"partial":false}},
                    "body":{"A": true,"B": true, "C": true},"status": 200}"#,
                r#"{"body":{"B": true,"C": true, "D": true},"status": 200}"#,
                false,
            ),
            5 => (
                frame_arr_response,
                r#"{"body":["A", "B", "C"],"status": 200}"#,
                true,
            ),
            6 => (
                frame_arr_response,
                r#"{"body":["other_value", false, "A", "B", "C"],"status": 200}"#,
                true,
            ),
            7 => (
                frame_arr_response,
                r#"{"body":["other_value", false, "B", "C"],"status": 200}"#,
                false,
            ),
            _ => panic!(),
        }
    }

    #[rstest(
        t_case,
        case(partial_case(1)),
        case(partial_case(2)),
        case(partial_case(3)),
        case(partial_case(4)),
        case(partial_case(5)),
        case(partial_case(6)),
        case(partial_case(7))
    )]
    fn test_partial_validation(t_case: (&str, &str, bool)) {
        let mut frame: Response = serde_json::from_str(t_case.0).unwrap();
        let mut actual: Response = serde_json::from_str(t_case.1).unwrap();
        let should_match = t_case.2;
        frame.apply_validation(&mut actual).unwrap();
        if should_match {
            pretty_assertions::assert_eq!(frame, actual);
        } else {
            pretty_assertions::assert_ne!(frame, actual);
        }
    }

    fn unordered_case(case: u32) -> (&'static str, &'static str, bool) {
        let frame_obj_response = r#"
{
  "validation": {
    "'response'.'body'": {
      "unordered": true
    }
  },
  "body": {
    "A": true,
    "B": true,
    "C": true
  },
  "status": 200
}
    "#;
        let frame_arr_response = r#"
{
  "validation": {
    "'response'.'body'": {
      "unordered": true
    }
  },
  "body": [
    "A",
    "B",
    "C"
  ],
  "status": 200
}
    "#;
        let frame_arr_with_f32 = r#"
{
  "validation": {
    "'response'.'body'": {
      "unordered": true
    }
  },
  "body": [
    "A",
    "B",
    "C",
    13.37
  ],
  "status": 200
}
            "#;

        match case {
            1 => (
                frame_obj_response,
                r#"{"body":{"A":true,"B":true,"C":true},"status":200}"#,
                true,
            ),
            2 => (
                frame_obj_response,
                r#"{"body":{"A":true,"B":false,"C":true},"status":200}"#,
                false,
            ),
            3 => (
                frame_obj_response,
                r#"{"body":{"A":true,"B":true,"C":true,"D":true},"status":200}"#,
                false,
            ),
            4 => (
                frame_obj_response,
                r#"{"body":{"A":true,"B":true},"status":200}"#,
                false,
            ),
            5 => (
                frame_obj_response,
                r#"{"body":{"B":true,"C":true,"A":true},"status":200}"#,
                true,
            ),
            6 => (
                frame_arr_response,
                r#"{"body":["A","B","C"],"status":200}"#,
                true,
            ),
            7 => (
                frame_arr_response,
                r#"{"body":["other_value",false,"A","B","C"],"status":200}"#,
                false,
            ),
            8 => (
                frame_arr_response,
                r#"{"body":[false,false,"A","B","C"],"status":200}"#,
                false,
            ),
            9 => (
                frame_arr_response,
                r#"{"body":["B","A","C"],"status":200}"#,
                true,
            ),
            10 => (
                frame_arr_response,
                r#"{"body":["B","A","D","C"],"status":200}"#,
                false,
            ),
            11 => (
                frame_arr_with_f32,
                r#"{"body":["C", 13.37, "B", "A"],"status": 200}"#,
                true,
            ),
            12 => (
                frame_arr_response,
                r#"{"body":["A", "C", 13.37, "B", "A"],"status": 200}"#,
                true,
            ),
            _ => panic!(),
        }
    }

    #[rstest(
        t_case,
        case(unordered_case(1)),
        case(unordered_case(2)),
        case(unordered_case(3)),
        case(unordered_case(4)),
        case(unordered_case(5)),
        case(unordered_case(6)),
        case(unordered_case(7)),
        case(unordered_case(8)),
        case(unordered_case(9)),
        case(unordered_case(10)),
        case(unordered_case(11))
    )]
    fn test_unordered_validation(t_case: (&str, &str, bool)) {
        let mut frame: Response = serde_json::from_str(t_case.0).unwrap();
        let mut actual: Response = serde_json::from_str(t_case.1).unwrap();
        let should_match = t_case.2;
        frame.apply_validation(&mut actual).unwrap();
        if should_match {
            pretty_assertions::assert_eq!(frame, actual);
        } else {
            pretty_assertions::assert_ne!(frame, actual);
        }
    }
}
