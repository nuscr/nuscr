use std::path::{Path, PathBuf};
use std::process::Command;

const EXPECTED_FAIL_SUITES: &[&str] = &[
    "codegen",
    "refinements",
    "core/protobuf.t",
    "core/invalid-pragma.t",
];

#[test]
fn expected_fail_registry_covers_unsupported_suites() {
    for suite in EXPECTED_FAIL_SUITES {
        assert!(
            fixture(suite).exists(),
            "expected-fail fixture path should exist: {}",
            suite
        );
    }

    let invalid_pragma = fixture("core/invalid-pragma.t/Invalid.nuscr");
    let err = ruscr::parse_file(invalid_pragma.to_str().unwrap()).unwrap_err();
    assert!(matches!(err, ruscr::Error::Unsupported(feature) if feature == "pragmas"));

    let protobuf_input = fixture("core/protobuf.t/TwoBuyer.nuscr");
    let output = Command::new(env!("CARGO_BIN_EXE_ruscr"))
        .arg("--show-global-type-protobuf")
        .arg("TwoBuyer")
        .arg(protobuf_input)
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Unsupported feature"));
}

#[test]
fn parses_and_enumerates_two_buyer() {
    let file = fixture("core/mpstk.t/TwoBuyer.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    assert_eq!(
        ruscr::enumerate(&module),
        vec![
            ("TwoBuyer".to_string(), "B1".to_string()),
            ("TwoBuyer".to_string(), "B2".to_string()),
            ("TwoBuyer".to_string(), "S".to_string())
        ]
    );
}

#[test]
fn projects_two_buyer_b1() {
    let file = fixture("core/mpstk.t/TwoBuyer.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let local = ruscr::project_role(&module, "TwoBuyer", "B1").unwrap();
    let shown = ruscr::format::show_local(&local);
    assert!(shown.contains("s(string) to S;"));
    assert!(shown.contains("b1(int) from S;"));
    assert!(shown.contains("bi2(int) to B2;"));
}

#[test]
fn recursive_do_expands_to_recursion() {
    let file = fixture("core/do-conversion.t/Do1.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let global = ruscr::get_global_type(&module, "PingPong").unwrap();
    let shown = ruscr::format::show_global(&global);
    assert!(shown.contains("rec __PingPong_A_B"));
    assert!(shown.contains("continue __PingPong_A_B;"));
}

#[test]
fn recursive_do_with_permuted_roles_terminates() {
    let file = fixture("core/do-conversion.t/Do2.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let global = ruscr::get_global_type(&module, "PingPong").unwrap();
    let shown = ruscr::format::show_global(&global);
    assert!(shown.contains("Ping() from B to A;"));
    assert!(shown.contains("Pong() from A to B;"));
    assert!(shown.contains("continue __PingPong_A_B;"));
}

#[test]
fn multi_role_recursive_do_with_nested_cycles_terminates() {
    let file = fixture("core/do-conversion.t/DoMulti3.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let global = ruscr::get_global_type(&module, "Pass").unwrap();
    let shown = ruscr::format::show_global(&global);
    assert!(shown.contains("rec __Pass_A_B_C"));
    assert!(shown.contains("rec __Pass_B_C_A"));
    assert!(shown.contains("rec __Pass_C_A_B"));
}

#[test]
fn projection_merge_allows_matching_send_prefixes() {
    let file = fixture("core/merge-send.t/Merging.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let local = ruscr::project_role(&module, "Merging", "C").unwrap();
    let shown = ruscr::format::show_local(&local);
    assert!(shown.contains("Baz() to B;"));
    assert!(shown.contains("Foo() from B;"));
    assert!(shown.contains("Bar() from B;"));
}

#[test]
fn duplicate_choice_labels_are_errors() {
    let file = fixture("core/ambiguous-choice.t/Ambiguous.nuscr");
    let module = ruscr::parse_file(file.to_str().unwrap()).unwrap();
    let err = ruscr::project_role(&module, "Ambiguous", "A").unwrap_err();
    assert!(matches!(err, ruscr::Error::DuplicateLabel(_)));
}

#[test]
fn nested_duplicate_choice_labels_are_errors() {
    let module = ruscr::parse_string(
        r#"
        global protocol Nested(role A, role B) {
          choice at A {
            choice at A {
              m() from A to B;
            } or {
              m() from A to B;
            }
          } or {
            n() from A to B;
          }
        }
        "#,
    )
    .unwrap();
    let err = ruscr::project_role(&module, "Nested", "A").unwrap_err();
    assert!(matches!(err, ruscr::Error::DuplicateLabel(label) if label == "m"));
}

#[test]
fn refinement_syntax_is_explicitly_unsupported() {
    let input = r#"
      global protocol P(role A, role B) {
        rec X [count<A>: int = 0] {
          Num(curr: int) from A to B;
          continue X [count + 1];
        }
      }
    "#;
    let err = ruscr::parse_string(input).unwrap_err();
    assert!(matches!(err, ruscr::Error::Unsupported(feature) if feature == "refinement types"));
}

#[test]
fn cli_enum_smoke_test() {
    let file = fixture("core/mpstk.t/TwoBuyer.nuscr");
    let output = Command::new(env!("CARGO_BIN_EXE_ruscr"))
        .arg("--enum")
        .arg(file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("B1@TwoBuyer"));
    assert!(stdout.contains("B2@TwoBuyer"));
    assert!(stdout.contains("S@TwoBuyer"));
}

fn fixture(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("cram-tests")
        .join(relative)
}
