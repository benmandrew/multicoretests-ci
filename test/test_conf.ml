module Lib = Multicoretests_ci_lib
module OV = Ocaml_version
module OVR = OV.Releases
module DD = Dockerfile_opam.Distro

let extract { Lib.Conf.Platform.distro; ocaml_version; arch; _ } =
  (distro, OV.with_just_major_and_minor ocaml_version, arch)

let eq a b = a = b
let distro_arch_eq (d0, a0) (d1, _, a1) = d0 = d1 && a0 = a1

let to_string (distro, ov, arch) =
  Printf.sprintf "%s-%s-%s" distro (OV.to_string ov) (OV.string_of_arch arch)

let to_string_no_ov (distro, arch) =
  Printf.sprintf "%s-[any]-%s" distro (OV.string_of_arch arch)

let to_tag d = DD.((resolve_alias d :> t) |> tag_of_distro)

let test_platforms () =
  let platforms = Lib.Conf.platforms () |> List.map extract in
  List.iter (fun p -> Printf.printf "%s\n" (to_string p)) platforms;
  let exists =
    [
      (true, (to_tag (`Debian `V11), OVR.v5_0, `Aarch64));
      (true, (to_tag (`Debian `V11), OVR.v5_1, `Aarch64));
      (true, (to_tag (`Debian `V11), OVR.v5_2, `Aarch64));
    ]
    |> List.map (fun (a, (b, ov, c)) ->
           (a, (b, OV.with_just_major_and_minor ov, c)))
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool) (to_string p) expect (List.exists (eq p) platforms))
    exists

let test_macos_platforms () =
  let platforms = Lib.Conf.platforms () |> List.map extract in
  let exists =
    [
      (true, ("macos-homebrew", OVR.v5_0, `Aarch64));
      (true, ("macos-homebrew", OVR.v5_1, `Aarch64));
      (true, ("macos-homebrew", OVR.v5_2, `Aarch64));
    ]
    |> List.map (fun (a, (b, ov, c)) ->
           (a, (b, OV.with_just_major_and_minor ov, c)))
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool) (to_string p) expect (List.exists (eq p) platforms))
    exists;
  (* Test that macos doesn't occur with these arches under any OCaml version *)
  let exists =
    [
      (false, ("macos-homebrew", `X86_64));
      (false, ("macos-homebrew", `S390x));
      (false, ("macos-homebrew", `Ppc64le));
      (false, ("macos-homebrew", `I386));
      (false, ("macos-homebrew", `Riscv64));
      (false, ("macos-homebrew", `Aarch32));
    ]
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool)
        (to_string_no_ov p) expect
        (List.exists (distro_arch_eq p) platforms))
    exists

let test_distro_arches () =
  let platforms = Lib.Conf.platforms () |> List.map extract in
  (* Test that these distros don't occur with these arches under any OCaml version *)
  let exists =
    [
      (false, (to_tag (`CentOS `Latest), `X86_64));
      (false, (to_tag (`OracleLinux `Latest), `X86_64));
      (false, (to_tag (`Debian `Stable), `Riscv64));
      (false, (to_tag (`Debian `Stable), `X86_64));
    ]
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool)
        (to_string_no_ov p) expect
        (List.exists (distro_arch_eq p) platforms))
    exists

let tests =
  [
    Alcotest.test_case "test_platforms" `Quick test_platforms;
    Alcotest.test_case "test_macos_platforms" `Quick test_macos_platforms;
    Alcotest.test_case "test_distro_arches" `Quick test_distro_arches;
  ]
