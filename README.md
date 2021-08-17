# swhid [![Actions Status](https://github.com/ocamlpro/swhid/workflows/build/badge.svg)](https://github.com/ocamlpro/swhid/actions) [![coverage percentage](https://raw.githubusercontent.com/ocamlpro/swhids/gh-pages/coverage/badge.svg)](https://ocamlpro.github.io/swhid/coverage/)

[swhid] is an [OCaml] library to work with [persistent identifiers] found in [Software Heritage], also known as swhid.

## Quickstart

```ocaml
let id = "swh:1:rev:db21f0afdb54c16b265754ca599869fda0ca4bfc"
let id = Swhid.Parse.from_string id
let url =
  match id with
  | Error _e -> None
  | Ok id ->
    match Swhid.Download.revision (Swhid.Lang.get_object_id id) with
    | Ok url -> Some url
    | Error _e -> None

let () =
  match url with
  | None -> Format.eprintf "can't get download URL@."
  | Some url -> Format.printf "you can download the revision at the URL: `%s`@." url
```

For more, have a look at the [example] folder or at the [documentation].

## About

- [LICENSE]
- [CHANGELOG]

[CHANGELOG]: ./CHANGES.md
[example]: ./example/
[LICENSE]: ./LICENSE.md

[documentation]: https://ocamlpro.github.io/swhid/api/swhid/
[OCaml]: https://ocaml.org
[persistent identifiers]: https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html
[Software Heritage]: https://www.softwareheritage.org
[swhid]: https://ocamlpro.github.io/swhid/
