# swhid [![Actions Status](https://github.com/ocamlpro/swhid/workflows/build/badge.svg)](https://github.com/ocamlpro/swhid/actions) [![coverage percentage](https://raw.githubusercontent.com/ocamlpro/swhid/gh-pages/coverage/badge.svg)](https://ocamlpro.github.io/swhid/coverage/)

[swhid] is an [OCaml] executable and library to work with [persistent identifiers] found in [Software Heritage], also known as swhid. It provides:

* a parser and a printer for swhid (`Swhid_core.Object` module)
* functions to compute a swhid for a given object of any kind : content, directory, revision, release or snapshot (`Swhid.Compute`)
* functions to query the swh API in order to get an URL from which you can download a given object (`Swhid.Download`)

## Installation

`swhid` can be installed with [opam]:

```sh
opam install swhid
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, consult the [opam file] for build instructions.

## Quickstart

### Executable

```shell-session
$ swhid-compute content-string "let rec f x = f x\n"
swh:1:cnt:032873006d15555befc1b39017a342580b01f1be
$ swhid compute content-file LICENSE.md
swh:1:cnt:6ea040672f7995fb6d850519df1b1a5408b243a0
$ swhid compute directory src
swh:1:dir:3e89af4dd5b7110d511867be65d25dd51c198ad4
$ swhid download swh:1:dir:3eb35765545dcca55cb5a7f30ab31d794cc36c95
https://archive.softwareheritage.org/api/1/vault/flat/swh:1:dir:3eb35765545dcca55cb5a7f30ab31d794cc36c95/raw/
```

### Library

```ocaml
let id = "swh:1:rev:db21f0afdb54c16b265754ca599869fda0ca4bfc"

let url =
  match Swhid_core.Object.of_string id with
  | Error e -> Error e
  | Ok id -> Swhid.Download.revision id

let () =
  match url with
  | Error e -> Format.eprintf "can't get download URL: `%s`@." e
  | Ok url ->
    Format.printf "you can download the revision at the URL: `%s`@." url
```

For more, have a look at the [example] folder, at the [documentation] or at the [test suite].

## About

- [LICENSE]
- [CHANGELOG]

[CHANGELOG]: ./CHANGES.md
[example]: ./example/
[LICENSE]: ./LICENSE.md
[opam file]: ./swhid.opam
[test suite]: ./test/

[documentation]: https://ocamlpro.github.io/swhid/api/swhid/
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[OCaml]: https://ocaml.org
[opam]: https://opam.ocaml.org/
[persistent identifiers]: https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html
[Software Heritage]: https://www.softwareheritage.org
[swhid]: https://ocamlpro.github.io/swhid/
