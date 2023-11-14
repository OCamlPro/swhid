(** A module providing various functions to compute the swhid of a given object.
    Supported objects are [content], [directory], [release], [revision] and
    [snapshot]. The origins and visits objects are not supported. *)

(** [content_identifier s] computes the swhid for the [s] content. [s] is the
    raw content of a file as a [string].

    E.g. [content_identifier "_build\n"] is the swhid of this library's
    [.gitignore] file. *)
val content_identifier : string -> (Swhid_core.Object.t, string) result

(** Same as [content_identifier] but reads the content from a file. *)
val content_identifier_from_file :
  string -> (Swhid_core.Object.t, string) result

(** [directory_identifier_deep] compute the swhid for a given directory name, it
    uses the various functions provided in the [OS] module parameter to list
    directory contents, get file permissions and read file contents.*)
val directory_identifier_deep : string -> (Swhid_core.Object.t, string) result

(** The type for dates, needed to compute releases and revisions identifiers. *)
type date =
  { timestamp : Int64.t
  ; tz_offset : int
  ; negative_utc : bool
  }

(** [release_identifier target target_kind name ~author date ~message] computes
    the swhid for a release object pointing to an object of type [target_kind]
    whose identifier is [target], the release having [~name], [~author] and has
    been published on [date] with the release [~message]. *)
val release_identifier :
     Swhid_core.Object.Hash.t
  -> Swhid_core.Object.Kind.t
  -> name:string
  -> author:string option
  -> date option
  -> message:string option
  -> (Swhid_core.Object.t, string) result

(** [revision dir parents ~author ~author_date ~committer ~committer_date extra_headers message]
    computes the swhid for a revision object whose directory has id [dir] and
    whose parents has ids [parents] which was authored by [~author] on
    [~author_date] and committed by [~committer] on [~committer_date] with extra
    headers [extra_headers] and message [message]. *)
val revision_identifier :
     Swhid_core.Object.Hash.t
  -> Swhid_core.Object.Hash.t list
  -> author:string
  -> author_date:date option
  -> committer:string
  -> committer_date:date option
  -> (string * string) array
  -> message:string option
  -> (Swhid_core.Object.t, string) result

(** [snapshot_identifier branches] computes the swhid of the snapshot made of
    branches [branches] where [branches] is a list of branch elements. Each
    branch is of the form [name, target] where [name] is the name of the branch
    and where [target] is a pair made of the identifier of the branch and its
    type. *)
val snapshot_identifier :
     (string * (string * string) option) list
  -> (Swhid_core.Object.t, string) result
