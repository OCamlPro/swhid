(** Module providing functions to get a download url from a swhid. *)

(** The base URL of the software heritage instance used, defaults to
    [https://archive.softwareheritage.org]. *)
val instance : string ref

(** For a given content identifier, compute an URL from which the content can be
    downloaded. *)
val content : Swhid_core.Object.t -> (string, string) result

(** Same as [content] but expects an object identifier hash directly. *)
val content_unsafe :
  hash_type:string -> Swhid_core.Object.Hash.t -> (string, string) result

(** For a given directory identifier, compute an URL from which the directory
    can be downloaded. *)
val directory : Swhid_core.Object.t -> (string, string) result

(** Same as [directory] but expects an object identifier hash directly. *)
val directory_unsafe : Swhid_core.Object.Hash.t -> (string, string) result

(** For a given revision identifier, compute an URL from which the revision can
    be downloaded. *)
val revision : Swhid_core.Object.t -> (string, string) result

(** Same as [revision] but expects an object identifier hash directly. *)
val revision_unsafe : Swhid_core.Object.Hash.t -> (string, string) result

(** For a given release identifier, compute an URL from which the release can be
    downloaded. *)
val release : Swhid_core.Object.t -> (string, string) result

(** Same as [release] but expects an object identifier hash directly. *)
val release_unsafe : Swhid_core.Object.Hash.t -> (string, string) result

(** For a given snapshot identifier, compute a list of URL from which the
    snapshot's branches can be downloaded. *)
val snapshot :
  Swhid_core.Object.t -> ((string, string) result list, string) result

(** Same as [snapshot] but expects an object identifier hash directly. *)
val snapshot_unsafe :
  Swhid_core.Object.Hash.t -> ((string, string) result list, string) result

(** For any object identifier, compute a list of URLs from which the object can
    be downloaded. For all kind of object, the list should contain a single URL
    except for snapshot objects which may lead to a list of many URLs (one URL
    per branch). In the snapshot branch, if a single error is encountered, then
    the result will be an [Error] type with the list of all errors, and no URL
    is returned (even if we succeeded to compute some of them).*)
val any : Swhid_core.Object.t -> (string list, string list) result
