%{
open Lang

  %}

%token EOF
%token COLUM
%token SEMICOL
%token EQUAL
%token DASH
%token<string> STR
%token<string> URL
%token<int> INT

%start <Lang.identifier> identifier

%%
  let identifier_core :=
      | swh = STR; COLUM; scheme_v = INT; COLUM; obj_t = STR; COLUM; hash = STR; {

    if swh <> "swh" then
      failwith "Prefix incorrect"

    else (
      let obj_id = String.lowercase_ascii hash in

      String.iter (fun c ->
        match c with
        | 'a'..'f' | '0'..'9' -> ()
        | _ -> failwith "Object_ID is invalid"
      ) hash;

      if String.length obj_id = 40 then
        let obj_array = Array.init 40 (String.get obj_id) in
        match Lang.object_id_of_string obj_t with
        | Some obj ->
          (scheme_v, obj, obj_array)
        | None ->
          failwith "Object_type invalid"
      else
        failwith "Object_ID has wrong length")
  }

let fragment_qualifier :=
    | lines = STR; EQUAL; l1 = INT; l2 = option(preceded(DASH, INT));
  {
    if String.equal lines "lines" then
      (l1, l2)
    else
      failwith "Fragment_qualifier non-compliant"
  }

(* TODO
 *
 * I. Parsing
 * ORIGIN URL_ESCAPED
 * Make compliant with (* RFC 3987 IRI *)
 *
 * PATH ABSOLUTE_ESCAPED
 * Make compliant with (* RFC 3987 absolute path *)
 *
 * II. Error handling
 * Make custom catchable errout
 *
 * *)
let context_qualifier :=
    | ctx_path = STR; EQUAL; pathabs = STR ;
  {
    if String.equal ctx_path "path" then
      (* Here insert RFC 3987 IRI [url_or_path] variable compliance *)
      Path pathabs
    else
      failwith "Context_qualifier absolute path wrong"
  }

| ctx_origin = STR; EQUAL; url = URL ;
  {
    if String.equal ctx_origin "origin" then
      (* Here insert RFC 3987 absolute  path [url_or_path] variable compliance *)
      Origin url
    else
      failwith "Context_qualifier origin url"
  }
| ctx_visit_or_anchor = STR; EQUAL; ~ = identifier_core ;
  {
    if String.equal ctx_visit_or_anchor "visit" then
      Visit identifier_core
    else if String.equal ctx_visit_or_anchor "anchor" then
      Anchor identifier_core
    else
      failwith "Context_qualifier visit/anchor incorrect"
  }

let qualifier :=
    |  ~ = context_qualifier ; <Context>
  | ~ = fragment_qualifier ; <Fragment>

  let qualifiers :=
      |  ~ = list(preceded(SEMICOL,qualifier)); <>

      let identifier :=
          | ~ = identifier_core; ~ = qualifiers; EOF; { identifier_core, qualifiers }
