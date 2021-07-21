%{
open Lang

  %}

%token EOF

%start <Lang.identifier> identifier

%%
  let identifier_core :=
      | { (1, Snapshot, [||]) }

let qualifiers :=
    | { [] }

let identifier :=
    | ~ = identifier_core; ~ = qualifiers; EOF; { identifier_core, qualifiers }
                                              | ~ = identifier_core; _blah = identifier_core; ~ = qualifiers; EOF; { identifier_core, qualifiers }
