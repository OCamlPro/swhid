include Swhid_compute.Make (struct
    let digest_string_to_hex v =
      Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string v
  end)
    (struct
      let contents name =
        let name = Fpath.v name in
        match Bos.OS.Dir.contents ~dotfiles:true ~rel:false name with
        | Ok contents -> Some (List.map Fpath.to_string contents)
        | Error _e -> None

      let typ name =
        let name = Fpath.v name in
        match Bos.OS.File.exists name with
        | Ok true -> Some "file"
        | Ok false | Error _ -> (
            match Bos.OS.Dir.exists name with
            | Ok true -> Some "dir"
            | Ok false | Error _ -> None )

      let read_file name =
        let name = Fpath.v name in
        match Bos.OS.File.read name with
        | Ok content -> Some content
        | _ -> None

      let permissions name =
        let name = Fpath.v name in
        match Bos.OS.Path.stat name with
        | Ok stat -> begin
            match stat.st_kind with
            | S_LNK -> Some 0o120000 (* symlinks *)
            | S_DIR -> Some 0o040000 (* directories *)
            | S_REG ->
              if Bos.OS.File.is_executable name then
                Some 0o100755 (* executable files *)
              else Some 0o100644 (* normal files *)
            | S_CHR | S_BLK | S_FIFO | S_SOCK -> None
          end
        | Error _e -> None

      let base name =
        let name = Fpath.v name in
        let name = Fpath.normalize name in
        let name = Fpath.base name in
        Fpath.to_string name
    end)
(** @inline *)
