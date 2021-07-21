let () =

  if Array.length Sys.argv <> 2 then Swhids.Utils.error (Format.sprintf "usage: %s" Sys.argv.(0));

  let swhid = Sys.argv.(1) in

  let swhid = Swhids.Parser.from_string swhid in

  Format.printf "swhid: `%a`@." Swhids.Pp.identifier swhid
