let () =

  if Array.length Sys.argv <> 2 then Swhids.Utils.error (Format.sprintf "usage: %s <filename>" Sys.argv.(0));

  let filename = Sys.argv.(1) in

  let content =
    let chan = open_in filename in
    let buff = Buffer.create 512 in
    begin try while true do
        Buffer.add_string buff (input_line chan);
        Buffer.add_string buff "\n"
      done;
      with _ -> ()
    end;
    Buffer.contents buff
  in

  let swhid = Swhids.Compute.of_content content  in

  Format.printf "swhid: `%a`@." Swhids.Pp.identifier swhid
