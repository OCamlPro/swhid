open Swhid_common

include Fcompute.C (struct
    let object_to_swhid =
      let module G = Git.G (struct let compute s = Digestif.SHA1.(to_hex (digest_string s)) end) in
      G.object_to_swhid
  end)

