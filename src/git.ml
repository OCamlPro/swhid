module SHA1 = struct
  let digest_string_to_hex v =
    Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string v
end

include Swhid_git.Make (SHA1)
