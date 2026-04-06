let new_random_uuid_v4 () =
  let random = String.to_bytes (Mirage_crypto_rng.generate 16) in
  Uuidm.(v4 random)
