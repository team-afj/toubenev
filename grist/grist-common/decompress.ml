(* This generated code has not been reviewed *)

let bigstring_length = Bigarray.Array1.dim

let bigstring_to_string buffer ~len =
  Bytes.init len (fun idx -> Bigarray.Array1.get buffer idx) |> Bytes.to_string

let make_refill input =
  let position = ref 0 in
  fun buffer ->
    let available = String.length input - !position in
    let len = min available (bigstring_length buffer) in
    for idx = 0 to len - 1 do
      Bigarray.Array1.set buffer idx input.[!position + idx]
    done;
    position := !position + len;
    len

let make_flush output buffer len =
  Buffer.add_string output (bigstring_to_string buffer ~len)

let compress_string ?level ?filename ?comment ?(mtime = 0l) input =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let output = Buffer.create (String.length input) in
  let refill = make_refill input in
  let flush = make_flush output in
  let configuration = Gz.Higher.configuration Gz.Unix Fun.id in
  Gz.Higher.compress ?level ?filename ?comment ~w ~q ~refill ~flush mtime
    configuration i o;
  Buffer.contents output

let decompress_string input =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let output = Buffer.create (String.length input) in
  let refill = make_refill input in
  let flush = make_flush output in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _metadata -> Ok (Buffer.contents output)
  | Error _ as error -> error
