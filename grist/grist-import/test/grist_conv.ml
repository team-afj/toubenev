open Grist_import

let usage () =
	prerr_endline
		"Usage: grist_conv <task_type|lieu|benevole|quete> < input.json > output.json";
	exit 2

let recode_by_kind kind input =
	match kind with
	| "task_type" -> Jsont_bytesrw.recode_string' Task_type.jsont input
	| "lieu" -> Jsont_bytesrw.recode_string' Lieu.jsont input
	| "benevole" -> Jsont_bytesrw.recode_string' Benevole.jsont input
	| "quete" -> Jsont_bytesrw.recode_string' Quete.jsont input
	| _ -> usage ()

let fail_with_jsont_error err =
	Format.kasprintf
		(fun msg ->
			prerr_endline msg;
			exit 1)
		"%a" Jsont.Error.pp err

let () =
	let kind = if Array.length Sys.argv = 2 then Sys.argv.(1) else usage () in
	let input = In_channel.input_all In_channel.stdin in
	match recode_by_kind kind input with
	| Ok output ->
			Out_channel.output_string Out_channel.stdout output;
			Out_channel.output_char Out_channel.stdout '\n'
	| Error e -> fail_with_jsont_error e
