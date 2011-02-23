open Ocamlbuild_plugin

let singletons = List.map (fun x -> [x])

let ml_name =
  let default_char = '_' in
  let name_char = function
    | ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\'') as c -> c
    | _ -> default_char in
  let name_char0 = function
    | ('_' | 'a'..'z') as c -> c
    | 'A'..'Z' as c -> Char.lowercase c
    | _ -> default_char in
  fun s ->
    let s = String.copy s in
    for i = 0 to String.length s - 1 do
      s.[i] <- (if i = 0 then name_char0 else name_char) s.[i];
    done;
    s

let _ = dispatch begin function
  | After_rules ->

      (*
	A .of file should contain a list of filenames
	It produces a .ml file containing these files as string values
      *)
      rule "%.of -> %.ml"
	~prod:"%.ml"
	~dep:"%.of"
	begin fun env build ->
	  let dir = Filename.dirname & env "%.of" in
	  let contents =
	    env "%.of"
            |> string_list_of_file
	    |> List.map ((/) dir)
	    |> List.map Ocamlbuild_pack.Pathname.normalize
	    |> singletons
	    |> build
	    |> List.map (fun file ->
			   let file = Outcome.good file in
			   Printf.sprintf "let %s = %S\n" (ml_name & Filename.basename file) (read_file file)
			) in
	  Echo(contents, env "%.ml")
	end;
  | _ -> ()
end
