(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Unix
open Ast
open Common
open Type
open Gencommon
open Gencommon.SourceWriter
open Printf
open Option
open ExtString
module SS = Set.Make(String)

let is_boxed_type t = match follow t with
	| TInst ({ cl_path = (["swift";"lang"], "Boolean") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Double") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Integer") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Byte") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Short") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Character") }, [])
	| TInst ({ cl_path = (["swift";"lang"], "Float") }, []) -> true
	| TAbstract ({ a_path = (["swift";"lang"], "Boolean") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Double") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Integer") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Byte") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Short") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Character") }, [])
	| TAbstract ({ a_path = (["swift";"lang"], "Float") }, []) -> true
	| _ -> false

let unboxed_type gen t tbyte tshort tchar tfloat = match follow t with
	| TInst ({ cl_path = (["swift";"lang"], "Boolean") }, []) -> gen.gcon.basic.tbool
	| TInst ({ cl_path = (["swift";"lang"], "Double") }, []) -> gen.gcon.basic.tfloat
	| TInst ({ cl_path = (["swift";"lang"], "Integer") }, []) -> gen.gcon.basic.tint
	| TInst ({ cl_path = (["swift";"lang"], "Byte") }, []) -> tbyte
	| TInst ({ cl_path = (["swift";"lang"], "Short") }, []) -> tshort
	| TInst ({ cl_path = (["swift";"lang"], "Character") }, []) -> tchar
	| TInst ({ cl_path = (["swift";"lang"], "Float") }, []) -> tfloat
	| TAbstract ({ a_path = (["swift";"lang"], "Boolean") }, []) -> gen.gcon.basic.tbool
	| TAbstract ({ a_path = (["swift";"lang"], "Double") }, []) -> gen.gcon.basic.tfloat
	| TAbstract ({ a_path = (["swift";"lang"], "Integer") }, []) -> gen.gcon.basic.tint
	| TAbstract ({ a_path = (["swift";"lang"], "Byte") }, []) -> tbyte
	| TAbstract ({ a_path = (["swift";"lang"], "Short") }, []) -> tshort
	| TAbstract ({ a_path = (["swift";"lang"], "Character") }, []) -> tchar
	| TAbstract ({ a_path = (["swift";"lang"], "Float") }, []) -> tfloat
	| _ -> assert false

let rec t_has_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) -> List.exists t_has_type_param params
	| TFun(f,ret) -> t_has_type_param ret || List.exists (fun (_,_,t) -> t_has_type_param t) f
	| _ -> false

let is_dynamic gen t =
	match follow (gen.greal_type t) with
		| TDynamic _ -> true
		| _ -> false

let is_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, _) -> true
	| _ -> false

let rec t_has_type_param_shallow last t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) when not last -> List.exists (t_has_type_param_shallow true) params
	| TFun(f,ret) when not last -> t_has_type_param_shallow true ret	|| List.exists (fun (_,_,t) -> t_has_type_param_shallow true t) f
	| _ -> false

let rec replace_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> t_dynamic
	| TEnum(e, params) -> TEnum(e, List.map replace_type_param params)
	| TAbstract(a, params) -> TAbstract(a, List.map replace_type_param params)
	| TInst(cl, params) -> TInst(cl, List.map replace_type_param params)
	| _ -> t

let is_swift_basic_type t =
	match follow t with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TInst( { cl_path = (["haxe"], "Int64") }, [] )
		| TAbstract( { a_path = ([], "Single") }, [] )
		| TAbstract( { a_path = (["swift"], ("Int8" | "Int16" | "Char16" | "Int64")) }, [] )
		| TAbstract( { a_path =	([], "Int") }, [] )
		| TAbstract( { a_path =	([], "Float") }, [] )
		| TAbstract( { a_path =	([], "Bool") }, [] ) ->
			true
		| _ -> false

let is_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let like_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[])
		| TAbstract ({ a_path = (["swift";"lang"],"Boolean") },[])
		| TInst ({ cl_path = (["swift";"lang"],"Boolean") },[]) ->
			true
		| _ -> false

let is_int_float gen t =
	match follow (gen.greal_type t) with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TAbstract( { a_path =	([], "Int") }, [] )
		| TAbstract( { a_path =	([], "Float") }, [] ) ->
			true
		| (TAbstract _ as t) when like_float t && not (like_i64 t)-> true
		| _ -> false

let parse_explicit_iface =
	let regex = Str.regexp "\\." in
	let parse_explicit_iface str =
		let split = Str.split regex str in
		let rec get_iface split pack =
			match split with
				| clname :: fn_name :: [] -> fn_name, (List.rev pack, clname)
				| pack_piece :: tl -> get_iface tl (pack_piece :: pack)
				| _ -> assert false
		in
		get_iface split []
	in parse_explicit_iface

let is_string t =
	match follow t with
		| TInst( { cl_path = ([], "String") }, [] ) -> true
		| _ -> false

let is_cl t = match follow t with
	| TInst({ cl_path = ["swift";"lang"],"Class" },_)
	| TAbstract({ a_path = [], ("Class"|"Enum") },_) -> true
	| TAnon(a) when is_some (anon_class t) -> true
	| _ -> false

let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "swift"
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved swift words *)
let reserved = let res = Hashtbl.create 120 in
	List.iter (fun lst -> Hashtbl.add res lst ("_" ^ lst)) ["abstract"; "assert"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class";
		"const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extends"; "final";
		"false"; "finally"; "float"; "for"; "goto"; "if"; "implements"; "import"; "instanceof"; "int";
		"interface"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "short";
		"static"; "strictfp"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws"; "transient"; "true"; "try";
		"void"; "volatile"; "while"; ];
	res

let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
	match meta with
		| [] -> cl_type,cl_access,cl_modifiers
		(*| (Meta.Struct,[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers*)
		| (Meta.Protected,[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
		| (Meta.Internal,[],_) :: meta -> get_class_modifiers meta cl_type "" cl_modifiers
		(* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
		| (Meta.Static,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
		| (Meta.Final,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("final" :: cl_modifiers)
		| _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
	match meta with
		| [] -> access,modifiers
		| (Meta.Protected,[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
		| (Meta.Internal,[],_) :: meta -> get_fun_modifiers meta "" modifiers
		| (Meta.ReadOnly,[],_) :: meta -> get_fun_modifiers meta access ("final" :: modifiers)
		(*| (Meta.Unsafe,[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)*)
		| (Meta.Volatile,[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
		| (Meta.Transient,[],_) :: meta -> get_fun_modifiers meta access ("transient" :: modifiers)
		| (Meta.Native,[],_) :: meta -> get_fun_modifiers meta access ("native" :: modifiers)
		| _ :: meta -> get_fun_modifiers meta access modifiers

(* this was the way I found to pass the generator context to be accessible across all functions here *)
(* so 'configure' is almost 'top-level' and will have all functions needed to make this work *)
let configure gen =
	let native_arr_cl = get_cl ( get_type gen (["swift"], "NativeArray") ) in
	gen.gclasses.nativearray <- (fun t -> TInst(native_arr_cl,[t]));
	gen.gclasses.nativearray_type <- (function TInst(_,[t]) -> t | _ -> assert false);
	gen.gclasses.nativearray_len <- (fun e p -> mk_field_access gen e "length" p);

	let basic = gen.gcon.basic in

	(*let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in*)

	let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in
	let nulltdef = get_tdef (get_type gen ([],"Null")) in

	(*let string_ref = get_cl ( get_type gen (["haxe";"lang"], "StringRefl")) in*)

	(*let ti64 = match ( get_type gen (["StdTypes"], "Int64") ) with | TAbstractDecl a -> TAbstract(a,[]) | _ -> assert false in*)

	let has_tdynamic params =
		List.exists (fun e -> match run_follow gen e with | TDynamic _ -> true | _ -> false) params
	in

	(*
		The type parameters always need to be changed to their boxed counterparts
	*)
	let change_param_type md params =
		(*match md with
			| TClassDecl( { cl_path = (["swift"], "NativeArray") } ) -> params
			| TAbstractDecl { a_path=[],("Class" | "Enum") } | TClassDecl { cl_path = (["swift";"lang"],("Class"|"Enum")) } ->
				List.map (fun _ -> t_dynamic) params
			| _ ->
				match params with
					| [] -> []
					| _ ->
						if has_tdynamic params then List.map (fun _ -> t_dynamic) params else
							List.map (fun t ->
								let f_t = gen.gfollow#run_f t in
								match f_t  with
									| TAbstract ({ a_path = ([], "Bool") },[])
									| TAbstract ({ a_path = ([],"Float") },[])
									| TInst ({ cl_path = ["haxe"],"Int32" },[])
									| TInst ({ cl_path = ["haxe"],"Int64" },[])
									| TAbstract ({ a_path = ([],"Int") },[])
									| TType ({ t_path = ["swift"], "Int64" },[])
									| TAbstract ({ a_path = ["swift"], "Int64" },[])
									| TType ({ t_path = ["swift"],"Int8" },[])
									| TAbstract ({ a_path = ["swift"],"Int8" },[])
									| TType ({ t_path = ["swift"],"Int16" },[])
									| TAbstract ({ a_path = ["swift"],"Int16" },[])
									| TType ({ t_path = ["swift"],"Char16" },[])
									| TAbstract ({ a_path = ["swift"],"Char16" },[])
									| TType ({ t_path = [],"Single" },[])
									| TAbstract ({ a_path = [],"Single" },[]) ->
										TType(nulltdef, [f_t])
									(*| TType ({ t_path = [], "Null"*)
									| TInst (cl, ((_ :: _) as p)) when cl.cl_path <> (["swift"],"NativeArray") ->
										(* TInst(cl, List.map (fun _ -> t_dynamic) p) *)
										TInst(cl,p)
									| TEnum (e, ((_ :: _) as p)) ->
										TEnum(e, List.map (fun _ -> t_dynamic) p)
									| _ -> t
							)*) params
	in

	let change_clname name =
		String.map (function | '$' -> '.' | c -> c) name
	in
	let change_id name = try Hashtbl.find reserved name with | Not_found -> name in
	let rec change_ns ns = match ns with
		| [] -> ["haxe"; "root"]
		| _ -> List.map change_id ns
	in
	let change_field = change_id in

	let write_id w name = write w (change_id name) in

	let write_field w name = write w (change_field name) in

	gen.gfollow#add ~name:"follow_basic" (fun t -> match t with
			| TAbstract ({ a_path = ([], "Bool") },[])
			| TAbstract ({ a_path = ([], "Void") },[])
			| TAbstract ({ a_path = ([],"Float") },[])
			| TAbstract ({ a_path = ([],"Int") },[])
			| TInst( { cl_path = (["haxe"], "Int32") }, [] )
			| TInst( { cl_path = (["haxe"], "Int64") }, [] )
			| TType ({ t_path = ["swift"], "Int64" },[])
			| TAbstract ({ a_path = ["swift"], "Int64" },[])
			| TType ({ t_path = ["swift"],"Int8" },[])
			| TAbstract ({ a_path = ["swift"],"Int8" },[])
			| TType ({ t_path = ["swift"],"Int16" },[])
			| TAbstract ({ a_path = ["swift"],"Int16" },[])
			| TType ({ t_path = ["swift"],"Char16" },[])
			| TAbstract ({ a_path = ["swift"],"Char16" },[])
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) ->
					Some t
			| TType (({ t_path = [],"Null" } as tdef),[t2]) ->
					Some (TType(tdef,[gen.gfollow#run_f t2]))
			| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					Some (gen.gfollow#run_f ( Abstract.get_underlying_type a pl) )
			| TAbstract( { a_path = ([], "EnumValue") }, _ )
			| TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
			| _ -> None);

	let change_path path = (change_ns (fst path), change_clname (snd path)) in

	let path_s path meta = 
        match path with
			| (ns,clname) -> s_type_path (change_ns ns, change_clname clname)
	in

	(*let cl_cl = get_cl (get_type gen (["swift";"lang"],"Class")) in*)

	let rec real_type t =
		let t = gen.gfollow#run_f t in
		match t with
			| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				real_type (Abstract.get_underlying_type a pl)
			| TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
			| TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> t
			| TAbstract( { a_path = ([], "Class") }, p	)
			| TAbstract( { a_path = ([], "Enum") }, p  )
			| TInst( { cl_path = ([], "Class") }, p  )
			| TInst( { cl_path = ([], "Enum") }, p	) -> (*TInst(cl_cl,[t_dynamic])*) t_dynamic
			| TEnum(e,params) -> TEnum(e, List.map (fun _ -> t_dynamic) params)
			| TInst(c,params) when Meta.has Meta.Enum c.cl_meta ->
				TInst(c, List.map (fun _ -> t_dynamic) params)
			| TInst({ cl_kind = KExpr _ }, _) -> t_dynamic
			| TInst _ -> t
			| TType({ t_path = ([], "Null") }, [t]) when is_swift_basic_type (gen.gfollow#run_f t) -> t_dynamic
			| TType({ t_path = ([], "Null") }, [t]) ->
				(match follow t with
					| TInst( { cl_kind = KTypeParameter _ }, []) ->
							t_dynamic
							(* real_type t *)
					| _ -> real_type t
				)
			| TType _ | TAbstract _ -> t
			| TAnon (anon) ->  t
			| TFun _ -> (*TInst(fn_cl,[])*) t
			| _ -> t_dynamic
	in

	let scope = ref PMap.empty in
	let imports = ref [] in

	let clear_scope () =
		imports := [];
	in

	let add_scope name =
		scope := PMap.add name () !scope
	in

	let add_import pos path meta =
		let name = snd path in
		let rec loop = function
			| (pack, n) :: _ when name = n ->
					if path <> (pack,n) then
						gen.gcon.error ("This expression cannot be generated because " ^ path_s path meta ^ " is shadowed by the current scope and ") pos
			| _ :: tl ->
					loop tl
			| [] ->
					(* add import *)
					imports := path :: !imports
		in
		loop !imports
	in

	let path_s_import pos path meta = match path with
		| [], name when PMap.mem name !scope ->
				gen.gcon.error ("This expression cannot be generated because " ^ name ^ " is shadowed by the current scope") pos;
				name
		| pack1 :: _, name when PMap.mem pack1 !scope -> (* exists in scope *)
				add_import pos path meta;
				(* check if name exists in scope *)
				if PMap.mem name !scope then
					gen.gcon.error ("This expression cannot be generated because " ^ pack1 ^ " and " ^ name ^ " are both shadowed by the current scope") pos;
				name
		| _ -> path_s path meta
	in

	(*let is_dynamic t = match real_type t with
		| TMono _ | TDynamic _
		| TInst({ cl_kind = KTypeParameter _ }, _) -> true
		| TAnon anon ->
			(match !(anon.a_status) with
				| EnumStatics _ | Statics _ | AbstractStatics _ -> false
				| _ -> true
			)
		| _ -> false
	in*)

	let rec t_s pos t =
		match real_type t with
			(* basic types *)
			| TAbstract ({ a_path = ([], "Bool") },[]) -> "Bool"
			| TAbstract ({ a_path = ([], "Void") },[]) -> "Void"
			| TAbstract ({ a_path = ([],"Float") },[]) -> "Double"
			| TAbstract ({ a_path = ([],"Int") },[]) -> "Int"
			| TType ({ t_path = ["swift"], "Int64" },[])
			| TAbstract ({ a_path = ["swift"], "Int64" },[]) -> "Int64"
			| TType ({ t_path = ["swift"],"Int8" },[])
			| TAbstract ({ a_path = ["swift"],"Int8" },[]) -> "Int8"
			| TType ({ t_path = ["swift"],"Int16" },[])
			| TAbstract ({ a_path = ["swift"],"Int16" },[]) -> "Int16"
			| TType ({ t_path = ["swift"],"Char16" },[])
			| TAbstract ({ a_path = ["swift"],"Char16" },[]) -> "Char"
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) -> "Float"
			| TInst ({ cl_path = ["haxe"],"Int32" },[])
			| TAbstract ({ a_path = ["haxe"],"Int32" },[]) -> "Int32"
			| TInst ({ cl_path = ["haxe"],"Int64" },[])
			| TAbstract ({ a_path = ["haxe"],"Int64" },[]) -> "Int64"
            | TInst({ cl_path = (["haxe";"root"], "Array") }, [param]) 
            | TInst({ cl_path = (["swift"], "NativeArray") }, [param]) ->
				let rec check_t_s t =
					match real_type t with
						| TInst({ cl_path = (["swift"], "NativeArray") }, [param])
                        | TInst({ cl_path = (["haxe"; "root"], "Array") }, [param]) ->
                            check_t_s param
						| _ -> t_s pos (run_follow gen t)
				in
                "Array<" ^ (check_t_s param) ^ ">"

			(* end of basic types *)
			| TInst ({ cl_kind = KTypeParameter _; cl_path=p }, []) -> snd p
			| TAbstract ({ a_path = [], "Dynamic" },[]) ->
					"Any?"
			| TMono r -> (match !r with | None -> "Any?" | Some t -> t_s pos (run_follow gen t))
			| TInst ({ cl_path = [], "String" }, []) ->
                    "String?"
			| TAbstract ({ a_path = [], "Class" }, [p]) | TAbstract ({ a_path = [], "Enum" }, [p])
			| TInst ({ cl_path = [], "Class" }, [p]) | TInst ({ cl_path = [], "Enum" }, [p]) ->
                    "Any?"
			| TAbstract ({ a_path = [], "Class" }, _) | TAbstract ({ a_path = [], "Enum" }, _)
			| TInst ({ cl_path = [], "Class" }, _) | TInst ({ cl_path = [], "Enum" }, _) ->
					"Any?"
			| TEnum ({e_path = p; e_meta = meta}, _) ->
					path_s_import pos p meta
			| TInst (({cl_path = p; cl_meta = meta} as cl), _) when Meta.has Meta.Enum cl.cl_meta ->
					path_s_import pos p meta
			| TInst (({cl_path = p; cl_meta = meta} as cl), params) -> (path_param_s pos (TClassDecl cl) p params meta)
			| TType (({t_path = p; t_meta = meta} as t), params) -> (path_param_s pos (TTypeDecl t) p params meta)
			| TAnon (anon) ->
			    "Any?"	
			| TDynamic _ ->
				"Any?"
            | TFun (l,t) ->
		        "( " ^ String.concat ", " (List.map (fun (s,b,t) ->
			    (if b then "?" else "") ^ (if s = "" then "" else s ^ " : ") ^ t_s pos t) l) ^ " )" ^ " -> " ^ t_s pos t
			| _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"

	and param_t_s pos t =
		match run_follow gen t with
			| TAbstract ({ a_path = ([], "Bool") },[]) ->
					"Bool"
			| TAbstract ({ a_path = ([],"Float") },[]) ->
					"Double"
			| TAbstract ({ a_path = ([],"Int") },[]) ->
					"Int"
			| TType ({ t_path = ["swift"], "Int64" },[])
			| TAbstract ({ a_path = ["swift"], "Int64" },[]) 
			| TInst ({ cl_path = ["haxe"],"Int64" },[])
			| TAbstract ({ a_path = ["haxe"],"Int64" },[]) ->
					"Int64"	
			| TInst ({ cl_path = ["haxe"],"Int32" },[])
			| TAbstract ({ a_path = ["haxe"],"Int32" },[]) ->
					"Int32"
			| TType ({ t_path = ["swift"],"Int8" },[])
			| TAbstract ({ a_path = ["swift"],"Int8" },[]) ->
				    "Int8"	
			| TType ({ t_path = ["swift"],"Int16" },[])
			| TAbstract ({ a_path = ["swift"],"Int16" },[]) ->
				    "Int16"	
			| TType ({ t_path = ["swift"],"Char16" },[])
			| TAbstract ({ a_path = ["swift"],"Char16" },[]) ->
				    "Char"	
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) ->
					"Float"
			| TDynamic _ -> "Any?"
            | TType({ t_path = [],"String" },[]) -> 
                    "String"
            | TInst({ cl_path = (["haxe";"root"], "Array") }, [param]) ->
                param_t_s pos param
			| TInst (cl, params) -> t_s pos (TInst(cl, change_param_type (TClassDecl cl) params)) ^ "?"
 			| TType (cl, params) -> t_s pos (TType(cl, change_param_type (TTypeDecl cl) params)) 
			| TEnum (e, params) -> t_s pos (TEnum(e, change_param_type (TEnumDecl e) params))
			| _ -> t_s pos t

	and path_param_s pos md path params meta =
			match params with
				| [] -> path_s_import pos path meta
				| _ when has_tdynamic (change_param_type md params) -> path_s_import pos path meta
				| _ -> sprintf "%s<%s>" (path_s_import pos path meta) (String.concat ", " (List.map (fun t -> param_t_s pos t) (change_param_type md params)))
	in

	let rett_s pos t =
		match t with
			| TAbstract ({ a_path = ([], "Void") },[]) -> "Void"
			| _ -> t_s pos t
	in

	let high_surrogate c = (c lsr 10) + 0xD7C0 in
	let low_surrogate c = (c land 0x3FF) lor 0xDC00 in

	let escape ichar b =
		match ichar with
			| 92 (* \ *) -> Buffer.add_string b "\\\\"
			| 39 (* ' *) -> Buffer.add_string b "\\\'"
			| 34 -> Buffer.add_string b "\\\""
			| 13 (* \r *) -> Buffer.add_string b "\\r"
			| 10 (* \n *) -> Buffer.add_string b "\\n"
			| 9 (* \t *) -> Buffer.add_string b "\\t"
			| c when c < 32 || (c >= 127 && c <= 0xFFFF) -> Buffer.add_string b (Printf.sprintf "\\u%.4x" c)
			| c when c > 0xFFFF -> Buffer.add_string b (Printf.sprintf "\\u%.4x\\u%.4x" (high_surrogate c) (low_surrogate c))
			| c -> Buffer.add_char b (Char.chr c)
	in

	let escape s =
		let b = Buffer.create 0 in
		(try
			UTF8.validate s;
			UTF8.iter (fun c -> escape (UChar.code c) b) s
		with
			UTF8.Malformed_code ->
				String.iter (fun c -> escape (Char.code c) b) s
		);
		Buffer.contents b
	in

    (*let has_semicolon e =
		match e.eexpr with
			| TLocal { v_name = "__fallback__" }
			| TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt _) } ] ) -> false
			| TCall ({ eexpr = TLocal( { v_name = "__lock__" } ) }, _ ) -> false
			| TBlock _ | TFor _ | TSwitch _ | TTry _ | TIf _ -> false
			| TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
			| _ -> true
	in*)

	let in_value = ref false in

	let rec md_s pos md =
		let md = follow_module (gen.gfollow#run_f) md in
		match md with
			| TClassDecl (cl) ->
				t_s pos (TInst(cl,[]))
			| TEnumDecl (e) ->
				t_s pos (TEnum(e,[]))
			| TTypeDecl t ->
				t_s pos (TType(t, []))
			| TAbstractDecl a ->
				t_s pos (TAbstract(a, []))
	in

	(*let rec transform_nativearray_t t = match real_type t with
		| TInst( ({ cl_path = (["swift"], "NativeArray") } as narr), [t]) ->
			TInst(narr, [transform_nativearray_t t])
		| TInst(cl, params) -> TInst(cl, List.map (fun _ -> t_dynamic) params)
		| TEnum(e, params) -> TEnum(e, List.map (fun _ -> t_dynamic) params)
		| TType(t, params) -> TType(t, List.map (fun _ -> t_dynamic) params)
		| _ -> t
	in *)

	let rec extract_tparams params el =
		match el with
			| ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
				extract_tparams (tp.etype :: params) tl
			| _ -> (params, el)
	in

    (*let rec extract_tparams params el =
		match el with
			| ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
				extract_tparams (tp.etype :: params) tl
            | _ :: tl -> extract_tparams params tl
            | [] -> (params, el)
	in*)

	let line_directive =
		if Common.defined gen.gcon Define.RealPosition then
			fun w p -> ()
		else fun w p ->
			let cur_line = Lexer.get_error_line p in
			let file = Common.get_full_path p.pfile in
			print w "//line %d \"%s\"" cur_line (Ast.s_escape file); newline w
	in

	let extract_statements expr =
		let ret = ref [] in
		let rec loop expr = match expr.eexpr with
			| TUnop (Ast.Increment, _, _)
			| TUnop (Ast.Decrement, _, _)
			| TBinop (Ast.OpAssign, _, _)
			| TBinop (Ast.OpAssignOp _, _, _) ->
				ret := expr :: !ret
			| TConst _
			| TLocal _
			| TArray _
			| TBinop _
			| TField _
			| TEnumParameter _
			| TTypeExpr _
			| TObjectDecl _
			| TArrayDecl _
			| TCast _
			| TMeta _
			| TParenthesis _
			| TUnop _ ->
				Type.iter loop expr
			| TFunction _ -> () (* do not extract parameters from inside of it *)
			| _ ->
				ret := expr :: !ret
		in
		loop expr;
		(* [expr] *)
		List.rev !ret
	in

	let expr_s w e =
		in_value := false;
		let rec expr_s w e =
			let was_in_value = !in_value in
			in_value := true;
			match e.eexpr with
				| TConst c ->
					(match c with
						| TInt i32 ->
							print w "%ld" i32;
						| TFloat s ->
							write w s;
							(* fix for Int notation, which only fit in a Float *)
							(if not (String.contains s '.' || String.contains s 'e' || String.contains s 'E') then write w ".0");
						| TString s -> print w "\"%s\"" (escape s)
						| TBool b -> write w (if b then "true" else "false")
						| TNull ->
							(match real_type e.etype with
								| TAbstract( { a_path = (["swift"], "Int64") }, [] )
								| TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> write w "0 as Int64"
								| TInst( { cl_path = (["haxe"], "Int32") }, [] )
								| TAbstract ({ a_path = ([], "Int") },[]) -> expr_s w ({ e with eexpr = TConst(TInt Int32.zero) })
								| TAbstract ({ a_path = ([], "Float") },[]) -> expr_s w ({ e with eexpr = TConst(TFloat "0.0") })
								| TAbstract ({ a_path = ([], "Bool") },[]) -> write w "false"
								| TAbstract _ when like_int e.etype ->
									expr_s w (mk_cast e.etype { e with eexpr = TConst(TInt Int32.zero) })
								| TAbstract _ when like_float e.etype ->
									expr_s w (mk_cast e.etype { e with eexpr = TConst(TFloat "0.0") } )
								| t -> write w ("nil") )
						| TThis -> write w "this"
						| TSuper -> write w "super")
				| TLocal var ->
					write_id w var.v_name
				| TField(_, FEnum(en,ef)) ->
					let s = ef.ef_name in
					print w "%s!." (path_s_import e.epos en.e_path en.e_meta); write_field w s
				| TArray (e1, e2) ->
					expr_s w e1; write w "["; expr_s w e2; write w "]"
				| TBinop ((Ast.OpAssign as op), e1, e2)
				| TBinop ((Ast.OpAssignOp _ as op), e1, e2) ->
					expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
				| TBinop (op, e1, e2) ->
					write w "(";
					expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2;
					write w ")"
				| TField (e, FStatic(_, cf)) when Meta.has Meta.Native cf.cf_meta ->
					let rec loop meta = match meta with
						| (Meta.Native, [EConst (String s), _],_) :: _ ->
							expr_s w e; write w "."; write_field w s
						| _ :: tl -> loop tl
						| [] -> expr_s w e; write w "."; write_field w (cf.cf_name)
					in
					loop cf.cf_meta
				| TField (e, s) ->
                    let maybeExcl = function  
                        | FStatic(_,_) -> "."
                        | _ -> "!."
                    in 
					expr_s w e; write w (maybeExcl s); write_field w (field_name s)
				| TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int32") }) ->
					write w (path_s_import e.epos (["haxe"], "Int32") [])
				| TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int64") }) ->
					write w (path_s_import e.epos (["haxe"], "Int64") [])
				| TTypeExpr mt -> write w (md_s e.epos mt)
				| TParenthesis e ->
					write w "("; expr_s w e; write w ")"
				| TMeta (_,e) ->
					expr_s w e
				| TCall ({ eexpr = TLocal { v_name = "__array__" } }, el)
				| TCall ({ eexpr = TField(_, FStatic({ cl_path = (["swift"],"NativeArray") }, { cf_name = "make" })) }, el)
				| TArrayDecl el ->
					let et, el = extract_tparams [] el in
                    let foldFunc acc t = acc ^ (param_t_s e.epos t) in
                    let stringParams = List.fold_left foldFunc "" et in 
                    let isNotEmpty = (List.length et >= 1) in
                    if isNotEmpty then
                        (write w "Array<";
                        write w stringParams;
                        print w ">";
    					write w "(";
					    ignore (List.fold_left (fun acc e ->
					    	(if acc <> 0 then write w ", ");
					    	expr_s w e;
					    	acc + 1
					    ) 0 el);
					    write w ")")
                    else 
                        write w "[]"
				| TCall ({ eexpr = TLocal( { v_name = "__swift__" } ) }, [ { eexpr = TConst(TString(s)) } ] ) ->
					write w s
				| TCall ({ eexpr = TLocal( { v_name = "__swift__" } ) }, { eexpr = TConst(TString(s)) } :: tl ) ->
					Codegen.interpolate_code gen.gcon s tl (write w) (expr_s w) e.epos
				| TCall (e, el) ->
					let params, el = extract_tparams [] el in

					expr_s w e;

					(match params with
						| [] -> ()
						| params ->
							let md = match e.eexpr with
								| TField(ef, _) -> t_to_md (run_follow gen ef.etype)
								| _ -> assert false
							in
							write w "<";
							ignore (List.fold_left (fun acc t ->
								(if acc <> 0 then write w ", ");
								write w (param_t_s e.epos (change_param_type md t));
								acc + 1
							) 0 params);
							write w ">"
					);

					write w "(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TNew (({ cl_path = (["swift"], "NativeArray") } as cl), params, [ _ ]) ->
					let rec check_t_s t times =
						match real_type t with
							| TInst({ cl_path = (["swift"], "NativeArray") }, [param]) ->
								(check_t_s param (times+1))
							| _ ->
                                print w "Array<%s" (t_s e.epos t);
								print w ">";
								let rec loop i =
									if i <= 0 then () else (write w "[]"; loop (i-1))
								in
								loop (times - 1)
					in
					check_t_s (TInst(cl, params)) 0
				| TNew ({ cl_path = ([], "String") }, [], el) ->
					write w "String(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TNew ({ cl_kind = KTypeParameter _ } as cl, params, el) ->
						print w "nil /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */" (path_param_s e.epos (TClassDecl cl) cl.cl_path params cl.cl_meta)
				| TNew (cl, params, el) ->
					write w (path_param_s e.epos (TClassDecl cl) cl.cl_path params cl.cl_meta);
					write w "(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TUnop ((Ast.Increment as op), flag, e)
				| TUnop ((Ast.Decrement as op), flag, e) ->
					(match flag with
						| Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " " ); expr_s w e
						| Ast.Postfix -> expr_s w e; write w (Ast.s_unop op))
				| TUnop (op, flag, e) ->
					(match flag with
						| Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " (" ); expr_s w e; write w ") "
						| Ast.Postfix -> write w "("; expr_s w e; write w (") " ^ Ast.s_unop op))
				| TVar (var, eopt) ->
					write_id w ("var " ^ var.v_name);
					print w " : %s" (t_s e.epos var.v_type);
					(match eopt with
						| None ->
							write w " = ";
							expr_s w (null var.v_type e.epos)
						| Some e ->
							write w " = ";
							expr_s w e
					)
				| TBlock [e] when was_in_value ->
					expr_s w e
				| TBlock el ->
					begin_block w;
					List.iter (fun e ->
						List.iter (fun e ->
							in_value := false;
							line_directive w e.epos;
							expr_s w e;
							newline w;
						) (extract_statements e)
					) el;
					end_block w
				| TIf (econd, e1, Some(eelse)) when was_in_value ->
					write w "( ";
					expr_s w (mk_paren econd);
					write w " ? ";
					expr_s w (mk_paren e1);
					write w " : ";
					expr_s w (mk_paren eelse);
					write w " )";
				| TIf (econd, e1, eelse) ->
					write w "if ";
					expr_s w (mk_paren econd);
					write w " ";
					in_value := false;
					expr_s w (mk_block e1);
					(match eelse with
						| None -> ()
						| Some e ->
							write w "else";
							in_value := false;
							expr_s w (mk_block e)
					)
				| TWhile (econd, eblock, flag) ->
					(match flag with
						| Ast.NormalWhile ->
							write w "while ";
							expr_s w (mk_paren econd);
							write w "";
							in_value := false;
							expr_s w (mk_block eblock)
						| Ast.DoWhile ->
							write w "do ";
							in_value := false;
							expr_s w (mk_block eblock);
							write w "while ";
							in_value := true;
							expr_s w (mk_paren econd);
					)
				| TSwitch (econd, ele_l, default) ->
					write w "switch ";
					expr_s w (mk_paren econd);
					begin_block w;
					List.iter (fun (el, e) ->
						List.iter (fun e ->
							write w "case ";
							in_value := true;
							(match e.eexpr with
								| TField(_, FEnum(e, ef)) ->
									let changed_name = change_id ef.ef_name in
									write w changed_name
								| _ ->
									expr_s w e);
							write w ":";
							newline w;
						) el;
						in_value := false;
						expr_s w (mk_block e);
						newline w;
						newline w
					) ele_l;
					if is_some default then begin
						write w "default:";
						newline w;
						in_value := false;
						expr_s w (get default);
						newline w;
					end;
					end_block w
				| TTry (tryexpr, ve_l) ->
					write w "try ";
					in_value := false;
					expr_s w (mk_block tryexpr);
					let pos = e.epos in
					List.iter (fun (var, e) ->
						print w "catch (%s %s)" (t_s pos var.v_type) (var.v_name);
						in_value := false;
						expr_s w (mk_block e);
						newline w
					) ve_l
				| TReturn eopt ->
					write w "return ";
					if is_some eopt then expr_s w (get eopt)
				| TBreak -> write w "break"
				| TContinue -> write w "continue"
				| TThrow e ->
					write w "throw ";
					expr_s w e
				| TCast (e1,md_t) ->
					((*match gen.gfollow#run_f e.etype with
						| TType({ t_path = ([], "UInt") }, []) ->
							write w "( unchecked ((uint) ";
							expr_s w e1;
							write w ") )"
						| _ ->*)
							(* FIXME I'm ignoring module type *)
                            write w "(";
                            expr_s w e1;
                            write w ")";
							print w " as %s" (t_s e.epos e.etype);
					)
				| TFor (_,_,content) ->
					write w "[ for not supported ";
					expr_s w content;
					write w " ]";
					if !strict_mode then assert false
				| TObjectDecl obj ->
                    (match obj with 
                    | [] -> ()
                    | _ ->
                        (write w "(");
                        (
                        let fst = ref true in 
                        let iterFunc = fun (name,expr) ->
                            ( (if !fst then fst := false else write w ", ");
                            (write w (change_id name));
                            (write w ":");
                            (expr_s w expr); ) in                     
                        List.iter iterFunc obj;    
                        );
                        (write w ")");
                    );
                | TFunction func -> 
                    (match func.tf_args with
                    | [] -> print w "()"; 
                    | _ ->
                        let foldFunc = fun (tvar,_) acc -> acc @ [(sprintf "%s : %s" (change_id tvar.v_name) (t_s e.epos (run_follow gen tvar.v_type)))] in
                        let getArgList () = List.rev (List.fold_right foldFunc func.tf_args []) in
                        print w "(%s)" (String.concat ", " (getArgList ()));
				    );
                    write w (" -> " ^ (t_s e.epos func.tf_type) ^ " in "); 
                    expr_s w func.tf_expr;
                    ();
				| TEnumParameter _ -> write w "[ enum parameter not supported ]"; if !strict_mode then assert false
		in
		expr_s w e
	in

	let rec gen_fpart_attrib w = function
		| EConst( Ident i ), _ ->
			write w i
		| EField( ef, f ), _ ->
			gen_fpart_attrib w ef;
			write w ".";
			write w f
		| _, p ->
			gen.gcon.error "Invalid expression inside @:meta metadata" p
	in

	let rec gen_spart w = function
		| EConst c, p -> (match c with
			| Int s | Float s | Ident s ->
				write w s
			| String s ->
				write w "\"";
				write w (escape s);
				write w "\""
			| _ -> gen.gcon.error "Invalid expression inside @:meta metadata" p)
		| EField( ef, f ), _ ->
			gen_spart w ef;
			write w ".";
			write w f
		| EBinop( Ast.OpAssign, (EConst (Ident s), _), e2 ), _ ->
			write w s;
			write w " = ";
			gen_spart w e2
		| EArrayDecl( el ), _ ->
			write w "{";
			let fst = ref true in
			List.iter (fun e ->
				if !fst then fst := false else write w ", ";
				gen_spart w e
			) el;
			write w "}"
		| ECall(fpart,args), _ ->
			gen_fpart_attrib w fpart;
			write w "(";
			let fst = ref true in
			List.iter (fun e ->
				if !fst then fst := false else write w ", ";
				gen_spart w e
			) args;
			write w ")"
		| _, p ->
			gen.gcon.error "Invalid expression inside @:meta metadata" p
	in

	let gen_annotations w ?(add_newline=true) metadata =
		List.iter (function
			| Meta.Meta, [meta], _ ->
				write w "@";
				gen_spart w meta;
				if add_newline then newline w else write w " ";
			| _ -> ()
		) metadata
	in

	let argt_s p t =
		let w = new_source_writer () in
		let rec run t =
			match t with
				| TType (tdef,p) ->
					gen_annotations w ~add_newline:false tdef.t_meta;
					run (follow_once t)
				| TMono r ->
					(match !r with
					| Some t -> run t
					| _ -> () (* avoid infinite loop / should be the same in this context *))
				| TLazy f ->
					run (!f())
				| _ -> ()
		in
		run t;
		let ret = t_s p t in
		let c = contents w in
		if c <> "" then
			c ^ " " ^ ret
		else
			ret
	in

	let get_string_params cl_params =
		match cl_params with
			| [] ->
				("","")
			| _ ->
				let params = sprintf "<%s>" (String.concat ", " (List.map (fun (_, tcl) -> match follow tcl with | TInst(cl, _) -> snd cl.cl_path | _ -> assert false) cl_params)) in
				let params_extends = List.fold_left (fun acc (name, t) ->
					match run_follow gen t with
						| TInst (cl, p) ->
							(match cl.cl_implements with
								| [] -> acc
								| _ -> acc) (* TODO
								| _ -> (sprintf " where %s : %s" name (String.concat ", " (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements))) :: acc ) *)
						| _ -> trace (t_s Ast.null_pos t); assert false (* FIXME it seems that a cl_params will never be anything other than cl.cl_params. I'll take the risk and fail if not, just to see if that confirms *)
				) [] cl_params in
				(params, String.concat " " params_extends)
	in

	let write_parts w parts =
		let parts = List.filter (fun s -> s <> "") parts in
		write w (String.concat " " parts)
	in

	let rec gen_class_field w ?(is_overload=false) is_static cl is_final cf =
		let is_interface = cl.cl_interface in
		let name, is_new, is_explicit_iface = match cf.cf_name with
			| "new" -> snd cl.cl_path, true, false
			| name when String.contains name '.' ->
				let fn_name, path = parse_explicit_iface name in
				(path_s path cl.cl_meta) ^ "." ^ fn_name, false, true
			| name -> name, false, false
		in
		(match cf.cf_kind with
			| Var _
			| Method (MethDynamic) when not (Type.is_extern_field cf) ->
				(if is_overload || List.exists (fun cf -> cf.cf_expr <> None) cf.cf_overloads then
					gen.gcon.error "Only normal (non-dynamic) methods can be overloaded" cf.cf_pos);
				if not is_interface then begin
					(*let access, modifiers = get_fun_modifiers cf.cf_meta "dynamic" [] in*)
                write_parts w ("public" :: (if is_static then "class" else "") :: ["dynamic"] @ [ "func" ; (change_field name); 
                (t_s cf.cf_pos (run_follow gen cf.cf_type)) ]);
					(match cf.cf_expr with
						| Some e ->
								write w " = ";
								expr_s w e;
						| None -> ()
					)
				end (* TODO see how (get,set) variable handle when they are interfaces *)
			| Method _ when Type.is_extern_field cf || (match cl.cl_kind, cf.cf_expr with | KAbstractImpl _, None -> true | _ -> false) ->
				List.iter (fun cf -> if cl.cl_interface || cf.cf_expr <> None then
					gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
				) cf.cf_overloads
			| Var _ | Method MethDynamic -> ()
			| Method mkind ->
				List.iter (fun cf ->
					if cl.cl_interface || cf.cf_expr <> None then
						gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
				) cf.cf_overloads;
				let is_virtual = is_new || (not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false) in
				let is_override = match cf.cf_name with
					| _ -> List.memq cf cl.cl_overrides
				in
				let visibility = if is_interface then "" else "public" in

				let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
				let visibility, is_virtual = if is_explicit_iface then "",false else visibility, is_virtual in
				let v_n = if is_static then "class" else if is_override && not is_interface then "" else if not is_virtual then "final" else "" in
				let cf_type = if is_override && not is_overload && not (Meta.has Meta.Overload cf.cf_meta) then match field_access gen (TInst(cl, List.map snd cl.cl_params)) cf.cf_name with | FClassField(_,_,_,_,_,actual_t,_) -> actual_t | _ -> assert false else cf.cf_type in

				let params = List.map snd cl.cl_params in
				let ret_type, args = match follow cf_type, follow cf.cf_type with
					| TFun (strbtl, t), TFun(rargs, _) ->
							(apply_params cl.cl_params params (real_type t), List.map2 (fun(_,_,t) (n,o,_) -> (n,o,apply_params cl.cl_params params (real_type t))) strbtl rargs)
					| _ -> assert false
				in

                let is_override = (if is_override && not is_interface then "override" else "") in
				gen_annotations w cf.cf_meta;
				(* public override static func funcName<T> *)
				let params, _ = get_string_params cf.cf_params in
                
                let isInitOrFunc = (if is_new then "init" else "func") in

                let functionName = (if is_new then [""] else [ (change_field name); params; ]) in

                write_parts w ("public" :: v_n :: is_override :: [isInitOrFunc] @ functionName);

				(* (arg1 : String, arg2 : Object) *)
				(match cf.cf_expr with
					| Some { eexpr = TFunction tf } ->
							print w "(%s)" (String.concat ", " (List.map2 (fun (var,_) (_,_,t) -> sprintf "%s : %s" (change_id var.v_name) (argt_s cf.cf_pos (run_follow gen t))) tf.tf_args args))
					| _ ->
							print w "(%s)" (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s : %s" (change_id name) (argt_s cf.cf_pos (run_follow gen t)) ) args))
				);

                (* -> ret_type *)
                write_parts w ([""] @ [ (" -> "); (rett_s cf.cf_pos (run_follow gen ret_type)); ]);

				if is_interface || List.mem "native" modifiers then
				    ()	
				else begin
					let rec loop meta =
						match meta with
							| [] ->
								let expr = match cf.cf_expr with
									| None -> mk (TBlock([])) t_dynamic Ast.null_pos
									| Some s ->
										match s.eexpr with
											| TFunction tf ->
												mk_block (tf.tf_expr)
											| _ -> assert false (* FIXME *)
								in
							    expr_s w expr;
							| (Meta.FunctionCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
								begin_block w;
								write w contents;
								end_block w
							| _ :: tl -> loop tl
					in
					loop cf.cf_meta

				end);
			newline w;
			newline w
	in

	let gen_class w cl =
		let should_close = match change_ns (fst cl.cl_path) with
			| [] -> false
			| ns ->
				print w "package %s;" (String.concat "." (change_ns ns));
				newline w;
				newline w;
				false
		in

		write w "import haxe.root.*;";
		newline w;
		let w_header = w in
		let w = new_source_writer () in
		clear_scope();

		(* add all haxe.root.* to imports *)
		List.iter (function
			| TClassDecl { cl_path = ([],c) } ->
					imports := ([],c) :: !imports
			| TEnumDecl { e_path = ([],c) } ->
					imports := ([],c) :: !imports
			| TAbstractDecl { a_path = ([],c) } ->
					imports := ([],c) :: !imports
			| _ -> ()
		) gen.gtypes_list;

		newline w;
		
		gen_annotations w cl.cl_meta;

		let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
		let is_final = Meta.has Meta.Final cl.cl_meta in

		write_parts w ("public" :: modifiers @ [clt; (change_clname (snd cl.cl_path))]);

		(* type parameters *)
		let params, _ = get_string_params cl.cl_params in
		let cl_p_to_string (c,p) =
			let p = List.map (fun t -> match follow t with
				| TMono _ | TDynamic _ -> t_empty
				| _ -> t) p
			in
			path_param_s cl.cl_pos (TClassDecl c) c.cl_path p c.cl_meta
		in
		print w "%s" params;

        let cl_intf_or_super = (if is_some cl.cl_super then (cl_p_to_string (get cl.cl_super)) else "") :: (List.map cl_p_to_string cl.cl_implements) in
		
        (
            match cl_intf_or_super with 
                | [] -> () 
                | [one] -> if one <> "" then print w " : %s" one else ()
                | _ -> print w " : %s" (String.concat ", " cl_intf_or_super)
		);


		(* class head ok: *)
		(* public class Test<A> : X, Y, Z where A : Y *)
		begin_block w;
		(* our constructor is expected to be a normal "new" function *
		if !strict_mode && is_some cl.cl_constructor then assert false;*)

		let rec loop cl =
			List.iter (fun cf -> add_scope cf.cf_name) cl.cl_ordered_fields;
			List.iter (fun cf -> add_scope cf.cf_name) cl.cl_ordered_statics;
			match cl.cl_super with
				| Some(c,_) -> loop c
				| None -> ()
		in
		loop cl;

		let rec loop meta =
			match meta with
				| [] -> ()
				| (Meta.ClassCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
					write w contents
				| _ :: tl -> loop tl
		in
		loop cl.cl_meta;

        (*
		(match gen.gcon.main_class with
			| Some path when path = cl.cl_path ->
				write w "public static function main(args : Array<String>) -> Void";
				begin_block w;
				(try
					let t = Hashtbl.find gen.gtypes ([], "Sys") in
							match t with
								| TClassDecl(cl) when PMap.mem "_args" cl.cl_statics ->
									write w "Sys._args = args;"; newline w
								| _ -> ()
				with | Not_found -> ()
				);
				end_block w;
				newline w
			| _ -> ()
		);*)

		(if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
		(if not cl.cl_interface then List.iter (gen_class_field w true cl is_final) cl.cl_ordered_statics);
		List.iter (gen_class_field w false cl is_final) cl.cl_ordered_fields;

		end_block w;
		if should_close then end_block w;

		(* add imports *)
		List.iter (function
			| ["haxe";"root"], _ | [], _ -> ()
			| path ->
					write w_header "import ";
					write w_header (path_s path []);
					write w_header ";\n"
		) !imports;
		add_writer w w_header
	in


	let gen_enum w e =
		let should_close = match change_ns (fst e.e_path) with
			| [] -> false
			| ns ->
				print w "package %s;" (String.concat "." (change_ns ns));
				newline w;
				newline w;
				false
		in

		gen_annotations w e.e_meta;
		print w "public enum %s" (change_clname (snd e.e_path));
		begin_block w;
		write w (String.concat ", " (List.map (change_id) e.e_names));
		end_block w;

		if should_close then end_block w
	in

    let gen_type w td = 
        let should_close = match change_ns (fst td.t_path) with
			| [] -> false
			| ns ->
				print w "package %s;" (String.concat "." (change_ns ns));
				newline w;
				newline w;
				false
		in

        let a_fields = match td.t_type with 
            | TMono(m) -> 
                (match !m with 
                | Some m ->
                    (match run_follow gen m with 
                    | TAnon(a) -> a.a_fields
                    | _ -> assert false 
                    )
                | _ -> assert false)
            | _ -> assert false
        in
		gen_annotations w td.t_meta;
		print w "public typealias %s = " (change_clname (snd td.t_path));
        write w "\n(\n";
        
        let iterFunc = fun name cf ->
            write w ("\t" ^ (change_id name) ^ " : " ^ (t_s cf.cf_pos cf.cf_type) ^ "\n")
        in
		PMap.iter (iterFunc) a_fields;
		write w ")\n";

		if should_close then end_block w 
    in

	let module_type_gen w md_tp =
		Codegen.map_source_header gen.gcon (fun s -> print w "// %s\n" s);
		match md_tp with
			| TClassDecl cl ->
				if not cl.cl_extern then begin
					gen_class w cl;
					newline w;
					newline w
				end;
				(not cl.cl_extern)
			| TEnumDecl e ->
				if not e.e_extern && not (Meta.has Meta.Class e.e_meta) then begin
					gen_enum w e;
					newline w;
					newline w
				end;
				(not e.e_extern)
			| TTypeDecl td -> (match td.t_type with 
                | TMono(m) -> 
                    (match !m with 
                    | Some m ->
                        (*print_string (t_s td.t_pos m);
                        print_string "\t";
                        print_string (s_type_path td.t_path);*)
                        (match run_follow gen m with 
                        | TAnon(a) -> gen_type w td; (*print_string "\n";*) true
                        | _ -> false 
                        )
                        (* print_string "\n"; *)
                    | None -> false);
                | _ -> false)
			| _ -> 
				false
	in

	(* generate source code *)
	init_ctx gen;

	Hashtbl.add gen.gspecial_vars "__swift__" true;

	gen.greal_type <- real_type;
	gen.greal_type_param <- change_param_type;

	SetHXGen.run_filter gen;

	(* before running the filters, follow all possible types *)
	(* this is needed so our module transformations don't break some core features *)
	(* like multitype selection *)
	let run_follow_gen = run_follow gen in
	let rec type_map e = Type.map_expr_type (fun e->type_map e) (run_follow_gen)	(fun tvar-> tvar.v_type <- (run_follow_gen tvar.v_type); tvar) e in
	let super_map (cl,tl) = (cl, List.map run_follow_gen tl) in
	List.iter (function
		| TClassDecl cl ->
				let all_fields = (Option.map_default (fun cf -> [cf]) [] cl.cl_constructor) @ cl.cl_ordered_fields @ cl.cl_ordered_statics in
				List.iter (fun cf ->
					cf.cf_type <- run_follow_gen cf.cf_type;
					cf.cf_expr <- Option.map type_map cf.cf_expr
				) all_fields;
			 cl.cl_dynamic <- Option.map run_follow_gen cl.cl_dynamic;
			 cl.cl_array_access <- Option.map run_follow_gen cl.cl_array_access;
			 cl.cl_init <- Option.map type_map cl.cl_init;
			 cl.cl_super <- Option.map super_map cl.cl_super;
			 cl.cl_implements <- List.map super_map cl.cl_implements
		| _ -> ()
		) gen.gtypes_list;

	let get_vmtype t = match real_type t with
		| TInst({ cl_path = ["swift"],"NativeArray" }, tl) -> t
		| TInst(c,tl) -> TInst(c,List.map (fun _ -> t_dynamic) tl)
		| TEnum(e,tl) -> TEnum(e, List.map (fun _ -> t_dynamic) tl)
		| TType(t,tl) -> TType(t, List.map (fun _ -> t_dynamic) tl)
		| TAbstract(a,tl) -> TAbstract(a, List.map (fun _ -> t_dynamic) tl)
		| t -> t
	in

	FixOverrides.configure ~get_vmtype gen;
	Normalize.configure gen ~metas:(Hashtbl.create 0);
	AbstractImplementationFix.configure gen;

	IteratorsInterface.configure gen;

	let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen (get_cl (get_type gen (["haxe";"lang"],"Function"))) 6 in
	(*ClosuresToClass.configure gen closure_t;

	let enum_base = (get_cl (get_type gen (["haxe";"lang"],"Enum")) ) in
	let param_enum_base = (get_cl (get_type gen (["haxe";"lang"],"ParamEnum")) ) in
	EnumToClass.configure gen (None) false true enum_base param_enum_base;

	InterfaceVarsDeleteModf.configure gen;*)

	let dynamic_object = (get_cl (get_type gen (["haxe";"lang"],"DynamicObject")) ) in

	let object_iface = get_cl (get_type gen (["haxe";"lang"],"IHxObject")) in

	let empty_en = match get_type gen (["haxe";"lang"], "EmptyObject") with TEnumDecl e -> e | _ -> assert false in
	let empty_ctor_type = TEnum(empty_en, []) in
	let empty_en_expr = mk (TTypeExpr (TEnumDecl empty_en)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics empty_en) }) null_pos in
	let empty_ctor_expr = mk (TField (empty_en_expr, FEnum(empty_en, PMap.find "EMPTY" empty_en.e_constrs))) empty_ctor_type null_pos in
	
    (*OverloadingConstructor.configure ~empty_ctor_type:empty_ctor_type ~empty_ctor_expr:empty_ctor_expr gen;*)

	let rcf_static_find = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "findHash" Ast.null_pos [] in
	let get_specialized_postfix t = match t with
		| TAbstract({a_path = [],"Float"}, _) -> "Float"
		| TInst({cl_path = [],"String"},_) -> "String"
		| TAnon _ | TDynamic _ -> "Dynamic"
		| _ -> print_endline (debug_type t); assert false
	in
	let rcf_static_insert t = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("insert" ^ get_specialized_postfix t) Ast.null_pos [] in
	let rcf_static_remove t = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("remove" ^ get_specialized_postfix t) Ast.null_pos [] in

	let can_be_float t = like_float (real_type t) in

	let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
		let is_float = can_be_float (if is_none may_set then main_expr.etype else (get may_set).etype) in
		let fn_name = if is_some may_set then "setField" else "getField" in
		let fn_name = if is_float then fn_name ^ "_f" else fn_name in
		let pos = field_expr.epos in

		let is_unsafe = { eexpr = TConst(TBool is_unsafe); etype = basic.tbool; epos = pos } in

		let should_cast = match main_expr.etype with | TAbstract({ a_path = ([], "Float") }, []) -> false | _ -> true in
		let infer = mk_static_field_access_infer runtime_cl fn_name field_expr.epos [] in
		let first_args =
			[ field_expr; { eexpr = TConst(TString field); etype = basic.tstring; epos = pos } ]
			@ if is_some may_hash then [ { eexpr = TConst(TInt (get may_hash)); etype = basic.tint; epos = pos } ] else []
		in
		let args = first_args @ match is_float, may_set with
			| true, Some(set) ->
				[ if should_cast then mk_cast basic.tfloat set else set ]
			| false, Some(set) ->
				[ set ]
			| _ ->
				[ is_unsafe ]
		in

		let call = { main_expr with eexpr = TCall(infer,args) } in
		let call = if is_float && should_cast then mk_cast main_expr.etype call else call in
		call
	in

	let rcf_on_call_field ecall field_expr field may_hash args =
		let infer = mk_static_field_access_infer runtime_cl "callField" field_expr.epos [] in

		let hash_arg = match may_hash with
			| None -> []
			| Some h -> [ { eexpr = TConst(TInt h); etype = basic.tint; epos = field_expr.epos } ]
		in

		let arr_call = if args <> [] then
			{ eexpr = TArrayDecl args; etype = basic.tarray t_dynamic; epos = ecall.epos }
		else
			null (basic.tarray t_dynamic) ecall.epos
		in


		let call_args =
			[field_expr; { field_expr with eexpr = TConst(TString field); etype = basic.tstring } ]
				@ hash_arg
				@ [ arr_call ]
		in

		mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args); etype = t_dynamic }
	in

	let rcf_ctx =
		ReflectionCFs.new_ctx
			gen
			closure_t
			object_iface
			false
			rcf_on_getset_field
			rcf_on_call_field
			(fun hash hash_array length -> { hash with eexpr = TCall(rcf_static_find, [hash; hash_array; length]); etype=basic.tint })
			(fun hash -> hash)
			(fun hash_array length pos value ->
				{ hash_array with
					eexpr = TBinop(OpAssign,
								hash_array,
								mk (TCall(rcf_static_insert value.etype, [hash_array; length; pos; value])) hash_array.etype hash_array.epos)
			})			(fun hash_array length pos ->
				let t = gen.gclasses.nativearray_type hash_array.etype in
				{ hash_array with eexpr = TCall(rcf_static_remove t, [hash_array; length; pos]); etype = gen.gcon.basic.tvoid }
			)
		in

	ReflectionCFs.UniversalBaseClass.configure gen (get_cl (get_type gen (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;

	ReflectionCFs.configure_dynamic_field_access rcf_ctx;

	ReflectionCFs.implement_varargs_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"], "VarArgsBase")) );

	let slow_invoke = mk_static_field_access_infer (runtime_cl) "slowCallField" Ast.null_pos [] in
	ReflectionCFs.configure rcf_ctx ~slow_invoke:(fun ethis efield eargs -> {
		eexpr = TCall(slow_invoke, [ethis; efield; eargs]);
		etype = t_dynamic;
		epos = ethis.epos;
	} ) object_iface;

	ObjectDeclMap.configure gen (ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object);

	(*InitFunction.configure gen;*)
	TArrayTransform.configure gen (
	fun e _ ->
		match e.eexpr with
			| TArray ({ eexpr = TLocal { v_extra = Some( _ :: _, _) } }, _) -> (* captured transformation *)
				false
			| TArray(e1, e2) ->
				( match run_follow gen (follow e1.etype) with
					| TInst({ cl_path = (["swift"], "NativeArray") }, _) -> false
					| _ -> true )
			| _ -> assert false
	) "__get" "__set";

	(*let field_is_dynamic t field =
		match field_access_esp gen (gen.greal_type t) field with
			| FClassField (cl,p,_,_,_,t,_) ->
				let p = change_param_type (TClassDecl cl) p in
				is_dynamic (apply_params cl.cl_params p t)
			| FEnumField _ -> false
			| _ -> true
	in

	let is_type_param e = match follow e with
		| TInst( { cl_kind = KTypeParameter _ },[]) -> true
		| _ -> false
	in

	let is_dynamic_expr e =
		is_dynamic e.etype || match e.eexpr with
		| TField(tf, f) ->
			field_is_dynamic tf.etype f
		| _ ->
			false
	in

	let may_nullable t = match gen.gfollow#run_f t with
		| TType({ t_path = ([], "Null") }, [t]) ->
			(match follow t with
				| TInst({ cl_path = ([], "String") }, [])
				| TAbstract ({ a_path = ([], "Float") },[])
				| TInst({ cl_path = (["haxe"], "Int32")}, [] )
				| TInst({ cl_path = (["haxe"], "Int64")}, [] )
				| TAbstract ({ a_path = ([], "Int") },[])
				| TAbstract ({ a_path = ([], "Bool") },[]) -> Some t
				| t when is_swift_basic_type t -> Some t
				| _ -> None )
		| _ -> None
	in

	let is_double t = like_float t && not (like_int t) in
	let is_int t = like_int t in

	DynamicOperators.configure gen
		(fun e -> match e.eexpr with
			| TBinop (Ast.OpEq, e1, e2) ->
				is_dynamic e1.etype || is_dynamic e2.etype || is_type_param e1.etype || is_type_param e2.etype
			| TBinop (Ast.OpAdd, e1, e2)
			| TBinop (Ast.OpNotEq, e1, e2) -> is_dynamic e1.etype || is_dynamic e2.etype || is_type_param e1.etype || is_type_param e2.etype
			| TBinop (Ast.OpLt, e1, e2)
			| TBinop (Ast.OpLte, e1, e2)
			| TBinop (Ast.OpGte, e1, e2)
			| TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype || is_dynamic_expr e1 || is_dynamic_expr e2 || is_string e1.etype || is_string e2.etype
			| TBinop (_, e1, e2) -> is_dynamic e.etype || is_dynamic_expr e1 || is_dynamic_expr e2
			| TUnop (_, _, e1) ->
				is_dynamic_expr e1
			| _ -> false)
		(fun e1 e2 ->
			let is_null e = match e.eexpr with | TConst(TNull) | TLocal({ v_name = "__undefined__" }) -> true | _ -> false in

			match e1.eexpr, e2.eexpr with
				| TConst c1, TConst c2 when is_null e1 || is_null e2 ->
					{ e1 with eexpr = TConst(TBool (c1 = c2)); etype = basic.tbool }
				| _ when is_null e1 || is_null e2 && not (is_swift_basic_type e1.etype || is_swift_basic_type e2.etype) ->
					{ e1 with eexpr = TBinop(Ast.OpEq, e1, e2); etype = basic.tbool }
				| _ ->
				let is_ref = match follow e1.etype, follow e2.etype with
					| TDynamic _, _
					| _, TDynamic _
					| TAbstract ({ a_path = ([], "Float") },[]) , _
					| TInst( { cl_path = (["haxe"], "Int32") }, [] ), _
					| TInst( { cl_path = (["haxe"], "Int64") }, [] ), _
					| TAbstract ({ a_path = ([], "Int") },[]) , _
					| TAbstract ({ a_path = ([], "Bool") },[]) , _
					| _, TAbstract ({ a_path = ([], "Float") },[])
					| _, TAbstract ({ a_path = ([], "Int") },[])
					| _, TInst( { cl_path = (["haxe"], "Int32") }, [] )
					| _, TInst( { cl_path = (["haxe"], "Int64") }, [] )
					| _, TAbstract ({ a_path = ([], "Bool") },[])
					| TInst( { cl_kind = KTypeParameter _ }, [] ), _
					| _, TInst( { cl_kind = KTypeParameter _ }, [] ) -> false
					| _, _ -> true
				in

				let static = mk_static_field_access_infer (runtime_cl) (if is_ref then "refEq" else "eq") e1.epos [] in
				{ eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tbool; epos=e1.epos }
		)
		(fun e e1 e2 ->
			match may_nullable e1.etype, may_nullable e2.etype with
				| Some t1, Some t2 ->
					let t1, t2 = if is_string t1 || is_string t2 then
						basic.tstring, basic.tstring
					else if is_double t1 || is_double t2 then
						basic.tfloat, basic.tfloat
					else if is_int t1 || is_int t2 then
						basic.tint, basic.tint
					else t1, t2 in
					{ eexpr = TBinop(Ast.OpAdd, mk_cast t1 e1, mk_cast t2 e2); etype = e.etype; epos = e1.epos }
				| _ ->
					let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
					mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
		(fun e1 e2 ->
			if is_string e1.etype then begin
				{ e1 with eexpr = TCall(mk_field_access gen e1 "compareTo" e1.epos, [ e2 ]); etype = gen.gcon.basic.tint }
			end else begin
				let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
				{ eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos }
			end);*)

	(*FilterClosures.configure gen (fun e1 s -> true) (ReflectionCFs.get_closure_func rcf_ctx closure_cl);

	let base_exception = get_cl (get_type gen (["swift"; "lang"], "Throwable")) in
	let base_exception_t = TInst(base_exception, []) in

	let hx_exception = get_cl (get_type gen (["haxe";"lang"], "HaxeException")) in
	let hx_exception_t = TInst(hx_exception, []) in

	let rec is_exception t =
		match follow t with
			| TInst(cl,_) ->
				if cl == base_exception then
					true
				else
					(match cl.cl_super with | None -> false | Some (cl,arg) -> is_exception (TInst(cl,arg)))
			| _ -> false
	in

	TryCatchWrapper.configure gen
		(fun t -> not (is_exception (real_type t)))
		(fun throwexpr expr ->
			let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], hx_exception_t)) expr.epos in
			{ throwexpr with eexpr = TThrow { expr with eexpr = TCall(wrap_static, [expr]); etype = hx_exception_t }; etype = gen.gcon.basic.tvoid }
		)
		(fun v_to_unwrap pos ->
			let local = mk_cast hx_exception_t { eexpr = TLocal(v_to_unwrap); etype = v_to_unwrap.v_type; epos = pos } in
			mk_field_access gen local "obj" pos
		)
		(fun rethrow ->
			let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], hx_exception_t)) rethrow.epos in
			{ rethrow with eexpr = TThrow { rethrow with eexpr = TCall(wrap_static, [rethrow]); etype = hx_exception_t }; }
		)
		(base_exception_t)
		(hx_exception_t)
		(fun v e ->

			let exc_cl = get_cl (get_type gen (["haxe";"lang"],"Exceptions")) in
			let exc_field = mk_static_field_access_infer exc_cl "setException" e.epos [] in
			let esetstack = { eexpr = TCall(exc_field,[mk_local v e.epos]); etype = gen.gcon.basic.tvoid; epos = e.epos } in

			Type.concat esetstack e;
		); *)

	(*ClassInstance.configure gen (fun e _ -> { e with eexpr = TCall({ eexpr = TLocal(alloc_var "__typeof__" t_dynamic); etype = t_dynamic; epos = e.epos }, [e]) });

	CastDetect.configure gen (Some empty_ctor_type) false;*)

	(*SwitchToIf.configure gen (fun e ->
		match e.eexpr with
			| TSwitch(cond, cases, def) ->
				(match gen.gfollow#run_f cond.etype with
					| TInst( { cl_path = (["haxe"], "Int32") }, [] )
					| TAbstract ({ a_path = ([], "Int") },[])
					| TInst({ cl_path = ([], "String") },[]) ->
						(List.exists (fun (c,_) ->
							List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
						) cases)
					| _ -> true
				)
			| _ -> assert false
	);*)

	ExpressionUnwrap.configure gen (fun e -> Some { eexpr = TVar(mk_temp gen "expr" e.etype, Some e); etype = gen.gcon.basic.tvoid; epos = e.epos });

	(*UnnecessaryCastsRemoval.configure gen;*)

	IntDivisionSynf.configure gen;

	UnreachableCodeEliminationSynf.configure gen true;

	ArrayDeclSynf.configure gen native_arr_cl;

	(*let goto_special = alloc_var "__goto__" t_dynamic in
	let label_special = alloc_var "__label__" t_dynamic in
	
    SwitchBreakSynf.configure gen
		(fun e_loop n api ->
			{ e_loop with eexpr = TBlock( { eexpr = TCall( mk_local label_special e_loop.epos, [ Codegen.ExprBuilder.make_int gen.gcon n e_loop.epos ] ); etype = t_dynamic; epos = e_loop.epos } :: [e_loop] ) };
		)
		(fun e_break n api ->
			{ eexpr = TCall( mk_local goto_special e_break.epos, [  Codegen.ExprBuilder.make_int gen.gcon n e_break.epos ] ); etype = t_dynamic; epos = e_break.epos }
		);*)

	DefaultArguments.configure gen;
	(*InterfaceMetas.configure gen;

	(* add native String as a String superclass *)
	let str_cl = match gen.gcon.basic.tstring with | TInst(cl,_) -> cl | _ -> assert false in
	str_cl.cl_super <- Some (get_cl (get_type gen (["haxe";"lang"], "NativeString")), []);*)

	let mkdir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
	mkdir gen.gcon.file;
	mkdir (gen.gcon.file ^ "/src");

	let out_files = ref [] in

	(* add resources array *)
	let res = ref [] in
	Hashtbl.iter (fun name v ->
		res := { eexpr = TConst(TString name); etype = gen.gcon.basic.tstring; epos = Ast.null_pos } :: !res;
		let name = Codegen.escape_res_name name true in
		let full_path = gen.gcon.file ^ "/src/" ^ name in
		mkdir_from_path full_path;

		let f = open_out_bin full_path in
		output_string f v;
		close_out f;

		out_files := (unique_full_path full_path) :: !out_files
	) gen.gcon.resources;
	(try
		let c = get_cl (Hashtbl.find gen.gtypes (["haxe"], "Resource")) in
		let cf = PMap.find "content" c.cl_statics in
		cf.cf_expr <- Some ({ eexpr = TArrayDecl(!res); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = Ast.null_pos })
	with | Not_found -> ());

	run_filters gen;

	TypeParams.RenameTypeParameters.run gen;

	let parts = Str.split_delim (Str.regexp "[\\/]+") gen.gcon.file in
	mkdir_recursive "" parts;

	let source_dir = gen.gcon.file ^ "/src" in
	List.iter (fun md ->
		let w = SourceWriter.new_source_writer () in
		let should_write = module_type_gen w md in
		if should_write then begin
			let path = change_path (t_path md) in
			write_file gen w (source_dir ^ "/" ^ (String.concat "/" (fst path))) path "swift" out_files;
		end
	) gen.gtypes_list;

	if not (Common.defined gen.gcon Define.KeepOldOutput) then
		clean_files (gen.gcon.file ^ "/src") !out_files gen.gcon.verbose;
   
(*this next void return is necessary to compile when I comment out the block below.*)
    ()

(*
	let path_s_desc path = path_s path [] in
	dump_descriptor gen ("hxswift_build.txt") path_s_desc (fun md -> path_s_desc (t_infos md).mt_path);
	if ( not (Common.defined gen.gcon Define.NoCompilation) ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir gen.gcon.file;
		let cmd = "haxelib run hxswift hxswift_build.txt --haxe-version " ^ (string_of_int gen.gcon.version) ^ " --feature-level 1" in
		print_endline cmd;
		if gen.gcon.run_command cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end
*)

(* end of configure function *)

let before_generate con = 
    match con with 
    | _ -> () 

let generate con =
	let gen = new_ctx con in
	gen.gallow_tp_dynamic_conversion <- true;

	(*let basic = con.basic in*)
	(* make the basic functions in swift *)
	(*let cl_cl = get_cl (get_type gen (["swift";"lang"],"Class")) in
	let basic_fns =
	[
		mk_class_field "equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "toString" (TFun([], basic.tstring)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "hashCode" (TFun([], basic.tint)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "getClass" (TFun([], (TInst(cl_cl,[t_dynamic])))) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "wait" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "notify" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "notifyAll" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
	] in *)

    let basic_fns = [] in 
	List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;

	(try
		configure gen
	with | TypeNotFound path -> con.error ("Error. Module '" ^ (s_type_path path) ^ "' is required and was not included in build.")	Ast.null_pos);
