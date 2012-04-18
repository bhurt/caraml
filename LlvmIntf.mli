val ctx : Llvm.llcontext;;

val void_type : Llvm.lltype;;
val unit_type : Llvm.lltype;;
val bool_type : Llvm.lltype;;
val int_type : Llvm.lltype;;
val float_type : Llvm.lltype;;
val intptr_type : Llvm.lltype;;
val ptr_type : Llvm.lltype -> Llvm.lltype;;
val func_type : Llvm.lltype list -> Llvm.lltype -> Llvm.lltype;;
val struct_type : Llvm.lltype list -> Llvm.lltype;;
val llvm_of_type : Type.t -> Llvm.lltype;;

val int_const : int -> Llvm.llvalue;;
val int64_const : Int64.t -> Llvm.llvalue;;
val float_const : float -> Llvm.llvalue;;
val bool_const : bool -> Llvm.llvalue;;
val unit_const : unit -> Llvm.llvalue;;

val const_struct : Llvm.llvalue list -> Llvm.llvalue;;

val int_init : Llvm.llvalue;;
val float_init : Llvm.llvalue;;
val bool_init : Llvm.llvalue;;
val unit_init : Llvm.llvalue;;
val ptr_init : Llvm.llvalue;;
val init_of_type : Type.t -> Llvm.llvalue;;


val mdl : unit -> Llvm.llmodule;;

val dump_module : unit -> unit;;
val lookup_global : string -> Llvm.llvalue;;
val lookup_function : string -> Llvm.llvalue;;
val define_global : string -> Llvm.llvalue -> Llvm.llvalue;;
val declare_global : string -> Llvm.lltype -> Llvm.llvalue;;
val declare_function : string -> Llvm.lltype -> Llvm.llvalue;;
val app_table_type : unit -> Llvm.lltype;;
val write_bitcode_file : string -> bool;;
val make_app_fn_type : int -> Llvm.lltype;;
val make_app_table_type : unit -> Llvm.lltype;;

val with_module : ?no_tables:bool -> string -> unit;;

val with_function : string -> Llvm.lltype -> unit;;
val end_function : unit -> unit;;

val alloc_reg_name : unit -> string;;
val alloc_block_name : unit -> string;;

val param : int -> Llvm.llvalue;;
val params : unit -> Llvm.llvalue list;;


type block_t;;

val entry_block : unit -> block_t;;
val new_block : unit -> block_t;;

val ret : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val ret_void : block_t -> Llvm.llvalue;;
val br : block_t -> block_t -> Llvm.llvalue;;
val cond_br : block_t -> test:Llvm.llvalue
                    -> on_true:block_t -> on_false:block_t -> Llvm.llvalue;;

val add : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val sub : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val mul : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val div : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val bool_and : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val bool_or : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;

val lt : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val le : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val gt : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val ge : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val eq : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val ne : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;

val neg : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val bool_not : block_t -> Llvm.llvalue -> Llvm.llvalue;;

val fadd : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fsub : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fmul : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fdiv : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;

val flt : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fle : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fgt : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fge : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val feq : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;
val fne : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;

val fneg : block_t -> Llvm.llvalue -> Llvm.llvalue;;

val phi : block_t -> (Llvm.llvalue * block_t) list -> Llvm.llvalue;;

val load : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val store : block_t -> ptr:Llvm.llvalue -> value:Llvm.llvalue -> Llvm.llvalue;;
val offset : block_t -> Llvm.llvalue -> int -> Llvm.llvalue;;
val struct_gep : block_t -> Llvm.llvalue -> int -> Llvm.llvalue;;

val call : block_t -> Llvm.llvalue -> Llvm.llvalue list -> Llvm.llvalue;;
val void_call : block_t -> Llvm.llvalue -> Llvm.llvalue list -> Llvm.llvalue;;

val int_to_bool : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val bool_to_int : block_t -> Llvm.llvalue -> Llvm.llvalue;;

val box_unit : 'a -> 'b -> 'b;;
val box_bool : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val box_int : 'a -> 'b -> 'b;;
val box_float : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val box_ptr : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val box : Type.t -> block_t -> Llvm.llvalue -> Llvm.llvalue;;

val unbox_unit : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val unbox_bool : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val unbox_int : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val unbox_float : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val unbox_ptr : block_t -> Llvm.llvalue -> Llvm.llvalue;;
val unbox : Type.t -> block_t -> Llvm.llvalue -> Llvm.llvalue;;

val bitcast : block_t -> Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue;;
val ptr_cmp_lt : block_t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue;;

val load_global : block_t -> string -> Llvm.llvalue;;

val set_tail_call : ?is_tail:bool -> Llvm.llvalue -> unit;;

