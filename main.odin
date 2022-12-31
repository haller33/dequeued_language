package dequeue

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import strc "core:strconv"
import qu "core:container/queue"

MIN_RING_SIZE :: 64
MIN_INSTRUCTION_SIZE :: 32
MAX_STRING_SIZE :: 64

MAX_INSTRUCTIONS :: 42

DEBUG_MODE :: false

raw_value :: distinct union {int, bool, string, f32}

TOTAL_INSTRUCTIONS :: 47

ST_INS_Flags :: enum {
    // instructions with queue
    BC_PUSH_VAL,
    BC_LOAD_VAL,
    BC_DROP_VAL,
    BC_TWO_DROP_VAL,
    BC_DUBLE_VAL,
    BC_TWO_DUBLE_VAL,
    BC_SWAP_VAL,
    BC_TWO_SWAP_VAL,
    BC_OVER_VAL,
    BC_ROT_VAL,
    BC_TWO_ROT_VAL,
    BC_HALF_ROT_VAL,
    // inverse instructions ( rest of queue ~ end of queue )
    BC_IN_PUSH_VAL,
    BC_IN_LOAD_VAL,
    BC_IN_DROP_VAL,
    BC_IN_TWO_DROP_VAL,
    BC_IN_DUBLE_VAL,
    BC_IN_TWO_DUBLE_VAL,
    BC_IN_SWAP_VAL,
    BC_IN_TWO_SWAP_VAL,
    BC_IN_OVER_VAL,
    BC_IN_ROT_VAL,
    BC_IN_TWO_ROT_VAL,
    BC_IN_HALF_ROT_VAL,
    // operations on the "stack" ~ queue
    ADD,
    MULTIPLY,
    SUBTRACT,
    RETURN,
    WHILE_LOOP,
    END_WHILE,
    LESSTHAN_VAL,
    EQUALTHAN_VAL,
    /// metafisic stuff (eval - applay)
    VALUE_VAL,
    APPLY_PROCEDURE,
    EVAL_PROCEDURE,
    // special structions
    DO_WRITE,
    DO_SYMBOL,
    DO_NOT,
    DOT_STACK,
    DOT_BYTECODE,
    NO_OP,
    DUP_FIRST_INSTRUCTION,
    ROT_FIRST_INSTRUCTION,
    DUP_LAST_INSTRUCTION,
    ROT_LAST_INSTRUCTION,
    END_INSTRUCTION,
    NIL_INS,
}

ST_Flags :: enum {

    ST_ARG,
    ST_VAR,
    ST_NUM,
    ST_FLOAT,
    ST_BOOL,
    ST_PROC,
    ST_SYMBOL,
    ST_NIL,
}

ST_Data :: struct {
    
    enable : bool,
    value : raw_value,
    arg : ST_Flags,
    line : int,
}

ST_Bytecode :: struct {
    
    enable : bool,
    instruction : ST_INS_Flags,
    param : ST_Data,
    line : int,
}

Error :: struct {

    has_error : bool,
    place : string,
    line : int,
    reason : string,
    value_expected : raw_value,
    value_get : raw_value,
    data_st : ST_Data,
}



st_dset :: proc ( tmp : ST_Data, value : raw_value ) -> ST_Data {

    new_data : ST_Data
    
    new_data = ST_Data{ value = value, arg = tmp.arg, enable = tmp.enable, line = tmp.line }

    return new_data
}


st_dcreate_nil :: proc (line_of : int = 0) -> ST_Data {

    return ST_Data{ value = false, arg = ST_Flags.ST_NIL, enable = true, line = line_of }
}

st_dcreate :: proc (arg_c : raw_value, line_of : int = 0, symb : ST_Flags = ST_Flags.ST_NIL) -> ST_Data {

    if symb == ST_Flags.ST_NIL {
	#partial switch in arg_c {

	    case int: return ST_Data{ value = arg_c, arg = ST_Flags.ST_NUM, enable = true, line = line_of }
	    case bool: return ST_Data{ value = arg_c, arg = ST_Flags.ST_BOOL, enable = true, line = line_of }
	    case string: return ST_Data{ value = arg_c, arg = ST_Flags.ST_ARG, enable = true, line = line_of }
	    case :

	}
    } else {
	return ST_Data{ value = arg_c, arg = symb, enable = true, line = line_of }
    }
    return ST_Data { enable = true, value = false, arg = ST_Flags.ST_NIL}
}

bc_dcreate :: proc ( outside_instruction : ST_INS_Flags = ST_INS_Flags.NIL_INS, outside_param : ST_Data, line_of : int = 0) -> ST_Bytecode {

    if ! ( outside_instruction == ST_INS_Flags.NIL_INS ) {

	return ST_Bytecode { enable = true, instruction = outside_instruction, param = outside_param, line = line_of }
    }
    return ST_Bytecode{}
}

interpret_broke_by_newline_and_space :: proc ( file_path_name : string ) -> ([dynamic]string, [dynamic]int) {

    dy_atoms : [dynamic]string
    dy_lines_context : [dynamic]int

    idx : int = 1
    
    data, ok := os.read_entire_file(file_path_name, context.allocator)
    if !ok {
	// could not read file
	fmt.println("cannot read file")
	return [dynamic]string { "" }, [dynamic]int {}
    }
    // defer delete(data, context.allocator)
    
    it := string(data)

    tmp_string : [dynamic] string
    defer delete ( tmp_string )

    tmp_simbol : [dynamic] string
    defer delete ( tmp_simbol )
    
    
    has_string : bool = false
    has_simbol : bool = false
    in_string : bool = false
    in_simbol : bool = false

    
    for line in strings.split_lines_iterator(&it) {
	if strings.contains ( line, "\"" ) {

	    has_string = true
	}
	if strings.contains ( line, "\'" ) {
	    has_simbol = true
	}
	for atomic in strings.split_after(line, " ") {

	    if in_simbol {
		append ( &tmp_simbol, atomic )
	    } else if !has_string && !in_string && !has_simbol {
		append ( &dy_atoms, strings.trim_space ( atomic ) )
		append ( &dy_lines_context, idx )
	    } else {
		if strings.contains ( atomic, "\'" ) {
		    append ( &tmp_simbol, atomic )
		    in_simbol = true
		} else if in_string && strings.contains ( atomic, "\"" ) {
		    append ( &tmp_string, atomic )
		    append ( &dy_atoms, strings.trim_space ( strings.concatenate ( tmp_string[:] ) ) )
		    append ( &dy_lines_context, idx )
		    tmp_string = {""}
		    in_string = false
		} else if in_string || strings.contains ( atomic, "\"" ) {
		    append ( &tmp_string, atomic )
		    in_string = true
		} else {
		    append ( &dy_atoms, strings.trim_space ( atomic ) )
		    append ( &dy_lines_context, idx )
		}
	    }

	    when DEBUG_MODE {
		fmt.println ( atomic, "- " , has_string, " - ", in_string, " # ", has_simbol, " - ", in_simbol )
	    }
	}


	if in_simbol {
	    append ( &dy_atoms, strings.trim_space ( strings.concatenate ( tmp_simbol[:] ) ) )
	    append ( &dy_lines_context, idx )
	    in_simbol = false
	}   
	
	has_simbol = false
	has_string = false
	in_string = false
	in_simbol = false
	idx = idx + 1
    }
    
    return dy_atoms, dy_lines_context
}



st_dplus :: proc ( value_one, value_two : ST_Data ) -> ST_Data {
    
    value_sum : raw_value
    flag_error : bool = false

    value_one_digit := value_one.value.(int)
    value_two_digit := value_two.value.(int)

    value_sum = value_one_digit + value_two_digit
    
    swap_value := st_dcreate ( value_sum, value_two.line )

    return swap_value
//    return ST_Data {}
}

parser :: proc ( file_path : string, bytecode : ^qu.Queue ( ST_Bytecode ) ) {

    return_data_atoms, lines_idx : = interpret_broke_by_newline_and_space ( file_path )
    defer delete ( return_data_atoms )

    FIRST_BYTE_RUNE_STRING_REPRESENT :: 34
    FIRST_BYTE_RUNE_SIMBOL_REPRESENT :: 39

    // assert ( TOTAL_INSTRUCTIONS == len ( return_data_atoms ) ) // TODO ADD ASSERTION OF NUMBER OF INSTRUCTIONS

    for atom, idx in return_data_atoms {

	fmt.println ( atom )
	
	if DEBUG_MODE {
	    fmt.println ( "l ", lines_idx[idx], " -- ", atom )
	}
	// if strings.compare ( "push", atom ) == 0 {
	if atom[0] == FIRST_BYTE_RUNE_STRING_REPRESENT { // String
	    // qu.push_back ( &ring_data, st_dcreate ( atom, idx ) )
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_PUSH_VAL, st_dcreate ( atom, lines_idx[idx] ), lines_idx[idx] ) )
	} else if atom[0] == FIRST_BYTE_RUNE_SIMBOL_REPRESENT { 
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_PUSH_VAL, st_dcreate ( atom, lines_idx[idx], ST_Flags.ST_SYMBOL), lines_idx[idx] ) )
	    
	    // bc_dcreate ( ST_INS_Flags.BC_PUSH_VAL, st_dcreate ( atom, idx ), idx )
	} else if strings.compare ( "load", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_LOAD_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "drop", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_DROP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "2drop", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_TWO_DROP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "dup", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_DUBLE_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "2dup", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_DUBLE_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "swap", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_SWAP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "2swap", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_TWO_SWAP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "over", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_OVER_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "rot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "2rot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_TWO_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	    // inverted commads
	} else if strings.compare ( "hrot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_HALF_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	    // inverted commads
	    
	}else if strings.compare ( "!drop", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_DROP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!2drop", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_TWO_DROP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!dup", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_DUBLE_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!2dup", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_DUBLE_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!swap", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_SWAP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!2swap", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_TWO_SWAP_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!over", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_OVER_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!rot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "!2rot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_TWO_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )
	    // inverted commads
	} else if strings.compare ( "!hrot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_IN_HALF_ROT_VAL, st_dcreate_nil ( lines_idx[idx] ) ) )

	    // inverted commads
	} else if strings.compare ( "do", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	} else if strings.compare ( "end", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	    
	} else if strings.compare ( "add", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.ADD, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "mult", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.MULTIPLY, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "sub", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.SUBTRACT, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "while", atom ) == 0 { // TODOOOOOO :: WHILEEEEEE
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DO_WRITE, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "if", atom ) == 0 { // maybe need to be removed.
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	} else if strings.compare ( "not", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DO_NOT, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "write", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DO_WRITE, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "end_while", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	} else if strings.compare ( "end", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	} else if strings.compare ( "symb", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DO_SYMBOL, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	} else if strings.compare ( ".", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DOT_STACK, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "dot", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DOT_STACK, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "dob", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.DOT_BYTECODE, st_dcreate_nil ( lines_idx[idx] ) ) )
	} else if strings.compare ( "nop", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.NO_OP, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	    /// DO NOTHING
	} else if strings.compare ( ":end", atom ) == 0 {
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.END_INSTRUCTION, st_dcreate ( false, lines_idx[idx] ), lines_idx[idx] ) )
	    
	} else if type_of ( strc.atoi( atom ) ) == int {
	    when DEBUG_MODE {
		fmt.println ("digit :: ", strc.atoi( atom ) )
	    }
	    qu.push_back ( bytecode, bc_dcreate ( ST_INS_Flags.BC_PUSH_VAL, st_dcreate ( atom, lines_idx[idx] ), lines_idx[idx] ) )
	} else {
	    fmt.println ("Error")
	    fmt.println ("linha :: ", atom, ":",idx)
	}
    }

    when DEBUG_MODE {
	fmt.println ( return_data_atoms )
    }
}

ring_front_add :: proc ( myQueue_Data : ^qu.Queue ( ST_Data ) ) {

    value_one := qu.pop_front ( myQueue_Data )
    value_two := qu.pop_front ( myQueue_Data )

    new_value := st_dplus ( value_one, value_two )

    qu.push_front ( myQueue_Data, new_value )

}

evalo :: proc ( ring_byte : ^qu.Queue ( ST_Bytecode ), ring_data : ^qu.Queue ( ST_Data ) ) {

    ins_local : ST_Bytecode

    for idx in 0..<MAX_INSTRUCTIONS {

	ins_local = qu.pop_front ( ring_byte )

	// fmt.print ( ins_local )
    }

    // ring_front_add (  )
    return 
}

queue_ring_language :: proc ( path : string ) {
    
    ring_bytecode := qu.Queue ( ST_Bytecode ){}

    qu.init ( &ring_bytecode, MIN_INSTRUCTION_SIZE )
    defer qu.destroy ( &ring_bytecode )
    
    ring_data := qu.Queue ( ST_Data ){}
    
    qu.init ( &ring_data, MIN_RING_SIZE )
    defer qu.destroy ( &ring_data )

    parser ( path, &ring_bytecode )

    when DEBUG_MODE {
	for i in 0..<qu.len(ring_bytecode) {

	    fmt.println ( qu.get( &ring_bytecode, i ) )
	}
    }


    evalo ( &ring_bytecode, &ring_data )
    
    fmt.println ( qu.cap ( ring_bytecode ) )
    fmt.println ( qu.len ( ring_bytecode ) )
    // fmt.println ( "data : ", qu.peek_front ( &ring_bytecode )^.param.value )
    
}


example_usage_queue :: proc () {

    
    myQueue_Data := qu.Queue ( ST_Data ){}
    
    qu.init ( &myQueue_Data, MIN_RING_SIZE )
    defer qu.destroy ( &myQueue_Data )
    
    qu.push_front ( &myQueue_Data, st_dcreate ( 1, 0 ) )
    qu.push_front ( &myQueue_Data, st_dcreate ( 2, 1 ) )
    qu.push_front ( &myQueue_Data, st_dcreate ( 3, 2 ) )
    qu.push_front ( &myQueue_Data, st_dcreate ( 4, 4 ) )

    /// suposed bytecoded part manipulation queue

    value_one := qu.pop_front ( &myQueue_Data )
    value_two := qu.pop_front ( &myQueue_Data )

    new_value := st_dplus ( value_one, value_two )

    qu.push_front ( &myQueue_Data, new_value )
    
    fmt.println ( qu.get ( &myQueue_Data, 0 ) )
    fmt.println ( qu.get ( &myQueue_Data, 1 ) )
    fmt.println ( qu.get ( &myQueue_Data, 2 ) )
    fmt.println ( qu.get ( &myQueue_Data, 3 ) )
    fmt.println ( qu.get ( &myQueue_Data, 4 ) )
    fmt.println ( qu.get ( &myQueue_Data, 5 ) )
    

    fmt.println ( qu.len ( myQueue_Data ) )

}


main :: proc () {

    if len(os.args) == 1 {
	queue_ring_language ( "test.deque" )
    } else {
	queue_ring_language ( os.args[1] )
    }
    // queue_ring_language ( "code.deque" )
    
    // queue_ring_language ( "stress.deque" )

    // test : [dynamic]string = {"some", "stuff", "goingon" }
    // fmt.println ( strings.concatenate(test[:]) )
    // example_usage_queue () ;
}

example_union_maybe :: proc() {
	fmt.println("\n#union based maybe")

	// NOTE: This is already built-in, and this is just a reimplementation to explain the behaviour
	Maybe :: union($T: typeid) {T}

	i: Maybe(u8)
	p: Maybe(^u8) // No tag is stored for pointers, nil is the sentinel value

	// Tag size will be as small as needed for the number of variants
	#assert(size_of(i) == size_of(u8) + size_of(u8))
	// No need to store a tag here, the `nil` state is shared with the variant's `nil`
	#assert(size_of(p) == size_of(^u8))

	i = 123
	x := i.?
	y, y_ok := p.?
	p = &x
	z, z_ok := p.?

	fmt.println(i, p)
	fmt.println(x, &x)
	fmt.println(y, y_ok)
	fmt.println(z, z_ok)
}
