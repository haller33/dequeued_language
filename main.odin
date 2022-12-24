package dequeue

import "core:fmt"
import "core:mem"
import qu "core:container/queue"

ST_INS_Flags :: enum {
    // instructions
    BC_PUSH_VAL,
    BC_LOAD_VAL,
    BC_DROP_VAL,
    BC_TWO_DROP_VAL,
    BC_DUBLE_VAL,
    BC_TWO_DUBLE_VAL,
    BC_SWAP_VAL,
    BC_TWO_SWAP_VAL,
    BC_OVER_VAL,
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
    BC_IN_TWO_ROT_VAL,
    BC_IN_HALF_ROT_VAL,
    // operations on the "stack" ~ queue
    ADD,
    PULTIPLY,
    SUBTRACT,
    RETURN,
    WHILE_LOOP,
    END_WHILE,
    LESSTHAN_VAL,
    EQUALTHAN_VAL,
    APPLY_PROCEDURE,
    NOT_VAL,
    // special structions
    DO_SYMBOL,
    DOT_STACK,
    DOT_BYTECODE,
    NO_OP,
    DUP_FIRST_INSTRUCTION,
    ROT_FIRST_INSTRUCTION,
    DUP_LAST_INSTRUCTION,
    ROT_LAST_INSTRUCTION,
    END_INSTRUCTION,
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
    value : union {int, bool, string},
    arg : ST_Flags,
    line : int,
}

ST_Bytecode :: struct {
    
    enable : bool,
    Instruction : ST_INS_Flags,
    param : ST_Data,
    line : int,
}

Error :: struct {

    has_error : bool,
    place : string,
    line : int,
    reason : string,
    value_expected : union {int, bool, string},
    value_get : union {int, bool, string},
    data_st : ST_Data,
}



st_dset :: proc ( tmp : ST_Data, value : union {int, bool, string} ) -> ST_Data {

    new_data : ST_Data
    
    new_data = ST_Data{ value = value, arg = tmp.arg, enable = tmp.enable, line = tmp.line }

    return new_data
}


st_dcreate :: proc (arg_c : union {int, bool, string}, line_of : int = 0) -> ST_Data {

    switch in arg_c {

    case int: return ST_Data{ value = arg_c, arg = ST_Flags.ST_NUM, enable = true, line = line_of }
    case bool: return ST_Data{ value = arg_c, arg = ST_Flags.ST_BOOL, enable = true, line = line_of }
    case string: return ST_Data{ value = arg_c, arg = ST_Flags.ST_ARG, enable = true, line = line_of }
				      
    }
    return ST_Data { enable = true, value = false, arg = ST_Flags.ST_NIL}
}


parser :: proc () {

    
}

st_dplus :: proc ( value_one, value_two : ST_Data ) -> ST_Data {

    
    value_sum : union {int, bool, string}
    flag_error : bool = false

    value_one_digit := value_one.value.(int)
    value_two_digit := value_two.value.(int)
	
    value_sum = value_one_digit + value_two_digit
    
    swap_value := st_dcreate ( value_sum, value_two.line )

    return swap_value
//    return ST_Data {}
}

main_two :: proc () {

    

    union_maybe ();
}

example_usage_queue :: proc () {

    
    myQueue_Data := qu.Queue ( ST_Data ){}
    
    qu.init ( &myQueue_Data )
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
