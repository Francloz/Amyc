(module 
  (import "system" "printInt" (func $Std_printInt (param i32) (result i32)))
  (import "system" "printString" (func $Std_printString (param i32) (result i32)))
  (import "system" "readString0" (func $js_readString0 (param i32) (result i32)))
  (import "system" "readInt" (func $Std_readInt (result i32)))
  (import "system" "mem" (memory 100))
  (global (mut i32) i32.const 0) 

  (func $String_concat (param i32 i32) (result i32) (local i32 i32)
    get_global 0
    set_local 3
    get_local 0
    set_local 2
    loop $label_1
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_1
      else
      end
    end
    get_local 1
    set_local 2
    loop $label_2
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_2
      else
      end
    end
    loop $label_0
      get_local 3
      i32.const 0
      i32.store8
      get_local 3
      i32.const 4
      i32.rem_s
      if
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        br $label_0
      else
      end
    end
    get_global 0
    get_local 3
    i32.const 1
    i32.add
    set_global 0
  )

  (func $Std_digitToString (param i32) (result i32) 
    get_global 0
    get_local 0
    i32.const 48
    i32.add
    i32.store
    get_global 0
    get_global 0
    i32.const 4
    i32.add
    set_global 0
  )

  (func $Std_readString (result i32) 
    get_global 0
    get_global 0
    call $js_readString0
    set_global 0
  )

  (func $Arithmetic_pow (param i32 i32) (result i32) (local i32)
    get_local 1
    i32.const 0
    i32.eq
    if (result i32)
      get_local 1
      i32.const 2
      i32.rem_s
      i32.const 0
      i32.eq
      if (result i32)
        get_local 0
        unreachable
        i32.mul
      else
        unreachable
        set_local 2
        get_local 2
        get_local 2
        i32.mul
      end
    else
      i32.const 1
    end
  )

  (func $Arithmetic_gcd (param i32 i32) (result i32) 
    get_local 0
    i32.const 0
    i32.eq
    get_local 1
    i32.const 0
    i32.eq
    i32.or
    if (result i32)
      get_local 0
      get_local 1
      i32.lt_s
      if (result i32)
        unreachable
      else
        unreachable
      end
    else
      get_local 0
      get_local 1
      i32.add
    end
  )
  (export "Arithmetic_main" (func $Arithmetic_main))
  (func $Arithmetic_main 
    unreachable
    unreachable
    unreachable
    unreachable
    unreachable
    unreachable
    unreachable
    unreachable
    drop
  )

  (func $L_isEmpty (param i32) (result i32) 
    unreachable
  )

  (func $L_length (param i32) (result i32) 
    unreachable
  )

  (func $L_head (param i32) (result i32) 
    unreachable
  )

  (func $L_headOption (param i32) (result i32) 
    unreachable
  )

  (func $L_reverse (param i32) (result i32) 
    unreachable
  )

  (func $L_reverseAcc (param i32 i32) (result i32) 
    unreachable
  )

  (func $L_indexOf (param i32 i32) (result i32) 
    unreachable
  )

  (func $L_range (param i32 i32) (result i32) 
    get_local 1
    get_local 0
    i32.lt_s
    if (result i32)
      unreachable
    else
      unreachable
    end
  )

  (func $L_sum (param i32) (result i32) 
    unreachable
  )

  (func $L_concat (param i32 i32) (result i32) 
    unreachable
  )

  (func $L_contains (param i32 i32) (result i32) 
    unreachable
  )

  (func $L_merge (param i32 i32) (result i32) 
    unreachable
  )

  (func $L_split (param i32) (result i32) 
    unreachable
  )

  (func $L_mergeSort (param i32) (result i32) 
    unreachable
  )

  (func $L_toString (param i32) (result i32) 
    unreachable
  )

  (func $L_toString1 (param i32) (result i32) 
    unreachable
  )

  (func $L_take (param i32 i32) (result i32) 
    get_local 1
    i32.const 0
    i32.le_s
    if (result i32)
      unreachable
    else
      unreachable
    end
  )

  (func $Std_printBoolean (param i32) (result i32) 
    unreachable
  )

  (func $Std_intToString (param i32) (result i32) (local i32 i32)
    get_local 0
    i32.const 0
    i32.lt_s
    if (result i32)
      get_local 0
      i32.const 10
      i32.rem_s
      set_local 1
      get_local 0
      i32.const 10
      i32.div_s
      set_local 2
      get_local 2
      i32.const 0
      i32.eq
      if (result i32)
        unreachable
        unreachable
        get_global 0
        set_local 3
        get_local 0
        set_local 2
        loop $label_1
          get_local 2
          i32.load8_u
          if
            get_local 3
            get_local 2
            i32.load8_u
            i32.store8
            get_local 3
            i32.const 1
            i32.add
            set_local 3
            get_local 2
            i32.const 1
            i32.add
            set_local 2
            br $label_1
          else
          end
        end
        get_local 1
        set_local 2
        loop $label_2
          get_local 2
          i32.load8_u
          if
            get_local 3
            get_local 2
            i32.load8_u
            i32.store8
            get_local 3
            i32.const 1
            i32.add
            set_local 3
            get_local 2
            i32.const 1
            i32.add
            set_local 2
            br $label_2
          else
          end
        end
        loop $label_0
          get_local 3
          i32.const 0
          i32.store8
          get_local 3
          i32.const 4
          i32.rem_s
          if
            get_local 3
            i32.const 1
            i32.add
            set_local 3
            br $label_0
          else
          end
        end
        get_global 0
        get_local 3
        i32.const 1
        i32.add
        set_global 0
      else
        unreachable
      end
    else
      get_global 0
      i32.const 0
      i32.add
      i32.const 45
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 4
      i32.add
      set_global 0
      unreachable
      get_global 0
      set_local 3
      get_local 0
      set_local 2
      loop $label_1
        get_local 2
        i32.load8_u
        if
          get_local 3
          get_local 2
          i32.load8_u
          i32.store8
          get_local 3
          i32.const 1
          i32.add
          set_local 3
          get_local 2
          i32.const 1
          i32.add
          set_local 2
          br $label_1
        else
        end
      end
      get_local 1
      set_local 2
      loop $label_2
        get_local 2
        i32.load8_u
        if
          get_local 3
          get_local 2
          i32.load8_u
          i32.store8
          get_local 3
          i32.const 1
          i32.add
          set_local 3
          get_local 2
          i32.const 1
          i32.add
          set_local 2
          br $label_2
        else
        end
      end
      loop $label_0
        get_local 3
        i32.const 0
        i32.store8
        get_local 3
        i32.const 4
        i32.rem_s
        if
          get_local 3
          i32.const 1
          i32.add
          set_local 3
          br $label_0
        else
        end
      end
      get_global 0
      get_local 3
      i32.const 1
      i32.add
      set_global 0
    end
  )

  (func $Std_booleanToString (param i32) (result i32) 
    get_local 0
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 102
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 97
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 108
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 115
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    else
      get_global 0
      i32.const 0
      i32.add
      i32.const 116
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 117
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    end
  )

  (func $O_isDefined (param i32) (result i32) 
    unreachable
  )

  (func $O_get (param i32) (result i32) 
    unreachable
  )

  (func $O_getOrElse (param i32 i32) (result i32) 
    unreachable
  )

  (func $O_orElse (param i32 i32) (result i32) 
    unreachable
  )

  (func $O_toList (param i32) (result i32) 
    unreachable
  )
)