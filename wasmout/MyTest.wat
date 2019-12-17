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
  (export "MyTest_main" (func $MyTest_main))
  (func $MyTest_main (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    get_global 0
    i32.const 0
    i32.store
    get_global 0
    i32.const 4
    i32.add
    set_global 0
    i32.const 1
    get_global 0
    set_local 1
    get_global 0
    i32.const 1
    i32.store
    get_global 0
    i32.const 4
    i32.add
    set_global 0
    set_local 2
    get_global 0
    get_local 2
    i32.store
    get_global 0
    i32.const 4
    i32.add
    set_global 0
    set_local 3
    get_global 0
    get_local 3
    i32.store
    get_global 0
    i32.const 4
    i32.add
    set_global 0
    get_local 1
    set_local 0
    get_local 0
    set_local 5
    block $EndMatch
      block $EndPat
        get_local 5
        i32.const 1
        set_local 7
        get_global 0
        i32.const 0
        i32.add
        i32.const 77
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 109
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 121
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 32
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
        i32.const 0
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 4
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 8
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 12
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 10
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 76
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 99
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 97
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 108
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 12
        i32.add
        set_global 0
        call $String_concat
        get_local 6
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        get_local 8
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        get_local 9
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        call $Std_printString
        drop
        set_local 8
        get_global 0
        i32.const 0
        i32.add
        i32.const 83
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 105
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 103
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 112
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 58
        i32.store8
        get_global 0
        i32.const 12
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 13
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 14
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 15
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 16
        i32.add
        set_global 0
        call $Std_printString
        drop
        i32.const 8
        call $Std_digitToString
        call $Std_printString
        drop
        get_local 8
        i32.const 2
        i32.const 4
        i32.mul
        i32.add
        i32.load
        get_local 8
        i32.const 1
        i32.const 4
        i32.mul
        i32.add
        i32.load
        set_local 9
        get_global 0
        i32.const 0
        i32.add
        i32.const 83
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 100
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 46
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
        call $Std_printString
        drop
        get_global 0
        i32.const 0
        i32.add
        i32.const 77
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 109
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 121
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 32
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
        i32.const 0
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 4
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 8
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        i32.const 12
        i32.load
        call $Std_digitToString
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        get_global 0
        i32.const 0
        i32.add
        i32.const 10
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 76
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 99
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 97
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 108
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 12
        i32.add
        set_global 0
        call $String_concat
        get_local 6
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        get_local 8
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        get_local 9
        call $Std_digitToString
        get_global 0
        i32.const 0
        i32.add
        i32.const 32
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
        call $String_concat
        call $String_concat
        call $Std_printString
        drop
        drop
        get_local 7
        call $Std_digitToString
        call $Std_printString
        drop
        get_local 8
        i32.load
        i32.const 1
        i32.eq
        get_local 7
        i32.and
        set_local 7
        get_local 7
        if
        else
          get_global 0
          i32.const 0
          i32.add
          i32.const 83
          i32.store8
          get_global 0
          i32.const 1
          i32.add
          i32.const 107
          i32.store8
          get_global 0
          i32.const 2
          i32.add
          i32.const 105
          i32.store8
          get_global 0
          i32.const 3
          i32.add
          i32.const 112
          i32.store8
          get_global 0
          i32.const 4
          i32.add
          i32.const 112
          i32.store8
          get_global 0
          i32.const 5
          i32.add
          i32.const 105
          i32.store8
          get_global 0
          i32.const 6
          i32.add
          i32.const 110
          i32.store8
          get_global 0
          i32.const 7
          i32.add
          i32.const 103
          i32.store8
          get_global 0
          i32.const 8
          i32.add
          i32.const 32
          i32.store8
          get_global 0
          i32.const 9
          i32.add
          i32.const 109
          i32.store8
          get_global 0
          i32.const 10
          i32.add
          i32.const 97
          i32.store8
          get_global 0
          i32.const 11
          i32.add
          i32.const 116
          i32.store8
          get_global 0
          i32.const 12
          i32.add
          i32.const 99
          i32.store8
          get_global 0
          i32.const 13
          i32.add
          i32.const 104
          i32.store8
          get_global 0
          i32.const 14
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          i32.const 15
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          get_global 0
          i32.const 16
          i32.add
          set_global 0
          call $Std_printString
          drop
          br $EndPat
        end
        get_global 0
        i32.const 0
        i32.add
        i32.const 67
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 105
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 117
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 105
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 103
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 12
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 13
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 14
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 15
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 16
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 17
        i32.add
        i32.const 117
        i32.store8
        get_global 0
        i32.const 18
        i32.add
        i32.const 108
        i32.store8
        get_global 0
        i32.const 19
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 20
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 21
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 22
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 23
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 24
        i32.add
        set_global 0
        call $Std_printString
        drop
        get_local 9
        set_local 10
        get_local 10
        get_global 0
        i32.const 0
        i32.add
        i32.const 82
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 117
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 108
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 116
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 120
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 112
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 114
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 12
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 13
        i32.add
        i32.const 105
        i32.store8
        get_global 0
        i32.const 14
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 15
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 16
        i32.add
        i32.const 58
        i32.store8
        get_global 0
        i32.const 17
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 18
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 19
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 20
        i32.add
        set_global 0
        call $Std_printString
        drop
        call $Std_digitToString
        call $Std_printString
        drop
        get_local 10
        set_local 6
        br $EndMatch
      end
      get_global 0
      i32.const 0
      i32.add
      i32.const 77
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 97
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 116
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 99
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 104
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 32
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 8
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 9
      i32.add
      i32.const 111
      i32.store8
      get_global 0
      i32.const 10
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 11
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 12
      i32.add
      set_global 0
      call $Std_printString
      unreachable
    end
    get_local 6
    get_global 0
    i32.const 0
    i32.add
    i32.const 82
    i32.store8
    get_global 0
    i32.const 1
    i32.add
    i32.const 101
    i32.store8
    get_global 0
    i32.const 2
    i32.add
    i32.const 115
    i32.store8
    get_global 0
    i32.const 3
    i32.add
    i32.const 117
    i32.store8
    get_global 0
    i32.const 4
    i32.add
    i32.const 108
    i32.store8
    get_global 0
    i32.const 5
    i32.add
    i32.const 116
    i32.store8
    get_global 0
    i32.const 6
    i32.add
    i32.const 32
    i32.store8
    get_global 0
    i32.const 7
    i32.add
    i32.const 115
    i32.store8
    get_global 0
    i32.const 8
    i32.add
    i32.const 116
    i32.store8
    get_global 0
    i32.const 9
    i32.add
    i32.const 111
    i32.store8
    get_global 0
    i32.const 10
    i32.add
    i32.const 114
    i32.store8
    get_global 0
    i32.const 11
    i32.add
    i32.const 101
    i32.store8
    get_global 0
    i32.const 12
    i32.add
    i32.const 100
    i32.store8
    get_global 0
    i32.const 13
    i32.add
    i32.const 32
    i32.store8
    get_global 0
    i32.const 14
    i32.add
    i32.const 97
    i32.store8
    get_global 0
    i32.const 15
    i32.add
    i32.const 116
    i32.store8
    get_global 0
    i32.const 16
    i32.add
    i32.const 58
    i32.store8
    get_global 0
    i32.const 17
    i32.add
    i32.const 32
    i32.store8
    get_global 0
    i32.const 18
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    i32.const 19
    i32.add
    i32.const 0
    i32.store8
    get_global 0
    get_global 0
    i32.const 20
    i32.add
    set_global 0
    call $Std_printString
    drop
    i32.const 6
    call $Std_digitToString
    call $Std_printString
    drop
    set_local 4
    get_local 4
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 89
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 115
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
      call $Std_printString
      unreachable
    else
      get_global 0
      i32.const 0
      i32.add
      i32.const 78
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 111
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
      call $Std_printString
      unreachable
    end
    drop
  )
)