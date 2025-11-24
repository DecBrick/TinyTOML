program test
    use TinyTOML
    use fort_test

    implicit none

    type(toml_object):: input
    type(Testset), allocatable:: testsets(:)
    logical:: verbose = .false.

    allocate(testsets(0))

    ! Tokenizer
    block
        type(Testset):: ts
        character(len=:), allocatable:: content
        type(toml_tokenizer):: tokenizer

        call ts%init("Tokenizer")

        call tokenizer%init("")
        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "eof")

        content = &
            "[]= , " // C_NEWLINE // '   { s1 = "string 1", s2 = "string 2"} # a comment' // C_NEWLINE // &
            "# another comment # with a hash"   // C_NEWLINE // c_tab // &
            "bool_true = true"                  // C_NEWLINE // &
            "bool_false = false"                // C_NEWLINE // &
            "[[table.array]]"                   // C_NEWLINE // &
            "dec_int = 10_000"                  // C_NEWLINE // &
            "hex_int = 0xdeadbeef"              // C_NEWLINE // &
            "bin_int = 0b01"                    // C_NEWLINE // &
            "oct_int = 0o755"                   // C_NEWLINE // &
            "f1 = 3.14159e-20"                  // C_NEWLINE // &
            "f2 = inf"                          // C_NEWLINE // &
            "f3 = nan"                          // C_NEWLINE // &
            "i1 = +0x2f"                        // C_NEWLINE // &
            "f4 = -12.8"                        // C_NEWLINE // &
            "f5 = +nan"                         // C_NEWLINE // &
            "f6 = -inf"                         // C_NEWLINE // &
            ".7"                                // C_NEWLINE // &
            "7."                                // C_NEWLINE // &
            "1dzuew"                            // C_NEWLINE // &
            "3.e+20"                            // C_NEWLINE // &
            "+2e.+0"                            // C_NEWLINE // &
            "//"

        call tokenizer%init(content)

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "[")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "]")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, ",")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "{")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "s1")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "string")
        call assert_eq(ts, tokenizer%content, "string 1")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, ",")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "s2")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "string")
        call assert_eq(ts, tokenizer%content, "string 2")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "}")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "comment")
        call assert_eq(ts, tokenizer%content, " a comment")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "comment")
        call assert_eq(ts, tokenizer%content, " another comment # with a hash")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "bool_true")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "true")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "bool_false")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "false")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "[[")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "table")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, ".")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "array")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "]]")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "dec_int")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_dec")
        call assert_eq(ts, tokenizer%content, "10000")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "hex_int")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_hex")
        call assert_eq(ts, tokenizer%content, "deadbeef")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "bin_int")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_bin")
        call assert_eq(ts, tokenizer%content, "01")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "oct_int")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_oct")
        call assert_eq(ts, tokenizer%content, "755")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f1")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "3.14159d-20")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f2")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "inf")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f3")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "nan")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "i1")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_hex")
        call assert_eq(ts, tokenizer%content, "2f")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f4")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "-12.8")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f5")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "nan")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "ident")
        call assert_eq(ts, tokenizer%content, "f6")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "=")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "float")
        call assert_eq(ts, tokenizer%content, "-inf")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, ".")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "int_dec")
        call assert_eq(ts, tokenizer%content, "7")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "error")
        call assert_eq(ts, tokenizer%content, "7.")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "error")
        call assert_eq(ts, tokenizer%content, "1dzuew")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "error")
        call assert_eq(ts, tokenizer%content, "3.e+20")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "error")
        call assert_eq(ts, tokenizer%content, "2e.+0")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "newline")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "error")
        call assert_eq(ts, tokenizer%content, "//")

        call read_token(tokenizer)
        call assert_eq(ts, tokenizer%tok_kind, "eof")

        testsets = [testsets, ts]
    end block

    block
        type(toml_object):: obj
        type(Testset):: ts

        call ts%init("Basics 1")
        obj = parse_string_2("a = 1")

        testsets = [testsets, ts]
    end block

    input = parse_file("test/tests.toml")

    ! Some basic key-value pair tests
    block
        type(toml_object):: basics
        integer(i32):: int_var
        real(f64):: float_var
        character(len=:), allocatable:: string_var
        integer(i32), allocatable:: int_arr(:), int_arr_multiline(:)
        real(f64), allocatable:: float_arr(:)
        integer(i32):: int_arr_exp(3) = (/1, 2000, 50000000/)
        integer(i32):: int_arr_ml_exp(3) = (/ 1, 2, 3 /)
        real(f64):: float_arr_exp(3) = (/-1.d0, 3.14159d0, 9.1d-31/)
        type(Testset):: ts

        call ts%init("Basics")

        basics = input%get("basics")
        call read_value(basics%get("int_var"), int_var)
        if (verbose) print "(A,g0)", "int_var: ", int_var
        call assert_eq(ts, int_var, 2)

        call read_value(basics%get("float_var"), float_var)
        if (verbose) print "(A,g0)", "float_var: ", float_var
        call assert_eq(ts, float_var, 10.0_f64)

        call read_value(basics%get("string_var"), string_var)
        if (verbose) print "(A,g0)", "string_var: ", string_var
        call assert_eq(ts, string_var, "string")

        call read_value(basics%get("int_arr"), int_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr: [", int_arr, "]"
        call assert_eq(ts, int_arr, int_arr_exp)

        call read_value(basics%get("float_arr"), float_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "float_arr: [", float_arr, "]"
        call assert_eq(ts, float_arr, float_arr_exp)

        call read_value(basics%get("int_arr_multiline"), int_arr_multiline)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr_multiline: [", int_arr_multiline, "]"
        call assert_eq(ts, int_arr_multiline, int_arr_ml_exp)

        testsets = [testsets, ts]
    end block

    block
        type(toml_object):: options, more_options, fruits, fruit, properties
        character(len = :), allocatable:: string_option, fruit_name, fruit_color
        character(len=10):: fruit_name_exp(4), fruit_color_exp(4)
        integer(i32):: num_array_elements, in_stock, fruit_in_stock_exp(4)
        real(f64), allocatable:: array_option(:), not_present_option(:)
        real(f64):: float_option, mass_g, fruit_mass_exp(4)
        integer(i32):: integer_option, another_option, test_ind, i
        logical:: bool_option
        type(Testset):: ts

        call ts%init("Options")

        options = input%get("options")

        call read_value(options%get("string_option", error = .false.), string_option)
        call assert_eq(ts, string_option, "a string")

        call read_value(options%get("float_option", error = .false.), float_option)
        call assert(ts, isnan(float_option))

        call read_value(options%get("bool_option", error = .false.), bool_option)
        call assert_eq(ts, bool_option, .false.)

        call read_value(options%get("integer_option", error = .false.), integer_option)
        call assert_eq(ts, integer_option, 2)

        call read_value(options%get("array_option", error = .false.), array_option)
        call assert_eq(ts, array_option, (/1.0_f64, -0.1e20_f64/))

        call read_value(&
            options%get("not_present_option", error = .false.), &
            not_present_option, &
            default = (/1.0_f64, 2.0_f64, 3.0_f64/)&
        )
        call assert_eq(ts, not_present_option, (/1.0_f64, 2.0_f64, 3.0_f64/))

        more_options = input%get("more-options")
        call read_value(more_options%get("another_option"), another_option)
        call assert_eq(ts, another_option, 2)

        call assert(ts, input%has_key("fruits")) 
        fruits = input%get("fruits")

        fruit_name_exp = (/"apple ", "orange", "banana", "pomelo"/)
        fruit_mass_exp = (/200.828_f64, 156.2_f64, 90.0_f64, 150.0_f64 /)
        fruit_in_stock_exp = (/1, 3, 5, -1/)
        fruit_color_exp = (/"red   ", "orange", "yellow", "yellow"/)

        call assert_eq(ts, num_children(fruits), 4)

        do i = 1, num_children(fruits)
            fruit = get(input%get("fruits"), i)

            call assert(ts, fruit%has_key("properties"))
            properties = fruit%get("properties")

            call read_value(fruit%get("type"), fruit_name)
            call assert_eq(ts, fruit_name, strip(fruit_name_exp(i)))

            call read_value(properties%get("in_stock"), in_stock)
            call assert_eq(ts, in_stock, fruit_in_stock_exp(i))

            call read_value(properties%get("color"), fruit_color)
            call assert_eq(ts, fruit_color, strip(fruit_color_exp(i)))

            call read_value(properties%get("mass_g"), mass_g)
            call assert_eq(ts, mass_g, fruit_mass_exp(i))

            if (verbose) print*, ""
            if (verbose) print*, "Fruit: ", i
            if (verbose) print*, "Name: ", fruit_name
            if (verbose) print*, "Mass (g): ", mass_g
            if (verbose) print*, "Color: ", fruit_color
            if (verbose) print*, "In stock: ", in_stock
        end do

        testsets = [testsets, ts]

    end block

    block
        type(toml_object):: arrays, arr3
        integer(i32), allocatable:: arr1(:), long_arr(:)
        integer(i32):: j, long_arr_exp(256) = (/ (j, j=1, 256) /)
        integer(i32):: arr1_exp(3)
        type(Testset):: ts

        call ts%init("Arrays")
        call assert(ts, has_key(input, "complex-arrays"))

        arrays = input%get("complex-arrays")

        call assert(ts, arrays%has_key("arr1"))
        call read_value(arrays%get("arr1"), arr1)
        call assert_eq(ts, arr1, [1, 2, 3])

        call assert(ts, arrays%has_key("arr2"))
        call assert_eq(ts, num_children(arrays%get("arr2")), 0)

        call assert(ts, arrays%has_key("arr3"))
        arr3 = arrays%get("arr3")
        call assert_eq(ts, num_children(arr3), 3)
        ! TODO: string arrays don't work
        call assert_eq(ts, arr3%value, '["ball", "red", "element",]')

        call assert(ts, arrays%has_key("long_arr"))
        call read_value(arrays%get("long_arr"), long_arr)
        call assert_eq(ts, long_arr, long_arr_exp)

        testsets = [testsets, ts]
    end block

    ! Float parsing
    block
        type(toml_object):: float_obj
        real(f64):: f, infinity = huge(1.0_f64)
        type(Testset):: ts

        call ts%init("Floats")
        float_obj = input%get("floats")

        call read_value(float_obj%get("inf"), f)
        call assert_gt(ts, f, infinity)

        call read_value(float_obj%get("pos_inf"), f)
        call assert_gt(ts, f, infinity)

        call read_value(float_obj%get("neg_inf"), f)
        call assert_lt(ts, f, -infinity)

        call read_value(float_obj%get("neg_nan"), f)
        call assert(ts, isnan(f))

        call read_value(float_obj%get("pos_nan"), f)
        call assert(ts, isnan(f))

        call read_value(float_obj%get("nan"), f)
        call assert(ts, isnan(f))

        call read_value(float_obj%get("pos_zero"), f)
        call assert_eq(ts, f, 0.0_f64)

        call read_value(float_obj%get("neg_zero"), f)
        call assert_eq(ts, f, -0.0_f64)

        call read_value(float_obj%get("flt1"), f)
        call assert_eq(ts, f, 1.0_f64)

        call read_value(float_obj%get("flt2"), f)
        call assert_eq(ts, f, 3.1415_f64)

        call read_value(float_obj%get("flt3"), f)
        call assert_eq(ts, f, -0.01_f64)

        call read_value(float_obj%get("flt4"), f)
        call assert_eq(ts, f, 5e22_f64)

        call read_value(float_obj%get("flt5"), f)
        call assert_eq(ts, f, 1e6_f64)

        call read_value(float_obj%get("flt6"), f)
        call assert_eq(ts, f, -2e-2_f64)

        call read_value(float_obj%get("flt7"), f)
        call assert_eq(ts, f, 6.626e-34_f64)

        call read_value(float_obj%get("flt8"), f)
        call assert_eq(ts, f, 224617.445991228_f64)

        testsets = [testsets, ts]

    end block

    ! Int parsing
    block
        type(toml_object):: int_obj
        integer(i64):: num
        type(Testset):: ts

        call ts%init("Integers")
        int_obj = input%get("integers")

        call read_value(int_obj%get("int1"), num)
        call assert_eq(ts, num, 99_i64)

        call read_value(int_obj%get("int2"), num)
        call assert_eq(ts, num, 42_i64)

        call read_value(int_obj%get("int3"), num)
        call assert_eq(ts, num, 0_i64)

        call read_value(int_obj%get("int4"), num)
        call assert_eq(ts, num, -17_i64)

        call read_value(int_obj%get("int5"), num)
        call assert_eq(ts, num, 1000_i64)

        call read_value(int_obj%get("int6"), num)
        call assert_eq(ts, num, 5349221_i64)

        call read_value(int_obj%get("int7"), num)
        call assert_eq(ts, num, 5349221_i64)

        call read_value(int_obj%get("int8"), num)
        call assert_eq(ts, num, 12345_i64)

        ! Hexadecimals
        call read_value(int_obj%get("hex0"), num)
        call assert_eq(ts, num, int(z'A', kind=i64))

        call read_value(int_obj%get("hex1"), num)
        call assert_eq(ts, num, int(z'DEADBEEF', kind=i64))

        call read_value(int_obj%get("hex2"), num)
        call assert_eq(ts, num, int(z'DEADBEEF', kind=i64))

        call read_value(int_obj%get("hex3"), num)
        call assert_eq(ts, num, int(z'DEADBEEF', kind=i64))

        ! Octals
        call read_value(int_obj%get("oct1"), num)
        call assert_eq(ts, num, int(o'01234567', kind=i64))

        call read_value(int_obj%get("oct2"), num)
        call assert_eq(ts, num, int(o'755', kind=i64))

        ! Binaries
        call read_value(int_obj%get("bin1"), num)
        call assert_eq(ts, num, int(b'11010110', kind=i64))

        testsets = [testsets, ts]

    end block

    call run_and_exit(testsets)

end program

