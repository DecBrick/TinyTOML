program test
    use TinyTOML
    use fort_test

    implicit none

    integer(i32):: status, i
    character(len = 32):: filename
    character:: nl
    integer(i32):: num_failed = 0, exit_code = 0

    nl = new_line('a')

    filename = "test/example.toml"

    ! Some basic key-value pair tests
    block
        type(toml_object):: input, object_1
        integer(i32):: int_var
        real(f64):: float_var
        character(len=:), allocatable:: string_var
        integer(i32), allocatable:: int_arr(:), int_arr_multiline(:)
        real(f64), allocatable:: float_arr(:)
        type(TestSet):: ts
        integer(i32):: int_arr_exp(3) = (/1, 2000, 50000000/)
        integer(i32):: int_arr_ml_exp(3) = (/ 1, 2, 3 /)
        real(f64):: float_arr_exp(3) = (/-1.d0, 3.14159d0, 9.1d-31/)
        logical:: verbose = .false.

        input = parse_file("test/basics.toml")

        object_1 = input%get("basics")
        call read_value(object_1%get("int_var"), int_var)
        if (verbose) print "(A,g0)", "int_var: ", int_var

        call read_value(object_1%get("float_var"), float_var)
        if (verbose) print "(A,g0)", "float_var: ", float_var

        call read_value(object_1%get("string_var"), string_var)
        if (verbose) print "(A,g0)", "string_var: ", string_var

        call read_value(object_1%get("int_arr"), int_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr: [", int_arr, "]"

        call read_value(object_1%get("float_arr"), float_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "float_arr: [", float_arr, "]"

        call read_value(object_1%get("int_arr_multiline"), int_arr_multiline)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr_multiline: [", int_arr_multiline, "]"

        ts = new_testset( &
            (/ &
                assert_eq(int_var, 2), &
                assert_eq(float_var, 10.0d0), &
                assert_eq(string_var, "string"), &
                assert(all(int_arr .eq. int_arr_exp)), &
                assert(all(float_arr .eq. float_arr_exp)), &
                assert(all(int_arr_multiline .eq. int_arr_ml_exp)), &
                assert(.true.) &
            /), &
            name = "Variables" & 
        )

        num_failed = num_failed + run_all((/ts/))
    end block

    if (num_failed > 0) then
        exit_code = 1
    end if

    call exit(exit_code)

end program

