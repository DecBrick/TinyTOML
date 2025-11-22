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
        type(toml_object):: input
        integer(i32):: int_var
        real(f64):: float_var
        character(len=:), allocatable:: string_var
        type(TestSet):: ts

        input = parse_string(&
            "int_var = 2" // nl // &
            "float_var = 10.0" // nl // &
            "string_var = 'str'" &
        )

        call read_value(input%get("int_var"), int_var)
        call read_value(input%get("float_var"), float_var)
        call read_value(input%get("string_var"), string_var)

        ts = new_testset( &
            (/ &
                assert_eq(int_var, 2), &
                assert_eq(float_var, 10.0d0), &
                assert_eq(string_var, "str") &
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

