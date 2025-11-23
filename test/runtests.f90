program test
    use TinyTOML
    use fort_test

    implicit none

    type(toml_object):: input
    integer(i32):: status, i
    character(len = 32):: filename
    character:: nl
    integer(i32):: num_failed = 0, exit_code = 0
    type(TestSet):: ts_basics, ts_options, ts_arrays
    logical:: verbose = .false.

    nl = new_line('a')

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

        basics = input%get("basics")
        call read_value(basics%get("int_var"), int_var)
        if (verbose) print "(A,g0)", "int_var: ", int_var

        call read_value(basics%get("float_var"), float_var)
        if (verbose) print "(A,g0)", "float_var: ", float_var

        call read_value(basics%get("string_var"), string_var)
        if (verbose) print "(A,g0)", "string_var: ", string_var

        call read_value(basics%get("int_arr"), int_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr: [", int_arr, "]"

        call read_value(basics%get("float_arr"), float_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "float_arr: [", float_arr, "]"

        call read_value(basics%get("int_arr_multiline"), int_arr_multiline)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr_multiline: [", int_arr_multiline, "]"

        ts_basics = new_testset( &
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
    end block

    block
        type(toml_object):: options, more_options, fruits, fruit, properties
        character(len = :), allocatable:: string_option, fruit_name, fruit_color
        character(len=10):: fruit_name_exp(4), fruit_color_exp(4)
        integer(i32):: num_array_elements, in_stock, fruit_in_stock_exp(4)
        real(f64), allocatable:: array_option(:), not_present_option(:)
        real(f64):: float_option, mass_g, fruit_mass_exp(4)
        integer(i32):: integer_option, another_option, test_ind
        logical:: bool_option
        type(Result), allocatable:: tests(:)

        allocate(tests(1024))
        test_ind = 1

        options = input%get("options")

        call read_value(options%get("string_option", error = .false.), string_option)
        tests(test_ind) = assert_eq(string_option, "a string")
        test_ind = test_ind + 1

        call read_value(options%get("float_option", error = .false.), float_option)
        tests(test_ind) = assert(isnan(float_option))
        test_ind = test_ind + 1

        call read_value(options%get("bool_option", error = .false.), bool_option)
        tests(test_ind) = assert_eq(bool_option, .false.)
        test_ind = test_ind + 1

        call read_value(options%get("integer_option", error = .false.), integer_option)
        tests(test_ind) = assert_eq(integer_option, 2)
        test_ind = test_ind + 1

        call read_value(options%get("array_option", error = .false.), array_option)
        num_array_elements = size(array_option)
        tests(test_ind) = assert_eq(num_array_elements, 2)
        tests(test_ind+1) = assert_eq(array_option(1), 1.0_f64)
        tests(test_ind+2) = assert_eq(array_option(2), -0.1e20_f64)
        test_ind = test_ind + 3

        call read_value(&
            options%get("not_present_option", error = .false.), &
            not_present_option, &
            default = (/1.0_f64, 2.0_f64, 3.0_f64/)&
        )
        tests(test_ind) = assert_eq(size(not_present_option), 3)
        tests(test_ind+1) = assert_eq(not_present_option(1), 1.0_f64)
        tests(test_ind+2) = assert_eq(not_present_option(2), 2.0_f64)
        tests(test_ind+3) = assert_eq(not_present_option(3), 3.0_f64)
        test_ind = test_ind + 4

        more_options = input%get("more-options")
        call read_value(more_options%get("another_option"), another_option)
        tests(test_ind) = assert_eq(another_option, 2)
        tests(test_ind+1) = assert(input%has_key("fruits"))
        test_ind = test_ind + 2

        fruits = input%get("fruits")

        fruit_name_exp = (/"apple ", "orange", "banana", "pomelo"/)
        fruit_mass_exp = (/200.828_f64, 156.2_f64, 90.0_f64, 150.0_f64 /)
        fruit_in_stock_exp = (/1, 3, 5, -1/)
        fruit_color_exp = (/"red   ", "orange", "yellow", "yellow"/)

        tests(test_ind) = assert_eq(num_children(fruits), 4)
        test_ind = test_ind + 1

        do i = 1, num_children(fruits)
            fruit = get(input%get("fruits"), i)

            tests(test_ind) = assert(fruit%has_key("properties"))
            test_ind = test_ind + 1

            properties = fruit%get("properties")
            call read_value(fruit%get("type"), fruit_name)
            call read_value(properties%get("in_stock"), in_stock)
            call read_value(properties%get("color"), fruit_color)
            call read_value(properties%get("mass_g"), mass_g)

            tests(test_ind) = assert_eq(fruit_name, strip(fruit_name_exp(i)))
            tests(test_ind+1) = assert_eq(in_stock, fruit_in_stock_exp(i))
            tests(test_ind+2) = assert_eq(fruit_color, strip(fruit_color_exp(i)))
            tests(test_ind+3) = assert_eq(mass_g, fruit_mass_exp(i))
            test_ind = test_ind + 4

            if (verbose) print*, ""
            if (verbose) print*, "Fruit: ", i
            if (verbose) print*, "Name: ", fruit_name
            if (verbose) print*, "Mass (g): ", mass_g
            if (verbose) print*, "Color: ", fruit_color
            if (verbose) print*, "In stock: ", in_stock
        end do

        ts_options = new_testset(tests(1:test_ind-1), "Options")

    end block

    block
        type(toml_object):: arrays, arr3
        integer(i32), allocatable:: arr1(:), long_arr(:)
        integer(i32):: arr1_exp(3)
        integer(i32):: test_ind = 1
        type(Result), allocatable:: tests(:)

        allocate(tests(1024))

        tests(test_ind) = assert(has_key(input, "complex-arrays"))
        test_ind = test_ind + 1
        arrays = input%get("complex-arrays")

        arr1_exp = (/1,2,3/)
    
        tests(test_ind) = assert(has_key(arrays, "arr1"))
        test_ind = test_ind + 1
        call read_value(arrays%get("arr1"), arr1)
        do i = 1, 3
            tests(test_ind) = assert_eq(arr1(i), arr1_exp(i))
            test_ind = test_ind+1
        end do

        tests(test_ind) = assert(has_key(arrays, "arr2"))
        test_ind = test_ind + 1
        tests(test_ind) = assert_eq(num_children(arrays%get("arr2")), 0)
        test_ind = test_ind + 1

        tests(test_ind) = assert(has_key(arrays, "arr3"))
        test_ind = test_ind + 1
        arr3 = arrays%get("arr3")
        tests(test_ind) = assert_eq(num_children(arr3), 3)
        ! TODO: string arrays don't work
        tests(test_ind) = assert_eq(arr3%value, '["ball", "red", "element",]')
        test_ind = test_ind + 1

        tests(test_ind) = assert(has_key(arrays, "long_arr"))
        test_ind = test_ind + 1
        call read_value(arrays%get("long_arr"), long_arr)
        do i = 1, size(long_arr)
            tests(test_ind) = assert_eq(long_arr(i), i)            
            test_ind = test_ind + 1
        end do

        ts_arrays = new_testset(tests(1:test_ind-1), "Arrays")
    end block


    call run_and_exit((/ts_basics, ts_options, ts_arrays/))

end program

