program test
    use TinyTOML
    use fort_test

    implicit none

    type(toml_object):: input
    type(Testset):: ts_basics, ts_options, ts_arrays, ts_floats, ts_ints
    logical:: verbose = .false.

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

        ts_basics = Testset("Basics")

        basics = input%get("basics")
        call read_value(basics%get("int_var"), int_var)
        if (verbose) print "(A,g0)", "int_var: ", int_var
        call assert_eq(ts_basics, int_var, 2)

        call read_value(basics%get("float_var"), float_var)
        if (verbose) print "(A,g0)", "float_var: ", float_var
        call assert_eq(ts_basics, float_var, 10.0_f64)

        call read_value(basics%get("string_var"), string_var)
        if (verbose) print "(A,g0)", "string_var: ", string_var
        call assert_eq(ts_basics, string_var, "string")

        call read_value(basics%get("int_arr"), int_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr: [", int_arr, "]"
        call assert_eq(ts_basics, int_arr, int_arr_exp)

        call read_value(basics%get("float_arr"), float_arr)
        if (verbose) print "(A,*(g0,:,','),A)", "float_arr: [", float_arr, "]"
        call assert_eq(ts_basics, float_arr, float_arr_exp)

        call read_value(basics%get("int_arr_multiline"), int_arr_multiline)
        if (verbose) print "(A,*(g0,:,','),A)", "int_arr_multiline: [", int_arr_multiline, "]"
        call assert_eq(ts_basics, int_arr_multiline, int_arr_ml_exp)
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
        type(Result), allocatable:: tests(:)

        ts_options = Testset("Options")
    
        options = input%get("options")

        call read_value(options%get("string_option", error = .false.), string_option)
        call assert_eq(ts_options, string_option, "a string")

        call read_value(options%get("float_option", error = .false.), float_option)
        call assert(ts_options, isnan(float_option))

        call read_value(options%get("bool_option", error = .false.), bool_option)
        call assert_eq(ts_options, bool_option, .false.)

        call read_value(options%get("integer_option", error = .false.), integer_option)
        call assert_eq(ts_options, integer_option, 2)

        call read_value(options%get("array_option", error = .false.), array_option)
        call assert_eq(ts_options, array_option, (/1.0_f64, -0.1e20_f64/))

        call read_value(&
            options%get("not_present_option", error = .false.), &
            not_present_option, &
            default = (/1.0_f64, 2.0_f64, 3.0_f64/)&
        )
        call assert_eq(ts_options, not_present_option, (/1.0_f64, 2.0_f64, 3.0_f64/))

        more_options = input%get("more-options")
        call read_value(more_options%get("another_option"), another_option)
        call assert_eq(ts_options, another_option, 2)

        call assert(ts_options, input%has_key("fruits")) 
        fruits = input%get("fruits")

        fruit_name_exp = (/"apple ", "orange", "banana", "pomelo"/)
        fruit_mass_exp = (/200.828_f64, 156.2_f64, 90.0_f64, 150.0_f64 /)
        fruit_in_stock_exp = (/1, 3, 5, -1/)
        fruit_color_exp = (/"red   ", "orange", "yellow", "yellow"/)

        call assert_eq(ts_options, num_children(fruits), 4)

        do i = 1, num_children(fruits)
            fruit = get(input%get("fruits"), i)

            call assert(ts_options, fruit%has_key("properties"))
            properties = fruit%get("properties")

            call read_value(fruit%get("type"), fruit_name)
            call assert_eq(ts_options, fruit_name, strip(fruit_name_exp(i)))

            call read_value(properties%get("in_stock"), in_stock)
            call assert_eq(ts_options, in_stock, fruit_in_stock_exp(i))

            call read_value(properties%get("color"), fruit_color)
            call assert_eq(ts_options, fruit_color, strip(fruit_color_exp(i)))

            call read_value(properties%get("mass_g"), mass_g)
            call assert_eq(ts_options, mass_g, fruit_mass_exp(i))

            if (verbose) print*, ""
            if (verbose) print*, "Fruit: ", i
            if (verbose) print*, "Name: ", fruit_name
            if (verbose) print*, "Mass (g): ", mass_g
            if (verbose) print*, "Color: ", fruit_color
            if (verbose) print*, "In stock: ", in_stock
        end do

    end block

    block
        type(toml_object):: arrays, arr3
        integer(i32), allocatable:: arr1(:), long_arr(:)
        integer(i32):: test_ind = 1
        integer(i32):: j, long_arr_exp(256) = (/ (j, j=1, 256) /)
        integer(i32):: arr1_exp(3)

        ts_arrays = Testset("Arrays")

        call assert(ts_arrays, has_key(input, "complex-arrays"))

        arrays = input%get("complex-arrays")

        arr1_exp = (/1,2,3/)

        call assert(ts_arrays, arrays%has_key("arr1"))
        call read_value(arrays%get("arr1"), arr1)
        call assert_eq(ts_arrays, arr1, arr1_exp)

        call assert(ts_arrays, arrays%has_key("arr2"))
        call assert_eq(ts_arrays, num_children(arrays%get("arr2")), 0)

        call assert(ts_arrays, arrays%has_key("arr3"))
        arr3 = arrays%get("arr3")
        call assert_eq(ts_arrays, num_children(arr3), 3)
        ! TODO: string arrays don't work
        call assert_eq(ts_arrays, arr3%value, '["ball", "red", "element",]')

        call assert(ts_arrays, arrays%has_key("long_arr"))
        call read_value(arrays%get("long_arr"), long_arr)
        call assert_eq(ts_arrays, long_arr, long_arr_exp)
    end block

    ! Float parsing
    block
        type(toml_object):: float_obj
        real(f64):: f, infinity = huge(1.0_f64)

        ts_floats = Testset("Floats")
        float_obj = input%get("floats")

        call read_value(float_obj%get("inf"), f)
        call assert_gt(ts_floats, f, infinity)

        call read_value(float_obj%get("pos_inf"), f)
        call assert_gt(ts_floats, f, infinity)

        call read_value(float_obj%get("neg_inf"), f)
        call assert_lt(ts_floats, f, -infinity)

        call read_value(float_obj%get("neg_nan"), f)
        call assert(ts_floats, isnan(f))

        call read_value(float_obj%get("pos_nan"), f)
        call assert(ts_floats, isnan(f))

        call read_value(float_obj%get("nan"), f)
        call assert(ts_floats, isnan(f))

        call read_value(float_obj%get("pos_zero"), f)
        call assert_eq(ts_floats, f, 0.0_f64)

        call read_value(float_obj%get("neg_zero"), f)
        call assert_eq(ts_floats, f, -0.0_f64)

        call read_value(float_obj%get("flt1"), f)
        call assert_eq(ts_floats, f, 1.0_f64)

        call read_value(float_obj%get("flt2"), f)
        call assert_eq(ts_floats, f, 3.1415_f64)

        call read_value(float_obj%get("flt3"), f)
        call assert_eq(ts_floats, f, -0.01_f64)

        call read_value(float_obj%get("flt4"), f)
        call assert_eq(ts_floats, f, 5e22_f64)

        call read_value(float_obj%get("flt5"), f)
        call assert_eq(ts_floats, f, 1e6_f64)

        call read_value(float_obj%get("flt6"), f)
        call assert_eq(ts_floats, f, -2e-2_f64)

        call read_value(float_obj%get("flt7"), f)
        call assert_eq(ts_floats, f, 6.626e-34_f64)

        call read_value(float_obj%get("flt8"), f)
        call assert_eq(ts_floats, f, 224617.445991228_f64)

    end block

    ! Int parsing
    block
        type(toml_object):: int_obj
        integer(i32):: num

        ts_ints = Testset("Integers")
        int_obj = input%get("integers")

        call read_value(int_obj%get("int1"), num)
        call assert_eq(ts_ints, num, 99)

        call read_value(int_obj%get("int2"), num)
        call assert_eq(ts_ints, num, 42)

        call read_value(int_obj%get("int3"), num)
        call assert_eq(ts_ints, num, 0)

        call read_value(int_obj%get("int4"), num)
        call assert_eq(ts_ints, num, -17)

        call read_value(int_obj%get("int5"), num)
        call assert_eq(ts_ints, num, 1000)

        call read_value(int_obj%get("int6"), num)
        call assert_eq(ts_ints, num, 5349221)

        call read_value(int_obj%get("int7"), num)
        call assert_eq(ts_ints, num, 5349221)

        call read_value(int_obj%get("int8"), num)
        call assert_eq(ts_ints, num, 12345)

    end block

    call run_and_exit((/ts_basics, ts_options, ts_arrays, ts_floats, ts_ints/))

end program

