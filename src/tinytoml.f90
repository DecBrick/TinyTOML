module TinyTOML
    ! TinyTOML v0.2.1
    ! https://github.com/archermarx/TinyTOML
    ! Copyright 2025, Thomas A. Marks and contributors licensed under the MIT license.
    ! The full text of this license can be found in LICENSE.md in the root directory of this repository.
    ! This repository provides utilities for parsing TOML files for input reading
    ! When possible, try upgrading to TOML-f
    ! which is sure to be more robust, but I was unable to get it to install on the cluster
    ! https://github.com/toml-f/toml-f
    !
    ! ================NOT CURRENTLY SUPPORTED =====================
    ! Heterogeneous arrays
    ! Arrays of strings
    ! Nested arrays
    ! Nested tables (i.e. tab1.tab2)
    ! Dates and times
    ! Multi-line strings
    ! Escape characters in strings
    
    use, intrinsic:: iso_fortran_env, only : &
        stderr => error_unit, &
        stdin => input_unit, &
        stdout => output_unit, &
        f32 => real32, &
        f64 => real64, &
        f128 => real128, &
        i8 => int8, &
        i16 => int16, &
        i32 => int32, &
        i64 => int64

    implicit none

    integer(i32), parameter:: ERROR_LENGTH = 64

    ! List of error messages
    character(len = *), dimension(1), parameter:: ERROR_MESSAGES(20) = [character(len = ERROR_LENGTH):: &
        "General error", &
        "No equals sign for key-value pair", &
        "No value provided for key", &
        "No closing brace for inline table", &
        "No closing bracket for array", &
        "No closing bracket for table", &
        "No closing bracket for table array", &
        "Number may only have one decimal point",   &
        "Invalid character in number ", &
        "Invalid underscore placement in number", &
        "Invalid + or - in number", &
        "Invalid entry", &
        "Missing closing double quote in string", &
        "Missing closing single quote in string", &
        "File not found", &
        "Invalid read", &
        "Key not found", &
        "Unrecognized token", &
        "Empty decimal part in number", &
        "Expected integer part in number" &
    ]

    ! List of corresponding error codes
    enum, bind(c)
        enumerator:: SUCCESS = 0
        enumerator:: DEFAULT_ERROR = 1
        enumerator:: NO_EQUALS_SIGN_IN_KEY = 2
        enumerator:: EMPTY_VALUE = 3
        enumerator:: NO_CLOSING_BRACE_INLINE_TABLE = 4
        enumerator:: NO_CLOSING_BRACKET_ARRAY = 5
        enumerator:: NO_CLOSING_BRACKET_TABLE = 6
        enumerator:: NO_CLOSING_BRACKET_TABLE_ARRAY = 7
        enumerator:: ONLY_ONE_DECIMAL_POINT = 8
        enumerator:: INVALID_CHAR_IN_NUMBER = 9
        enumerator:: INVALID_UNDERSCORE_IN_NUMBER = 10
        enumerator:: INVALID_PLUS_OR_MINUS_IN_NUMBER = 11
        enumerator:: INVALID_ENTRY = 12
        enumerator:: MISSING_CLOSING_DOUBLE_QUOTE = 13
        enumerator:: MISSING_CLOSING_SINGLE_QUOTE = 14
        enumerator:: FILE_NOT_FOUND = 15
        enumerator:: INVALID_READ = 16
        enumerator:: KEY_NOT_FOUND = 17
        enumerator:: UNRECOGNIZED_TOKEN = 18
        enumerator:: MISSING_DECIMAL_PART = 19
        enumerator:: EXPECTED_INTEGER_PART = 20
    end enum

    type::toml_tokenizer
        character(len = :), allocatable:: str 
        character(len=:), allocatable:: content
        character(len=:), allocatable:: tok_kind
        integer(i32):: error_code, ind, line_num
        contains
            procedure, public:: init => tokenizer_init
            procedure, public:: peek => tokenizer_peek 
            procedure, public:: advance => tokenizer_advance
    end type

    type, abstract:: toml_abstract_object
        character(len = :), allocatable:: key
    end type

    ! Define nodes of parse tree
    ! These are generally key-value pairs, but can
    ! also be things without values (table headers)
    ! or without keys (numbers in an array)
    type, extends(toml_abstract_object):: toml_object
        character(len = :), allocatable:: value
        character(len = :), allocatable:: type
        integer(i32):: line_num
        integer(i32):: error_code
        class(toml_abstract_object), allocatable:: children(:)
    contains
        procedure, public:: stringify
        procedure, public:: get_key
        procedure, public:: get_ind
        procedure, public:: has_key
        generic, public:: get => get_key, get_ind
    end type

    ! Interface for various routines for reading TOML data back into fortran data-types
    interface read_value
        module procedure read_value_bool
        module procedure read_value_string
        module procedure read_value_f32
        module procedure read_value_f64
        module procedure read_value_i32
        module procedure read_value_i64
        module procedure read_array_bool
        module procedure read_array_f32
        module procedure read_array_f64
        module procedure read_array_i32
        module procedure read_array_i64
    end interface

    interface get
        module procedure get_key
        module procedure get_ind
    end interface

    character, parameter:: C_NEWLINE = new_line('a')
    character, parameter:: C_EOF = achar(0)
    character, parameter:: C_TAB = achar(9)

    contains

!================ String manipulation functions ==============================
    pure logical function is_ident_char(d)
        character(len = 1), intent(in):: d
        select case (d)
        case ("'", '"', " ", C_TAB, C_NEWLINE, "[", "]", ",", "=", "/", "\", "#", ".", "+", "-")
            is_ident_char = .false.
        case default
            is_ident_char = .true.
        end select
    end function

    pure logical function is_digit(d)
        character(len = 1), intent(in):: d
        is_digit = (d .ge. "0") .and. (d .le. "9")
    end function

    pure logical function is_binary(d)
        character(len = 1), intent(in):: d
        is_binary = (d .ge. "0") .and. (d .le. "1")
    end function

    pure logical function is_octal(d)
        character(len = 1), intent(in):: d
        is_octal = (d .ge. "0") .and. (d .le. "7")
    end function

    pure logical function is_hex(d)
        character(len = 1), intent(in):: d
        logical:: digit, lc, uc
        digit = (d .ge. "0") .and. (d .le. "9")
        lc = (d .ge. "a") .and. (d .le. "f")
        uc = (d .ge. "A") .and. (d .le. "F")
        is_hex = digit .or. lc .or. uc
    end function

    pure logical function isspace(c)
        character(len = 1), intent(in):: c
        isspace = (c .eq. ' ') .or. (c .eq. new_line('a')) .or. (c .eq. char(9))
    end function

    pure integer(i32) function findfirst(needle, haystack)
        character(len = *), intent(in):: haystack
        character(len = 1), intent(in):: needle
        integer(i32):: i

        do i = 1, len(haystack)
            if (haystack(i:i) == needle) then
                findfirst = i
                return
            elseif (i == len(haystack)) then
                findfirst = 0
            endif
        end do
    end function

    function strip(str) result(s)
        ! Strip leading and trailing whitespace from input string str
        character(len = *), intent(in):: str
        character(len = :), allocatable:: s
        integer(i32):: iL, iR

        ! first check for 0 length strings, if empty should skip
        if (len(str) == 0) then
            return
        endif

        ! remove end of line non-space, non-letter, non-number characters (such as carriage return)
        ! ascii 32 is the space character with all values above it being numbers/letters and values below being things like tab  
        if (iachar(str(len(str):len(str))) < 32) then 
            s = str(1:len(str)-1) 
        else 
            s = str
        endif

        iL=1
        iR=len(s)

        do iL=1, iR
            if (.not. isspace(s(iL:iL))) then
                exit
            endif
        end do

        if (iL .lt. iR) then
            do iR = len(s), iL, -1
                if (.not. isspace(s(iR:iR))) then
                    exit
                endif
            end do
        end if
        s = s(iL:iR)
    end function

    pure integer(i32) function count(str, ch)
        ! Count occurances of character ch in string str
        character(len = *), intent(in):: str
        character(len = 1), intent(in):: ch
        integer(i32):: i

        count = 0
        do i = 1, len(str)
            if (str(i:i) == ch) count = count + 1
        end do

    end function

    function split(str, dlm) result(split_str)
        ! Split input string str into several substrings on occurances of
        ! a comma. Padded spaces are also removed.
        character(len = *), intent(in):: str
        character(len = 1), intent(in):: dlm
        character(len = 64), allocatable, dimension(1):: split_str(:)
        integer(i32):: length, num_substrings, i, ind, dlm_ind

        ! Count how many times delimiter occurs in string
        num_substrings = count(str, ",") + 1

        ! Get length of string
        length = len(str)

        ! Allocate result array
        allocate(split_str(num_substrings))

        ! Use internal list-directed io to read comma-separated values
        dlm_ind = 1
        ind = 1
        do i = 1, length
            if (str(i:i) == dlm) then
                split_str(ind) = str(dlm_ind:i-1)
                dlm_ind = i + 1
                ind = ind + 1
            endif
        end do

        if (dlm_ind <= length) then
            split_str(num_substrings) = str(dlm_ind:length)
        else
            split_str(num_substrings) = ""
        endif

    end function

!================ End string manipulation functions ==========================

!================ Error-handling routines ====================================
    subroutine toml_error(message, code)
        character(len = *), intent(in):: message
        integer(i32), intent(in):: code
        write(stderr, '(A)') "TOML ERROR: " //  message // "."
        call exit(code)
    end subroutine

    subroutine key_not_found_error(target_key, node, context)
        character(len = *), intent(in):: target_key
        type(toml_object), intent(in):: node
        character(len = *), optional:: context
        character(len = :), allocatable:: message
        message = "Key '" // target_key // "' not found in object"
        if (node%key /= "") message = message // " '" // node%key // "'"
        if (present(context)) message = message // " " // context
        call toml_error(message, KEY_NOT_FOUND)
    end subroutine

    subroutine invalid_read_error(type, node)
        character(len = *), intent(in):: type
        type(toml_object), intent(in):: node
        call toml_error("Tried to read '"//node%key//"' of type "//node%type//" as a "// type, INVALID_READ)
    end subroutine

    subroutine parse_error(error_code, line)
        integer(i32), intent(in):: error_code, line
        character(len = 256):: line_str
        write(line_str, '(g0)') line
        call toml_error(strip(ERROR_MESSAGES(error_code)) // " (line " // strip(line_str) // ")", error_code)
    end subroutine
!================ End error-handling routines ================================

!================ Routines for manipulating `toml_object`s ===================
    pure integer(i32) function num_children(node)
        type(toml_object), intent(in):: node
        num_children = size(node%children)
    end function

    logical function use_default(node, default_present)
        type(toml_object), intent(in):: node
        logical, intent(in):: default_present

        use_default = .false.
        if (node%error_code == KEY_NOT_FOUND) then
            if (default_present) then
                use_default = .true.
            else
                call key_not_found_error(node%value, node, "and no default provided")
            endif
        endif
    end function

    subroutine read_value_bool(node, val, default)
        logical, intent(out):: val
        logical, intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("bool")
            if (node%value == "true") val = .true.
            if (node%value == "false") val = .false.
        case default
            call invalid_read_error("boolean", node)
        end select
    end subroutine

    subroutine read_value_string(node, val, default)
        character(len = :), allocatable, intent(out):: val
        character(len = *), optional, intent(in):: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        val = node%value
    end subroutine

    subroutine read_value_f32(node, val, default)
        real(f32), intent(out):: val
        real(f32), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int_hex", "int_bin", "int_oct", "int_dec", "float")
            read(node%value, *) val
        case default
            call invalid_read_error("32-bit float", node)
        end select
    end subroutine

    subroutine read_value_f64(node, val, default)
        real(f64), intent(out):: val
        real(f64), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int_hex", "int_bin", "int_oct", "int_dec", "float")
            read(node%value, *) val
        case default
            call invalid_read_error("64-bit float", node)
        end select
    end subroutine

    subroutine read_value_i32(node, val, default)
        integer(i32), intent(out):: val
        integer(i32), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int_dec")
            read(node%value, "(I64)") val
        case("int_hex")
            read(node%value, "(Z64)") val
        case("int_oct")
            read(node%value, "(O64)") val
        case("int_bin")
            read(node%value, "(B64)") val
        case default
            call invalid_read_error("32-bit integer", node)
        end select
    end subroutine

    subroutine read_value_i64(node, val, default)
        integer(i64), intent(out):: val
        integer(i64), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int_dec")
            read(node%value, "(I64)") val
        case("int_hex")
            read(node%value, "(Z64)") val
        case("int_oct")
            read(node%value, "(O64)") val
        case("int_bin")
            read(node%value, "(B64)") val
        case default
            call invalid_read_error("64-bit integer", node)
        end select
    end subroutine

    subroutine read_array_bool(node, val, default)
        logical, allocatable, intent(out):: val(:)
        logical, optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_i32(node, val, default)
        integer(i32), allocatable, intent(out):: val(:)
        integer(i32), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_i64(node, val, default)
        integer(i64), allocatable, intent(out):: val(:)
        integer(i64), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif

        n = num_children(node)

        allocate(val(n))

        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_f32(node, val, default)
        real(f32), allocatable, intent(out):: val(:)
        real(f32), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif

        n = num_children(node)

        allocate(val(n))

        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_f64(node, val, default)
        real(f64), allocatable, intent(out):: val(:)
        real(f64), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif

        n = num_children(node)

        allocate(val(n))

        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    pure function find_key(nodes, key) result(index)
        class(toml_abstract_object), intent(in):: nodes(:)
        character(len = *), intent(in):: key
        integer(i32):: index, i
        index = 0
        do i = 1, size(nodes)
            if (nodes(i)%key == key) then
                index = i
                return
            endif
        end do
    end function

    pure function insert_child(children, new_child) result(new_children)
        class(toml_abstract_object), intent(in), allocatable:: children(:)
        type(toml_object), intent(in):: new_child
        type(toml_object), allocatable:: new_children(:)
        integer(i32):: i, n_children

        if (.not. allocated(children)) then
            n_children = 1
        else
            n_children = size(children) + 1
        endif

        allocate(new_children(n_children))

        do i = 1, n_children - 1
            select type (child => children(i))
            type is (toml_object)
                new_children(i) = child
            end select
        end do

        new_children(n_children) = new_child
    end function

    recursive logical function has_key(node, key) result(result)
        class(toml_object), intent(in):: node
        character(len = *), intent(in):: key
        character(len = :), allocatable:: subkey, remainder
        integer(i32):: index, first_dot_index
        type(toml_object):: gotten

        ! Check for subkeys
        do first_dot_index = 1, len(key)
            if (key(first_dot_index:first_dot_index) == ".") exit
        end do

        subkey = key(1:first_dot_index-1)
        if (subkey == key) then
            remainder = ""
        else
            remainder = key(first_dot_index+1:len(key))
        endif

        index = find_key(node%children, subkey)

        result = .false.

        if (index > 0) then
            result = .true.
            return
        endif

        gotten = node%get(index)

        if (remainder /= "") then
            result = result .or. has_key(gotten, strip(remainder))
        endif
    end function

    recursive function get_key(node, key, error) result(gotten)
        class(toml_object), intent(in):: node
        character(len = *), intent(in):: key
        character(len = :), allocatable:: subkey, remainder
        integer(i32):: index, first_dot_index
        type(toml_object):: gotten
        logical, optional:: error
        logical:: throw_error

        if (present(error)) then
            throw_error = error
        else
            throw_error = .true.
        endif

        ! Check for subkeys
        do first_dot_index = 1, len(key)
            if (key(first_dot_index:first_dot_index) == ".") exit
        end do

        subkey = key(1:first_dot_index-1)
        if (subkey == key) then
            remainder = ""
        else
            remainder = key(first_dot_index+1:len(key))
        endif

        index = find_key(node%children, subkey)

        if (index == 0 .and. throw_error) call key_not_found_error(subkey, node)

        gotten = node%get(index)

        if (gotten%error_code == KEY_NOT_FOUND) then
            gotten%key = node%key
            gotten%value = subkey
        endif

        if (remainder /= "") then
            gotten = gotten%get(strip(remainder))
        endif
    end function

    pure function get_ind(node, index) result(gotten)
        class(toml_object), intent(in):: node
        integer, intent(in):: index
        type(toml_object):: gotten

        if (index <= 0 .or. index > num_children(node)) then
            gotten%key = ""
            gotten%value = ""
            gotten%type = ""
            gotten%error_code = KEY_NOT_FOUND
            gotten%children = no_children()
        else
            select type (found => node%children(index))
                type is (toml_object); gotten = found
            end select
        endif
    end function

    ! Instructions for converting a node into a string
    recursive pure function stringify(node) result(str)
        class(toml_object), intent(in):: node
        character(len = :), allocatable:: str, val_str
        character(len = 1):: left_bracket, right_bracket
        integer(i32):: i, nchildren

        nchildren = size(node%children)

        str = ""

        select case (node%type)
        case("table", "array")
            if (node%key /= "") str = node%key // ": "
            if (node%type == "table") then
                left_bracket = "{"
                right_bracket = "}"
            else
                left_bracket = "["
                right_bracket = "]"
            endif
            str = str // left_bracket
            do i = 1, nchildren
                str = str // stringify(node%get(i))
                if (i < nchildren) str = str // ", "
            end do
            str = str // right_bracket
        case default
            val_str = node%value
            if (node%type == "string") val_str = '"' // val_str // '"'
            if (node%key == "") then
                str = val_str // "::" // node%type
            else
                str = node%key // "::" // node%type // " = " // val_str
            endif

        end select
    end function

    pure function no_children() result(children)
        type(toml_object), allocatable:: children(:)
        allocate(children(0))
    end function
!================ End routines for manipulating `toml_object`s =================

!================ Parsing routines =============================================

    function read_file(file) result(contents)
        character(len=*), intent(in):: file
        character(len = :), allocatable:: contents
        integer(i32):: unit, file_size
        logical:: exists

        inquire(file = file, exist = exists)

        if (exists) then
            open( newunit=unit, access='stream', &
                  file=file, action='read', form='unformatted')

            inquire(unit, size=file_size)
            allocate(character(file_size)::contents)
            read(unit) contents
            close(unit)
        else
            write(stdout, *) "ERROR: File "// strip(file) // " not found."
            call exit(2)
        endif
    end

    function parse_file(file) result(parse_tree)
        character(len = *), intent(in):: file
        character(len = :), allocatable:: file_contents
        type(toml_object):: parse_tree

        file_contents = read_file(file)
        parse_tree = parse_string(file_contents)

    end function

    function parse_string(toml_str) result(parse_tree)
        character(len = *), intent(in)::toml_str
        type(toml_object):: parse_tree

        parse_tree%key = ""
        parse_tree%value = ""
        parse_tree%type = "table"
        parse_tree%children = parse_tokens(tokenize(toml_str))
    end function

    function read_line(toml_str, start_pos, line_num) result (line)
        character(len = *), intent(in):: toml_str
        integer(i32), intent(inout):: start_pos, line_num
        character(len = :), allocatable:: line
        integer(i32):: i

        do i = start_pos,len(toml_str)
            if (toml_str(i:i) == C_NEWLINE) exit
        end do

        line = toml_str(start_pos:i-1)
        start_pos = i + 1
        line_num = line_num + 1
    end function read_line

    subroutine tokenizer_init(t, str)
        class(toml_tokenizer), intent(inout):: t
        character(len = *), intent(in):: str

        t%ind = 1
        t%line_num = 1
        t%str = str
    end subroutine

    pure function tokenizer_peek(t, amt) result(c)
        class(toml_tokenizer), intent(in):: t
        integer(i32), optional, intent(in):: amt
        integer(i32):: ind
        character:: c
    
        if (present(amt)) then
            ind = t%ind + amt
        else
            ind = t%ind
        end if

        if (ind > len(t%str)) then
            c = C_EOF
        else
            c = t%str(ind:ind)
        end if
    end function

    function tokenizer_advance(t) result(c)
        class(toml_tokenizer), intent(inout):: t
        character:: c
        t%ind = t%ind + 1
        c = t%peek() 
    end function

    subroutine consume_space(t)
        type(toml_tokenizer), intent(inout):: t
        character:: c

        c = t%peek()

        do while (c == ' ' .or. c == C_TAB)
            c = t%advance()
        end do
    end subroutine

    pure subroutine read_error_token(t)
        type(toml_tokenizer), intent(inout):: t
        integer(i32):: start_pos

        ! Read until we hit a defined separator character
        start_pos = t%ind
        do while (t%ind <= len(t%str))
            select case (t%peek())
            case (C_NEWLINE, C_TAB, ' ', '[', ']', '{', '}', ',')
                exit
            end select
            t%ind = t%ind + 1
        end do

        t%tok_kind = "error"
        t%content = t%str(start_pos:t%ind-1)

    end subroutine

    subroutine read_integer_part(t, signed)
        type(toml_tokenizer), intent(inout):: t
        character:: c
        integer(i32):: num_start_pos
        logical, intent(in), optional:: signed
        logical:: underscore_allowed, sign_allowed, positive

        underscore_allowed = .false.
        positive = .true.

        if (present(signed)) then
            sign_allowed = signed
        else
            sign_allowed = .false.
        end if

        ! Check for leading sign
        if (sign_allowed) then
            if (t%peek() == '+') then
                t%ind = t%ind + 1
            elseif (t%peek() == '-') then
                positive = .false.
                t%ind = t%ind + 1
            end if
        end if 

        t%error_code = SUCCESS
        num_start_pos = t%ind

        do while (t%ind .le. len(t%str))
            c = t%peek() 

            if (c .eq. "_") then
                if (underscore_allowed) then
                    underscore_allowed = .false.
                else
                    t%error_code = INVALID_UNDERSCORE_IN_NUMBER
                    exit
                endif
            else if (c .eq. '.' .or. c .eq. ',' .or. isspace(c) .or. c .eq. ']' .or. c .eq. '}') then
                exit
            else if ((t%tok_kind .eq. "int_dec" .or. t%tok_kind .eq. "float") .and. (c == 'e' .or. c == 'E')) then
                exit
            else if ((t%tok_kind .eq. "int_dec" .and. .not. is_digit(c)) .or. &
                     (t%tok_kind .eq. "int_oct" .and. .not. is_octal(c)) .or. & 
                     (t%tok_kind .eq. "int_bin" .and. .not. is_binary(c)) .or. &
                     (t%tok_kind .eq. "int_hex" .and. .not. is_hex(c))) then
                t%error_code = INVALID_CHAR_IN_NUMBER
                exit
            else
                if (.not. underscore_allowed) underscore_allowed = .true.
            endif
            t%ind = t%ind + 1
        end do

        t%content = t%str(num_start_pos:t%ind-1)

        if (.not. positive)             t%content = "-" // t%content

        if (len(t%content) .eq. 0)      t%error_code = EXPECTED_INTEGER_PART

        if (t%error_code .ne. SUCCESS)  call read_error_token(t)

    end subroutine

    subroutine read_number(t)
        type(toml_tokenizer), intent(inout):: t
        character:: c
        logical:: decimal_found, exp_allowed, sign_allowed, positive 
        integer(i32):: tok_start_pos, num_start_pos

        decimal_found = .false.
        exp_allowed = .false.
        sign_allowed = .false.
        positive = .true.

        t%tok_kind = "int_dec"
        t%error_code = SUCCESS
        tok_start_pos = t%ind

        ! Check for leading sign
        if (t%peek() == '+') then
            t%ind = t%ind + 1
        elseif (t%peek() == '-') then
            positive = .false.
            t%ind = t%ind + 1
        end if

        ! Check for nan or inf
        if ((len(t%str) - t%ind) .ge. 2) then
            if ((t%str(t%ind:t%ind+2) .eq. 'nan') .or. (t%str(t%ind:t%ind+2) .eq. 'inf')) then
                t%tok_kind = "float"
                num_start_pos = t%ind
                t%ind = t%ind + 3
                goto 999
            end if
        end if

        ! Check for octal, hex, binary prefixes
        if ((len(t%str) - t%ind) .ge. 2) then
            if (t%str(t%ind:t%ind+1) .eq. '0x') then
                t%tok_kind = "int_hex"
                t%ind = t%ind + 2
            else if (t%str(t%ind:t%ind+1) .eq. '0o') then
                t%tok_kind = "int_oct"
                t%ind = t%ind + 2
            else if (t%str(t%ind:t%ind+1) .eq. '0b') then
                t%tok_kind = "int_bin"
                t%ind = t%ind + 2
            end if
        end if

        num_start_pos = t%ind

        ! Read integral part
        call read_integer_part(t)

        if (t%ind > len(t%str)) goto 999

        ! Check for decimal
        if (t%peek() .eq. '.') then
            t%tok_kind = "float"
            t%ind = t%ind + 1
            call read_integer_part(t)
            if (len(t%content) == 0) then
                t%tok_kind = "error"
                t%error_code = MISSING_DECIMAL_PART
            end if
            if (t%error_code .ne. SUCCESS) goto 999
        end if

        ! Check for exponent
        if (t%peek() .eq. 'e' .or. c .eq. "E") then
            t%tok_kind = "float"
            t%ind = t%ind + 1
            if (t%peek() .eq. '-' .or. t%peek() .eq. '+') t%ind = t%ind + 1
            call read_integer_part(t)
        endif

    999 t%content = t%str(num_start_pos:t%ind-1)

        if (.not. positive) then
            t%content = "-" // t%content
        endif

        if (t%error_code .eq. SUCCESS)  t%content = clean_number(t%content, t%tok_kind)

        t%ind = t%ind - 1

    end subroutine

    subroutine read_token(t)
        type(toml_tokenizer), intent(inout):: t
        integer(i32):: start_pos
        character:: c

        call consume_space(t)

        t%tok_kind = "eof"
        t%error_code = SUCCESS

        c = t%peek()
        select case (c)
        case (C_EOF)
            t%tok_kind = "eof"
        case ('[')
            t%tok_kind = "["
            if (t%ind .lt. len(t%str)) then
                if (t%peek(1) == '[') then
                    t%tok_kind = "[["
                    c = t%advance()
                endif
            endif

        case (']')
            t%tok_kind = "]"
            if (t%ind .lt. len(t%str)) then
                if (t%peek(1) == ']') then
                    t%tok_kind = "]]"
                    c = t%advance()
                endif
            endif

        case ('{', '}', '=', ',', '.')
            t%tok_kind = c

        case (C_NEWLINE)
            t%tok_kind = "newline"
            t%line_num = t%line_num + 1

        case ('#')
            t%tok_kind = "comment"
            start_pos = t%ind + 1

            do while ((t%advance() .ne. C_NEWLINE) .and. (t%peek() .ne. C_EOF))
            end do

            t%content = t%str(start_pos:t%ind-1)

            ! Back up so we find the newline
            if (t%peek() .eq. C_NEWLINE) t%ind = t%ind - 1

        case ('"')
            t%tok_kind = "string"
            start_pos = t%ind + 1
            do while (t%advance() .ne. '"')
                if (t%peek() .eq. C_EOF) then
                    t%error_code = MISSING_CLOSING_DOUBLE_QUOTE
                    exit
                end if
            end do

            t%content = t%str(start_pos:t%ind-1)
        case default
            if (is_ident_char(c) .and. .not. is_digit(c)) then
                ! Read identifier
                start_pos = t%ind
                t%tok_kind = "ident"
                do while (is_ident_char(t%advance()))
                    if (t%peek() == C_EOF) exit
                end do

                t%content = t%str(start_pos:t%ind-1)

                ! Back up so we find the newline
                if (t%peek() .ne. C_EOF) t%ind = t%ind - 1

                select case(t%content)
                case ("true")
                    t%tok_kind = "true"
                    deallocate(t%content)
                case ("false")
                    t%tok_kind = "false"
                    deallocate(t%content)
                case ("inf", "nan")
                    t%tok_kind = "float"
                end select
            else if (is_digit(c) .or. c == '-' .or. c == '+') then
                ! Read number
                call read_number(t)
            else
                t%tok_kind = "error" 
                call read_error_token(t)
            end if
        end select

        c = t%advance()

        return

    end subroutine

    subroutine read_token_err(t)
        type(toml_tokenizer), intent(inout):: t
        call read_token(t)
        if (t%error_code .ne. SUCCESS) call parse_error(t%error_code, t%line_num)
    end subroutine

    function parse_table_body(t) result(nodes)
        type(toml_tokenizer), intent(inout):: t
        type(toml_object), allocatable:: nodes(:)

        allocate(nodes(len(t%str)))

        do
            call read_token(t)
            !if (tok%error_code .ne. SUCCESS) then
            !    call parse_error(tok%error_code, 0)
            !end if

            if (t%tok_kind == "eof") exit
        end do


    end function

    function parse_string_2(toml_str) result(parse_tree)
        character(len = *), intent(in):: toml_str
        type(toml_object):: parse_tree
        integer(i32):: ind, line_num
        type(toml_tokenizer):: t

        call t%init(toml_str)

        ind = 1
        line_num = 1

        parse_tree%key = ""
        parse_tree%value = ""
        parse_tree%type = "table"
        parse_tree%children = parse_table_body(t)
    end function

    function tokenize(toml_str) result(tokens)
        character(len = *), intent(in):: toml_str
        type(toml_object), allocatable:: tokens(:), tmp(:)
        character(len = :), allocatable:: key, val, typ, line
        type(toml_object):: pair

        integer(i32):: comment_ind, error_code
        integer(i32):: line_start = 1, num_tokens = 0, line_num = 0


        allocate(tokens(len(toml_str)))

        do
            if (line_start >= len(toml_str)) then
                exit
            endif

            ! Read a line from the file
            line = read_line(toml_str, line_start, line_num)
            if (len(line) == 0) cycle

            ! Find first occurance of a pound sign (comment) in the line
            comment_ind = findfirst("#", line)
            if (comment_ind == 0) then
                ! Line has no comment. Just trim whitespace from line
                line = strip(line)
            else
                if (comment_ind == 1) cycle !account for lines that are just comments
                line = strip(line(1:comment_ind-1))
            endif
            
            ! Try parsing as key-value pair
            pair = parse_key_value_pair(line, line_num)
            error_code = pair%error_code

            if (error_code == SUCCESS) then
                num_tokens = num_tokens + 1
                tokens(num_tokens) = pair
                tokens(num_tokens)%line_num = line_num

            elseif (error_code == NO_CLOSING_BRACKET_ARRAY) then

                ! Read lines until we find one that ends in a right bracket
                do 
                    if (line_start .ge. len(toml_str)) then
                        exit
                    end if

                    line = read_line(toml_str, line_start, line_num)
                    if (len(line) == 0) cycle

                    line = strip(line)

                    pair%value = pair%value // line

                    if (line(len(line):len(line)) .eq. ']') then 
                        num_tokens = num_tokens + 1
                        tokens(num_tokens) = pair
                        tokens(num_tokens)%line_num = line_num
                        error_code = SUCCESS
                        exit
                    end if

                end do

            elseif (error_code == NO_EQUALS_SIGN_IN_KEY) then
                key = line
                val = " "!adding a space fixes a small memory bug when assigning 

                ! Check for table header
                if (key(1:1) /= "[") then
                    ! Check for blank line
                    if (len(key) == 0 .and. num_tokens > 0) then
                        ! Don't bother recording multiple blank lines
                        ! Blank lines with comments don't count
                        if (tokens(num_tokens)%type == "blank" .or. comment_ind > 0) cycle
                        typ = "blank"
                    else
                        error_code = INVALID_ENTRY
                    endif
                else
                    if (key(1:2) == "[[") then
                        if (key(len(key)-1:len(key)) /= "]]") then
                            error_code = NO_CLOSING_BRACKET_TABLE_ARRAY
                        else
                            typ = "tablearray"
                            ! Strip brackets
                            key = key(3:len(key)-2)
                        endif
                    else
                        if (key(len(key):len(key)) /= "]") then
                            error_code = NO_CLOSING_BRACKET_TABLE
                        else
                            typ = "table"
                            ! Strip brackets
                            key = key(2:len(key)-1)
                        endif
                    endif
                endif


                if (typ /= "unknown") error_code = SUCCESS
                if (error_code == SUCCESS) then
                    num_tokens = num_tokens + 1
                    tokens(num_tokens)%key = strip(key)
                    tokens(num_tokens)%value = strip(val)
                    tokens(num_tokens)%type = typ
                    tokens(num_tokens)%line_num = line_num
                endif
            endif

            if (ERROR_CODE /= success) then
                call parse_error(ERROR_CODE, line_num)
            endif
        end do

        call move_alloc(tokens, tmp)
        allocate(tokens(num_tokens))
        tokens(1:num_tokens) = tmp(1:num_tokens)

    end function

    recursive function parse_tokens(tokens) result(nodes)
        type(toml_object):: tokens(:)
        type(toml_object), allocatable:: nodes(:), tmp(:)
        type(toml_object):: node
        integer(i32):: num_tokens, i, n_children, ind

        num_tokens = size(tokens, 1)
        allocate(nodes(num_tokens))

        ind = 0
        i = 1
        do while (i <= num_tokens)
            select case(tokens(i)%type)
            case("blank")
                i = i + 1
            case("array")
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%type = tokens(i)%type
                nodes(ind)%value = tokens(i)%value
                nodes(ind)%children = parse_array_content(tokens(i)%value, tokens(i)%line_num)
                i = i + 1
            case("inline_table")
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%type = "table"
                nodes(ind)%value = ""
                nodes(ind)%children = parse_tokens(&
                    parse_key_value_pairs(  &
                        tokens(i)%value(2:len(tokens(i)%value)-1), &
                        tokens(i)%line_num  &
                    )   &
                )
                i = i + 1
            case("table", "tablearray")
                ! Token is either a table or a table array
                ! So we need to count how many of the following lines
                ! belong under the heading, and then figure out what to do from there
                node%key = tokens(i)%key
                node%type = tokens(i)%type
                node%value = ""
                n_children = 0

                do
                    ! tables terminate at EOF or at the beginning of another table
                    if (i + n_children + 1 > num_tokens) exit
                    if (tokens(i + n_children + 1)%type == "table" .or. tokens(i + n_children + 1)%type == "tablearray") exit
                    ! Increment the child we're on
                    n_children = n_children + 1
                end do
                ! Recursively parse tokens of this table
                node%children = parse_tokens(tokens(i+1:i+n_children))

                if (node%type == "tablearray") then
                    ! If we have a table array, we need to figure out if we've seen this
                    ! table array before
                    ! If so, insert the contents in that table array
                    ! If not, create a new array containing only this table
                    node%type = "table"
                    block
                        integer(i32):: tabarray_ind
                        tabarray_ind = find_key(nodes, node%key)

                        if (tabarray_ind > 0) then
                            node%key = ""
                            nodes(tabarray_ind)%children = insert_child(nodes(tabarray_ind)%children, node)
                        else
                            ind = ind + 1
                            nodes(ind)%key = node%key
                            nodes(ind)%type = "array"
                            node%key = ""
                            nodes(ind)%value = ""
                            nodes(ind)%children = insert_child(nodes(ind)%children, node)
                        endif
                    end block
                else
                    ind = ind + 1
                    nodes(ind) = node
                endif
                i = i + n_children + 1
            case default
                ! Token is a plain old key-value pair
                ! with no possible children
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%value = tokens(i)%value
                nodes(ind)%type = tokens(i)%type
                nodes(ind)%children = no_children()
                i = i + 1
            end select
        end do

        call move_alloc(nodes, tmp)
        allocate(nodes(ind))
        nodes(1:ind) = tmp(1:ind)

    end function

    function parse_key_value_pairs(str, line_num) result(pairs)
        ! Parse comma-separated key-value pairs
        character(len = *), intent(in):: str
        integer(i32), intent(in), optional:: line_num
        type(toml_object), allocatable:: pairs(:)
        type(toml_object), allocatable:: result
        character(len = :), allocatable, dimension(1):: split_str(:)
        integer(i32):: num_pairs, i, location

        if (present(line_num)) then
            location = line_num
        else
            location = -1
        endif

        split_str = split(str, ",")
        num_pairs = size(split_str)
        ! Ignore trailing commas
        if (strip(split_str(num_pairs)) == "") then
            num_pairs = num_pairs - 1
        endif

        allocate(pairs(num_pairs))

        do i = 1, num_pairs
            result = parse_key_value_pair(strip(split_str(i)))
            if (result%error_code == SUCCESS) then
                pairs(i) = result
            else
                call parse_error(result%error_code, location)
            endif
        end do

    end function

    function parse_key_value_pair(str, line_num) result(pair)
        character(len = *), intent(in):: str
        integer(i32), intent(in), optional:: line_num
        integer(i32):: equals_ind, line_number

        character(len = :), allocatable:: key, val
        type(toml_object):: pair
        type(toml_object):: parse_result

        if (present(line_num)) then
            line_number = line_num
        else
            line_number = -1
        endif

        ! Find first occurance of an equals sign in the line
        equals_ind = findfirst("=", str)
        if (equals_ind == 0) then
            ! No key-value pair found
            pair%error_code = NO_EQUALS_SIGN_IN_KEY
            pair%key = ""
            pair%value = ""
            pair%type = "unknown"
            return
        endif
        ! This line has a key-value pair
        key = strip(str(1:equals_ind-1))
        val = strip(str(equals_ind+1:len(str))) 
        parse_result = parse_value(val)
        pair%error_code = parse_result%error_code
        pair%type = parse_result%type
        pair%key = key
        pair%value = parse_result%value
        pair%line_num = line_number
    end function

    pure function clean_number(str, type) result(num)
        ! Make a number readable as 64-bit by Fortran by removing underscores and
        ! replacing e or E with d and D
        character(len = *), intent(in):: str
        character(len = :), allocatable:: num
        character(len = *), intent(in):: type
        character:: c
        integer(i32):: i

        num = ""
        do i = 1, len(str)
            c = str(i:i)
            if (type == "float" .and. (c == "e" .or. c == "E")) then
                num = num // "d"
            elseif (c /= "_" .and. c /= "+") then
                num = num // c
            endif
        end do
    end function


    function parse_value(value) result(parse_result)
        character(len = *), intent(in):: value
        type(toml_object):: parse_result
        character(len = :), allocatable:: typ, val
        integer(i32):: error_code
        logical :: isnumber

        val = strip(value)

        typ = "unknown"
        error_code = SUCCESS

        !check if value starts with integer part
        !extra if statement needed to handle single length integers 
        isnumber = .false.
        if ((len(val) >= 2) .and. (val(1:1) == "+" .or. val(1:1) == "-")) then !check for signs
            isnumber = is_digit(val(2:2))
        else  
            isnumber = is_digit(val(1:1)) 
        endif

        ! Check if value is a bool
        if (val == "") then
            error_code = EMPTY_VALUE
        elseif (val == "true" .or. val == "false") then
            typ = "bool"
        ! Check if value is an inf or nan
        elseif (val == "inf" .or. val == "+inf" .or. val == "-inf" .or. &
                val == "nan" .or. val == "+nan" .or. val == "-nan") then
            typ = "float"
            val = clean_number(val, typ)

        ! Check if value starts with an integer part
        elseif(isnumber) then

            parse_result = parse_number(val)
            typ = parse_result%type
            val = parse_result%value
            error_code = parse_result%error_code

        ! Check if value is an inline table
        elseif (val(1:1) == "{") then

            if (val(len(val):len(val)) .ne. "}") then
                error_code = NO_CLOSING_BRACE_INLINE_TABLE
            else
                typ = "inline_table"
            endif

        ! Check if value is a double-quoted string
        elseif (val(1:1) == '"') then
            if (val(len(val):len(val)) .ne. '"') then
                error_code = MISSING_CLOSING_DOUBLE_QUOTE
            else
                typ = "string"
                val = val(2:len(val)-1)
            endif
        ! Check if value is a single-quoted string
        elseif (val(1:1) == "'") then
            if (val(len(val):len(val)) .ne. "'") then
                error_code = MISSING_CLOSING_SINGLE_QUOTE
            else
                typ = "string"
                val = val(2:len(val)-1)
            endif
        ! Check if value is an array
        elseif (val(1:1) == "[") then
            typ = "array"
            if (val(len(val):len(val)) .ne. "]") then
                error_code = NO_CLOSING_BRACKET_ARRAY
            endif
        endif

        ! Return our result
        parse_result%key = ""
        parse_result%value = val
        parse_result%type = typ
        parse_result%error_code = error_code

    end function

    pure function parse_number(str) result(parse_result)
        character(len = *), intent(in):: str
        type(toml_object):: parse_result
        character(len = :), allocatable:: typ, tmp
        character(len = 1), allocatable:: next
        logical:: decimal_point_found, exponent_found, underscore_allowed, plusminus_allowed
        character(len = 3):: int_kind
        integer(i32):: ind, l, error_code

        decimal_point_found = .false.
        exponent_found = .false.
        underscore_allowed = .false.
        plusminus_allowed = .true.

        l = len(str)
        ! Check for octal, hex, binary
        if (l .lt. 2) then!avoid trying to index greater than str length 
            int_kind = "dec"
        else if (l .gt. 2 .and. str(1:2) .eq. '0x') then
            int_kind = "hex"
        else if (l .gt. 2 .and. str(1:2) .eq. '0o') then
            int_kind = "oct"
        else if (l .gt. 2 .and. str(1:2) .eq. '0b') then
            int_kind = "bin"
        else
            int_kind = "dec"
        endif

        if (int_kind .eq. "dec") then
            ind = 0
        else
            ind = 2
        endif

        typ = "unknown"
        error_code = SUCCESS
        tmp = str(ind+1:)

        ind = 0
        do while (ind+1 .le. len(tmp))
            ind = ind + 1
            next = tmp(ind:ind)
            if (next .eq. ".") then
                if (decimal_point_found) then
                    error_code = ONLY_ONE_DECIMAL_POINT
                    exit
                else
                    decimal_point_found = .true.
                    cycle
                endif
            elseif (int_kind .eq. "dec" .and. (next .eq. "e" .or. next .eq. "E")) then
                if (exponent_found .or. ind .eq. l) then
                    error_code = INVALID_CHAR_IN_NUMBER
                    exit
                else
                    exponent_found = .true.
                    plusminus_allowed = .true.
                endif
            elseif (next == "_") then
                if (underscore_allowed) then
                    underscore_allowed = .false.
                else
                    error_code = INVALID_UNDERSCORE_IN_NUMBER
                    exit
                endif
            elseif (next == "+" .or. next == "-") then
                if (.not. plusminus_allowed) then
                    error_code = INVALID_PLUS_OR_MINUS_IN_NUMBER
                    exit
                endif
                plusminus_allowed = .false.
            elseif ((int_kind .eq. "dec" .and. .not. is_digit(next)) .or. &
                    (int_kind .eq. "oct" .and. .not. is_octal(next)) .or. & 
                    (int_kind .eq. "bin" .and. .not. is_binary(next)) .or. &
                    (int_kind .eq. "hex" .and. .not. is_hex(next))) then

                error_code = INVALID_CHAR_IN_NUMBER
                exit
            else
                if (plusminus_allowed) plusminus_allowed = .false.
                if (.not. underscore_allowed) underscore_allowed = .true.
                if (ind == l - 1) underscore_allowed = .false.
            endif
        end do

        parse_result%key = ""
        parse_result%error_code = error_code

        if (error_code == SUCCESS) then
            if  (decimal_point_found .or. exponent_found) then
                typ = "float"
            else
                typ = "int_" // int_kind
            endif
            parse_result%value = clean_number(tmp, typ)
        else
            parse_result%value = tmp
        endif

        parse_result%type = typ

    end function

    function parse_array_content(str, line_num) result(nodes)
        character(len = *), intent(in):: str
        integer(i32), intent(in):: line_num
        type(toml_object), allocatable:: nodes(:)
        type(toml_object):: child
        type(toml_object):: result
        character(len = :), allocatable:: array_content, element
        character(len = :), allocatable, dimension(1):: array_elements(:)
        integer(i32):: num_elements_expected, num_elements_found, i

        array_content = str(2:len(str)-1)
        array_elements = split(array_content, ",")
        num_elements_expected = count(array_content, ",") + 1
        num_elements_found = 0
        allocate(nodes(num_elements_expected))

        do i = 1, num_elements_expected
            element = strip(array_elements(i))
            ! if last element is blank, then we just have a trailing comma
            if (len(element) == 0 .and. i == num_elements_expected) exit
            num_elements_found = num_elements_found + 1
            result = parse_value(element)
            if (result%error_code == SUCCESS) then
                child%key = ""
                child%value = result%value
                child%type = result%type
                child%children = no_children()
                nodes(i) = child
            else
                call parse_error(result%error_code, line_num)
            endif
        end do
        nodes = nodes(1:num_elements_found)
    end function
!================ End parsing routines ========================================
end module