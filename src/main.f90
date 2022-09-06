module key
    use, intrinsic :: iso_fortran_env, only: int64
    character(len=1) :: str(94) = ['A', 'B', 'C', 'D', 'E', 'F',&
                                &'G', 'H', 'I', 'J', 'K', 'L',&
                                &'N', 'M', 'O', 'P', 'Q', 'R',&
                                &'S', 'T', 'U', 'V', 'W', 'X',&
                                &'Y', 'Z', 'a', 'b', 'c', 'd',&
                                &'e', 'f', 'g', 'h', 'i', 'j',&
                                &'k', 'l', 'n', 'm', 'o', 'p',&
                                &'q', 'r', 's', 't', 'u', 'v',&
                                &'w', 'x', 'y', 'z', '1', '2',&
                                &'3', '4', '5', '6', '7', '8',&
                                &'9', '0', '!', '"', '#', '$',&
                                &'%', '&', '''', '(', ')', '=',&
                                &'-', '~', '^', '|', '@', '`',&
                                &'[', '{', ']', '}', '*', ':',&
                                &'+', ';', '_', '?', '/', '.',&
                                &',', '<', '>', '\']
    integer(int64) seed, max, err, i, re, level, t
contains
    integer(int64) function xor128(w)
        implicit none
        integer(int64), intent(inout) :: w
        integer(int64), save :: x = 123456789, y = 362436069, z = 521288629
        integer(int64) t
        t = xor(x, lshift(x, 11))
        x = y; y = z; z = w;
        w = xor(xor(w, rshift(w, 19)), xor(t, rshift(t, 8)))
        xor128 = w
    end function xor128
end module key

subroutine level_1()
    use key
    implicit none
    t = seed + 1
    i = 0
    do
        re = mod(xor(xor128(seed), xor128(t)), 94) + 1
        if (re > 62 .or. re < 53) then
            cycle
        else if (i .eq. max) then
            exit
        end if
        write (*, '(A)', advance='no') str(re:re)
        i = i + 1
    end do
end subroutine level_1

subroutine level_2()
    use key
    implicit none
    t = seed + 1
    i = 0
    do
        re = mod(xor(xor128(seed), xor128(t)), 94) + 1
        if (re > 52 .or. re < 23) then
            cycle
        else if (i .eq. max) then
            exit
        end if
        write (*, '(A)', advance='no') str(re:re)
        i = i + 1
    end do
end subroutine level_2

subroutine level_3()
    use key
    implicit none
    t = seed + 1
    i = 0
    do
        re = mod(xor(xor128(seed), xor128(t)), 94) + 1
        if (re > 62 .or. re < 23) then
            cycle
        else if (i .eq. max) then
            exit
        end if
        write (*, '(A)', advance='no') str(re:re)
        i = i + 1
    end do
end subroutine level_3

subroutine level_4()
    use key
    implicit none
    t = seed + 1
    i = 0
    do
        re = mod(xor(xor128(seed), xor128(t)), 94) + 1
        if (re > 62) then
            cycle
        else if (i .eq. max) then
            exit
        end if
        write (*, '(A)', advance='no') str(re:re)
        i = i + 1
    end do
end subroutine level_4

subroutine level_no()
    use key
    implicit none
    t = seed + 1
    do i = 1, max
        re = mod(xor(xor128(seed), xor128(t)), 94) + 1
        write (*, '(A)', advance='no') str(re:re)
    end do
end subroutine level_no

program main
    use key
    implicit none
    seed = time()
    if (mod(seed, 2) .eq. 0) then
        seed = seed * 5
    else
        seed = seed * 10
    end if
    write (*, '(A)', advance='no') "レベルを選択してください。(1~4 or 0): "
    read (*, *) level
    print '(A, A, I0, A)', new_line(""), "1~", HUGE(seed), "までの数値を入力してください。"
    write (*, '(A)', advance='no') ": "
    read (*, *, iostat=err) max
    if (err .ne. 0) then
        error stop ": オーバーフローしました."
    else if (max <= 0) then
        print '(A, A)', new_line(""), "* 0 <= max の条件に当てはまったので終了します."
        read *
        stop
    end if
    print *, ""
    select case (level)
    case (1)
        call level_1()
    case (2)
        call level_2()
    case (3)
        call level_3()
    case (4)
        call level_4()
    case (0)
        call level_no()
    case default
        stop "エラーが発生しました。"
    end select
    print '(A, A, A)', new_line(""), new_line(""), "終わるにはEnterを押してください。"
    read *
end program main
