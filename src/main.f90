program main
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
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
    character(len=1) tf
    integer(int64) seed, max, err, i, re
    seed = time()
    print '(A)', "生成されたパスワードをファイルに出力しますか？[Y/N]"
    write (*, '(A)', advance='no') ": "
    read (*, *) tf
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
    if (trim(tf) .eq. "Y" .or. trim(tf) .eq. "y") then
        open(10, file="Password.txt", status="replace")
        do i = 1, max - 1
            re = mod(xor128(seed), 94) + 1
            write (10, '(A)', advance='no') str(re:re)
        end do
        close(10)
    else
        do i = 1, max - 1
            re = mod(xor128(seed), 94) + 1
            write (*, '(A)', advance='no') str(re:re)
        end do
    end if
    print '(A, A, A)', new_line(""), new_line(""), "終わるにはEnterを押してください。"
    read *
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
end program main
