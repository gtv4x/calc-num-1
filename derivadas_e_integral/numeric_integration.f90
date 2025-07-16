module functions
    implicit none
    contains  
    function f(x) result(y)
        real :: x , y

        y = (9.8*68.0/ 12.5) * (1 - exp(-12.5/68.0 *x))  ! Defina aqui a função a ser resolvida
    end function f

end module functions

program numeric_integration
    use functions
    implicit none
    integer :: o, n
    real :: a, b, S, T

    ! Usuário deve escolher o método
    print*, "Este programa calcula uma integral numericamente"
    print*, "Defina previamente a função no módulo functions a ser resolvida"

    print*, "Escolha o método a ser resolvido:"
    print*, "(1) Método Composto dos Trapézios"
    print*, "(2) Método Composto de Simpson"
    print*, "(3) Use os dois métodos"
    read*, o

    print*, "Defina o intervalo inicial da integral"
    read*, a
    print*, "Defina o intervalo final da integral"
    read*, b

    print*, "Defina o número de subdivisões do intervalo"
    read*, n


    select case (o)
        case (1)
            print*, "Calculando usando o método Composto dos Trapézios"
            call trapezoid(a, b, n, T)
            print "(A, F10.2, F10.2, A)", "A integral numérica no intervalo", a, b, " é:"
            print "(F10.6)", T

        case (2)
            print*, "Calculando usando o método Composto de Simpson"
            call simpson(a, b, n, S)
            print "(A, F10.5, F10.5, A)", "A integral numérica no intervalo", a, b, " é:"
            print "(F10.6)", S

        case(3)
            call trapezoid(a, b, n, T)
            call simpson(a, b, n, S)
            print "(A, F10.2, F10.2, A)", "A integral numérica no intervalo", a, b, " é:"
            print*, "Usando o método Composto dos Trapézios:"
            print "(F10.6)", T
            print*, "Usando o método Composto de Simpson:"
            print "(F10.6)", S

        case default
            print*, "Opção inválida"
    end select

    contains ! Métodos aqui
    subroutine trapezoid(a, b, n, T)
        real, intent(in) :: a, b
        integer, intent(in) :: n
        real, intent(out) :: T
        real :: h
        integer :: i

        h = (b - a) / n
        T = (f(a) + f(b))/ 2.0 ! Inclui as pontas do intervalo
        do i = 1, n-1
            T = T + f(a + i*h)
        end do
        T = T * h
    end subroutine trapezoid

    subroutine simpson(a, b, n, S)
        real, intent(in) :: a, b
        integer, intent(in) :: n
        real, intent(out) :: S
        real :: h, x_i, x_ip1
        integer :: i

        h = (b - a) / n

        S = f(a) + f(b)

        do i = 1, n-1
            S = S + 2.0 * f(a + i*h)
        end do

        do i = 0, n-1
            x_i = a + i*h
            x_ip1 = a + (i+1)*h
            S = S + 4.0 * f((x_i + x_ip1)/2.0)
        end do

        S = S * h / 6.0
    end subroutine simpson

end program numeric_integration