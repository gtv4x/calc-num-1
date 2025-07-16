module functions
    implicit none
    contains  
    function f(x) result(y)
        real :: x , y
        y = exp(0.5*x)  ! Defina aqui a função a ser resolvida
    end function f
    function d(x) result(y)
        real :: x, y
        y = exp(0.5*x)*0.5   ! Defina aqui a função derivada
    end function d

end module functions

program finite_differences
    use functions
    implicit none
    integer :: o
    real :: x, h
    real :: P, R, C, T, Error

    print*, "Este programa calcula uma derivada em dado ponto numericamente"
    print*, "Defina previamente a função a ser resolvida e sua derivada para o cálculo do erro relativo no módulo functions"

    ! Usuário deve escolher o método
    print*, "Escolha o método a ser resolvido:"
    print*, "(1) Diferença Finita progressiva"
    print*, "(2) Diferença Finita regressiva"
    print*, "(3) Diferença Finita central"
    print*, "(4) Fórmula de três pontos"
    print*, "(5) Use todos os métodos"
    read*, o

    print*, "Defina o valor de X"
    read*, x

    print*, "Defina o incremento a ser utilizado"
    read*, h

    select case (o)
        case (1)
            print*, "Calculando usando Diferença Finita progressiva"
            call progressive(x, h, P)
            print*, "A derivada numérica é", P
            call error_calc (x, P, Error)
            print "(A,F10.5,A)", "O erro relativo é:", Error, "%"

        case (2)
            print*, "Calculando usando Diferença Finita regressiva"
            call regressive(x, h, R)
            print*, "A derivada numérica é", R
            call error_calc (x, R, Error)
            print "(A,F10.5,A)", "O erro relativo é:", Error, "%"

        case (3)
            print*, "Calculando usando Diferença Finita central"
            call central(x, h, C)
            print*, "A derivada numérica é", C
            call  error_calc (x, C, Error)
            print "(A,F10.5,A)", "O erro relativo é:", Error, "%"

        case (4)
            print*, "Calculando usando Fórmula de três pontos"
            call three_point(x, h, T)
            print*, "A derivada numérica é", T
            call  error_calc (x, T, Error)
            print "(A,F10.5,A)", "O erro relativo é:", Error, "%"

        case (5)
            call progressive(x, h, P)
            call regressive(x, h, R)
            call central(x, h, C)
            call three_point(x, h, T)

            print*, "Mostrando:"
            print*, "derivada ; respectivo erro (%)"

            print*, "Usando Diferença Finita progressiva temos:"
            call error_calc(x, P, Error)
            print "(F10.6, F10.5, A)", P, Error, "%"

            print*, "Usando Diferença Finita regressiva temos:"
            call error_calc(x, R, Error)
            print "(F10.6, F10.5, A)", R, Error, "%"

            print*, "Usando Diferença Finita central temos:"
            call error_calc(x, C, Error)
            print "(F10.6, F10.5, A)", C, Error, "%"

            print*, "Usando Fórmula de três pontos temos:"
            call error_calc(x, T, Error)
            print "(F10.6, F10.5, A)", T, Error, "%"

        case default
            print*, "Opção inválida"
    end select

    contains ! Métodos aqui
    subroutine progressive(x, h, P)
        real, intent(in) :: x, h
        real, intent(out) :: P

        P = (f(x + h) - f(x)) / h
    end subroutine progressive

    subroutine regressive(x, h, R)
        real, intent(in) :: x, h
        real, intent(out) :: R

        R = (f(x) - f(x - h)) / h
    end subroutine regressive

    subroutine central(x, h, C)
        real, intent(in) :: x, h
        real, intent(out) :: C

        C = (f(x + h) - f(x - h)) / (2*h)
    end subroutine central

    subroutine three_point(x, h, T)
        real, intent(in) :: x, h
        real, intent(out) :: T

        T = 1.0/h * (-1.5*f(x) + 2.0*f(x + h) - 0.5*f(x + 2.0*h))
    end subroutine three_point

    subroutine error_calc (x, N_approx, Error)
        real, intent(in) :: x, N_approx
        real, intent(out) :: Error
        Error = (abs(N_approx - d(x)) / abs(d(x))) * 100.0 ! Cálculo do erro em porcentagem
    end subroutine error_calc

end program finite_differences