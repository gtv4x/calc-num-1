module functions
    implicit none
    contains  
    function f(x) result(y)
        real :: x, y
        y = x**3 - sin(x)  ! Defina aqui a função a ser resolvida
    end function f 

    function C(x) result(z) ! Diferença finita central
        real :: h, x, z

        h = 0.00001 ! Defina o incremento aqui
        z = (f(x + h) - f(x - h)) / (2*h)
    end function

end module functions

program newton_raphson
    use functions
    implicit none
    integer :: i
    real :: X, X_ant, Tol, Erro

    print*, "Este programa resolve uma equação do tipo f(x) = 0 pelo método de Newton-Raphson"
    print*, "Defina a função a ser resolvida previamente no módulo functions, sua respectiva derivada será calculada usando uma diferença finita central."

    print*, "Defina uma tolerância"
    read*, Tol

    print*, "Defina um valor inicial"
    read*, X_ant
    
    do i = 1, 100 ! Defina aqui número máximo de iterações
        print*, "Iteração: ", i
        X = X_ant - (f(X_ant)/C(X_ant))
        print*, "X= ", X

        Erro = abs(X - X_ant) / abs(X)
        print*, "Erro: ", Erro

        if (Erro < Tol) then
            print*, "Convergencia atingida na iteração:", i
            write(*, '(A, F10.5)') "A raiz aproximada é: ", X ! Resultado aprox para 5 casas decimais
            write(*, '(A, ES12.5)') "Erro aproximado: ", Erro
            stop
        end if

        X_ant = X ! Atualiza X_ant para a próxima iteração
        
        if (i == 100) then
            print*, "Número máximo de iterações atingido."
            stop
        end if
    end do
end program newton_raphson