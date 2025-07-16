module funcao
    implicit none
    contains  
    function f(x) result(y)
        real :: x
        real :: y
        y = x**3 - 0.5 ! Defina aqui a função a ser resolvida
    end function f 

    function d(x) result(y)
        real :: x
        real :: y
        y = 3*(x**2) ! Defina aqui a função derivada
    end function d
end module funcao

program newton_raphson
    use funcao
    implicit none
    integer :: i
    real :: X, X_ant, Tol, Erro

    print*, "Este programa resolve uma equação do tipo f(x) = 0 pelo método de Newton-Raphson"
    print*, "Defina a função a ser resolvida e sua derivada previamente no módulo funcao"

    print*, "Defina uma tolerância"
    read*, Tol

    print*, "Defina um valor inicial"
    read*, X_ant
    
    do i = 1, 100 ! Defina aqui número máximo de iterações
        print*, "Iteração: ", i
        X = X_ant - (f(X_ant)/d(X_ant))
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