module funcao
    implicit none
    contains  
    function f(x) result(y)
        real :: x
        real :: y
        y = x**3 - 0.5 ! Defina aqui a função a ser resolvida
    end function f
end module funcao

program secantes
    use funcao
    implicit none
    integer :: i
    real :: X, X_ant, X_ant_ant, Tol, Erro

    print*, "Este programa resolve uma equação do tipo f(x) = 0 pelo método das Secantes"
    print*, "Defina a função a ser resolvida previamente no módulo funcao"

    print*, "Defina uma tolerância"
    read*, Tol

    print*, "Defina o primeiro ponto X"
    read*, X_ant
    print*, "Defina o segundo ponto X"
    read*, X_ant_ant

    
    do i = 1, 100 ! Defina aqui número máximo de iterações
        print*, "Iteração: ", i
        X = (X_ant_ant*f(X_ant) - X_ant*f(X_ant_ant)) / (f(X_ant)-f(X_ant_ant))
        print*, "X= ", X

        Erro = abs(X - X_ant) / abs(X)
        print*, "Erro: ", Erro

        if (Erro < Tol) then
            print*, "Convergencia atingida na iteração:", i
            write(*, '(A, F10.5)') "A raiz aproximada é: ", X ! Resultado aprox para 5 casas decimais
            write(*, '(A, ES12.5)') "Erro aproximado: ", Erro
            stop
        end if

        X_ant_ant = X_ant
        X_ant = X ! Atualiza X para a próxima iteração
        
        if (i == 100) then
            print*, "Número máximo de iterações atingido."
            stop
        end if
    end do
end program secantes