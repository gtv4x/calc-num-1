module funcao
    implicit none
    contains   
    function p(x) result(y)
        real :: x
        real :: y
        y = 2*cos(x) - x ! Defina aqui a função de iteração
    end function p
end module funcao

program iteracao_ponto_fixo
    use funcao
    implicit none
    integer :: i
    real :: X, X_ant, Tol, Erro

    print*, "Este programa resolve uma equação do tipo f(x) = 0 pelo método Iteração de ponto fixo"
    print*, "Defina a função de iteração previamente no módulo funcao"

    print*, "Defina uma tolerância"
    read*, Tol

    print*, "Defina um valor inicial"
    read*, X_ant
    
    do i = 1, 100 ! Defina aqui número máximo de iterações
        print*, "Iteração: ", i
        X = p(X_ant)
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
            print*, "Número máximo de iterações atingido. Tente mudar a função de iteração."
            stop
        end if

    end do
end program iteracao_ponto_fixo