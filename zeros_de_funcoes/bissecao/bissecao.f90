module funcao
    implicit none
    contains
    function f(x) result(y)
        real :: x
        real :: y
        y = x**2 - 3 ! Defina aqui a função que deseja resolver
    end function f    
end module funcao


program bissecao
    use funcao
    implicit none
    integer :: i
    real :: r, s, X, X_ant, Tol, Erro

    print*, "Este programa resolve uma equação do tipo f(x) = 0 pelo método da bisseção"
    print*, "Defina a função previamente no módulo funcao"

    print*, "Defina uma tolerância"
    read*, Tol
    print*, "Defina o intervalo inicial [r, s]"
    read*, r, s
    
    if (f(r) * f(s) > 0.0) then
        print*, "O intervalo [r, s] não contém raízes."
        stop
    end if

    do i = 1, 100 ! Defina aqui número máximo de iterações
        print*, "Iteração: ", i
        X = (r + s) / 2
        print*, "X= ", X

        if (f(X) == 0.0) then
            print*, "A raiz é: ", X
            stop
        end if

        Erro = abs(X - X_ant) / abs(X)
        print*, "Erro: ", Erro

        if (Erro < Tol) then
            print*, "Convergencia atingida na iteração:", i
            write(*, '(A, F10.5)') "A raiz aproximada é: ", X ! Resultado exibido aproximado para 5 casas decimais
            write(*, '(A, ES12.5)') "Erro aproximado: ", Erro
            stop
        end if

        if (f(r) * f(X) < 0.0) then
            s = X
        else
            r = X
        end if

        X_ant = X ! Atualiza X_ant para a próxima iteração
    end do

end program bissecao