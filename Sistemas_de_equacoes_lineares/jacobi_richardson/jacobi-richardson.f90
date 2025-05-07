program jacobi_richardson
    implicit none
    real, allocatable :: A(:, :), B(:), X(:), Y(:)
    integer :: n, i, j, k
    real :: S, Tol, Erro

    print*, "Este programa resolve um sistema linear de ordem n pelo metodo Jacobi-Richardson"

    print*, "Digite a ordem n da matriz A:"
    read*, n

    ! Alocando memoria
    allocate (A(n, n), B(n), X(n), Y(n))

    ! Lendo os elementos da matriz A por linha
    print*, "Digite os elementos da matriz A:"
    do i = 1, n
        print*, "Digite os elementos da linha", i, ":"
        read*, (A(i, j), j = 1, n)
    end do

    ! Testar se a matriz é estritamente diagonalmente dominante
    do i = 1, n
        S = 0.0
        do j = 1, n
            if (i /= j) then
                S = S + abs(A(i, j))
            end if
        end do   

        if (abs(A(i, i)) <= S) then
            print*, "A matriz não é estritamente diagonalmente dominante."
            stop
        end if
    end do
    print*, "A matriz é estritamente diagonalmente dominante"

    ! Lendo os elementos da matriz B
    print*, "Digite os elementos da matriz B (coluna):"
    read*, B

    ! Definindo a tolerancia
    print*, "Defina uma tolerancia"
    read*, Tol

    ! Solucao inicial de X
    do i = 1, n
        Y(i) = 0
    end do

    ! Construindo a sequencia de solucoes
    do k = 1, 1000 ! No de iteracoes
        do i = 1, n
            S = 0.0
            do j = 1, n
                if (j /= i) then
                    S = S + A(i, j) * X(j)
                end if
            end do
            X(i) = (B(i) - S) / A(i, i)
        end do

        print*, "Iteracao", k
        print*, "X = ", X

        ! Verificando a convergencia
        Erro = maxval(abs(X - Y)) / maxval(abs(X))
        print*, "Erro relativo = ", Erro
        
        if (Erro <= Tol) then
            print*, "Convergência atingida em", k , "iterações!"
            print*, "Solução encontrada:"
            print*, "X = ", X
            exit
        end if

        ! Atualizando Y
        do i = 1, n
            Y(i) = X(i)
        end do

    end do

    ! Liberando a memoria alocada
    deallocate(A, B, X, Y)

end program jacobi_richardson
