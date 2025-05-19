program gauss_seidel
    implicit none
    real, allocatable :: A(:, :), B(:), X(:), Y(:)
    integer :: n, i, j, k
    real :: S1, S2, Tol, Erro

    print*, "Este programa resolve um sistema linear de ordem n pelo metodo Gauss-Seidel"

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
        S1 = 0.0
        do j = 1, n
            if (i /= j) then
                S1 = S1 + abs(A(i, j))
            end if
        end do   

        if (abs(A(i, i)) <= S1) then
            print*, "A matriz não é estritamente diagonalmente dominante."
            stop
        end if
    end do
    print*, "A matriz é estritamente diagonalmente dominante"

    ! Lendo os elementos da matriz B
    print*, "Digite os elementos da matriz B (coluna):"
    read*, B

    ! Definindo a tolerância
    print*, "Defina uma tolerância"
    read*, Tol

    ! Solucao inicial de X
    do i = 1, n
        Y(i) = 0
    end do

    ! Construindo a sequência de soluções
    do k = 1, 1000 ! No de iteracoes
        do i = 1, n
            S1 = 0.0
            S2 = 0.0
            
            do j = 1, (i-1)
                S1 = S1 + A(i, j) * X(j)
            end do

            do j = i + 1, n
                S2 = S2 + A(i, j) * Y(j)
            end do

            X(i) = (B(i) - S1 - S2) / A(i, i)

        end do

        print*, "Iteração", k
        print*, "X = ", X

        ! Verificando a convergência
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

    ! Liberando a memória alocada
    deallocate(A, B, X, Y)

end program gauss_seidel
