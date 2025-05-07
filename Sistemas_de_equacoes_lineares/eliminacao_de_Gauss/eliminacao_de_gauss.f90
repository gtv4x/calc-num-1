module matriz_utils
    implicit none
contains
    subroutine matriz_triangular_superior(n, A, B, X)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n, n), B(n)
        real, intent(out) :: X(n)
        integer :: i, j
        real :: S

        ! Calculando a solução X
        X(n) = B(n) / A(n, n)

        do i = (n-1), 1, -1
            S = 0.0
            ! Calcula a soma dos produtos dos coeficientes e soluções já encontradas
            do j = i + 1, n
                S = S + A(i, j) * X(j)
            end do
            ! Calcula o valor de X(i)
            X(i) = (B(i) - S) / A(i, i)
        end do

        ! Imprimindo a solução X formatada
        print*, "Solução X:"
        do i = 1, n
            write(*, '(F10.2)') X(i)
        end do
    end subroutine matriz_triangular_superior
end module matriz_utils

program eliminacao_de_gauss
    use matriz_utils
    implicit none
    real, allocatable :: A(:, :), B(:), X(:), M(:,:) 
    integer :: n, i, j, k

    ! Exibindo a descrição do programa
    print*, "Este programa resolve sistemas lineares utilizando o método de eliminação de Gauss."

    print*, "Digite a ordem da matriz A:"
    read*, n

    ! Alocando memória
    allocate (A(n, n), B(n), X(n), M(n, n))

    ! Lendo os elementos da matriz A
    print*, "Digite os elementos da matriz A:"
    do i = 1, n
        print*, "Digite os elementos da linha", i, ":"
        read*, (A(i, j), j = 1, n)
    end do

    ! Lendo os elementos da matriz B
    print*, "Digite os elementos da matriz B (coluna):"
    read*, B

    ! Construindo o sistema triangular superior
    do k = 1, n - 1
        do i = k + 1, n
                        M(i, k) = A(i, k) / A(k, k)
            do j = k, n  ! Inclui a última coluna
                A(i, j) = A(i, j) - M(i, k) * A(k, j)
            end do
                        B(i) = B(i) - M(i, k) * B(k)
        end do
    end do

    ! Imprimindo a matriz escalonada A
    print*, "A matriz escalonada A é:"
    do i = 1, n
        do j = 1, n
            write(*, '(F10.2)', advance='no') A(i, j)
        end do
        print*
    end do

    ! Imprimindo o vetor B atualizado
    print*, "A nova matriz B é:"
    do i = 1, n
        write(*, '(F10.2)') B(i)
    end do

    ! Liberando a memória alocada para M
    deallocate(M)

    ! Chamando a sub-rotina para calcular a solução X
    call matriz_triangular_superior(n, A, B, X)

    ! Liberando a memória alocada
    deallocate(A, B, X)

end program eliminacao_de_gauss
