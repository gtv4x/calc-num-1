module matriz_utils
    implicit none
contains
    subroutine eliminacao_de_gauss(n, A, B)
        implicit none
        integer, intent(in) :: n
        real, intent(inout) :: A(n, n), B(n)
        real, allocatable:: M(:,:), G(:,:), H(:)
        integer :: i, j, k

        allocate (M(n,n), G(n,n), H(n))

        ! Troca as últimas linhas lidas para o topo da matriz
        do i = 1, n
            G(i,:) = A(n - i + 1,:)
            H(i) = B(n - i + 1)

        end do
        A = G
        B = H

        ! Eliminação de Gauss
        do k = 1, n - 1
            do i = k + 1, n
                            M(i, k) = A(i, k) / A(k, k)
                do j = k, n
                    A(i, j) = A(i, j) - M(i, k) * A(k, j)
                end do
                            B(i) = B(i) - M(i, k) * B(k)
            end do
        end do

        ! Imprimindo a matriz escalonada A
        print*, "A matriz de Vandermonde com pivotamento parcial é:"
        do i = 1, n
            do j = 1, n
                write(*, '(F10.2)', advance='no') A(i, j)
            end do
            print*
        end do

        ! Imprimindo o vetor B atualizado
        print*, "A nova matriz Y é:"
        do i = 1, n
            write(*, '(F10.2)') B(i)
        end do

        ! Chamando a sub-rotina para calcular a solução X
        call matriz_triangular_superior(n, A, B)
        
        deallocate(M, G, H)
    end subroutine eliminacao_de_gauss

    subroutine matriz_triangular_superior(n, A, B)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(n, n), B(n)
        real, allocatable :: X(:)
        integer :: i, j
        real :: S

        allocate (X(n))
        ! Calculando a solução X
        X(n) = B(n) / A(n, n)

        do i = (n-1), 1, -1
            S = 0.0
            ! Calcula a soma dos produtos dos coeficientes e soluções já encontradas
            do j = i + 1, n
                S = S + A(i, j) * X(j)
            end do
            ! Calcula o valor de X(i)
            X(i) = (B(i) - S) / A(i, i) ! Matriz dos coeficientes aqui
        end do

        ! Imprimindo a solução X formatada
        print*, "Os coeficientes do polinômio são:"
        do i = 1, n
            write(*, '(F10.2)') X(i)
        end do
    end subroutine matriz_triangular_superior
end module matriz_utils

program vandermonde
    use matriz_utils
    implicit none
    real, allocatable :: V(:,:), Y(:), X(:)
    integer :: n, i, j

    print*, "Dado um conjunto de pontos, este programa constrói um polinômio interpolador resolvendo o sistema linear Va=Y"

    print*, "Digite o número de pontos"
    read*, n

    allocate (V(n,n), Y(n), X(n))

    print*, "Digite o conjunto de pontos a serem interpolados com os valores de x crescentes em módulo"

    do i = 1, n
        read*, X(i), Y(i)
    end do

    ! Preenchendo a matriz de Vandermonde com potências de X
    do i = 1, n
        do j = 1, n
            V(i, j) = X(i)**(n-j)
        end do
    end do

    ! Imprimindo a matriz V
    print*, "A matriz de vandermonde é:"
    do i = 1, n
        do j = 1, n
            write(*, '(F10.2)', advance='no') V(i, j)
        end do
        print*
    end do

    print*, "A matriz Y é:"
    do i = 1, n
        write(*, '(F10.2)', advance='no') Y(i)
        print*
    end do

    call eliminacao_de_gauss (n, V, Y)

    deallocate(X, Y, V)
end program vandermonde