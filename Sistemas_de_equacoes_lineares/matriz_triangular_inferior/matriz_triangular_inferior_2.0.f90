program matriz_triangular_inferior
    implicit none
    real, allocatable :: A(:, :), B(:), X(:)
    integer :: n, i, j
    real :: S
    logical :: triangular

    ! Declaração da interface da função
    interface
        logical function verificar_triangular_inferior(A, n)
            implicit none
            real, intent(in) :: A(:, :)
            integer, intent(in) :: n
        end function verificar_triangular_inferior
    end interface

    print*, "Digite a ordem da matriz A:"
    read*, n

    ! Alocando memória
    allocate (A(n, n), B(n), X(n))

    ! Lendo os elementos da matriz A por linha
    print*, "Digite os elementos da matriz A:"
    do i = 1, n
        print*, "Digite os elementos da linha", i, ":"
        read*, (A(i, j), j = 1, n)
    end do

    ! Verificando se a matriz é triangular inferior
    triangular = verificar_triangular_inferior(A, n)
    if (.not. triangular) then
        print*, "Erro: A matriz fornecida não é triangular inferior."
        deallocate(A, B, X)
        stop
    end if

    ! Lendo os elementos da matriz B
    print*, "Digite os elementos da matriz B (coluna):"
    read*, B

    ! Calculando a solução X
    X(1) = B(1) / A(1, 1)

    do i = 2, n
        S = 0.0
        do j = 1, i - 1
            S = S + A(i, j) * X(j)
        end do
        X(i) = (B(i) - S) / A(i, i)
    end do

    ! Imprimindo a solução X
    print*, "Solução X:"
    do i = 1, n
        print*, X(i)
    end do

    ! Liberando a memória alocada
    deallocate(A, B, X)

end program matriz_triangular_inferior


! Função para verificar se a matriz é triangular inferior
logical function verificar_triangular_inferior(A, n)
    implicit none
    real, intent(in) :: A(:, :)
    integer, intent(in) :: n
    integer :: i, j

    verificar_triangular_inferior = .true.
    do i = 1, n
        do j = i + 1, n
            if (A(i, j) /= 0.0) then
                verificar_triangular_inferior = .false.
                return
            end if
        end do
    end do
end function verificar_triangular_inferior