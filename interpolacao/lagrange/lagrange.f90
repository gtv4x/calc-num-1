program lagrange
    implicit none
    real, allocatable :: X(:), Y(:), L(:)
    integer :: n, i, k, j
    real :: u, S

    print*, "Este programa constrói um polinômio interpolador através da fórmula interpoladora de Lagrange"

    print*, "Digite o número de pontos a serem intepolados"
    read*, n

    allocate (L(n), Y(n), X(n))

    print*, "Digite o conjunto de pontos a serem interpolados"

    do i = 1, n
        read*, X(i), Y(i)
    end do

    print*, "Digite a abscissa do ponto a ser interpolado"
    read*, u

    S = 0.0
    do k = 1, n

        ! Calculando cada monômio
        L(k) = 1.0
        do j = 1, n
            if (j /= k) then
                L(k) = L(k) * (u - X(j)) / (X(k) - X(j))
            end if
        end do

        ! Calculando o polinômio
        S = S + Y(k) * L(k)
    end do

    print*, "O par ordenado interpolado é:"
    write(*, '(F10.4, F10.4)') u, S

    deallocate(X, Y, L)
end program lagrange