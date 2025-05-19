# Numerical Methods Implemented in Programming

This repository contains implementations of various numerical methods in Fortran (for the fulture I want to rewrite them using Python libraries too). These methods are commonly used in numerical calculus to solve equations, find roots, and solve systems of linear equations.

It started as separete college projects and evolved into this repo. In the future I might add an Latex file explaining some of the math behind each method, for in the case of curiosity. Feel free to use/copy the code for your project or just for study ;) 

## Implemented Methods

### Zeros de Funções (Root-Finding Methods)
1. **Método das Secantes** (`secantes.f90`): Finds roots of a function using the secant method.
2. **Método de Newton-Raphson** (`newton-raphson.f90`): Uses the Newton-Raphson method for root-finding.
3. **Método da Bisseção** (`bissecao.f90`): Implements the bisection method for finding roots.
4. **Iteração de Ponto Fixo** (`iteracao_ponto_fixo.f90`): Solves equations using fixed-point iteration.

### Sistemas de Equações Lineares (Linear Systems)
1. **Matriz Triangular Superior** (`matriz_triangular_superior_2.0.f90`): Solves upper triangular systems.
2. **Matriz Triangular Inferior** (`matriz_triangular_inferior_2.0.f90`): Solves lower triangular systems.
3. **Método de Jacobi-Richardson** (`jacobi-richardson.f90`): Iterative method for solving linear systems.
4. **Método de Gauss-Seidel** (`gauss_seidel.f90`): Another iterative method for solving linear systems.
5. **Eliminação de Gauss** (`eliminacao_de_gauss.f90`): Solves linear systems using Gaussian elimination.

## How to Execute

1. **Prerequisites**:
   - A Fortran compiler (e.g., `gfortran`) must be installed on your system.

2. **Compilation**:
   - Navigate to the directory containing the desired `.f90` file.
   - Compile the program using the following command:
     ```bash
     gfortran -o program_name file_name.f90
     ```
     Replace `program_name` with the desired name for the executable and `file_name.f90` with the name of the Fortran file.

3. **Execution**:
   - Run the compiled program:
     ```bash
     ./program_name
     ```
   - Follow the on-screen instructions to input the required data (e.g., tolerance, initial guesses, or matrix elements).

## License

This project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for details.

