# Numerical Methods Implemented in Programming

This repository contains implementations of various numerical methods in Fortran. These methods are commonly used in numerical calculus to solve equations, find roots, perform interpolation, and solve systems of linear equations.

It started as separate college projects and evolved into this repo. In the future, I might add a LaTeX file explaining some of the math behind each method, in case of curiosity. Feel free to use/copy the code for your project or just for study ;)

## Implemented Methods

### Root-Finding Methods (Zeros de Funções)
1. **Secant Method** (`secantes.f90`): Finds roots of a function using the secant method.
2. **Newton-Raphson Method** (`newton-raphson.f90`): Uses the Newton-Raphson method for root-finding.
3. **Bisection Method** (`bissecao.f90`): Implements the bisection method for finding roots.
4. **Fixed-Point Iteration** (`iteracao_ponto_fixo.f90`): Solves equations using fixed-point iteration.

### Linear Systems (Sistemas de Equações Lineares)
1. **Upper Triangular Matrix** (`matriz_triangular_superior_2.0.f90`): Solves upper triangular systems.
2. **Lower Triangular Matrix** (`matriz_triangular_inferior_2.0.f90`): Solves lower triangular systems.
3. **Jacobi-Richardson Method** (`jacobi-richardson.f90`): Iterative method for solving linear systems.
4. **Gauss-Seidel Method** (`gauss_seidel.f90`): Another iterative method for solving linear systems.
5. **Gaussian Elimination** (`eliminacao_de_gauss.f90`): Solves linear systems using Gaussian elimination.

### Interpolation (Interpolação)
1. **Vandermonde** (`vandermonde.f90`): Implements polynomial interpolation by solving a linear system of the form Va=Y and returns the polynomial coefficients.
2. **Lagrange Method** (`lagrange.f90`): Implements Lagrange polynomial interpolation; for a given x coordinate provided by the user, it returns the interpolated point.

### Derivatives and Numerical Integration (Derivadas e Integral)
1. **Finite Differences** (`finite_differences.f90`): Calculates the numerical derivative of a function at a given point using progressive, regressive, central, and three-point finite difference formulas. The user can select the method at the beginning of the execution. Also computes the relative error if the analytical derivative is provided.
2. **Numeric Integration** (`numeric_integration.f90`): Calculates the definite integral of a user-defined function over an interval using the Composite Trapezoidal and Composite Simpson's methods. The user can select the method at the beginning of the execution.
3. **Newton-Raphson 2.0** (`newton-raphson_2.0.f90`): A modified Newton-Raphson method where the derivative is approximated numerically using the central finite difference formula to find roots of a function.

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

