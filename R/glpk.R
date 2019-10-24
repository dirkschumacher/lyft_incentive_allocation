
# the problem from lyft as a simple MIP with GLPK
# https://eng.lyft.com/how-to-solve-a-linear-optimization-problem-on-incentive-allocation-5a8fb5d04db1

# rmpk is still work in progress
library(rmpk) # https://github.com/dirkschumacher/rmpk
library(ROI.plugin.glpk) # we use ROI as a backend
suppressPackageStartupMessages(library(dplyr))

# some toy data
m <- 1000
k <- 10
v <- matrix(round(runif(m * k) * 100), nrow = m, ncol = k)
c <- matrix(round(runif(m * k) * 100), nrow = m, ncol = k)
C <- sum(c) / 2

# some riders do not have the full k incentives,
# but only the first k_i
k_i <- pmax(1, round(runif(m) * k))

model <- MIPModel(ROI_solver("glpk", control = list(presolve = TRUE, verbose = TRUE)))

# a binary variable that is 1 if rider i receives incentive j
model$add_variable(x[i,j], i = 1:m, j = 1:k, type = "integer", lb = 0, ub = 1)

# we maximise the value of each incentive assigned to each rider
model$set_objective(sum_expr(x[i, j] * v[i, j], i = 1:m, j = 1:k), sense = "max")

# at most one incentive is assigned to a rider
model$add_constraint(sum_expr(x[i, j], j = 1:k) <= 1, i = 1:m)

# the total cost is within the budget
model$add_constraint(sum_expr(x[i, j] * c[i, j], i = 1:m, j = 1:k) <= C)

# here we set certain variable combinations to 0 using the variable bounds
# this can then be used by the solver in the presolve step reduce the model size
for (rider in 1:m) {
  not_available <- seq_len(k)[-seq_len(k_i[rider])]
  for (incentive in not_available) {
    model$set_bounds(x[rider, incentive], ub = 0)
  }
}

# it seems the LP relaxation often satisfies the integrality constraints
system.time(model$optimize())
#> <SOLVER MSG>  ----
#> GLPK Simplex Optimizer, v4.65
#> 1001 rows, 10000 columns, 19950 non-zeros
#> Preprocessing...
#> 852 rows, 4877 columns, 4877 non-zeros
#> Scaling...
#>  A: min|aij| =  1.000e+00  max|aij| =  1.000e+00  ratio =  1.000e+00
#> Problem data seem to be well scaled
#> Constructing initial basis...
#> Size of triangular part is 852
#> *     0: obj =   7.284000000e+03 inf =   0.000e+00 (4856)
#> Perturbing LP to avoid stalling [734]...
#> Removing LP perturbation [1702]...
#> *  1702: obj =   7.783400000e+04 inf =   0.000e+00 (0)
#> OPTIMAL LP SOLUTION FOUND
#> GLPK Integer Optimizer, v4.65
#> 1001 rows, 10000 columns, 19950 non-zeros
#> 10000 integer variables, 5025 of which are binary
#> Preprocessing...
#> 852 rows, 4877 columns, 4877 non-zeros
#> 4877 integer variables, all of which are binary
#> Scaling...
#>  A: min|aij| =  1.000e+00  max|aij| =  1.000e+00  ratio =  1.000e+00
#> Problem data seem to be well scaled
#> Constructing initial basis...
#> Size of triangular part is 852
#> Solving LP relaxation...
#> GLPK Simplex Optimizer, v4.65
#> 852 rows, 4877 columns, 4877 non-zeros
#> *  1702: obj =   7.284000000e+03 inf =   0.000e+00 (4856)
#> Perturbing LP to avoid stalling [2436]...
#> Removing LP perturbation [3404]...
#> *  3404: obj =   7.783400000e+04 inf =   0.000e+00 (0)
#> OPTIMAL LP SOLUTION FOUND
#> Integer optimization begins...
#> Long-step dual simplex will be used
#> +  3404: mip =     not found yet <=              +inf        (1; 0)
#> +  3404: >>>>>   7.783400000e+04 <=   7.783400000e+04   0.0% (1; 0)
#> +  3404: mip =   7.783400000e+04 <=     tree is empty   0.0% (0; 1)
#> INTEGER OPTIMAL SOLUTION FOUND
#> <!SOLVER MSG> ----
#>    user  system elapsed
#>   0.229   0.005   0.234

# get all variables values as a data frame and retain only those that
# have a value of 1
result <- filter(model$get_variable_value(x[i, j]), value == 1)

# histogram of all assigned incentives
result %>%
  group_by(j) %>%
  count()
#> # A tibble: 10 x 2
#> # Groups:   j [10]
#>        j     n
#>    <int> <int>
#>  1     1   348
#>  2     2   208
#>  3     3   150
#>  4     4    83
#>  5     5    77
#>  6     6    52
#>  7     7    30
#>  8     8    35
#>  9     9    12
#> 10    10     5

# it is also a good idea to check that the resulting data satisfies the constraints

# check that the cost is within the budget
assignment_matrix <- tidyr::spread(result, "j", "value", fill = 0) %>%
  select(-name, -i) %>%
  as.matrix()

(cost <- sum(assignment_matrix * c))
#> [1] 50264
cost < C
#> [1] TRUE

# total value is
(value <- sum(assignment_matrix * v))
#> [1] 77834

# as a last check we verify that a rider only has one incentive at most
all(rowSums(assignment_matrix) <= 1)
#> [1] TRUE
