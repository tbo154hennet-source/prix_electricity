library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Example data
set.seed(123)
N <- 5  # edges
Tt <- 10 # time points
cap <- runif(N, 5, 15)
X <- array(runif(N * Tt * 2), dim = c(N, Tt, 2))
pred <- X[,,1] * 3 + X[,,2] * -2  # fake predictors

model <- MIPModel() %>%
  add_variable(f[i, t], i = 1:N, t = 1:Tt, type = "continuous") %>%
  add_variable(z[i, t], i = 1:N, t = 1:Tt, type = "binary") %>%
  add_constraint(f[i, t] <= z[i, t] * cap[i], i = 1:N, t = 1:Tt) %>%
  add_constraint(f[i, t] >= - (1 - z[i, t]) * cap[i], i = 1:N, t = 1:Tt) %>%
  # Note: use sum_expr with the pred value looked up outside the expression
  set_objective(
    sum_expr((f[i, t] - pred[i, t])^2, 
             i = 1:N, t = 1:Tt,
             replace = list(pred = pred)), 
    "min"
  )
result <- solve_model(model, with_ROI(solver = "glpk"))
flows <- get_solution(result, f[i, t])
dirs <- get_solution(result, z[i, t])
