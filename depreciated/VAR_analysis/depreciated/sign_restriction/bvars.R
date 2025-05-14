library(bsvarSIGNs)
# investigate the effects of the optimism shock
#data(optimism)


# specify identifying restrictions:
# + no effect on productivity (zero restriction)
# + positive effect on stock prices (positive sign restriction)

headers_var
#sign_irf       = matrix(c(1, -1, rep(NA, 14)), 4, 4)


sign_irf       = matrix(c(1, -1, -1, NA,
                          1, 1, NA, NA,
                          1, 1, 1, NA,
                          NA, NA, -1, 1), 4, 4)
sign_irf
# specify the model
specification  = specify_bsvarSIGN$new(vars * 100,
                                       p        = 4,
                                       sign_irf = sign_irf)

# estimate the model
posterior      = estimate(specification, S = 100)

# compute and plot impulse responses
irf            = compute_impulse_responses(posterior, horizon = 40)
windows(width = 10, height = 7)  # For Windows users
plot(irf, probability = 0.9)


sign_irf_min = matrix(c(1, -1, NA, NA,
                        1, 1, NA, NA,
                        rep(NA,8)), 4, 4)
specification  = specify_bsvarSIGN$new(vars * 100,
                                       p        = 4,
                                       sign_irf = sign_irf_min)
posterior      = estimate(specification, S = 100)
# compute and plot impulse responses
irf            = compute_impulse_responses(posterior, horizon = 40)
windows(width = 10, height = 7)  # For Windows users
plot(irf, probability = 0.9)


#tester si ça change quelque chose ou pas du tout
sign_irf_test = matrix(c(1, 1, NA, NA,
                        -1, 1, NA, NA,
                        rep(NA,8)), 4, 4)
specification  = specify_bsvarSIGN$new(vars * 100,
                                       p        = 4,
                                       sign_irf = sign_irf_test)
posterior      = estimate(specification, S = 100)
# compute and plot impulse responses
irf            = compute_impulse_responses(posterior, horizon = 40)
windows(width = 10, height = 7)  # For Windows users
plot(irf, probability = 0.9)

