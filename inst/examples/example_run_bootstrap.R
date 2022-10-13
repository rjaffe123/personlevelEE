library(personlevelEE)

data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id, data_example$tx)

effect_object <- define_effect(data_example$effect,
                               data_example$id, data_example$tx)

bootstrap <- run_bootstrap(n=1000, cost = cost_object,
                           effect = effect_object, lambda_min = 10,
                           lambda_max = 10000, breaks = 4)

plot(bootstrap, type = "cloud", bw =TRUE)
plot(bootstrap, type = "ceac", bw = TRUE)
