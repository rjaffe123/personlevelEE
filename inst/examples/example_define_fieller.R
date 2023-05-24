data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = "cost", id = "id", tx = "tx")

effect_object <- define_effect(data_example, effect = "effect", id = "id", tx = "tx")

fieller <- define_fieller(cost = cost_object, effect = effect_object)

plot(fieller)

plot(fieller, CI = TRUE, bw = TRUE)
print(fieller) ## 95% confidence interval
