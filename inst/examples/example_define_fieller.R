data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id, data_example$tx)

effect_object <- define_effect(data_example$effect,
                               data_example$id, data_example$tx)

fieller <- define_fieller(cost = cost_object, effect = effect_object)

plot(fieller)

plot(fieller, CI = TRUE, bw = TRUE)
print(fieller) ## 95% confidence interval
