data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id, data_example$tx)

effect_object <- define_effect(data_example$effect, data_example$id, data_example$tx)

nb_value <- define_NB(lambdas = c(100, 350, 1000, 10000), cost = cost_object, effect = effect_object)
