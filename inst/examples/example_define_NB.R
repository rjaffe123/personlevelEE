data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = cost, id = id, tx = tx)

effect_object <- define_effect(data_example, effect = effect, id = id, tx = tx)

nb_value <- define_NB(lambdas = c(100, 350, 1000, 10000),
                      cost = cost_object, effect = effect_object)
