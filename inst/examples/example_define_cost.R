data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = cost, id = id, tx = tx)

class(cost_object)

print(cost_object)

plot(cost_object, type = "regression")

plot(cost_object, type = "barchart")

plot(cost_object, type = "boxplot")
