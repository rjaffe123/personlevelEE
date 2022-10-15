data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id, data_example$tx)

class(cost_object)

print(cost_object)

plot(cost_object, type = "regression")

plot(cost_object, type = "barchart")

plot(cost_object, type = "boxplot")
