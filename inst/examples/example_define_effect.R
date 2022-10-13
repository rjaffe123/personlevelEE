data(data_example, package = "personlevelEE")

effect_object <- define_effect(data_example$effect, data_example$id, data_example$tx)

class(effect_object)

print(effect_object)

plot(effect_object, type = "regression")

plot(effect_object, type = "barchart")

plot(effect_object, type = "boxplot")
