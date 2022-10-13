data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id,
                           data_example$tx)

effect_object <- define_effect(data_example$effect, data_example$id,
                               data_example$tx)

covariates_list <- define_covariates(covariates = list(data_example$age,
                                                       data_example$sex),
                                     names = c("age", "sex"),
                                     id = data_example$id)

nb_value <- define_NB(lambdas = c(100, 350, 1000, 10000),
                      cost = cost_object, effect = effect_object)

nb_model <- run_INB_model(nb_value, covariates = covariates_list)

print(nb_model)

plot(nb_model, type = "regression", bw = TRUE)

plot(nb_model, type = "boxplot", bw = TRUE)

plot(nb_model, type = "barchart", bw = TRUE)

plot(nb_model, type = "ceac", bw = TRUE)




