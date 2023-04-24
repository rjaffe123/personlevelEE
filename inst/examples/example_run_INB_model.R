data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = cost, id = id, tx = tx)

effect_object <- define_effect(data_example, effect = effect, id = id, tx = tx)

##add cluster variable
data_example <- data_example %>% mutate(cluster_v = sample(1:4, length(data_example$tx), replace = TRUE))
covariates <- define_covariates(data_frame = data_example, covariates_names = c("age", "sex"), id = id, cluster ="cluster_v")


nb_value <- define_NB(lambdas = c(100, 350, 1000, 10000),
                      cost = cost_object, effect = effect_object)

nb_model <- run_INB_model(nb_value, covariates = covariates_list)

print(nb_model)

plot(nb_model, type = "regression", bw = TRUE)

plot(nb_model, type = "boxplot", bw = TRUE)

plot(nb_model, type = "barchart", bw = TRUE)

plot(nb_model, type = "ceac", bw = TRUE)




