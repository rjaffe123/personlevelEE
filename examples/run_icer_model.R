data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example$cost, data_example$id, data_example$tx)

effect_object <- define_effect(data_example$effect, data_example$id, data_example$tx)

covariates <- define_covariates(covariates = list(data_example$age, data_example$sex), names = c("age", "sex"), id = data_example$id)

icer_model <- run_icer_model(cost_object, effect_object, covariates = covariates, interaction = FALSE)
#icer_model <- run_icer_model(cost_object, effect_object, covariates = covariates, interaction = TRUE)

print(icer_model)

summary(icer_model)

plot(icer_model, type = "regression")
plot(icer_model, type = "regression", bw = TRUE)
