data(data_example, package = "personlevelEE")

cost_object <- define_cost(data_example, cost = "cost", id = "id", tx = "tx")

effect_object <- define_effect(data_example, effect = "effect", id = "id", tx = "tx")

covariates <- define_covariates(data_frame = data_example, covariates_names = c("age", "sex"), id = id)

icer_model <- run_icer_model(cost_object, effect_object,
                             covariates = covariates, interaction = FALSE)

print(icer_model)

summary(icer_model)

plot(icer_model, type = "regression")
plot(icer_model, type = "regression", bw = TRUE)
