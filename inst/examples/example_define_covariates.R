data(data_example, package = "personlevelEE")

covariates <- define_covariates(covariates = list(data_example$age, data_example$sex), names = c("age", "sex"), id = data_example$id)

print(covariates)
