data(data_example, package = "personlevelEE")

covariates <- define_covariates(data_frame = data_example, covariates_names = c("age", "sex"), id = id)

print(covariates)
