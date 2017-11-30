options(java.parameters = "-Xmx5000m")
install.packages('bartMachine')
install.packages('rJava')

library('rJava')
library("bartMachine") 

# returns the number of cores being currently used
set_bart_machine_num_cores(4)

data(automobile) 

# removing null or na's from the dataset
automobile = na.omit(automobile)
# removed 41 rows

# creating a new ser of values "Y" carring the column log_price
y <- automobile$log_price 

# duplicating the data and creating a new column which is log transformation of price
X <- automobile; X$log_price <- NULL

# Builds a BART model for regression or classification.
bart_machine <- bartMachine(X, y)

bart_machine

# measuring the out of samples error using k-fold cv
k_fold_cv(X, y, k_folds = 10)

# measures the rmse of the developed bart 
rmse_by_num_trees(bart_machine, num_replicates = 20)

# similar to the f-fold-cv above, cv'ing parameters
bart_machine_cv <- bartMachineCV(X, y)

k_fold_cv(X, y, k_folds = 10, k = 2, nu = 3, q = 0.9, num_trees = 200)

# predicting the repsonse for the first 7 rows using the built model
predict(bart_machine_cv, X[1 : 7, ])

# checks for constant variance and normality of errors
check_bart_error_assumptions(bart_machine_cv)

# assess convergence diagonstics and features of the BART model
plot_convergence_diagnostics(bart_machine_cv)

# finding the 95% confidence interval of the 100th row
calc_credible_intervals(bart_machine_cv, X[100, ], ci_conf = 0.95)

# finding the 95% prediction interval of the 100th row, 
calc_prediction_intervals(bart_machine_cv, X[100, ], pi_conf = 0.95)

# actual versus fitted values with the confidence intervals of the predictions
plot_y_vs_yhat(bart_machine_cv, credible_intervals = TRUE) 

# actual versus fitted values with the prediction intervals of the predictions
plot_y_vs_yhat(bart_machine_cv, prediction_intervals = TRUE)

# finding the importance of variable in explaining the variance in the data similar to random forest
investigate_var_importance(bart_machine_cv, num_replicates_for_avg = 20)

# testing whether the covriates used in the model have an effect on the response
# invidually testing the effect of width, body_style and then in combinations

cov_importance_test(bart_machine_cv, covariates = c("width"))

cov_importance_test(bart_machine_cv, covariates = c("body_style"))

cov_importance_test(bart_machine_cv, covariates = c("width", "curb_weight", "city_mpg", "length", "horsepower", "body_style", "engine_size", "highway_mpg", "peak_rpm", "normalized_losses"))

cov_importance_test(bart_machine_cv)

# Partial Dependence Plot represents of the marginal effect of a set of predictors
# ensemble predictions  ignoring the rest of variables

pd_plot(bart_machine_cv, j = "horsepower")

pd_plot(bart_machine_cv, j = "stroke")

y <- automobile$log_price

X <- automobile; X$log_price <- NULL 

bart_machine <- bartMachine(X, y, use_missing_data = TRUE,use_missing_data_dummies_as_covars = TRUE)

bart_machine

# create a new value x-start which will contain the 20th row
x_star <- X[20, ]

# estimating the confidence interval for the above 20th data point using the new bart model
calc_credible_intervals(bart_machine, x_star, ci_conf = 0.95)

x_star[c("curb_weight", "symboling")] <- NA

calc_credible_intervals(bart_machine, x_star, ci_conf = 0.95)

# variable selection
vs <- var_selection_by_permute(bart_machine, bottom_margin = 3, num_permute_samples = 5)

vs$important_vars_local_names

# var_selection_by_response_permute_cv is throwing an error stating the function doesn't exist

# listing all the functions in the package
lsf.str("package:bartMachine")

var_selection_by_permute_cv(bart_machine)

# finding the interactions between pairs of variables
interaction_investigator(bart_machine, num_replicates_for_avg = 25, num_var_plot = 10, bottom_margin = 5)

save.image(file="Pazhamalai_Lab11_04032016.Rdata")
