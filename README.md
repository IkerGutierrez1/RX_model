# RX model temperature signal
This R code for a RX model to predict room temperature.

# Repository strcuture

- model.R script with the code to train the models and predict temperature

- functions.R script with auxiliar functions

# Models
Before line 215, the model is created using the lm() function, where the parameter bounds cannot be specified. After that, the optim() function is used to create models with specified bounds.

To create models using lm(), only the function variable needs to be updated. When using optim(), the expression in ll_lm must be updated, along with the lower and upper bounds and the initial conditions. To perform predictions, the expression in line 268 where df_opt$Ti_pred is updated must be changed to the one used in ll_mm. Additionally, formula and coeff_names varaibles should be updated to match the new expression; however, this only affects the .txt file with the results and does not influence the predictions.
