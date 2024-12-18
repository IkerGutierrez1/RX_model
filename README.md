# RX model temperature signal
This R code for a RX model to predict room temperature.

# Repository strcuture

- model.R script with the code to train the models and predict temperature

- functions.R script with auxiliar functions

# Models
Before line 215 the model using lm() functions is creted, where parameter boudds cannot be specify after that optim() is used for the creation of models with bounds

To create models using lm() only the function varaible has to be updated, when using optim() the expresion in ll_lm has to be update, as weel as lower and upper bound and the initial conditions. To perform the preditions the expresion in line 268 where df_opt$Ti_pred is updated has to be changed to the one used in ll_mm. Formula and coeff_names also should be updated to match the new expression but this only affect the .txt not predictions.  
