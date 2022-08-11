varImp_cvglmnet <- function(matrix, model){
  # matrix = matrix in cv.glmnet()
  # model = saved model object of cv.glmnet()
  # set up empty df 
  std_coefs = data.frame(var = rep(NA,ncol(mat)+1), # need an extra row for intercept
                         sds = rep(NA,ncol(mat)+1), 
                         coefs = rep(NA,ncol(mat)+1), 
                         stand_coefs = rep(NA,ncol(mat)+1),
                         stdcoef_abs = rep(NA,ncol(mat)+1))
  std_coefs[-1,1] = colnames(mat) # populate var names from matrix except col 1 because intercept doesn't exist in mat
  std_coefs[1,1] = "(Intercept)" # hard code intercept name
  std_coefs[-1,2] = apply(mat, 2, sd) # pop sds for features in mat
  std_coefs$coefs = as.matrix(coef(mod, s = "lambda.min")) # pull coefs from model
  std_coefs[-1,4] = std_coefs[-1,3]*std_coefs[-1,2] # standardize them by multiplying by sd
  std_coefs[-1,5] = abs(std_coefs[-1,4]) # absolute values to make for easy arrangement/calling later
  
  .GlobalEnv$std_coefs <- std_coefs # save this df to environment
  
  # return absolute values of standardized coefs in descending order.
  return(std_coefs %>% filter(var!="(Intercept)") %>% select(var, stdcoef_abs) %>% arrange(desc(stdcoef_abs)))
}