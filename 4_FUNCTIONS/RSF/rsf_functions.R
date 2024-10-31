#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# RSF functions
#*******************************************************************




# function to obtain the parameters mu_x = beta x, mu_y = beta y, and variance = 1/beta rr (beta = estimates from the glm model)
model_results <- function(model_output)
{
  
  mu_x = model_output["x_","Estimate"]/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  mu_y = model_output["y_","Estimate"]/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  variance = 1/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  
  dims = 2 # x,y
  errbnd <- 0.05 
  confidence <- 1-errbnd
  chich = qchisq(confidence, dims)
  CovMatrix = diag(variance,2) #CovMatrix = contains the variance of the 2 variables (x,y) in a diag matrix 
  area = (pi*sqrt(det(CovMatrix))*chich)/1000000
  
  para_est_dt <- data.frame(parameters = c("IA", "mu_x","mu_y","variance","area","x_","y_","I(-(x_^2 + y_^2)/2)"),
                            estimates = c(paste0("IA_",buff_vector[i],"_m"), mu_x, mu_y, variance, area, model_output["y_","Estimate"], model_output["x_","Estimate"], model_output["I(-(x_^2 + y_^2)/2)","Estimate"]))
  
  return(para_est_dt)
}
