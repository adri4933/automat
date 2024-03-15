#' regression with multiple covariables
#'
#' @param model a model object
#' @param showCI whether to show a second output with confidence intervals for coefficients
#'
#' @return function returns model summary and coefficients with confidence intervals
#' @import rstatix
#' @import stats
#' @export
#'
#' @examples

ancov_multiv <- function(model, showCI=F){

  if (!inherits(model, "lm")) {
    stop("'model' argument has to be a lm object.")
  }

  result <- summary(model) #get results summary
  IC <- confint(model)
  list_result <- list(model_summary=result, IC_95=IC)

  if(showCI){
    if("coefficients" %in% names(result)){
      coef_ic <- cbind(list_result$model_summary$coefficients, list_result$IC_95)
    }else{
      coef_ic <- "No coefficient for this model"
    }
  }else{
    coef_ic <- NULL
  }

  return(Filter(Negate(is.null), list(full_summary=result,
              coefficients_only=coef_ic)))
}
