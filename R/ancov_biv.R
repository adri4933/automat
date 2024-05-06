#' regression with 2 independant variables
#'
#' @param model a model object
#' @param showShapiro if True, Shapiro test is realized on residuals to test for normality
#' @param showVarianceTest if True, Levene's or Bartlett's test is realized on residuals to test for equality of variance
#'
#'
#' @return function returns a list of 3 object : "model" containing model summary, "shapiro pvalue" and "bartlett pvalue"
#' @import rstatix
#' @import stats
#' @export
#'
#' @examples

ancov_biv <- function(model, showShapiro=T, showVarianceTest=T) {

  if(!inherits(model, "lm")){
    stop("'model' argument has to be a lm object")
  }

  result <- summary(model) #get results summary

  if(showShapiro){
    shapiro_pval <- shapiro.test(model$residuals)$p.value
  }else{
    shapiro_pval <- NULL
  }


  if(showVarianceTest){

    test_function <- ifelse(shapiro_pval<0.05, "levene_test", "bartlett.test")


    if(length(model$terms[[3]]) < 2){
      varTest <- tryCatch(
        do.call(test_function, list(as.formula(
          paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]]))
        ), data=get(model$call$data))),
        error=function(e){
          return("Independant variable is not a factor")
        }
      )
    }else{
      varTest <- tryCatch(
        do.call(test_function, list(as.formula(
          paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]])[2])
        ), data=get(model$call$data))),
        error=function(e){
          tryCatch(
            do.call(test_function, list(as.formula(
              paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]])[3])
            ), data=get(model$call$data))),
            error=function(e2){
              return("No factor variable among independant variables")
            }
          )
        }
      )
    }


  }else{
    varTest <- NULL
  }

  return(Filter(Negate(is.null), list(shapiro_pvalue=shapiro_pval,
                                      varTest_pvalue=paste(test_function, ":", varTest$p),
                                      model=result)))
}

