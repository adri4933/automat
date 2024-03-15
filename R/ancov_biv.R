#' regression with 2 independant variables
#'
#' @param model a model object
#' @param showShapiro if True, Shapiro test is realised on residuals to test for normality
#' @param showBartlett if True, Bartlett test is realised on residuals to test for equality of variance
#'
#'
#' @return function returns a list of 3 object : "model" containing model summary, "shapiro pvalue" and "bartlett pvalue"
#' @import rstatix
#' @import stats
#' @export
#'
#' @examples

ancov_biv <- function(model, showShapiro=T, showBartlett=T) {

  if(!inherits(model, "lm")){
    stop("'model' argument has to be a lm object")
  }

  result <- summary(model) #get results summary

  if(showShapiro){
    shapiro_pval <- shapiro.test(model$residuals)$p.value
  }else{
    shapiro_pval <- NULL
  }


  if(showBartlett){

    if(length(model$terms[[3]]) < 2){
      bartlett_pval <- tryCatch(
        bartlett.test(as.formula(
          paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]]))
        ), data=get(model$call$data)),
        error=function(e){
          return("Independant variable is not a factor")
        }
      )
    }else{
      bartlett_pval <- tryCatch(
        bartlett.test(as.formula(
          paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]])[2])
        ), data=get(model$call$data)),
        error=function(e){
          tryCatch(
            bartlett.test(as.formula(
              paste(deparse(model$terms[[2]]), "~", as.character(model$terms[[3]])[3])
            ), data=get(model$call$data)),
            error=function(e2){
              return("No factor variable among independant variables")
            }
          )
        }
      )
    }


  }else{
    bartlett_pval <- NULL
  }

  return(Filter(Negate(is.null), list(shapiro_pvalue=shapiro_pval, bartlett_pvalue=bartlett_pval, model=result)))
}

