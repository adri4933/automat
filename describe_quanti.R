#' describe numerical variables of the provided dataset (mean, sd, min, max, median,
#' iqr, ci)
#'
#' @param data a data set with only numerical variables
#' @param createCSV if True, a CSV file is created
#'
#' @return the function writes a csv, with variables as rows and mean, sd, min, max... as columns
#' @import dplyr
#' @import purrr
#' @importFrom utils write.csv
#' @importFrom psych describe
#' @export
#'
#' @examples

describe_quanti <- function(data, createCSV=F) {

  data_num <- dplyr::select_if(data, is.numeric) #df num

  n <- nrow(data_num) - colSums(is.na(data_num)) #count na's and substract them

  liste_describe <- apply(data_num, 2, describe, IQR=T) #2 for column application (row is 1)

  #data description (rounded to 2 decimals)
  data_mean <- format(round(sapply(liste_describe, function(x) x$mean), 2), nsmall=2)
  data_sd <- format(round(sapply(liste_describe, function(x) x$sd), 2), nsmall=2)
  data_min <- format(round(sapply(liste_describe, function(x) x$min), 2), nsmall=2)
  data_median <- format(round(sapply(liste_describe, function(x) x$median), 2), nsmall=2)
  data_max <- format(round(sapply(liste_describe, function(x) x$max), 2), nsmall=2)
  data_iqr <- format(round(sapply(liste_describe, function(x) x$IQR), 2), nsmall=2)

  #get confidence interval
  data_ci <- data_num %>% map_df(~{
    result <- t.test(.)
    conf_int <- result$conf.int
    data.frame(conf_int_low = conf_int[1], conf_int_high = conf_int[2])
  })

  #format CI
  ci <- gsub(" ", "",
             paste0("[", format(round(data_ci$conf_int_low, 2), nsmall=2), ";", format(round(data_ci$conf_int_high,2),nsmall=2), "]"))

  data_descri = data.frame(N=n,
                           Mean=data_mean,
                           SD=data_sd,
                           Min=data_min,
                           Median=data_median,
                           Max=data_max,
                           IQR=data_iqr,
                           "CI95"=ci
                           )

  if(createCSV){
    nom_fichier <- "data_descri.csv"
    if(file.exists(nom_fichier)) {
      i <- 1
      while(file.exists(paste0("data_descri", i, ".csv"))) {
        i <- i + 1
      }
      nom_fichier <- paste0("data_descri",i,".csv")
    }
    write.csv(data_descri, file=nom_fichier, row.names = F)
    print("CSV file has been created.")
  }

  print(data_descri)
}
