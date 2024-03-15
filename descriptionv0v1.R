#' describe
#'
#' @param vecteurj0 a numerical vector (t0)
#' @param vecteurj90 a numerical vector (t+1)
#' @param groupe a group variable
#'
#' @return return a list : numeric description, test for difference between both groups, at t0 and paired by group
#'
#' @import dplyr
#' @export
#'
#' @examples


descri <- function(vecteurj0, vecteurj90, groupe){


  df <- data.frame(vecteurj0, vecteurj90, groupe,
                   diff=vecteurj90-vecteurj0)

  result <- df %>%
    dplyr::filter(!is.na(groupe)) %>%
    dplyr::group_by(groupe) %>%
    summarise(
      nj0=sum(!is.na(vecteurj0)),
      moyj0 = format(round(mean(vecteurj0, na.rm=T), 2), nsmall=2),
      sdj0 = format(round(sd(vecteurj0, na.rm = T), 2), nsmall=2),
      nj90 = sum(!is.na(vecteurj90)),
      moyj90 = format(round(mean(vecteurj90, na.rm=T), 2), nsmall=2),
      sdj90 = format(round(sd(vecteurj90, na.rm = T), 2), nsmall=2),
      variationscore = format(round(mean(vecteurj90, na.rm=T) - mean(vecteurj0, na.rm=T), 2), nsmall=2),
      se = sd(vecteurj90-vecteurj0, na.rm=T) / sqrt(sum(!is.na(vecteurj90)))
    )

  shapiro <- df %>%
    dplyr::filter(!is.na(groupe)) %>%
    dplyr::group_by(groupe) %>%
    shapiro_test(diff) %>%
    pull(p)

  if(shapiro[1]>=0.05 & shapiro[2]>=0.05){

    levene <- df %>% levene_test(diff~groupe) %>% pull(p)

    if(levene>=0.05){
      stud_wil <- t.test(df$diff[df$groupe=="PLACEBO"],
                         df$diff[df$groupe=="VERUM"], var.equal=T)
    }else{
      stud_wil <- t.test(df$diff[df$groupe=="PLACEBO"],
                         df$diff[df$groupe=="VERUM"], var.equal=F)
    }
  }else{
    stud_wil <- wilcox.test(df$diff[df$groupe=="PLACEBO"],
                            df$diff[df$groupe=="VERUM"])
  }


  #J0
  shapiro_j0 <- df %>%
    dplyr::filter(!is.na(groupe)) %>%
    dplyr::group_by(groupe) %>%
    shapiro_test(vecteurj0) %>%
    pull(p)

  if(shapiro_j0[1]>=0.05 & shapiro_j0[2]>=0.05){

    levene_j0 <- df %>% levene_test(vecteurj0~groupe) %>% pull(p)

    if(levene_j0>=0.05){
      stud_wil_j0 <- t.test(df$vecteurj0[df$groupe=="PLACEBO"],
                            df$vecteurj0[df$groupe=="VERUM"], var.equal=T)
    }else{
      stud_wil_j0 <- t.test(df$vecteurj0[df$groupe=="PLACEBO"],
                            df$vecteurj0[df$groupe=="VERUM"], var.equal=F)
    }
  }else{
    stud_wil_j0 <- wilcox.test(df$vecteurj0[df$groupe=="PLACEBO"],
                               df$vecteurj0[df$groupe=="VERUM"])
  }


  #PAIRED
  if(shapiro[1]>=0.05){
    stud_wil_paired_placebo <- t.test(df$vecteurj0[df$groupe=="PLACEBO"],
                                      df$vecteurj90[df$groupe=="PLACEBO"],
                                      paired=T)
  }else{
    stud_wil_paired_placebo <- wilcox.test(df$vecteurj0[df$groupe=="PLACEBO"],
                                           df$vecteurj90[df$groupe=="PLACEBO"],
                                           paired=T)
  }

  if(shapiro[2]>=0.05){
    stud_wil_paired_verum <- t.test(df$vecteurj0[df$groupe=="VERUM"],
                                    df$vecteurj90[df$groupe=="VERUM"],
                                    paired=T)
  }else{
    stud_wil_paired_verum <- wilcox.test(df$vecteurj0[df$groupe=="VERUM"],
                                         df$vecteurj90[df$groupe=="VERUM"],
                                         paired=T)
  }



  return(list(descri=result, test_diff=stud_wil,
              test_j0=stud_wil_j0,
              test_paired_placebo=stud_wil_paired_placebo,
              test_paired_verum=stud_wil_paired_verum))

}
