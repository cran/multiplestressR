
#'Classify Interactions (for either null model)
#'
#'Using the output from either the \code{\link{effect_size_additive}} or \code{\link{effect_size_multiplicative}} functions,
#'interactions can be assigned classifications (e.g., antagonisms or synergisms) based on the frameworks used by
#'\emph{Burgess et al. (2021)}, or \emph{Jackson et al. (2016)}.
#'
#'
#'@param effect_size_dataframe     Output from either the \code{\link{effect_size_additive}} or \code{\link{effect_size_multiplicative}} functions.
#'@param assign_reversals          Specify whether reversals should be distinguished from antagonisms (see \emph{Jackson et al. (2016)}
#'(TRUE or FALSE; default = TRUE)
#'@param remove_directionality     Specify whether directionality should be removed from the effect sizes.
#'This is most pertinent where subsequent analyses will involve conducting a formal meta-analysis (set value to TRUE).
#' (TRUE or FALSE; default = FALSE)
#'
#
#'
#'@return The function returns a dataset containing
#'i)   the same results as from either the \code{\link{effect_size_additive}} or \code{\link{effect_size_multiplicative}} functions.
#'
#'ii)  the classification of any interactions (see below)
#'
#'iii) if remove_directionality = TRUE; effect sizes, and confidence intervals will be adjusted (see below)
#'
#'---
#'
#'---
#'
#'For ii), interactions are classified in the following method.
#'
#'  a) if the confidence intervals for any effect size overlap zero then the interaction is assigned a \emph{null} classification.
#'
#'  This is analogous to the classification of \emph{additive} which other studies (e.g., \emph{Jackson et al. (2016)}) may use.
#'
#'  Note that this is given precedence over the process described in b).
#'
#'
#'  ---
#'
#'  b) if the confidence intervals for any effect size do not overlap zero then \emph{Expected} and \emph{Observed} effects are calculated.
#'
#'  Where the additive null model has been used:
#'  \deqn{Expected = X_a + X_b - 2 * X_c}
#'  \deqn{Observed = X_i - X_c}
#'
#'  Where the multiplicative null model has been used:
#'  \deqn{Expected = ln(X_a) + ln(X_b) - 2 * ln(X_c)}
#'  \deqn{Observed = ln(X_i) - ln(X_c)}
#'
#'  Here \emph{X_c}, \emph{X_a}, \emph{X_b}, and \emph{X_i} correspond to the means of the control,
#'  stressor A, stressor B, and stressors A and B treatments respectively.
#'
#'  ---
#'
#'  A \emph{synergistic} classification is assigned where:
#'
#'  an effect size is positive \emph{and} \emph{Expected} is greater than zero.
#'
#'  or
#'
#'  an effect size is negative \emph{and} \emph{Expected} is less than zero.
#'
#'  ---
#'
#'  An \emph{antagonistic} classification is assigned where:
#'
#'  an effect size is negative \emph{and both} \emph{Expected and Observed} are greater than zero.
#'
#'  or
#'
#'  an effect size is positive \emph{and both} \emph{Expected and Observed} are less than zero.
#'
#'  ---
#'
#'  A \emph{reversal} classification is assigned where:
#'
#'  an effect size is negative \emph{and} \emph{Expected} is greater than zero but \emph{Observed} is less than zero.
#'
#'  or
#'
#'  an effect size is positive \emph{and} \emph{Expected} is less than zero but \emph{Observed} is greater than zero.
#'
#'
#'  If \emph{assign_reversals} = FALSE, then where a reversal would be assigned using the above method, it is simply assigned
#'  an \emph{antagonistic} classification instead.
#'
#'---
#'
#'---
#'
#'  If \emph{remove_directionality} = TRUE, then the following method is implemented.
#'
#'  Where an interaction has an \emph{Expected} value <0, the corresponding effect size
#'  for this interaction has its polarity changed (i.e., ES = -ES).
#'
#'  Confidence intervals for these altered effect sizes are likewise updated.
#'
#'  The remove of directionality is only likely to be of concern for meta-analyses.
#'
#'@examples
#'
#'#loading up an example dataset from the multiplestressR package
#'df <- multiplestressR::survival
#'
#'#calculating effect sizes
#'df <- effect_size_additive(Control_N         = df$Sample_Size_Control,
#'                            Control_SD        = df$Standard_Deviation_Control,
#'                            Control_Mean      = df$Mean_Control,
#'                            StressorA_N       = df$Sample_Size_Temperature,
#'                            StressorA_SD      = df$Standard_Deviation_Temperature,
#'                            StressorA_Mean    = df$Mean_Temperature,
#'                            StressorB_N       = df$Sample_Size_pH,
#'                            StressorB_SD      = df$Standard_Deviation_pH,
#'                            StressorB_Mean    = df$Mean_pH,
#'                            StressorsAB_N     = df$Sample_Size_Temperature_pH,
#'                            StressorsAB_SD    = df$Standard_Deviation_Temperature_pH,
#'                            StressorsAB_Mean  = df$Mean_Temperature_pH,
#'                            Significance_Level = 0.05);
#'
#'#classifying interactions
#'df <- classify_interactions(effect_size_dataframe = df,
#'                    assign_reversals = TRUE,
#'                    remove_directionality = TRUE)
#'
#'
#'@references
#'Burgess, B. J., Purves, D., Mace, G., & Murrell, D. J. (2021). Classifying ecosystem stressor interactions: Theory highlights the data limitations of the additive null model and the difficulty in revealing ecological surprises. \emph{Global Change Biology}.
#'
#'Jackson, M. C., Loewen, C. J., Vinebrooke, R. D., & Chimimba, C. T. (2016). Net effects of multiple stressors in freshwater ecosystems: a meta-analysis. \emph{Global Change Biology}, 22(1), 180-189.
#'
#'@export


classify_interactions <- function(effect_size_dataframe,
                                  assign_reversals,
                                  remove_directionality){

  if(missing(assign_reversals) == TRUE){
    assign_reversals <- TRUE
  }

  if(missing(remove_directionality) == TRUE){
    remove_directionality <- FALSE
  }


  df_MA <- effect_size_dataframe

  if(is.data.frame(df_MA) != TRUE){
    stop("effect_size_dataframe is not a data.frame
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }

  check_col_names <- colnames(df_MA)

  col_names_correct <-  c("Control_N",
                          "Control_SD",
                          "Control_Mean",
                          "StressorA_N",
                          "StressorA_SD",
                          "StressorA_Mean",
                          "StressorB_N",
                          "StressorB_SD",
                          "StressorB_Mean",
                          "StressorsAB_N",
                          "StressorsAB_SD",
                          "StressorsAB_Mean",
                          "Interaction_Effect_Size",
                          "Interaction_Variance",
                          "Interaction_CI_Upper",
                          "Interaction_CI_Lower",
                          "Null_Model")

  if(FALSE %in% c(sort(check_col_names) == sort(col_names_correct)) == TRUE){
    stop("Column names are different to those anticipated
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }



  ### Ensure all N, SD, and Mean are numeric
  booleans_numeric <- c(is.numeric(df_MA$Control_N) |        is.na(df_MA$Control_N),
                        is.numeric(df_MA$Control_SD) |       is.na(df_MA$Control_SD),
                        is.numeric(df_MA$Control_Mean) |     is.na(df_MA$Control_Mean),
                        is.numeric(df_MA$StressorA_N) |      is.na(df_MA$StressorA_N),
                        is.numeric(df_MA$StressorA_SD) |     is.na(df_MA$StressorA_SD),
                        is.numeric(df_MA$StressorA_Mean) |   is.na(df_MA$StressorA_Mean),
                        is.numeric(df_MA$StressorB_N) |      is.na(df_MA$StressorB_N),
                        is.numeric(df_MA$StressorB_SD) |     is.na(df_MA$StressorB_SD),
                        is.numeric(df_MA$StressorB_Mean) |   is.na(df_MA$StressorB_Mean),
                        is.numeric(df_MA$StressorsAB_N) |    is.na(df_MA$StressorsAB_N),
                        is.numeric(df_MA$StressorsAB_SD) |   is.na(df_MA$StressorsAB_SD),
                        is.numeric(df_MA$StressorsAB_Mean) | is.na(df_MA$StressorsAB_Mean))

  if(FALSE %in% booleans_numeric == TRUE){
    stop("Means, Sample sizes, and SDs are non-numeric
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }


  ### Check whether N variables are all integers
  integer_check <- c(df_MA$Control_N, df_MA$StressorA_N, df_MA$StressorB_N, df_MA$StressorsAB_N)
  if(length(integer_check) != length(integer_check[integer_check %% 1 == 0])){
    warning("It is expected that all sample sizes will be integer values.
            Double check whether non-integer values for sample sizes are correct.")
  }



  ### Ensure that the lengths of all variables are the same
  lengths <- c(length(df_MA$Control_N),
               length(df_MA$Control_SD),
               length(df_MA$Control_Mean),
               length(df_MA$StressorA_N),
               length(df_MA$StressorA_SD),
               length(df_MA$StressorA_Mean),
               length(df_MA$StressorB_N),
               length(df_MA$StressorB_SD),
               length(df_MA$StressorB_Mean),
               length(df_MA$StressorsAB_N),
               length(df_MA$StressorsAB_SD),
               length(df_MA$StressorsAB_Mean))

  if(length(unique(lengths)) != 1){
    stop("Variables are of differing lengths
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }


  ###Ensure that null model is same across dataframe and that it is either additive or multiplicative
  if(length(unique(df_MA$Null_Model)) != 1){
    stop("There is more than one Null Model specified
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }
  if(unique(df_MA$Null_Model) %in% c("Additive", "Multiplicative") == FALSE){
    stop("Null model should be either 'Additive' or 'Multiplicative'
         Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
  }

  if(assign_reversals %in% c(TRUE, FALSE) == FALSE){
    stop("assign_reversals should be either TRUE or FALSE
         Please specify assign_reversals as either TRUE or FALSE      default is TRUE")
  }

  if(remove_directionality %in% c(TRUE, FALSE) == FALSE){
    stop("remove_directionality should be either TRUE or FALSE
         Please specify remove_directionality as either TRUE or FALSE      default is FALSE")
  }

  ###Check whether Means are greater than 0
  if(unique(df_MA$Null_Model) == "Multiplicative"){
    check_0 <- c(df_MA$Control_Mean, df_MA$StressorA_Mean, df_MA$StressorB_Mean, df_MA$StressorsAB_Mean)
    if(length(check_0[check_0 > 0]) != length(check_0)){
      stop("Please specify effect_size_dataframe as the output from either effect_size_additive or effect_size_multiplicative")
    }}


  Null_Model <- unique(df_MA$Null_Model)

  df_MA$Exp_effect <- NA
  df_MA$Obs_effect <- NA

  if(Null_Model == "Additive"){

    df_MA$Exp_effect <- df_MA$StressorA_Mean + df_MA$StressorB_Mean - df_MA$Control_Mean - df_MA$Control_Mean

    df_MA$Obs_effect <- df_MA$StressorsAB_Mean - df_MA$Control_Mean

  }

  if(Null_Model == "Multiplicative"){

    df_MA$Exp_effect <- log(df_MA$StressorA_Mean) + log(df_MA$StressorB_Mean) - log(df_MA$Control_Mean) - log(df_MA$Control_Mean)

    df_MA$Obs_effect <- log(df_MA$StressorsAB_Mean) - log(df_MA$Control_Mean)

  }


  Classify_function <- function(es, CI_U, CI_L, Observed, Expected){

    if(is.na(es) == T){
      return(c(es, CI_U, CI_L, "Unable to Classify Interaction"))
    }

    if(is.na(CI_L) == T){
      return(c(es, CI_U, CI_L, "Unable to Classify Interaction"))
    }

    if(is.na(CI_U) == T){
      return(c(es, CI_U, CI_L, "Unable to Classify Interaction"))
    }

    CI <- CI_U - es

    if(Expected < 0){

      es <- -es

      CI_L <- es - CI
      CI_U <- es + CI
    }

    int.class <- NA


    if(es > 0){
      if(CI_U > 0 & CI_L > 0){
        int.class <- "Synergistic"
      }
    }


    if(es < 0){
      if(CI_U < 0 & CI_L < 0){
        if((Expected >= 0 & Observed < 0) | (Expected <= 0 & Observed > 0)){
          int.class <- "Reversal"
        }
      }
      if(CI_U < 0 & CI_L < 0){
        if((Expected >= 0 & Observed >= 0) | (Expected <= 0 & Observed <= 0)){
          int.class <- "Antagonistic"
        }
      }
    }

    if((CI_U <= 0 & CI_L >= 0) | (CI_U >= 0 & CI_L <= 0)){
      int.class <- "Null"
    }

    to.return <- c(es, CI_U, CI_L, int.class)
    if(is.na(int.class) == TRUE){
      warning("Unable to classify an interaction")
    }
    return(to.return)
  }

  df_AA <- data.frame(Index = 1:dim(df_MA)[1])

  df_AA$ES_Dir    <- NA
  df_AA$CI_U_Dir  <- NA
  df_AA$CI_L_Dir  <- NA
  df_AA$Int_Class <- NA

  classified <- mapply(Classify_function,
                       df_MA$Interaction_Effect_Size,
                       df_MA$Interaction_CI_Upper,
                       df_MA$Interaction_CI_Lower,
                       df_MA$Obs_effect,
                       df_MA$Exp_effect)

  df_AA$ES_Dir    <- as.numeric(as.character(classified[1,]))
  df_AA$CI_U_Dir  <- as.numeric(as.character(classified[2,]))
  df_AA$CI_L_Dir  <- as.numeric(as.character(classified[3,]))
  df_AA$Int_Class <- classified[4,]



  if(assign_reversals == FALSE){

    rev_to_ant <- function(int_class){
      if(int_class == "Reversal"){
        int_class = "Antagonistic"
      }
      return(int_class)
    }


    df_AA$Int_Class <- mapply(rev_to_ant, df_AA$Int_Class)

  }

  df_MA$Interaction_Classification <- df_AA$Int_Class

  if(remove_directionality == TRUE){

    df_MA$Interaction_Effect_Size <- df_AA$ES_Dir
    df_MA$Interaction_CI_Upper <- df_AA$CI_U_Dir
    df_MA$Interaction_CI_Lower <- df_AA$CI_L_Dir

  }

  drop_columns <- c("Obs_effect", "Exp_effect")

  df_MA <- df_MA[ , !(names(df_MA) %in% drop_columns)]

  #df_MA <- base::subset(df_MA, select = -c(Obs_effect))
  #df_MA <- base::subset(df_MA, select = -c(Exp_effect))

  return(df_MA)

  }


