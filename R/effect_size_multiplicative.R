
#'Multiplicative Null Model
#'
#'Calculate the multiplicative null model for one, or more, experiments.
#'
#'The form of the multiplicative null model used here is taken from \emph{Lajeunesse (2011)}.
#'
#'Interaction effect sizes, variances, and confidence intervals are calculated.
#'
#'Here, the factorial form of the response ratio is calculated.
#'
#'@param Control_N                 Sample size of the control treatment (numeric)
#'@param Control_SD                Standard deviation of the control treatment (numeric)
#'@param Control_Mean              Mean value of the control treatment (numeric)
#'
#'@param StressorA_N               Sample size of stressor A treatment (numeric)
#'@param StressorA_SD              Standard deviation of stressor A treatment (numeric)
#'@param StressorA_Mean            Mean value of stressor A treatment (numeric)
#'
#'@param StressorB_N               Sample size of stressor B treatment (numeric)
#'@param StressorB_SD              Standard deviation of stressor B treatment (numeric)
#'@param StressorB_Mean            Mean value of stressor B treatment (numeric)
#'
#'@param StressorsAB_N             Sample size of stressors A and B treatment (numeric)
#'@param StressorsAB_SD            Standard deviation of stressors A and B treatment (numeric)
#'@param StressorsAB_Mean          Mean value of stressors A and B treatment (numeric)
#'
#'@param Significance_Level        The value of alpha for which confidence intervals are calculated
#'(numeric, between 0 and 1; default is 0.05)
#'
#'
#'@return The function returns a dataframe containing
#'
#'  i.   effect sizes
#'
#'  ii.  effect size variances
#'
#'  iii. upper and lower confidence intervals
#'
#'  iv.  user specified numeric parameters
#'
#'The equations used to calculate effect sizes, effect size variances, and confidence intervals
#'are described in \emph{Burgess et al. (2021)}.
#'
#'
#'
#'
#'@examples
#'effect_size_multiplicative(Control_N = 4,
#'                           Control_SD = 0.114,
#'                           Control_Mean = 0.90,
#'                           StressorA_N = 4,
#'                           StressorA_SD = 0.11,
#'                           StressorA_Mean = 0.77,
#'                           StressorB_N = 3,
#'                           StressorB_SD = 0.143,
#'                           StressorB_Mean = 0.72,
#'                           StressorsAB_N = 4,
#'                           StressorsAB_SD = 0.088,
#'                           StressorsAB_Mean = 0.55,
#'                           Significance_Level = 0.05)
#'
#'#loading up an example dataset from the multiplestressR package
#'df <- multiplestressR::survival
#'
#'#calculating effect sizes
#'df <- effect_size_multiplicative(Control_N         = df$Sample_Size_Control,
#'                                 Control_SD        = df$Standard_Deviation_Control,
#'                                 Control_Mean      = df$Mean_Control,
#'                                 StressorA_N       = df$Sample_Size_Temperature,
#'                                 StressorA_SD      = df$Standard_Deviation_Temperature,
#'                                 StressorA_Mean    = df$Mean_Temperature,
#'                                 StressorB_N       = df$Sample_Size_pH,
#'                                 StressorB_SD      = df$Standard_Deviation_pH,
#'                                 StressorB_Mean    = df$Mean_pH,
#'                                 StressorsAB_N     = df$Sample_Size_Temperature_pH,
#'                                 StressorsAB_SD    = df$Standard_Deviation_Temperature_pH,
#'                                 StressorsAB_Mean  = df$Mean_Temperature_pH,
#'                                 Significance_Level = 0.05);
#'
#'#classifying interactions
#'df <- classify_interactions(effect_size_dataframe = df,
#'                    assign_reversals = TRUE,
#'                    remove_directionality = TRUE)
#'
#'@references
#'
#'Burgess, B. J., Jackson, M. C., & Murrell, D. J. (2021). Multiple stressor null models frequently fail to detect most interactions due to low statistical power. \emph{bioRxiv}.
#'
#'Lajeunesse, M. J. (2011). On the meta-analysis of response ratios for studies with correlated and multi-group designs. \emph{Ecology}, 92(11), 2049-2055.
#'
#'@export
#'
#'

effect_size_multiplicative <- function(Control_N,
                                       Control_SD,
                                       Control_Mean,
                                       StressorA_N,
                                       StressorA_SD,
                                       StressorA_Mean,
                                       StressorB_N,
                                       StressorB_SD,
                                       StressorB_Mean,
                                       StressorsAB_N,
                                       StressorsAB_SD,
                                       StressorsAB_Mean,
                                       Significance_Level){


  ### Need some checks here to ensure input data is correct.


  ### Ensure all N, SD, and Mean are numeric
  booleans_numeric <- c(is.numeric(Control_N) | is.na(Control_N),
                        is.numeric(Control_SD) | is.na(Control_SD),
                        is.numeric(Control_Mean) | is.na(Control_Mean),
                        is.numeric(StressorA_N) | is.na(StressorA_N),
                        is.numeric(StressorA_SD) | is.na(StressorA_SD),
                        is.numeric(StressorA_Mean) | is.na(StressorA_Mean),
                        is.numeric(StressorB_N) | is.na(StressorB_N),
                        is.numeric(StressorB_SD) | is.na(StressorB_SD),
                        is.numeric(StressorB_Mean) | is.na(StressorB_Mean),
                        is.numeric(StressorsAB_N) | is.na(StressorsAB_N),
                        is.numeric(StressorsAB_SD) | is.na(StressorsAB_SD),
                        is.numeric(StressorsAB_Mean) | is.na(StressorsAB_Mean))

  if(FALSE %in% booleans_numeric == TRUE){
    stop("Ensure that all values for all variables are numeric or NA")
  }


  ### Ensure Significance_Level is >0 and <1 or 'missing'
  if(is.numeric(Significance_Level) == FALSE){
    stop("Please ensure that Significance_Level is numeric")
  }
  if(Significance_Level < 0 | Significance_Level > 1){
    stop("Please ensure that Significance_Level is between 0 and 1")
  }

  ### Check whether N variables are all integers
  integer_check <- c(Control_N, StressorA_N, StressorB_N, StressorsAB_N)
  if(length(integer_check) != length(integer_check[integer_check %% 1 == 0])){
    warning("It is expected that all sample sizes will be integer values.
            Double check whether non-integer values for sample sizes are correct.")
  }

  ###Check whether Means are greater than 0
  check_0 <- c(Control_Mean, StressorA_Mean, StressorB_Mean, StressorsAB_Mean)
  if(length(check_0[check_0 > 0]) != length(check_0)){
    stop("Given the logarithmic transformation this effect size calculation implements,
         please ensure that all means are greater than 0.")
  }



  ### Ensure that the lengths of all variables are the same
  lengths <- c(length(Control_N),
               length(Control_SD),
               length(Control_Mean),
               length(StressorA_N),
               length(StressorA_SD),
               length(StressorA_Mean),
               length(StressorB_N),
               length(StressorB_SD),
               length(StressorB_Mean),
               length(StressorsAB_N),
               length(StressorsAB_SD),
               length(StressorsAB_Mean))

  if(length(unique(lengths)) != 1){
    stop("Ensure that all variables are the same length")
  }



  #Multiplicative Null Model Functions
  Multiplicative_Effect_Size_Variance <- function(N_C, N_A, N_B, N_I, S_C, S_A, S_B, S_I, D_C, D_A, D_B, D_I){
    M_V <- (((S_C*S_C) / (N_C*D_C*D_C)) + ((S_A*S_A) / (N_A*D_A*D_A)) + ((S_B*S_B) / (N_B*D_B*D_B)) + ((S_I*S_I) / (N_I*D_I*D_I)))
    return(M_V)
  }

  Multiplicative_Effect_Size <- function(D_C, D_A, D_B, D_I){
    E_S <- log(D_C) - log(D_A) - log(D_B) + log(D_I)
    return(E_S)
  }



  df_MA <- base::data.frame(Control_N,
                            Control_SD,
                            Control_Mean,
                            StressorA_N,
                            StressorA_SD,
                            StressorA_Mean,
                            StressorB_N,
                            StressorB_SD,
                            StressorB_Mean,
                            StressorsAB_N,
                            StressorsAB_SD,
                            StressorsAB_Mean)



  #Calculate Additive Interaction Effect Size
  df_MA$Interaction_Effect_Size <- NA
  df_MA$Interaction_Effect_Size <- base::mapply(Multiplicative_Effect_Size,
                                                df_MA$Control_Mean,
                                                df_MA$StressorA_Mean,
                                                df_MA$StressorB_Mean,
                                                df_MA$StressorsAB_Mean)

  #Calculate Additive Interaction Effect Size Variance
  df_MA$Interaction_Variance <- NA
  df_MA$Interaction_Variance <- base::mapply(Multiplicative_Effect_Size_Variance,
                                             df_MA$Control_N,
                                             df_MA$StressorA_N,
                                             df_MA$StressorB_N,
                                             df_MA$StressorsAB_N,
                                             df_MA$Control_SD,
                                             df_MA$StressorA_SD,
                                             df_MA$StressorB_SD,
                                             df_MA$StressorsAB_SD,
                                             df_MA$Control_Mean,
                                             df_MA$StressorA_Mean,
                                             df_MA$StressorB_Mean,
                                             df_MA$StressorsAB_Mean)
  #Calculate Standard Error
  df_MA$Interaction_Standard_Error <- base::sqrt(df_MA$Interaction_Variance)

  #Calculate 95% Confidence Intervals
  df_MA$Interaction_CI          <- df_MA$Interaction_Standard_Error * abs(stats::qnorm(Significance_Level/2))
  df_MA$Interaction_CI_Upper   <- df_MA$Interaction_Effect_Size + df_MA$Interaction_CI
  df_MA$Interaction_CI_Lower   <- df_MA$Interaction_Effect_Size - df_MA$Interaction_CI

  df_MA$Null_Model <- "Multiplicative"

  drop_columns <- c("Interaction_Standard_Error", "Interaction_CI")

  df_MA <- df_MA[ , !(names(df_MA) %in% drop_columns)]

  #df_MA <- base::subset(df_MA, select = -c(Interaction_Standard_Error))
  #df_MA <- base::subset(df_MA, select = -c(Interaction_CI))



  return(df_MA)


  }


