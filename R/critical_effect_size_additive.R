
#'Critical Effect Sizes (for the additive null model)
#'
#'The critical effect size for a given experimental design (i.e., number of replicates per treatment).
#'
#'The critical effect size represents the minimum effect size required for a significant result to be returned
#'(see Burgess et al. (2021)).
#'
#'For the additive null model, the critical effect size is directly related to treatment sample sizes.
#'
#'
#'@param Control_N                 Sample size of the control treatment (numeric)
#'@param StressorA_N               Sample size of stressor A treatment (numeric)
#'@param StressorB_N               Sample size of stressor B treatment (numeric)
#'@param StressorsAB_N             Sample size of stressors A and B treatment (numeric)
#'
#'@param Small_Sample_Correction   Whether the correction for small sample sizes should be enacted
#'(TRUE or FALSE; default is TRUE)
#'@param Significance_Level        The value of alpha for which confidence intervals are calculated
#'(numeric, between 0 and 1; default is 0.05)
#'
#
#'
#'@return The function returns the critical effect size (for the additive null model; see \code{\link{effect_size_additive}})
#'for a given experimental design.

#'@examples
#'critical_effect_size_additive(Control_N     = 4,
#'                              StressorA_N   = 4,
#'                              StressorB_N   = 4,
#'                              StressorsAB_N = 4)
#'
#'critical_effect_size_additive(Control_N     = 3,
#'                              StressorA_N   = 3,
#'                              StressorB_N   = 3,
#'                              StressorsAB_N = 3,
#'                              Small_Sample_Correction = FALSE,
#'                              Significance_Level = 0.10)
#'
#'@references
#'
#'Burgess, B. J., Jackson, M. C., & Murrell, D. J. (2021). Multiple stressor null models frequently fail to detect most interactions due to low statistical power. \emph{bioRxiv}.
#'
#'@export


critical_effect_size_additive <- function(Control_N,
                                          StressorA_N,
                                          StressorB_N,
                                          StressorsAB_N,
                                          Small_Sample_Correction,
                                          Significance_Level){



  ### Need some checks here to ensure input data is correct.

  if(missing(Small_Sample_Correction) == TRUE){
    Small_Sample_Correction <- TRUE
  }

  if(missing(Significance_Level) == TRUE){
    Significance_Level <- 0.05
  }


  ### Ensure all N, SD, and Mean are numeric
  booleans_numeric <- c(is.numeric(Control_N) | is.na(Control_N),
                        is.numeric(StressorA_N) | is.na(StressorA_N),
                        is.numeric(StressorB_N) | is.na(StressorB_N),
                        is.numeric(StressorsAB_N) | is.na(StressorsAB_N))

  if(FALSE %in% booleans_numeric == TRUE){
    stop("Ensure that all values for all variables are numeric or NA")
  }

  ### Ensure Small_Sample_Correction is either TRUE or FALSE or 'missing'
  if(Small_Sample_Correction %in% c(TRUE, FALSE) == FALSE){
    stop("Please ensure that Small_Sample_Correction is either TRUE or FALSE (note that default value is TRUE)")
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

  ### Ensure that the lengths of all variables are the same
  lengths <- c(length(Control_N),
               length(StressorA_N),
               length(StressorB_N),
               length(StressorsAB_N))

  if(length(unique(lengths)) != 1){
    stop("Ensure that all variables are the same length")
  }


  z_v <- stats::qnorm(Significance_Level/2)


  if(Small_Sample_Correction == TRUE){
    J_m <- function(m){

      m <- m-4

      J <- 1 - (3)/((4*m) -1)
      return(J)
    }
  }

  if(Small_Sample_Correction == FALSE){
    J_m <- function(m){

      J <- 1
      return(J)
    }
  }

  m <- Control_N + StressorA_N + StressorB_N + StressorsAB_N

  es_cv <- (1/Control_N + 1/StressorA_N + 1/StressorB_N + 1/StressorsAB_N)*(((2*z_v*z_v)*(m)*J_m(m)*J_m(m))/(2*(m)-z_v*z_v*J_m(m)*J_m(m)))

  es_cv <- sqrt(es_cv)

  return(es_cv)

  }

