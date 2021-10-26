
#'Generate Summary Figures
#'
#'Using the output from \code{\link{classify_interactions}} function, summary figures can be created using this function.
#'
#'The figures include:
#'
#'  a) The proportions of the different interaction classifications from the dataset
#'
#'  b) Median sample sizes plotted against effect size (different interaction classifications are highlighted).
#'     Where the additive null model was used in the analysis, lines for critical effect sizes are plotted
#'     (see \code{\link{critical_effect_size_additive}} function).
#'
#'  c) Density of different median sample sizes.
#'
#'  d) Inverse of effect size variance plotted against effect size (i.e., one iteration of a funnel plot).
#'
#'  e) Effect size standard error (i.e., the square root of the effect size variance) plotted against effect size (i.e., one iteration of a funnel plot)).
#'
#'  Note that c - e) are most useful for researchers conducting a meta-analysis.
#'
#'@param effect_size_dataframe     Output from the \code{\link{classify_interactions}} function.
#'@param Small_Sample_Correction   Whether the correction for small sample sizes should be enacted
#'(TRUE or FALSE; default is TRUE) \emph{Note that if the multiplicative null model (see \code{\link{effect_size_multiplicative}})
#'was implemented, this parameter is not used and can be ignored.
#'If the additive null model (see \code{\link{effect_size_additive}}) was implemented, then this
#'parameter should be assigned the same value as in that analysis}.
#'@param Significance_Level        The value of alpha for which confidence intervals are calculated
#'(numeric, between 0 and 1; default is 0.05) \emph{Note that if the multiplicative null model (see \code{\link{effect_size_multiplicative}})
#'was implemented, this parameter is not used and can be ignored.
#'If the additive null model (see \code{\link{effect_size_additive}}) was implemented, then this
#'parameter should be assigned the same value as in that analysis}.
#'
#
#'
#'@return The function returns a series of figures each of which is outlined above.

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
#'                    remove_directionality = TRUE);
#'
#'#generate summary plots
#'df_plots <- summary_plots(effect_size_dataframe = df,
#'                    Significance_Level = 0.05)
#'
#'@export



summary_plots <- function(effect_size_dataframe,
                          Small_Sample_Correction,
                          Significance_Level){

  if(missing(Small_Sample_Correction) == TRUE){
    Small_Sample_Correction <- TRUE
  }

  if(missing(Significance_Level) == TRUE){
    Significance_Level <- 0.05
  }


  Sample_Size_Median <- Interaction_Effect_Size <- Interaction_Classification <- NULL



  df_MA <- effect_size_dataframe

  if(is.data.frame(df_MA) != TRUE){
    stop("effect_size_dataframe is not a data.frame
         Please specify effect_size_dataframe as the output from classify_interactions")
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
                          "Null_Model",
                          "Interaction_Classification")

  if(FALSE %in% c(sort(check_col_names) == sort(col_names_correct)) == TRUE){
    stop("Column names are different to those anticipated
         Please specify effect_size_dataframe as the output from classify_interactions")
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
         Please specify effect_size_dataframe as the output from classify_interactions")
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
         Please specify effect_size_dataframe as the output from classify_interactions")
  }


  ###Ensure that null model is same across dataframe and that it is either additive or multiplicative
  if(length(unique(df_MA$Null_Model)) != 1){
    stop("There is more than one Null Model specified
         Please specify effect_size_dataframe as the output from classify_interactions")
  }
  if(unique(df_MA$Null_Model) %in% c("Additive", "Multiplicative") == FALSE){
    stop("Null model should be either 'Additive' or 'Multiplicative'
         Please specify effect_size_dataframe as the output from classify_interactions")
  }


  ###Check whether Means are greater than 0
  if(unique(df_MA$Null_Model) == "Multiplicative"){
    check_0 <- c(df_MA$Control_Mean, df_MA$StressorA_Mean, df_MA$StressorB_Mean, df_MA$StressorsAB_Mean)
    if(length(check_0[check_0 > 0]) != length(check_0)){
      stop("Please specify effect_size_dataframe as the output from classify_interactions")
    }}

  calculate_median <- function(x1, x2, x3, x4){

    x <- stats::median(x1, x2, x3, x4)
    floor(x)
    return(x)

  }


  df_MA$Sample_Size_Median <- mapply(calculate_median,
                                     df_MA$Control_N,
                                     df_MA$StressorA_N,
                                     df_MA$StressorB_N,
                                     df_MA$StressorsAB_N)




  check_int_class <- function(x){
    if(x == "Unable to Classify Interaction"){
      x <- "Unable\nto\nClass."
    }
    if(x == "Antagonistic"){
      x <- "Ant."
    }
    if(x == "Reversal"){
      x <- "Rev."
    }
    if(x == "Synergistic"){
      x <- "Syn."
    }
    return(x)
  }

  df_MA$Interaction_Classification <- mapply(check_int_class,
                                             df_MA$Interaction_Classification)


  if(is.na(match("Null", unique(df_MA$Interaction_Classification))) == TRUE){
    col_vals <- viridis::magma(dim(df_plot1)[1])
  }

  if(is.na(match("Null", unique(df_MA$Interaction_Classification))) == FALSE){
    col_vals <- viridis::magma(length(unique(df_MA$Interaction_Classification)))[2:length(unique(df_MA$Interaction_Classification))]
    col_vals <- append(col_vals, "Black", after = match("Null", sort(unique(df_MA$Interaction_Classification)))-1)
  }



  ##Plot1 - Proportions of interactions


  df_plot1 <- data.frame(Index=1:length(unique(df_MA$Interaction_Classification)))
  df_plot1$Interaction_Classification <- unique(df_MA$Interaction_Classification)
  df_plot1$Proportion <- NA

  for(i in 1:dim(df_plot1)[1]){
    df_plot1$Proportion[i] <- dim(subset(df_MA,
                                         df_MA$Interaction_Classification == df_plot1$Interaction_Classification[i]))[1]/dim(df_MA)[1]
  }





  plot1 <- ggplot2::ggplot(df_plot1,
                           ggplot2::aes(x=as.factor(Interaction_Classification),
                                        y=Proportion,
                                        fill=as.factor(Interaction_Classification) )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = col_vals) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none") +
    ggplot2::ylab("Proportion") +
    ggplot2::xlab("Interaction Classification") #+
  #ggplot2::annotate("text", x=0.5, y=0.9*max(df_plot1$Proportion), "i)")


  ##Plot 2 - Critical effect size plot

  plot2 <- ggplot2::ggplot(df_MA,
                           ggplot2::aes(x=Sample_Size_Median,
                                        y=Interaction_Effect_Size,
                                        fill=Interaction_Classification,
                                        shape=Interaction_Classification)) +

    ggplot2::geom_point() +
    ggplot2::scale_fill_manual(values = col_vals) +
    ggplot2::scale_colour_manual(values = rep("black", length(unique(df_MA$Interaction_Classification)))) +
    ggplot2::scale_shape_manual(values = c(21, 22, 23, 24, 25)[c(1:length(unique(df_MA$Interaction_Classification)))]) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none") +
    ggplot2::ylab("Effect Size") +
    ggplot2::xlab("Sample Size") #+
  #ggplot2::annotate("text", x=0.5, y=0.9*max(df_MA$Interaction_Effect_Size), "ii)")



  if(unique(df_MA$Null_Model) == "Additive"){

    critical_effect_size_additivef <- function(Control_N,
                                               StressorA_N,
                                               StressorB_N,
                                               StressorsAB_N,
                                               Small_Sample_Correction = TRUE,
                                               Significance_Level = 0.05){



      #### Need some checks here to ensure input data is correct.
      #
      #### Ensure all N, SD, and Mean are numeric
      #booleans_numeric <- c(is.numeric(Control_N) | is.na(Control_N),
      #                      is.numeric(StressorA_N) | is.na(StressorA_N),
      #                      is.numeric(StressorB_N) | is.na(StressorB_N),
      #                      is.numeric(StressorsAB_N) | is.na(StressorsAB_N))
      #
      #if(FALSE %in% booleans_numeric == TRUE){
      #  stop("Ensure that all values for all variables are numeric or NA")
      #}
      #
      #### Ensure Small_Sample_Correction is either TRUE or FALSE or 'missing'
      #if(Small_Sample_Correction %in% c(TRUE, FALSE) == FALSE){
      #  stop("Please ensure that Small_Sample_Correction is either TRUE or FALSE (note that default value is TRUE)")
      #}
      #
      #### Ensure Significance_Level is >0 and <1 or 'missing'
      #if(is.numeric(Significance_Level) == FALSE){
      #  stop("Please ensure that Significance_Level is numeric")
      #}
      #if(Significance_Level < 0 | Significance_Level > 1){
      #  stop("Please ensure that Significance_Level is between 0 and 1")
      #}
      #
      #### Check whether N variables are all integers
      #integer_check <- c(Control_N, StressorA_N, StressorB_N, StressorsAB_N)
      #if(length(integer_check) != length(integer_check[integer_check %% 1 == 0])){
      #  warning("It is expected that all sample sizes will be integer values.
      #          Double check whether non-integer values for sample sizes are correct.")
      #}
      #
      #### Ensure that the lengths of all variables are the same
      #lengths <- c(length(Control_N),
      #             length(StressorA_N),
      #             length(StressorB_N),
      #             length(StressorsAB_N))
      #
      #if(length(unique(lengths)) != 1){
      #  stop("Ensure that all variables are the same length")
      #}


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


    df_line1 <- data.frame(Sample_Size_Median = min(df_MA$Sample_Size_Median, na.rm = T):max(df_MA$Sample_Size_Median, na.rm = T))
    df_line1$Interaction_Effect_Size <- critical_effect_size_additivef(df_line1$Sample_Size_Median,
                                                                       df_line1$Sample_Size_Median,
                                                                       df_line1$Sample_Size_Median,
                                                                       df_line1$Sample_Size_Median,
                                                                       Small_Sample_Correction,
                                                                       Significance_Level)

    df_line1$Interaction_Classification = "Null"

    df_line2 <- df_line1
    df_line2$Interaction_Effect_Size <- -df_line2$Interaction_Effect_Size


    plot2 <- plot2 +
      ggplot2::geom_line(data = df_line1, color="black") +
      ggplot2::geom_line(data = df_line2, color="black")

  }



  ##Plot 3 - Median Sample Size Distribution

  plot3 <- ggplot2::ggplot(df_MA,
                           ggplot2::aes(x=df_MA$Sample_Size_Median)) +
    ggplot2::geom_density(alpha=0.2, na.rm = T) +
    ggplot2::ylab("Density") +
    ggplot2::xlab("Sample Size") +
    #ggplot2::annotate("text", x=0.5, y=0.9, "iii)") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")



  ##Plot 4 - Effect size to inverse of variance

  plot4 <- ggplot2::ggplot(df_MA,
                           ggplot2::aes(y=1/(df_MA$Interaction_Variance),
                                        x=df_MA$Interaction_Effect_Size)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Effect Size") +
    ggplot2::ylab("Inverse Variance") +
    #ggplot2::annotate("text", x=0.5, y=0.9*max(df_MA$Interaction_Effect_Size), "iv)") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")



  ##Plot 5 - Effect size to inverse of variance

  df_MA$Interaction_Standard_Error <- sqrt(df_MA$Interaction_Variance)

  plot5 <- ggplot2::ggplot(df_MA,
                           ggplot2::aes(y=Interaction_Standard_Error,
                                        x=Interaction_Effect_Size)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Effect Size") +
    ggplot2::ylab("Standard Error") +
    #ggplot2::annotate("text", x=0.5, y=0.9*max(df_MA$Interaction_Effect_Size), "v)") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")



  plot_overall <- patchwork::wrap_plots(plot1, plot2, plot3, plot4, plot5, ncol = 3, nrow=2)
  return(plot_overall)

  }

