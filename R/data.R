#' Survival data for 250 populations exposed to the stressors of temperature and pH.
#'
#' A generated dataset on the survival rates of 250 populations (each composed of 100 individuals) exposed to the stressors of temperature and pH.
#' The dataset uses a factorial design comprising four treatments: i) Control conditions; ii) Exposed to temperature;
#' iii) Exposed to pH; iv) Exposed to both temperature and pH. This generated dataset is used in the examples for each function in the multiplestressR package.
#' Please note that this is a generated dataset and does not reflect data from actual ecological experiments.
#'
#' @format A data frame with 250 rows and 12 variables:
#' \describe{
#'   \item{Sample_Size_Control}{Number of samples for the control treatment}
#'   \item{Standard_Deviation_Control}{standard deviation for mean survival of the control treatment}
#'   \item{Mean_Control}{mean survival (as a proportion) for the control treatment}
#'
#'   \item{Sample_Size_Temperature}{Number of samples for the temperature treatment}
#'   \item{Standard_Deviation_Temperature}{standard deviation for mean survival of the temperature treatment}
#'   \item{Mean_Temperature}{mean survival (as a proportion) for the temperature treatment}
#'
#'   \item{Sample_Size_pH}{Number of samples for the pH treatment}
#'   \item{Standard_Deviation_pH}{standard deviation for mean survival of the pH treatment}
#'   \item{Mean_pH}{mean survival (as a proportion) for the pH treatment}
#'
#'   \item{Sample_Size_Temperature_pH}{Number of samples for the combined temperature and pH treatment}
#'   \item{Standard_Deviation_Temperature_pH}{standard deviation for mean survival of the combined temperature and pH treatment}
#'   \item{Mean_Temperature_pH}{mean survival (as a proportion) for the combined temperature and pH treatment}
#' }
"survival"
