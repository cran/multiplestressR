# multiplestressR

For a detailed description of the multiplestressR package, see [this website](https://benjburgess.github.io/multiplestressR/).

## Aims of the package

The multiplestressR package, is aimed at researchers primarily working in multiple stressor ecology, to easily implement either the additive or multiplicative null models. These null model can be applied to the researcher's own primary data, or for data collated from multiple sources as part of a larger project (e.g., a meta-analyses). 

The package can be used to calculate either the additive or multiplicative null models, which correspond to the factorial forms of Hedges' d or the response ratio respectively. Furthermore, the multiplestressR package is capable of classifying interactions base upon a commonly implemented framework.

The multiplestressR package does not require researchers to have a firm understanding of the statistics of these null models. Instead, the user simply needs to specify the input data and the entire analysis will be conducted. As such, the multiplestressR package provides an outlet which allows researchers to implement a consistent, standardised, and statistically rigorous method for classifying stressor interactions across different analyses.

## Accessing the package

At present the package can be accessed from github using the following code:

`
library(devtools)
`

`
install_github("benjburgess/multiplestressR")
`

`
library(multiplestressR)
`

This package is being updated in preparation to being submitted to CRAN. Once it is uploaded, these instructions for installing the package will be updated.


## multiplestressR Example

Outlined below is an example for how the multiplestressR package can be conducted on a dataset. Assuming the package has been installed (see above section) the following code allows researchers to implement the additive null model on an example dataset provided within the multiplestressR package.

`
library(multiplestressR)
`

The multiplestressR package comes with an example dataset called `survival`, this dataset will be used throughout this example.

`
df <- multiplestressR::survival
`

For simplicity `survival` has been renamed `df`.

`
head(df)
`

By using, `head(df)` we can see the different columns within our dataset. It is evident that our dataset contains the variables of sample size, mean, and standard deviation for four different treatments. Here, the treatments correspond to control conditions, a temperature stressor, a pH stressor, and both a temperature and pH stressor.

For the analyses, control conditions corresponds to the Control treatment, temperature to the Stressor A treatment, pH to Stressor B treatment, and both temperature and pH to the Stressors A and B treatment. (Note that temperature and pH could have been assigned to the Stressors B and A treatments - the ordering for these two treatments is not important).

The additive null model can be calculated using the following code:

`
df2 <- effect_size_additive(Control_N         = df$Sample_Size_Control,
`

`                            
Control_SD        = df$Standard_Deviation_Control,
`

`
Control_Mean      = df$Mean_Control,
`

`
StressorA_N       = df$Sample_Size_Temperature,
`

`
StressorA_SD      = df$Standard_Deviation_Temperature,
`

`
StressorA_Mean    = df$Mean_Temperature,
`

`
StressorB_N       = df$Sample_Size_pH,
`

`
StressorB_SD      = df$Standard_Deviation_pH,
`

`
StressorB_Mean    = df$Mean_pH,
`

`
StressorsAB_N     = df$Sample_Size_Temperature_pH,
`

`
StressorsAB_SD    = df$Standard_Deviation_Temperature_pH,
`

`
StressorsAB_Mean  = df$Mean_Temperature_pH)
`

Here, the `effect_size_additive function` is used to calculate the additive null model for the example dataset. Note that the different parameter values (i.e., treatments, sample sizes, means and SDs) have to be specified by the user. 

This function returns a dataframe containing the specified values alongside the calculated values of `Interaction_Effect_Size`, `Interaction_Variance`, and confidence intervals. It is these values which are subsequently used to classify interactions, or should be used in meta-analyses.

`head(df2)`


The next step is to classify interactions, this can be easily conducted using the `classify_interactions` function. The function can be run in a single line of code. However, other options (e.g., distinguishing antagonistic and reversal interactions) can be specified.

Interactions can be classified using the following code:

`df2 <- classify_interactions(effect_size_dataframe = df2)`


At this stage, the calculation of the additive null model and subsequent classification of interactions has been conducted. However, one final option is to plot some summary figures to better understand these results. This can be done using the `summary_plots` function.

The summary plots can be generated using the following code:

`df_plots <- summary_plots(effect_size_dataframe = df2)`


## Summary

This is a very simple outline of a potential workflow for the multiplestressR package. For a more complex example of the multiplestressR package see [this tutorial](https://benjburgess.github.io/i/multiplestressR1). Likewise, the documentation for each of the functions details the various options and parameters which can be specified.



