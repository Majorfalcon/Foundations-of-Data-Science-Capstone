---
title: "Capstone - Statistical Analysis"
author: "Andre Velez"
output:
  html_document: 
    keep_md: true
---

# __Trends__ 

The comparison matrix of the variables of interest yielded interesting trends and differences between the variables themselves, and between the most and least violent cities' data sets.  The diagonal of the matrix displayed differences in distribution density and central tendencies of the two data sets.  These differences will allow us to develop models, specific to each type of city, which will help predict the change in violent crime rate when other correlated factors (i.e. unemployment, median income, etc.) change, however hidden factors not examined in this study could be at play.  The correlation coefficient between violent crime rate (VCR) and the remaining factors are significantly different between the two data sets (most violent vs. least violent). This will lead to developing separate predictive models by rank, and comparing the differences between significant predictors for VCR.

# __Comparison of factors to VCR__

An initial comparison of the data to VCR was only performed on GR and UR based upon our initial hypothesis that the raising of unemployment and lowering college graduation rate would alter the response of the violent crime rate in a city.  

### __Scatterplots__

Scatterplots were used to visualize VCR vs. GR and VCR vs. UR.  The results showed a dramatic difference between the cities with high VCR vs. low VCR.  Both GR and UR seem to have no statistical significance in low VCR cities.  The horizontal trend line indicates that the variance in data is random.  However, high VCR cities appear to have a slightly positive correlation with UR, and a slightly negative correlation with GR.  The error appears large in both plots, and was investigated more thoroughly with a summary analysis.

### __Histograms__

Histograms were created to evaluate the distribution differences between all 3 variables.  VCR reveals that the range for the top 100 least violent cities is much smaller than the right tailed distribution of the top 100 most violent cities.  This indicates there are more extreme examples of higher VCR than lower VCR. The GR histograms don't provide much detail in visualizing the differences between the two sets of data.  There appears to be a tighter concentration of GR on the lower end for more violent cities that will be examined closer.  The UR histograms reveal more violent cities have a higher distribution of unemployment.  All these factors seem to coincide with the hypothesis that unemployment rate has a positive correlation with violent crime rate, whereas graduation rate has a negative correlation.

### __Density__

Density plots were evaluated to easily compare the distributions on a relative scale rather than a direct count.  As seen in the histograms the VCR for the least violent cities is tightly concentrated from approximately 0 to 0.5%, and the most violent cities ranges from approximately 0.5% to 2%.  The GR density distributions appear to show the central tendency of the most violent cities is lower and more tightly concentrated between the interquartile range, whereas the GR for the least violent cities appears to be a normal distribution.  The UR density distributions were similar between the sets, with the most violent central tendency to be slightly higher, however interestingly both distributions have be bimodal.  This characteristic could be the result of a number of unforeseen factors, i.e. the data set isn't large enough, but is beyond the scope of this study.

### __Central Tendency__

A matrix of the factors in question was created to evaluate common statistical methods and create a correlation matrix of all variables used for this study.  The central tendency statistical methods evaluated were the mean, range, and quantile coefficients.  

Because the data sets were ranked by violent crime, the differences in VCR were expected. The mean of MV_VCR was ~0.84%, compared to the mean of the LV_VCR at ~0.23%.  The interquartile ranges follow the distributions previously examined. The significant value lies in the differences in range.  The LV_VCR is from ~0.05% to ~0.38% (a 0.33% total range), and the MV_VCR is from ~0.47% to ~2.1% (a 1.63% total range).  This shows a 5x difference in the extremes when comparing the two sets for VCR.  The GR statistics reveal the expected left shift to lower values of the MV set compared to the LV set.  The interquartile range does not show a significant difference in the distributions of the data as hypothesized from the density distribution. The MV_GR set was between ~39% and ~55%, and the LV_GR set from ~43% to ~60%. The UR statistical values confirm the right shift of the MV set towards higher values when compared to the LV set.  For example, the mean of the MV_UR is ~6.5% and the mean of the LV_UR is ~5.9%.

### __Correlation__

The correlation matrix contains all the variables that will be used as independent variables/predictors when creating a model to predict VCR via linear regression.  This matrix shows on the diagonal the remaining density distributions of the remaining variables (median income, median debt, retention rate of college students, and average cost of college).  On the lower side of the matrix, scatterplots of the data are mapped against their respective orthogonal variable, and a simple trend line is plotted to visualize positive/negative/no correlations.  The upper side of the matrix shows the calculated correlation values. This can give insight into the significance of the predictor in determining VCR with multiple linear regressions, and the predictors’ multicollinearity with other variables.
