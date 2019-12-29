# SimRDS Package

## 1.How to install
```{r setup, include=FALSE}
Install from GitHub :  devtools::install_github("AyeshaPerera/SimRDS")
```

## 2.Description
For better understanding of the usage of the package refer to **Perera, A., & Ramanayake, A. (2019). Assessing the effects of respondent driven sampling estimators on population characteristics. Proceeding of Asia International Conference on Multidisciplinary Research** 
Could be used to identify the fitting seeds, coupons and waves that should be used to extract samples and the most fitting estimator that should be used to make the estimate.
Samples extracted using the package could be used to make estimates from the estimators defined in the RDS package.

## 3.Guidelines
### 3.1.For Empirical Researchers
Except for the specially highlighted situation, in all other cases any estimator can be used to do the estimations.
<ul>
  <li><h4>For the Researcher at the very ground level who has not collected any data</h4></li>
        If the researcher has a sense of the population as whether the distribution of degrees, whether the parameter         is high or low, association of response with response with other variables (or at least a few of it), can use         the algorithm proposed in this study to simulate a similar population to determine the best combination of            seeds, coupons and estimator that is optimal for their study and then implement the empirical study                   accordingly.
        If the researcher does not have a sense of the population of interest an insight can be gained by conducting a         pilot study.  The proposed algorithm can be used to generate populations similar to the properties identified         by the pilot study and identify suitable variables accordingly 
  <li><h4>If the researcher has already collected the RDS sample </h4></li>
        Then the specific number of seeds and coupons and rough value for the parameter will be known. With that              similar populations can be simulated and perform estimations to identify the estimator that works well for the         used number of seeds and coupons 
</ul>
### 3.2.For Theoretical researchers
Researchers can simulate populations with different properties and check the performance of the proposed inference in a broader manner


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to GitHub. When you click the **Knit** button all R code chunks are run and a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
