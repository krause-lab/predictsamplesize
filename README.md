Predictsamplesize
=================
This package provides a way to predict the errorrates for certain sample sizes, based on a classifier that was trained and tested on a pilot dataset. This is done using the classifiers from the mlr3learners package. Also, this package provides a way to plot the predicted errorrates.

Installation
------------
The most recent version of Predictsamplesize can be downloaded and installed using
```r
library(devtools)
install_github('krause-lab/predictsamplesize')
```

Preparing example data
---
To show the functionality of the package, data from the Golub et. al study from 1999, 'Molecular classification of cancer: class discovery and class prediction by gene expression monitoring' is preproccessed and used using the following code.

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("golubEsets")
library(golubEsets)
library(SummarizedExperiment)
library(this.path)  ## to get full path of script

main.dir = dirname(this.path())

data("Golub_Merge")

se = SummarizedExperiment(
  assays = list(expr = exprs(Golub_Merge)),
  colData = pData(Golub_Merge))
saveRDS(se,
        file = file.path(main.dir, "se_golub.rds"))


## PCA
library(QCnormSE)

res.pca = calculate_mds_pca(se = se,
                            method = "pca")
plot_mds_pca_2d(res.pca,
                se = se,
                var.color = "ALL.AML")

```

Using the Package
---

We start by loading the package.

```r
library(Predictsamplesize)
```

After loading the library we set the parameters for the first of the first functions that is provided by this package.

```r
golub <- readRDS(file.path(main.dir, 'se_golub_rds'))
assay <- 'expr'
outcome <- 'ALL.AML'
```

After that we start the first function two times, where the first time a balanced approach is used and the second time an unbalanced approach is used. For example purposes a Random Forest classifier is chosen for both approaches. Also a repetition of 10 is chosen to generate the result faster.

```r
set.seed(1234)
ALL.AML_TRUE <- train_pred_model_on_subsets(se = golub, assay = assay, outcome = outcome, 
                                    classifier = "classif.ranger", balanced = TRUE, n_rep = 10)
ALL.AML_FALSE <-  train_pred_model_on_subsets(se = golub, assay = assay, outcome = outcome, 
                                    classifier = "classif.ranger", balanced = FALSE, n_rep = 10)
```

The resulting dataframes contain the mean, 25 and 75 quantile errorrates for all 10 used groupsizes, as well as the pvalue that was gained by using a onesided wilcoxon-test against the percentage of the lower group.

```r
ALL.AML_TRUE
```

After running the function two times a second function is used to predict the errorates up to a treshold of 500 individuals.

```r
set.seed(1234)
ALL.AML_TRUE_est <- estimate_learning_curve(ALL.AML_TRUE, 500)
ALL.AML_FALSE_est <- estimate_learning_curve(ALL.AML_FALSE, 500)
```

The resulting dataframe contains the the sized for which the error was predicted, as well as the predicted errors for the mean and the 25/75 quantile.

```r
ALL.AML_TRUE_est
```

After doing this we display the estimated errorrates and the mean errorates that were gained using the pilotdataset.

```r
plot_learning_curve(ALL.AML_TRUE_est , ALL.AML_TRUE)
plot_learning_curve(ALL.AML_FALSE_est , ALL.AML_FALSE)
```

Now it can be clearly seen, what sample size should be choosen to only get a certain error when training a classifier on the data.
