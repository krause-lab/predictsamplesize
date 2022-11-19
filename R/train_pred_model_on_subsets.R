#' @title Train pred model on subsets
#' @aliases train_pred_model_on_subsets
#'
#' @description This function takes an the data from an se object containing patient data from a pilotstudy to train and test a classifier by randomly splitting the data in two datasets, training the classifier on one set and than testing it on the other. The resulting errorrates are then tested with a one sided Wilcoxon-test against the ratio of the data of the smallest group. Than the function returns the used groupsizes, the pvalue and the mean and 25/ 75 \% quantile of the errorrates.
#' @usage train_pred_model_on_subsets(se, assay, outcome, n_size = 10, n_rep = 50, balanced = FALSE, classifier)
#'
#' @param se A se storing the assay, the data and the outcome.
#' @param assay A string, storing which of the assays of the se should be chosen.
#' @param outcome A string, storing which of the outcomes of the se should be chosen.
#' @param n_size An integer defining how many different groupsizes should be used. (Default = 10)
#' @param n_rep An integer defining how many reptetions should be performed. (Default = 50)
#' @param balanced A logic variable, defining if the trainingset should be balanced or have the same ratios as the original dataset.
#' @param classifier A string with the classifier that should be used, from the options of mlr3learners.
#'
#' @return A dataframe containing the size of the trainingssets, the mean error, the 25 and 75 \% quantile of the error and the pvalue, tested with a one sided wilcox test against the ratio of the smaller group.
#'
#' @author Jonathan Krause
#'
#' @seealso \code{estimate_learning_curve()}, \code{plot_learning_curve()}.
#'
#' @export

train_pred_model_on_subsets <- function(se, assay, outcome, n_size = 10, n_rep = 50, balanced = FALSE, classifier){
  # Test Input Parameters
  if(is(se, "SummarizedExperiment") == FALSE){
    stop(paste0("se should be of type se, is of type ", typeof(data)))
  }
  type_check(assay, 'character')
  type_check(outcome, 'character')
  type_check(n_size, 'double')
  type_check(n_rep, 'double')
  type_check(balanced, 'logical')

  # check outcome
  if(outcome %in% colnames(SummarizedExperiment::colData(se)) == FALSE){
    stop('outcome has to be one of the colData of the Summarized Experiment.')
  }

  # check classifier
  library(mlr3learners)
  name <- names(as.list(as.list(mlr3::lrns())$items))
  name <- name[which(grepl('classif', name))]
  if(classifier %in% name == FALSE){
    stop('classifier has to be one from the mlr3 or mlr3learners package.')
  }

  if(n_size <= 0){
    stop('n_size should be larger than zero')
  }
  if(n_rep <= 0){
    stop('n_rep should be larger than zero')
  }

  # Initialize variables
  groups <- list()
  error <- data.frame(matrix(nrow = n_size, ncol = n_rep))
  error_mean <- rep(0, n_size)
  error_lower_quant <- rep(0, n_size)
  error_upper_quant <- rep(0, n_size)
  wilcox  <- rep(0, n_size)
  pvalue <- NA

  # extract the outcome levels
  out_level <- SummarizedExperiment::colData(se)[, outcome]

  if(length(na.omit(out_level)) <= length(out_level)/2){
    warning('Half or more than half patients have missing outcome variables.')
  }

  # Extract the data from the experiment
  ex_data <- SummarizedExperiment::assay(se)

  if(nrow(ex_data) > ncol(ex_data)){
    ex_data <- t(ex_data)
  }

  ex_data <- data.frame(ex_data, out_level)


  # Prepare the dataset
  ex_data <- ex_data[!is.na(ex_data$out_level), ] # Remove entrys with NA in the outcome
  ex_data <- ex_data[, which(colMeans(!is.na(ex_data)) > 0.5)]
  ex_data$out_level <- factor(ex_data$out_level) # Factorize the out_level to correspond with the size_group factors
  levels(ex_data$out_level) <- 1:length(levels(ex_data$out_level))

  # Get the dimensions of the data
  data_length <- nrow(ex_data)
  data_size <- ncol(ex_data)


  if(data_length < 15){
    stop("Dataset not big enough for this approach.")
  }

  rownames(ex_data) <- 1:data_length # Set rownames that can easily be accessed

  # mlr3: define the task and the learner that is to be used
  task <- mlr3::as_task_classif(ex_data, target = 'out_level')
  learner <- mlr3::lrn(classifier)

  # Get the amount of patients in each group and number of groups
  n_group <- length(unique(na.omit(out_level)))
  size_group <- data.frame(table(out_level))

  # Check if the dataset is big enough
  if(balanced == TRUE){
    if(min(size_group$Freq) < 15)
      stop('Dataset not balanced enough to warrant a balanced training.')
  }

  # Get the maximum trainingssetsize
  if(balanced == TRUE){
    # When balanced, the trainset can at best be (size smaller group - 5) * 2
    # to leave patients for the testset.
    max <- (min(size_group$Freq) - 5) * 2
  }
  else{
    # in any other case, the max trainsize is 10 smaller than the datasize
    max <- data_length - 10
  }

  # Get the actual trainset sizes that should be used
  size <- round(seq(10, max, length.out = n_size))

  # Split the datarows into the different groups according to outcome
  for(i in 1:n_group){
    groups[[i]] <- rownames(ex_data[as.numeric(ex_data$out_level) == as.numeric(size_group$out_level[i]), ])
  }

  for(a in 1:n_size){
    # Get the ratios of the groups for building the trainset
    ratio <- rep(0, n_group)

    if(balanced == TRUE){ # if balanced, the groups are equally represented
      ratio[1:n_group] <- 0.5
    }
    else{
      # Get the ratios to be only 2 digits behind the comma for easier size calculation
      ratio <- round(size_group[, 2]/sum(size_group[,2]), digits = 2)
    }

    # Compute how big the groups have to be
    ratio_size <- round(ratio * size[a])

    for(i in 1:n_rep){

      # Build the Train- and Testset via rownames
      train_set <- NA
      for(j in 1:n_group){
        train_set <- c(train_set, sample(unlist(groups[[j]]), ratio_size[j]))
      }
      train_set <- as.integer(train_set[-1])

      # Take all the remaining patients for the testset
      test_set <- setdiff(seq_len(task$nrow), train_set)

      # get the errorrates from the prediction
      learner$train(task, row_ids = train_set)
      prediction <- learner$predict(task, row_ids = test_set)

      error[a,i] <- unname(prediction$score())

    }
    # Somehow r doesn't think error is numeric, even though it is
    # Get Mean and Quantiles
    error_mean[a] <- mean(as.numeric(error[a,]))
    error_lower_quant[a] <- quantile(as.numeric(error[a,]), probs = 0.25)
    error_upper_quant[a] <- quantile(as.numeric(error[a,]), probs = 0.75)

    # Perform one sided Wilcox Test to evaluate the pvalue of the errors
    pvalue[a] <- wilcox.test(
      as.numeric(error[a,]),
      mu = min(ratio),
      alternative = "less",
      correct = FALSE)$p.value
  }

  # Construct the return dataframe
  return_df <- data.frame(size, error_mean, error_lower_quant, error_upper_quant, pvalue)
  return(return_df)
}

