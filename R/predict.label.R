#' @title Experiments' Labels Predictor
#'
#' @description This package predict the label of the given experiment.
#'
#' @param table
#'
#' @return table
#'
#' @examples
#' @import stats
#' @import e1071
#' @import caret
#' @import rpart
#' @import randomForest
#' @import glmnet
#'
#' @export predict.label
#'


predict.label = function(table){
  if (colnames(table)[ncol(table)-1] == "Format" & colnames(table)[1] == "sdb" & all(table[, (ncol(table)-1)] == "Derda_Labelling")){
    rownames(table) = table$sdb
    file_name = table$Name[1]
    table = subset(table, select = -c(sdb, Format, Name))
    print("Predicting...")
    internal.predict.label(table, file_name)
  } else{
    stop("wrong format. Please first use make.table() to convert the data frame into specific format.")
  }
}

