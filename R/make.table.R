#' @title Experiments' Labels Predictor
#'
#' @description This package predict the label of the given experiment.
#'
#' @param directory The location (path) of the experiment data.
#' @param columns The included columns of the experiment data, input as list. Ex. c(1,2)
#' @param ExcludedSDBs The excluded SDBs of the experiment data, input as list. Ex. c("SDB3", "SDB77")
#'
#' @return table
#'
#' @examples
#'
#' @export make.table
#'

make.table = function(directory, columns, ExcludedSDBs){
  if(class(directory) == "character"){
    # check the file type
    split_name = strsplit(directory, "")
    name_length = length(split_name[[1]])
    file_type = paste0(split_name[[1]][name_length-3], split_name[[1]][name_length-2], split_name[[1]][name_length-1], split_name[[1]][name_length])
    if(file_type == ".txt"){
      if (class(columns) == "character" | class(columns) == "numeric"){
        columns = tryCatch(as.numeric(columns),
                           warning = function(w) {
                             stop("Please enter valid column NUMBER.")
                             return (columns)}
        )
        if (class(ExcludedSDBs) == "character"){
          test = read.table(directory, header = T)
          # make sure the format is correct
          if (all(toupper(colnames(test)[1:6]) == c("INDEX", "MINDEX", "PRIMER", "MOD", "NUC", "AA")) == TRUE){
            print("Correct input file. Creating table...")
            internal.make.table(directory, columns, ExcludedSDBs)
          } else{
            stop("This file is not in the correct format. The default format is: index, mindex, primer, mod, nuc, AA, and experiment data... *make sure the ALL spellings are correct.")
          }
        } else {
          stop("Please enter valid SDBs that you want to exclude. Ex. c('SDB3','SDB68').")
        }
      } else{
        stop("Please enter the included columns in a list.")
      }
    } else{
      stop("This is not a valid file type. (require .txt file)")
    }
  } else {
    stop("This is not a valid (format) directory. Maybe try as.character()?")
  }
}
