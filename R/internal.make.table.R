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
#' @import data.table
#'
#' @export internal.make.table
#'
internal.make.table = function(directory, columns, ExcludedSDBs){
  # actually make table
  # set up the infos first
  file_name = directory
  file_col = columns
  file_sdb = ExcludedSDBs

  # read table
  data_temp = read.table(directory, header = T)
  if (any(grepl("XX", apply(data_temp[1,], 2, as.character)))){ # the total counts row
    data_temp = data_temp[-1,]
  }
  if (length(unique(data_temp$Mod)) != 1){ # more than one name, things can move on!
    # get the wanted test columns (from the camp file!)
    # extract the nuc and data columns
    needed_col = c(5)
    for (col_i in 1:length(file_col)){
      file_col_temp = as.numeric(file_col[col_i])+6
      needed_col = c(needed_col, file_col_temp)
    }
    data_temp = data_temp[, needed_col]
    dict = system.file("extdata/all_dict.csv", package = "LabelFilename")
    dict_temp = read.csv(dict, header = T)

    # get rid of the unwanted sdb
    if (file_sdb != ""){
      for (sdb_i in 1:length(file_sdb)){
        remove_sdb = which(dict_temp$sdb == trimws(file_sdb[sdb_i]))
        if (any(remove_sdb)){
          dict_temp = dict_temp[-remove_sdb,]
        }
      }
    }

    # see how many ways to split the str
    ways = unique(dict_temp[c("first", "second")])
    # loops that amount of times

    side_df = data.frame()
    for (ways_i in 1:length(ways)){
      first = ways[[1]][ways_i]
      first = strsplit(strsplit(as.character(first), "\\[")[[1]][2], ":")[[1]]
      second = ways[[2]][ways_i]
      second = strsplit(strsplit(as.character(second), "\\]")[[1]][1], ":")[[1]]
      for (data_row in 1:nrow(data_temp)){
        compare_nuc = data_temp$Nuc[data_row]
        # split now!
        string_1 = substring(compare_nuc, first[1], first[2])
        string_2 = substring(compare_nuc, second[1], second[2])
        compare_nuc = paste0(string_1, string_2)
        found_dict = which(dict_temp$nuc == compare_nuc)
        if(any(found_dict)){
          found_sdb = dict_temp$sdb[found_dict]
          found_all = cbind(found_sdb, data_temp[data_row, ])
          side_df = rbind(side_df, found_all)
          data_temp = data_temp[-data_row,]
        }
      }
    }

    # now first combine the side_df
    side_df_temp = data.frame()
    unique_sdb = unique(side_df$found_sdb)
    for (sdb_i in 1:length(unique_sdb)){
      needed_sdb = as.character(unique_sdb[sdb_i])
      sdb_row = which(side_df$found_sdb == needed_sdb)
      sdb_chunk = side_df[sdb_row,]
      if (ncol(sdb_chunk) <= 3){
        sdb_sum = sum(sdb_chunk[,3])
      }else{
        sdb_sum = t(colSums(sdb_chunk[,3:ncol(sdb_chunk)]))
      }
      sdb_df = data.frame(sdb_chunk[1,1:2], sdb_sum)
      side_df_temp = rbind(side_df_temp, sdb_df)
    }

    #good!
    side_df = side_df_temp

    # now, take each row nuc again and compare to each row in the remaining row in data_temp
    side_df_temp = data.frame() # reset
    for (sdb_i in 1:nrow(side_df)){
      needed_nuc = side_df$Nuc[sdb_i]
      keep = c(which(stringdist::stringdist(needed_nuc, data_temp$Nuc) == 0), which(stringdist::stringdist(needed_nuc, data_temp$Nuc) == 1))
      sum_data = data_temp[keep,]
      data_temp = data_temp[-keep,]
      if (ncol(sum_data) <=2){
        colnames(side_df)[2:3]=colnames(sum_data)
        sum_data = rbind(sum_data, side_df[sdb_i, 2:3])
        sum_data = sum(sum_data[,2])
      }else{
        sum_data = rbind(sum_data, side_df[sdb_i, 2:ncol(side_df)])
        sum_data = t(colSums(sum_data[, 2:ncol(sum_data)]))
      }
      sum_data = data.frame(sdb = side_df$found_sdb[sdb_i], sum_data)
      side_df_temp = rbind(side_df_temp, sum_data)
    }

    good_df = side_df_temp

    # create colnames (file name)
    colnames(good_df) = c("sdb", paste0("Column ", 1:(ncol(good_df)-1)))
    good_df = data.frame(good_df, Format = "Derda_Labelling", Name = strsplit(file_name, "/")[[1]][length(strsplit(file_name, "/")[[1]])])

    print("DONE !")
    return(good_df)

  } else{
    stop("The name in the Mod column are the same. Maybe error in the original data file")
  }
}
