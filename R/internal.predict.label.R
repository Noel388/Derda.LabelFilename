#' @title Experiments' Labels Predictor
#'
#' @description This package predict the label of the given experiment.
#'
#' @param test_dict
#' @param file_name
#'
#' @return table
#'
#' @examples
#'
#' @import stats
#' @import e1071
#' @import caret
#' @import rpart
#' @import randomForest
#' @import glmnet
#'
#' @export internal.predict.label
#'

internal.predict.label = function(test_dict, file_name){
  # load environemnt again in case
  data(sysdata, envir=environment())
  # load function
  Mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  test_dict = t(test_dict)
  test_dict_rownames = paste0(file_name, 1:nrow(test_dict))
  if (class(test_dict) == "character"){
    test_dict_colnames = names(test_dict)
    test_dict = t(data.frame(as.numeric(test_dict)))
    colnames(test_dict) = test_dict_colnames
  } else{
    test_dict = apply(test_dict, 2, as.numeric)
  }
  rownames(test_dict) = test_dict_rownames

  # Run -- for EACH row:
  dict_predict_prob_df = c(1:6)
  rf_target_predict_prob_df = c(1:6)
  rf_post_predict_prob_df = c(1:6)
  for (row_i in 1:nrow(test_dict)){
    # dealing row
    test_row = t(data.frame(test_dict[row_i,]))

    # find the missing sdb and insert them
    missing_sdb = which(train_all_sdb %in% colnames(test_row))
    missing_sdb = train_all_sdb[-missing_sdb]
    missing_sdb_df = t(data.frame(rep(NA, length(missing_sdb))))
    colnames(missing_sdb_df) = missing_sdb
    test_row = data.frame(test_row, missing_sdb_df, check.names = F)

    # dict -----------------------------------------------------------
    test_row_dict = test_row

    # make 0/1
    test_row_dict[!is.na(test_row_dict)] = 1
    test_row_dict[is.na(test_row_dict)] = 0

    # predict!
    mlr_dict_predict_prob = stats::predict(mlr_dict_model, test_row_dict, type = "prob")
    dict_predict_prob = mlr_dict_predict_prob[order(mlr_dict_predict_prob, decreasing = T)]
    dict_predict = names(dict_predict_prob)[1]
    if (class(dict_predict_prob) != "numeric"){
      if (ncol(dict_predict_prob) < 3){
        if (ncol(dict_predict_prob) == 1){
          dict_predict_prob_df_temp = c(dict_predict_prob[1], "NA", "NA", names(dict_predict_prob[1]), "NA", "NA")
        }
        if (ncol(dict_predict_prob) == 2){
          dict_predict_prob_df_temp = c(dict_predict_prob[1], dict_predict_prob[2],"NA", names(dict_predict_prob[1]), names(dict_predict_prob[2]), "NA")
        }
      }else{
        dict_predict_prob_df_temp = c(dict_predict_prob[1], dict_predict_prob[2], dict_predict_prob[3], names(dict_predict_prob[1]), names(dict_predict_prob[2]), names(dict_predict_prob[3]))
      }
    } else{
      if (length(dict_predict_prob) < 3){
        if (length(dict_predict_prob) == 1){
          dict_predict_prob_df_temp = c(dict_predict_prob[1], "NA", "NA", names(dict_predict_prob[1]), "NA", "NA")
        }
        if (length(dict_predict_prob) == 2){
          dict_predict_prob_df_temp = c(dict_predict_prob[1], dict_predict_prob[2],"NA", names(dict_predict_prob[1]), names(dict_predict_prob[2]), "NA")
        }
      }else{
        dict_predict_prob_df_temp = c(dict_predict_prob[1], dict_predict_prob[2], dict_predict_prob[3], names(dict_predict_prob[1]), names(dict_predict_prob[2]), names(dict_predict_prob[3]))
      }
    }
    dict_predict_prob_df = rbind(dict_predict_prob_df, dict_predict_prob_df_temp)

    # target -----------------------------------------------------------
    # only do more than 1 prediction for target and acid if the dictionary accuracy is not 1
    #if (dict_predict_prob[1] == 1){ # only do once
    used_dict = names(dict_predict_prob[1])
    test_row_target = test_row
    test_row_target = test_row_target[,-which(is.na(test_row_target) == T)]
    test_row_target = replace(test_row_target, test_row_target <= 1, 1)
    test_row_target = log(test_row_target)
    temp_name = paste0(used_dict,"_target_rf_model")
    if (exists(temp_name)){
      col_name = paste0(used_dict,"_col_target_rf_model")
      model_variable = get(col_name)[1:(length(get(col_name))-1)]
      # remove extra variable
      test_row_target = test_row_target[,which(colnames(test_row_target) %in% model_variable == T)]
      # add in the missing variable
      missing = which((model_variable %in% colnames(test_row_target)) == F)
      if (length(missing) >0){
        for (missing_i in 1:length(missing)){
          test_row_target = data.frame(test_row_target, 0)
          colnames(test_row_target)[ncol(test_row_target)] = as.character(model_variable[missing[missing_i]])
        }
      }
      rf_target_predict_prob = stats::predict(get(temp_name), test_row_target, type = "prob")
      rf_target_predict_prob = data.frame(rf_target_predict_prob)
      rf_target_predict_prob = rf_target_predict_prob[,order(rf_target_predict_prob[1,], decreasing = T)]
    }else{
      model_variable =  rest_col_target_rf_model[1:(length(rest_col_target_rf_model)-1)]
      # remove extra variable
      test_row_target = test_row_target[,which(colnames(test_row_target) %in% model_variable == T)]
      # add in the missing variable
      missing = which((model_variable %in% colnames(test_row_target)) == F)
      if (length(missing) >0){
        for (missing_i in 1:length(missing)){
          test_row_target = data.frame(test_row_target, 0)
          colnames(test_row_target)[ncol(test_row_target)] = as.character(model_variable[missing[missing_i]])
        }
      }
      if (exists("rest_target_rf_model")){
        rf_target_predict_prob = stats::predict(rest_target_rf_model, test_row_target, type = "prob")
      }else{
        rf_target_predict_prob = 1
      }
      rf_target_predict_prob = data.frame(rf_target_predict_prob)
      rf_target_predict_prob = rf_target_predict_prob[,order(rf_target_predict_prob[1,], decreasing = T)]
    }

    if (class(rf_target_predict_prob) != "numeric"){
      if (ncol(rf_target_predict_prob) < 3){
        if (ncol(rf_target_predict_prob) == 1){
          rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], "NA", "NA", names(rf_target_predict_prob[1]), "NA", "NA")
        }
        if (ncol(rf_target_predict_prob) == 2){
          rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], rf_target_predict_prob[2],"NA", names(rf_target_predict_prob[1]), names(rf_target_predict_prob[2]), "NA")
        }
      }else{
        rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], rf_target_predict_prob[2], rf_target_predict_prob[3], names(rf_target_predict_prob[1]), names(rf_target_predict_prob[2]), names(rf_target_predict_prob[3]))
      }
    } else{
      if (length(rf_target_predict_prob) < 3){
        if (length(rf_target_predict_prob) == 1){
          rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], "NA", "NA", as.character(rest_target_rf_model_none), "NA", "NA")
        }
        if (length(rf_target_predict_prob) == 2){
          rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], rf_target_predict_prob[2],"NA", names(rf_target_predict_prob[1]), names(rf_target_predict_prob[2]), "NA")
        }
      }else{
        rf_target_predict_prob_df_temp = c(rf_target_predict_prob[1], rf_target_predict_prob[2], rf_target_predict_prob[3], names(rf_target_predict_prob[1]), names(rf_target_predict_prob[2]), names(rf_target_predict_prob[3]))
      }
    }

    rf_target_predict_prob_df = rbind(rf_target_predict_prob_df, rf_target_predict_prob_df_temp)

    # post -----------------------------------------------------------
    # only do more than 1 prediction for post and acid if the dictionary accuracy is not 1
    #if (dict_predict_prob[1] == 1){ # only do once
    used_dict = names(dict_predict_prob[1])
    test_row_post = test_row
    test_row_post = test_row_post[,-which(is.na(test_row_post) == T)]
    test_row_post = replace(test_row_post, test_row_post <= 1, 1)
    test_row_post = log(test_row_post)
    temp_name = paste0(used_dict,"_post_rf_model")
    if (exists(temp_name)){
      col_name = paste0(used_dict,"_col_post_rf_model")
      model_variable = get(col_name)[1:(length(get(col_name))-1)]
      # remove extra variable
      test_row_post = test_row_post[,which(colnames(test_row_post) %in% model_variable == T)]
      # add in the missing variable
      missing = which((model_variable %in% colnames(test_row_post)) == F)
      if (length(missing) >0){
        for (missing_i in 1:length(missing)){
          test_row_post = data.frame(test_row_post, 0)
          colnames(test_row_post)[ncol(test_row_post)] = as.character(model_variable[missing[missing_i]])
        }
      }
      rf_post_predict_prob = stats::predict(get(temp_name), test_row_post, type = "prob")
      rf_post_predict_prob = data.frame(rf_post_predict_prob)
      rf_post_predict_prob = rf_post_predict_prob[,order(rf_post_predict_prob[1,], decreasing = T)]
    }else{
      model_variable =  rest_col_post_rf_model[1:(length(rest_col_post_rf_model)-1)]
      # remove extra variable
      test_row_post = test_row_post[,which(colnames(test_row_post) %in% model_variable == T)]
      # add in the missing variable
      missing = which((model_variable %in% colnames(test_row_post)) == F)
      if (length(missing) >0){
        for (missing_i in 1:length(missing)){
          test_row_post = data.frame(test_row_post, 0)
          colnames(test_row_post)[ncol(test_row_post)] = as.character(model_variable[missing[missing_i]])
        }
      }
      if (exists("rest_post_rf_model")){
        rf_post_predict_prob = stats::predict(rest_post_rf_model, test_row_post, type = "prob")
      }else{
        rf_post_predict_prob = 1
      }
      rf_post_predict_prob = data.frame(rf_post_predict_prob)
      rf_post_predict_prob = rf_post_predict_prob[,order(rf_post_predict_prob[1,], decreasing = T)]
    }

    if (class(rf_post_predict_prob) != "numeric"){
      if (ncol(rf_post_predict_prob) < 3){
        if (ncol(rf_post_predict_prob) == 1){
          rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], "NA", "NA", names(rf_post_predict_prob[1]), "NA", "NA")
        }
        if (ncol(rf_post_predict_prob) == 2){
          rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], rf_post_predict_prob[2],"NA", names(rf_post_predict_prob[1]), names(rf_post_predict_prob[2]), "NA")
        }
      }else{
        rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], rf_post_predict_prob[2], rf_post_predict_prob[3], names(rf_post_predict_prob[1]), names(rf_post_predict_prob[2]), names(rf_post_predict_prob[3]))
      }
    } else{
      if (length(rf_post_predict_prob) < 3){
        if (length(rf_post_predict_prob) == 1){
          rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], "NA", "NA", as.character(rest_post_rf_model_none), "NA", "NA")
        }
        if (length(rf_post_predict_prob) == 2){
          rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], rf_post_predict_prob[2],"NA", names(rf_post_predict_prob[1]), names(rf_post_predict_prob[2]), "NA")
        }
      }else{
        rf_post_predict_prob_df_temp = c(rf_post_predict_prob[1], rf_post_predict_prob[2], rf_post_predict_prob[3], names(rf_post_predict_prob[1]), names(rf_post_predict_prob[2]), names(rf_post_predict_prob[3]))
      }
    }

    rf_post_predict_prob_df = rbind(rf_post_predict_prob_df, rf_post_predict_prob_df_temp)

  }
  dict_predict_prob_df = data.frame(dict_predict_prob_df[-1,])
  rf_target_predict_prob_df = data.frame(rf_target_predict_prob_df[-1,])
  rf_post_predict_prob_df = data.frame(rf_post_predict_prob_df[-1,])

  if (ncol(dict_predict_prob_df) > 4){
    most_dict = Mode(dict_predict_prob_df[,4])
    most_target = Mode(rf_target_predict_prob_df[,4])[[1]]
    most_post = Mode(rf_post_predict_prob_df[,4])[[1]]
  }else{
    dict_predict_prob_df = t(dict_predict_prob_df)
    most_dict = Mode(dict_predict_prob_df[,4])
    most_target = Mode(rf_target_predict_prob_df[,4])[[1]]
    most_post = Mode(rf_post_predict_prob_df[,4])[[1]]
  }

  # combine -----------------------------------------------
  predict_name = paste0(most_dict, most_target, most_post)

  cat("Done Prediction...\nCreating pdf....  (named 'results.pdf' in default)\nWarning messages may pop up, but can be safely ignored.\n")
  pdf("results.pdf")

  # create pdf =======================================
  # make a new plot
  plot.new()

  # stat the no. of input file
  text(x = 0.15, y = 1, paste0("Input File ", 1), font = 2, cex = 1)

  # state the input label of the file
  text(x = 0.242, y = 0.95, paste0("Input label: ", file_name), font = 1, cex = 0.8)
  # state the number of columns in the file
  plot_columns = nrow(test_dict)
  text(x = 0.161, y = 0.922, paste0("Number of input columns: ", plot_columns), font = 1, cex = 0.8)
  text(x = 0.357, y = 0.875, paste0("The predicted label (using Multinomial Regression) is: ", predict_name), font = 1, cex = 0.8)

  # inital start for x and y
  x = 0.173
  y = 0.816
  for (plot_i in 1:plot_columns){
    text(x, y, paste0("For column ", plot_i, ": (% confidence)"), font = 1, cex = 0.8)
    x = 0.515
    y = y - 0.04
    text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
    y = y - 0.03
    text(x, y, "1st                          2nd                         3rd", font = 1, cex = 0.8)
    y = y - 0.03
    x = 0.43
    text(x, y, paste0(
      "Dictionary                ", as.character(dict_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(dict_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 2])), digits = 3)*100, ")             ", as.character(dict_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
    ), font = 1, cex = 0.8)
    y = y - 0.03
    x = 0.413
    text(x, y, paste0(
      "Target                        ", as.character(rf_target_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(rf_target_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                  ", as.character(rf_target_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
    ), font = 1, cex = 0.8)
    y = y - 0.03
    x = 0.4
    text(x, y, paste0(
      "Post-processing          ", as.character(rf_post_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 1])), digits = 3)*100, ")            ", as.character(rf_post_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                ", as.character(rf_post_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
    ), font = 1, cex = 0.8)
    y = y - 0.03
    x = 0.515
    text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
    x = 0.173
    y = y - 0.1

    # in case there are nore than 3 columns
    if (plot_i == 4){
      # make a new plot --> repeats
      plot.new()
      # stat the no. of input file
      text(x = 0.15, y = 1, paste0("Input File ", 1), font = 2, cex = 1)
      # state the input label of the file
      text(x = 0.242, y = 0.95, paste0("Input label: ", file_name), font = 1, cex = 0.8)
      # state the number of columns in the file
      plot_columns = nrow(test_dict)
      text(x = 0.161, y = 0.922, paste0("Number of input columns: ", plot_columns), font = 1, cex = 0.8)
      text(x = 0.357, y = 0.875, paste0("The predicted label (using Multinomial Regression) is: ", predict_name), font = 1, cex = 0.8)

      # inital start for x and y
      x = 0.173
      y = 0.816
      for (plot_i in 4:plot_columns){
        text(x, y, paste0("For column ", plot_i, ": (% confidence)"), font = 1, cex = 0.8)
        x = 0.515
        y = y - 0.04
        text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
        y = y - 0.03
        text(x, y, "1st                          2nd                         3rd", font = 1, cex = 0.8)
        y = y - 0.03
        x = 0.43
        text(x, y, paste0(
          "Dictionary                ", as.character(dict_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(dict_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 2])), digits = 3)*100, ")             ", as.character(dict_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
        ), font = 1, cex = 0.8)
        y = y - 0.03
        x = 0.413
        text(x, y, paste0(
          "Target                        ", as.character(rf_target_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(rf_target_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                  ", as.character(rf_target_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
        ), font = 1, cex = 0.8)
        y = y - 0.03
        x = 0.4
        text(x, y, paste0(
          "Post-processing          ", as.character(rf_post_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 1])), digits = 3)*100, ")            ", as.character(rf_post_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                ", as.character(rf_post_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
        ), font = 1, cex = 0.8)
        y = y - 0.03
        x = 0.515
        text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
        x = 0.173
        y = y - 0.1
        # in case there are nore than 6 columns
        if (plot_i == 7){
          # make a new plot --> repeats
          plot.new()
          # stat the no. of input file
          text(x = 0.15, y = 1, paste0("Input File ", 1), font = 2, cex = 1)
          # state the input label of the file
          text(x = 0.242, y = 0.95, paste0("Input label: ", file_name), font = 1, cex = 0.8)
          # state the number of columns in the file
          plot_columns = nrow(test_dict)
          text(x = 0.161, y = 0.922, paste0("Number of input columns: ", plot_columns), font = 1, cex = 0.8)
          text(x = 0.357, y = 0.875, paste0("The predicted label (using Multinomial Regression) is: ", predict_name), font = 1, cex = 0.8)

          # inital start for x and y
          x = 0.173
          y = 0.816
          for (plot_i in 7:plot_columns){
            text(x, y, paste0("For column ", plot_i, ": (% confidence)"), font = 1, cex = 0.8)
            x = 0.515
            y = y - 0.04
            text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
            y = y - 0.03
            text(x, y, "1st                          2nd                         3rd", font = 1, cex = 0.8)
            y = y - 0.03
            x = 0.43
            text(x, y, paste0(
              "Dictionary                ", as.character(dict_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(dict_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 2])), digits = 3)*100, ")             ", as.character(dict_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(dict_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
            ), font = 1, cex = 0.8)
            y = y - 0.03
            x = 0.413
            text(x, y, paste0(
              "Target                        ", as.character(rf_target_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 1])), digits = 3)*100, ")                ", as.character(rf_target_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                  ", as.character(rf_target_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_target_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
            ), font = 1, cex = 0.8)
            y = y - 0.03
            x = 0.4
            text(x, y, paste0(
              "Post-processing          ", as.character(rf_post_predict_prob_df[plot_i, 4]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 1])), digits = 3)*100, ")            ", as.character(rf_post_predict_prob_df[plot_i, 5]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 2])), digits = 3)*100, ")                ", as.character(rf_post_predict_prob_df[plot_i, 6]), " (", signif(as.numeric(as.character(rf_post_predict_prob_df[plot_i, 3])), digits = 3)*100, ")"
            ), font = 1, cex = 0.8)
            y = y - 0.03
            x = 0.515
            text(x, y, "---------------------------------------------------------------------------------------------------------------------------------------------------------", font = 1, cex = 0.8)
            x = 0.173
            y = y - 0.1
          }
        }
      }
    }
  }
  dev.off()
}
