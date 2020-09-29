# Function Documentation

There are two main function in this R package: 1. `make.table()` and 2. `predict.label()`. Other function are used internally for the main functions.

### 1. make.table
                            make.table(directory, columns, ExcludedSDBs)

This function convert the input (raw) data into specific format for prediction. Note that this function stand alone can be used for data analysis as it organize the data according to their nucleic acid and then convert them into SDB. 

`directory` path to the input data

`columns` the columns of the input data that wish to include

`ExcludedSDBs` the sdbs that wish to remove from the input data

**Example**
`example_df = make.table("~/Desktop/Derda/Data/20171003-87YGqdEV-SS.txt", c(1,2), c("SDB3", "SDB68", "SDB69", "SDB72", "SDB77"))`


### 2. predict.label
                            predict.label(table)

This function predict the label (i.e., Dictionary, Target, and Post-processing) of the input table. The output will contain the prediction and the % confidence respectively. Note that the input table **MUST** be in the specific format (i.e., output of `make.table()`)

`table` input data(output of `make.table()`

**Example**
`prediction = predict.label(example_df)`

