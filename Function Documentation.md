# Function Documentation

There are two main function in this R package: 1. `make.table()` and 2. `predict.label()`. Other function are used internally for the main functions.

### make.table(directory, columns, ExcludedSDBs)
                            make.table = function(directory, columns, ExcludedSDBs)

This function convert the input (raw) data into specific format for prediction. Note that this function stand alone can be used for data analysis as it organize the data according to their nucleic acid and then convert them into SDB. 

`directory` path to the input data
`columns` the columns of the input data that wish to include
`ExcludedSDBs` the sdbs that wish to remove from the input data


