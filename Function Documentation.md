# Function Documentation

There are two main function in this R package: 1. `make.table()` and 2. `predict.label()`. Other function are used internally for the main functions.

### 1. make.table()
                            make.table(directory, columns, ExcludedSDBs)

This function convert the input (raw) data into specific format for prediction. Note that this function stand alone can be used for data analysis as it organize the data according to their nucleic acid and then convert them into SDB. 

#### Arguments

`directory` path to the input data

`columns` the columns of the input data that wish to include

`ExcludedSDBs` the sdbs that wish to remove from the input data

<br>

**Example**

```
# original input
> input_file = read.table("~/Desktop/Derda/Data/20171003-87YGqdEV-SS.txt", header = T)
> input_file[1,]
  index mindex Primer Mod Nuc    AA R2F11_RN1RP1 R3F11_RN1RP2
1     0      0     XX  XX  XX Total       354006       323275


# Run the function
> example_df = make.table("~/Desktop/Derda/Data/20171003-87YGqdEV-SS.txt", c(1,2), c("SDB3", "SDB68", "SDB69", "SDB72", "SDB77"))
[1] "Correct input file. Creating table..."
[1] "DONE !"
> example_df[1,]
   sdb Column.1 Column.2          Format                     Name
1 SDB9    21283    20677 Derda_Labelling 20171003-87YGqdEV-SS.txt

```


### 2. predict.label()
                            predict.label(table)

This function predict the label (i.e., Dictionary, Target, and Post-processing) of the input table. The output will be a pdf file containing the prediction and the % confidence respectively. Note that the input table **MUST** be in the specific format (i.e., output of `make.table()`)

#### Arguments
`table` input data(output of `make.table()`)

<br>

**Example**

```
> predict.label(example_df)
[1] "Predicting..."
Done Prediction...
Creating pdf....  (named 'results.pdf' in default)
Warning messages may pop up, but can be safely ignored.
null device 
          1 
# output file is provided as "example_output.pdf"
```

