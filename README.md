jdc-data-analysis
=================

Data preprocessing, analysis scripts and tools for the Journee des Classes experiment. The project is organized in several R scripts, that go through the different steps of the data analysis process:

* JDCLogPreprocess.R = Pre-processing: from a raw log file to a cleaner R data frame of moments and manipulable positions
* JDCExploratory.R = Exploratory analysis: First basic analyses of data, to know what we have

# Pre-processing

This stage has one main function that does most of the things:

```
preprocessJDCLogs(rootDir, doYAMLConversion=FALSE)
```

This function will assume that you have a directory `rootDir` with subdirectories for each lamp log data, named `lamp 1`, `lamp 2` etc. This `rootDir` will also contain a csv file with each group's beginning and ending times (see `logs/` folder in this project). If the `doYAMLConversion` parameter is set to TRUE, it will try to convert the original pseudo-YAML log files into JSON, a similar format which is more easily read by R (if the parameter is set to FALSE, that means you already did that conversion, and you have .json log files in the lamp folders instead). 

The function will generate a series of .rda files with the log data for each student group in each session, e.g., S4G2 denotes session 4, Group/Lamp 2. Each row has a numeric timestamp (milliseconds) and 64 more variables with the quadrant in which the element was placed (as character variables with values 0 -- not present, Q1, Q2, Q3, Q4). These data can be easily loaded in R using:

```
var <- get(load('filename.rda'))
```

*Important note*: The pseudo-YAML files have to be syntactically correct (all braces and parenthesis must be closed, no "half objects" are allowed -- something that happens sometimes in the log file). This is something you will have to check **manually**

*Note*: The quadrants are defined as in classical geometry

| Q2  | Q1 |
| ------------- | ------------- |
| Q3  | Q4  |

# Exploratory analysis

TBD
