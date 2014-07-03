jdc-data-analysis
=================

Data preprocessing, analysis scripts and tools for the Journee des Classes experiment. The project is organized in several R scripts, that go through the different steps of the data analysis process:

* JDCLogPreprocess.R = Pre-processing of logs: from a raw log file to a cleaner R data frame of moments and manipulable positions
* JDCSurveyPreprocess.R = Preprocessing of raw survey data, in csv form
* JDCMapPreprocess.R = Preprocessing of map performance data, from the human video analysis, in csv form
* JDCEyetrackPreprocess.R = Preprocessing of teacher eyetracker data, in txt/csv form
* JDCExploratory.R = Exploratory analysis: First basic analyses of data, to know what we have

# Pre-processing

These files normally have a single user function, that does most of the things provided the folder where the raw data is, e.g.:

```
preprocessJDCLogs(rootDir, doYAMLConversion=FALSE)
```

These functions will generate one or more .rda files with the clean/processed data for each student/group. These data can be easily loaded in R using:

```
var <- get(load('filename.rda'))
```
*Note for the log preprocessing*: This function will assume that you have a directory `rootDir` with subdirectories for each lamp log data, named `lamp 1`, `lamp 2` etc. This `rootDir` will also contain a csv file with each group's beginning and ending times (see `logs/` folder in this project). If the `doYAMLConversion` parameter is set to TRUE, it will try to convert the original pseudo-YAML log files into JSON, a similar format which is more easily read by R (if the parameter is set to FALSE, that means you already did that conversion, and you have .json log files in the lamp folders instead). 

*Another Note for log preprocessing*: The pseudo-YAML files have to be syntactically correct (all braces and parenthesis must be closed, no "half objects" are allowed -- something that happens sometimes in the log file). This is something you will have to check **manually**

*Note on log data*: S4G2.rda denotes session 4, Group/Lamp 2. Each row has a numeric timestamp (milliseconds) and 64 more variables with the quadrant in which the element was placed (as character variables with values 0 -- not present, Q1, Q2, Q3, Q4).The quadrants are defined as in classical geometry

| Q2  | Q1 |
| ------------- | ------------- |
| Q3  | Q4  |

# Exploratory analysis

These functions generate some basic descriptive graphics about the data, just run the function:

```
JDCExplorations(rootDir=".")
```

This function will take the pre-processed data .rda files (each kind in a different folder, e.g., quests, logs, maps...) and generate a number of graphic files.
