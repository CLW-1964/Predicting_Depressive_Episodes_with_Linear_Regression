# Data wrangling overview

## Sourcing the data

Data for this report was taken from the [Journal of Open Psychology Data](https://openpsychologydata.metajnl.com/articles/10.5334/jopd.29/) website.
Contained in the zip file were a csv file of the raw data and a pdf for column heading definitions.

## Understanding the data

The data file contained the answers to 75 questions the subject was asked 
to answer at different intervals. 

* Several times a day
* Once a day
  + Morning
  + Evening
* Once a week

## Checking the data

In general the dataset was relatively clean. The date variable was imported as a character value and was converted to the date format.

For the questions that were asked multiple times a day the few NA's were replaced with mean values. A few variables that were imported as integers, which will more likely than not be coded as factors or ordered at some point, had their NA's replaced with the most frequently occurring value. 

There are a group of questions that were asked multiple times a day that are only answered if the answer to a previous question met a certain criteria. These NA's were left alone with the understanding that in any future calculations na.rm = TRUE will have to be used in calculations.

The once a day (morning and evening) and once a week (depression index) questions all pose a problem with respect to the handling of NA's. All three of these batteries are only answered once, but their values are filled in, or repeated, for every instance that a question is answered that day. Whether or not to fill, fill forward or fill backward, will be addressed later on in data discovery.

The detailed record of this data processing can be found
[here](Data_Wrangling_-_Working.md).

## Creating working data set

For subsequent analysis, a new data file was created and can be found [here](ESMdata_clean.csv)
analysis. 
