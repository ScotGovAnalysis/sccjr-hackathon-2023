# SCCJR Crime Data Hackathon - Summer 2023
Written by Stuart Napier (<Stuart.Napier@gov.scot>)

## Introduction

The Scottish Centre for Crime & Justice Research (SCCJR) are organising a crime data hackathon - taking place in late July 2023. This is a three day event bringing together PhD students, post-doctoral researchers and more to work together in teams to produce a research project using crime data.

The projects will be heavily based on data from the Scottish Crime and Justice Survey (SCJS). This is a large scale social survey that asks people about their experiences of and attitudes to a number of different issues relating to crime and justice. In its current form the survey has been running since 2008/09 and produces extensive data on crime and justice. 

Results from each survey are published online on the [UK data service](https://ukdataservice.ac.uk/), which is freely available for anyone to download and explore.

If you have come to this page as part of the SCCJR hackathon, then this repository will walk you through each step to get you up and running with the data in as smooth a way as possible. However, anyone else can make use of the code in this repository and the data available on the UKDS to suit their own needs.


### Prerequisites
Before beginning you will need to have completed a few steps in order to make things run smoothly.
These are:
1. Download git/github
2. Have an account with the [UK data service](https://ukdataservice.ac.uk/)
3. Have R and R Studio downloaded on your machine


### Step 1: Cloning the repository
The first step is to clone this repository onto your machine, this will provide you with all of the code required to import the SCJS data and produce a final dataset that is much more manageable than the ones as they come 'out of the box' from the UKDS. 

If you are comfortable using the command line and are familiar enough with Git/GitHub then you can copy any of these in

* HTTPS: `https://github.com/DataScienceScotland/sccjr-hackathon-2023.git`
* SSH: `git@github.com:DataScienceScotland/sccjr-hackathon-2023.git`
* GitHub CLI: `gh repo clone DataScienceScotland/sccjr-hackathon-2023`

Alternatively you can download the scripts as a .zip file from this page and extract them into your desired location.


### Step 2: Downloading from UKDS
After cloning the repo on to your machine you will need to download the datasets from the UKDS.
 
In order to download them you will already need to already have an account on the platform, which can be done for free by anyone, but your account might need to be authorised by the UKDS before you can do anything.

The datasets first need to be added to a project before being downloadable. You can create a new project, add then add the datasets to your project and then download them.

The SCJS has been running since 2008/09 and has completed 9 survey sweeps in that time, with the most recent being in 2019/20. Not that there was no survey conducted in 2011/12, 2013/14 and 2015/16, so no need to spend time trying to hunt them down on the UKDS! Links to the locations of all datasets are below:

* [2008/09 survey (includes self-completion and victim form data)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6362)
* [2009/10 survey (includes self-completion and victim form data)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6685)
* [2010/11 survey (includes self-completion and victim form data)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7229)
* [2012/13 survey (no self-completion or victim form data available for this year onwards without special license access)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7543)
* [2014/15 survey](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8141)
* [2016/17 survey](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8365)
* [2017/18 survey](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8498)
* [2018/19 survey](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8795)
* [2019/20 survey](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8799)

If you are following along with these instructions then the only option is to download the files as SPSS files as this is how I have constructed the scripts to extract the data. You won't need to have SPSS installed on your machine as the R package 'haven' will be able to read these files. If you have a strong preference for another format then feel free to go down that route, however, note that this will make it much more difficult and time consuming to combine any of the datasets into a 'pooled sample', and many of the variables will not be reformatted to be tidier and more accessible to use.

### Step 3: Running the R scripts
Now that you have access to the code as well as the data files you are now able to begin running the R code to produce the datasets.

If all things go as intended these scripts should be very much 'plug and play' and you will not need to do much at all other than run the scripts and let them do everything for you.

Before proceeding you will need to do a couple of things first.

The first is to make sure you have the required packages installed, as these are essential for running the scripts. These are the 'tidyverse' collection of packages and 'haven'. If you don't have these installed, you should be able to get them by running these lines in your R Studio console, or through the R Studio package installer.

``` r
install.packages("tidyverse")
install.packages("haven")
```

#### Script 1 - extract datasets

Having downloaded the SPSS datasets from the UKDS you will have 9 zip files in your download folder. This script can be used to extract all of the actual datasets themselves into your desired folder for you. You will have to edit two lines of code yourself, lines 5 and 7, to specify where your download folder is, and which folder you want the datasets to go into, but aside from that it should do everything for you!

You may have done this manually already or prefer to do it on your own so don't worry this script isn't essential for the future steps, but might save you some time. 

One important thing to note is that this only extracts the actual datasets. When you download the file from the UKDS it also comes with some other files that might be useful to you, for example the questionnaire scripts, or data dictionaries which outline the link between questionnaire questions and dataset columns.

### Script 2 - check datasets

This will just check that you have all of the required files in your project directory. Again this isn't essential but the datasets as they come from UKDS have difficult to understand names, so you can use this to check you have everything good to go. You may have preferred to rename the datasets something more usable, which means this script won't check things for you but that's okay.

### Script 3 - read datasets

Script 3 reads in the necessary datasets for the next scripts. It does so using the `haven` R package to read the SPSS data. For some reason the 2018/19 and 2019/20 cyber crime datasets come separately to their other main surveys, so this script joins them together using a natural join (so all shared columns appear only once).

One thing to note is that the UKDS datasets also come with the victim form (VF) datasets for the first 3 survey years, but for the hackathon event we decided not to go down the route of using them.

### Scripts 4 - Non-victim form dataset (NVF)
#### Script 4.1 - create full pooled dataset

Individual SCJS datasets are really large, some of them have over a thousand columns. While this can be a great thing to have so much data available, it can be overwhelming too. The first script here aims to join all of the 9 individual surveys together without losing any of the data. In this sense it is the largest possible pooled sample dataset. However, this means it will also be the most difficult to handle.

Originally I wrote this script using dplyr's `bind_rows` function. At the time this would just coerce variables of different data classes together and could produce the full pooled dataset in one (slow) fell swoop. 

This isn't possible anymore and trying to do so will produce errors. The workaround for this is to convert all columns of all datasets to a character variable, and then use readr's `type_convert` function to re-interpolate the columns into its likely data class.

I have not done extensive investigation to determine the success of this workaround, so it would be wise to throw up a bunch of caveat's at this stage. However, this script throws everything including the kitchen sink into one dataset, so if you want to find something then it's in here somewhere!

#### Script 4.2 - create curated pooled dataset

Due to the complexity of the full dataset produced in 4.1, it is necessary to produce something that it much more easier to handle and understand, so 4.2 creates a different dataset that only includes a subset of variables, hopefully all of the ones you might need for your intended analysis.
Credit to Ben Matthews for the `scjs_clean_and_standardize` function that tidies up all of the demographic variables nicely.

This script also produces a summary of the base sizes for each question across the years. This is an easy way to track when a question was removed/introduced from the survey but is also useful in seeing if a variable might be broken across different years. My best effort has gone into making sure that as few variables as possible are broken, but I might not have caught them all.

#### Script 4.2.1 - curated dataset create binary vars
Taking the output from 4.2, this script transforms many of the variables of survey questions into a binary format, which is easier to analyse with. For example, a question might be originally coded as 1,2,3,4 corresponding to strongly agree, somewhat agree, somewhat disagree, strongly diagree. The recoded variable would be 1,1,0,0 meaning it is not simply net: agree or not agree.

### Script 5 - create full sc dataset
The last script employs the same methodology as 4.1 to apply to the self completion datasets. These are only availale for the first 3 years without special license access. There is a reduced need to create a subset of variables for the self-completion, in part due to better data integrity of variables across years, but also due to the fact that the structure of the self-completion section produces much more columns than other parts of the survey. I took out variables relating to a module asking about newspaper reading habits, but if that's of interest feel free to include.

