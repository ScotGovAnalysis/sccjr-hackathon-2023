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


### Step 1:
The first step is to clone this repository onto your machine, this will provide you with all of the code required to import the SCJS data and produce a final dataset that is much more manageable than the ones as they come 'out of the box' from the UKDS. 

If you are comfortable using the command line and are familiar enough with Git/GitHub then you can copy any of these in
HTTPS: `https://github.com/DataScienceScotland/sccjr-hackathon-2023.git`
SSH: `git@github.com:DataScienceScotland/sccjr-hackathon-2023.git`
GitHub CLI: `gh repo clone DataScienceScotland/sccjr-hackathon-2023`

### Step 2:
After cloning the repo on to your machine you will need to download the datasets from the UKDS. I would highly recommend downloading them as SPSS files, as this is in my opinion the easiest to work with and is what I have used when writing the other scripts you will see in the repo to make the data more manaageable.

### Step 3 - extracting the data:
The files that you download from the UKDS contain the SPSS datasets as well as a number of other files that are not strictly necessary.
The 'extract_datasets.R' file in the repo can be run to easily move just the datasets from your downloads folder into your project directory.





