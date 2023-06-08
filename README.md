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

I would highly recommend downloading them as SPSS files, as this is in my opinion the easiest to work with and is what I have used when writing the other scripts you will see in the repo to make the data more manageable.

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

### Step 3 - extracting the data:
The files that you download from the UKDS contain the SPSS datasets as well as a number of other files that are not strictly necessary.
The 'extract_datasets.R' file in the repo can be run to easily move just the datasets from your downloads folder into your project directory.


