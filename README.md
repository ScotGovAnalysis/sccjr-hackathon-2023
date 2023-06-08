# SCCJR Crime Data Hackathon - Summer 2023
Written by Stuart Napier (<Stuart.Napier@gov.scot>)

## Introduction

This repository will walk you through each step to get you up and running with the data for the SCCJR crime data hackathon

### Prerequisites
Before beginning you will need to have completed a few steps in order to make things run smoothly.
These are:
1. Download git/github
2. Have an account with the [UK data service](https://ukdataservice.ac.uk/)
3. Have R and R Studio downloaded on your machine


### Step 1:
The first step is to clone this repository onto your machine, this will provide you with all of the code required to import the SCJS data and will produce a final dataset that is much more manageable than the ones as they come from the UKDS. 

HTTPS: `git clone https://github.com/stuartnapier-sg/SCCJR_hackathon_2023.git`
SSH: `git clone git@github.com:stuartnapier-sg/SCCJR_hackathon_2023.git`
GitHub CLI: `gh repo clone stuartnapier-sg/SCCJR_hackathon_2023`

### Step 2:
After cloning the repo on to your machine you will need to download the datasets from the UKDS. I would highly recommend downloading them as SPSS files, as this is in my opinion the easiest to work with and is what I have used when writing the other scripts you will see in the repo to make the data more manaageable.

### Step 3 - extracting the data:
The files that you download from the UKDS contain the SPSS datasets as well as a number of other files that are not strictly necessary.
The 'extract_datasets.R' file in the repo can be run to easily move just the datasets from your downloads folder into your project directory.





