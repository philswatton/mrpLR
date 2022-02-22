# mrpLR

This repository contains the code for my blog post on MRP, which can be found [at this link](). I've included in this repository the data after initially recoding the variables used but not the original data files. To find the original data files, you can find links of where to download the raw data. I've mainly avoided sharing these original files because I have no idea what my rights are here but I'm pretty sure I'm not allowed to share them! I've also included a brief explanation of the contents of this repository which should make it easier to navigate.

## Raw Data

- BES wave 21, find on the BES website [at this link](https://www.britishelectionstudy.com/data-objects/panel-study-data/)
- BES constituency results file, find on the BES website [at this link](https://www.britishelectionstudy.com/data-objects/linked-data/)
- 2011 census data, find on the UK Data Service website [at this link](https://ukdataservice.ac.uk/)
- I also used the [parlitools](https://cran.r-project.org/web/packages/parlitools/vignettes/introduction.html) R package, which contains data used to build the Hex Map and GSS codes

## Scripts

- Script 01 recodes variables from all three data sources to match one another's coding
- Script 02 performs Aldrich-McKelvey scaling using my [psmisc](https://github.com/philswatton/psmisc) package
- Script 03 rakes the census data to produce a population frame. The associated shell script runs it on Essex's HPC
- Script 04 runs the multilevel model using lme4 and produes the poststratified constituency results
- Script 05 generates the results seen in the blog post


## Transformed Data

- bes.rds is the transformed BES data built in script 01
- bes2.rds is the transformed BES data with ideal points from the Aldrich-McKelvey model
- marginals.rds is the transformed dataset containing marginal distibutions
- extras.rds is the transformed dataset containing constituency variables for the models
- frame.rds is the constructed population frame

## Results

- model.rds is the outputted lme4 model
- pred.rds is the dataset containing left-right scores for the population frame
- post.rds contains left-right scores for each constituency
- results.rds and results.csv contain constituency names, left-right scores, gss codes, and 2019 winners
- map.html and map_files are the files building the hex map presented in the blog

