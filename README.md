# Honors-Thesis
---


----------
## **File Information** ##
- **segmentsample.R** : takes a list of json files of data from BigQuery and returns a list of size 2 with index 1 being a dataframe that can be inputted into PoTRA and index 2 being the coressponding genelist  

- **testresults.RData** : contains the varaibles to run. 'gboth' is a list returned from segmentsample.R using the json files in the repo and 'results2' is the output of PoTRA using gboth.

- **trypotra.R** : runs various sizes of genes to see which ones break PoTRA. I will add a file containing results when I finish