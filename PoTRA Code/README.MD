**PoTRA.R**

The R script to perform the main part (Fisher’s exact test) of PoTRA, the alternative part (K.S. test) of PoTRA, and the overlay plot of kernel density for the PR scores.

**PoTRA-example.R**

Example usage of PoTRA.R

**PoTRA-example-data.Rdata**

Data used in PoTRA-example.R

 

**Usage**

For PoTRA (Fisher’s exact test and K.S. test) based on correlation networks:

PoTRA.cor <- function(mydata,genelist,Num.sample.normal,Num.sample.case,Pathway.database, PR.quantile)

For PoTRA (Fisher’s exact test and K.S. test) based on combined networks:

PoTRA.comb <- function(mydata,genelist,Num.sample.normal,Num.sample.case,Pathway.database)

For the kernel density plot:

results.plot <- overlayplot(mydata,genelist,Num.sample.normal,Num.sample.case,Pathway.database)

 

**Arguments**

mydata:	a gene expression dataset (matrix). Rows represent genes, and columns represent samples (from control to case)
genelist:	a list of gene names
Num.sample.normal:	the number of normal samples
Num.sample.case:	the number of cancer samples
Pathway.database:	the pathway database used in the study, such as, KEGG, Biocarta and Reactome
PR.quantile:	The percentile of PageRank scores as a cutoff for hub genes
Value

 

Fishertest.p.value: the p-value of the Fisher’s exact test.

KStest.p.value: the p-value of the K.S. test.

LengthOfPathway: the length of pathways.

TheNumberOfHubGenes.normal: the number of hub genes for normal samples.

TheNumOfHubGene.case: the number of hub genes for cancer samples.

TheNumberOfEdges.normal: the number of edges in the network for normal samples.

TheNumberOfEdges.case: the number of edges in the network for cancer samples.


**Examples**

See PoTRA-example.R

 

**Credits**

Authors: Chaoxing Li, Li Liu and Valentin Dinu

 

If you use or modify the code, please cite:

“Pathways of Topological Rank Analysis (PoTRA): A Novel Method to Detect Pathways Involved in Cancer”, 2018.

 

**Issues?**

Please email Chaoxing Li <chaoxing@asu.edu> or Valentin Dinu <Valentin.Dinu@asu.edu> if you have any questions, concerns or feedback.