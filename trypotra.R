tryf <- function(mydata,genelist)
{

	dbell <- FALSE
	disp <- 1000
	cnum <- 0
	for(i in 1000:2000)
	{
	  res <- try(results <-PoTRA(mydata=head(mydata, i),genelist= head(genelist, i),Num.sample.normal=8,Num.sample.case=8,Pathway.database=humanKEGG[1:10],PR.quantile=0.95))
	  if(inherits(res, "try-error"))
	  {
	    #error handling code, maybe just skip this iteration using
	    #continue
	    if (dbell)
	    {
	    	if (i - disp > 0)
	    	{
	    		print("Broke")
	    		print(i)
	    		cnum <- cnum + 1
	    	}

	    }
	  }
	  else
	  {
	  	dbell <- TRUE 
	  	disp <- i 
	  }
	  #rest of iteration for case of no error
	  if(i %% 50 == 0)
	  {
		print("Counter")
		print(i)
	  }
	}
	print("NumDiscp")
	print(cnum)
}
