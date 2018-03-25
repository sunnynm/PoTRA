# df is the default table
loopData <- function(patientdf, project) {
    tsample <- vector("list", 2)

    seperatesample <- function(df, icol)
    {
        fgene <- vector()
        #sgene <- vector()
        fnum <- vector()
        #snum <- vector()
        fstring <- NULL
        #sstring <- NULL
        print(ncol(df))
        for (index in 1:nrow(df))
        {
            barcode <- df$"sample_barcode"[index]
            testtype <- substr(barcode, 14, 15)
            testtype <- as.numeric(as.character(testtype))
            if (testtype < 10 && icol == 2)
            {
                fstring <- df$"sample_barcode"[index]
                fgene <- c(fgene, df$"gene_id"[index])
                fnum <- c(fnum, df$"normalized_count"[index])
            }
            else if (testtype >= 10 && icol == 1)
            {
                fstring <- df$"sample_barcode"[index]
                fgene <- c(fgene, df$"gene_id"[index])
                fnum <- c(fnum, df$"normalized_count"[index])    
            }

             
        }
        alpha <- list(fstring, fgene, fnum)
        dflist <- vector("list", 1)
        for (index in 1:1)
        {
            df2 <- data.frame(alpha[[(index-1) * 3 + 3]])
            colnames(df2) <- c(alpha[[(index-1) * 3 + 1]])
            row.names(df2) <- alpha[[(index-1) * 3 + 2]]
            #print(df2)
            dflist[[index]] <-  df2
        }
        #print(dflist)
        return(dflist) 
        
    }
    

    for (icol in 1:ncol(patientdf))
    {
        sample <- vector("list", nrow(patientdf))
        for (irow in 1:nrow(patientdf))
        {
            pID <- patientdf[irow, icol]
            print(pID)
            sql <- paste0("SELECT case_barcode, sample_barcode, gene_id, normalized_count ", 
               "FROM [isb-cgc:TCGA_hg19_data_v0.RNAseq_Gene_Expression_UNC_RSEM] ",
               "WHERE project_short_name = 'TCGA-LIHC' AND  case_barcode = '",
               pID,
               "' AND gene_id IN (SELECT * ",
               "FROM [shining-axon-181607:GeneTable.genelist])",
                " ORDER BY gene_id")
            #print (sql)
            dfinput <- query_exec(sql, project = project)
            holder <- seperatesample(dfinput, icol)
            sample[[irow]] <- holder[[1]]
            #print(sample) 
        }
        tsample[[icol]] <- sample 
    }
    #print(tsample[[1]]) 
    #print(tsample[[2]])
    enddflist <- do.call(c, list(tsample[[1]], tsample[[2]]))
    #print(enddflist)
    df <- (Reduce(merge, lapply(enddflist, function(x) data.frame(x, rn = row.names(x)))))
    df$rn <- as.numeric(as.character(df$rn))
    df <- (df[order(df$rn),])
    genelist <- df[, "rn", drop = FALSE]
    row.names(df) <- df$rn
    df <- within(df, rm("rn"))
    genelist <- within(genelist, rm("rn"))
    return(list(df, genelist))

}

