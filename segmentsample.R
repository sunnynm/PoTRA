# df is the default table
Createdf <- function(samplelist) {
    tumorsample <- vector("list", length(samplelist))
    normalsample <- vector("list", length(samplelist))

    seperatesample <- function(df)
    {
        fgene <- vector()
        sgene <- vector()
        fnum <- vector()
        snum <- vector()
        fstring <- NULL
        sstring <- NULL
        print(ncol(df))
        for (index in 1:nrow(df))
        {
            barcode <- df$"sample_barcode"[index]
            testtype <- substr(barcode, 14, 15)
            testtype <- as.numeric(as.character(testtype))
            if (testtype < 10)
            {
                fstring <- df$"sample_barcode"[index]
                fgene <- c(fgene, df$"gene_id"[index])
                fnum <- c(fnum, df$"normalized_count"[index])
            }
            else
            {
                sstring <- df$"sample_barcode"[index]
                sgene <- c(sgene, df$"gene_id"[index])
                snum <- c(snum, df$"normalized_count"[index])      
            }

             
        }
        alpha <- list(fstring, fgene, fnum, sstring, sgene, snum)
        dflist <- vector("list", 2)
        for (index in 1:2)
        {
            df2 <- data.frame(alpha[[(index-1) * 3 + 3]])
            colnames(df2) <- c(alpha[[(index-1) * 3 + 1]])
            row.names(df2) <- alpha[[(index-1) * 3 + 2]]
            print(df2)
            dflist[[index]] <-  df2
        }

        return(dflist) 
        
    }

    for (index in 1:length(samplelist))
    {
        dfinput <- jsonlite::stream_in(file(samplelist[[index]]))
        holder <- seperatesample(dfinput)
        tumorsample[[index]] <- holder[[1]]
        normalsample[[index]] <- holder[[2]]
    }
    enddflist <- do.call(c, list(normalsample, tumorsample))
    df <- (Reduce(merge, lapply(enddflist, function(x) data.frame(x, rn = row.names(x)))))
    df$rn <- as.numeric(as.character(df$rn))
    df <- (df[order(df$rn),])
    genelist <- df[, "rn", drop = FALSE]
    row.names(df) <- df$rn
    df <- within(df, rm("rn"))
    return(list(df, genelist))
}

