#scatter plot of whole gene correlation between two cell populations via AverageExpression() output in dataframe format, with extracting ribosomal and mitochondrial genes when labeling by condition


#AE is AE df with column as cell population, row as genes, filled by numeric values
#patterns is pattern of genes not to label, even reaching threshold
#col1 and col2 are two cell population, column name of AE
#gene_label='genename' (don't modify)
#col1threshold and col2threshold are expression value threshold to be labeled

library(ggplot2)

LabelProcess <- function(AE, patterns) {
  genename = rownames(AE)
  a = length(patterns)
  for (pattern in patterns) {
    genename <- sapply(gene, function(x) if (grepl(pattern, x)) '' else x)
  }
  genename <- gsub("[^[:alnum:]]", "_", genename)
  AE$genename = genename
  return(AE)
}
PlotGeneCorrelation <- function(AE, patterns, col1, col2, gene_label, col1threshold, col2threshold) {
    AE = LabelProcess(AE,patterns)
    ggplot(AE, aes_string(col1, col2, label = gene_label)) +
        geom_point() +
        geom_text(data=subset(AE, AE[[col1]] > as.numeric(col1threshold) | AE[[col2]] > as.numeric(col2threshold)),
                  aes_string(col1,col2,label=gene_label))
}
#PlotGeneCorrelation(AE,c("^RPLP[[:digit:]]","^RP[SL][[:digit:]]","^RPSA","^MT-"),'mac_3','w21_3','genename',50,50)
