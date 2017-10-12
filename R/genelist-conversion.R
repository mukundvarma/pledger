ENTREZ =  toTable(org.Hs.egSYMBOL)
head(ENTREZ)
getToptagsWithEntrez <- function(lrt) {
    lrt$table$gene = rownames(lrt$table)
    match_id = match(lrt$table$gene, ENTREZ$symbol)
    lrt$table$Entrez = ENTREZ$gene_id[match_id]
    return(lrt)
    }

getEntrezFromGeneSymbol<- function(genelist){
    match_id = match(genelist, ENTREZ$symbol)
    return(ENTREZ$gene_id[match_id])
}


getGeneSymbolFromEntrez <- function(entrezlist){
    match_id = match(entrezlist, ENTREZ$gene_id)
    return(ENTREZ$symbol[match_id])
}