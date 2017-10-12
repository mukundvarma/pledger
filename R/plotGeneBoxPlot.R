#' Boxplot for gene expression values
#'
#' plotGeneBoxPlot takes as input a counts matrix (or dataframe), a metadata dataframe
#' and the names of two metadata variables to plot a boxplot of expression
#' values grouped along the x axis and with different fill colors to show
#' summarized expression of a gene across multiple conditions
#' 
#' @param counts.mat Counts matrix (or dataframe) containing expression values,
#' columns are samples and rows are genes.
#' @param metadata Metadata dataframe, rows are samples and columns are metadata
#' variables.
#' @param gene Gene whose expression variables need to be plotted (character).
#' @param fill.variable Metadata column name which serves as fill variable for 
#' box plot
#' @param x.variable Metadata column according to which x-axis of plot is arranged
#' @param cols Colors to use for fill
#'
#' @examples
#'
#' plotGeneBoxPlot(counts.mat = counts.tmm, metadata=md, gene="Actb", 
#' fill.variable="Celltype", x.variable=Stimulation")

plotGeneBoxPlot <- function(counts.mat, metadata, gene, fill.variable,
    x.variable, cols = c("#e41a1c", "#377eb8", "#4daf4a",  "#984ea3")) {
    plotdf = cbind(metadata, unname(counts.mat[gene, ] %>% as.numeric))
    plotdf = plotdf[order(plotdf[fill.variable,], ]
    plotdf$sample = rownames(plotdf)

    colnames(plotdf)[ncol(plotdf) - 1] = "expr"
 #   print(plotdf)
    ggplot(plotdf, aes_string(x = x.variable, y = expr, fill = fill.variable)) + 
    geom_boxplot(position = "dodge") + 
    theme_bw() + 
    theme(plot.title = element_text(size = 24)) + 
    scale_fill_manual(values = cols) + 
#scale_fill_manual(values = c("#1b9e77","#d95f02"))+
    labs(title = gene, y = paste(gene, 
        "Expression"), fill = fill.variable, x = x.variable) + 
    theme(strip.text.x = element_text(size = 16, face = "bold"))
}
