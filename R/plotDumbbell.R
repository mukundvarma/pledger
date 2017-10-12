#' Dumbbell plot for comparing two logFCs
#'
#' plotDumbbell takes as input a list of genes and two tables produced by edgeR
#' to visualize the difference in fold change between the two comparisons. This 
#' is particularly useful for displaying fold changes in a comparison from an 
#' interaction term where differences across two factors are being compared.
#'
#'
#' @param gene.list List of genes to plot
#' @param table.1 EdgeR table (topTags output) for the first comparison
#' @param table.2 EdgeR table (topTags output) for the second comparison
#' @param label.1 Label for the first comparison
#' @param label.2 Label for the second comparison
#' @param plot.title Title for the plot
#' @param plot.subtitle Subtitle for the plot
#' @param do.return Whether to return a ggplot object
#'
#' @importFrom dplyr %>%
#' @importFrom ggalt geom_dumbbell
#'
#' @examples
#' WT_table = edgeR::topTags(wt.lrt) %>% .$table
#' KO_table = edgeR::topTags(ko.lrt) %>% .$table
#' plotDumbBell(gene.list = c("Actb", "Gapdh", "Acta2"), table.1 = WT_table, table.2 = KO_table,
#'              label.1 = "WT", label.2="KO", plot.title = "Comparison of stimulation/nostim logFC",
#'              plot.subtitle="between WT and KO mice")

plotDumbbell <- function(gene.list, table.1, table.2, label.1, label.2, 
                         plot.title, plot.subtitle, do.return=F){

#   Create dataframe to be used or plotting
    plotdf = data.frame(matrix(nrow=gene.list%>% length, ncol=5))
    colnames(plotdf) = c("gene", "group.1.logFC", "group.2.logFC",
                         "group.1.FDR", "group.2.FDR")
    rownames(plotdf) = gene.list
    plotdf$gene = rownames(plotdf)
    plotdf$group.1.logFC = table.1[rownames(plotdf),"logFC"]
    plotdf$group.2.logFC = table.2[rownames(plotdf),"logFC"]
    plotdf$group.1.FDR = table.1[rownames(plotdf),"FDR"]
    plotdf$group.2.FDR = table.2[rownames(plotdf),"FDR"]
    
    plotdf = plotdf[order(-plotdf$group.1.logFC),]
    plotdf$gene = factor(plotdf$gene, levels = rev(plotdf$gene))
    plotdf$direction = ifelse(plotdf$group.1.logFC > plotdf$group.2.logFC,
                             "Down", "Up")
   
    label.1.x = plotdf$group.1.logFC[1]
    label.2.x = plotdf$group.2.logFC[1]
    topgene = plotdf$gene[1]
    
    gg <- ggplot(plotdf, aes(x=group.1.logFC, xend=group.2.logFC, 
                             y=gene, group=gene)) + 
    geom_dumbbell(colour_x="#a3c4dc", 
                         colour="#0e668b",
                         dot_guide=T,
                         dot_guide_size = 0.3,
                         size_x=5, 
                         size_xend=5,
                         colour_xend="#0e668b",
                         show.legend=T) + 
    scale_x_continuous() + 
    scale_color_manual(name = "", values = c("#a3c4dc", "#0e668b") ) + 
#    geom_text(aes(x=group.1.logFC, y=gene, label=round(group.1.FDR,3)),
#                 color="#a3c4dc", hjust=1, size=3, nudge_x=0.5*x1_val) +
#    geom_text(aes(x=group.2.logFC, y=gene, label=round(group.2.FDR,3)),
#               color="#0e668b", hjust=0, size=3, nudge_x=0.5*x2_val)+
    geom_text(aes(x=label.1.x, y=topgene, label="WT"),
              color="#a3c4dc", 
              hjust=0, 
              size=6, 
              nudge_y=-0.5) +
    geom_text(aes(x=label.2.x, y= topgene, label="KO"),
              color="#0e668b", 
              hjust=1, 
              size=6, 
              nudge_y=-0.5) +
    labs(x="logFC", 
         y="gene", 
         title=plot.title,
         subtitle=plot.subtitle) +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=20),
          plot.subtitle=element_text(hjust=0.5, size=12),
#          plot.background=element_rect(fill="#f7f7f7"),
#          panel.background=element_rect(fill="#f7f7f7"),
          panel.grid.minor=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_line(),
          axis.ticks=element_blank(),
#              legend.position="top",
          panel.border=element_blank(), 
          axis.text = element_text(size=30),
          axis.title = element_text(size=30) )
if (do.return)
    return(gg)
else plot(gg)

}