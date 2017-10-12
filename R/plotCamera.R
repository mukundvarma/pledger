#' Bubble plot to display results of CAMERA test
#'
#' plotGeneBarPlot takes as input a counts matrix (or dataframe), a metadata dataframe
#' and the names of two metadata variables to plot a barplot with error bars of expression
#' values grouped along the x axis and with different fill colors to show
#' summarized expression of a gene across multiple conditions
#' 
#' @param camera.obj Output of edgeR::camera or edgeR::fry test
#' @param max.pvalue P-value cutoff for displaying category labels
#' @param use.fdr (boolean) Whether to use FDR (default) or nominal p-value
#' @param n.label Maximum number of category labels to display
#' @param plot.title Title for plot
#' @param plot.subtitle Subtitle for plot
#' @param do.return Whether to return ggplot object (default: False)
#'
#' @examples
#'
#' plotCamera(camera.obj = camera.trrustdb, max.pvalue = 0.05, use.fdr=T, n.label=20,
#' plot.title="Camera test for transcription factors", 
#' plot.subtitle="LPS-treatment vs nostim", do.return=F)

plotCamera <- function(camera.obj, max.pvalue, use.fdr=T, n.label, 
	plot.title, plot.subtitle, do.return=F){
	plotdf = camera.obj
    plotdf$names = rownames(plotdf)
    plotdf$Sig = plotdf$PValue


    pval.type = "nominal"
    if (use.fdr)
    	plotdf$Sig = plotdf$FDR
    	pval.type = "adjusted"
    	

    n.label= min(plotdf[plotdf$Sig < max.pvalue,] %>% nrow, 40)
    p = ggplot(plotdf) + geom_point(aes(x=NGenes, y=-log10(Sig), color=Direction), 
    	                            alpha=0.7, size = 5) + 
        geom_text_repel(data=plotdf[1:n.label,], aes(x=NGenes,y=-log10(Sig), 
        	            label=names), size = 4,segment.size = 0.1) + 
        scale_x_log10() + theme(legend.position = "top") +
        scale_colour_manual(values = c("#1b9e77","#d95f02","#7570b3")) + 
        theme_bw() + 
        labs(title=plot.title, subtitle=plot.subtitle, 
            x="Term Size", y=paste(expression(-log[10]~PValue~), pval.type))
    if(do.return)
	    return(p)
	else
		plot(p)
}