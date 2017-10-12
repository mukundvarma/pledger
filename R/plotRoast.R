plotRoast = function (roast.obj, compname, test)  {
    plotdf = roast.obj
    plotdf$names = rownames(plotdf)
    plotdf$Sig = plotdf$PValue

    pval.type = "nominal"
    if (use.fdr)
        plotdf$Sig = plotdf$FDR
        pval.type = "adjusted"
    
    plotdf$prop.diff = ifelse(plotdf$Direction=="Up", plotdf$PropUp, -plotdf$PropDown)
    plotdf$n.diff = plotdf$NGenes*plotdf$prop.diff

    n.label= min(plotdf[plotdf$Sig < max.pvalue,] %>% nrow, 40)

    p = ggplot(plotdf) + 
        geom_point(aes(x = n.diff, y = Sig, color = Direction), alpha = 0.7, size = prop.diff) + 
        geom_text_repel(data = plotdf[1:n.label,], aes(x = n.diff, y = Sig, label = names), size = 3) + 
        #scale_x_log10() + 
        scale_y_log10() + scale_colour_manual(values = c("#1b9e77", 
        "#d95f02", "#7570b3")) + theme_bw() + labs(title = paste(test, 
        "gene set test for transcription factor targets"), subtitle = compname, 
        x = "Proportion of genes", y = "Sig adjusted p-value")
    plot(p)
}
