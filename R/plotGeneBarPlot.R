#' Barplot for gene expression values
#'
#' plotGeneBarPlot takes as input a counts matrix (or dataframe), a metadata dataframe
#' and the names of two metadata variables to plot a barplot with error bars of expression
#' values grouped along the x axis and with different fill colors to show
#' summarized expression of a gene across multiple conditions
#' 
#' @param counts.mat Counts matrix (or dataframe) containing expression values,
#' columns are samples and rows are genes.
#' @param metadata Metadata dataframe, rows are samples and columns are metadata
#' variables.
#' @param gene Gene whose expression variables need to be plotted (character).
#' @param fill.variable Metadata column name which serves as fill variable for 
#' barplot
#' @param x.variable Metadata column according to which x-axis of plot is arranged
#' @param cols Colors to use for fill
#'
#' @examples
#'
#' plotGeneBarPlot(counts.mat = counts.tmm, metadata=md, gene="Actb", 
#' fill.variable="Celltype", x.variable=Stimulation")

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

plotGeneBoxPlot <- function(counts.mat, metadata, gene, fill.variable,
    x.variable, cols = c("#e41a1c", "#377eb8", "#4daf4a",  "#984ea3")) {
    pdata = cbind(metadata, unname(counts.mat[gene, ] %>% as.numeric))
    pdata = pdata[order(pdata[fill.variable,], ]
    pdata$sample = rownames(pdata)

    colnames(pdata)[ncol(pdata) - 1] = "expr"

    plotdf = summarySE(pdata, measurevar = "expr", groupvars = c(fill.variable, x.variable))

 #   print(pdata)
    ggplot(plotdf, aes_string(x = x.variable, y = expr, fill = fill.variable)) + 
    geom_col(position = "dodge") + 
    geom_errorbar(aes(ymin=expr-se, ymax=expr+se),position=position_dodge(0.9), width=0.2) +
    theme_bw() + 
    theme(plot.title = element_text(size = 24)) + 
    scale_fill_manual(values = cols) + 
    labs(title = gene, y = paste(gene, 
        "Expression"), fill = fill.variable, x = x.variable) + 
    theme(strip.text.x = element_text(size = 16, face = "bold"))


}
