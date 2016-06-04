
# Load Dependencies -------------------------------------------------------
require(ggplot2)


# split.agg(cols, facets) -------------------------------------------------


split.agg <- function(data, facet = NULL){

    if(!is.null(facet) & (facet %in% names(data))){
        data2 <- data[, !names(data) %in% facet]
    } else{
        data2 <- data
    }
    split.df <- function(x, facet){
        shdw.list <- data.frame()        ## to hold data in loop
        shdw.list <- data[,c(x, facet)]
        colnames(shdw.list) <- c("Variable", "Facet")
        return(shdw.list)
    }

    agg.list <- lapply(names(data2), split.df, facet = facet)

    names(agg.list) <- names(data2)              ## name it
    return(agg.list)
}


### create the function to loop through in mapply()
dist.plot <- function(.data, x.name, facet.exists = TRUE){
    ### check to see if a facet exists in the data frame
    if(!facet.exists){
        Facet = NULL
    }
    ### begin writing the plot
    ggplot(data = .data, mapping = aes(x = Variable, fill = Facet)) +

        xlab(x.name) +

        geom_histogram(aes(y = ..density..), bins =  50, alpha = .5) +
        geom_density(aes(color = Facet), alpha = .1) +

        party.colors.1 +
        party.colors.2 +

        facet_grid(. ~ Facet) +
        geom_vline(data = .data,
                   aes(xintercept = mean(Variable),linetype = "dashed"),
                   size = 1) +
        geom_vline(data = .data,
                   aes(xintercept = median(Variable), linetype="solid"),
                   size = 1) +
        scale_linetype_identity(guide="legend", label = c("Mean", "Median"))
}
