#' @name plotHeatmapSRD
#' @title plotHeatmapSRD
#' @aliases plotHeatmapSRD
#' @author Attila Gere \email{gereattilaphd@@gmail.com}, Linus Olsson \email{linusmeol@@gmail.com}
#' @description Heatmap is generated based on the pairwise distance - measured in SRD - of the columns. 
#' Each column is set as reference once, then SRD values are calculated for the other columns. 
#' @param df A DataFrame.
#' @return Returns a heatmap.
#' @export plotHeatmapSRD
#' @examples 
#' srdInput <- data.frame(
#' A=c(32, 52, 44, 44, 47),
#' B=c(73, 75, 65, 76, 70),
#' C=c(60, 59, 57, 55, 60),
#' D=c(35, 24, 44, 83, 47),
#' E=c(41, 52, 46, 50, 65))
#' 
#' plotHeatmapSRD(srdInput)
plotHeatmapSRD <- function (df){
cnames<-colnames(df)
SRDs<-matrix(ncol=length(cnames)-1, nrow=length(cnames))
for(i in 1:length(cnames)) { 
  SRDdf2<-utilsDetailedSRDNoChars(df,referenceCol=cnames[i])
  SRDs[i,]<-as.matrix(SRDdf2[nrow(SRDdf2), !apply(is.na(SRDdf2), 2, any)])
  }
rownames(SRDs)<-colnames(df)
SRDs<-t(SRDs)
hmap<-heatmap(round(cor(SRDs),2), symm=TRUE, verbose = FALSE)
invisible(hmap)
}

