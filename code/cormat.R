#+++++++++++++++++++++++++
# Computing of correlation matrix
#+++++++++++++++++++++++++
# Required package : corrplot
# x : matrix
# type: possible values are "lower" (default), "upper", "full" or "flatten";
#display lower or upper triangular of the matrix, full  or flatten matrix.
# graph : if TRUE, a correlogram or heatmap is plotted
# graphType : possible values are "correlogram" or "heatmap"
# col: colors to use for the correlogram
# ... : Further arguments to be passed to cor or cor.test function
# Result is a list including the following components :
# r : correlation matrix, p :  p-values
# sym : Symbolic number coding of the correlation matrix
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
    library(corrplot)
    # Helper functions
    #+++++++++++++++++
    # Compute the matrix of correlation p-values
    cor.pmat <- function(x, ...) {
        mat <- as.matrix(x)
        n <- ncol(mat)
        p.mat<- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                tmp <- cor.test(mat[, i], mat[, j], ...)
                p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
    }
    # Get lower triangle of the matrix
    getLower.tri<-function(mat){
        upper<-mat
        upper[upper.tri(mat)]<-""
        mat<-as.data.frame(upper)
        mat
    }
    # Get upper triangle of the matrix
    getUpper.tri<-function(mat){
        lt<-mat
        lt[lower.tri(mat)]<-""
        mat<-as.data.frame(lt)
        mat
    }
    # Get flatten matrix
    flattenCorrMatrix <- function(cormat, pmat) {
        ut <- upper.tri(cormat)
        data.frame(
            row = rownames(cormat)[row(cormat)[ut]],
            column = rownames(cormat)[col(cormat)[ut]],
            cor  =(cormat)[ut],
            p = pmat[ut]
        )
    }
    # Define color
    if (is.null(col)) {
        col <- colorRampPalette(
            c("#67001F", "#B2182B", "#D6604D", "#F4A582",
              "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
              "#4393C3", "#2166AC", "#053061"))(200)
        col<-rev(col)
    }
    
    # Correlation matrix
    cormat<-signif(cor(x, use = "complete.obs", ...),2)
    pmat<-signif(cor.pmat(x, ...),2)
    # Reorder correlation matrix
    ord<-corrMatOrder(cormat, order="hclust")
    cormat<-cormat[ord, ord]
    pmat<-pmat[ord, ord]
    # Replace correlation coeff by symbols
    sym<-symnum(cormat, abbr.colnames=FALSE)
    # Correlogram
    if(graph & graphType[1]=="correlogram"){
        corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
                 tl.col="black", tl.srt=45,col=col,...)
    }
    else if(graphType[1]=="heatmap")
        heatmap(cormat, col=col, symm=TRUE)
    # Get lower/upper triangle
    if(type[1]=="lower"){
        cormat<-getLower.tri(cormat)
        pmat<-getLower.tri(pmat)
    }
    else if(type[1]=="upper"){
        cormat<-getUpper.tri(cormat)
        pmat<-getUpper.tri(pmat)
        sym=t(sym)
    }
    else if(type[1]=="flatten"){
        cormat<-flattenCorrMatrix(cormat, pmat)
        pmat=NULL
        sym=NULL
    }
    list(r=cormat, p=pmat, sym=sym)
}