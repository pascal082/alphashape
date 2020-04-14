
#' @title Computation of n dimension \eqn{\alpha}-shape 
#'
#' @description Implementation in n dimension of the alpha shape using the Q-hull library
#'
#' \tabular{ll}{ Package: \tab alphashape\cr  Date: \tab
#' 2019-03-14\cr License: \tab GPL-2\cr LazyLoad: \tab yes\cr }
#'
#' @name alphashape
#' @docType package
#' @author Pascal Omondiagbe\cr Tom Etherington\cr Maintainers: pascal Omondiagbe <omondiagbep@landcareresearch.co.nz>
#' @references http://www.qhull.org/html/qh-code.htm
#' @keywords package
#' @useDynLib alphashape
NULL



##' @title getAlphaShape
##' @description Compute an alpha Shape Grid using the Q-hull library. 
##' @param point observation as dataframe or matrix
##' @param alphaRange, range of alpha value
##' @param mins Vector of length \code{n} listing the point space minimum for each dimension.
#'  @param maxs Vector of length \code{n} listing the point space maximum for each  dimension.
##' @param n \code{n} dimension point co-ordinate
##' @details The calculation is done by assigning the trigulation index when the grid cell center lies within the trigulation or -1 if it lies outside 
##' @return grid stack as vector, gridSimplex, and the inputted grid point.
##' @examples 
##'  x = c(30,70,20,50,40,70,20)
##'  y = c(35,80,70,50,60,20,30)
##'  p = data.frame(x,y)
##'  aShape(point = p,maxs = c(70,80),mins = c(20,20),n = 5,alphaRange = c(1:20))
#' @export getAlphaShape
getAlphaShape <- function(point,alphaRange,maxs,mins,n) {
  
  tmpdir <- tempdir()
  error= list()
  
  ## check if R tmpdir is writable
  if (file.access(tmpdir, 2) == -1) {
    stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
  }
  
  if(typeof(alphaRange) != "integer"){
    error=append(error,paste0("alpha range must be a list or vector"))
  }else{
    alphaRange=as.list(alphaRange)
  }
  
  if( is.null(n)){
    error=append(error,paste0("n value cant be Null", "\n"))
  }
  
  #check point
  if (!is.data.frame(point) || !is.matrix(point) ){
    error=append(error,paste0("Point must be a dataframe or matrix", "\n"))
  }
  
  #check min/mx
  
  # Check input data
  if (length(mins) != length(maxs)) {
    error=append(error,paste0("Length of mins and maxs differ"))
  }
  if (FALSE %in% (mins < maxs)) {
    error=append(error,paste0("Maximums not greater than minimums in all dimensions"))
  }
  
  
  nPoints= nrow(point)
  pDim =length(point)
  
  #check to make sure the mins/max value specify is the same as the point dimension length
  if(length(mins) != length(point)){
    error=append(error,paste0("Length of mins differ from point dimension length. Please specify mins of length", pDim))
  }
  
  if(length(maxs) != length(point)){
    error=append(error,paste0("Length of maxs differ from point dimension length. Please specify maxs of length", pDim))
  }
  
  #check to make sure we have the required point for the number of dimension specify, hence Qhull will return an error
  nPointRequired = pDim +2
  if(nPoints < nPointRequired){
    error=append(error,paste0("You need at least ", nPointRequired, " point for ", pDim , "  dimension"))
  }
  
  
  
  #check to make sure the min/max are within the point
  p= c(1:length(maxs))
  for(i in p){
    if(mins[i] < min(point[i])){
      error=append(error,paste0("The min value specify in the position ", i ," of the mins vector is less than the minimum value specify in column ", i , " of the dataframe"))
    }
    
    if(maxs[i] > max(point[i])){
      error=append(error,paste0("The max value specify in the position ", i ," of the maxs vector is greater than the minimum value specify in column ", i , " of the dataframe"))
    }
  }
  
  
  
  
  if(length(error) ==0){
    
    #get voronoi object
    voronoiObject = voronoi(point=point)
    
    if(!is.matrix(point)){
      point <- as.matrix(point)
    }
    
    
    if(is.vector(alphaRange))
      alphaRange <- as.list(alphaRange) #change alpha range value to list
    
    
    dim = ncol(voronoiObject$voronoiVertices) # we can get the number of dimension from the Number of col on the voronoi vertices list
    tri = voronoiObject$tri
    circumRadii = voronoiObject$circumRadii
    inputPoint=voronoiObject$inputPoints
    center = voronoiObject$voronoiVertices
    colD = dim+1
    
    
    #create mesh grid
    meshGrdiSpace = grid.coords(mins, maxs, nCoords=n)
    
    
    #Secondly, each point which is inside the convex hull is now check to determine if the center lies in any of the trigulation, and if found the trigulation index is return
    #Secondly, each point which is inside the convex hull is now check to determine if the center lies in any of the trigulation, and if found the trigulation index is return
    gridSpaceSimplex <- findSimplex(tri, inputPoint,meshGrdiSpace)
    
    
    alphaComplexSimplicesList =NULL
    resultList=list()
    emptyGrid <- ifelse(gridSpaceSimplex !=0,0,gridSpaceSimplex)
    #for each alpha , Identify those simplices that will form the alpha-shape
    for(alpha in alphaRange){	
      alphaComplexSimplices= which(circumRadii<alpha)
      alphaComplexSimplices =alphaComplexSimplices -1
      
      j=1
      sp=NULL
      for(value in gridSpaceSimplex){
        
        if(! value %in% alphaComplexSimplices)
          sp[j] = 0
        else{
          sp[j]=1
        }
        
        j<-j+1
      }
      emptyGrid =emptyGrid+sp
      
    }
    #return alpha grid as matrix
    alphaGridMatrix = matrix(emptyGrid ,ncol=n,byrow=TRUE)
    return (list(alphaGrid=alphaGridMatrix,gridSpace=meshGrdiSpace,gridSpaceSimplex =gridSpaceSimplex))
    
  }else{
    print(error)
  }
  
  
}


#Internal function use by \code{\link{findSimplex}} to calculate the bycentri co-ordinate
getBycentriCoordinate <- function(X, P) {
  M <- nrow(P)
  N <- ncol(P)
  if (ncol(X) != N) {
    stop("Simplex X must have same number of columns as point matrix P")
  }
  if (nrow(X) != (N+1)) {
    stop("Simplex X must have N columns and N+1 rows")
  }
  X1 <- X[1:N,] - (matrix(1,N,1) %*% X[N+1,,drop=FALSE])
  if (rcond(X1) < .Machine$double.eps) {
    warning("Degenerate simplex")
    return(NULL)
  }
  Beta <- (P - matrix(X[N+1,], M, N, byrow=TRUE)) %*% solve(X1)
  Beta <- cbind(Beta, 1 - apply(Beta, 1, sum))
  return(Beta)
}
