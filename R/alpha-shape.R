
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



#' @title alpha_shape
#' @description Compute an alpha Shape Grid using the Q-hull library. 
#' @param simplicies A Delaunay trigulation list object created by 
#' \code{\link{delaunay}} or a alpha complex list object created by 
#' \code{\link{alpha_complex}}.
#' @param alpha_range, range of alpha value
#' @param mins Vector of length \code{n} listing the point space minimum for each dimension.
#'  @param maxs Vector of length \code{n} listing the point space maximum for each  dimension.
#' @param n \code{n} dimension point co-ordinate
#' @details The calculation is done by assigning the trigulation index when the grid cell center lies within the trigulation or -1 if it lies outside 
#' @return grid stack as vector, gridSimplex, and the inputed grid point.
#' @examples 
#' # Define points and create a Delaunay triangulation
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' a_complex <- alpha_complex(points = p, alpha = 20)
#' alpha_shape(simplicies = a_complex,maxs = c(70,80),mins = c(20,20),n = 5,alpha_range = c(1:20))
#' @export alpha_shape
alpha_shape <- function(simplicies,alpha_range,maxs,mins,n) {
  
  tmpdir <- tempdir()
  error= list()
  
  ## check if R tmpdir is writable
  if (file.access(tmpdir, 2) == -1) {
    stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
  }
  
  if(typeof(alpha_range) != "integer"){
    error=append(error,paste0("alpha range must be a list or vector"))
  }else{
    alpha_range=as.list(alpha_range)
  }
  
  if( is.null(n)){
    error=append(error,paste0("n value cant be Null", "\n"))
  }
  
  #retrieve point 
  
  point= simplicies$input_points
  
  
  #check min/mx

  if (length(mins) != length(maxs)) {
    error=append(error,paste0("Length of mins and maxs differ"))
  }
  if (FALSE %in% (mins < maxs)) {
    error=append(error,paste0("Maximums not greater than minimums in all dimensions"))
  }
  

  p_dim =length(point)
  
  #check to make sure the mins/max value specify is the same as the point dimension length
  if(length(mins) != ncol(point)){
    error=append(error,paste0("Length of mins differ from input point dimension length. Please specify mins of length", ncol(point)))
  }
  
  if(length(maxs) != ncol(point)){
    error=append(error,paste0("Length of maxs differ from input point dimension length. Please specify maxs of length", ncol(point)))
  }
  
  
  
  #check to make sure the min/max are within the input point
  p= c(1:length(maxs))
  for(i in p){
    if(mins[i] < min(point[,i])){
      error=append(error,paste0("The min value specify in the position ", i ," of the mins vector is less than the minimum value specify in column ", i , " of the dataframe"))
    }
    
    if(maxs[i] > max(point[,i])){
      error=append(error,paste0("The max value specify in the position ", i ," of the maxs vector is greater than the minimum value specify in column ", i , " of the dataframe"))
    }
  }
  
  
  
  
  if(length(error) ==0){
    
    #get alpha complex

    if(is.vector(alpha_range))
      alpha_range <- as.list(alpha_range) #change alpha range value to list
    
    
    dim = ncol(simplicies$circumcentres) # we can get the number of dimension from the Number of col on the voronoi vertices list
    circum_radii = simplicies$circumradii
    center = simplicies$circumcentres
    colD = dim+1
    
    
    #create mesh grid
    mesh_grid_space = grid_coordinates(mins, maxs, nCoords=n)
    
    
    #Secondly, each point which is inside the convex hull is now check to determine if the center lies in any of the trigulation, and if found the trigulation index is return
    grid_space_simplex <- find_simplex(simplicies,mesh_grid_space)
    
    
    alpha_complex_simplices_list =NULL
  
    empty_grid <- ifelse(grid_space_simplex !=0,0,grid_space_simplex)
    #for each alpha , Identify those simplices that will form the alpha-shape
    for(alpha in alpha_range){	
      alpha_complex_simplices= which(circum_radii<alpha)
      alpha_complex_simplices =alpha_complex_simplices -1
      
      j=1
      sp=NULL
      for(value in grid_space_simplex){
        
        if(! value %in% alpha_complex_simplices)
          sp[j] = 0
        else{
          sp[j]=1
        }
        
        j<-j+1
      }
      empty_grid =empty_grid+sp
      
    }
    #return alpha grid as matrix
    alpha_grid_matrix = matrix(empty_grid ,ncol=n,byrow=TRUE)
    return (list(alphaGrid=alpha_grid_matrix,gridSpace=mesh_grid_space,gridSpaceSimplex =grid_space_simplex))
    
  }else{
    print(error)
  }
  
  
}
