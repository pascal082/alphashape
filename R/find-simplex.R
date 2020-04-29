#' @title Find simplex
#' 
#' @description Returns the simplicies of a Delaunay triangulation or alpha 
#' complex that contain the given set of test points.
#' 
#' @param simplicies A Delaunay trigulation list object created by 
#' \code{\link{delaunay}} or a alpha complex list object created by 
#' \code{\link{alpha_complex}}.
#' @param test_points a \eqn{n}-by-\eqn{d} dataframe or matrix. The rows
#'   represent \eqn{n} points and the \eqn{d} columns the coordinates in 
#'   \eqn{d}-dimensional space. 
#' 
#' @return A \eqn{n} length vector containing the index of the simplex the test 
#' point is within, or a value of zero if a test point is not within any of the 
#' simplicies.
#' 
#' @examples 
#' # Define points and create a Delaunay triangulation
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' dt <- delaunay(points = p)
#' # Check which simplex the test points belong to
#' p_test <- data.frame(c(20, 50, 60), c(20, 50, 60))
#' simplex <- find_simplex(simplicies = dt, test_points = p_test)
#'  
#' @export
find_simplex <- function(tri,inputPoint,testPoint) {
  
  if(!is.data.frame(testPoint)){
    stop("please supply a test point dataframe")
  }
 
  #Identify the simplicies that each grid point belongs 
  #First this is done by first computing the convex hull and testing if each of the grid points lies in the hull
  hull <- convex_hull(points = inputPoint)
  
  #grid points in convex hull
  inHull = in_convex_hull(hull, testPoint)
  
  tri=as.matrix(tri)
  
  if(!is.matrix(testPoint))
  {
    testPoint= as.matrix(testPoint)
  }
  if(!is.matrix(inputPoint))
  {
    inputPoint= as.matrix(inputPoint)
  }
  
  
  #grid points outisde convex hull
  gridOutside=which(inHull==FALSE)
  
  testPoint[gridOutside,]=0.1 # set point outside the convex hull to 0.1 for now
  
  gridSpaceSimplex=NULL
  
  rowCount = nrow(testPoint)
  
  for(j in c(1:rowCount)){
    if(testPoint[j,][1] != 0.1){
      k=1;
      
      #loop over the tri
      for(i in c(1:length(tri[,1]))-1){
        
        #get the point which makes the simplex
        p1=tri[k,]
        p2=p1+1  # add 1 to each of the trigulation simplix before getting the points because R starts from 1
        
        triPoint= inputPoint[p2,]
        
        
        #get the baycentric of this point
        byPoint = rbind(c(testPoint[j,]))
        
        barycentricCoordinatesPoint = getBycentriCoordinate(triPoint,byPoint)
        
        #the point is inside the triangle if the sum of the barycentricoordinate =1 and and are all positive
        
        #first, we check if the barycentri coordinate are all positive
        checkSign = sign(barycentricCoordinatesPoint[1,])
        
        
        ##check if any of the barycentri are zero
        zeroBarcentric=barycentricCoordinatesPoint[1,]==0
        
        #next, sum all  barycentri coordinate and if ==1
        sumBarycentricCoordinatesPoint = rowSums(barycentricCoordinatesPoint)
        
        
        
        #If one or two barycentric coordinates are zero the point lies on the corresponding one or two edges of the element or if all are positive the point is inside.
        
        #if(length(which(zeroBarcentric==FALSE)) >0){
        # print("here1")
        #  print(t)
        #  gridSpaceSimplex[j]= t
        #  break    #point is inside the triangle, so break
        #	}
        if(length(which(checkSign==-1)) == 0 ){
          gridSpaceSimplex[j]= i
          break    #point is inside the triangle, so break
        } 
        else
          gridSpaceSimplex[j]= -1 #point is outside the triangle
        
        k<-k+1
        
      }
      
    }
    else{
      gridSpaceSimplex[j]= -1 #point is outside the triangle
    }
  }
  return (gridSpaceSimplex)
  #return(.Call("C_findSimplex", hull$convexhull, testPoints, PACKAGE="alphashape"))
}
