
##' @title Find Simplex
##' @description Returns the simplicies of the delaunay trigulation which contains a given point.
##' @param tri delaunay trigulation simplex using \code{\link{delaunay}}
##' @param testPoint  \code{n}-by-\code{dim} dataframe of points to check.  
##' @param inputPoint  \code{n}-by-\code{dim} dataframe or matrix of original inputPoint. 
##' @details Given a grid point and a test point point, the find Simplex will identify the simplicies contianing the test point. 
##' It works by first checking for all point inside a convex hull, and then check if the center of the grid cell is inside the trigulation
##' @return A \code{n*m} vector containing the result. -1  if a given point is outside the trigulation, or the trigulation index if the center of the cell is inside the alpha shape
##' @examples 
##'  x = c(30,70,20,50,40,70)
##'  y = c(35,80,70,50,60,20)
##'  p = matrix(c(x,y), ncol=2)
##'  v=voronoi(point =p)
##'  meshGrdiSpace = grid.coords(mins=c(15,0), maxs=c(35,200), nCoords=5)
##'  simplex <- findSimplex(v$tri, v$inputPoint,meshGrdiSpace)
#' @export
findSimplex <- function(tri,inputPoint,testPoint) {
  
  if(!is.data.frame(testPoint)){
    stop("please supply a test point dataframe")
  }
 
  #Identify the simplicies that each grid point belongs 
  #First this is done by first computing the convex hull and testing if each of the grid points lies in the hull
  hull <- convex(point = inputPoint)
  
  #grid points in convex hull
  inHull = inconvexhull(hull, testPoint)
  
  tri=as.matrix(tri)
  testPoint= as.matrix(testPoint)
  inputPoint= as.matrix(inputPoint)
  
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