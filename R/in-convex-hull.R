##' @title in convex hull
##'
##' @description  To test if a given point is inside the convex hull. \code{TRUE}  if the point  lies within the hull and \code{FALSE} if it
##' lies outwith the hull 
##' 
##' @param  hull object Convex hull simplices produced using convex function
##' @param points: dataframe or matrix  \code{n}-by-\code{dim} of points to check. 
##' @return A \code{n*m} vector containing the result. True if a given point was inside the convexhull , otherwise false
##' @examples 
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p =data.frame(x,y)
##' convex =in_convex_hull(point=p)
##' #point to check
##' p2 = data.frame(x,y)
##' in_convex_hull(convex,p2)
#' @export in_convex_hull
in_convex_hull <- function(hull, points) {
	
	if(!is.data.frame(points) & !is.matrix(points)){
		stop("Test input point is not a dataframe or matrix ");
	}
  if(!is.matrix(points)){
	  points=as.matrix(points)
  }
  
  
# REMOVED
# 	if(!is.matrix(hull$convex_hull)){
# 		stop("convex hull point produce is not a matrix. Please make sure you are passing the convex hull object ");
# 	
# 	}
# 	
# 	#modify R numbering to C by adding -1
#   hull_index = hull$convex_hull -1
  
# REPLACED WITH
  # Using the convex hull lists recreate the convex hull in C
  options <- "Qt"
  convex_hull <- .Call("C_convex", hull$hull_points, as.character(options), tmpdir, PACKAGE="alphashape")

	return(.Call("C_inconvexhull", convex_hull, points, PACKAGE="alphashape"))

}