##' @title in convex hull
##'
##' @description  To test if a given point is inside the convex hull. \code{TRUE}  if the point  lies within the hull and \code{FALSE} if it
##' lies outwith the hull 
##' 
##' @param  hull object Convex hull simplices produced using convex function
##' @param points: dataframe  \code{n}-by-\code{dim} of points to check. 
##' @return A \code{n*m} vector containing the result. True if a given point was inside the convexhull , otherwise false
##' @examples 
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p =data.frame(x,y)
##' convex =convex(point=p)
##' #point to check
##' p2 = data.frame(x,y)
##' inconvexhull(convex,p2)
#' @export inconvexhull
inconvexhull <- function(hull, points) {
	
	if(!is.data.frame(points)){
		stop("Test input point is not a dataframe ");
	}else{
	  points=as.matrix(points)
	}
	
	if(!is.matrix(hull$convexhull)){
		stop("convex hull point produce is not a matrix. Please make sure you are passing the convex hull object ");
	
	}
	
	
	return(.Call("C_inconvexhull", hull$convexhull, points, PACKAGE="alphashape"))
}