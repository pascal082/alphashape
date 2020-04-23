#' @title in convex hull
#'
#' @description  To test if a given point is inside the convex hull. \code{TRUE}  if the point  lies within the hull and \code{FALSE} if it
#' lies outwith the hull 
#' 
#' @param  points points to make convex hull
#' @param test_points: dataframe or matrix  \code{n}-by-\code{dim} of points to check. 
#' @return A \code{n*m} vector containing the result. True if a given point was inside the convexhull , otherwise false
#' @examples 
#' # Define points to create the convex hull
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' # Check if some new points are in the convex hull
#' new = data.frame(c(20, 50), c(20, 50))
#' checks <- in_convex_hull(points=p, test_points = new)
#' 
#' @export in_convex_hull
in_convex_hull <- function(points, test_points) {
  
  if(!is.data.frame(test_points) & !is.matrix(test_points)){
    stop("Test input point is not a dataframe or matrix ");
  }
  if(!is.matrix(test_points)){
    test_points=as.matrix(test_points)
  }
  

  
  # REPLACED WITH
  # Using the convex hull lists recreate the convex hull in C
  
  options <- "Qt"
  
  options <- paste(options, collapse=" ")
  
  # It is essential that delaunayn is called with either the QJ or Qt option.
  # Otherwise it may return a non-triangulated structure with more than d+1
  # points per structure, where d is the dimension in which the points p reside.
  if (!grepl("Qt", options) & !grepl("QJ", options)) {
    options <- paste(options, "Qt")
  }	
  
  # Coerce the input to be matrix
  if(is.null(points)){
    stop(paste("dataframe or matrix in n-dimension is needed", "\n"))
  }
  
  if(!is.data.frame(points) & !is.matrix(points)){
    stop(paste("Points to make convex hull must be a dataframe or matrix", "\n"))
  }
  if (is.data.frame(points)) {
    points <- as.matrix(points)
  }
  
  # Make sure we have real-valued input
  storage.mode(points) <- "double"
  
  # We need to check for NAs in the input, as these will crash the C code.
  if (any(is.na(points))) {
    stop("The points to make convex hull should not contain any NAs")
  }
  
  
  convex <- .Call("C_convex", points, as.character(options), tmpdir, PACKAGE="alphashape")
  
  
  return(.Call("C_inconvexhull", convex$convex_hull, test_points, PACKAGE="alphashape"))
  
}