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
#' point is within, or a value of NA if a test point is not within any of the 
#' simplicies.
#' 
#' @examples 
#' # Define points and create a Delaunay triangulation
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' a_complex <- alpha_complex(points = p, alpha = 20)
#' # Check which simplex the test points belong to
#' p_test <- data.frame(c(20, 50, 60), c(20, 50, 60))
#' simplex <- find_simplex(simplicies = a_complex, test_points = p_test)
#'  
#' @export
find_simplex <- function(simplicies, test_points) {
  
  # Coerce the input to be matrix
  if(is.null(test_points)){
    stop(paste("points must be an n-by-d dataframe or matrix", "\n"))
  }
  if(!is.data.frame(test_points) & !is.matrix(test_points)){
    stop(paste("points must be a dataframe or matrix", "\n"))
  }
  if (is.data.frame(test_points)) {
    test_points <- as.matrix(test_points)
  }

  # Identify the simplicies that test point belongs 
  # First check if the point lies in the convex hull
  hull <- convex_hull(points = simplicies$input_points)
  inHull <- in_convex_hull(hull, test_points)
  
  # Create an empty object to hold which simplex the test points are in
  test_points_simplex <- NULL
  
  # For each test point that is within the convex hull
  for (j in c(1:nrow(test_points))) {
    if (inHull[j] == TRUE) {
      k <- 1;
      
      # Loop over the tri
      for(i in c(1:length(simplicies$simplices[, 1]))) {
        
        # Get the point which makes the simplex
        p1 <- simplicies$simplices[k, ]
       
        tri_point <- simplicies$input_points[p1, ]
        
        
        #get the baycentric of this point
        by_point = rbind(c(test_points[j,]))
        
        barycentric_coordinates_point <- get_ny_centri_coordinate(tri_point,by_point)
        
        # The point is inside the triangle if the sum of the 
        # barycentricoordinate = 1 and and are all positive
        
        # Check if the barycentri coordinate are all positive
        check_sign <- sign(barycentric_coordinates_point[1, ])
        
        
        # Check if any of the barycentric coordinates are zero
        zero_ny_centri_coordinate <- barycentric_coordinates_point[1, ] == 0
        
        # Next, sum all  barycentric coordinates and if == 1
        sum_ny_centri_coordinate = rowSums(barycentric_coordinates_point)
        
        # If one or two barycentric coordinates are zero the point lies on the 
        # corresponding one or two edges of the element or if all are positive 
        # the point is inside.

        if (length(which(check_sign == -1)) == 0 ) {
          test_points_simplex[j] <- i
          break    # point is inside the triangle, so break
        } 
        else
          test_points_simplex[j] = NA # test point is not in a simplex
        
        k <- k + 1
        
      }
      
    }
    else {
      test_points_simplex[j] = NA # test point is not in a simplex
    }
  }
  return (test_points_simplex)
  #return(.Call("C_findSimplex", hull$convexhull, test_pointss, PACKAGE="alphashape"))
}

# Internal function used by \code{\link{find_simplex}} to calculate the bycentri co-ordinate
get_ny_centri_coordinate <- function(X, P) {
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

