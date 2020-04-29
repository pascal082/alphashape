#' @title Convex layer
#' 
#' @description  This function calculates a 
#' \href{https://en.wikipedia.org/wiki/Convex_layers}{convex layer} of specified 
#' depth from a set of \eqn{n} points in \eqn{d}-dimensional space using the
#' \href{http://www.qhull.org}{Qhull} library.
#'
#' @param points a \eqn{n}-by-\eqn{d} dataframe or matrix. The rows
#'   represent \eqn{n} points and the \eqn{d} columns the coordinates in 
#'   \eqn{d}-dimensional space.
#' @param layer an integer that specifies the desired convex layer.  If left 
#'   unspecified convex layer 1 is returned that is equivalent to the convex 
#'   hull.
#'   
#' @return Returns a list consisting of: [1] a matrix for which each row is the 
#' pair of point indices that define the egde of the convex layer; [2] a vector 
#' of the point indicies that form the convex layer; [3] a matrix of point 
#' coordinates that form the convex layer; and [4] the input points used to 
#' create the convex layer.
#'
#' @seealso \code{\link{convex_hull}}
#' 
#' @references Barber CB, Dobkin DP, Huhdanpaa H (1996) The Quickhull algorithm 
#' for convex hulls. ACM Transactions on Mathematical Software, 22(4):469-83 
#' \url{https://doi.org/10.1145/235815.235821}.
#' 
#' @examples
#' # Define points
#' x <- rnorm(50)
#' y <- rnorm(50)
#' p <- data.frame(x, y)
#' # Create convex layer and plot
#' cl <- convex_layer(points = p, layer = 3)
#' plot(p, pch = as.character(seq(nrow(p))))
#' polygon(cl$hull_points, border = "red")
#' 
#' @export
  convex_layer <- function(points = NULL, layer = 1) {
    
  	# Check directory writable
  	tmpdir <- tempdir()
  	# R should guarantee the tmpdir is writable, but check in any case
  	if (file.access(tmpdir, 2) == -1) {
  		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
  	}
  	
    # Coerce the input to be matrix
    if(is.null(points)){
      stop(paste("points must be an n-by-d dataframe or matrix", "\n"))
    }
    if(!is.data.frame(points) & !is.matrix(points)){
      stop(paste("points must be a dataframe or matrix", "\n"))
    }
    if (is.data.frame(points)) {
      points <- as.matrix(points)
    }
    # Make sure we have real-valued input
    storage.mode(points) <- "double"
    # We need to check for NAs in the input, as these will crash the C code.
    if (any(is.na(points))) {
      stop("points should not contain any NAs")
    }
  	
  	# Specify the Qhull options: http://www.qhull.org/html/qh-optq.htm
  	options <- "Qt"
	
    for (i in seq(layer)) {
      if (i == 1) {
        layerpoints <- points
        # Call C function to create the convex hull
      	ch <- .Call("C_convex", layerpoints, options, tmpdir, PACKAGE="alphashape")
      	# Re-index from C numbering to R numbering
      	ch$convex_hull[is.na(ch$convex_hull)] <- 0
      	edges <- as.data.frame(ch$convex_hull + 1)
      	indicies <- unique(c(as.integer(ch$convex_hull + 1)))
      	if (layer == 1) {
        	# Create list to return the desired convex hull information
        	convex <- list()
        	convex$hull_edges <- as.matrix(edges)
        	convex$hull_indices <- indicies
        	convex$hull_points <- layerpoints[convex$hull_indices,]
        	convex$input_points <- points
      	}
      } else {
        layerpoints <- layerpoints[-indicies,]
        # Call C function to create the convex hull
      	ch <- .Call("C_convex", layerpoints, options, tmpdir, PACKAGE="alphashape")
      	# Re-index from C numbering to R numbering
      	ch$convex_hull[is.na(ch$convex_hull)] <- 0
      	edges <- as.data.frame(ch$convex_hull + 1)
      	indicies <- unique(c(as.integer(ch$convex_hull + 1)))
      	if (i == layer) {
        	# Create list to return the desired convex hull information
        	convex <- list()
        	convex$hull_edges <- as.matrix(edges)
        	convex$hull_indices <- indicies
        	convex$hull_points <- layerpoints[convex$hull_indices,]
        	convex$input_points <- points
      	}
      }
    }
  	return(convex)
  }
 
