#' @title Convex hull in \eqn{d}-dimensions.
#' @description  This function calculates the 
#' \href{https://en.wikipedia.org/wiki/Convex_hull}{convex hull} around a set of
#' \eqn{n} points in \eqn{d}-dimensional space using the
#' \href{http://www.qhull.org}{Qhull} library.
#'
#' @param points \code{points} is an \eqn{n}-by-\eqn{d} of dataframe or
#'   matrix. The rows of \code{points} represent \eqn{n} points in 
#'   \eqn{d}-dimensional space.
#' @param options String containing extra options for the underlying Qhull
#'   command.(See the Qhull documentation (\url{../doc/html/qdelaun.html}) for
#'   the available options.) The \code{Qbb} option is always passed to Qhull.
#'   The default options are \code{Qt}.  The degenerate (zero area) regions are
#'   returned. For silent operation, specify the option \code{Pp}.
#'   
#' @return Returns a list consisting of...
#'
#' @seealso Used internally by \code{\link{convex_layer}}
#' 
#' @references Barber CB, Dobkin DP, Huhdanpaa H (1996) The Quickhull algorithm 
#' for convex hulls. ACM Transactions on Mathematical Software, 22(4):469-83 
#' \url{https://doi.org/10.1145/235815.235821}.
#' 
#' @examples
#' # Define points
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' # Create convex hull and plot
#' ch <- convex_hull(points = p)
#' plot(p, pch = as.character(seq(nrow(p))))
#' polygon(ch$hull_points, border = "red")
#' 
#' @export
  convex_hull <- function(points=NULL, options=NULL) {
	# Check directory writable
	tmpdir <- tempdir()

	# R should guarantee the tmpdir is writable, but check in any case
	if (file.access(tmpdir, 2) == -1) {
		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
	}
	
	# Coerce the input to be matrix
	if(is.null(points)){
		stop(paste("dataframe or matrix in n-dimension is needed", "\n"))
	}
	
	if(!is.data.frame(points) & !is.matrix(points)){
	  stop(paste("Points must be a dataframe or matrix", "\n"))
	}
	if (is.data.frame(points)) {
	  points <- as.matrix(points)
	}
	
	# Make sure we have real-valued input
	storage.mode(points) <- "double"
	
	# We need to check for NAs in the input, as these will crash the C code.
	if (any(is.na(points))) {
		stop("The points should not contain any NAs")
	}
	
	options <- "Qt"
	
	options <- paste(options, collapse=" ")
	
	# It is essential that delaunayn is called with either the QJ or Qt option.
	# Otherwise it may return a non-triangulated structure with more than d+1
	# points per structure, where d is the dimension in which the points p reside.
	if (!grepl("Qt", options) & !grepl("QJ", options)) {
		options <- paste(options, "Qt")
	}

  	result <- .Call("C_convex", points, as.character(options), tmpdir, PACKAGE="alphashape")
  	
  	# Create point indexing to fit R's numbering system
  	result$convex_hull[is.na(result$convex_hull)] <- 0
  	edges <- as.data.frame(result$convex_hull + 1)
  	
  	# Create list to return the desired convex hull information
  	convex <- list()
  	convex$hull_edges <- as.matrix(edges)
  	convex$hull_indices <- unique(c(as.integer(result$convex_hull + 1)))
  	convex$hull_points <- points[convex$hull_indices,]
  	convex$input_points <- points
  
  	return(convex)
  }
 
