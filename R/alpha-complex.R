#' @title Alpha complex
#' 
#' @description  This function calculates the 
#' \href{https://en.wikipedia.org/wiki/Alpha_shape}{alpha complex} of a set
#' of \eqn{n} points in \eqn{d}-dimensional space using the
#' \href{http://www.qhull.org}{Qhull} library.
#' 
#' @param points a \eqn{n}-by-\eqn{d} dataframe or matrix. The rows
#'   represent \eqn{n} points and the \eqn{d} columns the coordinates in 
#'   \eqn{d}-dimensional space.
#' @param alpha a positive number that defines the maximum circumradii for a 
#'   simplex to be included in the alpha complex.
#' 
#' @return Returns a list consisting of: [1] a \eqn{n}-by-\eqn{d+1} matrix of 
#' point indices that define the 
#' \href{https://en.wikipedia.org/wiki/Simplex}{simplices} that make up the
#' alpha complex; [2] a matrix of circumcentres for each simplex ; [3] a list of 
#' circumradii for each simplex; and [4] the input points used to create the 
#' alpha complex.
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
#' # Create alpha complex and plot
#' a_complex <- alpha_complex(points = p, alpha = 20)
#' plot(p, pch = as.character(seq(nrow(p))))
#' for (s in seq(nrow(a_complex$simplices))) {
#'   polygon(a_complex$input_points[a_complex$simplices[s,],], border="red")
#'   text(x=colMeans(a_complex$input_points[a_complex$simplices[s,],])[1],
#'        y=colMeans(a_complex$input_points[a_complex$simplices[s,],])[2],
#'        labels=s, col="red")
#' }
#' @export
alpha_complex <- function(points=NULL, alpha=NULL) {
	
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
    if (ncol(points) < 4) {
      options <- "Qt Qc Qz"
    } else {
      options <- "Qt Qc Qx"
    }
    options <- paste(options, collapse=" ")  

	  # Call C function to create the Voronoi diagram
  	vd <- .Call("C_voronoiR", points, options, tmpdir, PACKAGE="alphashape")
    # Re-index from C numbering to R numbering
    vd$tri[is.na(vd$tri)] <- 0
    tri <- vd$tri + 1
    
    # Create list to return the desired alpha complex information
    alpha_complex <- list()
    in_alpha_complex <- vd$circumRadii <= alpha
    
    alpha_complex$simplices <- tri[in_alpha_complex, ]
    
    if (nrow(alpha_complex$simplices) < 1) {
	    alpha_complex$circumcentres <- NULL
	    alpha_complex$circumradii <- NULL
	    } else {
	    alpha_complex$circumcentres <- vd$voronoi_vertices[in_alpha_complex,]
	    alpha_complex$circumradii <- vd$circumRadii[in_alpha_complex]
	  }
	  alpha_complex$input_points <- points

  	return(alpha_complex)
  }
