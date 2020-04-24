#' @title Voronoi diagram
#' 
#' @description  This function calculates the 
#' \href{https://en.wikipedia.org/wiki/Voronoi_diagram}{Voronoi digram} of a set
#' of \eqn{n} points in \eqn{d}-dimensional space using the
#' \href{http://www.qhull.org}{Qhull} library.
#' 
#' @param points a \eqn{n}-by-\eqn{d} dataframe or matrix. The rows
#'   represent \eqn{n} points and the \eqn{d} columns the coordinates in 
#'   \eqn{d}-dimensional space.
#' 
#' @return Returns a list consisting of...
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
#' # Create Voronoi diagram and plot
#' vd <- voronoi(points = p)
#' plot(p, pch = as.character(seq(nrow(p))))
#' @export
voronoi <- function(points=NULL) {
	
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
    tri = vd$tri + 1
    
    # Create list to return the desired Voronoi diagram information
    voronoi =list()
    voronoi$tri = tri
	  if (nrow(voronoi$tri) == 1) 
	  {		
	    voronoi$neighbours <- NULL
	    voronoi$voronoi_vertices <- NULL
	    voronoi$circumradii <- NULL
	    voronoi$tri <- NULL
	    
	  }else{
	    voronoi$neighbours <- vd$neighbours
	    voronoi$voronoi_vertices <- vd$voronoi_vertices
	    voronoi$circumradii <- vd$circumradii
	    voronoi$tri <- vd$tri
	  }
	  voronoi$input_points <- points

  	return(voronoi)
  }
