#' @title Get Voronoi triangle and Delaunay triangulation
#' @description  Get Voronoi vertices(circumcenters of Delaunay triangle) and Delaunay triangulation in N-dimensions using the qhull library. The Voronoi diagram is the nearest-neighbor map 
#' for a set of points. Each region contains those points that are nearer one input site than any other input site. They can also be describe as the circumcenters of Delaunay triangle.
#' The Delaunay triangulation is a tessellation of the convex hull ofthe points such that no N-sphere defined by the N-triangles contains any other points from the set. 
#' @param point \code{point} is an \code{n}-by-\code{dim} dataframe or matrix. The rows of \code{point} represent \code{n} points in \code{dim}-dimensional space.
#' @param options String containing extra options for the underlying Qhull command.(See the Qhull documentation (\url{../doc/html/qvoronoi.html}) for the available options.) The
#'   \code{Qbb} option is always passed to Qhull. The default options are \code{Qcc Qc Qt Qz} for \code{dim} <4 and \code{Qcc Qc Qt
#'   Qx} for \code{dim}>=4.  If neither of the \code{QJ} or \code{Qt} options are supplied, the \code{Qt} option is passed to Qhull.
#'   The \code{Qt} option ensures all Delaunay regions are simplical (e.g., triangles in 2-d).  See \url{../doc/html/qdelaun.html} for more details. 
#' @param full Return all information associated with triangulation as a list. This will return the triangulation (\code{tri}), list ofvoronoi Vertices (\code{voronoiVertices}) , vpoints (\code{points}) ,matrix list of the circum radii (\code{circumRadii}), matrix list of (\code{SimplicesPoints})  and a list of neighbours of each facet (\code{neighbours}).
#' @return The voronoi vertics, circumRadii,trigulation Points and Delaunay triangulation
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
	    voronoi$neighbours <- result$neighbours
	    voronoi$voronoi_vertices <- result$voronoi_vertices
	    voronoi$circumradii <- result$circumradii
	    voronoi$tri <- result$tri 
	  }
	  voronoi$input_points <- points

  	return(voronoi)
  }
