#' @title Delaunay triangulation in N-dimensions. 
#' @description  The Delaunay triangulation is a tessellation of the convex hull of the points such that no N-sphere defined by the N-triangles
#' contains any other points from the set. This function calculates the Delaunay triangulation  in N-dimensions using the qhull library
#' @param point \code{point} is an \code{n}-by-\code{dim} dataframevor matrix. The rows of
#'   \code{point} represent \code{n} points in \code{dim}-dimensional
#'   space.
#' @param options String containing extra options for the underlying Qhull command.(See the Qhull documentation (\url{../doc/html/qdelaun.html}) for the available options.) The
#'   \code{Qbb} option is always passed to Qhull. The default options are \code{Qcc Qc Qt Qz} for \code{dim} <4 and \code{Qcc Qc Qt Qx} for \code{dim}>=4.  If neither of the \code{QJ} or \code{Qt}
#'   options are supplied, the \code{Qt} option is passed to Qhull. The \code{Qt} option ensures all Delaunay regions are simplical
#'   (e.g., triangles in 2-d).  See \url{../doc/html/qdelaun.html} for more details. The degenerate (zero area) regions are returned For silent operation, specify the option \code{Pp}.
#' @param full Return all information associated with the triangulation as a list and these are triangulation (\code{tri}), a  vector of facet areas (\code{areas}) and a list of neighbours of
#'   each facet (\code{neighbours}) OR return the convexhull and the input point
#' @examples 
#' # Define points
#' x <- c(30, 70, 20, 50, 40, 70)
#' y <- c(35, 80, 70, 50, 60, 20)
#' p <- data.frame(x, y)
#' # Create Delaunay triangulation and plot
#' dt <- delaunay(points = p)
#' plot(p, pch = as.character(seq(nrow(p))))
#' @export
  delaunay <- function(points=NULL) {
	
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
    
    # Call C function to create the Delaunay triangulation
    dt <- .Call("C_delaunayn", points, options, tmpdir, PACKAGE="alphashape")
    # Re-index from C numbering to R numbering
    dt$tri[is.na(dt$tri)] <- 0
    tri = dt$tri + 1
    
    # Create list to return the desired Delaunay triangulation information
    delaunay =list()
    delaunay$tri = tri
    if (nrow(delaunay$tri) == 1) {
      delaunay$areas <- NULL
      delaunay$neighbours <- NULL
    }else{
      delaunay$areas <- dt$areas
      delaunay$neighbours <- dt$neighbours
    }
    delaunay$input_points <- points

    return(delaunay)
  }
