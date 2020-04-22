##' @title Delaunay triangulation in N-dimensions. 
##' @description  The Delaunay triangulation is a tessellation of the convex hull of the points such that no N-sphere defined by the N-triangles
##' contains any other points from the set. This function calculates the Delaunay triangulation  in N-dimensions using the qhull library
##' @param point \code{point} is an \code{n}-by-\code{dim} dataframevor matrix. The rows of
##'   \code{point} represent \code{n} points in \code{dim}-dimensional
##'   space.
##' @param options String containing extra options for the underlying Qhull command.(See the Qhull documentation (\url{../doc/html/qdelaun.html}) for the available options.) The
##'   \code{Qbb} option is always passed to Qhull. The default options are \code{Qcc Qc Qt Qz} for \code{dim} <4 and \code{Qcc Qc Qt Qx} for \code{dim}>=4.  If neither of the \code{QJ} or \code{Qt}
##'   options are supplied, the \code{Qt} option is passed to Qhull. The \code{Qt} option ensures all Delaunay regions are simplical
##'   (e.g., triangles in 2-d).  See \url{../doc/html/qdelaun.html} for more details. The degenerate (zero area) regions are returned For silent operation, specify the option \code{Pp}.
##' @param full Return all information associated with the triangulation as a list and these are triangulation (\code{tri}), a  vector of facet areas (\code{areas}) and a list of neighbours of
##'   each facet (\code{neighbours}) OR return the convexhull and the input point
##' @examples 
##' # define point
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p = data.frame(x,y)
##' delaunay =delaunay(point=p)
#' @export
  delaunay <- function(point=NULL, options=NULL, full=FALSE) {
	## Check directory writable
	tmpdir <- tempdir()
	## R should guarantee the tmpdir is writable, but check in any case
	if (file.access(tmpdir, 2) == -1) {
		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
	}
	
	
	if (!is.data.frame(point) & !is.matrix(point)) {
	  stop(paste("Point must be a dataframe or matrix", "\n"))
	}
	
	if (is.data.frame(point)) {
	  point <- as.matrix(point)
	}

	
	## Make sure we have real-valued input
	storage.mode(point) <- "double"
	
	## We need to check for NAs in the input, as these will crash the C
	## code.
	if (any(is.na(point))){
		stop("The first argument should not contain any NAs")
	}
	
	
	## Default options - using the same option call for delanauy and convexy hull
	if (is.null(options)) {
	
		if (ncol(point) < 4) {
			options <- "Qt Qc Qz"
		} else {
			options <- "Qt Qc Qx"
		}
		
	}
	
	options <- paste(options, collapse=" ")
	
	## It is essential that delaunayn is called with either the QJ or Qt
	## option. Otherwise it may return a non-triangulated structure, i.e
	## one with more than dim+1 points per structure, where dim is the
	## dimension in which the points p reside.
	if (!grepl("Qt", options) & !grepl("QJ", options)) {
		options <- paste(options, "Qt")
	}


	result <- .Call("C_delaunayn", point, as.character(options), tmpdir, PACKAGE="alphashape")
	
	#list to hold result
	delaunay =list()
	
	#re-index from C numbering to R
	result$tri[is.na(result$tri)] <- 0
	tri = result$tri + 1
	
	# Add element to delaunay list
	delaunay$tri = tri
	if (full) {
	
  	  if (nrow(delaunay$tri) == 1 ) {
  	    delaunay$areas <- NULL
  	    delaunay$neighbours <- NULL
  	  }else{
  	    delaunay$areas <- result$areas
  	    delaunay$neighbours <- result$neighbours
  	  }
  	  delaunay$input_points =point
	}else{
     return(delaunay[1])
  }

	return( delaunay)
}
