##' @title Convex hull in N-dimensions. 
##' @description  This function calculates the convex hull in N-dimensions using the qhull library
##'
##' @param point \code{point} is an \code{n}-by-\code{dim} of dataframe or matrix. The rows of
##'   \code{point} represent \code{n} points in \code{dim}-dimensional
##'   space.
##' @param options String containing extra options for the underlying Qhull command.(See the Qhull documentation (\url{../doc/html/qdelaun.html}) for the available options.) The
##'   \code{Qbb} option is always passed to Qhull. The default options are \code{Qt}.  The degenerate (zero area) regions are returned
##'   For silent operation, specify the option \code{Pp}.
##' @seealso Used internally by \code{\link{convex}}
##' @examples 
##' # define point
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p = data.frame(x,y)
##' ch = convex(point = p)
##' plot(p, pch=as.character(seq(nrow(p))))
##' polygon(ch$convexSet, border="red")
#' @export
  convex <- function(point=NULL, options=NULL) {
	## Check directory writable
	tmpdir <- tempdir()
	## R should guarantee the tmpdir is writable, but check in any case
	if (file.access(tmpdir, 2) == -1) {
		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
	}
	
	## Coerce the input to be matrix
	if(is.null(point)){
		stop(paste("dataframe or matrix in n-dimension is needed", "\n"))
	}
	
	if(!is.data.frame(point) & !is.matrix(point)){
	  stop(paste("Point must be a dataframe or matrix", "\n"))
	}
	if (is.data.frame(point)) {
	  point <- as.matrix(point)
	}
	
	## Make sure we have real-valued input
	storage.mode(point) <- "double"
	
	## We need to check for NAs in the input, as these will crash the C
	## code.
	if (any(is.na(point))) {
		stop("The first argument should not contain any NAs")
	}
	
	options <- "Qt"
	
	options <- paste(options, collapse=" ")
	
	## It is essential that delaunayn is called with either the QJ or Qt
	## option. Otherwise it may return a non-triangulated structure, i.e
	## one with more than dim+1 points per structure, where dim is the
	## dimension in which the points p reside.
	if (!grepl("Qt", options) & !grepl("QJ", options)) {
		options <- paste(options, "Qt")
	}

	ret <-.Call("C_convex", point, as.character(options), tmpdir, PACKAGE="alphashape")
	
	class(ret) <- "convexHull"
	names(ret)[1] = "edgeIndex"
	# Create point indexing to fit R's numbering system
	ret$edgeIndex[is.na(ret$edgeIndex)] = 0
	ret$edgeIndex = ret$edgeIndex + 1
	# Extract the convex set information
	ret$setIndex = unique(c(as.integer(ret$edgeIndex)))
	ret$setPoints = point[ret$convexSetIndex,]
	
	return(ret)
  }
  
