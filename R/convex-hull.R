##' @title convex_hull in d-dimensions. 
##' @description  This function calculates the convex hull in \code{d}-
##' dimensional space using the qhull library.
##'
##' @param points \code{points} is an \code{n}-by-\code{d} of dataframe or 
##'   matrix. The rows of \code{points} represent \code{n} points in \code{d}-
##'   dimensional space.
##' @param options String containing extra options for the underlying Qhull 
##'   command.(See the Qhull documentation (\url{../doc/html/qdelaun.html}) for 
##'   the available options.) The \code{Qbb} option is always passed to Qhull. 
##'   The default options are \code{Qt}.  The degenerate (zero area) regions are
##'   returned. For silent operation, specify the option \code{Pp}.
##' @seealso Used internally by \code{\link{convex}}
##' @examples 
##' # Define points
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p = data.frame(x,y)
##' # Create convex hull and plot
##' ch = convex_hull(points = p)
##' plot(p, pch=as.character(seq(nrow(p))))
##' polygon(ch$setPoints, border="red")
#' @export
  convex_hull <- function(points=NULL, options=NULL) {
	## Check directory writable
	tmpdir <- tempdir()
	## R should guarantee the tmpdir is writable, but check in any case
	if (file.access(tmpdir, 2) == -1) {
		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
	}
	
	## Coerce the input to be matrix
	if(is.null(points)){
		stop(paste("dataframe or matrix in n-dimension is needed", "\n"))
	}
	
	if(!is.data.frame(points) & !is.matrix(points)){
	  stop(paste("Points must be a dataframe or matrix", "\n"))
	}
	if (is.data.frame(points)) {
	  points <- as.matrix(points)
	}
	
	## Make sure we have real-valued input
	storage.mode(points) <- "double"
	
	## We need to check for NAs in the input, as these will crash the C
	## code.
	if (any(is.na(points))) {
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

	convex<-.Call("C_convex", points, as.character(options), tmpdir, PACKAGE="alphashape")
	
	# Create point indexing to fit R's numbering system
	convex$convex_hull[is.na(convex$convex_hull)] = 0
	convex$convex_hull = convex$convex_hull + 1
	# Extract the convex set information
	convex$setIndex = unique(c(as.integer(convex$convex_hull)))
	convex$setPoints = points[convex$setIndex,]
	
	return(convex)
  }
  
