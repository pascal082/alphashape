##' @title Get Voronoi triangle and Delaunay triangulation
##' @description  Get Voronoi vertices(circumcenters of Delaunay triangle) and Delaunay triangulation in N-dimensions using the qhull library. The Voronoi diagram is the nearest-neighbor map 
##' for a set of points. Each region contains those points that are nearer one input site than any other input site. They can also be describe as the circumcenters of Delaunay triangle.
##' The Delaunay triangulation is a tessellation of the convex hull ofthe points such that no N-sphere defined by the N-triangles contains any other points from the set. 
##' @param point \code{point} is an \code{n}-by-\code{dim} dataframe or matrix. The rows of \code{point} represent \code{n} points in \code{dim}-dimensional space.
##' @param options String containing extra options for the underlying Qhull command.(See the Qhull documentation (\url{../doc/html/qvoronoi.html}) for the available options.) The
##'   \code{Qbb} option is always passed to Qhull. The default options are \code{Qcc Qc Qt Qz} for \code{dim} <4 and \code{Qcc Qc Qt
##'   Qx} for \code{dim}>=4.  If neither of the \code{QJ} or \code{Qt} options are supplied, the \code{Qt} option is passed to Qhull.
##'   The \code{Qt} option ensures all Delaunay regions are simplical (e.g., triangles in 2-d).  See \url{../doc/html/qdelaun.html} for more details. 
##' @param full Return all information associated with triangulation as a list. This will return the triangulation (\code{tri}), list ofvoronoi Vertices (\code{voronoiVertices}) , vpoints (\code{points}) ,matrix list of the circum radii (\code{circumRadii}), matrix list of (\code{SimplicesPoints})  and a list of neighbours of each facet (\code{neighbours}).
##' @return The voronoi vertics, circumRadii,trigulation Points and Delaunay triangulation
##' @examples 
##' x = c(30,70,20,50,40,70)
##' y = c(35,80,70,50,60,20)
##' p = data.frame(x,y)
##' voronoi(point =p)
#' @export
voronoi <- function(point=NULL, options=NULL, full=TRUE) {
	## Check directory writable
	tmpdir <- tempdir()
	
	## R should guarantee the tmpdir is writable, but check in any case
	if (file.access(tmpdir, 2) == -1) {
		stop(paste("Unable to write to R temporary directory", tmpdir, "\n"))
	}

	
	if(!is.data.frame(point) || !is.matrix(point)){
	  stop("point must be a dataframe or matrix ");
	}
	if(!is.matrix(point)){
	  points=as.matrix(point)
	}
	## Make sure we have real-valued input
	storage.mode(point) <- "double"
	
	## We need to check for NAs in the input, as these will crash the C
	## code.
	if (any(is.na(point))) {
		stop("The first argument should not contain any NAs")
	}
	
	## Default options
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
	
	ret <- .Call("C_voronoiR", point, as.character(options), tmpdir, PACKAGE="alphashape")
	
	if (nrow(ret$tri) == 1) 
	{		
		ret$neighbours <- NULL
		ret$voronoiVertices <- NULL
		ret$circumRadii <- NULL
		ret$tri <- NULL
		
	} 
	ret$tri[is.na(ret$tri)] = 0
	ret$tri = ret$tri + 1
	
	if (!full) {
		return(ret$voronoiVertices)
	}
	
	ret$inputPoints =point
	
	class(ret) <- "Voronoi DIagram"
	return(ret)
}
