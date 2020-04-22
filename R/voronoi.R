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

	
	if(!is.data.frame(point) & !is.matrix(point)){
	  stop("point must be a dataframe or matrix ");
	}
	if(!is.matrix(point)){
	  point=as.matrix(point)
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
	
	result<- .Call("C_voronoiR", point, as.character(options), tmpdir, PACKAGE="alphashape")
 
	#list to hold the voronoi_object
	voronoi_object =list()
	
	#re-index from C numbering to R
	result$tri[is.na(result$tri)] = 0 
	tri= result$tri + 1
	
	
	#add to element to voronoi_object list
	voronoi_object$tri = tri
	
	if (full) {
	  
	  if (nrow(voronoi_object$tri) == 1) 
	  {		
	    voronoi_object$neighbours <- NULL
	    voronoi_object$voronoi_vertices <- NULL
	    voronoi_object$circumRadii <- NULL
	    voronoi_object$tri <- NULL
	    
	  }else{
	    voronoi_object$neighbours <- result$neighbours
	    voronoi_object$voronoi_vertices <- result$voronoi_vertices
	    voronoi_object$circumRadii <- result$circumRadii
	    voronoi_object$tri <- result$tri 
	  }
	  voronoi_object$input_point =point
	}else{
	  return(voronoi_object[1])
	}
	



	
	return(voronoi_object)
}

#' @title Print Voronoi Diagram Object
#' @description A function the print the voronoi_diagram class without the object.
#' @keywords internal
#' @export
print.voronoi_diagram <-function(x, ...) print(x[1:5])
