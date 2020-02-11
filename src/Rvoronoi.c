/* Copyright (C) 2018

** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
*/

// Enable C++11 via this plugin
//[[Rcpp::plugins(cpp11)]]

#include "Ralphashape.h"
#include <unistd.h>              /* For unlink() */


SEXP C_voronoiR(const SEXP p, const SEXP options, SEXP tmpdir)
{
	  SEXP retlist, retnames;       /* Return list and names */
	  int retlen = 5;               /* Length of return list */
	  SEXP tri, circumRadii;         /* The triangulation, array of circumradii */
	  SEXP neighbour, neighbours;   /* List of neighbours */
	  SEXP voronoiVertices,point0 ;   /* voronoi vertices and  */
	  int i, j;
	  unsigned dim, n, simpliexDim, simplexRow,nk;
	  int exitcode = 1;
	  boolT ismalloc;
	  char flags[250];             /* option flags for qhull, see qh_opt.htm */
	  double *pt_array;


  /* Initialise return values */
	tri = voronoiVertices = point0=retlist=circumRadii = R_NilValue;

  /* We cannot print directly to stdout in R, and the alternative of
     using R_Outputfile does not seem to work for all
     architectures. Setting outfile to NULL, is not an option, as an
     open file handle is required for a call to freopen in the Qhull
     code when qh_new_qhull() is called. Therefore use the ersatz
     stdout, tmpstdout (see below). */
  /* FILE *outfile = NULL; */
  /* qh_fprintf() in userprint.c has been redefined so that a NULL
     errfile results in printing via REprintf(). */
  FILE *errfile = NULL;

	if(!isString(options) || length(options) != 1){
		error("Second argument must be a single string.");
	}
	if(!isMatrix(p) || !isReal(p)){
		error("First argument should be a real matrix.");
	}
  
  /* Read options into command */
	i = LENGTH(STRING_ELT(options,0)); 
  if (i > 200) 
    error("Option string too long");
  sprintf(flags,"qhull d Qbb T0 Fn %s", CHAR(STRING_ELT(options,0))); 

  /* Check input matrix */
	dim = ncols(p);
	n   = nrows(p);
	if(dim <= 0 || n <= 0){
		error("Invalid input matrix.");
	}
  if (n <= dim) {
    error("Number of points is not greater than the number of dimensions.");
  }

  i = 0, j = 0;

  pt_array = (double *) R_alloc(n*dim, sizeof(double)); 
  for(i=0; i < n; i++)
    for(j=0; j < dim; j++)
      pt_array[dim*i+j] = REAL(p)[i+n*j];
  ismalloc = False;   /* True if qhull should free points in qh_freeqhull() or reallocation */


  const char *name;

  name = R_tmpnam("Rf", CHAR(STRING_ELT(tmpdir, 0)));
  qhT *qh= (qhT*)malloc(sizeof(qhT));
  qh_zero(qh, errfile);



 exitcode = qh_new_qhull(qh, dim, n, pt_array, ismalloc, flags, tmpstdout, errfile);
  fclose(tmpstdout);
  unlink(name);
  free((char *) name); 
  exitcode= setjmp(qh->errexit);
  if (!exitcode)
  {                    /* 0 if no error from qhull */


    facetT *facet;                  /* set by FORALLfacets */
    vertexT *vertex, **vertexp;
    facetT *neighbor, **neighborp;
    ridgeT *ridge, **ridgep;

    qh_option(qh,"voronoi  _bbound-last  _coplanar-keep", NULL, NULL);
    qh->DELAUNAY= True;     /* 'v'   */
    qh->VORONOI= True;
    qh->SCALElast= True;    /* 'Qbb' */
    qh->KEEPcoplanar= True;

    if (dim >= 5)
    {
        qh_option(qh,"_merge-exact", NULL, NULL);
        qh->MERGEexact= True; /* 'Qx' always */
     }

    /* Count the number of facets so we know how much space to allocate in R */
    int nf=0;                 /* Number of facets */
    FORALLfacets {
      if (!facet->upperdelaunay) {
        nf++;
      }
      nk=nf;
      /* Double check. Non-simplicial facets will cause segfault
         below */
      if (! facet->simplicial) {
        Rprintf("Qhull returned non-simplicial facets -- try delaunayn with different options");
        exitcode = 1;
        break;
      }
    }

    /* Alocate the space in R */
    PROTECT(tri = allocMatrix(INTSXP, nf, dim+1));
    PROTECT(circumRadii = allocMatrix(REALSXP, nf, 1));
    PROTECT(neighbours = allocVector(VECSXP, nf));
    PROTECT(voronoiVertices= allocMatrix(REALSXP, nf, dim));
    PROTECT(point0= allocMatrix(REALSXP, nf, dim));

    /* Iterate through facets to extract information */
    int i=0;
    FORALLfacets {

      if (!facet->upperdelaunay) {
        if (i >= nf) {
          error("Trying to access non-existent facet %i", i);
        }


        boolT inorder=True;

		 qh_RIDGE innerouter= qh_RIDGEall;
		 printvridgeT printvridge= NULL;
		 qh_eachvoronoi_all(qh, errfile, printvridge, qh->UPPERdelaunay, innerouter, inorder);

        /* Triangulation */
        int j=0;

        if(qh->hull_dim ==3 && facet->toporient == qh_ORIENTclock)
		{
			FOREACHvertex_ (facet->vertices)
			{
				  if ((i + nf*j) >= nf*(dim+1))
					error("Trying to write to non-existent area of memory i=%i, j=%i, nf=%i, dim=%i", i, j, nf, dim);


				 INTEGER(tri)[i + nf*j] = qh_pointid(qh, vertex->point);
				  j++;
			  }
		}
		else if (facet->simplicial || qh->hull_dim ==1)
		{
			if ((facet->toporient == qh_ORIENTclock) || (qh->hull_dim > 2 && !facet->simplicial))
			{
				FOREACHvertex_(facet->vertices){
				   INTEGER(tri)[i + nf*j] = qh_pointid(qh, vertex->point);
				   j++;
				}
			 }
			else
			{
				FOREACHvertexreverse12_(facet->vertices){
					INTEGER(tri)[i + nf*j] = qh_pointid(qh, vertex->point);

					j++;
				}
			}
		}
		else
		{
			 if (facet->visible && qh->NEWfacets)
			 {
				 error("cant process");
				 break;
			 }

			  FOREACHridge_(facet->ridges)
			  {

				if ((ridge->top == facet) ^ qh_ORIENTclock)
				{
				  FOREACHvertex_(ridge->vertices){
					  INTEGER(tri)[i + nf*j] = qh_pointid(qh, vertex->point);
					  j++;
				  }
				}
				else
				{
				  FOREACHvertexreverse12_(ridge->vertices){
					 INTEGER(tri)[i + nf*j] = qh_pointid(qh, vertex->point);
					 j++;
				  }
				}

		   }
		}



        /* Neighbours */
        PROTECT(neighbour = allocVector(INTSXP, qh_setsize(qh, facet->neighbors)));
        j=0;
        FOREACHneighbor_(facet) {
          INTEGER(neighbour)[j] = neighbor->visitid ? neighbor->visitid: 0 - neighbor->id;
          j++;
        }
        SET_VECTOR_ELT(neighbours, i, neighbour);
        UNPROTECT(1);


        /* voronoi vertices */
        int k, num;


		if (qh->CENTERtype == qh_ASvoronoi)
		{
			   num= qh->hull_dim-1;

			  if (!facet->normal || !facet->upperdelaunay || !qh->ATinfinity)
			  {
					if (!facet->center)
						  facet->center= qh_facetcenter(qh,facet->vertices);

					  for (k = 0; k < num; k++)
					  {

						  REAL(voronoiVertices)[i + nf*k] = facet->center[k];
					  }


			  }
			  else
			  {
					for (k=0; k < num; k++)
					{
						REAL(voronoiVertices)[i + nf*k] =qh_INFINITE;
					}

			  }
		}
		else /* qh.CENTERtype == qh_AScentrum */
		{
			 num= qh->hull_dim;
				num--;


			 if (!facet->center)
				facet->center= qh_getcentrum(qh,facet);
			 for (k = 0; k < num; k++)
			 {
				 REAL(voronoiVertices)[i + nf*k] =facet->center[k];

			 }


		}

        i++;
      }

    }

     unsigned int firstTemp=0,secondTemp=0;
     simpliexDim = ncols(tri);
     simplexRow   = nrows(tri);

     //swap the first and second value in the triangulation to have a counter clockwise orientation
     for(int i =0; i<simplexRow ; i++)
     {
		  firstTemp =INTEGER(tri)[i];
		  secondTemp=INTEGER(tri)[i+simplexRow];
		  INTEGER(tri)[i] =secondTemp;
		  INTEGER(tri)[i+simplexRow] = firstTemp;
     }


     i = 0, j = 0;

	 int k=0;
	 for(j=0; j < nrows(tri); j++)
	 {
			 //get the point value using the trigulation index
			 int value = INTEGER(tri)[j];
			 REAL(point0)[j] =REAL(p)[value];

			 for(i=1; i<ncols(p);i++)
			 {

				 REAL(point0)[k + nk*i] = REAL(p)[value+ nrows(p)*i];
			 }
			 k++;
	  }

	  /* calculate the circum radii using the voronoi vertices and points containing the tringulation */

	   double *radii = calculateradill(point0,voronoiVertices);
	   for(int i=0; i<nrows(tri); i++)
	   {

			REAL(circumRadii)[i] = radii[i];
	   }


  }
  else
  { /* exitcode != 1 */
		/* There has been an error; Qhull will print the error
		   message */
		PROTECT(tri = allocMatrix(INTSXP, 0, dim+1));
		PROTECT(neighbours = allocVector(VECSXP, 0));
		PROTECT(circumRadii = allocMatrix(REALSXP, 0, 1));
		PROTECT(voronoiVertices = allocMatrix(REALSXP, 0, dim));
		PROTECT(point0 = allocMatrix(REALSXP, 0, dim));


		/* If the error been because the points are colinear, coplanar
		   &c., then avoid mentioning an error by setting exitcode=2*/
		/* Rprintf("dim %d; n %d\n", dim, n); */
		if ((dim + 1) == n)
		{
		  exitcode = 2;
		}
	}
	  PROTECT(retlist = allocVector(VECSXP, retlen));
	  PROTECT(retnames = allocVector(VECSXP, retlen));

	  SET_VECTOR_ELT(retlist, 0, tri);
	  SET_VECTOR_ELT(retnames, 0, mkChar("tri"));
	  SET_VECTOR_ELT(retlist, 1, neighbours);
	  SET_VECTOR_ELT(retnames, 1, mkChar("neighbours"));

	  SET_VECTOR_ELT(retlist, 2,voronoiVertices);
	  SET_VECTOR_ELT(retnames, 2, mkChar("voronoiVertices"));
	  SET_VECTOR_ELT(retlist, 3,circumRadii);
	  SET_VECTOR_ELT(retnames, 3, mkChar("circumRadii"));
	  SET_VECTOR_ELT(retlist, 4,point0);
	  SET_VECTOR_ELT(retnames, 4, mkChar("trigulationPoints"));

	  setAttrib(retlist, R_NamesSymbol, retnames);
	  UNPROTECT(7);

	  /* Register qhullFinalizer() for garbage collection and attach a
		 pointer to the hull as an attribute for future use. */
	  SEXP ptr, tag;
	  PROTECT(tag = allocVector(STRSXP, 1));
	  SET_STRING_ELT(tag, 0, mkChar("voronoi diagram"));


  PROTECT(ptr = R_MakeExternalPtr(qh, tag, R_NilValue));
  if (exitcode)
  {
       //qhullFinalizer(ptr);
  }
  else
  {
     //R_RegisterCFinalizerEx(ptr, qhullFinalizer, TRUE);
     setAttrib(retlist, tag, ptr);
  }
  UNPROTECT(2);
  
  if (exitcode & (exitcode != 2))
  {
	  if(exitcode ==1)
	  {
		  error("Qhull returned non-simplicial facets -- try delaunayn with different options. exitcode", exitcode);
	  }
	  else
	  {
		  error("Received error code %d from qhull.", exitcode);
	  }

  } 

  
	return retlist;
}





