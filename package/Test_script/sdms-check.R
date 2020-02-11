x = c(30,70,20,50,40,70)
y = c(35,80,70,50,60,20)

p = matrix(c(x,y), ncol=2)

library(alphashape)

v = voronoi(point=p)

N = 100

as = nalphaShape(v, alphaRange=20, n=N)


X = matrix(as$gridSpace[,1], ncol=N, byrow=T)
Y = matrix(as$gridSpace[,2], ncol=N, byrow=T)
Z = matrix(as$alphaGrid, ncol=N)

library(fields)

image.plot(X, Y, Z, asp=1)


dt = delaunay(point=p) +1
for (i in 1:nrow(dt)) {
  tri = p[dt[i,],]
  polygon(x=tri[,1], y=tri[,2], col=NA, border="yellow")
}
# points(p, col="white")
text(x=p[,1], y=p[,2], labels=seq(1,6), col="white")
