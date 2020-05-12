library(alphashape)

x <- c(30, 70, 20, 50, 40, 70)
y <- c(35, 80, 70, 50, 60, 20)
p <- data.frame(x, y)
p_test <- data.frame(c(20, 50, 60, 40), c(20, 60, 60, 50))
dt <- delaunay(points = p)
simplex <- find_simplex(simplicies = a_complex, test_points = p_test)

# The correct answer should be [-1, 3, 1, 6] as the first test point isn't in any
# simplex, and the 2nd, 3rd, and 4th test points are in simplex 3, 1, and 6
# respectively.  Though I do wonder if NA would be better than -1 for those points
# not in any simplex, in which case the answer would be [NA, 3, 1, 6].

plot(p, pch = as.character(seq(nrow(p))), xlim=c(20,90))
for (s in seq(nrow(dt$simplices))) {
  polygon(dt$input_points[dt$simplices[s,],], border="red")
  text(x=colMeans(dt$input_points[dt$simplices[s,],])[1],
       y=colMeans(dt$input_points[dt$simplices[s,],])[2],
       labels=s, col="red")
}
points(p_test[,1], p_test[,2], pch=c("1", "2", "3", "4"), col="blue")
legend("topright", legend = c("data points", "simplicies", "test points"), 
       text.col=c("black", "red", "blue"), title = "Indicies for:", bty="n")


