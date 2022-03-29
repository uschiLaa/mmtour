

# Plot basis
#png("mtour_plots/mtour0.png", width=250, height=250)
#plot_basis(A)
#dev.off()

#A[vchange,] <- A[vchange,]*(1-eps)
#A <- tourr::orthonormalise(A)
#plot_basis(A)



# Utility function to plot a basis
plot_basis <- function(A) {
  # Set plot canvas
  plot(c(-1.1, 1.1), c(-1.1, 1.1),
       type="n", xlab="", ylab="", axes=FALSE)

  # draw circle
  plotrix::draw.circle(0,0,1)

  # label axes
  text(A[,1], A[,2], rownames(A), col="grey50")

  # draw axes
  A <- cbind(A, xctr=rep(0, nrow(A)), yctr=rep(0, nrow(A)))
  segments(A[,3], A[,4], A[,1], A[,2], lwd=3)
}
