

# Plot basis
#png("mtour_plots/mtour0.png", width=250, height=250)
#plot_basis(A)
#dev.off()

#A[vchange,] <- A[vchange,]*(1-eps)
#A <- tourr::orthonormalise(A)
#plot_basis(A)



# Utility function to plot a basis
# Need to generalise to arbitrary pxd
plot_basis <- function(A, v=NULL) {
  # Set plot canvas
  plot(c(-1.1, 1.1), c(-1.1, 1.1),
       type="n", xlab="", ylab="", axes=FALSE)

  # draw circle
  plotrix::draw.circle(0,0,1)

  # label axes
  text(A[,1], A[,2], rownames(A), col="grey50")
  if (!is.null(v))
    text(A[v,1], A[v,2], rownames(A), col="orange")


  # draw axes
  A <- cbind(A, xctr=rep(0, nrow(A)), yctr=rep(0, nrow(A)))
  segments(A[,3], A[,4], A[,1], A[,2], lwd=3)
  if (!is.null(v))
    segments(A[v,3], A[v,4], A[v,1], A[v,2],
             col="orange", lwd=3)

}
