# Utility function to plot a basis
# Need to generalise to arbitrary d
plot_basis <- function(A, v=NULL, vcol = "orange", labels=FALSE) {
  # A is an orthonormal basis
  # v needs to be an integer between 1, and nrow(A)

  # Set plot canvas
  plot(c(-1.1, 1.1), c(-1.1, 1.1),
       type="n", xlab="", ylab="", axes=FALSE)

  # draw circle
  plotrix::draw.circle(0,0,1)

  # label axes
  if (labels) {
    if (is.null(v)) {
      text(A[,1], A[,2], rownames(A), col="grey50")
    } else {
      text(A[-v,1], A[-v,2], rownames(A)[-v], col="grey80")
      text(A[v,1], A[v,2], rownames(A)[v], col=vcol)
    }
  }

  # draw axes
  Aaxes <- cbind(A, xctr=rep(0, nrow(A)), yctr=rep(0, nrow(A)))
  if (is.null(v)) {
    segments(Aaxes[,3], Aaxes[,4], Aaxes[,1], Aaxes[,2], lwd=3)
  } else {
    segments(Aaxes[-v,3], Aaxes[-v,4], Aaxes[-v,1], Aaxes[-v,2], lwd=3, col="grey80")
    segments(Aaxes[v,3], Aaxes[v,4], Aaxes[v,1], Aaxes[v,2],
             col=vcol, lwd=3)
  }
}
