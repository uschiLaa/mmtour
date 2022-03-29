# Load libraries
library(tourr)
library(plotrix)

# Code to re-produce mathematica manual control
p <- 4
d <- 2
set.seed(24)
A <- matrix(runif(p*d, min=-1), ncol=d, byrow=TRUE)
colnames(A) <- c("P1", "P2")
rownames(A) <- paste0("V", 1:p)
A <- tourr::orthonormalise(A)

# Plot basis
png("mtour_plots/mtour0.png", width=250, height=250)
plot_basis(A)
dev.off()

# Now set a new position
vchange <- 3
eps <- 0.01
#A[vchange,] <- A[vchange,]*(1-eps)
#A <- tourr::orthonormalise(A)
#plot_basis(A)

# Iterate and save images
for (i in 1:20) {
  flname <- paste0("mtour_plots/mtour", i, ".png")
  A[vchange,] <- A[vchange,]*(1-i*eps)
  A <- tourr::orthonormalise(A)
  png(flname, width=250, height=250)
  plot_basis(A)
  dev.off()
}

# Utility function to plot a basis
plot_basis <- function(A) {
  # draw circle
  plot.new()
  par(pty="s", xaxt="n", yaxt="n", bty="n",
      omi=c(0,0,0,0), mar=c(0,0,0,0))
  plot(c(-1.1, 1.1), c(-1.1, 1.1), type="n", xlab="", ylab="")
  plotrix::draw.circle(0,0,1)

  # label axes
  text(A[,1], A[,2], rownames(A), col="grey50")

  # draw axes
  A <- cbind(A, xctr=rep(0, nrow(A)), yctr=rep(0, nrow(A)))
  segments(A[,3], A[,4], A[,1], A[,2], lwd=3)
}
