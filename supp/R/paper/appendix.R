## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE, 
  message = FALSE,
  warning = FALSE)


## ----libraries-------------------------------------------------------------------------------
# Load libraries
library(tourr)
library(plotrix)
library(here)


## ----othermethod, out.width="100%", width=12, height=4, fig.cap="Manual controls with algorithm 2. The precise location of the axis is maintained."----
source(here("../src/plot_basis.R"))
source(here("../src/linear_alg.R"))

# Base plot set up
plot.new()
par(pty="s", xaxt="n", yaxt="n", bty="n",
      omi=c(0,0,0,0), mar=c(0,0,0,0),
    mfrow=c(1,4))

# Create manual tour example
p <- 4
d <- 2
set.seed(24)
A <- matrix(runif(p*d, min=-1), ncol=d, byrow=TRUE)
colnames(A) <- c("P1", "P2")
rownames(A) <- paste0("V", 1:p)
A <- tourr::orthonormalise(A)
  
# Now set a new position
vchange <- 3
eps <- 0.1
plot_basis(A, vchange, vcol="black")
text(A[vchange, 1], A[vchange, 2], expression(V[m]), pos=3, col="orange")

# j is the component that should be kept fixed
orthonormalize_rotated <- function(A, j){
  m1 <- A[j, 1]
  m2 <- A[j, 2]
  c <- 1 / (sqrt(1 + (m2/m1)^2))
  s <- (m2/m1) / (sqrt(1 + (m2/m1)^2))
  rot <- matrix(c(c, -s, s, c), nrow = 2)
  rot_inv <- matrix(c(c, s, -s, c), nrow = 2)
  for(i in 1:nrow(A)){
    A[i,] <- rot %*% A[i,]
  }
  A[,1] <- normalize_w_fixed(A[,1], j)
  A <- tourr::orthonormalise(A)
  for(i in 1:nrow(A)){
    A[i,] <- rot_inv %*% A[i,]
    }
  A
}

# normalize a vector while keeping component j fixed
normalize_w_fixed <- function(v, j){
  c <- v[j]
  k <- sqrt((1-c^2) / sum(v[-j]^2))
  v[-j] <- k * v[-j]
  v
}

# Iterate and save images
# Unconstrained orthonormalization
Anew <- A
for (i in 1:3) {
  Anew[vchange,] <- A[vchange,]*(1-i*eps)
  exact <- Anew[vchange,]
  Anew <- orthonormalize_rotated(Anew, vchange)
  plot_basis(Anew, vchange, vcol="black")
  points(exact[1], exact[2], pch=16, col="orange")
  text(Anew[vchange, 1], Anew[vchange, 2], expression(V[m]), pos=3, col="orange")

}



## ----anchornav, fig.width=10, fig.height=3, out.width="100%", fig.cap="Visual guide for the slice center. Variable axes are displayed in polar coordinates, where the center corresponds to the minimum value and the outer end corresponds to the maximum value. The position of the center corresponds to the dark polygon. If the center is at the center of the data, this will be displayed as a regular polygon (A), and plots B and C show its position when moving the center along one axis."----
par(pty="s", mfrow=c(1,3))
plot(c(-1, 1), c(-1, 1), type="n", axes=FALSE, xlab = "", ylab = "", main="A. data center")
anchor = data.frame(matrix(rep(0.5, 6), ncol=6))
tourr:::draw_slice_center(
   anchor, 
   rng = matrix(rep(c(0,1), 6), ncol=6, byrow=FALSE),
   limits = 1, anchor_nav = "center")
plot(c(-1, 1), c(-1, 1), type="n", axes=FALSE, xlab = "", ylab = "", main="B. center moved towards max value of X3")
anchor[3] <- 0.75
tourr:::draw_slice_center(
   anchor, 
   rng = matrix(rep(c(0,1), 6), ncol=6, byrow=FALSE),
   limits = 1, anchor_nav = "center")
plot(c(-1, 1), c(-1, 1), type="n", axes=FALSE, xlab = "", ylab = "", main="C. center moved towards min value of X3")
anchor[3] <- 0.25
tourr:::draw_slice_center(
   anchor, 
   rng = matrix(rep(c(0,1), 6), ncol=6, byrow=FALSE),
   limits = 1, anchor_nav = "center")

