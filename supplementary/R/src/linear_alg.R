# Orthonormalise with frozen tour, and post-process to
# constrain second vector with frozen components
orthonormalise_frozen <- function(x, v) {
  # x is a pxd basis, not orthonormal
  # v is the component that is frozen/fixed

  # Store frozen/fixed components
  p <- nrow(x)
  d <- ncol(x)
  frozen <- matrix(NA, nrow = p, ncol = d)
  frozen[v,] <- x[v,]

  # Remove from basis
  x_froz <- tourr::freeze(x, frozen)

  # Orthonormalise non-frozen matrix
  x_froz <- tourr::orthonormalise(x_froz)

  # Put back frozen components, and adjust second vector
  # to maintain orthogonality
  x_thaw <- thaw(x_froz, frozen)
  x_out <- x_thaw
  for (i in 1:p) {
    if (i != v) {
      x_out[i, 2] <- x_out[i, 2] + x_out[v, 1]*x_out[v, 2]/(p-1)
    }
  }

  # Return changed matrix
  x_out
}
