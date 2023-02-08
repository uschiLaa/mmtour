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

## ----manualsequence, out.width="100%", width=12, height=4, fig.cap="Sequence of projections where the contribution of one variable is controlled (dark) is changed using unconstrained orthonormalization. The dot indicates the chosen values for the controlled variable, $V_m$. It can be seen that the actual axis does not precisely match the chosen position, but it is close."----
source(here("../src/plot_basis.R"))
source(here("../src/linear_alg.R"))

# Base plot set up
plot.new()
par(pty="s", xaxt="n", yaxt="n", bty="n",
      omi=c(0,0,0,0), mar=c(0,0,0,0),
    mfrow=c(2,4))

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
points(A[vchange, 1], A[vchange, 2], pch=16, col="orange")

# Iterate and save images
# Unconstrained orthonormalisation
Anew <- A
for (i in 1:3) {
  Anew[vchange,] <- A[vchange,]*(1-i*eps)
  exact <- Anew[vchange,]
  Anew <- tourr::orthonormalise(Anew)
  plot_basis(Anew, vchange, vcol="black")
  text(Anew[vchange, 1], Anew[vchange, 2], expression(V[m]), pos=3, col="orange")
  points(exact[1], exact[2], pch=16, col="orange")
}

## ----penguins-scatmat, out.width="60%", width=10, height=10, fig.align='center', fig.cap="Scatterplot matrix of the (standardized) penguins data. The three species are reasonably different in size, with Gentoo distinguished from the other two on body depth relative to flipper length and body mass."----
library(tidyverse)
library(palmerpenguins)
library(GGally)
penguins_data <- penguins[, c(1,3, 4, 5, 6)] %>%
  drop_na() %>%
  mutate(across(where(is.numeric), function(x) (x-mean(x))/sd(x))) %>%
  rename("bl"="bill_length_mm",
         "bd"="bill_depth_mm",
         "fl"="flipper_length_mm",
         "bm"="body_mass_g")
clrs <- c("#245668FF", "#EA4F88FF", "#ABDDA4FF")
penguins_diag <- tibble(x=1, y=c(3,2,1),
     species=levels(penguins_data$species))
pscat <- ggpairs(penguins_data, columns = 2:5,
          mapping = ggplot2::aes(color = species,
                                 fill = species),
          upper = list(continuous = "points"),
          diag = list(continuous = "densityDiag")) +
  scale_colour_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3])) +
  scale_fill_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3]))
pscat[1,1] <- ggplot(penguins_diag,
                     aes(x=x,
                         y=y,
                         label=species,
                         colour=species)) +
  geom_text() +
  scale_colour_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3])) +
  ylim(c(0,4))
pscat[2,2] <- ggplot(penguins_diag,
                     aes(x=x,
                         y=y,
                         label=species,
                         colour=species)) +
  geom_text() +
  scale_colour_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3])) +
  ylim(c(0,4))
pscat[3,3] <- ggplot(penguins_diag,
                     aes(x=x,
                         y=y,
                         label=species,
                         colour=species)) +
  geom_text() +
  scale_colour_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3])) +
  ylim(c(0,4))
pscat[4,4] <- ggplot(penguins_diag,
                     aes(x=x,
                         y=y,
                         label=species,
                         colour=species)) +
  geom_text() +
  scale_colour_manual("", values=c("Adelie" = clrs[1], "Chinstrap" = clrs[2], "Gentoo" = clrs[3])) +
  ylim(c(0,4))
pscat + theme_bw() +
  theme(aspect.ratio=1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

