library(tidyverse)
library(palmerpenguins)
library(classifly)
library(MASS)
library(tourr)
library(randomForest)

penguins_data <- penguins[, c(1,3, 4, 5, 6)] %>%
  drop_na() %>%
  mutate(across(where(is.numeric), function(x) (x-mean(x))/sd(x)))


penguins_lda <- classifly(penguins_data, species ~ . , lda)
penguins_lda[,1:4] <- apply(penguins_lda[,1:4], 2,  function(x) (x-mean(x))/sd(x))
lda_samples <- penguins_lda %>%
  as_tibble() %>%
  filter(.TYPE == "simulated")

animate_slice(lda_samples[,1:4], col = lda_samples$species, v_rel = 0.01)

write_csv(penguins_lda[, c(1:4,6)], file = "data/pengins_lda.csv")

set.seed(21)
penguins_rf <- classifly(penguins_data, species ~ . , randomForest)
penguins_rf[,1:4] <- apply(penguins_rf[,1:4], 2,  function(x) (x-mean(x))/sd(x))
rf_samples <- penguins_rf %>%
  as_tibble() %>%
  filter(.TYPE == "simulated")

animate_slice(rf_samples[,1:4], col = rf_samples$species, v_rel = 0.01)

write_csv(penguins_rf[, c(1:4,6)], file = "data/pengins_rf.csv")

