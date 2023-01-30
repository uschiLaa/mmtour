library(tidyverse)
library(palmerpenguins)
library(classifly)
library(MASS)
library(tourr)
library(randomForest)
library(here)

penguins_data <- penguins[, c(1,3, 4, 5, 6)] %>%
  drop_na() %>%
  mutate(across(where(is.numeric), function(x) (x-mean(x))/sd(x)))


penguins_lda <- classifly(penguins_data, species ~ . , lda)
penguins_lda[,1:4] <- apply(penguins_lda[,1:4], 2,  function(x) (x-mean(x))/sd(x))
lda_samples <- penguins_lda %>%
  as_tibble() %>%
  filter(.TYPE == "simulated") %>%
  mutate(species_flag = as.numeric(as.factor(species)))

animate_slice(lda_samples[,1:4], col = lda_samples$species, v_rel = 0.01)

write_csv(lda_samples[, c(1:4,6,12)], file = here("../../data/penguins_lda.csv"))

set.seed(21)
penguins_rf <- classifly(penguins_data, species ~ . , randomForest)
penguins_rf[,1:4] <- apply(penguins_rf[,1:4], 2,  function(x) (x-mean(x))/sd(x))
rf_samples <- penguins_rf %>%
  as_tibble() %>%
  filter(.TYPE == "simulated") %>%
  mutate(species_flag = as.numeric(as.factor(species)))

animate_slice(rf_samples[,1:4], col = rf_samples$species, v_rel = 0.01)

write_csv(rf_samples[, c(1:4,6, 12)], file = here("../../data/penguins_rf.csv"))

# Plots for paper
library(GGally)
penguins_rf <- read_csv(here("../../data/penguins_rf.csv"))
penguins_lda <- read_csv(here("../../data/penguins_lda.csv"))
ggscatmat(penguins_data, columns = 2:5, col="species")
ggscatmat(penguins_rf, columns = 1:4, col="species", alpha=0.1)
ggscatmat(penguins_lda, columns = 1:4, col="species", alpha=0.1)


# saving the data to look at in mathematica
penguins_data %>%
  dplyr::select(c(2,3,4,5,1)) %>%
  mutate(species_flag =
           as.numeric(factor(species,
                             levels = # making sure we have the same flags
                               levels(as.factor(rf_samples$species))))) %>%
  write_csv(file = here("../../data/penguins.csv"))
