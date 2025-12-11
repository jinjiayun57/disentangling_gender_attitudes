# ------------------------------------------------------------
# This script generates figures for the Latent Profile Analysis (LPA) results.
#
# Section 1 (Model selection):
#   - Reads fit statistics across candidate numbers of profiles (k)
#   - Plots AIC, BIC, log-likelihood, and entropy as faceted panels
#   - Saves the figure as "iccpy.png"
#
# Section 2 (Profile characterization; selected solution k = 5):
#   - Reads the k=5 profile dataset (cluster assignments + latent dimensions)
#   - Plots boxplots of the four latent dimensions (D, NS, M, E) by profile
#   - Saves the figure as "profiles.png"
# ------------------------------------------------------------

# Packages
library(dplyr)    # data manipulation
library(tidyr)    # pivot_longer
library(ggplot2)  # plotting
library(readr)

# ---- 1) Read and prepare data ----
info_criterion <- read_csv("infoCriterion.csv") %>%
  setNames(c("AIC", "BIC", "k", "LL", "Entropy")) %>%
  mutate(k = as.integer(k))

# Reshape to long format for ggplot faceting
info_long <- info_criterion %>%
  pivot_longer(cols = c(AIC, BIC, LL, Entropy),
               names_to = "metric",
               values_to = "value") %>%
  mutate(
    metric = recode(metric,
                    AIC = "AIC",
                    BIC = "BIC",
                    LL  = "Log-likelihood",
                    Entropy = "Entropy"),
    metric = factor(metric, levels = c("AIC", "BIC", "Log-likelihood", "Entropy"))
  )

# ---- 2) Plot ----


p <- info_long %>%
  ggplot(aes(x = k, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Number of profiles (k)", y = NULL) +
  scale_x_continuous(breaks = 1:k_max) +
  theme(
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill = "white")
  )

# ---- 3) Save figure ----
png(filename = "iccpy.png",units = "in", width = 10, height = 6, res = 500)
print (p)
dev.off() 

# ------------------------------------------------------------
# Profile characterization (k = 5)
# ------------------------------------------------------------
# ---- 1) Prepare data ----

lpa_k5 <- read_csv("lpa_k5.csv", show_col_types = FALSE)
# - Cluster: profile membership (coded as 0..4)
# - four latent variables: ns, m, d, e

profile_labels <- c(
  "0" = "Passionate Egalitarians",
  "3" = "Genuine Neutrals",
  "4" = "Forthright Moderates",
  "2" = "Evasive Traditionalists",
  "1" = "Unembellished Traditionalists"
)

var_levels <- c("d", "ns", "m", "e")

lpa_k5_long <- lpa_k5 %>%
  select(cluster, all_of(var_levels)) %>%
  pivot_longer(cols = all_of(var_levels),
               names_to = "dimension",
               values_to = "value") %>%
  mutate(
    dimension = factor(dimension, levels = var_levels),
    cluster = factor(cluster,
                     levels = names(profile_labels),
                     labels = unname(profile_labels))
  )

# ---- 2) Plot ----
p_profiles <- ggplot(lpa_k5_long, aes(x = dimension, y = value)) +
  geom_boxplot() +
  facet_wrap(~ cluster) +
  scale_x_discrete(labels = c(
    d  = expression(theta[p]^{(D)}),
    ns = expression(theta[p]^{(NS)}),
    m  = expression(theta[p]^{(M)}),
    e  = expression(theta[p]^{(E)})
  )) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )


png(filename = "profiles.png",units = "in", width = 12, height = 8, res = 500)
print (p_profiles)
dev.off() 
