#alluvial, top 15 families with pooled ra and FOO.

#setting directory
setwd("C:/Users/coren/OneDrive/Bureau/Internship Vogelwarte/Data/R/data")

#packages:
library(dplyr)
library(ggplot2)
library(ggalluvial)

#data import:
long=read.csv("long_new.csv")
func=read.csv("otu_funcgroups.csv")

#anthropogenic food for the following otus:
long$anthropo[ long$prey == "otu1" ] <- TRUE #sunflower
long$anthropo[ long$prey == "otu146" ] <- TRUE #avena sterilis mixed with oats
long$anthropo[ long$prey == "otu2" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu321" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu478" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu738" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu40" ] <- TRUE #Maize

long <- long %>%
  mutate(
    anthropo = ifelse(anthropo == TRUE, "Anthropogenic", "Natural")
  )

func_clean <- func %>%
  mutate(Functional.group = tolower(trimws(Functionnal.group)))
fg_families <- func %>%
  select(otu_family, Functionnal.group) %>%
  distinct() %>%
  rename(fg = Functionnal.group)

fg_families <- func %>%
  count(otu_family, Functionnal.group) %>%
  group_by(otu_family) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  select(otu_family, Functionnal.group)

fg_families %>%
  count(otu_family) %>%
  filter(n > 1)

compute_FOO <- function(df, n_predators_total) {
  
  # total RA per predator
  pred_totals <- df %>%
    group_by(predator) %>%
    summarise(total_ra = sum(ra), .groups = "drop")
  
  # RA per predator per OTU
  otu_ra_pred <- df %>%
    filter(!is.na(otu_family)) %>%
    group_by(predator, otu_family, prey) %>%
    summarise(ra_otu = sum(ra), .groups = "drop") %>%
    left_join(pred_totals, by = "predator") %>%
    mutate(
      ra_rel = ra_otu / total_ra,
      pa_otu = ra_rel >= 0.001   # 0.1% threshold
    )
  
  # family presence = any OTU present
  family_pa <- otu_ra_pred %>%
    group_by(predator, otu_family) %>%
    summarise(
      pa = ifelse(all(is.na(pa_otu)), 0, max(pa_otu, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # FOO per family (fixed denominator)
  foo <- family_pa %>%
    group_by(otu_family) %>%
    summarise(
      FOO = sum(pa),
      FOO_prop = FOO / n_predators_total,
      .groups = "drop"
    )
  
  return(foo)
}

compute_pooled_RA <- function(df) {
  df %>%
    filter(!is.na(otu_family)) %>%
    group_by(otu_family) %>%
    summarise(
      pooled_RA = sum(ra, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pooled_RA_prop = pooled_RA / sum(pooled_RA))
}

build_table <- function(df, foo, pooled) {
  
  df_sites <- df %>%
    filter(!is.na(otu_family)) %>%
    group_by(site, otu_family) %>%
    summarise(ra_site = sum(ra), .groups = "drop") %>%
    group_by(site) %>%
    mutate(ra_site_rel = ra_site / sum(ra_site)) %>%
    ungroup() %>%
    left_join(fg_families, by = "otu_family")
  
  df_sites %>%
    group_by(otu_family) %>%
    summarise(
      fg = first(Functionnal.group),
      sites = paste(unique(site), collapse = ", "),
      .groups = "drop"
    ) %>%
    left_join(foo, by = "otu_family") %>%
    left_join(pooled, by = "otu_family") %>%
    arrange(desc(pooled_RA_prop))
}

foo_all <- compute_FOO(long, n_predators_total = 97)
pooled_all <- compute_pooled_RA(long)

all_birds_table <- build_table(long, foo_all, pooled_all)

pellet_totals <- long %>%
  group_by(predator) %>%
  summarise(
    total_ra = sum(ra, na.rm = TRUE),
    anthro_ra = sum(ra * (anthropo == "Anthropogenic"), na.rm = TRUE),
    prop_anthro = anthro_ra / total_ra,
    .groups = "drop"
  )

natural_predators <- pellet_totals %>%
  filter(prop_anthro < 0.01) %>%
  pull(predator)

length(natural_predators)
long_nat <- long %>%
  filter(predator %in% natural_predators)
foo_nat <- compute_FOO(long_nat, n_predators_total = length(natural_predators))
pooled_nat <- compute_pooled_RA(long_nat)
natural_birds_table <- build_table(long_nat, foo_nat, pooled_nat)
