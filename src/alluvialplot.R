setwd("C:/Users/coren/OneDrive/Bureau/Internship Vogelwarte/Data/R/data")

library(dplyr)
library(ggplot2)
library(ggalluvial)

long=read.csv("long_new.csv")
unique(long$otu_order)
unique(long$otu_order)

#Alluvial diagram (site / order / anthropogenic)

long$anthropo[ long$prey == "otu1" ] <- TRUE #sunflower
long$anthropo[ long$prey == "otu146" ] <- TRUE #avena sterilis mixed with oats
long$anthropo[ long$prey == "otu2" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu321" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu478" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu738" ] <- TRUE #C. sativa
long$anthropo[ long$prey == "otu40" ] <- TRUE #Maize


#also adding the functional groups:
func=read.csv("otu_funcgroups.csv")


func_clean <- func %>%
  mutate(Functional.group = tolower(trimws(Functionnal.group)))

#then adding the long$prey (otu number) to see if they are shrubs / tree etc

df <- long %>%
  
  # keep ALL orders, including Unclassified
  filter(!is.na(otu_family)) %>%
  
  mutate(
    otu_family = ifelse(otu_family == "Unclassified", "Unclassified", otu_family),
    
    anthropo = ifelse(anthropo == TRUE,
                      "Anthropogenic",
                      "Natural")
  ) %>%
  
  group_by(site, otu_family, anthropo) %>%
  summarise(abundance = sum(ra), .groups = "drop") %>%
  
  group_by(site) %>%
  mutate(abundance = abundance / sum(abundance)) %>%  # relative abundance
  ungroup()


fg_families <- func %>%
  distinct(otu_family, Functionnal.group)



fg_families #ok

df2 <- df %>%
  left_join(fg_families, by = "otu_family")
unique(fg_families$Functionnal.group)

fg_order <- c(
  "fern",
  "aquatic forb",
  "forb",
  "shrub",
  "Tree / shrub",
  "tree",
  "graminoids",
  "Unknown"
)

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


df2 <- df %>%
  left_join(fg_families, by = "otu_family")

df2
#top 15 otu_families
top15_families <- df2 %>%
  group_by(otu_family) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  arrange(desc(total_abundance)) %>%
  slice(1:15) %>%
  pull(otu_family)

df_top15 <- df2 %>%
  filter(otu_family %in% top15_families)
df_top15
fg_order <- c(
  "fern",
  "aquatic forb",
  "forb",
  "shrub",
  "Tree/shrub",
  "tree",
  "graminoids",
  "Unknown"
)
df_top15$fg <- factor(df_top15$Functionnal.group, levels = fg_order)


family_levels <- df_top15 %>%
  distinct(otu_family, Functionnal.group) %>%
  arrange(Functionnal.group, otu_family) %>%
  pull(otu_family)

df_top15$otu_family <- factor(df_top15$otu_family, levels = family_levels)
View(df_top15)


#plotting
ggplot(df_top15,
       aes(axis1 = site, axis2 = otu_family, y = abundance)) +
  
  geom_alluvium(aes(fill = otu_family),
                width = 0.15,
                alpha = 0.8) +
  
  geom_stratum(width = 0.3,
               fill = "grey90",
               color = "black") +
  
  # ✅ smarter labels
  geom_text(
    stat = "stratum",
    aes(label = case_when(
      after_stat(x) == 1 ~ as.character(after_stat(stratum)),  # sites
      after_stat(x) == 2 & after_stat(y) > 0.05 ~ as.character(after_stat(stratum)),  # major orders only
      TRUE ~ ""
    )),
    size = 3
  ) +
  
  scale_x_discrete(
    limits = c("Site", "Family"),
    expand = c(0.2, 0.2)   # ✅ spacing between columns
  ) +
  
  scale_fill_viridis_d() +
  
  labs(y = "Relative abundance",
       x = NULL,
       fill = "Family") +
  
  theme_minimal()

#adding anthropo

library(dplyr)


###same but in sankey plot


df <- long %>%
  
  # keep ALL orders, including Unclassified
  filter(!is.na(otu_order)) %>%
  
  mutate(
    otu_order = ifelse(otu_order == "Unclassified", "Unclassified", otu_order),
    
    anthropo = ifelse(anthropo == TRUE,
                      "Anthropogenic",
                      "Natural")
  ) %>%
  
  group_by(site, otu_order, anthropo) %>%
  summarise(abundance = sum(ra), .groups = "drop") %>%
  
  group_by(site) %>%
  mutate(abundance = abundance / sum(abundance)) %>%  # relative abundance
  ungroup()
order_levels <- sort(unique(df$otu_order))
order_levels

df$otu_order <- factor(df$otu_order, levels = order_levels)
df <- df %>%
  mutate(
    anthropo = ifelse(is.na(anthropo), "Natural", anthropo)
  )
df$anthropo

fg_order <- c(
  "fern",
  "aquatic forb",
  "forb",
  "shrub",
  "Tree/shrub",
  "tree",
  "graminoids",
  "Unknown"
)

df_top15$fg <- factor(df_top15$fg, levels = fg_order)

family_levels <- df_top15 |>
  distinct(otu_family, fg) |>
  arrange(fg, otu_family) |>
  pull(otu_family)

df_top15$otu_family <- factor(df_top15$otu_family, levels = family_levels)

#rra per samples! doing it over again.
# Start from long table with sample_id, site, anthropo, otu_family, abundance

df_rra <- long |>
  group_by(predator) |>
  mutate(rra = ra / sum(ra, na.rm = TRUE)) |>
  ungroup()

df_rra <- df_rra |>
  left_join(fg_families, by = "otu_family")

names(df_rra)

df_family_rra <- df_rra |>
  group_by(site, otu_family, anthropo, Functionnal.group) |>
  summarise(rra = sum(rra, na.rm = TRUE), .groups = "drop")

top15_families <- df_family_rra |>
  group_by(otu_family) |>
  summarise(total_rra = sum(rra), .groups = "drop") |>
  arrange(desc(total_rra)) |>
  slice(1:15) |>
  pull(otu_family)


df_top15 <- df_family_rra |>
  filter(otu_family %in% top15_families)

df_top15 <- df_top15 |>
  mutate(
    anthropo = case_when(
      is.na(anthropo) ~ FALSE,
      TRUE ~ anthropo
    )
  )

df_top15$Functionnal.group <- factor(df_top15$Functionnal.group , levels = sort(unique(df_top15$Functionnal.group )))
family_levels <- df_top15 |>
  distinct(otu_family, Functionnal.group) |>
  arrange(Functionnal.group, otu_family) |>
  pull(otu_family)

df_top15$otu_family <- factor(df_top15$otu_family, levels = family_levels)
library(ggplot2)
library(ggalluvial)

main_plot <- ggplot(df_top15,
                    aes(axis1 = anthropo,
                        axis2 = site,
                        axis3 = otu_family,
                        y = rra)) +
  
  geom_alluvium(aes(fill = anthropo),
                width = 0.12,
                alpha = 0.85) +
  
  geom_stratum(width = 0.15,
               fill = "grey",
               color = "#697D96") +
  
  geom_text(
    stat = "stratum",
    aes(label = case_when(
      after_stat(x) == 1 ~ as.character(after_stat(stratum)),
      after_stat(x) == 2 ~ as.character(after_stat(stratum)),
      after_stat(x) == 3 & after_stat(y) > 0.05 ~ as.character(after_stat(stratum)),
      TRUE ~ ""
    )),
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "#d07574",
      "FALSE" = "#75d175"
    ),
    drop = FALSE
  ) +
  
  scale_x_discrete(
    limits = c("OTU Origin", "Site", "OTU Family"),
    expand = c(0.2, 0.2)
  ) +
  
  labs(y = "Relative Read Abundance",
       fill = "Origin") +
  
  theme_minimal()

main_plot
####plot without legends
main_plot <- ggplot(df_top15,
                    aes(axis1 = anthropo,
                        axis2 = site,
                        axis3 = otu_family,
                        y = rra)) +
  
  geom_alluvium(aes(fill = anthropo),
                width = 0.12,
                alpha = 0.85) +
  
  geom_stratum(width = 0.25,
               fill = "grey80",
               color = "black") +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "#d07574",
      "FALSE" = "#75d175"
    ),
    drop = FALSE
  ) +
  
  scale_x_discrete(
    limits = c("OTU Origin", "Site", "OTU Family"),
    expand = c(0.2, 0.2)
  ) +
  
  labs(x = NULL, y = NULL, fill = NULL) +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )

main_plot






table(df_top15$anthropo, useNA = "ifany")

main_plot <- ggplot(df_top15,
                    aes(axis1 = anthropo,
                        axis2 = site,
                        axis3 = otu_family,
                        y = abundance)) +
  
  geom_alluvium(aes(fill = anthropo),
                width = 0.12,
                alpha = 0.85) +
  
  geom_stratum(width = 0.15,
               fill = "grey",
               color = "#697D96") +
  
  geom_text(
    stat = "stratum",
    aes(label = case_when(
      after_stat(x) == 1 ~ as.character(after_stat(stratum)),  
      after_stat(x) == 2 ~ as.character(after_stat(stratum)),  
      after_stat(x) == 3 & after_stat(y) > 0.05 ~ as.character(after_stat(stratum)),  
      TRUE ~ ""
    )),
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "Anthropogenic" = "#d07574",
      "Natural" = "#75d175"
    ),
    drop = FALSE   # ✅ keeps legend consistent even if one category missing
  ) +
  
  scale_x_discrete(
    limits = c("OTU Origin", "Site", "OTU Family"),
    expand = c(0.2, 0.2)
  ) +
  
  labs(y = "Relative Read Abundance",
       fill = "Origin") +
  
  theme_minimal()
main_plot

#different colors

df <- df %>%
  group_by(anthropo) %>%
  mutate(
    grad_id = as.numeric(rank(abundance, ties.method = "first")),
    grad_id = grad_id / max(grad_id)   # scale 0–1
  ) %>%
  ungroup()


# Anthropogenic: brown → orange
anthro_cols <- colorRampPalette(c("#8c510a", "#d07574"))(100)

# Natural: light green → dark green
natural_cols <- colorRampPalette(c("#E9FEE7", "#0A4403"))(100)

# Assign each row a color
df <- df %>%
  mutate(
    flow_col = case_when(
      anthropo == "Anthropogenic" ~ anthro_cols[ceiling(grad_id * 99) + 1],
      anthropo == "Natural" ~ natural_cols[ceiling(grad_id * 99) + 1]
    )
  )

library(ggplot2)
library(ggalluvial)

ggplot(df,
       aes(axis1 = anthropo,
           axis2 = site,
           axis3 = otu_order,
           y = abundance)) +
  
  # -------------------------
# BASE LAYER (LEFT COLORS)
# -------------------------
geom_alluvium(
  aes(fill = anthropo),
  width = 0.12,
  alpha = 0.9
) +
  
  # -------------------------
# OVERLAY (RIGHT GRADIENT)
# -------------------------
geom_alluvium(
  aes(fill = flow_col),
  width = 0.12,
  alpha = 0.6
) +
  
  geom_stratum(
    width = 0.28,
    fill = "#C4DCFF",
    color = "#697D96"
  ) +
  
  scale_fill_manual(
    values = c(
      # LEFT colors
      "Anthropogenic" = "#d07574",
      "Natural" = "#75d175",
      
      # RIGHT gradients
      setNames(df$flow_col, df$flow_col)
    ),
    guide = "none"
  ) +
  
  labs(y = "Relative Read Abundance") +
  
  theme_minimal()

#LEFT PLOT
left_plot <- ggplot(df,
                    aes(axis1 = anthropo,
                        axis2 = site,
                        y = abundance)) +
  
  geom_alluvium(aes(fill = anthropo),
                width = 0.12,
                alpha = 0.9) +
  
  geom_stratum(width = 0.3,
               fill = "grey90",
               color = "black") +
  
  scale_fill_manual(values = c(
    "Anthropogenic" = "#d07574",
    "Natural" = "#75d175"
  )) +
  
  scale_x_discrete(limits = c("Origin", "Site"),
                   expand = c(0.2, 0.2)) +
  
  theme_minimal()
left_plot

right_plot <- ggplot(df,
                     aes(axis1 = site,
                         axis2 = otu_order,
                         y = abundance)) +
  
  geom_alluvium(aes(fill = flow_col),
                width = 0.12,
                alpha = 0.9) +
  
  geom_stratum(width = 0.3,
               fill = "grey90",
               color = "black") +
  
  scale_fill_identity() +   # ✅ crucial for custom colors
  
  scale_x_discrete(limits = c("Site", "Order"),
                   expand = c(0.2, 0.2)) +
  
  theme_minimal()

right_plot
