# Baseline Reef Survey Data Analysis - Frontiers Manuscript Format
# Rosie Young | rosie.young@merton.ox.ac.uk
# Date: 2025-09-16

#---- 1. Environment Setup ----
# Restore reproducible R environment and load required packages
if (!require("renv")) install.packages("renv")
renv::restore()

required_packages <- c("tidyverse", "readxl", "janitor", "ggplot2", "ggsignif", "pheatmap", "scatterpie", "ggmap", "reshape2", "stringr")
for (pkg in required_packages) if (!require(pkg, character.only = TRUE)) install.packages(pkg)
lapply(required_packages, library, character.only = TRUE)

#---- 2. Data Import ----
# Load raw substrate composition data and clean column names
substrate_raw <- read_excel(
  "data/raw_data.xlsx",
  sheet = "Substrate composition 2023-2025",
  col_types = rep(c("date", "text", "numeric"), c(1,1,39))
) %>% clean_names()

# Load raw fish survey data and clean column names
fish_raw <- read_excel("data/raw_data.xlsx", sheet = "Fish 2024-2025") %>% clean_names()

# Load raw coral health data and clean column names
coral_health_raw <- read_excel("data/raw_data.xlsx", sheet = "Coral health 2023-2025", guess_max = 2000) %>% clean_names()

#---- 3. Substrate Data Cleaning ----
# Replace NA substrate values with 0 and remove empty rows
meta_cols <- c("date", "dive_site", "sd", "depth_start", "depth_end", "temperature_c")
substrate_raw <- substrate_raw %>%
  mutate(across(-all_of(meta_cols), ~replace_na(.x, 0))) %>%
  filter(!is.na(dive_site))

# Filter for valid transects (between 115-120 points) and correct missing point counts
substrate_filtered <- substrate_raw %>%
  filter(total_data_points >= 115 & total_data_points <= 120) %>%
  mutate(
    missing_points = 120 - total_data_points,
    unknown = unknown + missing_points,
    total_data_points = 120
  ) %>%
  select(-matches("total$|algae_prop|sp_total|co_total|others_total"))

# Summarise substrate categories (abiotic, algae, sponge, coral, other) and convert to proportion of total points
abiotic_cols <- c("si", "sa", "rb", "rk", "oc", "bc", "fc", "an", "cm", "tu", "bcm")
algae_cols   <- c("al_tf", "al_fma", "al_cca", "al_pac")
sponge_cols  <- c("sp_rp", "sp_ba", "sp_gb", "sp_en", "sp_tb", "sp_ag")
coral_cols   <- c("co_ma", "co_le", "co_br", "co_mb", "co_fl", "co_so", "co_di")

substrate_clean <- substrate_filtered %>%
  rowwise() %>%
  mutate(
    abiotic = sum(c_across(all_of(abiotic_cols))),
    algae   = sum(c_across(all_of(algae_cols))),
    sponge  = sum(c_across(all_of(sponge_cols))),
    coral   = sum(c_across(all_of(coral_cols))),
    other   = unknown
  ) %>%
  ungroup() %>%
  mutate(across(c(abiotic, algae, sponge, coral, other), ~ .x / 120, .names = "{.col}_prop"))

# Save cleaned substrate data
write_csv(substrate_clean, "data/substrate_cleaned.csv")

#---- 4. Substrate Summary Stats ----
# Calculate percent cover and site-level mean depth for each transect
substrate_clean <- substrate_clean %>%
  mutate(
    coral_pct = coral_prop * 100,
    algae_pct = algae_prop * 100,
    mean_depth = (depth_start + depth_end) / 2
  )

# Coral cover statistics
coral_stats <- substrate_clean %>%
  summarise(
    mean = mean(coral_pct, na.rm = TRUE),
    sd   = sd(coral_pct, na.rm = TRUE),
    min  = min(coral_pct, na.rm = TRUE),
    max  = max(coral_pct, na.rm = TRUE)
  )

# Algal cover statistics
algae_stats <- substrate_clean %>%
  summarise(
    mean = mean(algae_pct, na.rm = TRUE),
    sd   = sd(algae_pct, na.rm = TRUE),
    min  = min(algae_pct, na.rm = TRUE),
    max  = max(algae_pct, na.rm = TRUE)
  )

#---- 5. Substrate Figures ----
# Define a custom colour palette for sites
site_palette <- c(
  "Casablanca" = "#E69F00",
  "Buoy Line" = "#56B4E9",
  "Manuels Wall" = "#009E73",
  "Grandmas Garden" = "#F0E442",
  "Pandora" = "#0072B2"
)
substrate_clean$dive_site <- factor(substrate_clean$dive_site, levels = names(site_palette))

# Boxplot: Coral cover by site with pairwise significance comparisons
comparisons <- combn(levels(substrate_clean$dive_site), 2, simplify = FALSE)
ggplot(substrate_clean, aes(x = dive_site, y = coral_pct, fill = dive_site)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, outlier.colour = "red") +
  scale_fill_manual(values = site_palette) +
  labs(title = "Coral Cover (%) by Site", x = "Site", y = "Coral Cover (%)") +
  theme_classic(base_size = 16) +
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, step_increase = 0.07, hide_ns = TRUE)

# Boxplot: Algal cover by site with significance bars
ggplot(substrate_clean, aes(x = dive_site, y = algae_pct, fill = dive_site)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, outlier.colour = "red") +
  scale_fill_manual(values = site_palette) +
  labs(title = "Algal Cover (%) by Site", x = "Site", y = "Algal Cover (%)") +
  theme_classic(base_size = 16) +
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, step_increase = 0.07, hide_ns = TRUE)

# Stacked bar chart: Substrate composition per site
substrate_long <- substrate_clean %>%
  select(dive_site, coral_prop, algae_prop, sponge_prop, abiotic_prop, other_prop) %>%
  pivot_longer(-dive_site, names_to = "substrate", values_to = "prop") %>%
  mutate(substrate = factor(substrate, levels = c("other_prop", "algae_prop", "sponge_prop", "abiotic_prop", "coral_prop")))

ggplot(substrate_long, aes(x = dive_site, y = prop * 100, fill = substrate)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Site", y = "Percent cover", fill = "Substrate type") +
  theme_classic()

# Heatmap: Mean substrate cover by site and type
substrate_summary <- substrate_clean %>%
  group_by(dive_site) %>%
  summarise(across(c(coral_prop, algae_prop, sponge_prop, abiotic_prop, other_prop), ~mean(.x, na.rm=TRUE)*100)) %>%
  pivot_longer(-dive_site, names_to = "substrate", values_to = "percent_cover")
ggplot(substrate_summary, aes(x = substrate, y = dive_site, fill = percent_cover)) +
  geom_tile(color = "black", linewidth = 0.8) +
  scale_fill_gradient(low = "#f7fbff", high = "#D95F02", name = "Percent cover") +
  labs(x = "Substrate Type", y = "Site") +
  theme_classic(base_size = 16)


#---- 6. Fish Data Cleaning and Summaries ----
# Standardise site names, clean "n+" entries, convert fish counts to numeric, and replace NAs with 0

meta_cols_fish <- c("date", "dive_site", "depth_start", "depth_end", "temperature_c")

fish_clean <- fish_raw %>%
  filter(!is.na(dive_site)) %>%
  mutate(
    # Standardise site names (handle all common misspellings/variants)
    dive_site = str_to_lower(dive_site),
    dive_site = case_when(
      dive_site %in% c("casablanca", "casa blanca") ~ "Casablanca",
      dive_site %in% c("bouy line", "buoy line", "buoyline") ~ "Buoy Line",
      dive_site == "grandmas garden" ~ "Grandmas Garden",
      dive_site == "manuels wall" ~ "Manuels Wall",
      dive_site == "pandora" ~ "Pandora",
      TRUE ~ dive_site
    )
  ) %>%
  # Remove "+" from counts like "30+", "100+" etc
  mutate(across(-all_of(meta_cols_fish), ~gsub("\\+", "", .x))) %>%
  # Convert to numeric
  mutate(across(-all_of(meta_cols_fish), ~as.numeric(.x))) %>%
  # Replace NAs with 0
  mutate(across(-all_of(meta_cols_fish), ~replace_na(.x, 0)))

# Only keep metadata and totals (plus rugosity if present)
total_cols <- grep("^total_", names(fish_clean), value = TRUE)
rug_cols <- grep("^rug_", names(fish_clean), value = TRUE)
fish_summary <- fish_clean %>%
  select(all_of(meta_cols_fish), all_of(rug_cols), all_of(total_cols))

write_csv(fish_summary, "data/fish_summary_cleaned.csv")

# Fish Group Names 
fish_group_cols <- grep("^total_", names(fish_summary), value=TRUE)
fish_names <- c("Parrotfish", "Algal-farming Damselfish", "Grunts", "Butterflyfish", "Snappers",
                "Wrasses", "Surgeonfish", "Groupers", "Jacks", "Angelfish", "Porcupinefish",
                "Triggerfish", "Filefish", "Lionfish", "Porgys", "Chubs", "Barracudas")

site_levels <- c("Casablanca", "Buoy Line", "Manuels Wall", "Grandmas Garden", "Pandora")
fish_summary$dive_site <- factor(fish_summary$dive_site, levels = site_levels)

#---- 7. Fish Group Abundance by Site (Matrix and Heatmap) ----

# Site-by-group matrix: always includes all sites/groups, fills missing with 0
site_fish_matrix <- fish_summary %>%
  group_by(dive_site) %>%
  summarise(across(all_of(fish_group_cols), ~sum(.x, na.rm=TRUE)), .groups = "drop") %>%
  complete(dive_site = site_levels, fill = list(!!!setNames(rep(0, length(fish_group_cols)), fish_group_cols))) %>%
  arrange(dive_site)

rownames(site_fish_matrix) <- site_fish_matrix$dive_site
site_fish_matrix_mat <- as.matrix(site_fish_matrix[, fish_group_cols])
colnames(site_fish_matrix_mat) <- fish_names

# Heatmap
blue_gradient <- colorRampPalette(c("white", "#08306B"))(100)
pheatmap(
  mat = site_fish_matrix_mat,
  color = blue_gradient,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  border_color = NA,
  legend = TRUE,
  main = "Fish Group Abundance by Site",
  fontsize = 12,
  fontsize_row = 12,
  fontsize_col = 12,
  angle_col = 45,
  cellwidth = 30,
  cellheight = 40
)

# The above graph doesn't show the species with lower values, as it is so skewed towards the parrotfish and damselfish 
# So I am log transforming it so it shows when there are differences between the less common species between sites
site_fish_matrix_mat_log <- log1p(site_fish_matrix_mat) # log1p(x) = log(1 + x), safe for zeroes

blue_gradient <- colorRampPalette(c("white", "#08306B"))(100)
pheatmap(
  mat = site_fish_matrix_mat_log,
  color = blue_gradient,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  border_color = NA,
  legend = TRUE,
  main = "Fish Group Abundance by Site (log scale)",
  fontsize = 12,
  fontsize_row = 12,
  fontsize_col = 12,
  angle_col = 45,
  cellwidth = 30,
  cellheight = 40
)

# Fish Group Abundance Bar Chart 

# Overall sum (across all sites & times) for each group
group_sums <- fish_summary %>%
  summarise(across(all_of(fish_group_cols), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "group", values_to = "total_abundance") %>%
  mutate(common_name = factor(fish_names, levels = fish_names))

ggplot(group_sums, aes(x = common_name, y = total_abundance)) +
  geom_col(fill = "steelblue") +
  labs(title = "Fish Group Abundance Across All Sites", x = "Fish Group", y = "Total Count") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18))

#---- 8. Coral Health Data Cleaning and Figures ----
# Standardise coral health site names

if ("x3" %in% colnames(coral_health_raw)) {
  coral_health_raw <- coral_health_raw %>%
    rename(coral_type = x3)
}

coral_health_raw <- coral_health_raw %>%
  mutate(
    dive_site = str_to_lower(dive_site),
    dive_site = case_when(
      dive_site %in% c("casablanca", "casa blanca") ~ "Casablanca",
      dive_site %in% c("bouy line", "buoy line", "buoyline") ~ "Buoy Line",
      dive_site == "grandmas garden" ~ "Grandmas Garden",
      dive_site == "manuels wall" ~ "Manuels Wall",
      dive_site == "pandora" ~ "Pandora",
      TRUE ~ dive_site
    )
  )

# Fill down metadata and remove empty rows
coral_health_long <- coral_health_raw %>%
  fill(date, dive_site, .direction = "down") %>%
  filter(!(is.na(coral_type) & rowSums(!is.na(select(., -date, -dive_site))) == 0)) %>%
  filter(!(is.na(date) & is.na(dive_site))) %>%
  mutate(transect_id = paste(date, dive_site, sep="_"))

# Extract disease columns and convert to numeric
meta_cols_ch <- c("date", "dive_site", "coral_type", "transect_id")
disease_cols <- setdiff(names(coral_health_long), meta_cols_ch)
coral_health_long <- coral_health_long %>%
  mutate(across(all_of(disease_cols), ~ as.numeric(str_extract(as.character(.x), "^\\d+")), .names = "{.col}_num"))

# Calculate disease prevalence by site and save as CSV
disease_num_cols <- grep("_num$", names(coral_health_long), value=TRUE)
prevalence_table <- coral_health_long %>%
  group_by(dive_site) %>%
  summarise(across(all_of(disease_num_cols), ~ sum(.x > 0, na.rm = TRUE), .names = "n_{.col}"), n_transects = n()) %>%
  mutate(across(starts_with("n_"), ~ .x / n_transects, .names = "prev_{.col}"))
write_csv(prevalence_table, "data/coral_disease_prevalence_by_site.csv")

# Ensure these columns are numeric, coercing if needed
coral_health_long <- coral_health_long %>%
  mutate(
    he   = as.numeric(he),
    dg_m = as.numeric(dg_m),
    dg_h = as.numeric(dg_h),
    de   = as.numeric(de)
  )

# Prepare data for coral health status figure
coral_long <- coral_health_long %>%
  select(dive_site, coral_type, he, dg_m, dg_h, de) %>%
  pivot_longer(cols = c(he, dg_m, dg_h, de), names_to = "health_status", values_to = "abundance") %>%
  filter(abundance > 0) %>%
  group_by(dive_site, health_status) %>%
  summarise(total_abundance = sum(abundance), .groups = "drop") %>%
  mutate(
    health_status = recode(
      health_status,
      he = "Healthy",
      dg_h = "High Degradation (>50%)",
      dg_m = "Medium Degradation (10–50%)",
      de = "Dead"
    ),
    health_status = factor(health_status, levels = c("Healthy", "High Degradation (>50%)", "Medium Degradation (10–50%)", "Dead"))
  )

# Bar chart: Proportion of coral health status by site
ggplot(coral_long, aes(x = dive_site, y = total_abundance, fill = health_status)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  labs(title = "Coral Health Status Proportion by Site", x = "Dive Site", y = "Proportion", fill = "Health Status")

#---- 9. Final Data Overview ----
# Print summary stats for data overview section
cat("Substrate records:", nrow(substrate_clean), "\n")
cat("Sites:", paste(unique(substrate_clean$dive_site), collapse = ", "), "\n")
cat("Fish records:", nrow(fish_clean), "\n")
cat("Coral health records:", nrow(coral_health_long), "\n")
cat("Fish groups surveyed:", length(fish_group_cols), "\n")
cat("Dive sites:", paste(unique(fish_clean$dive_site), collapse = ", "), "\n")
cat("Unique coral types:", paste(unique(coral_health_long$coral_type), collapse = ", "), "\n")

#---- 10. Save figures ----
# Use ggsave() to save figures to "figures/" as needed.
# Example: ggsave("figures/coral_boxplot.png", width = 8, height = 6, dpi = 300)

# End of script