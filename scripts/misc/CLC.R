# install.packages(c("dplyr","tidyr","readr"))  # if needed
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(RColorBrewer)

# Example data (replace with read_csv("your.csv"))
df <- read_csv(
  "Bezeichnun,Code_18,area_m2
AWS20,142,55432
AWS20,211,717111
AWS30,231,234801
AWS30,311,164544
AWS30,313,103140
AWS30,512,270057
AWS40,311,298278
AWS40,312,444609
AWS40,411,29656
AWS50,231,584160
AWS50,311,22903
AWS50,313,161829
AWS50,512,3650
BIOMET10,311,6741
BIOMET10,312,619289
BIOMET10,512,146513
BIOMET20,311,7230
BIOMET20,312,613395
BIOMET20,512,151918",
  show_col_types = FALSE
)

# Lookup: Code_18 -> name (EN)
clc_lut <- tibble::tribble(
  ~Code_18, ~CLC_name_en,
  "142","Sport and leisure facilities",
  "211","Non-irrigated arable land",
  "231","Pastures",
  "311","Broad-leaved forest",
  "312","Coniferous forest",
  "313","Mixed forest",
  "411","Inland marshes",
  "512","Water bodies"
)

# Helper to make safe column names (no spaces/symbols)
clean_name <- function(x){
  x |>
    tolower() |>
    gsub("[^a-z0-9]+","_", x = _) |>
    gsub("^_|_$","", x = _) |>
    gsub("__+","_", x = _)
}

# Desired column order by code
order_codes <- c("142","211","231","311","312","313","411","512")

# Prep + percent calc
clc_long <- df %>%
  mutate(Code_18 = as.character(Code_18)) %>%
  left_join(clc_lut, by = "Code_18") %>%
  mutate(clc_col = clean_name(CLC_name_en)) %>%                  # column-safe names
  mutate(clc_col = factor(clc_col,
                          levels = clean_name(clc_lut$CLC_name_en[
                            match(order_codes, clc_lut$Code_18)
                          ]))) %>%
  group_by(Bezeichnun, clc_col) %>%
  summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total_area_m2 = sum(area_m2, na.rm = TRUE),
         pct = 100 * area_m2 / total_area_m2) %>%
  ungroup()

# Wide tables with land-cover names as column headers
pct_wide <- clc_long %>%
  select(Bezeichnun, clc_col, pct) %>%
  pivot_wider(names_from = clc_col, values_from = pct, values_fill = 0) %>%
  rename_with(~paste0(.x, "_pct"), -Bezeichnun) %>%
  arrange(Bezeichnun)

area_wide <- clc_long %>%
  select(Bezeichnun, clc_col, area_m2) %>%
  pivot_wider(names_from = clc_col, values_from = area_m2, values_fill = 0) %>%
  rename_with(~paste0(.x, "_m2"), -Bezeichnun) %>%
  arrange(Bezeichnun)

# (Optional) round
pct_wide  <- pct_wide  %>% mutate(across(-Bezeichnun, ~round(.x, 2)))
area_wide <- area_wide %>% mutate(across(-Bezeichnun, ~round(.x, 0)))

# Sanity check: per Bezeichnun percentages sum ~ 100
check <- clc_long %>% group_by(Bezeichnun) %>% summarise(sum_pct = sum(pct))
print(check)

# View results
pct_wide
area_wide

# Semicolon version:
write.csv2(pct_wide,  "data/CLC/AWS_clc_pct.csv",  row.names = FALSE, fileEncoding = "UTF-8")


# 1) Melt pct_wide to long just for plotting (still “using pct_wide”)
percent_cols <- grep("_pct$", names(pct_wide), value = TRUE)

long <- pct_wide %>%
  pivot_longer(all_of(percent_cols), names_to = "class", values_to = "pct") %>%
  mutate(class_label = class |> 
           gsub("_pct$", "", x = _) |>
           gsub("_", " ", x = _) |>
           tools::toTitleCase())

# Order classes by overall share (nice, stable legend)
class_order <- long %>%
  group_by(class_label) %>% summarise(total = sum(pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>% pull(class_label)
long$class_label <- factor(long$class_label, levels = class_order)

# Palette
n_cls <- length(class_order)
pal <- if (n_cls <= 12) RColorBrewer::brewer.pal(max(3, n_cls), "Set3") else scales::hue_pal()(n_cls)
names(pal) <- class_order

# Optional: slice labels (only >=3% to avoid clutter)
long <- long %>% mutate(lbl = ifelse(pct >= 3, paste0(round(pct, 1), "%"), ""))

# 2) Faceted pie for all AWS
p_facets <- ggplot(long, aes(x = "", y = pct, fill = class_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ Bezeichnun) +
  geom_text(aes(label = lbl), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = pal, name = "Land cover") +
  labs(title = "CLC share per AWS (from pct_wide)", x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))

print(p_facets)
