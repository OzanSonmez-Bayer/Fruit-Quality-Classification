# load the required packages -------------------------------------------------------------
dir <- "/repos/Fruit-Quality-Classification/" # adjust accordingly
source(paste0(dir, "Models/packages.R"))

# load the rater data --------------------------------------------------------------------
df <- read_xlsx("/mnt/marketability_merged_labels_w_digital_phenotypes.xlsx", sheet = 1)

# data cheks (QC) ------------------------------------------------------------------------

table(df$rater) # we do not have the same number of ratings
table(df$label) # some labels are not uniform across raters
table(df$rating_system) # not same number of rating systmes either

# number of images got rated and matched with digital Phenotypes ---------------------------
# -- Is the main reason here matching or not being rated? 
#   need to investigate that as if it is not matching we need to define some rules around it. 
df %>% 
  group_by(rater, rating_system) %>%
  summarise(N_image = n_distinct(Image),
            N_dig.Obj = n_distinct(digital_object))

# check label consistency ------------------------------------------------------------------

#-1 Three category rating ---- This seems consistent -----------
df %>%
  filter(rating_system == "three") %>%
  .$label %>%
  table()

#-2 Two category rating ---- Non-marketable vs Non-Marketable ----
df %>%
  filter(rating_system == "two") %>%
  .$label %>%
  table()

df %>%
  filter(label == "Non-marketable") %>%
  .$rater %>%
  table()

# Correct of the lower/upper case -------------------------------
df <- df %>%
  mutate(label = ifelse(label == "Non-marketable", "Non-Marketable", label))


# For now take a look at the peppers that are commonly rated and discard O'wise
# 3178 - 2922 = 256 objects are discarded for two category ratings
df_two <- df %>%
  filter(rating_system == "two") %>%
  group_by(digital_object) %>%
  mutate(N = n()) %>%
  filter(N == 3) %>%
  ungroup() %>%
  select(-N)

# 3200 - 2985= 215 lines are discarded
df_three <- df %>%
  filter(rating_system == "three") %>%
  group_by(digital_object) %>%
  mutate(N = n()) %>%
  filter(N == 3) %>%
  ungroup() %>%
  select(-N)


df_two %>%
  mutate(rating = ifelse(label == "Marketable", 1, 0)) %>%
  group_by(digital_object) %>%
  summarise(avg_r = mean(rating)) %>%
  .$avg_r %>%
  hist()
