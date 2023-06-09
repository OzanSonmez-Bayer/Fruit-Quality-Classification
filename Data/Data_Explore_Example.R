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
  select(-N) %>%
  mutate(rating = ifelse(label == "Marketable", 1, 0))


df_two %>%
  group_by(digital_object) %>%
  summarise(avg_r = mean(rating)) %>%
  .$avg_r %>%
  hist()

# 100% agrrement
df_two %>%
  group_by(digital_object) %>%
  summarise(avg_r = mean(rating),
            agree = ifelse(avg_r %in% c(0,1), 1, 0)) %>%
  .$agree %>%
  table() %>%
  prop.table()

# individual rater ratings agreement
(df_two %>%
    ggplot(., aes(digital_object, rater, fill = rating)) + 
    geom_tile(color = "black") +
    scale_fill_gradient(low = "lightblue", high = "red") +
    theme_minimal() +
    ggtitle("Ratings (Marketable = 1) per Rater per Pepper") +
    xlab("Pepper (Barcode)") +
    ylab("")) %>%
  ggplotly()


df_two %>%
  filter(rater == "Ozan") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Ozan")

df_two %>%
  filter(rater == "Kyle") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Kyle")

df_two %>%
  filter(rater == "Natalie") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Natalie")


# 3200 - 2985 = 215 lines are discarded
df_three <- df %>%
  filter(rating_system == "three") %>%
  group_by(digital_object) %>%
  mutate(N = n()) %>%
  filter(N == 3) %>%
  ungroup() %>%
  select(-N) %>%
  mutate(rating = case_when(
    label == "Quality - Poor" ~ 0, 
    label == "Quality - Average" ~ 0.5,
    TRUE ~ 1
  ))


df_three %>%
  group_by(digital_object) %>%
  summarise(avg_r = mean(rating)) %>%
  .$avg_r %>%
  hist()

# 100% agrrement
df_three %>%
  group_by(digital_object) %>%
  summarise(avg_r = mean(rating),
            agree = ifelse(avg_r <= 0.2 | avg_r >= 0.8, 1, 0)) %>%
  .$agree %>%
  table() %>%
  prop.table()

# individual rater ratings agreement
(df_three %>%
    ggplot(., aes(digital_object, rater, fill = rating)) + 
    geom_tile(color = "black") +
    scale_fill_gradient(low = "lightblue", high = "red") +
    theme_minimal() +
    ggtitle("Ratings (Marketable = 1) per Rater per Pepper") +
    xlab("Pepper (Barcode)") +
    ylab("")) %>%
  ggplotly()


df_three %>%
  filter(rater == "Ozan") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Ozan")

df_three %>%
  filter(rater == "Kyle") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Kyle")

df_three %>%
  filter(rater == "Natalie") %>%
  select(-c(Image, digital_object, label_object, , Box_x.x, Box_y.x, Box, Box_x.y, Box_y.y,
            Size1, Size2, label_sequence, rating_system, rating, rater))%>%
  gather("Trait", "Value", -label) %>%
  ggplot(., aes(x = Value, fill = label)) +
  geom_density() +
  facet_wrap(~Trait, scales = "free", ncol = 6) +
  theme_bw() +
  ggtitle("Trait Distribution - rated by Natalie")

# --------------------------------------------------
df_dif <- df_two %>%
  bind_rows(df_three) %>%
  group_by(rating_system, digital_object) %>%
  summarise(mean_rating = mean(rating)) %>%
  spread(rating_system, mean_rating) %>%
  mutate(dif = abs(three-two)) 

df_dif %>%
  ggplot(., aes(x = dif)) +
  geom_histogram(fill = "lightblue") +
  theme_minimal()

df_dif %>%
  mutate(agree = ifelse(dif < 0.4, 1, 0)) %>%
  .$agree %>%
  table() %>%
  prop.table()

# disagreement on the mean rating cat 2 vs 3 larger than 0.4
df_dif_great = df_dif %>%
  filter(dif > 0.4)

df_two %>%
  bind_rows(df_three) %>%
  filter(digital_object %in% df_dif_great$digital_object) %>%
  select(digital_object, label, rating_system, rater) %>%
  spread(rating_system, label) %>% datatable()



