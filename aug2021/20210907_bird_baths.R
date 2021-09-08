#' Author: Bruno Braga Montezano
#' Subject: Tidy Tuesday 2021 - Week 36 - Bird Baths

library(magrittr)

# Import -----------------------------------------------------------------------
da <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv")

#variable 	class 	description
#survey_year 	double 	Year of survey
#urban_rural 	character 	Urban or rural location
#bioregions 	character 	Region of Australia
#bird_type 	character 	Bird species
#bird_count 	double 	

da %>% 
  tibble::glimpse()

# Tidy -------------------------------------------------------------------------

# Quantos anos foram observados, e quantos missings essa variável ano possui?
# How many years were observed, and how many missings are present in this variable?
da %>% 
  dplyr::count(survey_year)

# Quantos valores ausentes temos em cada uma das variáveis?
# How many missing values do we have in each variable of the dataset?
purrr::map(da, \(x) sum(is.na(x)))

# Considerando que temos uma base bem grande, estes 169 valores ausentes não farão falta
# Considering we have a huge dataset, we could afford leaving those 169 observations behind
da %>% 
  dplyr::filter(!is.na(survey_year)) %>% 
  purrr::map(\(x) sum(is.na(x)))

# Portanto, podemos criar nossa base de dados livre de valores ausentes
# Now, we can calmly create our NA-free subset
da <- da %>% 
  dplyr::filter(!is.na(survey_year))

# Visualize --------------------------------------------------------------------

# Com que tipo de pássaros nós estamos trabalhando?
# What bird types are we working with?
b_types <- da %>% 
  dplyr::distinct(bird_type) %>% 
  dplyr::pull(bird_type)

b_types

# Tenho número suficiente de tipos de pássaros vermelhos para estratificar em um plot?
# Do I have enough red bird types to stratify on a plot? 
b_types %>% 
  stringr::str_detect(pattern = "[r,R]ed")

# Parece que sim para os pássaros vermelhos... (bem non-sense esse check)
# Looks so for the red birds... (very non-sense check) 

# Existem mais pássaros pretos na zona urbana ou rural, proporcionalmente?
# Are there more black birds in urban or rural areas, proportionally? 
da %>%
  dplyr::mutate(black = dplyr::case_when(
    stringr::str_detect(bird_type, "[b,B]lack") ~ "Black", 
    TRUE ~ "Not black")) %>%
  dplyr::group_by(urban_rural, black) %>%
  dplyr::summarize(n = sum(bird_count), .groups = "drop") %>%
  dplyr::mutate(perc = n/sum(n) * 100)

# Gostaria de encontrar as regiões onde mais existem pássaros vermelhos, proporcionalmente)
#I'd like to find the regions where there are more red birds, proportionally 
da %>%
  dplyr::mutate(red = dplyr::case_when(
    stringr::str_detect(bird_type, "[r,R]ed") ~ 1, 
    TRUE ~ 0)) %>%
  dplyr::filter(red >= 1) %>%
  dplyr::group_by(bioregions) %>%
  dplyr::summarize(n = sum(bird_count)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(prop_of_red = n/sum(n)) %>%
  dplyr::filter(prop_of_red > 0.10) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = forcats::fct_reorder(bioregions, -prop_of_red),
    y = prop_of_red,
    fill = bioregions)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c(rep("lightcoral", 3))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::percent(prop_of_red)),
    vjust = 2,
    col = "black",
    size = 5)+
  ggplot2::labs(x = "",
    y = "Proportion of all red bird sightings",
    title = "Australian red birds sighted taking a bath",
    subtitle = "Where do we have more red birds?",
    caption = "Montezano (2021)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
    text = ggplot2::element_text(size = 12))

# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------
