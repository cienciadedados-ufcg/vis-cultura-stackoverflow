library(tidyverse)

so = read_csv(here::here("data/raw/stackoverflow.csv"))
su = read_csv(here::here("data/raw/superuser.csv"))

ambos = bind_rows(
    "StackOverflow" = so, 
    "SuperUser" = su,
    .id = "site"
) %>% 
    select(
        site,
        country, 
        PDI, 
        IDV, 
        MAS, 
        UAI, 
        usuarios = num_u, 
        responderam_prop = perc_a, 
        perguntaram_prop = perc_q, 
        editaram_prop = perc_tp_e,
        comentaram_prop = perc_tp_c,
        GNI, Internet, EPI
    )

geographies = read_csv(here::here("data/raw/Data Geographies - v1 - by Gapminder - List of countries.csv"))

ambos %>% 
    left_join(geographies, by = c("country" = "name")) %>% 
    select(-members_oecd_g77, -`World bank region`, -`UN member since`) %>% 
    write_csv(here::here("data/participation-per-country.csv"))
