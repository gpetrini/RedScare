library(tidyverse)
library(tidygeocoder)
library(tabulizer)
library(geodist)
library(data.table)

capitals <- maps::world.cities %>%
  filter(capital == "1") %>%
  select(country.etc, name) %>%
  # long and lat will be used in osm method
  rename(pais = country.etc, capital = name) %>%
  geocode(city = capital, country = pais, method = "osm", lat = latitude, long = longitude) %>%
  mutate(ISO3C = countrycode::countrycode(pais, origin = "country.name", destination = "iso3c", warn = FALSE))

data.table::fwrite(capitals, file = "./clean/coord_capitais.csv")

capitais <- data.table::fread(file = "./clean/coord_capitais.csv")

conflicts <- conflicts %>%
  rename(Year = V1, Country = V2, Proxy = V3) %>%
  mutate(ISO3C = countrycode::countrycode(Country, origin = "country.name", destination = "iso3c", warn = FALSE)) %>%
  left_join(capitais) %>%
  mutate(
    pais = # Keep countries not found in dataset
    pais %>%
      is.na() %>%
      ifelse(Country, pais)
  ) %>%
  mutate(ISO3C = replace_na(ISO3C, "404"))

# Pensei em algo nesse sentido para colocar ISO3 faltantes (não rodei ainda)
## conflicts <- conflicts %>%
##   mutate(ISO3C = case_when(
##            pais == "East German" ~ "GERe",
##            pais == "Czechoslovakia" ~ "oCZE",
##            pais == "North and South Korea" ~ "South Korea",
##            TRUE ~ ISO3C # Não sei se faz isso, mas a ideia dessa linha é manter todo o restante como antes
##          ))

# Acho que assim daria para substituir o valor das coordenadas da Koreia do Norte (não testei)
## conflicts %>% filter(pais == "North Korea") %>% select(latitude) <- Val_Latitude
## conflicts %>% filter(pais == "North Korea") %>% select(longitude) <- Val_Longitude

proxies <- conflicts %>%
  filter(ISO3C == "404") %>%
  mutate(capital = Proxy) %>%
  # Keep countries not found in dataset
  select(-c(latitude, longitude)) %>%
  geocode(city = capital, method = "osm", lat = latitude, long = longitude)

## Atenção: Aqui você precisa importar os dados conflicts.csv. Vou deixar comentado.

## conflitcs <- data.table::fread(file = "./clean/conflicts.csv")
conflicts <- conflicts %>%
  filter(ISO3C != "404") %>%
  bind_rows(proxies) %>%
  arrange(Year) %>%
  mutate(
    name = paste0(capital, "_", Year %>% as.character() %>% str_sub(start = -2))
  ) %>%
  mutate(name = str_replace_all(name, " ", ""))


data.table::fwrite(conflicts, file = "./clean/coord_conflitos_capitais.csv")

capitais <- data.table::fread(file = "./clean/coord_capitais.csv") %>% arrange(ISO3C)
conflitos <- data.table::fread(file = "./clean/coord_conflitos_capitais.csv") %>% arrange(ISO3C)

repeticoes <- conflitos %>%
  group_by(name) %>%
  group_size() # Equivalente ao Cr do artigo (vezes que ocorreu)

df <- matrix(
  nrow = capitais$longitude %>% length(),
  ncol = conflitos$name %>% unique() %>% length()
) %>%
  as.data.frame()


names(df) <- conflitos$name %>% unique()
df$ISO <- capitais$ISO3
df <- df[, c("ISO", conflitos$name)] %>%
  arrange(ISO)
df <- df %>% pivot_longer(!ISO, names_to = "Origem")

distancia <- function(method = "geodesic", Destino, Origem) {
  geodist::geodist(
    x = capitais %>% filter(ISO3C == Destino) %>% select(latitude, longitude),
    y = conflitos %>% filter(name == Origem) %>% select(latitude, longitude),
    measure = method
    ## Nfrom = col_df %>% filter(name == matrix_col) %>% select(latitude) %>% as.numeric(), # latitude of origin
    ## Efrom = col_df %>% filter(name == matrix_col) %>% select(longitude) %>% as.numeric(), # latitude of origin
    ## Nto = row_df %>% filter(ISO3C == matrix_row) %>% select(latitude) %>% as.numeric(), # latitude of origin
    ## Eto = row_df %>% filter(ISO3C == matrix_row) %>% select(longitude) %>% as.numeric(), # latitude of origin
    ## units = units
  )[1]
}

## start <- sum(df$ISO == "") + 1
## for(i in start:nrow(df)){
##   for(j in 2:ncol(df)){
##     df[i,j] = distancia(
##       matrix_col = names(df)[j],
##       matrix_row = df$ISO[i]
##         )
##   }
## }

df <- data.table(df)
df[, Distancia := distancia(Origem = Origem, Destino = ISO), by = 1:nrow(df)]
df <- df %>% as.data.frame()

df <- df %>%
  distinct() %>%
  pivot_wider(names_from = Origem, values_from = Distancia)

data.table::fwrite(df, file = "./raw/distancia_geodesica.csv")