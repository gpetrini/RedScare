library(tidyverse)
library(tidygeocoder)
library(tabulizer)
library(geodist)
library(data.table)

capitals <-maps::world.cities %>%
  filter(capital == "1") %>%
  select(country.etc, name) %>% # long and lat will be used in osm method
  rename(pais = country.etc, capital = name) %>%
  geocode(city = capital, country = pais, method = 'osm', lat = latitude, long = longitude) %>%
  mutate(ISO3C = countrycode::countrycode(pais, origin ='country.name',destination ='iso3c', warn = FALSE))

data.table::fwrite(capitals, file = "./clean/coord_capitais.csv")

conflicts <- local({
     con <- textConnection(
       "\"1945\"	\"Poland\"		\"Europa\"
\"1945\"	\"Austria\"		\"Europa\"
\"1946\"	\"North Korea\"		\"Asia\"
\"1946\"	\"China\"		\"Asia\"
\"1946\"	\"Greece\"		\"Europa\"
\"1947\"	\"Bulgaria\"		\"Europa\"
\"1948\"	\"Czechoslovakia\"	\"Prague\"	\"Europa\"
\"1948\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1948\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1948\"	\"China\"		\"Asia\"
\"1949\"	\"China\"		\"Asia\"
\"1950\"	\"South Korea\"		\"Asia\"
\"1950\"	\"China\"		\"Asia\"
\"1950\"	\"North and South Korea\"	\"South Korea\"	\"Asia\"
\"1953\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1954\"	\"Vietinam\"		\"Asia\"
\"1954\"	\"Vietinam\"		\"Asia\"
\"1955\"	\"North and South Korea\"	\"South Korea\"	\"Asia\"
\"1956\"	\"Poland\"		\"Europa\"
\"1956\"	\"Hungary\"		\"Europa\"
\"1958\"	\"Taiwan\"		\"Asia\"
\"1959\"	\"Cuba\"		\"Americas\"
\"1960\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1961\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1962\"	\"Laos\"		\"Asia\"
\"1962\"	\"Cuba\"		\"Americas\"
\"1968\"	\"Vietinam\"		\"Asia\"
\"1968\"	\"Czechoslovakia\"	\"Prague\"	\"Europa\"
\"1969\"	\"North Korea\"		\"Asia\"
\"1969\"	\"Libya\"		\"Africa\"
\"1970\"	\"Cambodia\"		\"Asia\"
\"1975\"	\"Cambodia\"		\"Asia\"
\"1975\"	\"Vietinam\"		\"Asia\"
\"1975\"	\"Laos\"		\"Asia\"
\"1978\"	\"Afghanistan\"		\"Asia\"
\"1979\"	\"Nicaragua\"		\"Americas\"
\"1979\"	\"El Salvador\"		\"Americas\"
\"1979\"	\"Afghanistan\"		\"Asia\"
\"1981\"	\"Poland\"		\"Europa\"
\"1989\"	\"China\"		\"Asia\""
     )
     res <- utils::read.table(
       con,
       header    = FALSE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })

capitais <- data.table::fread(file = "./clean/coord_capitais.csv")

conflicts <- conflicts %>%
  rename(Year = V1, Country = V2, Proxy = V3, Continente = V4) %>%
  mutate(ISO3C = countrycode::countrycode(Country, origin ='country.name',destination ='iso3c', warn = FALSE)) %>%
  left_join(capitais) %>%
  mutate(pais = # Keep countries not found in dataset
           pais %>%
             is.na %>%
             ifelse(Country, pais) ) %>%
  mutate(ISO3C = replace_na(ISO3C, "404")) %>%
  mutate(Continente = factor(Continente))


## proxies <-conflicts %>% filter(ISO3C == "404") %>%
##   mutate(capital = case_when())
##   mutate(capital = Proxy) %>% # Keep countries not found in dataset
##   ## select(-c(latitude, longitude)) %>%
##   geocode(city = capital, method = 'osm', lat = latitude, long = longitude)

conflicts <- conflicts %>%
  mutate(capital = case_when(
         ISO3C == "404" ~ Proxy,
         TRUE ~ capital)) %>%
  mutate(ISO3C = case_when(
           pais == "East Germany" ~ "GERe",
           pais == "Czechoslovakia" ~ "oCZE",
           pais == "North and South Korea" ~ "PRK",
           TRUE ~ ISO3C # Não sei se faz isso, mas a ideia dessa linha é manter todo o restante como antes
         )) %>%
  mutate(pais = case_when( # Atualizando nome dos países para preencher coordenadas faltantes
           pais == "East Germany" ~ "Germany",
           pais == "Czechoslovakia" ~ "Czech Republic",
           pais == "North and South Korea" ~ "North Korea",
           TRUE ~ pais
         )) %>%
  select(-c(latitude, longitude)) %>%
  geocode(city = capital, country = pais, method = 'osm', lat = latitude, long = longitude)
  
    
conflicts <- conflicts %>%
  mutate(latitude  = case_when(
           ISO3C == "PRK" ~ 39.032, # https://pt.db-city.com/Coreia-do-Norte--Pyongyang
           TRUE ~ as.numeric(latitude)))
conflicts <- conflicts %>%
  mutate(longitude  = case_when(
           ISO3C == "PRK" ~ 125.75, # https://pt.db-city.com/Coreia-do-Norte--Pyongyang
           TRUE ~ as.numeric(longitude)))


conflicts <- conflicts %>%
  arrange(Year) %>%
  mutate(
    name = paste0(capital, "_", Year %>% as.character() %>% str_sub(start=-2), "_", Continente)
  ) %>%
  mutate(name = str_replace_all(name, " ", ""))
    

data.table::fwrite(conflicts, file = "./clean/coord_conflitos_violentos.csv")

capitais <- data.table::fread(file = "./clean/coord_capitais.csv") %>% arrange(ISO3C)
conflitos <- data.table::fread(file = "./clean/coord_conflitos_violentos.csv") %>% arrange(ISO3C)

repeticoes <- conflitos %>% group_by(name) %>% group_size() # Equivalente ao Cr do artigo (vezes que ocorreu)

df <- matrix(
  nrow = capitais$longitude %>% length(),
  ncol = conflitos$name %>% unique() %>% length()
) %>%
  as.data.frame()


names(df) <- conflitos$name %>% unique()
df$ISO <- capitais$ISO3
df <- df[,c("ISO", conflitos$name)] %>%
  arrange(ISO)
df <- df %>% pivot_longer(!ISO,names_to = "Origem")

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
  )[1]/1000 %>% as.numeric()
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

df <- data.table::fread("./raw/distancia_geodesica.csv") %>% select(!c(value))
df <- df %>% # Remove colunas que contêm apenas NA
  select(
    where(
      ~!all(is.na(.x))
    )
  )
    
continentes <- c(
  "Africa",
  "Americas",
  "Asia",
  "Europa"
    )

for(continente in continentes){
  subset <- str_subset(names(df), continente)
  df <- df %>%
    drop_na(any_of(subset)) %>%
    mutate("Violentos_{continente}" := rowMeans(across(all_of(subset))))
  
}
    
    
subset <- str_subset(names(df), "Violentos")
df <- df %>% select(ISO, all_of(subset)) %>%
    mutate("Violentos" := rowMeans(across(all_of(subset))))

data.table::fwrite(df, file = "./clean/conflitos_violentos_continentes.csv")

conflicts <- local({
     con <- textConnection(
       "\"1945\"	\"Czechoslovakia\"	\"Prague\"	\"Europa\"
\"1945\"	\"Bulgaria\"		\"Europa\"
\"1945\"	\"Yugoslavia\"	\"Belgrade\"	\"Europa\"
\"1945\"	\"Romania\"		\"Europa\"
\"1947\"	\"Poland\"		\"Europa\"
\"1947\"	\"Hungary\"		\"Europa\"
\"1947\"	\"Bulgaria\"		\"Europa\"
\"1947\"	\"Romania\"		\"Europa\"
\"1948\"	\"North Korea\"		\"Asia\"
\"1948\"	\"Hungary\"		\"Europa\"
\"1948\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1949\"	\"USSR\"	\"Moscow\"	\"Europa\"
\"1949\"	\"North Korea\"		\"Asia\"
\"1949\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1949\"	\"China\"		\"Asia\"
\"1952\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1954\"	\"East Germany\"	\"Bonn\"	\"Europa\"
\"1960\"	\"Cuba\"		\"Americas\"
\"1960\"	\"Cuba\"		\"Americas\"
\"1970\"	\"Chile\"		\"Americas\"
\"1976\"	\"Vietinam\"		\"Asia\"
\"1977\"	\"Ethiopia\"		\"Africa\""
     )
     res <- utils::read.table(
       con,
       header    = FALSE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })

capitais <- data.table::fread(file = "./clean/coord_capitais.csv")

conflicts <- conflicts %>%
  rename(Year = V1, Country = V2, Proxy = V3, Continente = V4) %>%
  mutate(ISO3C = countrycode::countrycode(Country, origin ='country.name',destination ='iso3c', warn = FALSE)) %>%
  left_join(capitais) %>%
  mutate(pais = # Keep countries not found in dataset
           pais %>%
             is.na %>%
             ifelse(Country, pais) ) %>%
  mutate(ISO3C = replace_na(ISO3C, "404")) %>%
  mutate(Continente = factor(Continente))



conflicts <- conflicts %>%
  mutate(capital = case_when(
         ISO3C == "404" ~ Proxy,
         TRUE ~ capital)) %>%
  mutate(ISO3C = case_when(
           pais == "East Germany" ~ "GERe",
           pais == "Czechoslovakia" ~ "oCZE",
           pais == "North and South Korea" ~ "PRK",
           pais == "Yugoslavia" ~ "YUG",
           pais == "USSR" ~ "USSR",
           TRUE ~ ISO3C # Não sei se faz isso, mas a ideia dessa linha é manter todo o restante como antes
         )) %>%
  mutate(pais = case_when( # Atualizando nome dos países para preencher coordenadas faltantes
           pais == "East Germany" ~ "Germany",
           pais == "Czechoslovakia" ~ "Czech Republic",
           pais == "North and South Korea" ~ "North Korea",
           pais == "Yugoslavia" ~ "Serbia",
           pais == "USSR" ~ "Russia",
           TRUE ~ pais
         )) %>%
  select(-c(latitude, longitude)) %>%
  geocode(city = capital, country = pais, method = 'osm', lat = latitude, long = longitude)


conflicts <- conflicts %>%
  mutate(latitude  = case_when(
           ISO3C == "PRK" ~ 39.032, # https://pt.db-city.com/Coreia-do-Norte--Pyongyang
           TRUE ~ as.numeric(latitude)))
conflicts <- conflicts %>%
  mutate(longitude  = case_when(
           ISO3C == "PRK" ~ 125.75, # https://pt.db-city.com/Coreia-do-Norte--Pyongyang
           TRUE ~ as.numeric(longitude)))


conflicts <- conflicts %>%
  arrange(Year) %>%
  mutate(
    name = paste0(capital, "_", Year %>% as.character() %>% str_sub(start=-2), "_", Continente)
  ) %>%
  mutate(name = str_replace_all(name, " ", ""))


data.table::fwrite(conflicts, file = "./clean/coord_conflitos_nao_violentos.csv")

capitais <- data.table::fread(file = "./clean/coord_capitais.csv") %>% arrange(ISO3C)
conflitos <- data.table::fread(file = "./clean/coord_conflitos_nao_violentos.csv") %>% arrange(ISO3C)

repeticoes <- conflitos %>% group_by(name) %>% group_size() # Equivalente ao Cr do artigo (vezes que ocorreu)

df <- matrix(
  nrow = capitais$longitude %>% length(),
  ncol = conflitos$name %>% unique() %>% length()
) %>%
  as.data.frame()


names(df) <- conflitos$name %>% unique()
df$ISO <- capitais$ISO3
df <- df[,c("ISO", conflitos$name)] %>%
  arrange(ISO)
df <- df %>% pivot_longer(!ISO,names_to = "Origem")

df <- data.table(df)
df[, Distancia := distancia(Origem = Origem, Destino = ISO), by = 1:nrow(df)]
df <- df %>% as.data.frame()

df <- df %>%
  distinct() %>%
  pivot_wider(names_from = Origem, values_from = Distancia)

data.table::fwrite(df, file = "./raw/distancia_geodesica_naoviolentos.csv")

df <- data.table::fread("./raw/distancia_geodesica_naoviolentos.csv") %>% select(!c(value))
df <- df %>% # Remove colunas que contêm apenas NA
  select(
    where(
      ~!all(is.na(.x))
    )
  )

continentes <- c(
  "Africa",
  "Americas",
  "Asia",
  "Europa"
    )

for(continente in continentes){
  subset <- str_subset(names(df), continente)
  df <- df %>%
    drop_na(any_of(subset)) %>%
    mutate("NaoViolentos_{continente}" := rowMeans(across(all_of(subset))))

}


subset <- str_subset(names(df), "Violentos")
df <- df %>% select(ISO, all_of(subset)) %>%
    mutate("NaoViolentos" := rowMeans(across(all_of(subset))))

data.table::fwrite(df, file = "./clean/conflitos_naoviolentos_continentes.csv")

data.table::fread("./clean/conflitos_violentos_continentes.csv") -> violentos
data.table::fread("./clean/conflitos_naoviolentos_continentes.csv") -> naoviolentos

df <- violentos %>%
  arrange(ISO) %>%
  left_join(naoviolentos) %>%
  arrange(ISO)

paises <- c(
  "Australia",
  "Canada",
  "Denmark",
  "Finland",
  "France",
  "Germany",
  "Ireland",
  "Italy",
  "Japan",
  "Netherlands",
  "New Zealand",
  "Norway",
  "Portugal",
  "Spain",
  "Sweden",
  "Switzerland",
  "UK",
  "USA" # Ausente no original
)


ISOs <- data.table::fread(file = "./clean/coord_capitais.csv") %>%
  filter(pais %in% paises) %>%
  select(ISO3C) %>%
  rename(ISO = ISO3C)

df <- df %>%
  filter(ISO %in% ISOs$ISO %>% c())

data.table::fwrite(df, file = "./clean/conflitos_filtrados_n_violentos_continentes.csv")

data.table::fread("./clean/conflitos_filtrados_n_violentos_continentes.csv") -> df

inv_dist2 <- function(x, pow=-1){
  x^pow*10^3
}

df <- df %>%
  mutate(across(where(is.numeric), ~inv_dist2(.x)))

data.table::fwrite(df, file = "./clean/coldwar_inv2_n_violentos_selecionados.csv")
