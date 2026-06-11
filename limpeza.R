# Lista de pacotes
packages <- c(
  "readxl","readxl", "RStata", "reticulate", "shiny", "bslib", "ggthemes", "RColorBrewer", 
  "sf", "shinythemes", "lubridate", "jsonlite", "stringr", "readr", "dplyr", "ggrepel", 
  "tidyverse", "shinyjs", "plotly", "ggplot2", "DT", "shinyWidgets","terra", 
  "shinydashboard", "shinycssloaders", "cowplot", "ggmap", "ggspatial", "sf", 
  "rmarkdown", "fontawesome", "haven", "gridExtra", "scales", "Rtools",
  "writexl", "openxlsx", "kableExtra", "rlang", "formattable", "glue", "httr2"
  
)

# Verifique quais pacotes não estão instalados
install_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

# Instale os pacotes que estão faltando
if (length(install_packages) > 0) {
  install.packages(install_packages)
}

# # Carregue os pacotes
lapply(packages, require, character.only = TRUE)
library(tidyr)
library(tidygeocoder)
library(dplyr)
library(openxlsx)
library(leaflet)
library(shinycssloaders)  # para o withSpinner
library(openxlsx)
library(readxl)

Baseline_Raizes <- readxl::read_excel("Baseline_Raizes_Azuis.xlsx")

table(Baseline_Raizes$Mulheres_Lideranca_Frequencia)



Presencas_Nexus <- read_excel("Presencas_Nexus.xlsx")

Presencas_Nexus <- Presencas_Nexus %>%
  select(-c(2, 3, 19))


Presencas_Nexus <- Presencas_Nexus %>%
  rename(
    Nome_participante = Nome_Participante.Nome_Participante,
    Sexo = Nome_Participante.Sexo,
    Idade   = Nome_Participante.Idade,
    Estado_Civil = Nome_Participante.Estado_Civil,
    Nivil_Educacao   = Nome_Participante.Nivil_Educacao,
    Faz_Poupanca  = Nome_Participante.Faz_Poupanca,
    Tem_Negocio = Nome_Participante.Tem_Negocio,
    Situacao_Participante = Nome_Participante.Situacao_Participante,
    Distrito = Nome_Participante.Distrito,
    Comunidade = Nome_Participante.Comunidade,
    Status = Nome_Participante.STATUS1,
    Facilitadores = Control_Facilitador,
    ID_MUVA = Nome_Participante.ID_Projecto,
    Tipo_Sessao = Control_Sessao,
    Nome_Sessao = Nome_da_Sess_o,
    Presenca = Presen_a,
    Turma = Nome_Participante.Turmas
  )


Presencas_Nexus <- Presencas_Nexus %>%
  pivot_wider(
    names_from = Nome_Sessao,  
    values_from = Presenca,     
    values_fn = first          
  )


Presencas_Nexus <- Presencas_Nexus %>%
  mutate(Facilitadores = str_to_title(Facilitadores))


sessao_cols <- names(Presencas_Nexus)[grepl("^Sessão_?\\d+$", names(Presencas_Nexus))]


sessao_cols_ordenadas <- sessao_cols[order(as.numeric(gsub("Sessão_?", "", sessao_cols)))]


################# RAIZES AZUIS
# =========================
# 1. PACOTES
# =========================
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(scales)

# =========================================================
# 2. BASE DE ESPÉCIES
# =========================================================

especies <- tribble(
  ~latin_name, ~xitswa_name,
  "Caesio xanthonota", "Batimenta",
  "Decapterus russelli", "Carapau",
  "Oplegnathus robinsoni", "Cherewa",
  "Lutjanus bohar", "Ndume",
  "Sardinella gibbosa", "Sardinha",
  "Thunnus albacares", "Txi Mussana",
  "Sphyraena barracuda", "Tsovane",
  "Mugil curema", "Mukanha"
)

# =========================================================
# 3. BASE FICTÍCIA — OCEAN GUARDIAN
# =========================================================

set.seed(2026)

n_ocean <- 1200

dados_ocean <- tibble(
  
  id = 1:n_ocean,
  
  data = sample(
    seq(as.Date("2026-01-01"),
        as.Date("2026-05-27"),
        by = "day"),
    n_ocean,
    TRUE
  ),
  
  pescador = sample(
    c("João","Carlos","Ana","Maria","Paulo"),
    n_ocean,
    TRUE
  ),
  
  ocean_guardian = sample(
    c("Celina","Mário","Nelson","Joana"),
    n_ocean,
    TRUE
  ),
  
  centro_pesca = sample(
    c("Tofo","Barra","Vilankulo","Xai-Xai"),
    n_ocean,
    TRUE
  ),
  
  tipo_arte = sample(
    c("Arrasto","Linha","Arpão","Emalhe","Apanha"),
    n_ocean,
    TRUE
  ),
  
  tipo_mare = sample(
    c("Cheia","Baixa","Enchente","Vazante"),
    n_ocean,
    TRUE
  ),
  
  peso_gramas = round(runif(n_ocean,100,10000),0),
  
  comprimento_cm = round(runif(n_ocean,5,120),1),
  
  latitude = runif(n_ocean,-25.5,-21.5),
  
  longitude = runif(n_ocean,32,35)
)

dados_ocean$latin_name <- sample(
  especies$latin_name,
  n_ocean,
  TRUE
)

dados_ocean <- left_join(
  dados_ocean,
  especies,
  by = "latin_name"
)
