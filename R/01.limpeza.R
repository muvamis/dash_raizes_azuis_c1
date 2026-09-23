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


Baseline_Raizes <- read_excel("Baseline_Raizes_Azuis.xls")


Baseline_Raizes <- Baseline_Raizes %>%
  rename(
    Data_Entrevista = `Data da entrevista`,
    Consentimento = consentimento_participar,
    Tipo_Avaliacao = momento,
    Local_Entrevista = `local entrevista`,
    Nome_Inquiridor = nome_do_inqueridor,
    Nome_Supervisora = `nome supervisor`,
    Comunidade = `Nome da comunidade`,
    Especifica_Comunidade = especificar...14,
    Nome_participante = `Nome participante`,
    Sexo = `Sexo participante`,
    ID_MUVA = `Id participante`,
    Estado_Civil = `estado civil do participante`,
    Numero_filhos = `numero filhos da participante`,
    Principal_Actividade_Esta_Ligada_Mar = `sua principal actividade`,
    Actividade_Mar = `actividade principal do mar`,
    Actividade_Fora_Mar = `actividade principal fora do mar`,
    Outra_Act_Fora_Mar = `tem alguma actividade SECUNDÁRIA ligada ao mar?`,
    Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica = `Quem decide sobre actividade economica`,
    Eu_Tenho_Poder_Sobre_Educacao = `Pessoa que decide sobre educacao`,
    Quem_Decide_Sobre_Futuro_Profissional = `Pessoa que decide sobre futuro proficional`,
    Quem_Decide_Sobre_Movimentos = `Pessoa que decide sobre seus movimento`,
    Quem_Decide_Sobre_Grandes_Despesas_familiares =`Quem toma decisoes grandes sobre as despesas da familia`,
    Quem_Decide_Sobre_Pequenas_Despesas_familiares = `Quem toma decisoes pequenas sobre as despesas da familia`,
    Gostaria_Escolhido_Lider_Duma_Organizacao = `Gostaria de alguma vez ser escolhido para ser líder duma organização`,
    Mulheres_Selecionadas_Para_Posicao_Lideranca = `Mulheres selecionadas para posicao de lideranca`,
    Aprovaria_Uma_Mulher_Selecionada_Para_Liderar = `Aprovaria ou não, que uma mulher aqui na zona fosse selecionada para liderar uma`,
    Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder = `Na sua opinião, quantas pessoas aprovariam uma mulher lider`,
    Uma_Mulher_Deveria_Aceitar_Violencia_Domestica = `tolerancia violencia domest`,
    Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal = `tolerancia violencia domest2`,
    Pessoas_para_falar_quando_se_sente_sozinha = `pessoas para fazer compania...53`,
    Pessoas_para_discutir_problemas = `pessoas para fazer compania...54`,
    
    Respeito_Pelo_Jovem_Bairro = `respeito pelo jovem no bairro`
  )



Baseline_Raizes <- Baseline_Raizes %>%
  filter (Consentimento == "Sim"
  )


Baseline_Raizes <- Baseline_Raizes %>%
  mutate(
    Sexo = gsub("^Femenino$", "Feminino", Sexo)
  )
Baseline_Raizes <- Baseline_Raizes %>%
  mutate(
    Distrito_Localidade = case_when(
      Comunidade %in% c(
        "Chibo",
        "Chicuinine",
        "Chingonguene",
        "Chiuzene",
        "Machuquele",
        "Marape",
        "Matsopane"
      ) ~ "Santuário Bravio",
      
      Comunidade %in% c(
        "19 de Outubro",
        "25 de Junho (Vilanculos)",
        "5˚Congresso",
        "7 de Setembro",
        "Aeroporto (Vilanculos)",
        "Alto Macassa",
        "Bairro central",
        "Chibuene",
        "Guitine",
        "Mangalisse",
        "Mondego (Desse)"
      ) ~ "Vilanculos",
      
      TRUE ~ NA_character_
    )
  )

table(Baseline_Raizes$Pessoas_para_discutir_problemas)

# Baseline_Raizes <- Baseline_Raizes %>%
#   mutate(
#     Comunidade = case_when(
#       str_to_upper(str_squish(Comunidade)) == "OUTRO ESPECIFICAR" ~ 
#         str_squish(Especifica_Comunidade),
#       TRUE ~ Comunidade
#     )
#   )
# 
# Baseline_Raizes <- Baseline_Raizes %>%
#   mutate(
#     Comunidade = if_else(
#       Comunidade == "Outro Especificar",
#       especificar...14,
#       Comunidade
#     )
#   )

# Presencas_Nexus <- read_excel("Presencas_Nexus.xlsx")
# 
# Presencas_Nexus <- Presencas_Nexus %>%
#   select(-c(2, 3, 19))
# 
# 
# Presencas_Nexus <- Presencas_Nexus %>%
#   rename(
#     Nome_participante = Nome_Participante.Nome_Participante,
#     Sexo = Nome_Participante.Sexo,
#     Idade   = Nome_Participante.Idade,
#     Estado_Civil = Nome_Participante.Estado_Civil,
#     Nivil_Educacao   = Nome_Participante.Nivil_Educacao,
#     Faz_Poupanca  = Nome_Participante.Faz_Poupanca,
#     Tem_Negocio = Nome_Participante.Tem_Negocio,
#     Situacao_Participante = Nome_Participante.Situacao_Participante,
#     Distrito = Nome_Participante.Distrito,
#     Comunidade = Nome_Participante.Comunidade,
#     Status = Nome_Participante.STATUS1,
#     Facilitadores = Control_Facilitador,
#     ID_MUVA = Nome_Participante.ID_Projecto,
#     Tipo_Sessao = Control_Sessao,
#     Nome_Sessao = Nome_da_Sess_o,
#     Presenca = Presen_a,
#     Turma = Nome_Participante.Turmas
#   )
# 
# 
# Presencas_Nexus <- Presencas_Nexus %>%
#   pivot_wider(
#     names_from = Nome_Sessao,  
#     values_from = Presenca,     
#     values_fn = first          
#   )
# 
# 
# Presencas_Nexus <- Presencas_Nexus %>%
#   mutate(Facilitadores = str_to_title(Facilitadores))
# 
# 
# sessao_cols <- names(Presencas_Nexus)[grepl("^Sessão_?\\d+$", names(Presencas_Nexus))]
# 
# 
# sessao_cols_ordenadas <- sessao_cols[order(as.numeric(gsub("Sessão_?", "", sessao_cols)))]
# 

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
