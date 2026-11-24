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
    Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas = `amigos com quem posso partilhar`,
    Eu_posso_discutir_meus_problemas_com_meus_amigos = `posso discutir os meus problemas`,
    Os_meus_amigos_familiares_dao_me_apoio_que_preciso = `Os meus amigos e familiares dão-me o apoio de que preciso.`,
    A_minha_familia_pode_me_ajudar_tomar_decisoes = `A minha família pode me ajudar a tomar decisões.`,
    Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas = `Eu sinto que sou uma pessoa de valor, no mínimo tanto quanto as outras pessoas.`,
    As_vezes_acho_que_nao_presto_para_nada = `Às vezes acho que não presto para nada.   (INVERSO — alta agência = discordar)`,
    Eu_penso_que_sou_uma_pessoa_fracassada = `Levando tudo em conta, eu penso que sou uma pessoa fracassada.   (INVERSO — alta`,
    Eu_acho_que_tenho_varias_boas_qualidades = `Eu acho que tenho várias boas qualidades.`,
    No_geral_estou_satisfeita_comigo_mesma = `No geral, eu estou satisfeita comigo mesma.`,
    Expressar_minha_opiniao_minha_familia = `Expressar a minha opinião na minha família.`,
    Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe = `Expressar a minha opinião na minha família, mesmo se não concordo com o(a) chefe`,
    Expressar_minha_opiniao_trabalho = `Expressar a minha opinião num âmbito de trabalho / serviço.`,
    Expressar_minha_opiniao_escola = `Expressar a minha opinião se estou em desacordo com o director da escola ou com`,
    Defender_me_se_for_tratada_injustamente = `Defender-me se for tratada injustamente (verbalmente).`,
    Dizer_outros_pararem_quando_fazer_me_infeliz = `Dizer aos outros para pararem quando estão a fazer-me infeliz.`,
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

table(Baseline_Raizes$Pessoas_para_falar_quando_se_sente_sozinha)

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

# ============================================================
# OCEAN GUARD — RAIZES AZUIS
# PREPARACAO E LIMPEZA DAS FICHAS DE AMOSTRAGEM
#
# Bases produzidas:
#   1. Amostragens
#   2. Especies
#   3. Comprimentos
#   4. Resumo_Comprimentos
#   5. Controlo_Qualidade
#   6. Controlo_Medicoes
#   7. dados_ocean
#   8. dados_especies
#   9. dados_comprimentos
# ============================================================


# ============================================================
# 1. PACOTES
# ============================================================

pacotes <- c(
  "readxl",
  "tidyverse",
  "writexl",
  "lubridate",
  "stringr",
  "janitor",
  "shiny",
  "plotly",
  "DT",
  "scales"
)

novos <- pacotes[
  !pacotes %in% installed.packages()[, "Package"]
]

if (length(novos) > 0) {
  install.packages(novos)
}

invisible(
  lapply(pacotes, library, character.only = TRUE)
)


# ============================================================
# 2. FICHEIRO DE ENTRADA
# ============================================================

ficheiro <- "Fichas_Amostragem_Raizes_Azuis.xlsx"

if (!file.exists(ficheiro)) {
  stop("Ficheiro Excel não encontrado: ", ficheiro)
}


# ============================================================
# 3. IMPORTAR TODAS AS FOLHAS
# ============================================================

nomes_folhas <- excel_sheets(ficheiro)

if (length(nomes_folhas) == 0) {
  stop("O ficheiro não contém folhas para leitura.")
}

cat("Folhas encontradas:", length(nomes_folhas), "\n")

fichas_originais <- set_names(
  map(nomes_folhas, function(aba) {
    
    read_excel(
      ficheiro,
      sheet = aba,
      col_names = FALSE,
      .name_repair = "minimal"
    )
    
  }),
  nomes_folhas
)


# ============================================================
# 4. FUNCOES AUXILIARES
# ============================================================

# ------------------------------------------------------------
# 4.1. Limpar texto
# ------------------------------------------------------------

limpar_texto <- function(x) {
  
  x <- as.character(x)
  
  x <- str_replace_all(x, "\u00a0", " ")
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}


# ------------------------------------------------------------
# 4.2. Normalizar rótulos
# ------------------------------------------------------------

normalizar_rotulo <- function(x) {
  
  x <- limpar_texto(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- str_to_lower(x)
  
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  
  str_squish(x)
}


# ------------------------------------------------------------
# 4.3. Obter valor à direita do rótulo
# ------------------------------------------------------------

obter_valor <- function(dados, rotulo) {
  
  matriz <- as.matrix(dados)
  
  rotulo <- normalizar_rotulo(rotulo)
  
  if (nrow(matriz) == 0 || ncol(matriz) < 2) {
    return(NA_character_)
  }
  
  for (i in seq_len(nrow(matriz))) {
    
    for (j in seq_len(ncol(matriz) - 1)) {
      
      celula <- normalizar_rotulo(matriz[i, j])
      
      if (!is.na(celula) && celula == rotulo) {
        
        return(matriz[i, j + 1])
        
      }
    }
  }
  
  NA_character_
}


# ------------------------------------------------------------
# 4.4. Converter números
# ------------------------------------------------------------

converter_numero <- function(x) {
  
  if (length(x) == 0 || is.na(x)) {
    return(NA_real_)
  }
  
  x <- limpar_texto(x)
  
  if (is.na(x)) {
    return(NA_real_)
  }
  
  x <- str_replace_all(x, ",", ".")
  
  suppressWarnings(as.numeric(x))
}


# ------------------------------------------------------------
# 4.5. Converter datas
# ------------------------------------------------------------

converter_data <- function(x) {
  
  if (length(x) == 0 || is.na(x)) {
    return(as.Date(NA))
  }
  
  # Se já for uma data
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  x <- limpar_texto(x)
  
  if (is.na(x)) {
    return(as.Date(NA))
  }
  
  # Datas numéricas do Excel
  # Exemplo: 46306 = 11/10/2026
  
  if (str_detect(x, "^\\d+(\\.0+)?$")) {
    
    numero <- as.numeric(x)
    
    if (!is.na(numero) &&
        numero >= 20000 &&
        numero <= 60000) {
      
      return(
        as.Date(numero, origin = "1899-12-30")
      )
    }
  }
  
  # Datas no formato ISO: 2026-10-11
  
  if (str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")) {
    
    return(
      suppressWarnings(
        as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
      )
    )
  }
  
  # Datas com barras
  # Prioridade ao formato americano MM/DD/YYYY,
  # confirmado pelos valores 10/11/2026 e 10/28/2026.
  
  data <- suppressWarnings(
    as.Date(x, format = "%m/%d/%Y")
  )
  
  # Alternativa: DD/MM/YYYY, quando necessário
  
  if (is.na(data)) {
    
    data <- suppressWarnings(
      as.Date(x, format = "%d/%m/%Y")
    )
    
  }
  
  data
}


# ------------------------------------------------------------
# 4.6. Criar identificador de amostragem
# ------------------------------------------------------------

criar_id <- function(x, numero) {
  
  nome <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  nome <- str_replace_all(
    str_to_upper(nome),
    "[^A-Z0-9]",
    ""
  )
  
  paste0(
    "RA_",
    nome,
    "_",
    sprintf("%03d", numero)
  )
}


# ============================================================
# 5. DIAGNOSTICO DAS FOLHAS
# ============================================================

diagnostico_fichas <- map_dfr(
  names(fichas_originais),
  function(aba) {
    
    dados <- fichas_originais[[aba]]
    
    tibble(
      Folha = aba,
      Numero_Linhas = nrow(dados),
      Numero_Colunas = ncol(dados),
      Celulas_Preenchidas = sum(
        !is.na(as.matrix(dados))
      )
    )
    
  }
)

print(diagnostico_fichas)


# ============================================================
# 6. EXTRACAO DAS AMOSTRAGENS
# ============================================================

amostragens_lista <- map2(
  fichas_originais,
  names(fichas_originais),
  function(dados, aba) {
    
    numero <- match(aba, nomes_folhas)
    
    tibble(
      
      ID_Amostragem = criar_id(aba, numero),
      
      Folha_Original = aba,
      
      # DATA JÁ CONVERTIDA PARA DATE
      Data = converter_data(
        obter_valor(dados, "Data")
      ),
      
      Hora = limpar_texto(
        obter_valor(dados, "Hora")
      ),
      
      Local = limpar_texto(
        obter_valor(dados, "Local")
      ),
      
      Ordem_Atracagem = converter_numero(
        obter_valor(dados, "Ordem de atracagem")
      ),
      
      Amostragem_Feita = limpar_texto(
        obter_valor(dados, "Amostragem feita?")
      ),
      
      Motivo_Nao_Amostragem = limpar_texto(
        obter_valor(dados, "Porquê não?")
      ),
      
      Dias_Viagem_Pesca = limpar_texto(
        obter_valor(dados, "Dias de viagem de pesca")
      ),
      
      Ocean_Guardian = limpar_texto(
        obter_valor(dados, "Nome do Ocean Guardian")
      ),
      
      Tipo_Mare = limpar_texto(
        obter_valor(dados, "Tipo de maré")
      ),
      
      Estado_Tempo = limpar_texto(
        obter_valor(dados, "Estado do tempo")
      ),
      
      Tipo_Arte = limpar_texto(
        obter_valor(dados, "Tipo de arte")
      ),
      
      Pescador_Principal = limpar_texto(
        obter_valor(dados, "Nome do pescador principal")
      ),
      
      Numero_Pescadores = converter_numero(
        obter_valor(dados, "Nº de pescadores")
      ),
      
      Embarcacao = limpar_texto(
        obter_valor(dados, "Embarcação")
      ),
      
      Tamanho_Rede = limpar_texto(
        obter_valor(dados, "Tamanho da rede")
      ),
      
      Polegadas_Principal = converter_numero(
        obter_valor(dados, "Polegadas – principal")
      ),
      
      Polegadas_Minimo = converter_numero(
        obter_valor(dados, "Polegadas – mínimo")
      ),
      
      Peso_Total_Capturas_Kg = converter_numero(
        obter_valor(
          dados,
          "Peso total capturas (kg, todas as espécies)"
        )
      ),
      
      Numero_Especies_Registado = converter_numero(
        obter_valor(
          dados,
          "Nº de espécies capturadas"
        )
      ),
      
      Pagina_Adicional = limpar_texto(
        obter_valor(dados, "Página adicional necessária?")
      ),
      
      Observacoes = NA_character_,
      
      Notas_Transcricao = NA_character_
      
    )
    
  }
)

Amostragens <- bind_rows(amostragens_lista)


# ============================================================
# 7. EXTRACAO DAS ESPECIES
# ============================================================

extrair_especies <- function(dados, id_amostragem) {
  
  matriz <- as.matrix(dados)
  
  resultado <- list()
  
  for (e in 1:4) {
    
    col_rotulo <- (e - 1) * 2 + 1
    
    col_valor <- (e - 1) * 2 + 2
    
    if (ncol(matriz) < col_valor) next
    
    rotulos <- normalizar_rotulo(
      matriz[, col_rotulo]
    )
    
    linhas_nome <- which(rotulos == "nome")
    
    if (length(linhas_nome) == 0) next
    
    linha_nome <- linhas_nome[1]
    
    nome <- limpar_texto(
      matriz[linha_nome, col_valor]
    )
    
    if (is.na(nome)) next
    
    # Peso correspondente ao bloco da espécie
    
    linhas_peso <- which(
      str_detect(
        rotulos,
        "^peso total"
      )
    )
    
    peso <- NA_real_
    
    if (length(linhas_peso) > 0) {
      
      peso <- converter_numero(
        matriz[linhas_peso[1], col_valor]
      )
      
    }
    
    resultado[[length(resultado) + 1]] <- tibble(
      
      ID_Amostragem = id_amostragem,
      
      ID_Especie = e,
      
      Nome_Especie = nome,
      
      Peso_Total_Kg = peso
      
    )
    
  }
  
  if (length(resultado) == 0) {
    
    return(
      tibble(
        ID_Amostragem = character(),
        ID_Especie = integer(),
        Nome_Especie = character(),
        Peso_Total_Kg = double()
      )
    )
    
  }
  
  bind_rows(resultado)
}


Especies <- map2_dfr(
  fichas_originais,
  Amostragens$ID_Amostragem,
  ~ extrair_especies(.x, .y)
)


# ============================================================
# 8. EXTRACAO DOS COMPRIMENTOS
# ============================================================

extrair_comprimentos <- function(dados, id_amostragem) {
  
  matriz <- as.matrix(dados)
  
  resultado <- list()
  
  for (e in 1:4) {
    
    col_rotulo <- (e - 1) * 2 + 1
    
    col_valor <- (e - 1) * 2 + 2
    
    if (ncol(matriz) < col_valor) next
    
    rotulos <- normalizar_rotulo(
      matriz[, col_rotulo]
    )
    
    linhas_nome <- which(rotulos == "nome")
    
    if (length(linhas_nome) == 0) next
    
    linha_nome <- linhas_nome[1]
    
    nome <- limpar_texto(
      matriz[linha_nome, col_valor]
    )
    
    if (is.na(nome)) next
    
    linhas_curto <- which(
      str_detect(
        rotulos,
        "comp.*7 mais.*curtos"
      )
    )
    
    linhas_comprido <- which(
      str_detect(
        rotulos,
        "comp.*7 mais.*compridos"
      )
    )
    
    grupos <- list(
      "Mais curtos" = linhas_curto,
      "Mais compridos" = linhas_comprido
    )
    
    for (grupo in names(grupos)) {
      
      linhas <- grupos[[grupo]]
      
      if (length(linhas) == 0) next
      
      linha_inicio <- linhas[1]
      
      linha_fim <- min(
        nrow(matriz),
        linha_inicio + 10
      )
      
      if (linha_inicio + 1 > linha_fim) next
      
      for (r in seq.int(linha_inicio + 1, linha_fim)) {
        
        numero <- converter_numero(
          matriz[r, col_rotulo]
        )
        
        comprimento <- converter_numero(
          matriz[r, col_valor]
        )
        
        if (!is.na(numero) &&
            numero >= 1 &&
            numero <= 14 &&
            !is.na(comprimento) &&
            comprimento > 0) {
          
          resultado[[length(resultado) + 1]] <- tibble(
            
            ID_Amostragem = id_amostragem,
            
            ID_Especie = e,
            
            Nome_Especie = nome,
            
            Grupo_Comprimento = grupo,
            
            Numero_Exemplar = as.integer(numero),
            
            Comprimento_Cm = comprimento
            
          )
          
        }
      }
    }
  }
  
  if (length(resultado) == 0) {
    
    return(
      tibble(
        ID_Amostragem = character(),
        ID_Especie = integer(),
        Nome_Especie = character(),
        Grupo_Comprimento = character(),
        Numero_Exemplar = integer(),
        Comprimento_Cm = double()
      )
    )
    
  }
  
  bind_rows(resultado)
}


Comprimentos <- map2_dfr(
  fichas_originais,
  Amostragens$ID_Amostragem,
  ~ extrair_comprimentos(.x, .y)
)


# ============================================================
# 9. RESUMO DOS COMPRIMENTOS
# ============================================================

Resumo_Comprimentos <- Comprimentos %>%
  
  group_by(
    ID_Amostragem,
    ID_Especie,
    Nome_Especie,
    Grupo_Comprimento
  ) %>%
  
  summarise(
    
    Numero_Medicoes = n(),
    
    Comprimento_Total_Cm = sum(
      Comprimento_Cm,
      na.rm = TRUE
    ),
    
    Comprimento_Medio_Cm = mean(
      Comprimento_Cm,
      na.rm = TRUE
    ),
    
    Comprimento_Minimo_Cm = min(
      Comprimento_Cm,
      na.rm = TRUE
    ),
    
    Comprimento_Maximo_Cm = max(
      Comprimento_Cm,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# 10. CONTROLO DE QUALIDADE
# ============================================================

Resumo_Especies <- Especies %>%
  
  group_by(ID_Amostragem) %>%
  
  summarise(
    
    Numero_Especies_Extraidas = n_distinct(
      ID_Especie
    ),
    
    Peso_Total_Especies_Kg = if (
      all(is.na(Peso_Total_Kg))
    ) {
      NA_real_
    } else {
      sum(Peso_Total_Kg, na.rm = TRUE)
    },
    
    .groups = "drop"
  )


Controlo_Qualidade <- Amostragens %>%
  
  left_join(
    Resumo_Especies,
    by = "ID_Amostragem"
  ) %>%
  
  mutate(
    
    Diferenca_Peso_Kg =
      Peso_Total_Capturas_Kg -
      Peso_Total_Especies_Kg,
    
    Diferenca_Numero_Especies =
      Numero_Especies_Registado -
      Numero_Especies_Extraidas,
    
    Estado_Peso = case_when(
      
      is.na(Peso_Total_Capturas_Kg) ~ "Sem dados",
      
      is.na(Numero_Especies_Extraidas) ~ "Sem especies",
      
      is.na(Peso_Total_Especies_Kg) ~ "Peso das especies sem dados",
      
      abs(Diferenca_Peso_Kg) < 0.01 ~ "Consistente",
      
      TRUE ~ "Verificar"
      
    ),
    
    Estado_Especies = case_when(
      
      is.na(Numero_Especies_Registado) ~ "Sem dados",
      
      is.na(Numero_Especies_Extraidas) ~ "Sem especies",
      
      Diferenca_Numero_Especies == 0 ~ "Consistente",
      
      TRUE ~ "Verificar"
      
    )
  )


# ============================================================
# 11. CONTROLO DAS MEDICOES
# ============================================================

Controlo_Medicoes <- Comprimentos %>%
  
  group_by(
    ID_Amostragem,
    ID_Especie,
    Nome_Especie,
    Grupo_Comprimento
  ) %>%
  
  summarise(
    
    Numero_Medicoes = n(),
    
    Medicoes_Repetidas = any(
      duplicated(Numero_Exemplar)
    ),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    Estado_Medicoes = case_when(
      
      Medicoes_Repetidas ~ "Verificar duplicados",
      
      Numero_Medicoes != 7 ~ "Verificar quantidade",
      
      TRUE ~ "Consistente"
      
    )
  )


# ============================================================
# 12. PREPARACAO DAS BASES PARA O OCEAN GUARD
# ============================================================


# ------------------------------------------------------------
# 12.1. Base geral das amostragens
# ------------------------------------------------------------

dados_ocean <- Amostragens %>%
  
  mutate(
    
    pescador = Pescador_Principal,
    
    centro_pesca = Local,
    
    tipo_arte = Tipo_Arte,
    
    tipo_mare = Tipo_Mare,
    
    embarcacao = Embarcacao,
    
    peso_total = Peso_Total_Capturas_Kg,
    
    # Utilizar directamente a data já convertida
    data_amostragem = Data
    
  )


# ------------------------------------------------------------
# 12.2. Base de espécies
# ------------------------------------------------------------

dados_especies <- Especies %>%
  
  left_join(
    
    Amostragens %>%
      
      select(
        ID_Amostragem,
        Local,
        Data
      ),
    
    by = "ID_Amostragem"
    
  )


# ------------------------------------------------------------
# 12.3. Base de comprimentos
# ------------------------------------------------------------

dados_comprimentos <- Comprimentos %>%
  
  left_join(
    
    Amostragens %>%
      
      select(
        ID_Amostragem,
        Local,
        Data
      ),
    
    by = "ID_Amostragem"
    
  )


# ============================================================
# 13. DIAGNOSTICO FINAL DAS BASES
# ============================================================

cat("\n============================================\n")
cat("DIAGNOSTICO DAS BASES — OCEAN GUARD\n")
cat("============================================\n")

cat("Folhas importadas:", length(fichas_originais), "\n")

cat("Amostragens:", nrow(dados_ocean), "\n")

cat("Espécies:", nrow(dados_especies), "\n")

cat("Comprimentos:", nrow(dados_comprimentos), "\n")

cat(
  "Pescadores:",
  n_distinct(dados_ocean$Pescador_Principal, na.rm = TRUE),
  "\n"
)

cat(
  "Locais:",
  n_distinct(dados_ocean$Local, na.rm = TRUE),
  "\n"
)

cat(
  "Espécies distintas:",
  n_distinct(dados_especies$Nome_Especie, na.rm = TRUE),
  "\n"
)

cat(
  "Medições biométricas:",
  nrow(dados_comprimentos),
  "\n"
)

cat(
  "Datas válidas:",
  sum(!is.na(dados_ocean$Data)),
  "\n"
)

cat(
  "Datas em falta:",
  sum(is.na(dados_ocean$Data)),
  "\n"
)

cat(
  "Amostragens sem espécies:",
  sum(
    !dados_ocean$ID_Amostragem %in%
      dados_especies$ID_Amostragem
  ),
  "\n"
)

cat("============================================\n")


# ============================================================
# 14. VERIFICACAO DAS DATAS
# ============================================================

Verificacao_Datas <- Amostragens %>%
  
  select(
    ID_Amostragem,
    Folha_Original,
    Data,
    Local
  ) %>%
  
  mutate(
    Estado_Data = if_else(
      is.na(Data),
      "Verificar data original",
      "Data válida"
    )
  )

print(Verificacao_Datas)


# ============================================================
# 15. EXPORTAR RESULTADOS
# ============================================================

saida <- "Raizes_Azuis_Dados_Consolidados.xlsx"

write_xlsx(
  
  list(
    
    Amostragens = Amostragens,
    
    Especies = Especies,
    
    Comprimentos = Comprimentos,
    
    Resumo_Comprimentos = Resumo_Comprimentos,
    
    Controlo_Qualidade = Controlo_Qualidade,
    
    Controlo_Medicoes = Controlo_Medicoes,
    
    Diagnostico = diagnostico_fichas,
    
    Verificacao_Datas = Verificacao_Datas,
    
    dados_ocean = dados_ocean,
    
    dados_especies = dados_especies,
    
    dados_comprimentos = dados_comprimentos
    
  ),
  
  path = saida
)

cat("\nFicheiro exportado:", saida, "\n")


# ============================================================
# 16. CONSULTAR RESULTADOS
# ============================================================

View(Amostragens)

View(Especies)

View(Comprimentos)

View(Resumo_Comprimentos)

View(Controlo_Qualidade)

View(Controlo_Medicoes)

View(Verificacao_Datas)

View(dados_ocean)

View(dados_especies)

View(dados_comprimentos)


# ============================================================
# FIM DA PREPARACAO DOS DADOS — OCEAN GUARD
# ============================================================
# Amostragens %>%
#   select(ID_Amostragem, Data, Local) %>%
#   print(n = 20)
# 
# str(Amostragens$Data)
# str(dados_ocean$data_amostragem)
# str(dados_especies$Data)
# str(dados_comprimentos$Data)
# 
# dados_especies %>%
#   select(ID_Amostragem, Nome_Especie, Data, Local) %>%
#   head(15)
# 
# dados_comprimentos %>%
#   select(ID_Amostragem, Nome_Especie, Data, Local) %>%
#   head(15)
# 
# dados_ocean %>%
#   count(Data, sort = TRUE)
# 
# 
# diagnostico_fichas
# Verificacao_Datas
# head(dados_especies)
# head(dados_comprimentos)
# 
# 
# # Resumo das amostragens
# Controlo_Qualidade %>%
#   select(
#     ID_Amostragem,
#     Peso_Total_Capturas_Kg,
#     Numero_Especies_Registado,
#     Numero_Especies_Extraidas,
#     Peso_Total_Especies_Kg,
#     Estado_Peso,
#     Estado_Especies
#   ) %>%
#   print(n = Inf)
# 
# Controlo_Medicoes %>%
#   count(
#     Estado_Medicoes,
#     name = "Total"
#   )
# 
# cat("Total de espécies:", nrow(Especies), "\n")
# 
# cat("Total de medições:", nrow(Comprimentos), "\n")
# 
# cat("Total de grupos biométricos:",
#     nrow(Controlo_Medicoes), "\n")
# 
# 
# 
# Comprimentos %>%
#   group_by(
#     ID_Amostragem,
#     Nome_Especie,
#     Grupo_Comprimento
#   ) %>%
#   summarise(
#     Total_Medicoes = n(),
#     Exemplares = paste(
#       sort(Numero_Exemplar),
#       collapse = ", "
#     ),
#     .groups = "drop"
#   ) %>%
#   print(n = Inf)