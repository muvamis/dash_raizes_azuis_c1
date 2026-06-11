# ==========================================================
# DASHBOARD SHINY – RAÍZES AZUIS
# BASE: Baseline_Raizes
# ==========================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(tidyr)
library(DT)

# ==========================================================
# UI
# ==========================================================
ui <- navbarPage(
  title = "Raízes Azuis",
  
  # ------------------ Estilo e Tema ------------------
  header = tags$head(
    tags$style(HTML("

      .navbar {
        background-color: #9442d4;
      }

      .navbar-default .navbar-nav > li > a {
        color: white;
        font-weight: bold;
      }

      .navbar-default .navbar-brand {
        color: white;
        font-weight: bold;
      }

      .tab-content {
        background: #ffffff;
        padding: 15px;
        border-radius: 10px;
      }

      .nav-tabs > li > a {
        color: #6a1b9a;
        font-weight: bold;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #9442d4 !important;
        color: white !important;
      }
 /* =========================
       VALUE BOXES (KPIs)
    ========================== */
    .value-box-container {
      display: flex;
      flex-wrap: wrap;
      justify-content: center;
      align-items: stretch;
      gap: 15px;
      margin-top: 10px;
    }

    .value-box {
      flex: 1 1 180px;
      max-width: 220px;
      min-width: 160px;

      padding: 16px;
      border-radius: 14px;
      color: white;
      font-weight: bold;
      text-align: center;

      box-shadow: 0 3px 10px rgba(0,0,0,0.15);
      transition: all 0.25s ease-in-out;
    }

    .value-box:hover {
      transform: translateY(-4px);
      box-shadow: 0 6px 18px rgba(0,0,0,0.25);
    }

    /* =========================
       CORES
    ========================== */
    .blue   { background-color: #6a1b9a; }
    .green  { background-color: #5cd6c7; }
    .orange { background-color: #f77333; }
    .yellow { background-color: #f9a825; color: #000; }
    .purple { background-color: #004c91; }

    /* =========================
       TEXTO
    ========================== */
    .value-title {
      font-size: 13px;
      margin-top: 6px;
      opacity: 0.95;
    }

    .value-number {
      font-size: 22px;
      font-weight: 800;
    }

    /* =========================
       RESPONSIVO
    ========================== */
    @media (max-width: 768px) {
      .value-box {
        flex: 1 1 45%;
      }
    }

    @media (max-width: 480px) {
      .value-box {
        flex: 1 1 100%;
      }
    }

    "))
  ),
  # ========================================================
  # 1. AGENCIA (TODOS OS INDICADORES)
  # ========================================================
  tabPanel(
    "Agência",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          "filtro_ciclo",
          "Ciclo:",
          choices = c("Todos", unique(Baseline_Raizes$Ciclo))
        ),
        
        selectInput(
          "filtro_tipo_avaliacao",
          "Avaliação:",
          choices = c("Todos", unique(Baseline_Raizes$Tipo_Avaliacao))
        ),
        
        selectInput(
          "filtro_local",
          "Local:",
          choices = c("Todos", unique(Baseline_Raizes$Local_Entrevista))
        ),
        
        selectInput(
          "filtro_comunidade",
          "Comunidade:",
          choices = c("Todos", unique(Baseline_Raizes$Comunidade))
        )
        
      ),
      
      mainPanel(
        
        fluidRow(uiOutput("kpi_agencia")),
        
        tabsetPanel(
          
          tabPanel("Perfil",
                   fluidRow(
                     column(6, plotlyOutput("grafico_sexo")),
                     column(6, plotlyOutput("grafico_estado_civil"))
                   )
          ),
          
          tabPanel("Tomada de Decisão",
                   fluidRow(
                     column(12, plotlyOutput("grafico_decisao_geral", height = "650px"))
                     ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_decisao_economica")),
                     column(6, plotlyOutput("grafico_Decisao_Educacao"))
                   ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_Decisao_Futuro_Profissional")),
                     column(6, plotlyOutput("grafico_Decisao_Movimentos"))
                   ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_Decisao_Grandes_Despesas")),
                     column(6, plotlyOutput("grafico_Decisao_Pequenas_Despesas"))
                   )
          ),
                   
          tabPanel("Normas Sociais",
                   fluidRow(
                     column(12, plotlyOutput("grafico_Violencia_Domestica_geral", height = "650px"))
                   ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_Violencia_Domestica_Aceitavel")),
                     column(6, plotlyOutput("grafico_Violencia_Assunto_Privado"))
                   ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_Gostaria_Ser_Lider")),
                     column(6, plotlyOutput("grafico_Mulheres_Lideranca_Frequencia"))
                   ),
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("grafico_Aprovacao_Lideranca_Mulher")),
                     column(6, plotlyOutput("grafico_Percepcao_Aprovacao_Comunidade"))
                   )
          ),
          
          tabPanel("Auto-Eficácia (Voice)",
                   fluidRow(
                     column(6, plotlyOutput("grafico_voice")),
                     column(6, plotlyOutput("grafico_a"))
                   )
          ),
          
          tabPanel("Auto-Estima",
                   plotlyOutput("grafico_decisoes")
          ),
          
          tabPanel("Suporte Social",
                   plotlyOutput("grafico_suporte_social")
          )
          
        )
      )
    )
  ),
  
  # ========================================================
  # 2. MONITORIA
  # ========================================================
  tabPanel(
    "Monitoria",
    
    tabsetPanel(
      tabPanel(
        tagList(icon("users"), "Presenças Gerais"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "distritoInput_namp_pi",
              "Distrito:",
              choices = c("TODOS", unique(Presencas_Nexus$Distrito))
            ),
            selectInput(
              "comunidadeInput_namp_pi",
              "Comunidade:",
              choices = c("TODAS", unique(Presencas_Nexus$Comunidade))
            )
          ),
          
          mainPanel(
            uiOutput("texto_participacao_sessoes"),
            br(),
            # downloadButton("baixarBasePresencasExcel", "Baixar Presenças"),
            withSpinner(plotlyOutput("graficoParticipacaoGlobal", height = "500px")),
            br(), br(),
            
            uiOutput("texto_participacao_sexo"),
            br(),
            withSpinner(plotlyOutput("graficoParticipacaoSexo", height = "400px"))
          )
        )
      ),
      
      tabPanel(
        tagList(icon("user-check"), "Presenças Individuais"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "distritoInput_",
              "Distrito:",
              choices = c("TODOS", unique(Presencas_Nexus$Distrito))
            ),
            selectInput(
              "comunidadeAcompanhamento",
              "Comunidade:",
              choices = c("TODAS", unique(Presencas_Nexus$Comunidade))
            ),
            selectInput(
              "facilitadorInput",
              "Facilitador/a:",
              choices = c("TODOS", unique(Presencas_Nexus$Facilitadores))
            )
          ),
          
          mainPanel(
            fluidRow(
              column(
                6,
                uiOutput("texto_grafico_N"),
                br(),
                plotlyOutput("grafico_N")
              ),
              column(
                6,
                uiOutput("texto_situacao_interpretacao"),
                br(),
                plotlyOutput("grafico_situacao_C")
              )
            ),
            br(),
            uiOutput("pontosPresenca"),
            br(),
            uiOutput("texto_presencas"),
            br(),
            dataTableOutput("tabelaPresencas")
          )
        )
      )
    )
  ),
  
  # ========================================================
  # 3. OCEAN GUARD
  # ========================================================
  tabPanel(
    "Ocean Guard",
    
    # =====================================================
    # 🎛️ FILTROS
    # =====================================================
    sidebarPanel(
      
      selectInput(
        "pescador",
        "Pescador:",
        choices = c("Todos", sort(unique(dados_ocean$pescador)))
      ),
      
      selectInput(
        "centro",
        "Centro de Pesca:",
        choices = c("Todos", sort(unique(dados_ocean$centro_pesca)))
      ),
      
      selectInput(
        "arte",
        "Tipo de Arte:",
        choices = c("Todos", sort(unique(dados_ocean$tipo_arte)))
      ),
      
      selectInput(
        "mare",
        "Tipo de Maré:",
        choices = c("Todas", sort(unique(dados_ocean$tipo_mare)))
      )
    ),
    
    # =====================================================
    # 📊 MAIN PANEL
    # =====================================================
    mainPanel(
      
      # =========================
      # VALUE BOXES (KPIs)
      # =========================
      div(class = "value-box-container",
          
          div(class = "value-box blue",
              h3(textOutput("kpi_capturas")),
              p("Total de Registos")
          ),
          
          div(class = "value-box green",
              h3(textOutput("kpi_peso")),
              p("Peso Total (Kg)")
          ),
          
          div(class = "value-box orange",
              h3(textOutput("kpi_especies")),
              p("Espécies")
          ),
          
          div(class = "value-box yellow",
              h3(textOutput("kpi_pescadores")),
              p("Pescadores")
          )
      ),
      
      br(),
      
      # =========================
      # GRÁFICOS 1
      # =========================
      fluidRow(
        
        column(
          6,
          plotlyOutput("grafico_especies", height = "380px")
        ),
        
        column(
          6,
          plotlyOutput("grafico_artes", height = "380px")
        )
      ),
      
      br(),
      
      # =========================
      # GRÁFICOS 2
      # =========================
      fluidRow(
        
        column(
          6,
          plotlyOutput("grafico_mare", height = "380px")
        ),
        
        column(
          6,
          plotlyOutput("grafico_embarcacao", height = "380px")
        )
      ),
      
      br(),
      
      # =========================
      # EXTRA (OPCIONAL MAS RECOMENDADO)
      # =========================
      fluidRow(
        
        column(
          12,
          plotlyOutput("grafico_pescadores", height = "420px")
        )
      )
    )
  )
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {
  
  # ==========================
  # DADOS FILTRADOS (BASE)
  # ==========================
  dados <- reactive({
    
    df <- Baseline_Raizes
    
    if (input$filtro_ciclo != "Todos") {
      df <- df %>% filter(Ciclo == input$filtro_ciclo)
    }
    
    if (input$filtro_tipo_avaliacao != "Todos") {
      df <- df %>% filter(Tipo_Avaliacao == input$filtro_tipo_avaliacao)
    }
    
    if (input$filtro_local != "Todos") {
      df <- df %>% filter(Local_Entrevista == input$filtro_local)
    }
    
    if (input$filtro_comunidade != "Todos") {
      df <- df %>% filter(Comunidade == input$filtro_comunidade)
    }
    
    df
  })
  
  # ========================================================
  # KPI AGENCIA
  # ========================================================
  output$kpi_agencia <- renderUI({
    
    df <- dados()
    
    div(class = "value-box-container",
        
        div(class = "value-box blue",
            span(class = "value-number", nrow(df)),
            span(class = "value-title", "Participantes")),
        
        div(class = "value-box green",
            span(class = "value-number", n_distinct(df$Ciclo)),
            span(class = "value-title", "Ciclos")),
        
        div(class = "value-box orange",
            span(class = "value-number", nrow(df)),
            span(class = "value-title", "Registos"))
    )
  })
  
  # ========================================================
  # PERFIL
  # ========================================================
  output$grafico_sexo <- renderPlotly({
    
    df <- dados()
    
    req("Sexo" %in% colnames(df))
    req(nrow(df) > 0)
    
    df_resumo <- df %>%
      group_by(Sexo) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Percentagem = round(Total / sum(Total) * 100, 1),
        label = paste0(Sexo, ": ", Percentagem, "%")
      )
    
    plot_ly(
      data = df_resumo,
      labels = ~Sexo,
      values = ~Total,
      type = "pie",
      textinfo = "percent",
      insidetextorientation = "radial",
      hole = 0.55,
      marker = list(
        # 2 cores fixas para sexo
        colors = c("#9442d4", "#ff7f0e"),
        line = list(color = "#FFFFFF", width = 2)
      )
    ) %>%
      layout(
        title = "",
        showlegend = TRUE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4"
      )
  })
  
  
  
  
   output$grafico_estado_civil <- renderPlotly({
    
    df <- dados()
    
    req("Estado_Civil" %in% colnames(df))
    req(nrow(df) > 0)
    
    df_resumo <- df %>%
      group_by(Estado_Civil) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Percentagem = round(Total / sum(Total) * 100, 1),
        label = paste0(Estado_Civil, ": ", Percentagem, "%")
      )
    
    plot_ly(
      data = df_resumo,
      labels = ~Estado_Civil,
      values = ~Total,
      type = "pie",
      textinfo = "percent",
      insidetextorientation = "radial",
      hole = 0.55,
      marker = list(
        colors = c('#9442d4', '#ff7f0e', '#69C7BE', '#FFD700', '#1f77b4', '#2ca02c'),
        line = list(color = "#FFFFFF", width = 2)
      )
    ) %>%
      layout(
        title = "",
        showlegend = TRUE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4"
      )
  })
  
  output$grafico_setor <- renderPlotly({
    df <- dados() %>% count(Sector)
    
    plot_ly(df, x = ~Sector, y = ~n, type = "bar")
  })
  
  # ========================================================
  # IDADE
  # ========================================================
  output$grafico_idade <- renderPlotly({
    df <- dados() %>%
      mutate(Idade = as.numeric(Idade)) %>%
      filter(!is.na(Idade)) %>%
      mutate(grupo = ifelse(Idade <= 35, "<=35", ">35")) %>%
      count(grupo)
    
    plot_ly(df, x = ~grupo, y = ~n, type = "bar")
  })
  
  # ========================================================
  # PROTECÇÃO
  # ========================================================
  
  output$grafico_decisao_geral <- renderPlotly({
    
    df <- dados()
    
    req(nrow(df) > 0)
    
    vars_decisao <- c(
      "Decisao_Actividades_Economicas",
      "Decisao_Educacao",
      "Decisao_Futuro_Profissional",
      "Decisao_Movimentos",
      "Decisao_Pequenas_Despesas",
      "Decisao_Grandes_Despesas"
    )
    
    # Transformar base
    df_long <- df %>%
      select(all_of(vars_decisao)) %>%
      pivot_longer(
        everything(),
        names_to = "Variavel",
        values_to = "Resposta"
      )
    
    # Ordem respostas
    ordem_respostas <- c(
      "Concordo totalmente",
      "Concordo parcialmente",
      "Não concordo nem discordo",
      "Discordo parcialmente",
      "Discordo totalmente"
    )
    
    # Preparar dados
    df_plot <- df_long %>%
      filter(!is.na(Resposta)) %>%
      group_by(Variavel, Resposta) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Variavel) %>%
      mutate(
        pct = round((n / sum(n)) * 100, 1),
        texto = ifelse(pct >= 5, paste0(pct, "%"), "")
      )
    
    # Ordem
    df_plot$Resposta <- factor(
      df_plot$Resposta,
      levels = ordem_respostas
    )
    
    # Labels bonitas
    df_plot$Variavel <- recode(
      df_plot$Variavel,
      "Decisao_Actividades_Economicas" = "Actividades Económicas",
      "Decisao_Educacao" = "Educação",
      "Decisao_Futuro_Profissional" = "Futuro Profissional",
      "Decisao_Movimentos" = "Mobilidade",
      "Decisao_Pequenas_Despesas" = "Pequenas Despesas",
      "Decisao_Grandes_Despesas" = "Grandes Despesas"
    )
    
    # Cores
    cores_respostas <- c(
      "Concordo totalmente" = "#9442d4",
      "Concordo parcialmente" = "#ff7f0e",
      "Não concordo nem discordo" = "#69C7BE",
      "Discordo parcialmente" = "#FFD700",
      "Discordo totalmente" = "#1f77b4"
    )
    
    plot_ly(
      data = df_plot,
      
      y = ~Variavel,
      x = ~pct,
      
      color = ~Resposta,
      colors = cores_respostas,
      
      type = "bar",
      orientation = "h",
      
      # VALORES NAS BARRAS
      text = ~texto,
      textposition = "inside",
      
      # TAMANHO TEXTO
      textfont = list(
        color = "white",
        size = 15
      ),
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "%{fullData.name}<br>",
        "%{x}%<extra></extra>"
      )
      
    ) %>%
      layout(
        
        barmode = "stack",
        
        uniformtext = list(
          minsize = 10,
          mode = "show"
        ),
        
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        
        xaxis = list(
          title = "Percentagem (%)",
          range = c(0, 100)
        ),
        
        yaxis = list(
          title = ""
        ),
        
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.12,
          title = list(text = "")
        )
      )
  })
  
  
  
  cores_respostas <- c(
    "Concordo totalmente" = "#9442d4",
    "Concordo parcialmente" = "#ff7f0e",
    "Não concordo nem discordo" = "#69C7BE",
    "Discordo parcialmente" = "#FFD700",
    "Discordo totalmente" = "#1f77b4"
  )
  
  ordem_respostas <- c(
    "Concordo totalmente",
    "Concordo parcialmente",
    "Não concordo nem discordo",
    "Discordo parcialmente",
    "Discordo totalmente"
  )
  
 
  
  output$grafico_decisao_economica <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Actividades_Economicas) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1)
      )
    
    df$Decisao_Actividades_Economicas <- factor(
      df$Decisao_Actividades_Economicas,
      levels = ordem_respostas
    )
    
    plot_ly(
      df,
      x = ~Decisao_Actividades_Economicas,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Actividades_Economicas,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Actividades_Economicas"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
  output$grafico_Decisao_Educacao <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Educacao) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    df$Decisao_Educacao <- factor(df$Decisao_Educacao, levels = ordem_respostas)
    
    plot_ly(
      df,
      x = ~Decisao_Educacao,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Educacao,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      texttemplate = "%{text}",
      textposition = "inside",
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Educacao"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
  output$grafico_Decisao_Futuro_Profissional <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Futuro_Profissional) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    df$Decisao_Futuro_Profissional <- factor(df$Decisao_Futuro_Profissional, levels = ordem_respostas)
    
    plot_ly(
      df,
      x = ~Decisao_Futuro_Profissional,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Futuro_Profissional,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      texttemplate = "%{text}",
      textposition = "inside",
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Futuro_Profissional"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  

  output$grafico_Decisao_Movimentos <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Movimentos) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    df$Decisao_Movimentos <- factor(df$Decisao_Movimentos, levels = ordem_respostas)
    
    plot_ly(
      df,
      x = ~Decisao_Movimentos,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Movimentos,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      texttemplate = "%{text}",
      textposition = "inside",
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Movimentos"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
 
  output$grafico_Decisao_Pequenas_Despesas <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Pequenas_Despesas) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1)
      )
    
    df$Decisao_Pequenas_Despesas <- factor(
      df$Decisao_Pequenas_Despesas,
      levels = ordem_respostas
    )
    
    plot_ly(
      df,
      x = ~Decisao_Pequenas_Despesas,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Pequenas_Despesas,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Pequenas_Despesas"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
  output$grafico_Decisao_Grandes_Despesas <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Decisao_Grandes_Despesas) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1)
      )
    
    df$Decisao_Grandes_Despesas <- factor(
      df$Decisao_Grandes_Despesas,
      levels = ordem_respostas
    )
    
    plot_ly(
      df,
      x = ~Decisao_Grandes_Despesas,
      y = ~pct,
      type = "bar",
      color = ~Decisao_Grandes_Despesas,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Decisao_Grandes_Despesas"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  

  # ========================================================
  # NORMAS
  # ========================================================
  output$grafico_Violencia_Domestica_geral <- renderPlotly({
    
    df <- dados()
    
    req(nrow(df) > 0)
    
    vars_violencia <- c(
      "Violencia_Domestica_Aceitavel",
      "Violencia_Assunto_Privado"
    )
    
    # Transformar base
    df_long <- df %>%
      select(all_of(vars_violencia)) %>%
      pivot_longer(
        everything(),
        names_to = "Variavel",
        values_to = "Resposta"
      )
    
    # Ordem respostas
    ordem_respostas <- c(
      "Concordo totalmente",
      "Concordo parcialmente",
      "Não concordo nem discordo",
      "Discordo parcialmente",
      "Discordo totalmente"
    )
    
    # Preparar dados
    df_plot <- df_long %>%
      filter(!is.na(Resposta)) %>%
      group_by(Variavel, Resposta) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Variavel) %>%
      mutate(
        pct = round((n / sum(n)) * 100, 1),
        texto = ifelse(pct >= 5, paste0(pct, "%"), "")
      )
    
    # Ordem
    df_plot$Resposta <- factor(
      df_plot$Resposta,
      levels = ordem_respostas
    )
    
    # Labels bonitas
    df_plot$Variavel <- recode(
      df_plot$Variavel,
      "Violencia_Domestica_Aceitavel" = "Mulher Aceitando a violência doméstica",
      "Violencia_Assunto_Privado" = "Violência Assunto Privado"
    )
    
    # Cores
    cores_respostas <- c(
      "Concordo totalmente" = "#9442d4",
      "Concordo parcialmente" = "#ff7f0e",
      "Não concordo nem discordo" = "#69C7BE",
      "Discordo parcialmente" = "#FFD700",
      "Discordo totalmente" = "#1f77b4" 
    )
    
    plot_ly(
      data = df_plot,
      
      y = ~Variavel,
      x = ~pct,
      
      color = ~Resposta,
      colors = cores_respostas,
      
      type = "bar",
      orientation = "h",
      
      # VALORES NAS BARRAS
      text = ~texto,
      textposition = "inside",
      
      # TAMANHO TEXTO
      textfont = list(
        color = "white",
        size = 15
      ),
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "%{fullData.name}<br>",
        "%{x}%<extra></extra>"
      )
      
    ) %>%
      layout(
        
        barmode = "stack",
        
        uniformtext = list(
          minsize = 10,
          mode = "show"
        ),
        
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        
        xaxis = list(
          title = "Percentagem (%)",
          range = c(0, 100)
        ),
        
        yaxis = list(
          title = ""
        ),
        
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.12,
          title = list(text = "")
        )
      )
  })
  
  
  output$grafico_Violencia_Domestica_Aceitavel <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Violencia_Domestica_Aceitavel) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1)
      )
    
    df$Violencia_Domestica_Aceitavel <- factor(
      df$Violencia_Domestica_Aceitavel,
      levels = ordem_respostas
    )
    
    plot_ly(
      df,
      x = ~Violencia_Domestica_Aceitavel,
      y = ~pct,
      type = "bar",
      color = ~Violencia_Domestica_Aceitavel,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Violencia_Domestica_Aceitavel"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
  output$grafico_Violencia_Assunto_Privado <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    df <- df %>%
      count(Violencia_Assunto_Privado) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1)
      )
    
    df$Violencia_Assunto_Privado <- factor(
      df$Violencia_Assunto_Privado,
      levels = ordem_respostas
    )
    
    plot_ly(
      df,
      x = ~Violencia_Assunto_Privado,
      y = ~pct,
      type = "bar",
      color = ~Violencia_Assunto_Privado,
      colors = cores_respostas,
      
      text = ~paste0(pct, "%"),
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(color = "white", size = 12)
      
    ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Violencia_Assunto_Privado"),
        yaxis = list(title = "Percentagem (%)")
      )
  })
  
  
  output$grafico_Gostaria_Ser_Lider <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    # Contagem
    df_plot <- df %>%
      count(Gostaria_Ser_Lider, Sexo) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1),
        
        label = paste0(
          n,
          "<br>",
          pct,
          "%"
        )
      )
    
    # Ordem das respostas
    df_plot$Gostaria_Ser_Lider <- factor(
      df_plot$Gostaria_Ser_Lider,
      levels = c(
        "Já ocupo uma posição de liderança",
        "Não",
        "Provavelmente não",
        "Provavelmente sim",
        "Sim"
      )
    )
    
    plot_ly(
      data = df_plot,
      
      x = ~Gostaria_Ser_Lider,
      y = ~pct,
      
      type = "bar",
      
      color = ~Sexo,
      
      colors = c(
        "Masculino" = "#ff7f0e",
        "Feminino" = "#9442d4"
      ),
      
      text = ~label,
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(
        color = "white",
        size = 11
      )
      
    ) %>%
      
      layout(
        
        barmode = "group",
        
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        
        xaxis = list(
          title = "Gostaria de Ser Líder"
        ),
        
        yaxis = list(
          title = "Percentagem Total (%)",
          ticksuffix = "%"
        ),
        
        legend = list(
          title = list(text = "<b>Sexo</b>")
        )
      )
  })
 
  
  output$grafico_Mulheres_Lideranca_Frequencia <- renderPlotly({
    
    df <- dados()
    req(nrow(df) > 0)
    
    # Contagem
    df_plot <- df %>%
      count(Mulheres_Lideranca_Frequencia, Sexo) %>%
      mutate(
        pct = round(n / sum(n) * 100, 1),
        
        label = paste0(
          n,
          "<br>",
          pct,
          "%"
        )
      )
    
    # Ordem das respostas
    df_plot$Mulheres_Lideranca_Frequencia <- factor(
      df_plot$Mulheres_Lideranca_Frequencia,
      levels = c(
        "Nunca",
        "Raramente",
        "Algumas vezes",
        "Frequentemente"
      )
    )
    
    plot_ly(
      data = df_plot,
      
      x = ~Mulheres_Lideranca_Frequencia,
      y = ~pct,
      
      type = "bar",
      
      color = ~Sexo,
      
      colors = c(
        "Masculino" = "#ff7f0e",
        "Feminino" = "#9442d4"
      ),
      
      text = ~label,
      
      texttemplate = "%{text}",
      textposition = "inside",
      
      insidetextanchor = "middle",
      
      textfont = list(
        color = "white",
        size = 11
      )
      
    ) %>%
      
      layout(
        
        barmode = "group",
        
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        
        xaxis = list(
          title = "Com que frequência as mulheres são selecionadas para posições de liderança"
        ),
        
        yaxis = list(
          title = "Percentagem Total (%)",
          ticksuffix = "%"
        ),
        
        legend = list(
          title = list(text = "<b>Sexo</b>")
        )
      )
  })
  
  
  
  # ========================================================
  # PARTICIPAÇÃO
  # ========================================================
  output$grafico_participacao <- renderPlotly({
    df <- dados() %>% count(Participacao_Comunitaria)
    
    plot_ly(df, x = ~Participacao_Comunitaria, y = ~n, type = "bar")
  })
  
  # ========================================================
  # MONITORIA
  # ========================================================
  
  observe({
    req(input$distritoInput_namp_pi)
    
    df <- Presencas_Nexus
    if (input$distritoInput_namp_pi != "TODOS") {
      df <- df %>% filter(Distrito == input$distritoInput_namp_pi)
    }
    
    comunidades <- c("TODAS", sort(unique(df$Comunidade)))
    
    updateSelectInput(
      session,
      "comunidadeInput_namp_pi",
      choices = comunidades,
      selected = "TODAS"
    )
  })
  
  observe({
    req(input$comunidadeInput_namp_pi)
    
    df <- Presencas_Nexus
    if (input$distritoInput_namp_pi != "TODOS") df <- df %>% filter(Distrito == input$distritoInput_namp_pi)
    if (input$comunidadeInput_namp_pi != "TODAS") df <- df %>% filter(Comunidade == input$comunidadeInput_namp_pi)
    
    facilitadores <- c("TODOS", sort(unique(df$Facilitadores)))
    
    updateSelectInput(
      session,
      "facilitadorInput_namp_pi",
      choices = facilitadores,
      selected = "TODOS"
    )
  })
  
  
  dados_filtrados_presencas <- reactive({
    df <- Presencas_Nexus
    if (input$distritoInput_namp_pi != "TODOS") df <- df %>% filter(Distrito == input$distritoInput_namp_pi)
    if (input$comunidadeInput_namp_pi != "TODAS") df <- df %>% filter(Comunidade == input$comunidadeInput_namp_pi)
    if (!is.null(input$facilitadorInput_namp_pi) && input$facilitadorInput_namp_pi != "TODOS") df <- df %>% filter(Facilitadores == input$facilitadorInput_namp_pi)
    df
  })
  
  
  output$graficoParticipacaoGlobal <- renderPlotly({
    df <- dados_filtrados_presencas() 
    
    if (nrow(df) == 0) {
      showNotification("Nenhum dado disponível para os filtros selecionados.", type = "warning")
      return(NULL)
    }
    
    
    sessao_cols <- names(df)[grepl("^Sessão_?\\d+$", names(df))]
    sessao_cols_ordenadas <- sessao_cols[order(as.numeric(gsub("Sessão_?", "", sessao_cols)))]
    
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = all_of(sessao_cols_ordenadas),
        names_to = "Sessao",
        values_to = "Presenca"
      ) %>%
      filter(Presenca == "Presente") %>%
      group_by(Sessao, Sexo) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(
        Count = as.numeric(Count),
        Sessao_Num = as.numeric(gsub("Sessão_?", "", Sessao))
      ) %>%
      arrange(Sessao_Num) %>%
      mutate(Sessao = factor(Sessao, levels = sessao_cols_ordenadas))
    
    totais_sessao <- df_long %>%
      group_by(Sessao) %>%
      summarise(total = sum(Count), .groups = "drop")
    
    linha_referencia <- if (input$distritoInput_namp_pi == "TODOS") 400 else 200
    
    df_long <- df_long %>%
      group_by(Sessao) %>%
      arrange(Sexo) %>%
      mutate(
        y0 = cumsum(lag(Count, default = 0)),
        y_center = y0 + Count / 2
      )
    
    annotations_segmentos <- lapply(1:nrow(df_long), function(i) {
      list(
        x = df_long$Sessao[i],
        y = df_long$y_center[i],
        text = as.character(df_long$Count[i]),
        showarrow = FALSE,
        font = list(size = 12, color = "white")
      )
    })
    
    annotations_totais <- lapply(1:nrow(totais_sessao), function(i) {
      list(
        x = totais_sessao$Sessao[i],
        y = totais_sessao$total[i] + 10,
        text = paste("", totais_sessao$total[i]),
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    })
    
    all_annotations <- c(annotations_segmentos, annotations_totais)
    
    plot_ly(
      data = df_long,
      x = ~Sessao,
      y = ~Count,
      color = ~Sexo,
      colors = c("Feminino" = "#9942D4", "Masculino" = "#F77333"),
      type = "bar",
      hovertemplate = "%{x}<br>Sexo: %{color}<br>Presenças: %{y}<extra></extra>"
    ) %>%
      layout(
        title = "",
        barmode = "stack",
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = ""),
        yaxis = list(title = "Número de Presenças"),
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = length(unique(df_long$Sessao)) + 1,
            y0 = linha_referencia,
            y1 = linha_referencia,
            line = list(color = "purple", dash = "dash", width = 2)
          )
        ),
        annotations = all_annotations
      )
  })
  
  output$texto_participacao_sessoes <- renderUI({
    
    df <- dados_filtrados_presencas()
    
    # ================================
    # 🎯 META DINÂMICA
    # ================================
    meta_total <- if (input$distritoInput_namp_pi == "TODOS") 400 else 200
    
    sessoes_cols <- names(df)[grepl("^Sessão_?\\d+$", names(df))]
    
    sessoes_data <- df[, sessoes_cols]
    
    presencas_por_sessao <- colSums(sessoes_data == "Presente", na.rm = TRUE)
    
    sessao_max <- names(which.max(presencas_por_sessao))
    valor_max <- max(presencas_por_sessao)
    
    sessao_min <- names(which.min(presencas_por_sessao))
    valor_min <- min(presencas_por_sessao)
    
    sessoes_atingiram <- names(presencas_por_sessao[presencas_por_sessao >= meta_total])
    
    media_sessoes <- mean(presencas_por_sessao)
    
    # ================================
    # 📌 CASO 1: TODOS
    # ================================
    if (input$distritoInput_namp_pi == "TODOS") {
      
      texto <- paste0(
        
        "A análise global do programa, considerando todos os distritos, ",
        "define uma meta de <b>", meta_total, "</b> participantes por sessão. ",
        
        "A sessão com maior participação foi <b>", sessao_max, "</b> com <b>", valor_max, "</b> presenças, ",
        "enquanto a menor participação ocorreu na <b>", sessao_min, "</b> com <b>", valor_min, "</b> presenças. ",
        
        if (length(sessoes_atingiram) > 0) {
          paste0("A(s) sessão(ões) que atingiu(aram) a meta foram: <b>",
                 paste(sessoes_atingiram, collapse = ", "),
                 "</b>. ")
        } else {
          "Nenhuma sessão atingiu a meta estabelecida. "
        },
        
        "Em média, as sessões registaram <b>", round(media_sessoes, 1), "</b> presenças."
      )
      
    } else {
      
      # ================================
      # 📌 CASO 2: DISTRITO SELECIONADO
      # ================================
      
      distrito <- input$distritoInput_namp_pi
      
      total_participantes <- nrow(df)
      
      texto <- paste0(
        
        "No distrito de <b>", distrito, "</b>, a análise das sessões ",
        "considera uma meta de <b>", meta_total, "</b> participantes por sessão. ",
        
        "Registam-se <b>", total_participantes, "</b> participantes no universo filtrado. ",
        
        "A sessão com maior participação foi <b>", sessao_max, "</b> com <b>", valor_max, "</b> presenças, ",
        "enquanto a menor participação ocorreu na <b>", sessao_min, "</b> com <b>", valor_min, "</b> presenças. ",
        
        if (length(sessoes_atingiram) > 0) {
          paste0("A(s) sessão(ões) que atingiu(aram) a meta foram: <b>",
                 paste(sessoes_atingiram, collapse = ", "),
                 "</b>. ")
        } else {
          "Nenhuma sessão atingiu a meta estabelecida. "
        },
        
        "Em média, as sessões registaram <b>", round(media_sessoes, 1), "</b> presenças no distrito."
      )
    }
    
    HTML(paste0(
      "<div style='background:#f5f3f4; padding:12px; border-radius:6px;'>",
      texto,
      "</div>"
    ))
  })
  # ###################### PARTICIPACAO POR SEXO ##################  
  # 
  output$graficoParticipacaoSexo <- renderPlotly({

    df <- dados_filtrados_presencas()

    if (nrow(df) == 0) {
      showNotification("Nenhum dado disponível para os filtros selecionados.", type = "warning")
      return(NULL)
    }

    sessao_cols <- names(df)[grepl("^Sessão_?\\d+$", names(df))]
    sessao_cols_ordenadas <- sessao_cols[order(as.numeric(gsub("Sessão_?", "", sessao_cols)))]

    previstos <- df %>%
      group_by(Sexo) %>%
      summarise(Previsto = n(), .groups = "drop")

    df_long <- df %>%
      tidyr::pivot_longer(
        cols = all_of(sessao_cols_ordenadas),
        names_to = "Sessao",
        values_to = "Presenca"
      ) %>%
      filter(Presenca == "Presente") %>%
      group_by(Sessao, Sexo) %>%
      summarise(Count = n(), .groups = "drop") %>%
      left_join(previstos, by = "Sexo") %>%
      mutate(
        Porcentagem = Count / Previsto * 100
      )

    df_long <- df_long %>%
      mutate(
        Sessao_Num = as.numeric(gsub("Sessão_?", "", Sessao)),
        Sessao = factor(Sessao, levels = sessao_cols_ordenadas)
      ) %>%
      arrange(Sessao_Num)

    df_long <- df_long %>%
      mutate(textpos = ifelse(Sexo == "Feminino", "top center", "bottom center"))


    cores_legenda <- c("Feminino" = "#9942D4", "Masculino" = "#F77333")

    max_porcentagem <- max(df_long$Porcentagem, na.rm = TRUE)
    limite_y <- ifelse(max_porcentagem + 10 > 100, max_porcentagem + 10, 110)

    plot_ly(
      data = df_long,
      x = ~Sessao,
      y = ~Porcentagem,
      type = 'scatter',
      mode = 'lines+markers+text',
      color = ~Sexo,
      colors = cores_legenda,
      text = ~paste0(round(Porcentagem,1), "%"),
      textposition = ~textpos,
      marker = list(size = 10),
      line = list(width = 3),
      hovertemplate = "%{x}<br>Sexo: %{color}<br>Percentual: %{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = "",
          font = list(size = 16, face = "bold")
        ),
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Sessão", tickfont = list(size = 12)),
        yaxis = list(title = "Percentual (%)", range = c(0, limite_y), tickfont = list(size = 12)),
        legend = list(title = list(text = "<b>Sexo</b>"))
      )
  })

  output$texto_participacao_sexo <- renderUI({

    df <- dados_filtrados_presencas()

    sessoes_cols <- names(df)[grepl("^Sessão_?\\d+$", names(df))]

    previstos <- df %>%
      dplyr::count(Sexo) %>%
      dplyr::rename(Previsto = n)

    df_long <- df %>%
      tidyr::pivot_longer(
        cols = all_of(sessoes_cols),
        names_to = "Sessao",
        values_to = "Presenca"
      ) %>%
      dplyr::filter(Presenca == "Presente") %>%
      dplyr::group_by(Sessao, Sexo) %>%
      dplyr::summarise(Count = n(), .groups = "drop") %>%
      dplyr::left_join(previstos, by = "Sexo") %>%
      dplyr::mutate(Porcentagem = (Count / Previsto) * 100)

    # médias por sexo ao longo das sessões
    media_sexo <- df_long %>%
      dplyr::group_by(Sexo) %>%
      dplyr::summarise(media = mean(Porcentagem, na.rm = TRUE))

    sessoes_medias <- df_long %>%
      dplyr::group_by(Sessao) %>%
      dplyr::summarise(total = sum(Count), .groups = "drop")

    sessao_max <- sessoes_medias %>% dplyr::slice_max(total, n = 1)
    sessao_min <- sessoes_medias %>% dplyr::slice_min(total, n = 1)

    texto <- paste0(

      "O gráfico apresenta a evolução da participação por sessão, desagregada por sexo, ",
      "permitindo analisar o comportamento de adesão ao longo do processo formativo. ",

      "Em média, as mulheres registam <b>", round(media_sexo$media[media_sexo$Sexo == "Feminino"], 1), "%</b> ",
      "de participação e os homens <b>", round(media_sexo$media[media_sexo$Sexo == "Masculino"], 1), "%</b>. ",

      "A sessão com maior participação global é <b>", sessao_max$Sessao, "</b>, ",
      "enquanto a menor participação ocorre na <b>", sessao_min$Sessao, "</b>, ",
      "indicando variações no nível de engajamento ao longo das sessões."
    )

    HTML(paste0(
      "<div style='background:#f5f3f4; padding:12px; border-radius:6px;'>",
      texto,
      "</div>"
    ))
  })
  # 
  # ################################ ACOMPANHAMENTO ################################## 
  # 
  # 
  # =====================================================
  # 🎨 FUNÇÃO PONTOS
  # =====================================================
  formatar_pontos <- function(x) {
    sapply(x, function(valor) {

      if (is.na(valor) || valor == "") {
        '<span style="color: grey; font-size: 40px;">&#9679;</span>'

      } else if (valor == "Presente") {
        '<span style="color: purple; font-size: 40px;">&#9679;</span>'

      } else if (valor == "Ausente") {
        '<span style="color: red; font-size: 40px;">&#9679;</span>'

      } else {
        '<span style="color: grey; font-size: 40px;">&#9679;</span>'
      }
    })
  }

  # =====================================================
  # 🔁 UPDATE COMUNIDADE
  # =====================================================
  observeEvent(input$distritoInput_, {

    df <- Presencas_Nexus

    comunidades <- if (input$distritoInput_ == "TODOS") {
      sort(unique(df$Comunidade))
    } else {
      sort(unique(df$Comunidade[df$Distrito == input$distritoInput_]))
    }

    updateSelectInput(
      session,
      "comunidadeAcompanhamento",
      choices = c("TODAS", comunidades),
      selected = "TODAS"
    )
  })

  # =====================================================
  # 🔁 UPDATE FACILITADOR
  # =====================================================
  observeEvent(
    list(input$distritoInput_, input$comunidadeAcompanhamento),
    {

      df <- Presencas_Nexus

      if (input$distritoInput_ != "TODOS") {
        df <- df %>% dplyr::filter(Distrito == input$distritoInput_)
      }

      if (input$comunidadeAcompanhamento != "TODAS") {
        df <- df %>% dplyr::filter(Comunidade == input$comunidadeAcompanhamento)
      }

      facilitadores <- sort(unique(df$Facilitadores))

      updateSelectInput(
        session,
        "facilitadorInput",
        choices = c("TODOS", facilitadores),
        selected = "TODOS"
      )
    },
    ignoreInit = TRUE
  )

  # =====================================================
  # 📊 COLUNAS DE SESSÕES
  # =====================================================
  col_sessoes <- names(Presencas_Nexus)[grepl("^Sessão_?\\d+$", names(Presencas_Nexus))]
  col_sessoes <- col_sessoes[order(as.numeric(gsub("Sessão_?", "", col_sessoes)))]

  # =====================================================
  # 📊 DADOS FILTRADOS + QUALIDADE (OPÇÃO 2)
  # =====================================================
  dados_filtered <- reactive({

    df <- Presencas_Nexus

    if (input$distritoInput_ != "TODOS") {
      df <- df %>% dplyr::filter(Distrito == input$distritoInput_)
    }

    if (input$comunidadeAcompanhamento != "TODAS") {
      df <- df %>% dplyr::filter(Comunidade == input$comunidadeAcompanhamento)
    }

    if (input$facilitadorInput != "TODOS") {
      df <- df %>% dplyr::filter(Facilitadores == input$facilitadorInput)
    }

    total_sessoes <- length(col_sessoes)

    df <- df %>%
      dplyr::mutate(

        sessoes_preenchidas = rowSums(
          dplyr::across(all_of(col_sessoes), ~ !is.na(.) & . != ""),
          na.rm = TRUE
        ),

        score = round((sessoes_preenchidas / total_sessoes) * 100, 1),
        score = ifelse(score > 100, 100, score),

        # =========================
        # 🚦 QUALIDADE (OPÇÃO 2)
        # =========================
        qualidade = dplyr::case_when(
          score == 100 ~ "Excelente",
          score >= 80 ~ "Bom",
          score >= 60 ~ "Médio",
          TRUE ~ "Crítico"
        )
      )

    df <- df[rowSums(df[col_sessoes] == "Presente", na.rm = TRUE) > 0, ]

    df
  })

  # =====================================================
  # 🎨 LEGENDA
  # =====================================================
  output$pontosPresenca <- renderUI({

    HTML(paste0(
      '<span style="color: purple; font-size: 25px;">&#9679;</span> Presente &nbsp;&nbsp;',
      '<span style="color: red; font-size: 25px;">&#9679;</span> Ausente &nbsp;&nbsp;',
      '<span style="color: grey; font-size: 25px;">&#9679;</span> Não Preenchido'
    ))
  })

  # =====================================================
  # 🧠 TEXTO EXPLICATIVO
  # =====================================================
  output$texto_presencas <- renderUI({

    df <- dados_filtered()

    total <- nrow(df)
    media <- round(mean(df$score, na.rm = TRUE), 1)

    criticos <- sum(df$qualidade == "Crítico")
    excelentes <- sum(df$qualidade == "Excelente")

    facilitadores_criticos <- df %>%
      dplyr::group_by(Facilitadores) %>%
      dplyr::summarise(media = mean(score, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(media < 60) %>%
      dplyr::pull(Facilitadores)

    txt_fac <- if (length(facilitadores_criticos) == 0) {
      "Nenhum facilitador crítico identificado."
    } else {
      paste(facilitadores_criticos, collapse = ", ")
    }

    div(
      style = "background-color:#f5f3f4; padding:12px; border-radius:6px;",

      tags$p(
        style = "margin:0; text-align:justify;",

        tags$b("📊 Qualidade de Dados — Presenças Individuais: "),

        "Foram analisados ", tags$b(total), " participantes. ",
        "A taxa média de qualidade é de ", tags$b(paste0(media, "%")), ". ",

        tags$br(), tags$br(),

        "🟢 Excelentes: ", tags$b(excelentes),
        " | 🟡 Bom/Médio/Crítico distribuídos no sistema. ",

        tags$br(), tags$br(),

        "🔴 Críticos: ", tags$b(criticos),

        tags$br(), tags$br(),

        tags$b("⚠️ Facilitadores com baixa qualidade de registo: "),
        txt_fac,

        tags$br(), tags$br(),

        "O indicador de qualidade segue uma escala de desempenho: ",
        "Excelente (100%), Bom (≥80%), Médio (≥60%) e Crítico (<60%). ",
        "Este painel permite monitoria contínua da qualidade dos dados e identificação de riscos operacionais."
      )
    )
  })
  
  # # =====================================================
  # # 📋 TABELA (CRÍTICOS PRIMEIRO)
  # # =====================================================
  output$tabelaPresencas <- renderDataTable({

    df <- dados_filtered()

    df[col_sessoes] <- lapply(df[col_sessoes], as.character)
    df[col_sessoes] <- lapply(df[col_sessoes], formatar_pontos)

    df$qualidade <- factor(
      df$qualidade,
      levels = c("Crítico", "Médio", "Bom", "Excelente")
    )

    datatable(
      df[order(df$qualidade), c(
        "Comunidade",
        "Nome_participante",
        "score",
        "qualidade",
        col_sessoes
      )],

      escape = FALSE,
      rownames = FALSE,

      options = list(
        pageLength = 10,
        dom = "lfrtip",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
  
  # ####### Participantes que concluiram a formacao PI 
  # 
  # ================================
  # 📊 BASE FILTRADA
  # ================================
  dados_filtrados <- reactive({

    df <- Presencas_Nexus

    if (input$distritoInput_ != "TODOS") {
      df <- df %>% dplyr::filter(Distrito == input$distritoInput_)
    }

    if (input$comunidadeAcompanhamento != "TODAS") {
      df <- df %>% dplyr::filter(Comunidade == input$comunidadeAcompanhamento)
    }

    if (input$facilitadorInput != "TODOS") {
      df <- df %>% dplyr::filter(Facilitadores == input$facilitadorInput)
    }

    df
  })

  # ================================
  # 📊 CLASSIFICAÇÃO DE CONCLUSÃO
  # ================================
  participantes_concluintes <- reactive({

    df <- dados_filtrados()

    sessoes <- df %>%
      dplyr::select(starts_with("Sessão"))

    df$total_presencas <- rowSums(sessoes == "Presente", na.rm = TRUE)

    df$concluiu <- ifelse(df$total_presencas >= 8, "Concluiu", "Não concluiu")

    df
  })

  # =====================================================
  # 📊 GRÁFICO 1 — CONCLUINTES POR DISTRITO E SEXO
  # =====================================================
  dados_grafico <- reactive({

    participantes_concluintes() %>%
      dplyr::filter(concluiu == "Concluiu") %>%
      dplyr::count(Distrito, Sexo) %>%
      dplyr::group_by(Distrito) %>%
      dplyr::mutate(
        percent = (n / sum(n)) * 100,
        label = paste0(n, "<br>", round(percent, 1), "%")
      ) %>%
      dplyr::ungroup()
  })

  limite_y <- reactive({
    max(dados_grafico()$percent, na.rm = TRUE) * 1.2
  })

  output$grafico_N <- renderPlotly({

    plot_ly(
      data = dados_grafico(),
      x = ~Distrito,
      y = ~percent,
      color = ~Sexo,
      colors = c("Feminino" = "#9942D4", "Masculino" = "#F77333"),
      type = "bar",
      text = ~label,
      textposition = "outside",
      cliponaxis = FALSE
    ) %>%
      layout(
        title = list(
          text = "",
          font = list(size = 16)
        ),
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(
          title = "Distrito",
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = "Percentual (%)",
          range = c(0, limite_y()),
          tickfont = list(size = 12)
        ),
        legend = list(title = list(text = "<b>Sexo</b>")),
        barmode = "group"
      )
  })

  output$texto_grafico_N <- renderUI({

    df_base <- participantes_concluintes() %>%
      dplyr::filter(concluiu == "Concluiu")

    distrito_sel <- input$distritoInput_

    # ================================
    # 📌 CASO 1: TODOS
    # ================================
    if (distrito_sel == "TODOS") {

      total_geral <- nrow(df_base)

      distritos <- df_base %>%
        dplyr::count(Distrito) %>%
        dplyr::mutate(percent = (n / sum(n)) * 100) %>%
        dplyr::arrange(desc(n))

      top <- distritos %>% dplyr::slice(1)

      sexo_geral <- df_base %>%
        dplyr::count(Sexo) %>%
        dplyr::mutate(percent = (n / sum(n)) * 100)

      fem <- sexo_geral$percent[sexo_geral$Sexo == "Feminino"]
      masc <- sexo_geral$percent[sexo_geral$Sexo == "Masculino"]

      texto <- paste0(
        "Consideram-se concluintes todos os participantes que participaram em pelo menos 8 das 12 sessões previstas. ",
        "No total, registam-se <b>", total_geral, "</b> concluintes (",
        round(sexo_geral$n[sexo_geral$Sexo == "Feminino"]), " mulheres e ",
        round(sexo_geral$n[sexo_geral$Sexo == "Masculino"]), " homens). ",

        "O distrito com maior representação é <b>", top$Distrito, "</b> com <b>", round(top$percent, 1), "%</b>."
      )

    } else {

      # ================================
      # 📌 CASO 2: DISTRITO SELECIONADO
      # ================================

      df_dist <- df_base %>%
        dplyr::filter(Distrito == distrito_sel)

      total_dist <- nrow(df_dist)

      sexo_dist <- df_dist %>%
        dplyr::count(Sexo) %>%
        dplyr::mutate(percent = (n / sum(n)) * 100)

      fem <- sexo_dist$percent[sexo_dist$Sexo == "Feminino"]
      masc <- sexo_dist$percent[sexo_dist$Sexo == "Masculino"]

      texto <- paste0(
        "No total, registam-se <b>", total_dist, "</b> concluintes do distrito de <b>", distrito_sel, "</b>, ",
        "com <b>", round(fem, 1), "%</b> feminino e <b>", round(masc, 1), "%</b> masculino."
      )
    }

    HTML(paste0(
      "<div style='background:#f5f3f4; padding:12px; border-radius:6px;'>",
      texto,
      "</div>"
    ))
  })
  # 
  # 
  # # =====================================================
  # # 📊 GRÁFICO 2 — SITUAÇÃO DOS CONCLUINTES
  # # =====================================================
  dados_situacao <- reactive({

    df <- participantes_concluintes() %>%
      dplyr::filter(concluiu == "Concluiu")

    total_geral <- nrow(df)

    df %>%
      dplyr::count(Situacao_Participante, Sexo) %>%
      dplyr::mutate(
        percent_global = (n / total_geral) * 100,
        label = paste0(n, " (", round(percent_global, 1), "%)")
      )
  })

  limite_y_situacao <- reactive({
    100
  })

  output$grafico_situacao_C <- renderPlotly({

    plot_ly(
      data = dados_situacao(),
      x = ~Situacao_Participante,
      y = ~n,   # ✔ valores reais
      color = ~Sexo,
      colors = c("Feminino" = "#9942D4", "Masculino" = "#F77333"),
      type = "bar",

      text = ~label,

      textposition = "inside",
      insidetextanchor = "middle",

      textfont = list(
        size = 12,
        color = "white"
      ),

      hovertemplate = paste(
        "<b>Situação:</b> %{x}<br>",
        "<b>Sexo:</b> %{color}<br>",
        "<b>Valor:</b> %{y}<br>",
        "<b>% do total geral:</b> %{customdata:.1f}%<extra></extra>"
      ),

      customdata = ~percent_global
    ) %>%
      layout(
        title = list(text = ""),

        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",

        xaxis = list(
          title = "Situação do Participante",
          tickangle = -25
        ),

        yaxis = list(
          title = "Número de Participantes"
        ),

        legend = list(title = list(text = "<b>Sexo</b>")),
        barmode = "stack"
      )
  })

  output$texto_situacao_interpretacao <- renderUI({

    df_base <- participantes_concluintes() %>%
      dplyr::filter(concluiu == "Concluiu")

    distrito_sel <- input$distritoInput_

    # ================================
    # 📌 CASO 1: TODOS
    # ================================
    if (distrito_sel == "TODOS") {

      total_geral <- nrow(df_base)

      sexo_geral <- df_base %>%
        dplyr::count(Sexo)

      situacao_geral <- df_base %>%
        dplyr::count(Situacao_Participante) %>%
        dplyr::mutate(percent = (n / sum(n)) * 100)

      texto <- paste0(
        "No total, registam-se <b>", total_geral, "</b> concluintes ",
        "(<b>", sexo_geral$n[sexo_geral$Sexo == "Feminino"], "</b> mulheres e ",
        "<b>", sexo_geral$n[sexo_geral$Sexo == "Masculino"], "</b> homens). ",

        "Em termos de situação dos concluintes, observa-se a seguinte distribuição: ",
        paste0(
          situacao_geral$Situacao_Participante,
          " (", round(situacao_geral$percent, 1), "%)",
          collapse = ", "
        ),
        "."
      )

    } else {

      # ================================
      # 📌 CASO 2: DISTRITO SELECIONADO
      # ================================

      df_dist <- df_base %>%
        dplyr::filter(Distrito == distrito_sel)

      total_dist <- nrow(df_dist)

      sexo_dist <- df_dist %>%
        dplyr::count(Sexo)

      situacao_dist <- df_dist %>%
        dplyr::count(Situacao_Participante) %>%
        dplyr::mutate(percent = (n / sum(n)) * 100)

      texto <- paste0(
        "No distrito de <b>", distrito_sel, "</b>, registam-se <b>", total_dist, "</b> concluintes ",
        "(<b>", sexo_dist$n[sexo_dist$Sexo == "Feminino"], "</b> mulheres e ",
        "<b>", sexo_dist$n[sexo_dist$Sexo == "Masculino"], "</b> homens). ",

        "Em termos de situação dos concluintes, observa-se a seguinte distribuição: ",
        paste0(
          situacao_dist$Situacao_Participante,
          " (", round(situacao_dist$percent, 1), "%)",
          collapse = ", "
        ),
        "."
      )
    }

    HTML(paste0(
      "<div style='background:#f5f3f4; padding:12px; border-radius:6px;'>",
      texto,
      "</div>"
    ))
  })

  # ========================================================
  # OCEAN GUARD (PLACEHOLDER – ajustar depois)
  # ========================================================

  dados_filtrados <- reactive({
    
    df <- dados_ocean
    
    if (!is.null(input$pescador) && input$pescador != "Todos") {
      df <- df %>% dplyr::filter(pescador == input$pescador)
    }
    
    if (!is.null(input$centro) && input$centro != "Todos") {
      df <- df %>% dplyr::filter(centro_pesca == input$centro)
    }
    
    if (!is.null(input$arte) && input$arte != "Todos") {
      df <- df %>% dplyr::filter(tipo_arte == input$arte)
    }
    
    if (!is.null(input$mare) && input$mare != "Todas") {
      df <- df %>% dplyr::filter(tipo_mare == input$mare)
    }
    
    df
  })
  
  # =====================================================
  # 📌 KPIs
  # =====================================================
  
  output$kpi_capturas <- renderText({
    format(nrow(dados_filtrados()), big.mark = ",")
  })
  
  output$kpi_peso <- renderText({
    round(sum(dados_filtrados()$peso_gramas, na.rm = TRUE) / 1000, 1)
  })
  
  output$kpi_especies <- renderText({
    n_distinct(dados_filtrados()$latin_name)
  })
  
  output$kpi_pescadores <- renderText({
    n_distinct(dados_filtrados()$pescador)
  })
  
  # =====================================================
  # 🐟 TOP ESPÉCIES
  # =====================================================
  
  output$grafico_especies <- renderPlotly({
    
    df <- dados_filtrados() %>%
      group_by(xitswa_name) %>%
      summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(peso_total)) %>%
      slice_head(n = 10)
    
    plotly::plot_ly(
      df,
      x = ~reorder(xitswa_name, peso_total),
      y = ~peso_total,
      type = "bar",
      text = ~format(peso_total, big.mark = ","),
      textposition = "outside"
    ) %>%
      layout(
        title = list(text = ""),
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Espécie", tickangle = -25),
        yaxis = list(title = "Peso (gramas)"),
        barmode = "stack"
      )
  })
  
  # =====================================================
  # 🎣 TIPO DE ARTE
  # =====================================================
  
  output$grafico_artes <- renderPlotly({
    
    df <- dados_filtrados() %>% count(tipo_arte)
    
    plotly::plot_ly(
      df,
      labels = ~tipo_arte,
      values = ~n,
      type = "pie"
    ) %>%
      layout(
        title = list(text = ""),
        
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        
        legend = list(title = list(text = "<b>Tipo de Arte</b>"))
      )
  })
  
  # =====================================================
  # 🌊 MARÉ
  # =====================================================
  
  output$grafico_mare <- renderPlotly({
    
    df <- dados_filtrados() %>%
      group_by(tipo_mare) %>%
      summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop")
    
    plotly::plot_ly(
      df,
      x = ~tipo_mare,
      y = ~peso_total,
      type = "bar",
      text = ~format(peso_total, big.mark = ","),
      textposition = "outside"
    ) %>%
      layout(
        title = list(text = ""),
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        xaxis = list(title = "Maré", tickangle = -25),
        yaxis = list(title = "Peso Total"),
        barmode = "stack"
      )
  })
  
  # =====================================================
  # 🧭 EMBARCAÇÃO (EXTRA OPCIONAL)
  # =====================================================
  
  output$grafico_embarcacao <- renderPlotly({
    
    df <- dados_filtrados() %>%
      group_by(pescador) %>%
      summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop")
    
    plotly::plot_ly(
      df,
      x = ~pescador,
      y = ~peso_total,
      type = "bar",
      text = ~format(peso_total, big.mark = ","),
      textposition = "outside"
    ) %>%
      layout(
        title = list(text = ""),
        paper_bgcolor = "#f5f3f4",
        plot_bgcolor = "#f5f3f4",
        yaxis = list(title = "Peso Total")
      )
  })
  
  # =====================================================
  # 👤 PESCADORES (EXTRA OPCIONAL)
  # =====================================================
  
  output$grafico_embarcacao  <- renderPlotly({
    
    df <- dados_filtrados() %>%
      group_by(pescador) %>%
      summarise(
        peso_total = sum(peso_gramas, na.rm = TRUE),
        .groups = "drop"
      )
    
    plotly::plot_ly(
      df,
      x = ~pescador,
      y = ~peso_total,
      type = "bar"
    ) %>%
      
    layout(
            title = list(text = ""),

            paper_bgcolor = "#f5f3f4",
            plot_bgcolor = "#f5f3f4",

            legend = list(title = list(text = "<b>Captura por Pescador</b>"))
          )

  })
}

# ==========================================================
# RUN APP
# ==========================================================
shinyApp(ui, server)