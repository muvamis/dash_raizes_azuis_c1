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
    .yellow { background-color: #f9a825; }
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
    "BASELINE & ENDLINE",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          "filtro_tipo_avaliacao",
          "Avaliação:",
          choices = c("Todos", sort(unique(Baseline_Raizes$Tipo_Avaliacao))),
          selected = "Todos"
        ),
        
        selectInput(
          "filtro_local",
          "Distrito/Localidade:",
          choices = c("Todos", sort(unique(Baseline_Raizes$Distrito_Localidade))),
          selected = "Todos"
        ),
        
        selectInput(
          "filtro_comunidade",
          "Comunidade:",
          choices = c("Todos", sort(unique(Baseline_Raizes$Comunidade))),
          selected = "Todos"
        )
      ),
      
      mainPanel(
        
        fluidRow(uiOutput("kpi_agencia")),
        
        tabsetPanel(
          
          tabPanel("PERFIL",
                   fluidRow(
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_sexo")
                            ),
                            plotlyOutput("grafico_sexo")),
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_estado_civil")
                            ),
                            plotlyOutput("grafico_estado_civil"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_actividade_mar")
                            ), 
                            plotlyOutput("grafico_actividade_mar")),
                     
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_actividade_mar_detalhe")
                            ),
                            plotlyOutput("grafico_actividade_mar_detalhe"))
                   ),
                   br(),
                   # ============================================================
                   # LINHA 3 - ACTIVIDADE FORA DO MAR E ACTIVIDADE SECUNDÁRIA
                   # ============================================================
                   
                   fluidRow(
                     
                     column(
                       6,
                       div(
                         style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                         uiOutput("leitura_actividade_fora_mar_detalhe")
                       ),
                       plotlyOutput(
                         "grafico_actividade_fora_mar_detalhe",
                         height = "450px"
                       )
                     ),
                     
                     column(
                       6,
                       div(
                         style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                         uiOutput("leitura_actividade_secundaria_mar")
                       ),
                       plotlyOutput(
                         "grafico_actividade_secundaria_mar",
                         height = "450px"
                       )
                     )
                   )
          ),
          
          tabPanel("TOMADA DE DECISÕES",
                   
                   fluidRow(
                     column(
                       12,
                       
                       div(
                         style = "
        background-color:#eef4fb;
        border-left:5px solid #8054A2;
        padding:15px;
        border-radius:6px;
        margin-bottom:25px;
      ",
                         
                         tags$h4(
                           style = "margin-top:0; color:#8054A2;",
                           "TOMANDO DECISÕES"
                         ),
                         tags$p(
                           "Abaixo, temos várias frases sobre a tomada de decisões. ",
                           "Nós gostaríamos de saber o quanto que você concorda com as frases a seguir, ",
                           "pensando em seus relacionamentos. Vamos usar uma escala de 1 até 5."
                         ),
                         
                         tags$p(
                           strong("Use a escala: "),
                           "1 = Discordo totalmente, ",
                           "2 = Discordo parcialmente, ",
                           "3 = Não concordo nem discordo, ",
                           "4 = Concordo parcialmente, ",
                           "5 = Concordo totalmente."
                         ),
                         
                         tags$p(
                           "Não existem respostas certas ou erradas e pedimos que responda ",
                           "de acordo com seus sentimentos."
                         )
                       )
                     )
                   ),
                   # fluidRow(
                   #   column(12, plotlyOutput("grafico_decisao_geral", height = "650px"))
                   #   ),
                   # br(),
                   fluidRow(
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_poder_decidir")
                            ),
                            plotlyOutput("grafico_poder_decidir_economica")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_poder_educacao")
                            ),
                            plotlyOutput("grafico_poder_educacao"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_decide_futuro_profissional")
                            ),
                            plotlyOutput("grafico_decide_futuro_profissional")),
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_decide_movimentos")
                            ),
                            plotlyOutput("grafico_decide_movimentos"))
                   ),
                   br(),
                   fluidRow(
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_decide_grandes_despesas")
                            ),
                            plotlyOutput("grafico_decide_grandes_despesas")),
                     column(6,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_decide_pequenas_despesas")
                            ),
                            plotlyOutput("grafico_decide_pequenas_despesas"))
                   ),
                   br(),
                   fluidRow(
                     column(12,
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_quem_decide")
                            ),
                            plotlyOutput("grafico_quem_decide", height = "650px"))
                   )
          ),
                   
          tabPanel("NORMAS SOCIAIS",
                   
                   fluidRow(
                     column(
                       12,
                       
                       div(
                         style = "
        background-color:#eef4fb;
        border-left:5px solid #8054A2;
        padding:15px;
        border-radius:6px;
        margin-bottom:25px;
      ",
                         
                         tags$h4(
                           style = "margin-top:0; color:#8054A2;",
                           "NORMAS SOCIAIS"
                         ),
                         
                         tags$p(
                           "Nesta secção, temos várias perguntas sobre as normas sociais ",
                           "e sobre as perceções relacionadas com os papéis de mulheres e homens ",
                           "na comunidade."
                         ),
                         
                         tags$p(
                           "Queremos saber como você percebe estas situações na sua comunidade, ",
                           "incluindo questões relacionadas com liderança, tomada de decisões, ",
                           "respeito e relações entre mulheres e homens."
                         ),
                         
                         tags$p(
                           strong("Não existem respostas certas ou erradas. "),
                           "Responda de acordo com a sua opinião, experiência e percepção ",
                           "sobre o que acontece na sua comunidade."
                         )
                       )
                     )
                   ),
                   
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_deseja_lideranca")
                            ),
                            plotlyOutput("grafico_deseja_lideranca")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_mulheres_lideranca")
                            ),
                            plotlyOutput("grafico_mulheres_lideranca"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_aprovaria_mulher_liderar")
                            ),
                            plotlyOutput("grafico_aprovaria_mulher_liderar")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_quantas_aprovariam_mulher")
                            ),
                            plotlyOutput("grafico_quantas_aprovariam_mulher"))  
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_mulher_aceitar_violencia")
                            ),
                            plotlyOutput("grafico_mulher_aceitar_violencia")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_homem_bate_mulher")
                            ),
                            plotlyOutput("grafico_homem_bate_mulher"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_respeito_jovem")
                            ),
                            plotlyOutput("grafico_respeito_jovem")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_grupos_comunidade")
                            ),
                            plotlyOutput("grafico_grupos_comunidade"))
                   )
          ),
          
          tabPanel("CAPITAL SOCIAL E REDES",
                   fluidRow(
                     column(
                       12,
                       
                       div(
                         style = "
        background-color:#eef4fb;
        border-left:5px solid #8054A2;
        padding:15px;
        border-radius:6px;
        margin-bottom:25px;
      ",
                         
                         tags$h4(
                           style = "margin-top:0; color:#8054A2;",
                           "CAPITAL SOCIAL E REDES"
                         ),
                         
                         tags$p(
                           "Abaixo, temos várias frases sobre a sua relação com outras pessoas ",
                           "e sobre o apoio que recebe das pessoas próximas."
                         ),
                         
                         tags$p(
                           "Queremos saber como você percebe as suas relações com amigos, ",
                           "familiares e outras pessoas da sua comunidade, incluindo as pessoas ",
                           "com quem pode conversar, partilhar problemas e receber apoio."
                         ),
                         
                         tags$p(
                           strong("Não existem respostas certas ou erradas. "),
                           "Responda de acordo com os seus sentimentos, experiências ",
                           "e relações com as pessoas à sua volta."
                         ),
                         
                         tags$p(
                           strong("Para as frases de concordância, use a escala: "),
                           "1 = Discordo totalmente, ",
                           "2 = Discordo parcialmente, ",
                           "3 = Não concordo, nem discordo, ",
                           "4 = Concordo parcialmente, ",
                           "5 = Concordo totalmente."
                         )
                       )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoas_falar_sozinha")
                            ),
                            plotlyOutput("grafico_pessoas_falar_sozinha")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoas_discutir_problemas")
                            ),
                            plotlyOutput("grafico_pessoas_discutir_problemas"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_partilhar_alegrias")
                            ),
                            plotlyOutput("grafico_partilhar_alegrias")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_discutir_amigos")
                            ),
                            plotlyOutput("grafico_discutir_amigos"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_familiares_apoiam")
                            ),
                            plotlyOutput("grafico_familiares_apoiam")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_familia_apoia_decisao")
                            ),
                            plotlyOutput("grafico_familia_apoia_decisao"))
                   )
          ),
          
          tabPanel("AUTO-ESTIMA",
                   
                   fluidRow(
                     column(
                       12,
                       div(
                         style = "
        background-color:#eef4fb;
        border-left:5px solid #8054A2;
        padding:15px;
        border-radius:6px;
        margin-bottom:25px;
      ",
                         
                         tags$h4(
                           style = "margin-top:0; color:#8054A2;",
                           "AUTO-ESTIMA"
                         ),
                         
                         tags$p(
                           "Abaixo, temos várias frases sobre a forma como você se vê ",
                           "e sobre o valor que atribui a si mesma."
                         ),
                         
                         tags$p(
                           "Queremos saber como você se sente em relação às suas qualidades, ",
                           "ao seu valor pessoal e à forma como avalia a si mesma. ",
                           "Responda pensando nos seus próprios sentimentos e experiências."
                         ),
                         
                         tags$p(
                           strong("Não existem respostas certas ou erradas. "),
                           "Responda de acordo com aquilo que realmente pensa e sente sobre si mesma."
                         ),
                         
                         tags$p(
                           strong("Para as frases de concordância, use a escala: "),
                           "1 = Discordo totalmente, ",
                           "2 = Discordo parcialmente, ",
                           "3 = Não concordo, nem discordo, ",
                           "4 = Concordo parcialmente, ",
                           "5 = Concordo totalmente."
                         )
                       )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoa_valor")
                            ),
                            plotlyOutput("grafico_pessoa_valor")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoa_nao_presta")
                            ),
                            plotlyOutput("grafico_pessoa_nao_presta"))
                   ),
                   
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoa_fracassada")
                            ),
                            plotlyOutput("grafico_pessoa_fracassada")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_pessoa_boas_qualidades")
                            ),
                            plotlyOutput("grafico_pessoa_boas_qualidades"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_satisfeita_comigo")
                            ),
                            plotlyOutput("grafico_satisfeita_comigo")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_alcancar_objectivos")
                            ),
                            plotlyOutput("grafico_alcancar_objectivos"))
                   )
          ),
          
          tabPanel("AUTOEFICÁCIA",
                   fluidRow(
                     column(
                       12,
                       div(
                         style = "
        background-color:#eef4fb;
        border-left:5px solid #8054A2;
        padding:15px;
        border-radius:6px;
        margin-bottom:25px;
      ",
                         
                         tags$h4(
                           style = "margin-top:0; color:#8054A2;",
                           "AUTOEFICÁCIA"
                         ),
                         
                         tags$p(
                           "Nesta parte, temos várias situações do dia a dia e queremos saber ",
                           "até que ponto você acredita que consegue agir ou expressar-se ",
                           "nessas situações."
                         ),
                         
                         tags$p(
                           "Pense naquilo que você acredita que seria capaz de fazer, ",
                           "mesmo quando a situação envolve outras pessoas, discordância ",
                           "ou alguma dificuldade."
                         ),
                         
                         tags$p(
                           strong("Não existem respostas certas ou erradas. "),
                           "Escolha a opção que melhor representa aquilo que você acredita ",
                           "que consegue fazer."
                         ),
                         
                         tags$p(
                           strong("Use a escala de 1 a 5: "),
                           "1 = Definitivamente não posso fazer, ",
                           "2 = Provavelmente não posso fazer, ",
                           "3 = Talvez possa fazer, ",
                           "4 = Provavelmente posso fazer, ",
                           "5 = Completamente certa que posso fazer."
                         )
                       )
                     )
                   ),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_expressar_opiniao")
                            ),
                            plotlyOutput("grafico_expressar_opiniao")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_expressar_nao_concordando")
                            ),
                            plotlyOutput("grafico_expressar_nao_concordando"))
                   ),
                   
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_expressar_trabalho")
                            ),
                            plotlyOutput("grafico_expressar_trabalho")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_expressar_escola")
                            ),
                            plotlyOutput("grafico_expressar_escola"))
                   ),
                   br(),
                   fluidRow(
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_defender_injusticas")
                            ),
                            plotlyOutput("grafico_defender_injusticas")),
                     column(6, 
                            div(
                              style="background-color:#f5f3f4; padding:12px; border-radius:6px; margin-bottom:20px;",
                              uiOutput("leitura_expressar_infeliz")
                            ),
                            plotlyOutput("grafico_expressar_infeliz"))
                            )
          )
          
        )
      )
    )
  )
)

#   
#   # ========================================================
#   # 3. OCEAN GUARD
#   # ========================================================
#   tabPanel(
#     "Ocean Guard",
#     
#     # =====================================================
#     # 🎛️ FILTROS
#     # =====================================================
#     sidebarPanel(
#       
#       selectInput(
#         "pescador",
#         "Pescador:",
#         choices = c("Todos", sort(unique(dados_ocean$pescador)))
#       ),
#       
#       selectInput(
#         "centro",
#         "Centro de Pesca:",
#         choices = c("Todos", sort(unique(dados_ocean$centro_pesca)))
#       ),
#       
#       selectInput(
#         "arte",
#         "Tipo de Arte:",
#         choices = c("Todos", sort(unique(dados_ocean$tipo_arte)))
#       ),
#       
#       selectInput(
#         "mare",
#         "Tipo de Maré:",
#         choices = c("Todas", sort(unique(dados_ocean$tipo_mare)))
#       )
#     ),
#     
#     # =====================================================
#     # 📊 MAIN PANEL
#     # =====================================================
#     mainPanel(
#       
#       # =========================
#       # VALUE BOXES (KPIs)
#       # =========================
#       div(class = "value-box-container",
#           
#           div(class = "value-box blue",
#               h3(textOutput("kpi_capturas")),
#               p("Total de Registos")
#           ),
#           
#           div(class = "value-box green",
#               h3(textOutput("kpi_peso")),
#               p("Peso Total (Kg)")
#           ),
#           
#           div(class = "value-box orange",
#               h3(textOutput("kpi_especies")),
#               p("Espécies")
#           ),
#           
#           div(class = "value-box yellow",
#               h3(textOutput("kpi_pescadores")),
#               p("Pescadores")
#           )
#       ),
#       
#       br(),
#       
#       # =========================
#       # GRÁFICOS 1
#       # =========================
#       fluidRow(
#         
#         column(
#           6,
#           plotlyOutput("grafico_especies", height = "380px")
#         ),
#         
#         column(
#           6,
#           plotlyOutput("grafico_artes", height = "380px")
#         )
#       ),
#       
#       br(),
#       
#       # =========================
#       # GRÁFICOS 2
#       # =========================
#       fluidRow(
#         
#         column(
#           6,
#           plotlyOutput("grafico_mare", height = "380px")
#         ),
#         
#         column(
#           6,
#           plotlyOutput("grafico_embarcacao", height = "380px")
#         )
#       ),
#       
#       br(),
#       
#       # =========================
#       # EXTRA (OPCIONAL MAS RECOMENDADO)
#       # =========================
#       fluidRow(
#         
#         column(
#           12,
#           plotlyOutput("grafico_pescadores", height = "420px")
#         )
#       )
#     )
#   )
# )

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {

  # ==========================
  # DADOS FILTRADOS
  # ==========================
  dados <- reactive({
    
    df <- Baseline_Raizes
    
    if (input$filtro_tipo_avaliacao != "Todos") {
      df <- df %>%
        filter(Tipo_Avaliacao == input$filtro_tipo_avaliacao)
    }
    
    if (input$filtro_local != "Todos") {
      df <- df %>%
        filter(Distrito_Localidade == input$filtro_local)
    }
    
    if (input$filtro_comunidade != "Todos") {
      df <- df %>%
        filter(Comunidade == input$filtro_comunidade)
    }
    
    df
  })
  
  # ========================================================
  # ATUALIZAR COMUNIDADES DE ACORDO COM O DISTRITO/LOCALIDADE
  # ========================================================
  observeEvent(input$filtro_local, {
    
    if (input$filtro_local == "Todos") {
      
      comunidades <- Baseline_Raizes %>%
        pull(Comunidade) %>%
        unique() %>%
        sort()
      
    } else {
      
      comunidades <- Baseline_Raizes %>%
        filter(Distrito_Localidade == input$filtro_local) %>%
        pull(Comunidade) %>%
        unique() %>%
        sort()
    }
    
    updateSelectInput(
      session,
      "filtro_comunidade",
      choices = c("Todos", comunidades),
      selected = "Todos"
    )
  })
  
  # ========================================================
  # KPI DINÂMICOS
  # ========================================================
  output$kpi_agencia <- renderUI({
    
    df <- dados()
    
    # Lista dos distritos/localidades
    locais <- sort(unique(Baseline_Raizes$Distrito_Localidade))
    
    # Remover NA
    locais <- locais[!is.na(locais)]
    
    # ======================================================
    # TOTAL DE PARTICIPANTES
    # ======================================================
    total_participantes <- nrow(df)
    
    
    # ======================================================
    # SE "TODOS" ESTIVER SELECIONADO
    # ======================================================
    if (input$filtro_local == "Todos") {
      
      # Totais por distrito/localidade
      totais_local <- Baseline_Raizes %>%
        group_by(Distrito_Localidade) %>%
        summarise(
          Total = n(),
          .groups = "drop"
        ) %>%
        filter(!is.na(Distrito_Localidade)) %>%
        arrange(Distrito_Localidade)
      
      # Primeiro local
      total_1 <- if (nrow(totais_local) >= 1) {
        totais_local$Total[1]
      } else {
        0
      }
      
      nome_1 <- if (nrow(totais_local) >= 1) {
        totais_local$Distrito_Localidade[1]
      } else {
        "Local 1"
      }
      
      # Segundo local
      total_2 <- if (nrow(totais_local) >= 2) {
        totais_local$Total[2]
      } else {
        0
      }
      
      nome_2 <- if (nrow(totais_local) >= 2) {
        totais_local$Distrito_Localidade[2]
      } else {
        "Local 2"
      }
      
      div(
        class = "value-box-container",
        
        div(
          class = "value-box green",
          span(
            class = "value-number",
            total_participantes
          ),
          span(
            class = "value-title",
            "Participantes"
          )
        ),
        
        div(
          class = "value-box purple",
          span(
            class = "value-number",
            total_1
          ),
          span(
            class = "value-title",
            nome_1
          )
        ),
        
        div(
          class = "value-box yellow",
          span(
            class = "value-number",
            total_2
          ),
          span(
            class = "value-title",
            nome_2
          )
        )
      )
      
    } else {
      
      # ====================================================
      # DISTRITO/LOCALIDADE SELECIONADO
      # ====================================================
      
      feminino <- df %>%
        filter(
          !is.na(Sexo),
          str_to_upper(str_squish(Sexo)) == "FEMININO"
        ) %>%
        nrow()
      
      masculino <- df %>%
        filter(
          !is.na(Sexo),
          str_to_upper(str_squish(Sexo)) == "MASCULINO"
        ) %>%
        nrow()
      
      
      div(
        class = "value-box-container",
        
        div(
          class = "value-box green",
          span(
            class = "value-number",
            total_participantes
          ),
          span(
            class = "value-title",
            input$filtro_local
          )
        ),
        
        div(
          class = "value-box blue",
          span(
            class = "value-number",
            feminino
          ),
          span(
            class = "value-title",
            "Feminino"
          )
        ),
        
        div(
          class = "value-box orange",
          span(
            class = "value-number",
            masculino
          ),
          span(
            class = "value-title",
            "Masculino"
          )
        )
      )
    }
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
  
  # ========================================================
  # LEITURA AUTOMÁTICA - SEXO
  # ========================================================
  output$leitura_sexo <- renderUI({
    
    df <- dados()
    
    req(nrow(df) > 0)
    
    resumo <- df %>%
      filter(!is.na(Sexo), Sexo != "") %>%
      count(Sexo, name = "Total") %>%
      mutate(
        Percentagem = round(Total / sum(Total) * 100, 1)
      )
    
    textos <- paste0(
      "<b>", resumo$Sexo, "</b>: ",
      resumo$Total, " participante(s) (",
      resumo$Percentagem, "%)"
    )
    
    div(
      class = "box-leitura",
      HTML(
        paste0(
          "<b>Sexo:</b> A distribuição dos participantes por sexo é composta por ",
          paste(textos, collapse = "; "),
          "."
        )
      )
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
        colors = c('#69C7BE', '#ffc107', '#1f77b4', '#8D6E63'),
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
  
   # ========================================================
   # LEITURA AUTOMÁTICA - ESTADO CIVIL
   # ========================================================
   output$leitura_estado_civil <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(!is.na(Estado_Civil), Estado_Civil != "") %>%
       count(Estado_Civil, name = "Total") %>%
       mutate(
         Percentagem = round(Total / sum(Total) * 100, 1)
       ) %>%
       arrange(desc(Total))
     
     textos <- paste0(
       "<b>", resumo$Estado_Civil, "</b>: ",
       resumo$Total, " participante(s) (",
       resumo$Percentagem, "%)"
     )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Estado_Civíl:</b> Em relação ao estado civil, ",
           paste(textos, collapse = "; "),
           "."
         )
       )
     )
   })
   
   output$grafico_actividade_mar <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Principal_Actividade_Esta_Ligada_Mar),
         Principal_Actividade_Esta_Ligada_Mar != "",
         !is.na(Sexo),
         Sexo != ""
       ) %>%
       mutate(
         Resposta = str_to_title(
           str_squish(Principal_Actividade_Esta_Ligada_Mar)
         )
       ) %>%
       count(
         Sexo,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Sexo) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           Total,
           "\n(",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c("Sim", "Não")
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Sexo,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Sexo: ", Sexo,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack") +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       
       scale_fill_manual(
         values = c(
           "Não" = "#69C7BE", 
           "Sim" = "#ffc107"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = ""
       ) +
       
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         )
       )
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_actividade_mar <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Principal_Actividade_Esta_Ligada_Mar),
         Principal_Actividade_Esta_Ligada_Mar != "",
         !is.na(Sexo),
         Sexo != ""
       ) %>%
       mutate(
         Resposta = str_to_title(
           str_squish(Principal_Actividade_Esta_Ligada_Mar)
         )
       ) %>%
       count(Sexo, Resposta, name = "Total") %>%
       group_by(Sexo) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     textos <- resumo %>%
       arrange(Sexo, Resposta) %>%
       mutate(
         texto = paste0(
           "<b>", Sexo, "</b>: ",
           Resposta, " = ",
           Total, " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Actividade ligada ao Mar:</b> Em relação à ligação da principal actividade ao mar, ",
           paste(textos, collapse = "; "),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - ACTIVIDADES LIGADAS AO MAR
   # ============================================================
   
   output$grafico_actividade_mar_detalhe <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     df_resumo <- df %>%
       filter(
         !is.na(Actividade_Mar),
         Actividade_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           str_detect(
             str_to_lower(Actividade_Mar),
             "marisqueira"
           ) ~ "Marisqueira",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "outra"
           ) ~ "Outra",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "pesca"
           ) ~ "Pesca artesanal",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "turismo"
           ) ~ "Turismo do mar",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "vendedora"
           ) ~ "Venda de peixe/marisco",
           
           TRUE ~ Actividade_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         ),
         Actividade = reorder(
           Actividade,
           Total
         )
       )
     
     req(nrow(df_resumo) > 0)
     
     # ----------------------------------------------------------
     # Gráfico
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Total,
         y = Actividade,
         text = paste0(
           "Actividade: ", Actividade,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         fill = "#ffc107",
         width = 0.7
       ) +
       
       geom_text(
         aes(label = label),
         hjust = -0.1,
         size = 3.5
       ) +
       
       scale_x_continuous(
         expand = expansion(
           mult = c(0, 0.15)
         )
       ) +
       
       labs(
         title = "",
         x = "Número de participantes",
         y = ""
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.major.y = element_blank(),
         
         panel.grid.minor = element_blank(),
         
         axis.text.y = element_text(
           size = 10
         ),
         
         axis.text.x = element_text(
           size = 9
         )
       )
     
     # ----------------------------------------------------------
     # Plotly
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # TEXTO DE LEITURA
   # ============================================================
   
   output$leitura_actividade_mar_detalhe <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Resumo
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Actividade_Mar),
         Actividade_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           str_detect(
             str_to_lower(Actividade_Mar),
             "marisqueira"
           ) ~ "Marisqueira",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "outra"
           ) ~ "Outra",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "pesca"
           ) ~ "Pesca artesanal",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "turismo"
           ) ~ "Turismo do mar",
           
           str_detect(
             str_to_lower(Actividade_Mar),
             "vendedora"
           ) ~ "Venda de peixe/marisco",
           
           TRUE ~ Actividade_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     # ----------------------------------------------------------
     # Actividade mais frequente
     # ----------------------------------------------------------
     
     maior <- resumo %>%
       slice(1)
     
     # ----------------------------------------------------------
     # Texto de cada actividade
     # ----------------------------------------------------------
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>",
           Actividade,
           "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     # ----------------------------------------------------------
     # Leitura final
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Qual é a sua actividade principal ligada ao mar?:</b> A actividade mais frequente é ",
           "<b>",
           maior$Actividade,
           "</b>, com ",
           maior$Total,
           " participante(s) (",
           round(maior$Percentagem, 1),
           "%). ",
           
           "A distribuição das actividades é: ",
           
           paste(
             textos,
             collapse = "; "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - ACTIVIDADE SECUNDÁRIA LIGADA AO MAR
   # ============================================================
   
   output$grafico_actividade_secundaria_mar <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     df_resumo <- df %>%
       filter(
         !is.na(Outra_Act_Fora_Mar),
         Outra_Act_Fora_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "não|nenhuma"
           ) ~ "Não tem actividade secundária",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "vendo peixe|vendo.*marisco|venda"
           ) ~ "Venda de peixe/marisco",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "apanhar marisco|apanha.*marisco"
           ) ~ "Apanha de marisco",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "turismo"
           ) ~ "Turismo do mar",
           
           TRUE ~ Outra_Act_Fora_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         ),
         
         Actividade = reorder(
           Actividade,
           Total
         )
       )
     
     req(nrow(df_resumo) > 0)
     
     # ----------------------------------------------------------
     # Gráfico
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Total,
         y = Actividade,
         text = paste0(
           "Actividade: ", Actividade,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         fill = "#69C7BE",
         width = 0.7
       ) +
       
       geom_text(
         aes(label = label),
         hjust = -0.1,
         size = 3.5
       ) +
       
       scale_x_continuous(
         expand = expansion(
           mult = c(0, 0.20)
         )
       ) +
       
       labs(
         title = "",
         x = "Número de participantes",
         y = ""
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.major.y = element_blank(),
         
         panel.grid.minor = element_blank(),
         
         axis.text.y = element_text(
           size = 10
         ),
         
         axis.text.x = element_text(
           size = 9
         )
       )
     
     # ----------------------------------------------------------
     # Plotly
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # TEXTO DE LEITURA
   # ============================================================
   
   output$leitura_actividade_secundaria_mar <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Resumo
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Outra_Act_Fora_Mar),
         Outra_Act_Fora_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "não|nenhuma"
           ) ~ "Não tem actividade secundária",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "vendo peixe|vendo.*marisco|venda"
           ) ~ "Venda de peixe/marisco",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "apanhar marisco|apanha.*marisco"
           ) ~ "Apanha de marisco",
           
           str_detect(
             str_to_lower(Outra_Act_Fora_Mar),
             "turismo"
           ) ~ "Turismo do mar",
           
           TRUE ~ Outra_Act_Fora_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     # ----------------------------------------------------------
     # Actividade mais frequente
     # ----------------------------------------------------------
     
     maior <- resumo %>%
       slice(1)
     
     # ----------------------------------------------------------
     # Texto de cada actividade
     # ----------------------------------------------------------
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>",
           Actividade,
           "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     # ----------------------------------------------------------
     # Leitura final
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Além da sua actividade principal, tem alguma actividade ",
           "secundária ligada ao mar?:</b> ",
           
           "A maioria dos participantes ",
           "(", 
           maior$Total,
           " ou ",
           round(maior$Percentagem, 1),
           "%) ",
           maior$Actividade,
           ". ",
           
           "A distribuição é: ",
           
           paste(
             textos,
             collapse = "; "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - ACTIVIDADE FORA DO MAR
   # ============================================================
   
   output$grafico_actividade_fora_mar_detalhe <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     df_resumo <- df %>%
       filter(
         !is.na(Actividade_Fora_Mar),
         Actividade_Fora_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "machamba|agricultura"
           ) ~ "Agricultura / machamba",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "barraca|negócio no mercado|venda ambulante"
           ) ~ "Barraca / negócio / venda ambulante",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "serviços|cabeleireira|costureira|cozinheira|restauração|moagem|transporte|pedreiro"
           ) ~ "Serviços",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "emprego com contrato"
           ) ~ "Emprego com contrato",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "estudante"
           ) ~ "Estudante",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "sem trabalho|à procura de trabalho"
           ) ~ "Sem trabalho / à procura de trabalho",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "outra"
           ) ~ "Outra",
           
           TRUE ~ Actividade_Fora_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         ),
         
         Actividade = reorder(
           Actividade,
           Total
         )
       )
     
     req(nrow(df_resumo) > 0)
     
     # ----------------------------------------------------------
     # Gráfico
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Total,
         y = Actividade,
         text = paste0(
           "Actividade: ", Actividade,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         fill = "#69C7BE",
         width = 0.7
       ) +
       
       geom_text(
         aes(label = label),
         hjust = -0.1,
         size = 3.5
       ) +
       
       scale_x_continuous(
         expand = expansion(
           mult = c(0, 0.15)
         )
       ) +
       
       labs(
         title = "",
         x = "Número de participantes",
         y = ""
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.major.y = element_blank(),
         
         panel.grid.minor = element_blank(),
         
         axis.text.y = element_text(
           size = 10
         ),
         
         axis.text.x = element_text(
           size = 9
         )
       )
     
     # ----------------------------------------------------------
     # Plotly
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # TEXTO DE LEITURA
   # ============================================================
   
   output$leitura_actividade_fora_mar_detalhe <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Resumo
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Actividade_Fora_Mar),
         Actividade_Fora_Mar != ""
       ) %>%
       mutate(
         Actividade = case_when(
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "machamba|agricultura"
           ) ~ "Agricultura / machamba",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "barraca|negócio no mercado|venda ambulante"
           ) ~ "Barraca / negócio / venda ambulante",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "serviços|cabeleireira|costureira|cozinheira|restauração|moagem|transporte|pedreiro"
           ) ~ "Serviços",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "emprego com contrato"
           ) ~ "Emprego com contrato",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "estudante"
           ) ~ "Estudante",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "sem trabalho|à procura de trabalho"
           ) ~ "Sem trabalho / à procura de trabalho",
           
           str_detect(
             str_to_lower(Actividade_Fora_Mar),
             "outra"
           ) ~ "Outra",
           
           TRUE ~ Actividade_Fora_Mar
         )
       ) %>%
       count(
         Actividade,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     # ----------------------------------------------------------
     # Actividade mais frequente
     # ----------------------------------------------------------
     
     maior <- resumo %>%
       slice(1)
     
     # ----------------------------------------------------------
     # Texto de cada actividade
     # ----------------------------------------------------------
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>",
           Actividade,
           "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     # ----------------------------------------------------------
     # Leitura final
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Qual é a sua principal actividade fora do mar?:</b> ",
           
           "A actividade mais frequente é ",
           "<b>",
           maior$Actividade,
           "</b>, com ",
           maior$Total,
           " participante(s) (",
           round(maior$Percentagem, 1),
           "%). ",
           
           "A distribuição das actividades é: ",
           
           paste(
             textos,
             collapse = "; "
           ),
           "."
         )
       )
     )
   })
   
  ############################## PAGINATOMADA DE DECISAO
  

  
  # ========================================================
  # PROTECÇÃO
  # ========================================================
  
  # output$grafico_decisao_geral <- renderPlotly({
  #   
  #   df <- dados()
  #   
  #   req(nrow(df) > 0)
  #   
  #   vars_decisao <- c(
  #     "Decisao_Actividades_Economicas",
  #     "Decisao_Educacao",
  #     "Decisao_Futuro_Profissional",
  #     "Decisao_Movimentos",
  #     "Decisao_Pequenas_Despesas",
  #     "Decisao_Grandes_Despesas"
  #   )
  #   
  #   # Transformar base
  #   df_long <- df %>%
  #     select(all_of(vars_decisao)) %>%
  #     pivot_longer(
  #       everything(),
  #       names_to = "Variavel",
  #       values_to = "Resposta"
  #     )
  #   
  #   # Ordem respostas
  #   ordem_respostas <- c(
  #     "Concordo totalmente",
  #     "Concordo parcialmente",
  #     "Não concordo nem discordo",
  #     "Discordo parcialmente",
  #     "Discordo totalmente"
  #   )
  #   
  #   # Preparar dados
  #   df_plot <- df_long %>%
  #     filter(!is.na(Resposta)) %>%
  #     group_by(Variavel, Resposta) %>%
  #     summarise(n = n(), .groups = "drop") %>%
  #     group_by(Variavel) %>%
  #     mutate(
  #       pct = round((n / sum(n)) * 100, 1),
  #       texto = ifelse(pct >= 5, paste0(pct, "%"), "")
  #     )
  #   
  #   # Ordem
  #   df_plot$Resposta <- factor(
  #     df_plot$Resposta,
  #     levels = ordem_respostas
  #   )
  #   
  #   # Labels bonitas
  #   df_plot$Variavel <- recode(
  #     df_plot$Variavel,
  #     "Decisao_Actividades_Economicas" = "Actividades Económicas",
  #     "Decisao_Educacao" = "Educação",
  #     "Decisao_Futuro_Profissional" = "Futuro Profissional",
  #     "Decisao_Movimentos" = "Mobilidade",
  #     "Decisao_Pequenas_Despesas" = "Pequenas Despesas",
  #     "Decisao_Grandes_Despesas" = "Grandes Despesas"
  #   )
  #   
  #   # Cores
  #   cores_respostas <- c(
  #     "Concordo totalmente" = "#9442d4",
  #     "Concordo parcialmente" = "#ff7f0e",
  #     "Não concordo nem discordo" = "#69C7BE",
  #     "Discordo parcialmente" = "#FFD700",
  #     "Discordo totalmente" = "#1f77b4"
  #   )
  #   
  #   plot_ly(
  #     data = df_plot,
  #     
  #     y = ~Variavel,
  #     x = ~pct,
  #     
  #     color = ~Resposta,
  #     colors = cores_respostas,
  #     
  #     type = "bar",
  #     orientation = "h",
  #     
  #     # VALORES NAS BARRAS
  #     text = ~texto,
  #     textposition = "inside",
  #     
  #     # TAMANHO TEXTO
  #     textfont = list(
  #       color = "white",
  #       size = 15
  #     ),
  #     
  #     hovertemplate = paste(
  #       "<b>%{y}</b><br>",
  #       "%{fullData.name}<br>",
  #       "%{x}%<extra></extra>"
  #     )
  #     
  #   ) %>%
  #     layout(
  #       
  #       barmode = "stack",
  #       
  #       uniformtext = list(
  #         minsize = 10,
  #         mode = "show"
  #       ),
  #       
  #       paper_bgcolor = "#f5f3f4",
  #       plot_bgcolor = "#f5f3f4",
  #       
  #       xaxis = list(
  #         title = "Percentagem (%)",
  #         range = c(0, 100)
  #       ),
  #       
  #       yaxis = list(
  #         title = ""
  #       ),
  #       
  #       legend = list(
  #         orientation = "h",
  #         x = 0,
  #         y = 1.12,
  #         title = list(text = "")
  #       )
  #     )
  # })
  # 
  # 
  # 
  # cores_respostas <- c(
  #   "Concordo totalmente" = "#9442d4",
  #   "Concordo parcialmente" = "#ff7f0e",
  #   "Não concordo nem discordo" = "#69C7BE",
  #   "Discordo parcialmente" = "#FFD700",
  #   "Discordo totalmente" = "#1f77b4"
  # )
  # 
  # ordem_respostas <- c(
  #   "Concordo totalmente",
  #   "Concordo parcialmente",
  #   "Não concordo nem discordo",
  #   "Discordo parcialmente",
  #   "Discordo totalmente"
  # )
  # 
  # 

   # ============================================================
   # PODER DE DECIDIR SOBRE A ACTIVIDADE ECONÓMICA
   # ============================================================
   
   output$grafico_poder_decidir_economica <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica),
         Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       
       # --------------------------------------------------------
     # APENAS PERCENTAGEM DENTRO DAS BARRAS
     # --------------------------------------------------------
     
     geom_text(
       aes(label = label),
       position = position_stack(
         vjust = 0.5
       ),
       size = 3.5
     ) +
       
       
       # --------------------------------------------------------
     # EIXO Y = 100%
     # --------------------------------------------------------
     
     scale_y_continuous(
       limits = c(0, 100),
       breaks = seq(0, 100, 20),
       labels = function(x) {
         paste0(x, "%")
       }
     ) +
       
       
       # --------------------------------------------------------
     # CORES
     # --------------------------------------------------------
     
     scale_fill_manual(
       values = c(
         "Concordo totalmente" = "#ffc107",
         "Concordo parcialmente" = "#F77333",
         "Não concordo, nem discordo" = "#BDBDBD",
         "Discordo parcialmente" = "#42A5F5",
         "Discordo totalmente" = "#69C7BE"
       )
     ) +
       
       
       # --------------------------------------------------------
     # TÍTULOS
     # --------------------------------------------------------
     
     labs(
       x = "",
       y = "Percentagem",
       fill = "Resposta"
     ) +
       
       
       # --------------------------------------------------------
     # TEMA
     # --------------------------------------------------------
     
     theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(
           size = 10
         ),
         
         axis.text.y = element_text(
           size = 9
         ),
         
         legend.title = element_text(
           face = "bold"
         ),
         # Legenda por baixo
         legend.position = "bottom"
       )
       
     
     
     # ----------------------------------------------------------
     # CONVERTER PARA PLOTLY
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   # ============================================================
   # LEITURA POR TIPO DE AVALIAÇÃO
   # ============================================================
   
   output$leitura_poder_decidir <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica),
         Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_Tenho_Poder_Decidir_Sobre_Actividade_Economica
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre as minhas actividades económicas (se trabalho, onde, que tipo de trabalho):</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # PODER SOBRE EDUCAÇÃO
   # ============================================================
   
   output$grafico_poder_educacao <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_Tenho_Poder_Sobre_Educacao),
         Eu_Tenho_Poder_Sobre_Educacao != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_Tenho_Poder_Sobre_Educacao
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # De baixo para cima
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       # Apenas percentagens dentro das barras
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       # Eixo Y em 100%
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) {
           paste0(x, "%")
         }
       ) +
       
       # Cores
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(
           size = 10
         ),
         
         axis.text.y = element_text(
           size = 9
         ),
         
         legend.title = element_text(
           face = "bold"
         ),
         
         # Legenda por baixo
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     # ----------------------------------------------------------
     # PLOTLY
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   # ============================================================
   # LEITURA - PODER SOBRE EDUCAÇÃO
   # ============================================================
   
   output$leitura_poder_educacao <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Eu_Tenho_Poder_Sobre_Educacao),
         Eu_Tenho_Poder_Sobre_Educacao != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_Tenho_Poder_Sobre_Educacao
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     
     # Ordem da leitura
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # Criar texto por Tipo_Avaliacao
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre a minha educação/escolaridade.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
  
   # ============================================================
   # GRÁFICO - QUEM DECIDE SOBRE O FUTURO PROFISSIONAL
   # ============================================================
   
   output$grafico_decide_futuro_profissional <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Futuro_Profissional),
         Quem_Decide_Sobre_Futuro_Profissional != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Futuro_Profissional
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # De baixo para cima
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) {
           paste0(x, "%")
         }
       ) +
       
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(
           size = 10
         ),
         
         axis.text.y = element_text(
           size = 9
         ),
         
         legend.title = element_text(
           face = "bold"
         ),
         
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     # ----------------------------------------------------------
     # PLOTLY
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - QUEM DECIDE SOBRE O FUTURO PROFISSIONAL
   # ============================================================
   
   output$leitura_decide_futuro_profissional <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Futuro_Profissional),
         Quem_Decide_Sobre_Futuro_Profissional != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Futuro_Profissional
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre as escolhas ligadas ao meu futuro profissional (aspirações, percurso):</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - QUEM DECIDE SOBRE OS MOVIMENTOS
   # ============================================================
   
   output$grafico_decide_movimentos <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Movimentos),
         Quem_Decide_Sobre_Movimentos != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Movimentos
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) {
           paste0(x, "%")
         }
       ) +
       
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(
           size = 10
         ),
         
         axis.text.y = element_text(
           size = 9
         ),
         
         legend.title = element_text(
           face = "bold"
         ),
         
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     # ----------------------------------------------------------
     # PLOTLY
     # ----------------------------------------------------------
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - QUEM DECIDE SOBRE OS MOVIMENTOS
   # ============================================================
   
   output$leitura_decide_movimentos <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Movimentos),
         Quem_Decide_Sobre_Movimentos != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Movimentos
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre os meus movimentos (onde vou, quando, com quem, por quanto tempo).:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - QUEM DECIDE SOBRE GRANDES DESPESAS FAMILIARES
   # ============================================================
   
   output$grafico_decide_grandes_despesas <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Grandes_Despesas_familiares),
         Quem_Decide_Sobre_Grandes_Despesas_familiares != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Grandes_Despesas_familiares
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) {
           paste0(x, "%")
         }
       ) +
       
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 9),
         
         legend.title = element_text(face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - GRANDES DESPESAS FAMILIARES
   # ============================================================
   
   output$leitura_decide_grandes_despesas <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Grandes_Despesas_familiares),
         Quem_Decide_Sobre_Grandes_Despesas_familiares != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Grandes_Despesas_familiares
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre as GRANDES despesas do agregado familiar (ex.:comprar terra, casa, geleira, TV).:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
 
   # ============================================================
   # GRÁFICO - QUEM DECIDE SOBRE PEQUENAS DESPESAS FAMILIARES
   # ============================================================
   
   output$grafico_decide_pequenas_despesas <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Pequenas_Despesas_familiares),
         Quem_Decide_Sobre_Pequenas_Despesas_familiares != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Pequenas_Despesas_familiares
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) {
           paste0(x, "%")
         }
       ) +
       
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 9),
         
         legend.title = element_text(face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - PEQUENAS DESPESAS FAMILIARES
   # ============================================================
   
   output$leitura_decide_pequenas_despesas <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Quem_Decide_Sobre_Pequenas_Despesas_familiares),
         Quem_Decide_Sobre_Pequenas_Despesas_familiares != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quem_Decide_Sobre_Pequenas_Despesas_familiares
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho poder de decidir sobre as PEQUENAS despesas do agregado familiar (ex.: comida, bebidas não alcoólicas).:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
  
   output$grafico_quem_decide <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     vars_decisao <- c(
       "Outra pessoa toma decisoes:Só eu",
       "Outra pessoa toma decisoes:Mãe",
       "Outra pessoa toma decisoes:Pai",
       "Outra pessoa toma decisoes:Esposo",
       "Outra pessoa toma decisoes:Namorado",
       "Outra pessoa toma decisoes:Irmã",
       "Outra pessoa toma decisoes:Irmão",
       "Outra pessoa toma decisoes:Tia",
       "Outra pessoa toma decisoes:Tio",
       "Outra pessoa toma decisoes:Avô",
       "Outra pessoa toma decisoes:Avó",
       "Outra pessoa toma decisoes:Outro",
       "Outro especificar"
     )
     
     # Número de participantes que responderam à pergunta
     n_respondentes <- df %>%
       select(all_of(vars_decisao)) %>%
       filter(if_any(everything(), ~ !is.na(.) & . != "")) %>%
       nrow()
     
     req(n_respondentes > 0)
     
     df_resumo <- df %>%
       select(all_of(vars_decisao)) %>%
       pivot_longer(
         cols = everything(),
         names_to = "Categoria",
         values_to = "Valor"
       ) %>%
       filter(!is.na(Valor), Valor != "", Valor == 1) %>%
       mutate(
         Pessoa = case_when(
           Categoria == "Outro especificar" ~ "Outro",
           TRUE ~ str_remove(
             Categoria,
             "Outra pessoa toma decisoes:"
           )
         )
       ) %>%
       count(Pessoa, name = "Total") %>%
       mutate(
         Percentagem = Total / n_respondentes * 100,
         label = paste0(
           Total, " (",
           round(Percentagem, 1),
           "%)"
         ),
         Pessoa = reorder(Pessoa, Total)
       )
     
     req(nrow(df_resumo) > 0)
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Total,
         y = Pessoa,
         text = paste0(
           "Pessoa: ", Pessoa,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         fill = "#69C7BE",
         width = 0.7
       ) +
       geom_text(
         aes(label = label),
         hjust = -0.1,
         size = 3.5
       ) +
       scale_x_continuous(
         expand = expansion(mult = c(0, 0.15))
       ) +
       labs(
         title = "",
         x = "Número de participantes",
         y = ""
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.y = element_text(size = 10),
         axis.text.x = element_text(size = 9)
       )
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_quem_decide <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     vars_decisao <- c(
       "Outra pessoa toma decisoes:Só eu",
       "Outra pessoa toma decisoes:Mãe",
       "Outra pessoa toma decisoes:Pai",
       "Outra pessoa toma decisoes:Esposo",
       "Outra pessoa toma decisoes:Namorado",
       "Outra pessoa toma decisoes:Irmã",
       "Outra pessoa toma decisoes:Irmão",
       "Outra pessoa toma decisoes:Tia",
       "Outra pessoa toma decisoes:Tio",
       "Outra pessoa toma decisoes:Avô",
       "Outra pessoa toma decisoes:Avó",
       "Outra pessoa toma decisoes:Outro",
       "Outro especificar"
     )
     
     n_respondentes <- df %>%
       select(all_of(vars_decisao)) %>%
       filter(if_any(everything(), ~ !is.na(.) & . != "")) %>%
       nrow()
     
     req(n_respondentes > 0)
     
     resumo <- df %>%
       select(all_of(vars_decisao)) %>%
       pivot_longer(
         cols = everything(),
         names_to = "Categoria",
         values_to = "Valor"
       ) %>%
       filter(!is.na(Valor), Valor != "", Valor == 1) %>%
       mutate(
         Pessoa = case_when(
           Categoria == "Outro especificar" ~ "Outro",
           TRUE ~ str_remove(
             Categoria,
             "Outra pessoa toma decisoes:"
           )
         )
       ) %>%
       count(Pessoa, name = "Total") %>%
       mutate(
         Percentagem = Total / n_respondentes * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     maior <- resumo %>% slice(1)
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>", Pessoa, "</b>: ",
           Total, " participante(s) (",
           round(Percentagem, 1), "%)"
         )
       ) %>%
       pull(texto)
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Por quem ou com quem as decisões são tomadas?:</b> ",
           "A opção mais frequentemente indicada é ",
           "<b>", maior$Pessoa, "</b>, selecionada por ",
           maior$Total, " participante(s) (",
           round(maior$Percentagem, 1), "%). ",
           "No total, responderam à pergunta ",
           "<b>", n_respondentes, " participante(s)</b>. ",
           "A distribuição das opções selecionadas é: ",
           paste(textos, collapse = "; "),
           "."
         )
       )
     )
   })
   
   

  # ========================================================
  #                               NORMAS Sociais
  # ========================================================
   
   
   # ============================================================
   # GRÁFICO - DESEJO DE SER ESCOLHIDO PARA LIDERANÇA
   # ============================================================
   output$grafico_respeito_jovem <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Respeito_Pelo_Jovem_Bairro),
         Respeito_Pelo_Jovem_Bairro != ""
       ) %>%
       count(Respeito_Pelo_Jovem_Bairro, name = "Total") %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       arrange(Total) %>%
       mutate(
         Respeito_Pelo_Jovem_Bairro = factor(
           Respeito_Pelo_Jovem_Bairro,
           levels = Respeito_Pelo_Jovem_Bairro
         )
       )
     
     p <- ggplot(
       resumo,
       aes(
         x = Total,
         y = Respeito_Pelo_Jovem_Bairro,
         text = paste0(
           "Resposta: ",
           Respeito_Pelo_Jovem_Bairro,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       geom_col(
         fill = "#69C7BE",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         hjust = -0.1,
         fontface = "bold",
         size = 4
       ) +
       scale_x_continuous(
         expand = expansion(mult = c(0, 0.15))
       ) +
       labs(
         x = "Número de participantes",
         y = "",
         title = ""
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.y = element_text(size = 11),
         axis.text.x = element_text(size = 9)
       )
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_respeito_jovem <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Respeito_Pelo_Jovem_Bairro),
         Respeito_Pelo_Jovem_Bairro != ""
       ) %>%
       count(
         Respeito_Pelo_Jovem_Bairro,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     maior <- resumo %>%
       slice(1)
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>",
           Respeito_Pelo_Jovem_Bairro,
           "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Como jovem, você se sente respeitada pelos outros membros da comunidade? (idosos, lideres...):</b> ",
           "A resposta mais frequente foi ",
           "<b>",
           maior$Respeito_Pelo_Jovem_Bairro,
           "</b>, indicada por ",
           maior$Total,
           " participante(s) (",
           round(maior$Percentagem, 1),
           "%). ",
           "A distribuição das respostas foi: ",
           paste(textos, collapse = "; "),
           "."
         )
       )
     )
   })
   
   output$grafico_deseja_lideranca <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Gostaria_Escolhido_Lider_Duma_Organizacao),
         Gostaria_Escolhido_Lider_Duma_Organizacao != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         # ------------------------------------------------------
         # ENCURTAR A RESPOSTA
         # ------------------------------------------------------
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Gostaria_Escolhido_Lider_Duma_Organizacao
             ),
             "já ocupo uma posição de líder"
           ) ~ "Já sou líder",
           
           TRUE ~ str_squish(
             Gostaria_Escolhido_Lider_Duma_Organizacao
           )
         )
       ) %>%
       
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       
       group_by(Tipo_Avaliacao) %>%
       
       mutate(
         Percentagem = Total / sum(Total) * 100,
         
         # Apenas percentagem dentro da barra
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       
       ungroup()
     
     
     # ==========================================================
     # ORDEM DAS RESPOSTAS
     # ==========================================================
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Já sou líder",
         "Sim",
         "Provavelmente sim",
         "Provavelmente não",
         "Não"
       )
     )
     
     
     # ==========================================================
     # GRÁFICO
     # ==========================================================
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         
         # ------------------------------------------------------
         # TOOLTIP
         # ------------------------------------------------------
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       # --------------------------------------------------------
     # BARRA 100%
     # --------------------------------------------------------
     
     geom_col(
       position = "stack",
       width = 0.65
     ) +
       
       # --------------------------------------------------------
     # PERCENTAGENS DENTRO DAS BARRAS
     # --------------------------------------------------------
     
     geom_text(
       aes(
         label = label
       ),
       position = position_stack(
         vjust = 0.5
       ),
       size = 3.5
     ) +
       
       # --------------------------------------------------------
     # EIXO Y
     # --------------------------------------------------------
     
     scale_y_continuous(
       limits = c(0, 100),
       breaks = seq(0, 100, 20),
       labels = function(x) {
         paste0(x, "%")
       }
     ) +
       
       # --------------------------------------------------------
     # CORES
     # --------------------------------------------------------
     
     scale_fill_manual(
       values = c(
         "Sim" = "#ffc107",
         "Provavelmente sim" = "#F77333",
         "Já sou líder" = "#9442d4",
         "Provavelmente não" = "#42A5F5",
         "Não" = "#69C7BE"
       )
     ) +
       
       # --------------------------------------------------------
     # TÍTULOS
     # --------------------------------------------------------
     
     labs(
       x = "",
       y = "Percentagem",
       fill = "Resposta"
     ) +
       
       # --------------------------------------------------------
     # TEMA
     # --------------------------------------------------------
     
     theme_minimal() +
       
       theme(
         
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         
         panel.grid.minor = element_blank(),
         
         axis.text.x = element_text(
           size = 10
         ),
         
         axis.text.y = element_text(
           size = 9
         ),
         
         legend.title = element_text(
           face = "bold"
         ),
         
         # Legenda por baixo
         legend.position = "bottom",
         
         legend.direction = "horizontal",
         
         legend.justification = "center"
       )
     
     
     # ==========================================================
     # PLOTLY
     # ==========================================================
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - DESEJO DE SER ESCOLHIDO PARA LIDERANÇA
   # ============================================================
   
   output$leitura_deseja_lideranca <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ==========================================================
     # PREPARAR DADOS
     # ==========================================================
     
     resumo <- df %>%
       filter(
         !is.na(Gostaria_Escolhido_Lider_Duma_Organizacao),
         Gostaria_Escolhido_Lider_Duma_Organizacao != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       
       mutate(
         
         # ------------------------------------------------------
         # ENCURTAR A RESPOSTA
         # ------------------------------------------------------
         
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Gostaria_Escolhido_Lider_Duma_Organizacao
             ),
             "já ocupo uma posição de líder"
           ) ~ "Já sou líder",
           
           TRUE ~ str_squish(
             Gostaria_Escolhido_Lider_Duma_Organizacao
           )
         )
       ) %>%
       
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       
       group_by(Tipo_Avaliacao) %>%
       
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ==========================================================
     # ORDEM DA LEITURA
     # ==========================================================
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Já sou líder",
         "Sim",
         "Provavelmente sim",
         "Provavelmente não",
         "Não"
       )
     )
     
     
     # ==========================================================
     # CRIAR TEXTO POR TIPO DE AVALIAÇÃO
     # ==========================================================
     
     leituras <- resumo %>%
       
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       
       group_by(Tipo_Avaliacao) %>%
       
       summarise(
         
         texto = paste0(
           
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         
         .groups = "drop"
       )
     
     
     # ==========================================================
     # APRESENTAR LEITURA
     # ==========================================================
     
     div(
       
       class = "box-leitura",
       
       HTML(
         paste0(
           
           "<b>Gostaria de alguma vez ser escolhido para ser líder duma organização (professional/negócio, escola, político, organização comunitária, etc.)?:</b> ",
           
           paste(
             leituras$texto,
             collapse = ". "
           ),
           
           "."
         )
       )
     )
   })
   
   output$grafico_mulheres_lideranca <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Mulheres_Selecionadas_Para_Posicao_Lideranca),
         Mulheres_Selecionadas_Para_Posicao_Lideranca != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Mulheres_Selecionadas_Para_Posicao_Lideranca
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # ----------------------------------------------------------
     # ORDEM DA BARRA
     # ----------------------------------------------------------
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Frequentemente",
         "Algumas vezes",
         "Raramente",
         "Nunca"
       )
     )
     
     
     # ----------------------------------------------------------
     # GRÁFICO
     # ----------------------------------------------------------
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ",
           Tipo_Avaliacao,
           "<br>Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       
       scale_fill_manual(
         values = c(
           "Frequentemente" = "#ffc107",
           "Algumas vezes" = "#F77333",
           "Raramente" = "#42A5F5",
           "Nunca" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Frequência"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 9),
         legend.title = element_text(face = "bold"),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.justification = "center"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA - MULHERES SELECIONADAS PARA LIDERANÇA
   # ============================================================
   
   output$leitura_mulheres_lideranca <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Mulheres_Selecionadas_Para_Posicao_Lideranca),
         Mulheres_Selecionadas_Para_Posicao_Lideranca != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Mulheres_Selecionadas_Para_Posicao_Lideranca
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     req(nrow(resumo) > 0)
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Nunca",
         "Raramente",
         "Algumas vezes",
         "Frequentemente"
       )
     )
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Aqui na sua comunidade com que frequência as mulheres são selecionadas 
           para posições de liderança em organizações (professional/negócio, inclusão de pequenas empresas, escola, 
           político, organização comunitária, etc.)?:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - APROVARIA UMA MULHER SELECIONADA PARA LIDERAR
   # ============================================================
   
   output$grafico_aprovaria_mulher_liderar <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Aprovaria_Uma_Mulher_Selecionada_Para_Liderar),
         Aprovaria_Uma_Mulher_Selecionada_Para_Liderar != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Aprovaria_Uma_Mulher_Selecionada_Para_Liderar
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()

     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Aprovo totalmente",
         "Aprovo moderadamente",
         "Não aprovo nem desaprovo",
         "Desaprovo moderadamente",
         "Desaprovo totalmente"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Aprovo totalmente" = "#ffc107",
           "Aprovo moderadamente" = "#F77333",
           "Não aprovo nem desaprovo" = "#BDBDBD",
           "Desaprovo moderadamente" = "#42A5F5",
           "Desaprovo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_aprovaria_mulher_liderar <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Aprovaria_Uma_Mulher_Selecionada_Para_Liderar),
         Aprovaria_Uma_Mulher_Selecionada_Para_Liderar != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Aprovaria_Uma_Mulher_Selecionada_Para_Liderar
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Desaprovo totalmente",
         "Desaprovo moderadamente",
         "Não aprovo nem desaprovo",
         "Aprovo moderadamente",
         "Aprovo totalmente"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Aprovaria ou não, que uma mulher aqui na zona fosse selecionada para liderar
uma organização? (professional/negócio, escola, político, organização comunitária, etc.):</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - QUANTAS PESSOAS APROVARIAM UMA MULHER PARA LIDERAR
   # ============================================================
   
   output$grafico_quantas_aprovariam_mulher <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder),
         Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     # Ordem visual:
     # Quase todos
     # Mais que metade
     # Cerca metade
     # Menos de metade
     # Muito poucas ou nenhuma
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Quase todos",
         "Mais que metade",
         "Cerca metade",
         "Menos de metade",
         "Muito poucas ou nenhuma"
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Quase todos" = "#9442d4",
           "Mais que metade" = "#ffc107",
           "Cerca metade" = "#F77333",
           "Menos de metade" = "#42A5F5",
           "Muito poucas ou nenhuma" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_quantas_aprovariam_mulher <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder),
         Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Quantas_Pessoas_Aprovariam_Uma_Mulher_LIder
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Quase todos",
         "Mais que metade",
         "Cerca metade",
         "Menos de metade",
         "Muito poucas ou nenhuma"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Na sua opinião, quantas pessoas aqui da zona aprovariam que uma mulher fosse selecionada para liderar uma organização? (professional/negócio, escola, político, organização comunitária, etc.)?:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - UMA MULHER DEVERIA ACEITAR VIOLÊNCIA DOMÉSTICA
   # ============================================================
   
   output$grafico_mulher_aceitar_violencia <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Uma_Mulher_Deveria_Aceitar_Violencia_Domestica),
         Uma_Mulher_Deveria_Aceitar_Violencia_Domestica != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Uma_Mulher_Deveria_Aceitar_Violencia_Domestica
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_mulher_aceitar_violencia <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Uma_Mulher_Deveria_Aceitar_Violencia_Domestica),
         Uma_Mulher_Deveria_Aceitar_Violencia_Domestica != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Uma_Mulher_Deveria_Aceitar_Violencia_Domestica
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Discordo totalmente",
         "Discordo parcialmente",
         "Não concordo, nem discordo",
         "Concordo parcialmente",
         "Concordo totalmente"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Uma mulher deveria aceitar violência doméstica para manter a familia junta.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - VIOLÊNCIA DO HOMEM CONTRA A MULHER
   # ============================================================
   
   output$grafico_homem_bate_mulher <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal),
         Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_homem_bate_mulher <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal),
         Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Se_um_homem_bate_sua_mulher_e_assunto_daquele_casal
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Discordo totalmente",
         "Discordo parcialmente",
         "Não concordo, nem discordo",
         "Concordo parcialmente",
         "Concordo totalmente"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Se um homem bate sua mulher, isto e um assunto daquele casal e nao devem falar sobre o assunto com outras pessoas:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   # ========================================================
   # EFICACIA
   # ========================================================
   
   
   # ============================================================
   # GRÁFICO - PESSOAS PARA FALAR QUANDO SE SENTE SOZINHA
   # ============================================================
   output$grafico_pessoas_falar_sozinha <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Pessoas_para_falar_quando_se_sente_sozinha),
         Pessoas_para_falar_quando_se_sente_sozinha != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "^não"
           ) ~ "Ninguém",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "um amigo"
           ) ~ "Uma pessoa",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "algumas pessoas"
           ) ~ "Algumas pessoas",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "vários amigos|varios amigos"
           ) ~ "Várias pessoas",
           
           TRUE ~ str_squish(
             Pessoas_para_falar_quando_se_sente_sozinha
           )
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Ninguém",
         "Uma pessoa",
         "Algumas pessoas",
         "Várias pessoas"
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 10),
         labels = function(x) paste0(x, "%"),
         expand = expansion(
           mult = c(0, 0.03)
         )
       ) +
       
       scale_fill_manual(
         values = c(
           "Várias pessoas" = "#ffc107",
           "Algumas pessoas" = "#F77333",
           "Uma pessoa" = "#42A5F5",
           "Ninguém" = "#69C7BE"
         )
       ) +
       
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal",
         axis.text.x = element_text(
           size = 10
         ),
         axis.text.y = element_text(
           size = 10
         ),
         axis.title.y = element_text(
           size = 11,
           face = "bold"
         )
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4",
         margin = list(
           t = 20,
           r = 30,
           b = 80,
           l = 60
         )
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_pessoas_falar_sozinha <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Pessoas_para_falar_quando_se_sente_sozinha),
         Pessoas_para_falar_quando_se_sente_sozinha != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "^não"
           ) ~ "Ninguém",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "um amigo"
           ) ~ "Uma pessoa",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "algumas pessoas"
           ) ~ "Algumas pessoas",
           
           str_detect(
             str_to_lower(
               Pessoas_para_falar_quando_se_sente_sozinha
             ),
             "varios amigos"
           ) ~ "Várias pessoas",
           
           TRUE ~ str_squish(
             Pessoas_para_falar_quando_se_sente_sozinha
           )
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Várias pessoas",
         "Uma pessoa",
         "Algumas pessoas",
         "Ninguém"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Se você se sentisse sozinha, tem algumas pessoas com quem poderia falar?:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ============================================================
   # GRÁFICO - PESSOAS PARA DISCUTIR PROBLEMAS
   # ============================================================
   
   output$grafico_pessoas_discutir_problemas <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Pessoas_para_discutir_problemas),
         Pessoas_para_discutir_problemas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "^não"
           ) ~ "Ninguém",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "um amigo"
           ) ~ "Uma pessoa",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "alguns"
           ) ~ "Algumas pessoas",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "varios amigos"
           ) ~ "Várias pessoas",
           
           TRUE ~ str_squish(
             Pessoas_para_discutir_problemas
           )
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Várias pessoas",
         "Algumas pessoas",
         "Uma pessoa",
         "Ninguém"
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Várias pessoas" = "#ffc107",
           "Algumas pessoas" = "#F77333",
           "Uma pessoa" = "#42A5F5",
           "Ninguém" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   # ============================================================
   # LEITURA
   # ============================================================
   
   output$leitura_pessoas_discutir_problemas <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Pessoas_para_discutir_problemas),
         Pessoas_para_discutir_problemas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "^não"
           ) ~ "Ninguém",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "um amigo"
           ) ~ "Uma pessoa",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "alguns"
           ) ~ "Algumas pessoas",
           
           str_detect(
             str_to_lower(
               Pessoas_para_discutir_problemas
             ),
             "varios amigos"
           ) ~ "Várias pessoas",
           
           TRUE ~ str_squish(
             Pessoas_para_discutir_problemas
           )
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Ninguém",
         "Uma pessoa",
         "Algumas pessoas",
         "Várias pessoas"
       )
     )
     
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Se você tivesse um problema (por exemplo com seu namorado, marido, sogra, mãe), tem alguma pessoa com quem pode discutir?:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
 ############## Amigos para partilhar alegria e tristeza
   
   
   output$grafico_partilhar_alegrias <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas),
         Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_partilhar_alegrias <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas),
         Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_tenho_amigos_com_quem_posso_compartilhar_alegrias_tristezas
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu tenho amigos com quem posso compartilhar as minhas alegrias e tristezas.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   ######################## Amigos para discutir
   
   
   output$grafico_discutir_amigos <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_posso_discutir_meus_problemas_com_meus_amigos),
         Eu_posso_discutir_meus_problemas_com_meus_amigos != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_posso_discutir_meus_problemas_com_meus_amigos
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_discutir_amigos <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Eu_posso_discutir_meus_problemas_com_meus_amigos),
         Eu_posso_discutir_meus_problemas_com_meus_amigos != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_posso_discutir_meus_problemas_com_meus_amigos
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu posso discutir os meus problemas com os meus amigos.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   ################################## Os meus amigos e familiares dão-me o apoio de que preciso.
   
   output$grafico_familiares_apoiam <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Os_meus_amigos_familiares_dao_me_apoio_que_preciso),
         Os_meus_amigos_familiares_dao_me_apoio_que_preciso != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Os_meus_amigos_familiares_dao_me_apoio_que_preciso
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_familiares_apoiam <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Os_meus_amigos_familiares_dao_me_apoio_que_preciso),
         Os_meus_amigos_familiares_dao_me_apoio_que_preciso != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Os_meus_amigos_familiares_dao_me_apoio_que_preciso
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Os meus amigos e familiares dão-me oapoio de que preciso.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   ################# A_minha_familia_pode_me_ajudar_tomar_decisoes
   
   output$grafico_familia_apoia_decisao <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(A_minha_familia_pode_me_ajudar_tomar_decisoes),
         A_minha_familia_pode_me_ajudar_tomar_decisoes != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           A_minha_familia_pode_me_ajudar_tomar_decisoes
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_familia_apoia_decisao <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(A_minha_familia_pode_me_ajudar_tomar_decisoes),
         A_minha_familia_pode_me_ajudar_tomar_decisoes != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           A_minha_familia_pode_me_ajudar_tomar_decisoes
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>A minha família pode me ajudar a tomar decisões.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   ######################## AUTO_ESTIMA
   
   output$grafico_pessoa_valor <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas),
         Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_pessoa_valor <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas),
         Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_sinto_que_sou_uma_pessoa_valor_quanto_outras_pessoas
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu sinto que sou uma pessoa de valor, no mínimo tanto quanto as outras pessoas.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   #############################  As_vezes_acho_que_nao_presto_para_nada
   
   
   output$grafico_pessoa_nao_presta <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(As_vezes_acho_que_nao_presto_para_nada),
         As_vezes_acho_que_nao_presto_para_nada != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           As_vezes_acho_que_nao_presto_para_nada
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_pessoa_nao_presta <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(As_vezes_acho_que_nao_presto_para_nada),
         As_vezes_acho_que_nao_presto_para_nada != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           As_vezes_acho_que_nao_presto_para_nada
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Às vezes acho que não presto para nada. (INVERSO — alta agência = discordar):</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   # ########################## Levando tudo em conta, eu penso que sou uma pessoa fracassada. (INVERSO — alta agência = discordar)
   # 
   output$grafico_pessoa_fracassada <- renderPlotly({

     df <- dados()

     req(nrow(df) > 0)

     df_resumo <- df %>%
       filter(
         !is.na(Eu_penso_que_sou_uma_pessoa_fracassada),
         Eu_penso_que_sou_uma_pessoa_fracassada != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_penso_que_sou_uma_pessoa_fracassada
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()

     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"



       )
     )


     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )


     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })

   output$leitura_pessoa_fracassada <- renderUI({

     df <- dados()

     req(nrow(df) > 0)


     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------

     resumo <- df %>%
       filter(
         !is.na(Eu_penso_que_sou_uma_pessoa_fracassada),
         Eu_penso_que_sou_uma_pessoa_fracassada != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_penso_que_sou_uma_pessoa_fracassada
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()


     req(nrow(resumo) > 0)


     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------

     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )


     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------

     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",

           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )


     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------

     div(
       class = "box-leitura",

       HTML(
         paste0(
           "<b>Levando tudo em conta, eu penso que sou uma pessoa fracassada. (INVERSO — alta agência = discordar):</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   ################# Boas QUALIDADES
   
   output$grafico_pessoa_boas_qualidades <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Eu_acho_que_tenho_varias_boas_qualidades),
         Eu_acho_que_tenho_varias_boas_qualidades != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_acho_que_tenho_varias_boas_qualidades
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_pessoa_boas_qualidades <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(Eu_acho_que_tenho_varias_boas_qualidades),
         Eu_acho_que_tenho_varias_boas_qualidades != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           Eu_acho_que_tenho_varias_boas_qualidades
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>Eu acho que tenho várias boas qualidades.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   ################################# satisfeita_comigo
   
   output$grafico_satisfeita_comigo <- renderPlotly({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(No_geral_estou_satisfeita_comigo_mesma),
         No_geral_estou_satisfeita_comigo_mesma != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           No_geral_estou_satisfeita_comigo_mesma
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           round(Percentagem, 1),
           "%"
         )
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
         
         
         
       )
     )
     
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(
         position = "stack",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Concordo totalmente" = "#ffc107",
           "Concordo parcialmente" = "#F77333",
           "Não concordo, nem discordo" = "#BDBDBD",
           "Discordo parcialmente" = "#42A5F5",
           "Discordo totalmente" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_satisfeita_comigo <- renderUI({
     
     df <- dados()
     
     req(nrow(df) > 0)
     
     
     # ----------------------------------------------------------
     # Preparar dados
     # ----------------------------------------------------------
     
     resumo <- df %>%
       filter(
         !is.na(No_geral_estou_satisfeita_comigo_mesma),
         No_geral_estou_satisfeita_comigo_mesma != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = str_squish(
           No_geral_estou_satisfeita_comigo_mesma
         )
       ) %>%
       count(
         Tipo_Avaliacao,
         Resposta,
         name = "Total"
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       ungroup()
     
     
     req(nrow(resumo) > 0)
     
     
     # ----------------------------------------------------------
     # ORDEM DA LEITURA
     # ----------------------------------------------------------
     
     resumo$Resposta <- factor(
       resumo$Resposta,
       levels = c(
         "Concordo totalmente",
         "Concordo parcialmente",
         "Não concordo, nem discordo",
         "Discordo parcialmente",
         "Discordo totalmente"
       )
     )
     
     
     # ----------------------------------------------------------
     # Criar texto para cada Tipo_Avaliacao
     # ----------------------------------------------------------
     
     leituras <- resumo %>%
       arrange(
         Tipo_Avaliacao,
         Resposta
       ) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>",
           first(Tipo_Avaliacao),
           "</b>: ",
           
           paste0(
             Resposta,
             " = ",
             Total,
             " (",
             round(Percentagem, 1),
             "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     
     # ----------------------------------------------------------
     # Apresentar leitura
     # ----------------------------------------------------------
     
     div(
       class = "box-leitura",
       
       HTML(
         paste0(
           "<b>No geral, eu estou satisfeita comigo mesma.:</b> ",
           paste(
             leituras$texto,
             collapse = ". "
           ),
           "."
         )
       )
     )
   })
   
   output$grafico_alcancar_objectivos <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(`Alcançar objectivos profissionais`),
         `Alcançar objectivos profissionais` != ""
       ) %>%
       count(
         `Alcançar objectivos profissionais`,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       arrange(Total) %>%
       mutate(
         Resposta = factor(
           `Alcançar objectivos profissionais`,
           levels = `Alcançar objectivos profissionais`
         )
       )
     
     p <- ggplot(
       resumo,
       aes(
         x = Total,
         y = Resposta,
         text = paste0(
           "Resposta: ",
           Resposta,
           "<br>N = ",
           Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       geom_col(
         fill = "#69C7BE",
         width = 0.65
       ) +
       geom_text(
         aes(label = label),
         hjust = -0.1,
         fontface = "bold",
         size = 4
       ) +
       scale_x_continuous(
         expand = expansion(mult = c(0, 0.15))
       ) +
       labs(
         x = "Número de participantes",
         y = "",
         title = ""
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.background = element_rect(
           fill = "#f5f3f4",
           colour = NA
         ),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.y = element_text(size = 11),
         axis.text.x = element_text(size = 9)
       )
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_alcancar_objectivos <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(`Alcançar objectivos profissionais`),
         `Alcançar objectivos profissionais` != ""
       ) %>%
       count(
         `Alcançar objectivos profissionais`,
         name = "Total"
       ) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     maior <- resumo %>%
       slice(1)
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>",
           `Alcançar objectivos profissionais`,
           "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Antes de engressar na formação, você tinha uma perspectiva clara sobre seus objectivos profissionais do futuro?:</b> ",
           "A resposta mais frequente foi ",
           "<b>",
           maior$`Alcançar objectivos profissionais`,
           "</b>, indicada por ",
           maior$Total,
           " participante(s) (",
           round(maior$Percentagem, 1),
           "%). ",
           "A distribuição das respostas foi: ",
           paste(textos, collapse = "; "),
           "."
         )
       )
     )
   })
   
   
   output$grafico_expressar_opiniao <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_minha_familia`),
         `Expressar_minha_opiniao_minha_familia` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(
             str_to_lower(str_squish(`Expressar_minha_opiniao_minha_familia`)),
             "^01|^1"
           ) ~ "Definitivamente não posso fazer",
           str_detect(
             str_to_lower(str_squish(`Expressar_minha_opiniao_minha_familia`)),
             "^02|^2"
           ) ~ "Provavelmente não posso fazer",
           str_detect(
             str_to_lower(str_squish(`Expressar_minha_opiniao_minha_familia`)),
             "^03|^3"
           ) ~ "Talvez possa fazer",
           str_detect(
             str_to_lower(str_squish(`Expressar_minha_opiniao_minha_familia`)),
             "^04|^4"
           ) ~ "Provavelmente posso fazer",
           str_detect(
             str_to_lower(str_squish(`Expressar_minha_opiniao_minha_familia`)),
             "^05|^5"
           ) ~ "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_minha_familia`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(
         x = "",
         y = "Percentagem",
         fill = "Resposta"
       ) +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_expressar_opiniao <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_minha_familia`),
         `Expressar_minha_opiniao_minha_familia` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(`Expressar_minha_opiniao_minha_familia`), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_minha_familia`), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_minha_familia`), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_minha_familia`), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_minha_familia`), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_minha_familia`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Expressar a minha opinião na minha família:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   
   output$grafico_expressar_nao_concordando <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`),
         `Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(x = "", y = "Percentagem", fill = "Resposta") +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_expressar_nao_concordando <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`),
         `Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_mesmo_nao_concordo_com_chefe`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Expressar a minha opinião mesmo quando não concordo com o/a chefe do agregado familiar:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   output$grafico_expressar_trabalho <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Expressar_minha_opiniao_trabalho),
         Expressar_minha_opiniao_trabalho != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Expressar_minha_opiniao_trabalho)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(x = "", y = "Percentagem", fill = "Resposta") +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_expressar_trabalho <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Expressar_minha_opiniao_trabalho),
         Expressar_minha_opiniao_trabalho != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Expressar_minha_opiniao_trabalho), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Expressar_minha_opiniao_trabalho)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Expressar a minha opinião num âmbito de trabalho/serviço.:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   output$grafico_expressar_escola <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_escola`),
         `Expressar_minha_opiniao_escola` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_escola`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(x = "", y = "Percentagem", fill = "Resposta") +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_expressar_escola <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(`Expressar_minha_opiniao_escola`),
         `Expressar_minha_opiniao_escola` != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(`Expressar_minha_opiniao_escola`), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(`Expressar_minha_opiniao_escola`)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Expressar a minha opinião se estou em desacordo com o director da escola ou com um colega sénior no trabalho:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   output$grafico_defender_injusticas <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Defender_me_se_for_tratada_injustamente),
         Defender_me_se_for_tratada_injustamente != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Defender_me_se_for_tratada_injustamente)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(x = "", y = "Percentagem", fill = "Resposta") +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_defender_injusticas <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Defender_me_se_for_tratada_injustamente),
         Defender_me_se_for_tratada_injustamente != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Defender_me_se_for_tratada_injustamente), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Defender_me_se_for_tratada_injustamente)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Defender-me se for tratada/o injustamente (verbalmente):</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   output$grafico_expressar_infeliz <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     df_resumo <- df %>%
       filter(
         !is.na(Dizer_outros_pararem_quando_fazer_me_infeliz),
         Dizer_outros_pararem_quando_fazer_me_infeliz != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(
         Percentagem = Total / sum(Total) * 100,
         label = paste0(round(Percentagem, 1), "%")
       ) %>%
       ungroup()
     
     df_resumo$Resposta <- factor(
       df_resumo$Resposta,
       levels = c(
         "Completamente certa que posso fazer",
         "Provavelmente posso fazer",
         "Talvez possa fazer",
         "Provavelmente não posso fazer",
         "Definitivamente não posso fazer"
       )
     )
     
     p <- ggplot(
       df_resumo,
       aes(
         x = Tipo_Avaliacao,
         y = Percentagem,
         fill = Resposta,
         text = paste0(
           "Tipo de avaliação: ", Tipo_Avaliacao,
           "<br>Resposta: ", Resposta,
           "<br>N = ", Total,
           "<br>Percentagem = ", round(Percentagem, 1), "%"
         )
       )
     ) +
       geom_col(position = "stack", width = 0.65) +
       geom_text(
         aes(label = label),
         position = position_stack(vjust = 0.5),
         size = 3.5
       ) +
       scale_y_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20),
         labels = function(x) paste0(x, "%")
       ) +
       scale_fill_manual(
         values = c(
           "Completamente certa que posso fazer" = "#ffc107",
           "Provavelmente posso fazer" = "#F77333",
           "Talvez possa fazer" = "#9442d4",
           "Provavelmente não posso fazer" = "#42A5F5",
           "Definitivamente não posso fazer" = "#69C7BE"
         )
       ) +
       labs(x = "", y = "Percentagem", fill = "Resposta") +
       theme_minimal() +
       theme(
         plot.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.background = element_rect(fill = "#f5f3f4", colour = NA),
         panel.grid.minor = element_blank(),
         legend.position = "bottom",
         legend.direction = "horizontal"
       )
     
     ggplotly(p, tooltip = "text") %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   
   output$leitura_expressar_infeliz <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     resumo <- df %>%
       filter(
         !is.na(Dizer_outros_pararem_quando_fazer_me_infeliz),
         Dizer_outros_pararem_quando_fazer_me_infeliz != "",
         !is.na(Tipo_Avaliacao),
         Tipo_Avaliacao != ""
       ) %>%
       mutate(
         Resposta = case_when(
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^01|^1") ~
             "Definitivamente não posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^02|^2") ~
             "Provavelmente não posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^03|^3") ~
             "Talvez possa fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^04|^4") ~
             "Provavelmente posso fazer",
           str_detect(str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz), "^05|^5") ~
             "Completamente certa que posso fazer",
           TRUE ~ str_squish(Dizer_outros_pararem_quando_fazer_me_infeliz)
         )
       ) %>%
       count(Tipo_Avaliacao, Resposta, name = "Total") %>%
       group_by(Tipo_Avaliacao) %>%
       mutate(Percentagem = Total / sum(Total) * 100) %>%
       ungroup()
     
     leituras <- resumo %>%
       arrange(Tipo_Avaliacao, Resposta) %>%
       group_by(Tipo_Avaliacao) %>%
       summarise(
         texto = paste0(
           "<b>", first(Tipo_Avaliacao), "</b>: ",
           paste0(
             Resposta, " = ", Total, " (",
             round(Percentagem, 1), "%)",
             collapse = "; "
           )
         ),
         .groups = "drop"
       )
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Dizer aos outros para pararem quando estão a fazer-me infeliz.:</b> ",
           paste(leituras$texto, collapse = ". "),
           "."
         )
       )
     )
   })
   
   output$grafico_grupos_comunidade <- renderPlotly({
     
     df <- dados()
     req(nrow(df) > 0)
     
     vars_grupos <- c(
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo religioso",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo de credito/poupança/x",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo comunitario",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo de desporto",
       "Se faz parte de alguns dos grupos na sua comunidade?:Escola/grupo de estudo",
       "Se faz parte de alguns dos grupos na sua comunidade?:Outro grupo"
     )
     
     n_respondentes <- df %>%
       select(all_of(vars_grupos)) %>%
       filter(
         if_any(everything(), ~ !is.na(.) & . != "")
       ) %>%
       nrow()
     
     req(n_respondentes > 0)
     
     resumo <- df %>%
       select(all_of(vars_grupos)) %>%
       pivot_longer(
         cols = everything(),
         names_to = "Grupo",
         values_to = "Valor"
       ) %>%
       filter(
         !is.na(Valor),
         Valor != "",
         Valor == 1
       ) %>%
       mutate(
         Grupo = case_when(
           str_detect(Grupo, "Grupo religioso") ~
             "Grupo religioso",
           
           str_detect(Grupo, "credito/poupança") ~
             "Grupo de crédito/poupança",
           
           str_detect(Grupo, "Grupo comunitario") ~
             "Grupo comunitário",
           
           str_detect(Grupo, "Grupo de desporto") ~
             "Grupo de desporto",
           
           str_detect(Grupo, "Escola/grupo de estudo") ~
             "Escola/grupo de estudo",
           
           str_detect(Grupo, "Outro grupo") ~
             "Outro grupo",
           
           TRUE ~ Grupo
         )
       ) %>%
       count(Grupo, name = "Total") %>%
       mutate(
         Percentagem = Total / n_respondentes * 100,
         label = paste0(
           Total,
           " (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       arrange(Total) %>%
       mutate(
         Grupo = factor(Grupo, levels = Grupo)
       )
     
     p <- ggplot(
       resumo,
       aes(
         x = Total,
         y = Grupo,
         text = paste0(
           "Grupo: ", Grupo,
           "<br>N = ", Total,
           "<br>Percentagem = ",
           round(Percentagem, 1),
           "%"
         )
       )
     ) +
       
       geom_col(
         fill = "#69C7BE",
         width = 0.65
       ) +
       
       geom_text(
         aes(label = label),
         hjust = -0.1,
         fontface = "bold",
         size = 4
       ) +
       
       scale_x_continuous(
         expand = expansion(
           mult = c(0, 0.15)
         )
       ) +
       
       labs(
         x = "Número de participantes",
         y = "",
         title = ""
       ) +
       
       theme_minimal() +
       
       theme(
         plot.background =
           element_rect(
             fill = "#f5f3f4",
             colour = NA
           ),
         panel.background =
           element_rect(
             fill = "#f5f3f4",
             colour = NA
           ),
         panel.grid.major.y =
           element_blank(),
         panel.grid.minor =
           element_blank(),
         axis.text.y =
           element_text(size = 11),
         axis.text.x =
           element_text(size = 9)
       )
     
     ggplotly(
       p,
       tooltip = "text"
     ) %>%
       layout(
         paper_bgcolor = "#f5f3f4",
         plot_bgcolor = "#f5f3f4"
       )
   })
   
   output$leitura_grupos_comunidade <- renderUI({
     
     df <- dados()
     req(nrow(df) > 0)
     
     vars_grupos <- c(
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo religioso",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo de credito/poupança/x",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo comunitario",
       "Se faz parte de alguns dos grupos na sua comunidade?:Grupo de desporto",
       "Se faz parte de alguns dos grupos na sua comunidade?:Escola/grupo de estudo",
       "Se faz parte de alguns dos grupos na sua comunidade?:Outro grupo"
     )
     
     n_respondentes <- df %>%
       select(all_of(vars_grupos)) %>%
       filter(
         if_any(everything(), ~ !is.na(.) & . != "")
       ) %>%
       nrow()
     
     resumo <- df %>%
       select(all_of(vars_grupos)) %>%
       pivot_longer(
         cols = everything(),
         names_to = "Grupo",
         values_to = "Valor"
       ) %>%
       filter(
         !is.na(Valor),
         Valor != "",
         Valor == 1
       ) %>%
       mutate(
         Grupo = case_when(
           str_detect(Grupo, "Grupo religioso") ~
             "Grupo religioso",
           str_detect(Grupo, "credito/poupança") ~
             "Grupo de crédito/poupança",
           str_detect(Grupo, "Grupo comunitario") ~
             "Grupo comunitário",
           str_detect(Grupo, "Grupo de desporto") ~
             "Grupo de desporto",
           str_detect(Grupo, "Escola/grupo de estudo") ~
             "Escola/grupo de estudo",
           str_detect(Grupo, "Outro grupo") ~
             "Outro grupo",
           TRUE ~ Grupo
         )
       ) %>%
       count(Grupo, name = "Total") %>%
       mutate(
         Percentagem = Total / n_respondentes * 100
       ) %>%
       arrange(desc(Total))
     
     req(nrow(resumo) > 0)
     
     maior <- resumo %>% slice(1)
     
     textos <- resumo %>%
       mutate(
         texto = paste0(
           "<b>", Grupo, "</b>: ",
           Total,
           " participante(s) (",
           round(Percentagem, 1),
           "%)"
         )
       ) %>%
       pull(texto)
     
     div(
       class = "box-leitura",
       HTML(
         paste0(
           "<b>Por favor diga se voce faz parte de alguns dos grupos na sua comunidade?:</b> ",
           "A participação mais frequente foi em ",
           "<b>", maior$Grupo, "</b>, indicada por ",
           maior$Total, " participante(s) (",
           round(maior$Percentagem, 1),
           "%). ",
           "A distribuição das respostas foi: ",
           paste(textos, collapse = "; "),
           ". ",
           "<br><br><i>",
           "Nota: as opções não são mutuamente exclusivas, ",
           "pelo que uma participante pode pertencer a mais de um grupo.",
           "</i>"
         )
       )
     )
   })
   
  # ========================================================
  # OCEAN GUARD (PLACEHOLDER – ajustar depois)
  # ========================================================
  # 
  # dados_filtrados <- reactive({
  #   
  #   df <- dados_ocean
  #   
  #   if (!is.null(input$pescador) && input$pescador != "Todos") {
  #     df <- df %>% dplyr::filter(pescador == input$pescador)
  #   }
  #   
  #   if (!is.null(input$centro) && input$centro != "Todos") {
  #     df <- df %>% dplyr::filter(centro_pesca == input$centro)
  #   }
  #   
  #   if (!is.null(input$arte) && input$arte != "Todos") {
  #     df <- df %>% dplyr::filter(tipo_arte == input$arte)
  #   }
  #   
  #   if (!is.null(input$mare) && input$mare != "Todas") {
  #     df <- df %>% dplyr::filter(tipo_mare == input$mare)
  #   }
  #   
  #   df
  # })
  # 
  # # =====================================================
  # # 📌 KPIs
  # # =====================================================
  # 
  # output$kpi_capturas <- renderText({
  #   format(nrow(dados_filtrados()), big.mark = ",")
  # })
  # 
  # output$kpi_peso <- renderText({
  #   round(sum(dados_filtrados()$peso_gramas, na.rm = TRUE) / 1000, 1)
  # })
  # 
  # output$kpi_especies <- renderText({
  #   n_distinct(dados_filtrados()$latin_name)
  # })
  # 
  # output$kpi_pescadores <- renderText({
  #   n_distinct(dados_filtrados()$pescador)
  # })
  # 
  # # =====================================================
  # # 🐟 TOP ESPÉCIES
  # # =====================================================
  # 
  # output$grafico_especies <- renderPlotly({
  #   
  #   df <- dados_filtrados() %>%
  #     group_by(xitswa_name) %>%
  #     summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop") %>%
  #     arrange(desc(peso_total)) %>%
  #     slice_head(n = 10)
  #   
  #   plotly::plot_ly(
  #     df,
  #     x = ~reorder(xitswa_name, peso_total),
  #     y = ~peso_total,
  #     type = "bar",
  #     text = ~format(peso_total, big.mark = ","),
  #     textposition = "outside"
  #   ) %>%
  #     layout(
  #       title = list(text = ""),
  #       paper_bgcolor = "#f5f3f4",
  #       plot_bgcolor = "#f5f3f4",
  #       xaxis = list(title = "Espécie", tickangle = -25),
  #       yaxis = list(title = "Peso (gramas)"),
  #       barmode = "stack"
  #     )
  # })
  # 
  # # =====================================================
  # # 🎣 TIPO DE ARTE
  # # =====================================================
  # 
  # output$grafico_artes <- renderPlotly({
  #   
  #   df <- dados_filtrados() %>% count(tipo_arte)
  #   
  #   plotly::plot_ly(
  #     df,
  #     labels = ~tipo_arte,
  #     values = ~n,
  #     type = "pie"
  #   ) %>%
  #     layout(
  #       title = list(text = ""),
  #       
  #       paper_bgcolor = "#f5f3f4",
  #       plot_bgcolor = "#f5f3f4",
  #       
  #       legend = list(title = list(text = "<b>Tipo de Arte</b>"))
  #     )
  # })
  # 
  # # =====================================================
  # # 🌊 MARÉ
  # # =====================================================
  # 
  # output$grafico_mare <- renderPlotly({
  #   
  #   df <- dados_filtrados() %>%
  #     group_by(tipo_mare) %>%
  #     summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop")
  #   
  #   plotly::plot_ly(
  #     df,
  #     x = ~tipo_mare,
  #     y = ~peso_total,
  #     type = "bar",
  #     text = ~format(peso_total, big.mark = ","),
  #     textposition = "outside"
  #   ) %>%
  #     layout(
  #       title = list(text = ""),
  #       paper_bgcolor = "#f5f3f4",
  #       plot_bgcolor = "#f5f3f4",
  #       xaxis = list(title = "Maré", tickangle = -25),
  #       yaxis = list(title = "Peso Total"),
  #       barmode = "stack"
  #     )
  # })
  # 
  # # =====================================================
  # # 🧭 EMBARCAÇÃO (EXTRA OPCIONAL)
  # # =====================================================
  # 
  # output$grafico_embarcacao <- renderPlotly({
  #   
  #   df <- dados_filtrados() %>%
  #     group_by(pescador) %>%
  #     summarise(peso_total = sum(peso_gramas, na.rm = TRUE), .groups = "drop")
  #   
  #   plotly::plot_ly(
  #     df,
  #     x = ~pescador,
  #     y = ~peso_total,
  #     type = "bar",
  #     text = ~format(peso_total, big.mark = ","),
  #     textposition = "outside"
  #   ) %>%
  #     layout(
  #       title = list(text = ""),
  #       paper_bgcolor = "#f5f3f4",
  #       plot_bgcolor = "#f5f3f4",
  #       yaxis = list(title = "Peso Total")
  #     )
  # })
  # 
  # # =====================================================
  # # 👤 PESCADORES (EXTRA OPCIONAL)
  # # =====================================================
  # 
  # output$grafico_embarcacao  <- renderPlotly({
  #   
  #   df <- dados_filtrados() %>%
  #     group_by(pescador) %>%
  #     summarise(
  #       peso_total = sum(peso_gramas, na.rm = TRUE),
  #       .groups = "drop"
  #     )
  #   
  #   plotly::plot_ly(
  #     df,
  #     x = ~pescador,
  #     y = ~peso_total,
  #     type = "bar"
  #   ) %>%
  #     
  #   layout(
  #           title = list(text = ""),
  # 
  #           paper_bgcolor = "#f5f3f4",
  #           plot_bgcolor = "#f5f3f4",
  # 
  #           legend = list(title = list(text = "<b>Captura por Pescador</b>"))
  #         )
  # 
  # })
}

# ==========================================================
# RUN APP
# ==========================================================
shinyApp(ui, server)