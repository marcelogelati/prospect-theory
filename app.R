library(shiny) 
library(ggraph) ## Para plotar os gráficos
library(igraph) ## Idem
library(tidyverse) ## Estou usando, pelo menos, o ggplot2
# library(googlesheets) ## Para exportar os resultados

## Criando valores do gráfico
edges1 <- data.frame(from = "origin", to = c(10, 8))
edges_fixo <- data.frame(from = "origin", to = c("---", 1))

## Criando objeto do tipo gráfico
mygraph1 <- graph_from_data_frame(edges1)
mygraph_fixo <- graph_from_data_frame(edges_fixo)



## Criando árvores
tree1 <- ggraph(mygraph1, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link(aes(label = c("Candidato 1", "Candidato 2")), hjust = 1.3) +
  geom_node_point() +
  geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
  ylim(-0.1, NA) + 
  theme_void()

tree_fixo <- ggraph(mygraph_fixo, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link(aes(label = c("Candidato 1", "Candidato 2")), hjust = 1.3) +
  geom_node_point() +
  geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
  ylim(-0.1, NA) + 
  theme_void()

## Criando caixas modais para o app
modal_confirm <- modalDialog(
  "Suas respostas foram enviadas!",
  title = "Confirmação",
  footer = list(
    actionButton("ok", "Ok")
  )
)

## Começando user interface
ui <- fluidPage(
  navbarPage("Navegação",
      
    ## Página de introdução
    tabPanel("Introdução",
      titlePanel(title = "", windowTitle = "Prospect Theory"),
      h1("Prospect Theory: Experimentos", align = "center"),
      h3("Aqui você irá responder algumas perguntas referentes às suas escolhas 
         sob incerteza. O objetivo final é descobrir o formato de sua função utilidade."),
      h3("No painel acima temos as perguntas que você deve responder.
         Quando tiver terminado de responder uma pergunta, passe para a próxima. Como cada
         pergunta depende da anterior, responda-as em ordem."),
      h3("O cenário é o seguinte: imagine que um país estrangeiro terá eleições e dois
         candidatos estarão participando, sendo que um, e apenas um, deles irá ganhar.
         O partido do candidato 1 tem ganhado a maioria das eleições no passado, mas 
         desta vez sua campanha foi mal. Você irá investir nesse país, e os payoffs 
         representam os lucros que você terá baseado em qual candidato venceu a eleição.
         Essas são as únicas informações que você possui. Os valores estão em milhares de 
         reais"),
      h3("Em cada uma das perguntas, você deve substituir o valor em branco pelo valor
         que o torne indiferente entre as duas loterias."),
      textInput("user", "Digite sua identificação")
    ),
    
    ## Página da pergunta 1
    tabPanel("Pergunta 1",
      fluidRow(
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2"))
      ), 
        numericInput("alpha1", "Seu valor", value = 0, step = 1)
    ),
    
    ## Pàgina da pergunta 2
    tabPanel("Pergunta 2",
      fluidRow(
        column(6, plotOutput("plot3")),
        column(6, plotOutput("plot4"))
      ),
        numericInput("alpha2", "Seu valor", value = 0, step = 1),
    ),
    
    ## Pàgina da pergunta 3
    tabPanel("Pergunta 3",
     fluidRow(
       column(6, plotOutput("plot5")),
       column(6, plotOutput("plot6"))
     ),
       numericInput("alpha3", "Seu valor", value = 0, step = 1)
    ),
      
    ## Pàgina da pergunta 4
    tabPanel("Pergunta 4",
      fluidRow(
        column(6, plotOutput("plot7")),
        column(6, plotOutput("plot8"))
      ),
       numericInput("alpha4", "Seu valor", value = 0, step = 1)
    ),
    
    ## Sua função utilidade
    tabPanel("Sua função utilidade",
      plotOutput("utilidade"),
      actionButton("submit", "Submeta suas respostas"))
  )
)

## Abrindo server
server <- function(input, output, session) {
  
  ## Output da pergunta 1
  output$plot1 <- renderPlot(tree1)
  output$plot2 <- renderPlot(tree_fixo)
  
  ## Output da pergunta 2
  output$plot3 <- renderPlot({
   edges2 <- data.frame(from = "origin", to = c(input$alpha1, 8))
   mygraph2 <- graph_from_data_frame(edges2)
   tree2 <- ggraph(mygraph2, layout = 'dendrogram', circular = FALSE) + 
     geom_edge_link(aes(label = c("Candidato 1", "Candidato 2")), hjust = 1.3) +
     geom_node_point() +
     geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
     ylim(-0.1, NA) + 
     theme_void()
   tree2
   })
  output$plot4 <- renderPlot(tree_fixo)

  ## Output da pergunta 3
  output$plot5 <- renderPlot({
    edges3 <- data.frame(from = "origin", to = c(input$alpha2, 8))
    mygraph3 <- graph_from_data_frame(edges3)
    tree3 <- ggraph(mygraph3, layout = 'dendrogram', circular = FALSE) + 
      geom_edge_link(aes(label = c("Candidato 1", "Candidato 2")), hjust = 1.3) +
      geom_node_point() +
      geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
      ylim(-0.1, NA) + 
      theme_void()
    tree3
  })
  output$plot6 <- renderPlot(tree_fixo)

  ## Output da pergunta 4
  output$plot7 <- renderPlot({
    edges4 <- data.frame(from = "origin", to = c(input$alpha3, 8))
    mygraph4 <- graph_from_data_frame(edges4)
    tree4 <- ggraph(mygraph4, layout = 'dendrogram', circular = FALSE) + 
      geom_edge_link(aes(label = c("Candidato 1", "Candidato 2")), hjust = 1.3) +
      geom_node_point() +
      geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
      ylim(-0.1, NA) + 
      theme_void()
    tree4
  })
  output$plot8 <- renderPlot(tree_fixo)
  
  ## Plotando função utilidade
  output$utilidade <- renderPlot(
    ggplot(data.frame(alfas = c(10, input$alpha1, input$alpha2, input$alpha3, input$alpha4),
                      dist = c(0, 1/4, 1/2, 3/4, 1)), 
           aes(alfas, dist)) + geom_line()
  )
    
  ## Mandando pro Google Sheets  
  observeEvent(input$submit, showModal(modal_confirm))
  observeEvent(input$ok, removeModal())
}
## Rodando app
shinyApp(ui, server)