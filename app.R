#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#FUNCIONES MODELOS 
#MODELO1

K <- c(1,2)

m_1 <- function(Trials, mu, b){  
  d = data.frame(
    fracasos = c(0L,0L), #Registra el numero de fracasos (r = 0)
    exitos = c(0L,0L),    #Registra el numero de exitos (r = 1)
    row.names = c('1','2')
  )
  
  choices <- c()
  rewards <- c()
  
  proba <- c(b,1-b) #Proba de elecciÃ³n
  
  for (i in 1:length(Trials)){
    c <- sample(range(K), 1,prob = proba)
    if (runif(1, min=0, max=1) < mu[c]){
      r = 1 
      d[c,2] = d[c,2]+1L
    } else{ r = 0 
    d[c,1] = d[c,1]+1L
    }
    
    choices[(i)] <- c
    rewards[(i)] <- r
  }
  
  return(list(d=d,choices=choices,rewards=rewards))
}

#MODELO2
m_2 <- function(Trials, mu, epsilon){
  d = data.frame(
    fracasos = c(0L,0L), #Registra el numero de fracasos (r = 0)
    exitos = c(0L,0L),    #Registra el numero de exitos (r = 1)
    row.names = c('1','2')
  )
  
  choices <- c()
  rewards <- c()
  
  proba <- c(0.4,0.6) #Proba de elecciÃ³n
  
  for (i in 1:length(Trials)){
    c <- sample(range(K), 1,prob = proba)
    if (runif(1, min=0, max=1) < mu[c]){
      r = 1 
      d[c,2] = d[c,2]+1L
    } else{ r = 0 
    d[c,1] = d[c,1]+1L
    }
    
    if (c== 1 & r == 1 | c==2 & r==0){
      proba[1] = 1 - epsilon /2
    }else{
      proba[2] = epsilon/2
    }
    
    
    choices[(i)] <- c
    rewards[(i)] <- r
  }
  
  return(list(d=d,choices=choices,rewards=rewards))
}

#MODELO 3
m_3 <- function(Trials, mu, alpha, beta){
  d = data.frame(
    fracasos = c(0L,0L), #Registra el numero de fracasos (r = 0)
    exitos = c(0L,0L),    #Registra el numero de exitos (r = 1)
    row.names = c('1','2')
  )
  
  choices <- c()
  rewards <- c()
  
  Q <- c(0.5,0.5) #Proba de elecciÃ³n
  
  for (i in 1:length(Trials)){
    
    p0 <- exp(beta*Q[1])/exp(beta*Q[1]+beta*Q[2])
    p1 <- 1-p0
    
    proba <- c(p0,p1)
    
    c <- sample(range(K), 1,prob = proba)
    if (runif(1, min=0, max=1) < mu[c]){
      r = 1 
      d[c,2] = d[c,2]+1L
    } else{ r = 0 
    d[c,1] = d[c,1]+1L
    }
    
    delta <- r-Q[c]
    Q[c] <- Q[c]+alpha*delta
    
    choices[(i)] <- c
    rewards[(i)] <- r
  }
  
  return(list(d=d,choices=choices,rewards=rewards))
}

#MODELO4
m_4 <- function(Trials, mu, alpha_c, beta_c){
  d = data.frame(
    fracasos = c(0L,0L), #Registra el numero de fracasos (r = 0)
    exitos = c(0L,0L),    #Registra el numero de exitos (r = 1)
    row.names = c('1','2')
  )
  
  choices <- c()
  rewards <- c()
  
  CK <- c(0,0) 
  
  for (i in 1:length(Trials)){
    
    p = exp(beta_c*CK) / sum(exp(beta_c*CK))
    
    c <- sample(range(K), 1,prob = p)
    if (runif(1, min=0, max=1) < mu[c]){
      r = 1 
      d[c,2] = d[c,2]+1L
    } else{ r = 0 
    d[c,1] = d[c,1]+1L
    }
    
    CK <- (1-alpha_c)*CK
    CK[c]+alpha_c 
    
    choices[(i)] <- c
    rewards[(i)] <- r
  }
  
  return(list(d=d,choices=choices,rewards=rewards))
}

#MODELO 5 
m_5 <- function(Trials, mu, alpha, beta,alpha_c, beta_c){
  d = data.frame(
    fracasos = c(0L,0L), #Registra el numero de fracasos (r = 0)
    exitos = c(0L,0L),    #Registra el numero de exitos (r = 1)
    row.names = c('1','2')
  )
  
  choices <- c()
  rewards <- c()
  
  Q <- c(0.5,0.5)
  CK <- c(0,0) 
  
  for (i in 1:length(Trials)){
    
    V <- beta*Q + beta_c*CK
    
    p = exp(V) / sum(exp(V))
    
    c <- sample(range(K), 1,prob = p)
    if (runif(1, min=0, max=1) < mu[c]){
      r = 1 
      d[c,2] = d[c,2]+1L
    } else{ r = 0 
    d[c,1] = d[c,1]+1L
    }
    
    delta <- c - Q[c]
    CK <- (1-alpha_c)*CK
    CK[c]+alpha_c 
    
    choices[(i)] <- c
    rewards[(i)] <- r
  }
  
  return(list(d=d,choices=choices,rewards=rewards))
}

#INICIO DE LA APP
# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyUI(
    navbarPage("Tareas de bandidos y Modelos de Comportamiento",    #navbarPage para abrir pestañas superiores
               tabPanel("Tareas de Bandidos",          #Iniciar código de primer pestaña 
                        h1("Tareas de Bandidos"),
                        br(),
                        h5("Xochitl Cárdenas - Lab 25"),
                        h5("Gabriela Facio - Lab 25"),
                        h5("Marco Negrete - Lab 25"),
                        hr(),
                        p("En muchas situaciones cotidianas las personas carecen de descripciones precisas acerca de las consecuencias de sus acciones, por lo cual, deben aprender a través de la experiencia, buscando disminuir la incertidumbre haciendo un balance entre dos conductas primordiales: exploración y explotación.",
                               "Por ejemplo, imagine que llegas a una ciudad relativamente nueva, donde conoces un buen restaurante para cenar. Sin embargo, te encuentras en el dilema de continuar cenando en este (explotación de la opción) o explorar en el resto de opciones (restaurantes) de la ciudad. El dilema existe ya que si te mantienes eligiendo el mismo restaurante, tendrás la confianza de recibir una buena comida, pero perderías la oportunidad de encontrar una mejor opción. Si cada día eliges un restaurante diferente, no te permitirías obtener la mayor recompensa del mejor lugar y probarías comidas desagradables más de una ocasión.",
                               "Un paradigma que ha permitido estudiar lo anterior desde Aprendizaje por Reforzamiento son las tareas de bandidos.",
                               "Bajo estas tareas se presentan varias opciones con diferentes distribuciones de recompensas, y el objetivo de los participantes es acumular la mayor cantidad de recompensa.",
                               "Se han propuesto diferentes modelos de cómo los participantes se pueden comportan ante estas tareas."),
                               br(),
                        h3("Modelando el comportamiento..."),
                        p("El objetivo del modelamiento computacional es utilizar modelos matematicos precisos para hacer comprender mejor los datos del comportamiento.",
                                "Los modelos vienen en forma de ecuaciones matemáticas que vinculan las variables observables experimentalmente con el comportamiento inmediato. En este sentido, instancian diferentes 'hipótesis algorítmicas' sobre cómo se genera el comportamiento.",
                                "Con estos modelos podemos generar datos 'falsos' estableciendo parámetros particulares (Wilson & Collins, 2019)."),
                               br(),
                               br(),
                        h4("Modelo 1: Random responding o respuesta aleatoria"),
                        p("Se asume que los participantes no se comprometen con la tarea y eligen de manera aleatoria, con un sesgo de preferencia por una de las opciones. Este sesgo es capturado por el parámetro b (que va entre 0 y 1). De modo que la probabilidad de elegir una de las dos opciones es:"),
                        tags$li(withMathJax('$$p_{t}^{1} = b \\ and \\ p_{t}^{2} = 1-b$$')),
                        tags$p("Para dos bandidos, este modelo sólo tiene un parámetro libre controlando el sesgo general por la opción 1 sobre la 2",
                               tags$li(withMathJax('$$ \\theta_1 = b$$'))),
                        br(),
                        br(),
                        h4("Modelo 2: Win-Stay-Lose-Shift"),
                        p("Este modelo adapta su comportamiento de acuerdo con la retroalimentación proveniente de las recompensas."),
                        br(),
                        p("El modelo repite acciones recompensadas y cambia a otra opción cuando no se le recompensa. En la versión ruidosa de este modelo, la regla 'win-stay-lose-shift- es aplicada de manera probabilistica, es decir, la regla se hace presente con probabilidad $$1 - \\epsilon$$"),
                        p(", y elige aleatoriamente con probabilidad $$\\epsilon$$"),
                        p("La probabilidad de elegir la opcion k es:"),
                        tags$li(withMathJax('$$ p_{t}^{k} $$')), #falta ecuacion
                        p("donde $$c_{t-1} = 1,2$$ es la elección en el ensayo $$t_{1} \\ y  \\ r_{t} = 0,1$$"),
                        p("la recompensa en el ensayo t. Este modelo sólo tiene un parámetro libre controlando el sesgo general"),
                        tags$li(withMathJax('$$\\theta_2 = \\epsilon$$')),
                        br(),
                        br(),
                        h4("Modelo 3: Rescorla - Wagner"),
                        p("En este modelo los participantes aprenden primero el *valor esperado* de cada opción basandose en la historia de recompensa de estas y utilizan este valor para elegir.", 
                           "Un modelo simple de aprendizaje es la *regla de aprendizaje de RW* donde el valor de cada opción *k*,"),
                        tags$li(withMathJax('$$Q_{t}^{k}$$')),
                        p("es actualizado en respuesta a la recompensa"),
                        tags$li(withMathJax('$$r_{t} $$')),
                        p("de acuerdo con:"),
                        tags$li(withMathJax('$$Q_{t+1}^{k}=Q_{k}^{t}+\\alpha(r_{t}-Q_{t}^{k})$$')),
                        p("donde"), 
                        tags$li(withMathJax('$$\\alpha$$')),
                        p("es la tasa de aprendizaje que puede ir entre 0 y 1, y captura la extensión en que el error de predicción,"),
                        tags$li(withMathJax('$$\\delta = r_{t}-Q_{t}^{k}$$')),
                        p("actualiza el valor. El modelo asume que los participantes utilizan los valores de las opciones para hacer sus elecciones, eligiendo aquella que tenga el valor más alto con mayor frecuencia, pero ocasionalmente comenen 'errores' eligiendo una opción con bajo valor. Una regla de elección con estas propiedades es *softmax*, la cual elige la opción *k* con probabilidad"),
                        tags$li(withMathJax('$$p_{t}^{k} = \\frac{exp(\\beta Q_{k}^{t})}{\\sum_{i=1}^{k}(exp\\beta Q_{i}^{t})}$$')),
                        p("donde"),
                        tags$li(withMathJax('$$\\beta$$')), 
                        p("es el parámetro de ´temperatura inversa´ que controla el nivel de estocasticidad en la elección. Comprendida desde"),
                        tags$li(withMathJax('$$\\beta = 0$$')),
                        p("para una respuesta completamente aleatoria y"),
                        tags$li(withMathJax('$$\\beta = \\infty$$')), 
                        p("para elección determinista para la opción con mayor valor. Este modelo tiene dos parámetros libres:"),
                        tags$li(withMathJax('$$ \\theta_3 = \\alpha,\\beta$$')),
                              br(),
                              br(),
                        h4("Modelo 4: Choice Kernel"),
                        p("Este modelo captura la tendencia de las personas a repetir elecciones previas. Se asume que se computa una 'elección kernel' para cada acción, la cual guarda un registro de la",
                          "frecuencia de elección de cada opción en el pasado reciente. Esta 'elección kernel' se actualiza de maneta similar a RW $$CK_{t+1}^{k}=CK_{t}^{k} + \\alpha_c(a_{t}^{k}-CK_{t}^{k})$$",
                          "donde $$\\alpha_{t}^{k}=1$$ si la opción k es jugada en el ensayo t, de otro modo $$\\alpha_{t}^{k}=0$$, y $$a_c$$ es la tasa de aprendizaje de elección kernel.",
                          "Este modelo igualmente se asume una regla de decisión softmax, pero sustituyendo el valor esperado por la elección kernel. $$\\theta_4 = \\alpha_c,\\beta_c$$"),
                              br(),
                              br(),
                        h4("Modelo 5: Choice Kernel + RW"),
                        p("En este modelo, actualizamos los valores de acuerdo con la regla de RW, mientras que la elección kernel se actualiza con la propia. De modo que la regla de elección es una",
                          "combinación de ambas reglas de aprendizaje $$p_{t}^{k}=\\frac{exp(\\beta Q_{k}^{t}+\\beta CK_{k}^{t})}{\\sum_{i=1}^{k}(exp\\beta Q_{i}^{t}+\\beta CK_{k}^{t})}$$",
                          "Los parámetros libres de este modelo son $$\\theta_5 = \\alpha, \\beta, \\alpha_c, \\beta_c$$"),
                        
                        
                        
                        br(em("Puedes encontrar el código de esta app en el siguiente link: ")),
                        tags$a (href="//github.com/xochitlcardenas/Teoria-del-Prospecto-Shinyapp.git", "Github repository")
               ), #cierra tabPanel

               
               tabPanel(title = "Modelamiento",          #segunda pestaÃ±a 
                        tags$h3("Modelamiento del comportamiento"),
                        tags$p("Un modelo computacional es una ecuación matemática parametrizada que representa la relación entre inputs y outputs.",  
                               "Sus componentes son la función, que describe las transformaciones que lleva a cabo el sistema, y los parámetros, que definen cómo se transforman los inputs.", 
                               "El comportamiento puede modelarse como una función optimizada para reducir algún costo, bajo un conjunto de restricciones, instrumentadas en arquitecturas de procesamiento.",
                               br(),
                               "El objetivo del modelamiento computacional es utilizar modelos matematicos precisos para hacer comprender mejor los datos del comportamiento.",
                               "Los modelos vienen en forma de ecuaciones matemáticas que vinculan las variables observables experimentalmente con el comportamiento inmediato. En este sentido, instancian diferentes 'hipótesis algorítmicas' sobre cómo se genera el comportamiento.",
                               "Con estos modelos podemos generar datos 'falsos' estableciendo parámetros particulares (Wilson & Collins, 2019).",
                               br(em("Te invito a que juegues con los valores y observes cómo se modifica la gráfica"))),
                        tabsetPanel(                              #abre subpestañas en pestaÃ±a superior 
                          
                          tabPanel(                               #primer pestaña
                            title = "Random responding",
                            tags$h3("Modelo Random Responding"),
                            
                            sidebarLayout(                       #divide pagina en dos partes
                              sidebarPanel(                      #primer parte, aparece barra de parametro variable
                               numericInput("trials1", "Elige el número de ensayos", value = 100, min = 1, max = 1000000),
                               sliderInput("mu1", "Elige el valor de mu (para la primera opción)", min = 0, max = 1, value = 0.5),
                               sliderInput("b", "Elige un valor del sesgo (b)", min = 0, max = 1, value = 0.5)
                              ),
                              mainPanel(                         #segunda parte, aparece gráfica 
                                plotOutput("modelo1"),      #variable que permite leer código de gráfica (se encuentra en server) 
                                tableOutput("tablem1")
                              )#cierra mainPanel
                            ) #cierra sidebarLayout
                          ), #cierra tabPanel de tabsetPanel RR
                          
                          tabPanel(
                            title = "WSLS",
                            tags$h3("Modelo Win-Sy-Lose-Shift"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("trials2", "Elige el número de ensayos", value = 100, min = 1, max = 1000000),
                                sliderInput("mu2", "Elige el valor de mu (para la primera opción)", min = 0, max = 1, value = 0.5),
                                sliderInput("epsilon", "Elige un valor de epsilon", min = 0, max = 1, value = 0.5)
                              ),
                              mainPanel(
                                plotOutput("modelo2"),
                                tableOutput("tablem2")
                              ) #Cierra mainPanel
                            ) #cierra layout
                          ), #cierra tabPanel WSLS
                          
                          tabPanel(
                            title = "RW",
                            tags$h3("Modelo Rescorla-Wagner"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("trials3", "Elige el número de ensayos", value = 100, min = 1, max = 1000000),
                                sliderInput("mu3", "Elige el valor de mu (para la primera opción)", min = 0, max = 1, value = 0.5),
                                sliderInput("alpha1", "Elige un valor de alpha", min = 0, max = 1, value = 0.5),
                                sliderInput("beta1", "Elige un valor de beta", min = 0, max = 100, value = 50)
                              ),
                              mainPanel(
                                plotOutput("modelo3"),
                                tableOutput("tablem3")
                              )
                            )
                          ), #cierra tabPanel modelo RW
                          
                          tabPanel(
                            title = "CK",
                            tags$h3("Modelo Choice Kernel"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("trials4", "Elige el número de ensayos", value = 100, min = 1, max = 1000000),
                                sliderInput("mu4", "Elige el valor de mu (para la primera opción)", min = 0, max = 1, value = 0.5),
                                sliderInput("alpha2", "Elige un valor de alpha", min = 0, max = 1, value = 0.5),
                                sliderInput("beta2", "Elige el valor de beta", min = 0, max = 100, value = 50)
                              ),
                              mainPanel(
                                plotOutput("modelo4"),
                                tableOutput("tablem4")
                              )
                            )
                          ),#cierra tabpanel de CK 
                          
                          tabPanel(
                            title = "RW - CK",
                            tags$h3("Modelo Rescorla - Wagner - Choice Kernel"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("trials5", "Elige el número de ensayos", value = 100, min = 1, max = 1000000),
                                sliderInput("mu5", "Elige el valor de mu (para la primera opción)", min = 0, max = 1, value = 0.5),
                                sliderInput("alpha3", "Elige un valor de alpha RW", min = 0, max = 1, value = 0.5),
                                sliderInput("beta3", "Elige el valor de beta RW", min = 0, max = 100, value = 50),
                                sliderInput("alphac3", "Elige un valor de alpha CK", min = 0, max = 1, value = 0.5),
                                sliderInput("betac3", "Elige el valor de beta RW", min = 0, max = 100, value = 50)
                              ),
                              mainPanel(
                                plotOutput("modelo5"),
                                tableOutput("tablem5")
                              )
                            )
                          )#cierra tabpanel RWCK
                          
                        ) #cierra el tabsetpanel
               ) #cierra el tabPanel
               
    ) #cierra el navbarPage
    
  ) #cierra shinyUI
)  #cierra fluidPage

#SERVER---------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$modelo1 <- renderPlot({
      
        Num1 <- input$trials1
        Trials1 <- 1:Num1
        mu_1 <- input$mu1
        mu1 <- c(mu_1 ,1-mu_1)
        data1 <- m_1(Trials1, mu1, input$b)
        
        plot(NULL, xlim = c(0,Num1), ylim = c(0,1), xlab = 'Ensayos', ylab = 'Resultados', yaxt = "n")
        axis(2, at = (0:1), labels = 0:1, tick = 0:1)
        lines(1:length(Trials1), data1$rewards, lty = 2, col = 'blue')
        elecciones1 <- data1$choices
        elecciones1 <- replace(elecciones1, elecciones1 == 1, 0)
        elecciones1 <- replace(elecciones1, elecciones1 == 2, 1)
        elecciones1 <- as.numeric(elecciones1)
        points(1:length(Trials1),elecciones1, pch = 4, col = 'red', bty="n")
        
        output$tablem1 <- renderTable({
          data1$d
        })
    })
    
    
    output$modelo2 <- renderPlot({
      
      Num2 <- input$trials2
      Trials2 <- 1:Num2
      mu_2 <- input$mu2
      mu2 <- c(mu_2 ,1-mu_2)
      data2 <- m_2(Trials2, mu2, input$epsilon)
      
      plot(NULL, xlim = c(0,Num2), ylim = c(0,1), xlab = 'Ensayos', ylab = 'Resultados', yaxt = "n")
      axis(2, at = (0:1), labels = 0:1, tick = 0:1)
      lines(1:length(Trials2), data2$rewards, lty = 2, col = 'blue')
      elecciones2 <- data2$choices
      elecciones2 <- replace(elecciones2, elecciones2 == 1, 0)
      elecciones2 <- replace(elecciones2, elecciones2 == 2, 1)
      elecciones2 <- as.numeric(elecciones2)
      points(1:length(Trials2),elecciones2, pch = 4, col = 'red', bty="n")
      
      output$tablem2 <- renderTable({
        data2$d
      })
      
    })
    
    output$modelo3 <- renderPlot({
      
      Num3 <- input$trials3
      Trials3 <- 1:Num3
      mu_3 <- input$mu3
      mu3 <- c(mu_3 ,1-mu_3)
      data3 <- m_3(Trials3, mu3, input$alpha1, input$beta1)
      
      plot(NULL, xlim = c(0,Num3), ylim = c(0,1), xlab = 'Ensayos', ylab = 'Resultados', yaxt = "n")
      axis(2, at = (0:1), labels = 0:1, tick = 0:1)
      lines(1:length(Trials3), data3$rewards, lty = 2, col = 'blue')
      elecciones3 <- data3$choices
      elecciones3 <- replace(elecciones3, elecciones3 == 1, 0)
      elecciones3 <- replace(elecciones3, elecciones3 == 2, 1)
      elecciones3 <- as.numeric(elecciones3)
      points(1:length(Trials3),elecciones3, pch = 4, col = 'red', bty="n")
      
      output$tablem3 <- renderTable({
        data3$d
      })
      
    })
    
    output$modelo4 <- renderPlot({
      
      Num4 <- input$trials4
      Trials4 <- 1:Num4
      mu_4 <- input$mu4
      mu4 <- c(mu_4 ,1-mu_4)
      data4 <- m_4(Trials4, mu4, input$alpha2, input$beta2)
      
      plot(NULL, xlim = c(0,Num4), ylim = c(0,1), xlab = 'Ensayos', ylab = 'Resultados', yaxt = "n")
      axis(2, at = (0:1), labels = 0:1, tick = 0:1)
      lines(1:length(Trials4), data4$rewards, lty = 2, col = 'blue')
      elecciones4 <- data4$choices
      elecciones4 <- replace(elecciones4, elecciones4 == 1, 0)
      elecciones4 <- replace(elecciones4, elecciones4 == 2, 1)
      elecciones4 <- as.numeric(elecciones4)
      points(1:length(Trials4),elecciones4, pch = 4, col = 'red', bty="n")
      
      output$tablem4 <- renderTable({
        data4$d
      })
      
    })
    
    output$modelo5 <- renderPlot({
      
      Num5 <- input$trials5
      Trials5 <- 1:Num5
      mu_5 <- input$mu5
      mu5 <- c(mu_5 ,1-mu_5)
      data5 <- m_5(Trials5, mu5, input$alpha3, input$beta3, input$alphac3, input$betac3)
      
      plot(NULL, xlim = c(0,Num5), ylim = c(0,1), xlab = 'Ensayos', ylab = 'Resultados', yaxt = "n")
      axis(2, at = (0:1), labels = 0:1, tick = 0:1)
      lines(1:length(Trials5), data5$rewards, lty = 2, col = 'blue')
      elecciones5 <- data5$choices
      elecciones5 <- replace(elecciones5, elecciones5 == 1, 0)
      elecciones5 <- replace(elecciones5, elecciones5 == 2, 1)
      elecciones5 <- as.numeric(elecciones5)
      points(1:length(Trials5),elecciones5, pch = 4, col = 'red', bty="n")
      
      output$tablem5 <- renderTable({
        data5$d
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
