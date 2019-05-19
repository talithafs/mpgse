## app.R ##
library(shiny)
library(shinydashboard)

source("models.R")
source("data.R")
source("graphs.R")

ui <- dashboardPage(
  dashboardHeader(title = "Master's Project"),
  dashboardSidebar(width=250,
    sidebarMenu(
      menuItem("Ventura and Voth's Models", tabName = "ventvoth", icon = icon("drafting-compass"),
               menuSubItem("Pre-Industrial", tabName = "preind"),
               menuSubItem("Industrial: No debt, No Frictions", tabName = "ndnf"),
               menuSubItem("Industrial: No debt, Frictions", tabName = "ndwf"),
               menuSubItem("Industrial: Debt, Frictions", tabName = "wdwf")),
      menuItem("Chinese Data",tabName = "data", icon = icon("database")),
      menuItem("Simulations for China", tabName = "simuls", icon = icon("play"),
               menuSubItem("With Debt, No Growth", tabName = "simul_debt"),
               menuSubItem("No Debt nor Growth", tabName = "simul_no_debt"),
               menuSubItem("With Growth, No Debt", tabName = "simul_growth_no_debt"),
               menuSubItem("With Growth and Debt", tabName = "simul_growth_debt"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "preind",
              fluidRow(
              
                box(
                  status = "info",
                  title = "Parameters",
                  sliderInput("preind_lambda", "Lambda:", 0, 1, 0.2, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("preind_alpha", "Alpha:", 0, 1, 0.4, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("preind_delta", "Delta:", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("preind_beta", "Beta:", 0, 1, 0.9, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("preind_x", "X:", 0, 1, 0.1, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  numericInput("preind_k0","k0:",50)
                ),
                
                box(status = "success", plotOutput("preind_k", height = 250)),
                
                infoBoxOutput("preind_kss")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "ndnf",
              fluidRow(
                box(
                  status = "info",
                  title = "Parameters",
                  radioButtons("ndnf_compare", "Compare with", inline = T, choices = list("None" = 1, "Pre-Industrial" = 2, "Frictions, No Debt" = 3,
                                                                  "Frictions and Debt" = 4),selected = 1),
                  sliderInput("ndnf_pi", "Pi:", 1, 10, 3, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndnf_lambda", "Lambda:", 0, 1, 0.2, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndnf_alpha", "Alpha:", 0, 1, 0.4, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndnf_delta", "Delta:", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndnf_beta", "Beta:", 0, 1, 0.9, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndnf_x", "X:", 0, 1, 0.1, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  numericInput("ndnf_k0","k0:",50),
                  numericInput("ndnf_s0","s0:",0.1)
                ),
                
                box(status = "success", plotOutput("ndnf_k", height = 250)),
                
                box(status = "danger", plotOutput("ndnf_s", height = 250)),
                
                infoBoxOutput("ndnf_kss"),
                infoBoxOutput("ndnf_sss")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "ndwf",
              fluidRow(
                box(
                  status = "info",
                  title = "Parameters",
                  radioButtons("ndwf_compare", "Compare with", inline = T, choices = list("None" = 1, "Pre-Industrial" = 2, "Frictions, No Debt" = 3,
                                                                                          "Frictions and Debt" = 4),selected = 1),
                  sliderInput("ndwf_pi", "Pi:", 1, 10, 3, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndwf_lambda", "Lambda:", 0, 1, 0.2, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndwf_alpha", "Alpha:", 0, 1, 0.4, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndwf_delta", "Delta:", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndwf_beta", "Beta:", 0, 1, 0.9, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("ndwf_x", "X:", 0, 1, 0.13, step = 0.01, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  numericInput("ndwf_k0","k0:",50),
                  numericInput("ndwf_s0","s0:",0.001)
                ),
                
                box(status = "success", plotOutput("ndwf_k", height = 250)),
                
                box(status = "danger", plotOutput("ndwf_s", height = 250)),
                
                infoBoxOutput("ndwf_kss"),
                infoBoxOutput("ndwf_sss")
              )
      ),
      
      tabItem(tabName = "wdwf",
              fluidRow(
                box(
                  status = "info",
                  title = "Parameters",
                  radioButtons("wdwf_compare", "Compare with", inline = T, choices = list("None" = 1, "Pre-Industrial" = 2, "No Frictions, No Debt" = 3,
                                                                                          "Frictions, No Debt" = 4),selected = 1),
                  sliderInput("wdwf_pi", "Pi:", 1, 10, 3, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_lambda", "Lambda:", 0, 1, 0.2, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_alpha", "Alpha:", 0, 1, 0.4, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_delta", "Delta:", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_beta", "Beta:", 0, 1, 0.9, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_x", "X:", 0, 1, 0.13, step = 0.01, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("wdwf_tau", "Tau:", 0, 1, 0.11, step = 0.01, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  numericInput("wdwf_k0","k0:",50),
                  numericInput("wdwf_s0","s0:",0.001),
                  numericInput("wdwf_d0","d0:",0)
                ),
                
                box(status = "success", plotOutput("wdwf_k", height = 250)),
                box(status = "danger", plotOutput("wdwf_s", height = 250)),
                box(status = "warning", plotOutput("wdwf_d", height = 250)),
                
                infoBoxOutput("wdwf_kss"),
                infoBoxOutput("wdwf_sss")
              )
      ),
      
      tabItem(tabName = "simul_debt",
              fluidRow(
                box(
                  status = "info",
                  title = "Options",
                  selectInput("except", "Calibrate all except:",
                              c("None" = "none",
                                "Non-Productive Factors as %GDP" = "lambda",
                                "Private Capital as %GDP" = "alpha",
                                "Saving Rate" = "beta",
                                "FDI as %GDP" = "x",
                                "Taxes as %GDP" = "tau")),
                  htmlOutput("china_pars_pre"),
                  htmlOutput("china_pars_post"),
                  br(),
                  sliderInput("china_pi", "Relative Efficiency of Investment (Pi):", 1, 50, 15, step = 0.5, animate =
                                 animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("china_delta", "Depreciation Rate (Delta):", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  conditionalPanel("input.except == 'lambda'",
                     sliderInput("china_lambda", "Non-Productive Factors as %GDP (Lambda):", 0, 1, 0.2, animate =
                                   animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except == 'alpha'",
                     sliderInput("china_alpha", "Private Capital as %GDP (Alpha):", 0, 1, 0.4, animate =
                                   animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except == 'beta'",
                     sliderInput("china_beta", "Saving Rate (Beta):", 0, 1, 0.9, animate =
                                   animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except == 'x'",
                     sliderInput("china_x", "FDI as %GDP (X):", 0, 1, 0.13, animate =
                                   animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except == 'tau'",
                     sliderInput("china_tau", "Taxes as %GDP (Tau):", 0, 1, 0.11, animate =
                                     animationOptions(interval = 300, loop = FALSE))
                  ),

                  numericInput("china_k0","k0:",50),
                  numericInput("china_s0","s0:",0.001),
                  numericInput("china_d0","d0:",0),
                  numericInput("china_iters","Number of Iterations:",500)
                ),
                
                box(status = "success", plotOutput("china_k", height = 250)),
                box(status = "danger", plotOutput("china_s", height = 250)),
                box(status = "warning", plotOutput("china_d", height = 250)),
                box(status = "info", plotOutput("china_real", height = 250)),
                
               infoBoxOutput("china_pre_ss"),
               infoBoxOutput("china_post_ss")
                
               
              )
      ),
      
      tabItem(tabName = "simul_no_debt",
              fluidRow(
                box(
                  status = "info",
                  title = "Options",
                  selectInput("except_alt", "Calibrate all except:",
                              c("None" = "none",
                                "Non-Productive Factors as %GDP" = "lambda",
                                "Private Capital as %GDP" = "alpha",
                                "Saving Rate" = "beta",
                                "FDI as %GDP" = "x")),
                  htmlOutput("china_alt_pars_pre"),
                  htmlOutput("china_alt_pars_post"),
                  br(),
                  sliderInput("china_alt_pi", "Relative Efficiency of Investment (Pi):", 0, 50, 6, step = 0.5, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("china_alt_delta", "Depreciation Rate (Delta):", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  conditionalPanel("input.except_alt == 'lambda'",
                                   sliderInput("china_alt_lambda", "Non-Productive Factors as %GDP (Lambda):", 0, 1, 0.2, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_alt == 'alpha'",
                                   sliderInput("china_alt_alpha", "Private Capital as %GDP (Alpha):", 0, 1, 0.4, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_alt == 'beta'",
                                   sliderInput("china_alt_beta", "Saving Rate (Beta):", 0, 1, 0.9, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_alt == 'x'",
                                   sliderInput("china_alt_x", "FDI as %GDP (X):", 0, 1, 0.01, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  
                  numericInput("china_alt_k0","k0:",50),
                  numericInput("china_alt_s0","s0:",0.001),
                  numericInput("china_alt_iters","Number of Iterations:",500)
                ),
                
                box(status = "success", plotOutput("china_alt_k", height = 250)),
                box(status = "danger", plotOutput("china_alt_s", height = 250)),
                box(status = "info", plotOutput("china_alt_real", height = 250)),
                
                infoBoxOutput("china_alt_pre_ss"),
                infoBoxOutput("china_alt_post_ss")
                
                
              )
      ),
      
      tabItem(tabName = "simul_growth_no_debt", tabsetPanel(
              tabPanel("The Model", fluidRow(
                box(
                  status = "info",
                  title = "Options",
                  selectInput("except_gnd", "Calibrate all except:",
                              c("None" = "none",
                                "Non-Productive Factors as %GDP" = "lambda",
                                "Private Capital as %GDP" = "alpha",
                                "Saving Rate" = "beta",
                                "FDI as %GDP" = "x",
                                "Growth Rate" = "g")),
                  htmlOutput("china_gnd_pars_pre"),
                  htmlOutput("china_gnd_pars_post"),
                  br(),
                  sliderInput("china_gnd_pi", "Relative Efficiency of Investment (Pi):", 0, 50, 6, step = 0.5, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("china_gnd_delta", "Depreciation Rate (Delta):", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  conditionalPanel("input.except_gnd == 'lambda'",
                                   sliderInput("china_gnd_lambda", "Non-Productive Factors as %GDP (Lambda):", 0, 1, 0.2, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gnd == 'alpha'",
                                   sliderInput("china_gnd_alpha", "Private Capital as %GDP (Alpha):", 0, 1, 0.4, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gnd == 'beta'",
                                   sliderInput("china_gnd_beta", "Saving Rate (Beta):", 0, 1, 0.9, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gnd == 'x'",
                                   sliderInput("china_gnd_x", "FDI as %GDP (X):", 0, 1, 0.01, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gnd == 'g'",
                                   sliderInput("china_gnd_g", "GDP Growth (g):", 0, 1, 0.1, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  
                  numericInput("china_gnd_k0","k0:",1),
                  numericInput("china_gnd_s0","s0:",0.001),
                  numericInput("china_gnd_A0","A0:",1),
                  numericInput("china_gnd_iters","Number of Iterations:",100)
                ),
                
                box(status = "success", plotOutput("china_gnd_k", height = 250)),
                box(status = "danger", plotOutput("china_gnd_s", height = 250)),
                
                infoBoxOutput("china_gnd_post_ss")
              )),
              tabPanel("Other Results", 
                       fluidRow(box(status = "info", plotOutput("china_gnd_cap", height = 250)),
                                box(status = "info", plotOutput("china_gnd_wag", height = 250)),
                                box(status = "info", plotOutput("china_gnd_gdp", height = 250)),
                                box(status = "info", plotOutput("china_gnd_con", height = 250))
                       )
              )
      )),
      
      tabItem(tabName = "simul_growth_debt",
              fluidRow(
                box(
                  status = "info",
                  title = "Options",
                  selectInput("except_gd", "Calibrate all except:",
                              c("None" = "none",
                                "Non-Productive Factors as %GDP" = "lambda",
                                "Private Capital as %GDP" = "alpha",
                                "Saving Rate" = "beta",
                                "FDI as %GDP" = "x",
                                "Growth Rate" = "g",
                                "Tax Revenue as %GDP" = "tau")),
                  htmlOutput("china_gd_pars_pre"),
                  htmlOutput("china_gd_pars_post"),
                  br(),
                  sliderInput("china_gd_pi", "Relative Efficiency of Investment (Pi):", 0, 50, 6, step = 0.5, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  sliderInput("china_gd_delta", "Depreciation Rate (Delta):", 0, 1, 0.04, animate =
                                animationOptions(interval = 300, loop = FALSE)),
                  conditionalPanel("input.except_gd == 'lambda'",
                                   sliderInput("china_gd_lambda", "Non-Productive Factors as %GDP (Lambda):", 0, 1, 0.2, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gd == 'alpha'",
                                   sliderInput("china_gd_alpha", "Private Capital as %GDP (Alpha):", 0, 1, 0.4, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gd == 'beta'",
                                   sliderInput("china_gd_beta", "Saving Rate (Beta):", 0, 1, 0.9, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gd == 'x'",
                                   sliderInput("china_gd_x", "FDI as %GDP (X):", 0, 1, 0.01, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gd == 'g'",
                                   sliderInput("china_gd_g", "GDP Growth (g):", 0, 1, 0.1, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  conditionalPanel("input.except_gd == 'tau'",
                                   sliderInput("china_gd_tau", "Taxes as %GDP (Tau):", 0, 1, 0.11, animate =
                                                 animationOptions(interval = 300, loop = FALSE))
                  ),
                  
                  numericInput("china_gd_k0","k0:",1),
                  numericInput("china_gd_s0","s0:",0.001),
                  numericInput("china_gd_d0","d0:",0),
                  numericInput("china_gd_A0","A0:",1),
                  numericInput("china_gd_iters","Number of Iterations:",100)
                ),
                
                box(status = "success", plotOutput("china_gd_k", height = 250)),
                box(status = "danger", plotOutput("china_gd_s", height = 250)),
                box(status = "warning", plotOutput("china_gd_d", height = 250)),
                box(status = "info", plotOutput("china_gd_real", height = 250)),
                
                infoBoxOutput("china_gd_post_ss")
              )
      ),
      
      tabItem(tabName = "data", tabsetPanel(
        
        tabPanel("Savings Rate (Beta)", fluidRow(infoBoxOutput("info_beta", width = 12), plotOutput("plot_beta"))),
        tabPanel("Compensation as %GDP (1 - Alpha - Lambda)", fluidRow(infoBoxOutput("info_lambda", width = 12), plotOutput("plot_lambda"))),
        tabPanel("FDI as %GDP (X)", fluidRow(infoBoxOutput("info_x", width = 12), plotOutput("plot_x"))),
        tabPanel("Tax Revenue as %GDP (Tau)", fluidRow(infoBoxOutput("info_tau", width = 12), plotOutput("plot_tau"))),
        tabPanel("Capital as %GDP (Alpha)", fluidRow(infoBoxOutput("info_alpha", width = 12), plotOutput("plot_alpha"))),
        tabPanel("GDP Growth (g)", fluidRow(infoBoxOutput("info_g", width = 12), plotOutput("plot_g"))),
        tabPanel("GDP", fluidRow(infoBoxOutput("info_gdp", width = 12), plotOutput("plot_gdp"))),
        tabPanel("Consumption", fluidRow(infoBoxOutput("info_cons", width = 12), plotOutput("plot_cons"))),
        tabPanel("Capital Stock", fluidRow(infoBoxOutput("info_stk", width = 12), plotOutput("plot_stk")))
      
      ))
    )
  )
)

server <- function(input, output) { 
  
  preind <- reactive({
    
    k0 <- input$preind_k0
    x <- input$preind_x
    delta <- input$preind_delta
    beta <- input$preind_beta
    lambda <- input$preind_lambda
    alpha <- input$preind_alpha
    
    values <- recur.k(k.preind,500,k0,1,0,x,delta,beta,lambda,alpha)
    kss <- kss.preind(x,delta,beta,lambda,alpha)
    
    info <- infoBox("Steady State", paste0("k = ",round(kss,2)), icon = icon("anchor"), fill= TRUE, width = 2)
    p <- plot.plain(values,kss,title="Capital Stock","Pre-Industrial Model",ylbl="k")
    
    return(list(p = p, info = info))
  })  
  
  ndnf <- reactive({
    
    k0 = input$ndnf_k0
    s0 = input$ndnf_s0
    x = input$ndnf_x
    delta = input$ndnf_delta
    beta = input$ndnf_beta
    lambda = input$ndnf_lambda
    alpha = input$ndnf_alpha
    pi = input$ndnf_pi
    
    k.vals <- recur.k(k.ndnf,500,k0,1,0,x,delta,beta,lambda,alpha,pi)
    s.vals <- recur.s(s.ndnf,500,k.vals,s0,x,delta,beta,lambda,alpha,pi)
    kss <- kss.ndnf(x,delta,beta,lambda,alpha,pi)
    sss <- 0 
    
    info <- infoBox(
      "Steady States", paste0("k = ",round(kss,2)), paste0("s = ",sss), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    if(input$ndnf_compare == 1){
      
      ps <- plot.plain(s.vals,sss,title="Capital Share","No Frictions/No Debt Model",ylbl="s", col="#f8766d")
      pk <- plot.plain(k.vals,kss,title="Capital Stock","No Frictions/No Debt Model",ylbl="k")
      
    } else if(input$ndnf_compare == 2){
      
      tit <- "No Frictions/No Debt compared to Pre-Industrial"
      
      pre.s <- 0
      pre.sss <- 0
      ps <- plot.compared(s.vals,pre.s,sss,pre.sss,"NFND","Pre-Ind","Capital Share",tit,"s")
      
      pre.k <- recur.k(k.preind,500,k0,1,0,x,delta,beta,lambda,alpha)
      pre.kss <- kss.preind(x,delta,beta,lambda,alpha)
      pk <- plot.compared(k.vals,pre.k,kss,pre.kss,"NFND","Pre-Ind","Capital Stock",tit,"k")
    
    } else if(input$ndnf_compare == 3){
      
      tit <- "No Frictions/No Debt compared to Frictions/No Debt"
      
      ret <- recur.both(k.ndwf,s.ndwf,500,k0,s0,x,delta,beta,lambda,alpha,pi)
      alt.kss <- kss.ndwf(x,delta,beta,lambda,alpha,pi)
      alt.sss <- sss.ndwf(x,delta,beta,lambda,alpha,pi)
      
      ps <- plot.compared(s.vals,ret$s,sss,alt.sss,"NFND","With Frictions","Capital Share",tit,"s")
      pk <- plot.compared(k.vals,ret$k,kss,alt.kss,"NFND","With Frictions","Capital Stock", tit,"k")
      
    } else if(input$ndnf_compare == 4){
      
      tit <- "No Frictions/No Debt compared to Frictions/Debt"
      
      ret <- recur.debt(k.wdwf,s.wdwf,d.wdwf,500,k0,s0,d0,x,delta,beta,lambda,alpha,pi,tau)
      alt.kss <- kss.wdwf(delta,beta,lambda,alpha,pi,tau)
      alt.sss <- sss.wdwf(x,delta,beta,lambda,alpha,pi,tau)
      
      ps <- plot.compared(s.vals,ret$s,sss,alt.sss,"NFND","With both","Capital Share",tit,"s")
      pk <- plot.compared(k.vals,ret$k,kss,alt.kss,"NFND","With both","Capital Stock", tit,"k")
      
    } 
    
    list(info = info, pk = pk, ps = ps)
  })
  
  ndwf <- reactive({
    
    k0 = input$ndwf_k0
    s0 = input$ndwf_s0
    x = input$ndwf_x
    delta = input$ndwf_delta
    beta = input$ndwf_beta
    lambda = input$ndwf_lambda
    alpha = input$ndwf_alpha
    pi = input$ndwf_pi
    
    ret <- recur.both(k.ndwf,s.ndwf,500,k0,s0,x,delta,beta,lambda,alpha,pi)
    kss <- kss.ndwf(x,delta,beta,lambda,alpha,pi)
    sss <- sss.ndwf(x,delta,beta,lambda,alpha,pi)
    
    info <- infoBox(
      "Steady States", paste0("k = ",round(kss,2)), paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    if(input$ndwf_compare == 1){
      
      ps <- plot.plain(ret$s,sss,title="Capital Share","With Frictions/No Debt Model",ylbl="s", col="#f8766d")
      pk <- plot.plain(ret$k,kss,title="Capital Stock","With Frictions/No Debt Model",ylbl="k")
      
    } else if(input$ndwf_compare == 2){
      
      tit <- "With Frictions/No Debt compared to Pre-Industrial"
      
      pre.s <- 0
      pre.sss <- 0
      ps <- plot.compared(ret$s,pre.s,sss,pre.sss,"WFND","Pre-Ind","Capital Share",tit,"s")
      
      pre.k <- recur.k(k.preind,500,k0,1,0,x,delta,beta,lambda,alpha)
      pre.kss <- kss.preind(x,delta,beta,lambda,alpha)
      pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"WFND","Pre-Ind","Capital Stock",tit,"k")
      
    } else if(input$ndwf_compare == 3){
      
      tit <- "With Frictions/No Debt compared to No Frictions/No Debt"
      
      k.vals <- recur.k(k.ndnf,500,k0,1,0,x,delta,beta,lambda,alpha,pi)
      s.vals <- recur.s(s.ndnf,500,k.vals,s0,x,delta,beta,lambda,alpha,pi)
      alt.kss <- kss.ndnf(x,delta,beta,lambda,alpha,pi)
      alt.sss <- 0 
      
      ps <- plot.compared(ret$s,s.vals,sss,alt.sss,"WFND","No frictions","Capital Share",tit,"s")
      pk <- plot.compared(ret$k,k.vals,kss,alt.kss,"WFND","No Frictions","Capital Stock", tit,"k")
      
    } else if(input$ndwf_compare == 4){
      
      tit <- "With Frictions/No Debt compared to Frictions/Debt"
      
      alt.ret <- recur.debt(k.wdwf,s.wdwf,d.wdwf,500,k0,s0,d0,x,delta,beta,lambda,alpha,pi,tau)
      alt.kss <- kss.wdwf(delta,beta,lambda,alpha,pi,tau)
      alt.sss <- sss.wdwf(x,delta,beta,lambda,alpha,pi,tau)
      
      ps <- plot.compared(ret$s,alt.ret$s,sss,alt.sss,"WFND","With debt","Capital Share",tit,"s")
      pk <- plot.compared(ret$k,alt.ret$k,kss,alt.kss,"WFND","With debt","Capital Stock", tit,"k")
    } 
    
    list(info = info, pk = pk, ps = ps)
  })
  
  wdwf <- reactive({
    
    k0 = input$wdwf_k0
    s0 = input$wdwf_s0
    d0 = input$wdwf_d0
    x = input$wdwf_x
    delta = input$wdwf_delta
    beta = input$wdwf_beta
    lambda = input$wdwf_lambda
    alpha = input$wdwf_alpha
    pi = input$wdwf_pi
    tau = input$wdwf_tau
    
    ret <- recur.debt(k.wdwf,s.wdwf,d.wdwf,500,k0,s0,d0,x,delta,beta,lambda,alpha,pi,tau)
    kss <- kss.wdwf(delta,beta,lambda,alpha,pi,tau)
    sss <- sss.wdwf(x,delta,beta,lambda,alpha,pi,tau)
    
    info <- infoBox(
      "Steady States", paste0("k = ",round(kss,2)), paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    if(input$wdwf_compare == 1){
      
      ps <- plot.plain(ret$s,sss,title="Capital Share","With Frictions/No Debt Model",ylbl="s", col="#f8766d")
      pk <- plot.plain(ret$k,kss,title="Capital Stock","With Frictions/No Debt Model",ylbl="k")
      
    } else if(input$wdwf_compare == 2){
      
      tit <- "With Frictions/With Debt compared to Pre-Industrial"
      
      pre.s <- 0
      pre.sss <- 0
      ps <- plot.compared(ret$s,pre.s,sss,pre.sss,"WFWD","Pre-Ind","Capital Share",tit,"s")
      
      pre.k <- recur.k(k.preind,500,k0,1,0,x,delta,beta,lambda,alpha)
      pre.kss <- kss.preind(x,delta,beta,lambda,alpha)
      pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"WFWD","Pre-Ind","Capital Stock",tit,"k")
      
    } else if(input$wdwf_compare == 3){
      
      tit <- "With Frictions/With Debt compared to No Frictions/No Debt"
      
      k.vals <- recur.k(k.ndnf,500,k0,1,0,x,delta,beta,lambda,alpha,pi)
      s.vals <- recur.s(s.ndnf,500,k.vals,s0,x,delta,beta,lambda,alpha,pi)
      alt.kss <- kss.ndnf(x,delta,beta,lambda,alpha,pi)
      alt.sss <- 0 
      
      ps <- plot.compared(ret$s,s.vals,sss,alt.sss,"WFWD","No frictions","Capital Share",tit,"s")
      pk <- plot.compared(ret$k,k.vals,kss,alt.kss,"WFWD","No frictions","Capital Stock", tit,"k")
      
    } else if(input$wdwf_compare == 4){
      tit <- "With Frictions/With Debt compared to Frictions/No Debt"
      
      alt.ret <- recur.both(k.ndwf,s.ndwf,500,k0,s0,x,delta,beta,lambda,alpha,pi)
      alt.kss <- kss.ndwf(x,delta,beta,lambda,alpha,pi)
      alt.sss <- sss.ndwf(x,delta,beta,lambda,alpha,pi)
      
      ps <- plot.compared(ret$s,alt.ret$s,sss,alt.sss,"WFWD","No debt","Capital Share",tit,"s")
      pk <- plot.compared(ret$k,alt.ret$k,kss,alt.kss,"WFWD","No debt","Capital Stock", tit,"k")
      
    } 
    
    pd <- plot.plain(ret$d,0,title="Debt","With Frictions/With Debt Model",ylbl="d")
    
    list(info = info, pk = pk, ps = ps, pd = pd)
  })
  
  china <- reactive({
    
    exc <- input$except 
    params <- calibrate(exc)
    
    if(exc == 'x'){
      post_x = input$china_x
    } else {
      post_x = params$post$x
    }
    
    pre_x = params$pre$x
    
    if(exc == 'alpha'){
      post_alpha = input$china_alpha
      post_lambda = params$post$lambda - post_alpha
    } else {
      post_alpha = params$post$alpha
    }
    
    pre_alpha = params$pre$alpha
    
    if(exc == 'lambda'){
      post_lambda = input$china_lambda
    } else {
      post_lambda = params$post$lambda
    }
    
    pre_lambda = params$pre$lambda
    
    if(exc == 'beta'){
      post_beta = input$china_beta
    } else {
      post_beta = params$post$beta
    }
    
    pre_beta = params$pre$beta
    
    if(exc == 'tau'){
      post_tau = input$china_tau
    } else {
      post_tau = params$post$tau
    }
    
    pre_tau = params$pre$tau
    
    k0 = input$china_k0
    s0 = input$china_s0
    d0 = input$china_d0
    delta = input$china_delta
    pi = input$china_pi
    
    iters <- input$china_iters
    
    # Pre
    pre.s <- 0
    pre.sss <- 0
    pre.k <- recur.k(k.preind,iters,k0,1,0,pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    pre.kss <- kss.preind(pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    
    # Post
    ret <- recur.debt(k.wdwf,s.wdwf,d.wdwf,iters,pre.kss,s0,d0,post_x,delta,post_beta,post_lambda,post_alpha,pi,post_tau)
    kss <- kss.wdwf(delta,post_beta,post_lambda,post_alpha,pi,post_tau)
    sss <- sss.wdwf(post_x,delta,post_beta,post_lambda,post_alpha,pi,post_tau)
    
    info_post <- infoBox(
      "Post Steady States", paste0("k = ",round(kss,2)), paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    info_pre <- infoBox(
      "Pre Steady States", paste0("k = ",round(pre.kss,2)), icon = icon("anchor"), fill= TRUE, width = 2, color="purple"
    )
    
    pars_post <- paste0("<b>Post:</b> Alpha = ", post_alpha, ", Lambda = ", post_lambda, ", X = ", post_x, ", Tau = ", post_tau, ", Beta = ", post_beta)
    pars_pre <- paste0("<b>Pre:</b> Alpha = ", pre_alpha, ", Lambda = ", pre_lambda, ", X = ", pre_x, ", Tau = ", pre_tau, ", Beta = ", pre_beta)
    
    tit = "Pre-1982 versus Post-1982 Models"
    ps <- plot.plain(ret$s,sss,title="Capital Share",subtitle="Post-1982 Model", ylbl="s", col="#f8766d")
    pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"Post","Pre","Capital Stock",tit,"k")
    pd <- plot.plain(ret$d,0,title="Debt","Post-1982 Model",ylbl="d",col = "#fd7e14")
    
    stk = adj.series(get.series("stkpc")$series,k0)
    n = length(stk$series)
    subtit <- paste0("Per capita; Mapping: 1 unit of capital pc = ", stk$factor, " US$ in PPP")
    pr <- plot.real(ret$k[1:n],stk$series,title="Capital Stock: Real versus Simulated Data", cols = c("#00ba38","black"), subtitle = subtit)
    
    
    list(info_pre = info_pre, info_post = info_post, pk = pk, ps = ps, pd = pd, pr = pr, pars_post = pars_post, pars_pre = pars_pre)
    
  })
  
  china_alt <- reactive({
    
    exc <- input$except_alt 
    params <- calibrate(exc)
    
    if(exc == 'x'){
      post_x = input$china_alt_x
    } else {
      post_x = params$post$x
    }
    
    pre_x = params$pre$x
    
    if(exc == 'alpha'){
      post_alpha = input$china_alt_alpha
      post_lambda = params$post$lambda - post_alpha
    } else {
      post_alpha = params$post$alpha
    }
    
    pre_alpha = params$pre$alpha
    
    if(exc == 'lambda'){
      post_lambda = input$china_alt_lambda
    } else {
      post_lambda = params$post$lambda
    }
    
    pre_lambda = params$pre$lambda
    
    if(exc == 'beta'){
      post_beta = input$china_alt_beta
    } else {
      post_beta = params$post$beta
    }
    
    pre_beta = params$pre$beta
    
    k0 = input$china_alt_k0
    s0 = input$china_alt_s0
    d0 = input$china_alt_d0
    delta = input$china_alt_delta
    pi = input$china_alt_pi
    
    iters <- input$china_alt_iters
    
    # Pre
    pre.s <- 0
    pre.sss <- 0
    pre.k <- recur.k(k.preind,iters,k0,1,0,pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    pre.kss <- kss.preind(pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    
    # Post
    ret <- recur.both(k.ndwf,s.ndwf,iters,pre.kss,s0,post_x,delta,post_beta,post_lambda,post_alpha,pi)
    kss <- kss.ndwf(post_x,delta,post_beta,post_lambda,post_alpha,pi)
    sss <- sss.ndwf(post_x,delta,post_beta,post_lambda,post_alpha,pi)
    
    info_post <- infoBox(
      "Post Steady States", paste0("k = ",round(kss,2)), paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    info_pre <- infoBox(
      "Pre Steady States", paste0("k = ",round(pre.kss,2)), icon = icon("anchor"), fill= TRUE, width = 2, color="purple"
    )
    
    pars_post <- paste0("<b>Post:</b> Alpha = ", post_alpha, ", Lambda = ", post_lambda, ", X = ", post_x, ", Beta = ", post_beta)
    pars_pre <- paste0("<b>Pre:</b> Alpha = ", pre_alpha, ", Lambda = ", pre_lambda, ", Beta = ", pre_beta)
    
    tit = "Pre-1982 versus Post-1982 Models"
    ps <- plot.plain(ret$s,sss,title="Capital Share",subtitle="Post-1982 Model", ylbl="s", col="#f8766d")
    pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"Post","Pre","Capital Stock",tit,"k")
    
    stk = adj.series(get.series("stkpc")$series,k0)
    n = length(stk$series)
    subtit <- paste0("Per Capita; Mapping: 1 unit of capital pc = ", stk$factor, " US$ in PPP")
    pr <- plot.real(ret$k[1:n],stk$series,title="Capital Stock: Real versus Simulated Data", cols = c("#00ba38","black"), subtitle = subtit)
    
    list(info_pre = info_pre, info_post = info_post, pk = pk, ps = ps, pr = pr, pars_post = pars_post, pars_pre = pars_pre)
    
  })
  
  china_gnd <- reactive({
    
    exc <- input$except_gnd 
    params <- calibrate(exc)
    
    if(exc == 'x'){
      post_x = input$china_gnd_x
    } else {
      post_x = params$post$x
    }
    
    pre_x = params$pre$x
    
    if(exc == 'alpha'){
      post_alpha = input$china_gnd_alpha
      post_lambda = params$post$lambda - post_alpha
    } else {
      post_alpha = params$post$alpha
    }
    
    pre_alpha = params$pre$alpha
    
    if(exc == 'lambda'){
      post_lambda = input$china_gnd_lambda
    } else {
      post_lambda = params$post$lambda
    }
    
    pre_lambda = params$pre$lambda
    
    if(exc == 'beta'){
      post_beta = input$china_gnd_beta
    } else {
      post_beta = params$post$beta
    }
    
    pre_beta = params$pre$beta
    
    if(exc == 'g'){
      post_g = input$china_gnd_g
    } else {
      post_g = params$post$g
    }
    
    pre_g = params$pre$g
    
    k0 = input$china_gnd_k0
    s0 = input$china_gnd_s0
    A0 = input$china_gnd_A0
    delta = input$china_gnd_delta
    pi = input$china_gnd_pi
    
    iters <- input$china_gnd_iters
    
    # Pre
    pre.s <- 0
    pre.sss <- 0
    pre.k <- recur.k(k.gpreind,iters,k0,A0,pre_g,pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    pre.kss <- 0
    
    # Post 
    ret <- recur.growth(k.gndwf,s.gndwf,NULL,iters,post_g,k0,s0,NULL,A0,post_x,delta,post_beta,post_lambda,post_alpha,pi)
    sss <- ret$s[length(ret$s)]
    kss <- 0

    info_post <- infoBox(
      "Post Steady State", paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    pars_post <- paste0("<b>Post:</b> Alpha = ", post_alpha, ", Lambda = ", post_lambda, ", X = ", post_x, ", Beta = ", post_beta, ", g = ", post_g)
    pars_pre <- paste0("<b>Pre:</b> Alpha = ", pre_alpha, ", Lambda = ", pre_lambda, ", Beta = ", pre_beta, ", g = ", post_g)
    
    tit = "Pre-1982 versus Post-1982 Models"
    ps <- plot.plain(ret$s,sss,title="Capital Share",subtitle="Post-1982 Model", ylbl="s", col="#f8766d")
    pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"Post","Pre","Capital Stock",tit,"k")
    
    stk = adj.series(get.series("stkpc")$series,k0)
    n = length(stk$series)
    subtit <- paste0("Per Capita; Mapping: 1 unit of capital pc = ", stk$factor, " US$ in PPP")
    pcap <- plot.real(ret$k[1:n],as.vector(stk$series),title="Capital Stock", cols = c("royalblue","black"), subtitle = subtit)
    
    sts <- get.stats(ret$k,ret$s,A0,post_g,post_x,delta,post_beta,post_lambda,post_alpha,pi)
    
    gdp = adj.series(get.series("ypc")$series,sts$y[1])
    n = length(gdp$series)
    subtit <- paste0("Per Capita; Mapping: 1 unit of final good pc = ", gdp$factor, " Constant 2010 US$")
    pgdp <- plot.real(sts$y[1:n],gdp$series,title="Gross Domestic Product: Real versus Simulated Data", cols = c("royalblue","black"), subtitle = subtit)
    
    pcon <- plot.compared(sts$w,sts$pk, 0, 0, name1 = "Labor", name2 = "Prod. Cap.", title="Marginal Costs: Labor versus Productive Capital", subtitle="Simulated Data",ylbl="MgC", cols = c("#00ba38","#fd7e14"))
    pwag <- plot.compared(sts$w,sts$uk, 0, 0, name1 = "Labor", name2 = "Non-Prod. Cap.", title="Marginal Costs: Labor versus Non-Productive Capital", subtitle="Simulated Data",ylbl="MgC", cols = c("#00ba38","#fd7e14"))
    
    list(info_post = info_post, pk = pk, ps = ps, pcap = pcap, pcon = pcon, pwag = pwag, pgdp = pgdp, pars_post = pars_post, pars_pre = pars_pre)
  })
  
  
  china_gd <- reactive({
    
    exc <- input$except_gd 
    params <- calibrate(exc)
    
    if(exc == 'x'){
      post_x = input$china_gd_x
    } else {
      post_x = params$post$x
    }
    
    pre_x = params$pre$x
    
    if(exc == 'alpha'){
      post_alpha = input$china_gd_alpha
      post_lambda = params$post$lambda - post_alpha
    } else {
      post_alpha = params$post$alpha
    }
    
    pre_alpha = params$pre$alpha
    
    if(exc == 'lambda'){
      post_lambda = input$china_gd_lambda
    } else {
      post_lambda = params$post$lambda
    }
    
    pre_lambda = params$pre$lambda
    
    if(exc == 'beta'){
      post_beta = input$china_gd_beta
    } else {
      post_beta = params$post$beta
    }
    
    pre_beta = params$pre$beta
    
    if(exc == 'g'){
      post_g = input$china_gd_g
    } else {
      post_g = params$post$g
    }
    
    pre_g = params$pre$g
    
    if(exc == 'tau'){
      post_tau = input$china_gd_tau
    } else {
      post_tau = params$post$tau
    }
    
    pre_tau = params$pre$tau
    
    k0 = input$china_gd_k0
    s0 = input$china_gd_s0
    d0 = input$china_gd_d0
    A0 = input$china_gd_A0
    delta = input$china_gd_delta
    pi = input$china_gd_pi
    
    iters <- input$china_gd_iters
    
    # Pre
    pre.s <- 0
    pre.sss <- 0
    pre.k <- recur.k(k.gpreind,iters,k0,A0,pre_g,pre_x,delta,pre_beta,pre_lambda,pre_alpha)
    pre.kss <- 0
    
    # Post
    ret <- recur.growth(k.gwdwf,s.gwdwf,d.gwdwf,iters,post_g,k0,s0,d0,A0,post_x,delta,post_beta,post_lambda,post_alpha,pi,post_tau)
    kss <- 0
    sss <- ret$s[length(ret$s)]

    info_post <- infoBox(
      "Post Steady State", paste0("s = ",round(sss,2)), icon = icon("anchor"), fill= TRUE, width = 2
    )
    
    pars_post <- paste0("<b>Post:</b> Alpha = ", post_alpha, ", Lambda = ", post_lambda, ", X = ", post_x, ", Beta = ", post_beta, ", g = ", post_g, ", Tau = ",post_tau)
    pars_pre <- paste0("<b>Pre:</b> Alpha = ", pre_alpha, ", Lambda = ", pre_lambda, ", Beta = ", pre_beta, ", g = ", post_g, ", Tau = ",post_tau)
    
    tit = "Pre-1982 versus Post-1982 Models"
    ps <- plot.plain(ret$s,sss,title="Capital Share",subtitle="Post-1982 Model", ylbl="s", col="#f8766d")
    pk <- plot.compared(ret$k,pre.k,kss,pre.kss,"Post","Pre","Capital Stock",tit,"k")
    pd <- plot.plain(ret$d,0,title="Debt","Post-1982 Model",ylbl="d",col = "#fd7e14")

    stk = adj.series(get.series("stkpc")$series,k0)
    n = length(stk$series)
    subtit <- paste0("Per Capita; Mapping: 1 unit of capital pc = ", stk$factor, " US$ in PPP")
    pr <- plot.real(ret$k[1:n],stk$series,title="Capital Stock", cols = c("#00ba38","black"), subtitle = subtit)
    
    list(info_post = info_post, pk = pk, ps = ps, pd =pd, pr = pr, pars_post = pars_post, pars_pre = pars_pre)
  })
  
  output$preind_kss<- renderInfoBox(preind()$info)
  output$preind_k <- renderPlot(preind()$p)
  
  output$ndnf_kss <- renderInfoBox(ndnf()$info)
  output$ndnf_k <- renderPlot(ndnf()$pk)
  output$ndnf_s <- renderPlot(ndnf()$ps)
  
  output$ndwf_kss <- renderInfoBox(ndwf()$info)
  output$ndwf_k <- renderPlot(ndwf()$pk)
  output$ndwf_s <- renderPlot(ndwf()$ps)

  output$wdwf_kss <- renderInfoBox(wdwf()$info)
  output$wdwf_k <- renderPlot(wdwf()$pk)
  output$wdwf_s <- renderPlot(wdwf()$ps)
  output$wdwf_d <- renderPlot(wdwf()$pd)
  
  output$china_pre_ss <- renderInfoBox(china()$info_pre)
  output$china_post_ss <- renderInfoBox(china()$info_post)
  output$china_k <- renderPlot(china()$pk)
  output$china_s <- renderPlot(china()$ps)
  output$china_d <- renderPlot(china()$pd)
  output$china_pars_pre <- renderText(china()$pars_pre)
  output$china_pars_post <- renderText(china()$pars_post)
  output$china_real <- renderPlot(china()$pr)
  
  output$china_alt_pre_ss <- renderInfoBox(china_alt()$info_pre)
  output$china_alt_post_ss <- renderInfoBox(china_alt()$info_post)
  output$china_alt_k <- renderPlot(china_alt()$pk)
  output$china_alt_s <- renderPlot(china_alt()$ps)
  output$china_alt_pars_pre <- renderText(china_alt()$pars_pre)
  output$china_alt_pars_post <- renderText(china_alt()$pars_post)
  output$china_alt_real <- renderPlot(china_alt()$pr)
  
  output$china_gnd_post_ss <- renderInfoBox(china_gnd()$info_post)
  output$china_gnd_k <- renderPlot(china_gnd()$pk)
  output$china_gnd_s <- renderPlot(china_gnd()$ps)
  output$china_gnd_pars_pre <- renderText(china_gnd()$pars_pre)
  output$china_gnd_pars_post <- renderText(china_gnd()$pars_post)
  
  output$china_gnd_cap <- renderPlot(china_gnd()$pcap)
  output$china_gnd_gdp <- renderPlot(china_gnd()$pgdp)
  output$china_gnd_con <- renderPlot(china_gnd()$pcon)
  output$china_gnd_wag <- renderPlot(china_gnd()$pwag)
  
  output$china_gd_post_ss <- renderInfoBox(china_gd()$info_post)
  output$china_gd_k <- renderPlot(china_gd()$pk)
  output$china_gd_s <- renderPlot(china_gd()$ps)
  output$china_gd_d <- renderPlot(china_gd()$pd)
  output$china_gd_pars_pre <- renderText(china_gd()$pars_pre)
  output$china_gd_pars_post <- renderText(china_gd()$pars_post)
  output$china_gd_real <- renderPlot(china_gd()$pr)
  
  s.beta <- get.series("beta")
  output$info_beta <- renderInfoBox(infoBox(title = s.beta$name, subtitle = s.beta$description, icon = icon("chart-line")))
  output$plot_beta <- renderPlot(plot.series(s.beta$series, title = s.beta$name, subtitle = "Used to calibrate Beta", caption = "Source: World Bank"))
  
  s.alpha <- get.series("alpha")
  output$info_alpha <- renderInfoBox(infoBox(title = s.alpha$name, subtitle = s.alpha$description,icon = icon("chart-line")))
  output$plot_alpha <- renderPlot(plot.series(s.alpha$series, title = s.alpha$name, subtitle = "Used to calibrate Alpha", caption = "Source: World Bank"))
  
  s.lambda <- get.series("lambda")
  output$info_lambda <- renderInfoBox(infoBox(title = s.lambda$name, subtitle = s.lambda$description,icon = icon("chart-line")))
  output$plot_lambda <- renderPlot(plot.series(s.lambda$series, title = s.lambda$name, subtitle = "Used to calibrate Lambda (= 1 - Comp/GDP - GFCF/GDP)", caption = "Source: Federal Reserve of St. Louis"))
  
  s.tau <- get.series("tau")
  output$info_tau <- renderInfoBox(infoBox(title = s.tau$name, subtitle = s.tau$description,icon = icon("chart-line")))
  output$plot_tau <- renderPlot(plot.series(s.tau$series, title = s.tau$name, subtitle = "Used to calibrate Tau", caption = "Source: World Bank"))
  
  s.x <- get.series("x")
  output$info_x <- renderInfoBox(infoBox(title = s.x$name, subtitle = s.x$description,icon = icon("chart-line")))
  output$plot_x <- renderPlot(plot.series(s.x$series, title = s.x$name, subtitle = "Used to calibrate X", caption = "Source: World Bank"))
  
  s.g <- get.series("g")
  output$info_g <- renderInfoBox(infoBox(title = s.g$name, subtitle = s.g$description,icon = icon("chart-line")))
  output$plot_g <- renderPlot(plot.series(s.g$series, title = s.g$name, subtitle = "Used to calibrate g", caption = "Source: IMF"))
  
  s.gdp <- get.series("y")
  output$info_gdp <- renderInfoBox(infoBox(title = s.gdp$name, subtitle = s.gdp$description,icon = icon("chart-line")))
  output$plot_gdp <- renderPlot(plot.series(s.gdp$series, title = s.gdp$name, subtitle = "Constant 2010 US$", caption = "Source: World Bank"))

  s.c <- get.series("c")
  output$info_cons <- renderInfoBox(infoBox(title = s.c$name, subtitle = s.c$description,icon = icon("chart-line")))
  output$plot_cons <- renderPlot(plot.series(s.c$series, title = s.c$name, subtitle = "Constant 2010 US$", caption = "Source: World Bank"))
  
  s.stk <- get.series("stk")
  output$info_stk <- renderInfoBox(infoBox(title = s.stk$name, subtitle = s.stk$description,icon = icon("chart-line")))
  output$plot_stk <- renderPlot(plot.series(s.stk$series, title = s.stk$name, subtitle = "Constant National Prices", caption = "Source: Federal Reserve of St. Louis"))
  
}

shinyApp(ui, server)