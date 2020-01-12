
#setwd("~/Documents/R stuff/sas-bkp/")

library(shiny)
library(shinyWidgets)
library(readxl)
library(readr)
library(dplyr)

# Total skattesats per kommun
skattes <- read_excel("2.-skattesatser-2018-forandring-kommunvis-korrigerad-2019-01-18..xlsx", skip=7, range = "A8:B298")

# Timmar och minuter i sträng-format
timmar = c(paste("0", as.character(0:9), sep="" ), as.character(10:23) )
minuter = c("00", as.character(seq(15,45, by=15) ) )
# Dagar med särskild OB
dag1OB4 = c("Trettondagen", "Första maj", "Nationaldagen", "Kristi himmelsfärdsdag", "Alla helgons dag")
dag2OB4 = paste(dag1OB4, "+1")
dag1OB5a = c("Skärtorsdagen", "Nyårsafton")
dag2OB5a = c("Långfredagen", "Nyårsdagen")
dag1OB5b = c("Pingstdagen", "Midsommarafton", "Julafton")
dag2OB5b = paste(dag1OB5b, "+1")

helgd_plus <- as.matrix(read.csv("helgd_plus.csv", stringsAsFactors = F))
colnames(helgd_plus) = c("Nyårsdagen", "Trettondagen", "Skärtorsdagen", "Långfredagen", "Första maj", "Pingstdagen", "Nationaldagen", "Kristi Himmelsfärdsdag", "Midsommarafton", "Alla helgons dag", "Julafton", "Nyårsafton", dag2OB4, dag2OB5b)

# Matriser för alla tider med/utan ob
obMat = matrix(data = 0, nrow = 7, ncol = 24)
rownames(obMat) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
obMat[1:5, 19:24] <- 29.16
obMat[2:5, 1:6] <- 63.63
obMat[c(1,6,7), 1:6] <- 59.65

# Helgdags-matriser för alla tider med/utan ob
ob5Mat1 = ob5Mat2 = ob5Mat3 = ob5Mat4 = ob4Mat1 = ob4Mat2 = obMat
ob4Mat1[,] <- 62.30; ob4Mat2[,1:6] <- 62.30;
ob5Mat1[,19:24] <- 116.64; ob5Mat2[,1:6] <- 116.64; 
ob5Mat3[,] <- 116.64; ob5Mat4[,1:6] <- 116.64; 
obList <- list(ob4Mat1, ob4Mat2, ob5Mat1, ob5Mat2, ob5Mat3, ob5Mat4)

# Matris för arbetstid och timmar
arbTim <- matrix(0, ncol = 24, nrow = 2)

# Define UI for application
ui <- fluidPage(
  setBackgroundColor("Honeydew"),
  tags$head( tags$meta(name = "viewport", content = "width=1300"),uiOutput("body")),
                 

  # Application title
  titlePanel("Inkomst per arbetspass"),
  h4(em("SAS"), "Bagagekastare"),

  sidebarLayout(
    sidebarPanel(
  fluidRow( # Ange datum och timlön
    column(width=6, 
           dateInput("datum", "Datum", min = , format = "yyyy-mm-dd", weekstart = 1, width = "100%" ) ),
    column(width=6, 
           numericInput("lön", "Timlön", min = 80, value = 153, max = 220, width = "60%") )
  ),
  fluidRow( # Ange start- och slut-tider
    column(width=3, selectInput("tidst", "Start (h)", choices = timmar, width = "110%" ) ),
    column(width=3, selectInput("tidsm", "(min)", choices = minuter, width = "110%" ) ),
    column(width=3, selectInput("tidslt", "Slut (h)", choices = timmar, width = "110%" ) ),
    column(width=3, selectInput("tidslm", "(min)", choices = minuter, width = "110%" ) )
  ),
  fluidRow( 
    column(width=12, # Ange skattesats (kommun) 
           selectInput("sks", "Skattesats", choices = skattes$Kommun, selected = "Sollentuna kommun", width = "73%"), 
           column(width=3,offset = 9, # Lägg till/ladda ned info
                  actionButton("add", "Lägg till", width = "136%"),
                  downloadButton("dl", "Spara") ) 
           ) 
  )
  ),
  mainPanel( # Dina arbetspass i tabellformat
           tableOutput("Pass"),
           tags$style(type="text/css", "#Pass tr:last-child {font-weight:bold;}")
    )
  )
)

# Define server logic required
server <- function(input, output) {
   
  sttid <- reactive({ # starttid för passet
    as.numeric(input$tidst) + as.numeric(input$tidsm)/60
  })
  timmar <- reactive({ # antal jobbtimmar
    x = as.numeric(input$tidslt) + as.numeric(input$tidslm)/60 - (as.numeric(input$tidst) + as.numeric(input$tidsm)/60)
    if(x < 0) { 24+x } else { x }
  })
  sltid <- reactive({ # sluttid för passet
    sttid() + timmar()
  })
  dag <- reactive({ # veckodag då passet börjar
    format(input$datum, "%a")
  })
  datum <- reactive({ # passets start-datum
    as.character(input$datum)
  })
  datum2 <- reactive({ # dagen efter
    as.character(input$datum+1)
  })
  dats <- reactive({ # bägge datum
    c(datum(), datum2() )
  })
  datumDag <- reactive({
    if(datum() %in% helgd_plus) {
      # vilket index har datum i helgd-matrisen?
      ixmat <- which(helgd_plus %in% datum() )
      k <- arrayInd(ixmat, dim(helgd_plus))
      
      # vilket kolumn-namn har detta index?
      colnames(helgd_plus)[k[,2]]

    } else { format(input$datum, "%a %d %b") }
  })
  skats <- reactive({
    skattes[which(skattes == input$sks), 2]
  })
  
  lön <- reactive({ # beräkna lön/inkomst
    rownames(arbTim) <- c(dag(), format(input$datum+1, "%a"))
    
    if( sltid() <= 24 && sltid() != sttid() ) { # ej över midnatt
      tim=c(sttid()-floor(sttid()), rep(1, abs(ceiling(sttid()) - sltid() ) ), sltid()-floor(sltid()) )
      arbTim[1,(floor(sttid())+1):ceiling(sltid() )] <- tim[tim > 0]
    } else if( sltid() > 24 && sttid() > 0) { # över midnatt
      tim1 = c(floor(sttid()+1)-sttid(), rep(1, (24-floor(sttid()+1) ) ) )
      arbTim[1,(floor(sttid())+1):24] <- tim1[tim1 > 0]
      tim2 = c(rep(1, floor(sltid() ) - 24), sltid()-floor(sltid() ) )
      arbTim[2, 1:length(tim2[tim2 > 0 ])] <- tim2[tim2 > 0]
    }

    # är datum/datum+1 en helgd?
    if( any(dats() %in% helgd_plus) ) {
      # vilket/vilka av dats() är i helgd?
      ixd <- which(dats() %in% helgd_plus)

      # vilket/vilka index har datum/datum+1 i helgd-matrisen?
      ixmat <- which(helgd_plus %in% dats()[ixd])
      k <- arrayInd(ixmat, dim(helgd_plus))

      # i vilken/vilka kolumn(er) (helgdag) finns detta index?
      coln <- colnames(helgd_plus)[k[,2]]

      # i vilken/vilka OB-vektor finns denna helgdag?
      ix1OB4 <- which(coln %in% dag1OB4)
      ix2OB4 <- which(coln %in% dag2OB4)
      ix1OB5a <- which(coln %in% dag1OB5a)
      ix2OB5a <- which(coln %in% dag2OB5a)
      ix1OB5b <- which(coln %in% dag1OB5b)
      ix2OB5b <- which(coln %in% dag2OB5b)
      cix <- list(ix1OB4, ix2OB4, ix1OB5a, ix2OB5a, ix1OB5b, ix2OB5b)
      
      if(length(coln) == 2 ) { 
        obMat1 <- obList[[which(cix == 1)]]; obMat2 <- obList[[which(cix == 2)]]
      } else { if(ixd == 2) { obMat1 <- obMat; obMat2 <- obList[[which(cix==1)]] } else { obMat2 <- obMat; obMat1 <- obList[[which(cix==1)]] } }

      ixr1 <- which(rownames(obMat1) %in% rownames(arbTim)[1])
      ixr2 <- which(rownames(obMat2) %in% rownames(arbTim)[2])
      pay = sum(obMat1[ixr1, ]*arbTim[1,]) + sum(obMat2[ixr2, ]*arbTim[2,]) + sum(arbTim)*input$lön
      pay

    } else {
      ixr <- which(rownames(obMat) %in% rownames(arbTim))
      pay = sum(obMat[ixr, ]*arbTim) + sum(arbTim)*input$lön
      pay 
    }
    
  }) 
  output$ink = renderUI(HTML( # Output med inkomst för arbetspasset
    paste("Under ett ", timmar(), " timmars arbetspass ", datumDag(),
          " kl ", paste(paste(input$tidst, input$tidsm, sep=":"), paste(input$tidslt, input$tidslm, sep=":"), sep="-" ), "" ," <br>tjänar du ", round(lön(), 2)," kr, före skatt.", 
          " <br> Total skattesats i ", input$sks, " är ", skats(), " %. <br> Din inkomst efter skatt blir därmed ", round(lön()*(1-skats()/100 ), digits = 2), " kr." )
  ))
  

  dig <- reactiveValues() # Tilläggsnummer
  
  observeEvent(input$add, { # vid "Lägg till", insert UI med table
    nr = input$add
    id = paste0("input",input$add)
    dig$dList <- c(isolate(dig$dList), isolate(input$add) )
    insertUI(
      selector = "#Pass",
      ui = tableOutput("pass")
    )
  })
  
  # Listor med för pass relevanta värden
  Datum <- reactiveValues(); Datum$list <- character(40)
  #Timlön <- reactiveValues();
  Tid <- reactiveValues(); Tid$list <- character(40)
  Timmar <- reactiveValues(); Timmar$list <- numeric(40)
  Inkomst <- reactiveValues(); Inkomst$list <- numeric(40)
  OB <- reactiveValues(); OB$list <- numeric(40)
  Totalink <- reactiveValues(); Totalink$list <- numeric(40)
  
  
  observeEvent(input$add, { # Pre-allocate!
    if(input$add > 0) { # Vid "Lägg till", utöka listor
      Datum$list[input$add] <- as.character(isolate(input$datum))
      #Datum$list <- c(isolate(Datum$list), as.character(isolate(input$datum)) )
      Tid$list[input$add] <- paste(paste(input$tidst, input$tidsm, sep=":"), paste(input$tidslt, input$tidslm, sep=":"), sep="-" )
      Timmar$list[input$add] <- isolate(timmar())
      Inkomst$list[input$add] <- isolate(round(lön()-(lön() - input$lön*timmar() ) ) ) 
      OB$list[input$add] <- isolate(round(lön() - input$lön*timmar(), 2) ) 
      Totalink$list[input$add] <- isolate(lön() )  
      }
  })
  
  # Skapa dataframe av alla pass
  df_mt <- reactive({
    ix <- which(Datum$list != "")
    df_p <- data.frame(
             Datum = Datum$list[ix],
             Tid = Tid$list[ix],
             Timmar = Timmar$list[ix], 
             Inkomst = Inkomst$list[ix],
             OB = OB$list[ix],
             Totalink1 = Totalink$list[ix],
             Totalink2 = round(isolate(Totalink$list[ix])*(1-as.numeric(skats() )/100 ), 2),
             stringsAsFactors = F
            )
    names(df_p)[4:7] <- c("Primär inkomst","Intjänad OB","Inkomst före skatt","Inkomst efter skatt")
    summation <- c(NA,"Totalt:", sum(df_p$Timmar), sum(df_p$`Primär inkomst`), round(sum(df_p$`Intjänad OB`), 2), round(sum(df_p$`Inkomst före skatt`), 2), round(sum(df_p$`Inkomst efter skatt`), 2) )
    df_p <- rbind(df_p, summation)
    df_p
  })
  
  # Skapa tabell för din måltid
  output$pass <- renderTable(colnames = T, na="", {
    if(input$add > 0) { df_mt() }
  })
  
  # Ladda ned tabell med arbetspass
  output$dl = downloadHandler(
    filename = function() { paste("ArbetspassInkomst_", df_mt()[1,1], ":", input$datum, ".tsv", sep = "")},
    content = function(file2) {
      write_tsv(df_mt(), file2, col_names = T,  na = "")
      }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

