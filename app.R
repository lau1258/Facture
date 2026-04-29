library(shiny)
library(shinyjs)
library(glue)
library(quarto)

# Infos fixes de l'entreprise
entreprise <- list(
  nom = "",
  adresse = "",
  tel = "",
  mail = ""
)

# Numéros de taxes fixes
taxes_entreprise <- list(
  tps = "",
  tvq = ""
)

# Fichier qui garde le prochain numéro de facture
fichier_numero <- "dernier_numero_facture.txt"

if (!file.exists(fichier_numero)) {
  writeLines("1", fichier_numero)
}

numero_facture_auto <- as.numeric(readLines(fichier_numero))

format_argent <- function(x) {
  paste0(
    format(round(x, 2), decimal.mark = ",", big.mark = " ", nsmall = 2),
    " $"
  )
}

ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("Créateur de facture"),
  
  fluidRow(
    
    column(
      width = 6,
      h4("Informations de l'entreprise"),
      tags$div(
        style = "border:1px solid #999; border-top:20px solid #b3b3b3; padding:10px;",
        strong(entreprise$nom), br(),
        entreprise$adresse, br(),
        entreprise$tel, br(),
        tags$span(entreprise$mail, style = "color:#0070C0; text-decoration: underline;")
      )
    ),
    
    column(
      width = 6,
      h4("Facture"),
      tags$div(
        style = "border:1px solid #999; padding:10px;",
        
        checkboxInput(
          "modifier_facture",
          "Modifier le numéro ou la date",
          value = FALSE
        ),
        
        textInput(
          "numero",
          "Numéro de facture",
          value = sprintf("%03d", numero_facture_auto)
        ),
        
        dateInput(
          "date",
          "Date",
          value = Sys.Date(),
          format = "dd-mm-yyyy"
        )
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 9,
      
      h4("Client"),
      textInput("client", "Vendu à", value = "", width = "100%"),
      textInput(
        "adresse_client",
        "Adresse client",
        placeholder = "1999 Avenue Laurence, Montréal, QC, H2H 1J6",
        width = "100%"
      ),
      textInput(
        "telephone_client",
        "Téléphone",
        value = "",
        placeholder = "(123) 123-1234",
        width = "100%"
      ),
      
      hr(),
      
      h4("Service"),
      textAreaInput(
        "description",
        "Description",
        value = "",
        width = "100%",
        rows = 4
      ),
      numericInput("montant", "Montant avant taxes", value = NULL, min = 0, width = "100%"),
      
      checkboxInput(
        "inclure_taxes",
        "Inclure les taxes (TPS + TVQ)",
        value = TRUE
      ),
      
      hr(),
      
      fluidRow(
        column(
          width = 6,
          
          h4("Numéros de taxes"),
          
          tags$div(
            style = "background-color:#eeeeee; padding:10px; border:1px solid #ccc; border-radius:5px;",
            
            textInput(
              "numero_tps",
              "Numéro TPS",
              value = taxes_entreprise$tps,
              width = "100%"
            ),
            
            textInput(
              "numero_tvq",
              "Numéro TVQ",
              value = taxes_entreprise$tvq,
              width = "100%"
            )
          )
        ),
        
        column(
          width = 6,
          
          h4("Aperçu"),
          tableOutput("resume")
        )
      ),
      
      br(),
      
      downloadButton("download_pdf", "Télécharger la facture PDF")
    )
  )
)
server <- function(input, output, session) {
  
  numero_courant <- reactiveVal(numero_facture_auto)
  
  observe({
    shinyjs::toggleState("numero", input$modifier_facture)
    shinyjs::toggleState("date", input$modifier_facture)
    
    shinyjs::disable("numero_tps")
    shinyjs::disable("numero_tvq")
  })
  
  # Format automatique du téléphone client
  observeEvent(input$telephone_client, {
    
    chiffres <- gsub("[^0-9]", "", input$telephone_client)
    
    if (nchar(chiffres) >= 10) {
      chiffres <- substr(chiffres, 1, 10)
      
      tel_format <- paste0(
        "(", substr(chiffres, 1, 3), ") ",
        substr(chiffres, 4, 6), "-",
        substr(chiffres, 7, 10)
      )
      
      updateTextInput(session, "telephone_client", value = tel_format)
    }
  })
  
  calculs <- reactive({
    montant <- input$montant
    if (input$inclure_taxes) {
      tps <- montant * 0.05
      tvq <- montant * 0.09975
      taxes <- tps + tvq
      total <- montant + taxes
    } else {
      tps <- 0
      tvq <- 0
      taxes <- 0
      total <- montant
    }
    
    data.frame(
      Élément = c("Sous-total", "TPS", "TVQ", "Taxes", "Total"),
      Montant = c(
        format_argent(montant),
        format_argent(tps),
        format_argent(tvq),
        format_argent(taxes),
        format_argent(total)
      )
    )
  })
  
  output$resume <- renderTable({
    calculs()
  })
  
  output$download_pdf <- downloadHandler(
    
    filename = function() {
      paste0("facture_", input$numero, ".pdf")
    },
    
    contentType = "application/pdf",
    
    content = function(file) {
      
      # Validation du téléphone
      if (!grepl("^\\(\\d{3}\\) \\d{3}-\\d{4}$", input$telephone_client)) {
        showNotification("Numéro de téléphone invalide. Format attendu : (123) 123-1234", type = "error")
        return(NULL)
      }
      
      montant <- input$montant
      
      if (input$inclure_taxes) {
        tps <- montant * 0.05
        tvq <- montant * 0.09975
        taxes <- tps + tvq
        total <- montant + taxes
      } else {
        tps <- 0
        tvq <- 0
        taxes <- 0
        total <- montant
      }
      
      temp_dir <- tempdir()
      temp_qmd <- file.path(temp_dir, "facture.qmd")
      temp_pdf <- file.path(temp_dir, "facture.pdf")
      
      file.copy("facture.qmd", temp_qmd, overwrite = TRUE)
      
      quarto::quarto_render(
        input = temp_qmd,
        output_format = "typst",
        output_file = "facture.pdf",
        execute_params = list(
          numero = input$numero,
          date_facture = format(input$date, "%d-%m-%Y"),
          
          entreprise_nom = entreprise$nom,
          entreprise_adresse = entreprise$adresse,
          entreprise_tel = entreprise$tel,
          entreprise_mail = entreprise$mail,
          
          client = input$client,
          adresse_client = input$adresse_client,
          telephone_client = input$telephone_client,
          
          description = input$description,
          montant = montant,
          inclure_taxes = input$inclure_taxes,
          
          numero_tps = taxes_entreprise$tps,
          numero_tvq = taxes_entreprise$tvq,
          tps = tps,
          tvq = tvq,
          taxes = taxes,
          total = total
        )
      )
      
      file.copy(temp_pdf, file, overwrite = TRUE)
      
      numero_actuel <- as.numeric(input$numero)
      
      if (!is.na(numero_actuel)) {
        prochain_numero <- numero_actuel + 1
        writeLines(as.character(prochain_numero), fichier_numero)
        numero_courant(prochain_numero)
        
        updateTextInput(
          session,
          "numero",
          value = sprintf("%03d", prochain_numero)
        )
      }
    }
  )
}
shinyApp(ui, server)
