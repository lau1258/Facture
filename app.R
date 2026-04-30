library(shiny)
library(shinyjs)
library(glue)
library(pagedown)
library(htmltools)

entreprise_initiale <- list(
  nom = "",
  adresse = "",
  tel = "",
  mail = "",
  tps = "",
  tvq = ""
)

fichier_numero <- "dernier_numero_facture.txt"

if (!file.exists(fichier_numero)) {
  writeLines("1", fichier_numero)
}

numero_facture_auto <- as.numeric(readLines(fichier_numero))

format_argent <- function(x) {
  if (is.null(x) || is.na(x)) x <- 0
  
  paste0(
    format(round(x, 2), decimal.mark = ",", big.mark = " ", nsmall = 2),
    " $"
  )
}

ui <- fluidPage(
  
  useShinyjs(),
  
  fluidRow(
    
    column(
      width = 6,
      
      fileInput(
        "fichier_entreprise",
        "Charger les informations de l'entreprise",
        accept = c(".csv")
      ),
      
      tags$div(
        style = "border:1px solid #999; border-top:20px solid #b3b3b3; padding:10px;",
        strong(textOutput("entreprise_nom", inline = TRUE)), br(),
        textOutput("entreprise_adresse", inline = TRUE), br(),
        textOutput("entreprise_tel", inline = TRUE), br(),
        tags$span(
          textOutput("entreprise_mail", inline = TRUE),
          style = "color:#0070C0; text-decoration: underline;"
        )
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
          value = sprintf("%03d", numero_facture_auto),
          width = "100%"
        ),
        
        dateInput(
          "date",
          "Date",
          value = Sys.Date(),
          format = "dd-mm-yyyy",
          width = "100%"
        )
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 12,
      
      h4("Vendu à"),
      
      textInput("client", "Nom du client", value = "", width = "100%"),
      
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
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 12,
      
      h4("Description du service"),
      
      textAreaInput(
        "description",
        label = NULL,
        value = "",
        width = "100%",
        rows = 5
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 12,
      
      numericInput(
        "montant",
        "Montant avant taxes",
        value = NULL,
        min = 0,
        width = "100%"
      ),
      
      checkboxInput(
        "inclure_taxes",
        "Inclure les taxes (TPS + TVQ)",
        value = TRUE
      )
    )
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
          value = "",
          width = "100%"
        ),
        
        textInput(
          "numero_tvq",
          "Numéro TVQ",
          value = "",
          width = "100%"
        )
      )
    ),
    
    column(
      width = 6,
      
      h4("Total"),
      tableOutput("resume")
    )
  ),
  
  br(),
  
  downloadButton("download_html", "Télécharger la facture")
)

server <- function(input, output, session) {
  
  numero_courant <- reactiveVal(numero_facture_auto)
  entreprise <- reactiveVal(entreprise_initiale)
  
  observeEvent(input$fichier_entreprise, {
    
    fichier <- input$fichier_entreprise$datapath
    
    infos <- read.csv(
      fichier,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM",
      strip.white = TRUE
    )
    
    entreprise(list(
      nom = infos$nom[1],
      adresse = infos$adresse[1],
      tel = infos$tel[1],
      mail = infos$mail[1],
      tps = infos$tps[1],
      tvq = infos$tvq[1]
    ))
    
    updateTextInput(session, "numero_tps", value = infos$tps[1])
    updateTextInput(session, "numero_tvq", value = infos$tvq[1])
  })
  
  output$entreprise_nom <- renderText({
    entreprise()$nom
  })
  
  output$entreprise_adresse <- renderText({
    entreprise()$adresse
  })
  
  output$entreprise_tel <- renderText({
    entreprise()$tel
  })
  
  output$entreprise_mail <- renderText({
    entreprise()$mail
  })
  
  observe({
    shinyjs::toggleState("numero", input$modifier_facture)
    shinyjs::toggleState("date", input$modifier_facture)
    
    shinyjs::disable("numero_tps")
    shinyjs::disable("numero_tvq")
  })
  
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
    
    if (is.null(montant) || is.na(montant)) {
      montant <- 0
    }
    
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
  
  output$download_html <- downloadHandler(
  
  filename = function() {
    paste0("facture_", input$numero, ".html")
  },
  
  contentType = "text/html",
  
  content = function(file) {
    
    if (!grepl("^\\(\\d{3}\\) \\d{3}-\\d{4}$", input$telephone_client)) {
      showNotification(
        "Numéro de téléphone invalide. Format attendu : (123) 123-1234",
        type = "error"
      )
      return(NULL)
    }
    
    montant <- input$montant
    if (is.null(montant) || is.na(montant)) montant <- 0
    
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
    
    ent <- entreprise()
    date_facture <- format(input$date, "%d-%m-%Y")
    
    # Sécurisation HTML
    e_nom <- htmltools::htmlEscape(ent$nom)
    e_adresse <- htmltools::htmlEscape(ent$adresse)
    e_tel <- htmltools::htmlEscape(ent$tel)
    e_mail <- htmltools::htmlEscape(ent$mail)
    
    client <- htmltools::htmlEscape(input$client)
    adresse_client <- htmltools::htmlEscape(input$adresse_client)
    telephone_client <- htmltools::htmlEscape(input$telephone_client)
    
    description <- htmltools::htmlEscape(input$description)
    description <- gsub("\n", "<br>", description)
    
    numero <- htmltools::htmlEscape(input$numero)
    numero_tps <- htmltools::htmlEscape(input$numero_tps)
    numero_tvq <- htmltools::htmlEscape(input$numero_tvq)
    
    if (input$inclure_taxes) {
      
        section_bas <- glue::glue('
<div class="bottom">

  <table>
    <tr><td colspan="4"><b># taxe</b></td></tr>
    <tr>
      <td>TPS</td>
      <td>{numero_tps}</td>
      <td>5%</td>
      <td class="montant">{format_argent(tps)}</td>
    </tr>
    <tr>
      <td>TVQ</td>
      <td>{numero_tvq}</td>
      <td>9,975%</td>
      <td class="montant">{format_argent(tvq)}</td>
    </tr>
  </table>

  <table>
    <tr>
      <td>Sous-total</td>
      <td class="montant">{format_argent(montant)}</td>
    </tr>
    <tr>
      <td>Taxes</td>
      <td class="montant">{format_argent(taxes)}</td>
    </tr>
    <tr>
      <td><b>Total</b></td>
      <td class="montant"><b>{format_argent(total)}</b></td>
    </tr>
  </table>

</div>
')
        
      } else {
        
        section_bas <- glue::glue('
<div class="bottom-simple">
  <table>
    <tr>
      <td><b>Montant total</b></td>
      <td class="montant"><b>{format_argent(total)}</b></td>
    </tr>
  </table>
</div>
')
      }
      
      html <- glue::glue('
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">

<style>
  @page {{
    size: Letter;
    margin: 1.2cm;
  }}

  body {{
    font-family: Arial, sans-serif;
    font-size: 15px;
    margin: 20px;
    color: #222;
  }}

  .top {{
    display: grid;
    grid-template-columns: 1fr 240px;
    gap: 80px;
    margin-bottom: 40px;
  }}

  .entreprise {{
    border: 1px solid #999;
    border-top: 15px solid #b3b3b3;
    padding: 10px;
    min-height: 110px;
    line-height: 1.8;
  }}

  .titre {{
    text-align: center;
    font-size: 18px;
    margin-bottom: 0px;
    border-bottom: 15px solid #b3b3b3;
  }}

  table {{
    border-collapse: collapse;
    width: 100%;
  }}

  td, th {{
    border: 1px solid #999;
    padding: 5px;
  }}

  .facture-info td {{
    text-align: center;
  }}

  .client {{
    border: 1px solid #999;
    border-top: 15px solid #b3b3b3;
    padding: 10px;
    min-height: 110px;
    line-height: 1.8;
    margin-top: 20px;
    margin-bottom: 15px;
  }}

  .items {{
    margin-top: 0px;
  }}

  .items th {{
    border-top: 15px solid #b3b3b3;
    border-bottom: none;
    text-align: left;
    font-weight: normal;
    padding: 10px;
  }}

  .items td {{
    border: 1px solid #999;
    border-top: none;
    height: 90px;
    vertical-align: top;
    white-space: normal;
    overflow-wrap: break-word;
    padding: 5px 10px 10px 10px;
  }}

  .montant {{
    text-align: right;
    white-space: nowrap;
  }}

  .bottom {{
    display: grid;
    grid-template-columns: 1.5fr 1fr;
    margin-top: 20px;
  }}

  .bottom-simple {{
    width: 40%;
    margin-left: auto;
    margin-top: 20px;
  }}
  
</style>
</head>

<body>

<div class="top">

  <div class="entreprise">
    <b>{e_nom}</b><br>
    {e_adresse}<br>
    {e_tel}<br>
    <span style="color:#0070C0; text-decoration: underline;">{e_mail}</span>
  </div>

  <div>
    <div class="titre">FACTURE</div>
    <table class="facture-info">
      <tr>
        <td><b>#</b></td>
        <td><b>date</b></td>
      </tr>
      <tr>
        <td>{numero}</td>
        <td>{date_facture}</td>
      </tr>
    </table>
  </div>

</div>

<div class="client">
  <b>Vendu à</b><br>
  {client}<br>
  {adresse_client}<br>
  {telephone_client}
</div>

<table class="items">
  <tr>
    <th><b>Description</b></th>
  </tr>
  <tr>
    <td>{description}</td>
  </tr>
</table>

{section_bas}

</body>
</html>
')
    
      writeLines(html, file, useBytes = TRUE)
      
      # Incrément facture
      numero_actuel <- as.numeric(input$numero)
      
      if (!is.na(numero_actuel)) {
        prochain_numero <- numero_actuel + 1
        writeLines(as.character(prochain_numero), fichier_numero)
        
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
