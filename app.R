install.packages('highcharter')
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinymanager)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(stats)
library(RMySQL)
library(openxlsx)
library(highcharter)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(shinyBS)
library(shinyanimate)
library(waiter)
library(googlesheets4)
library(magick)
library(DT)
library(blastula)
library(htmlwidgets)
library(fontawesome)



modal_img <- function(src,scrurl, size = "100%") {
    a(
        href = scrurl,
        target = "_blank",
        img(
            src = src,
            id = "zoom_image",
            width = size,
            height = size,
            border = "0",
            alt = "This is alternate text"
        )
    )
}
# designate project-specific cache
Certificado <- function(Nombre){
    Image <- image_read("https://github.com/JavierRojasC/JavierRCam/blob/master/Certificado%20ARTES.png?raw=true")
    A=image_annotate(Image,Nombre,size = 120, location = "-0+320", 
                     gravity = "center", font = "Trebuchet", color = "#008cfa")
    
    
    image_write(A, paste0('Mención_de_Honor_ARTES_',Nombre,'.pdf'),format = 'pdf')
}
#create_smtp_creds_key(
#  id = "yahooAlt",
#  user = "cepwol@yahoo.com",
#  host = "smtp.mail.yahoo.com",
#  port = 465,
#  use_ssl = TRUE
#)

connection = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')

saveData <- function(data) {
  data <- as.data.frame(t(data))
  data$Fecha=lubridate::as_date(as.numeric(data$Fecha))
  #data$idEgresos=NULL
  
  responses <<- data
  
}
loadData <- function() {
  if (exists("responses")) {

    responses
  }
}

dt_output = function(title, id) {
  fluidRow(column(
    12, h1("Tabla de Egresos"),
    hr(), DTOutput(id)
  ))
}
fields <- c("idEgresos","Comentario", "Categoria","Monto","Factura","Fecha")

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}
ui = dashboardPage(
    #preloader = list(html = tagList(spin_three_bounce(), h3("Espere un momento ...")), color = "#1E3A4E"),
    
    title =  'Unidad Educativa Altamira' ,
    
    dashboardHeader(title = "Sistema de Secretaría 2022-2023",titleWidth = 450, tags$li(actionButton("go", "Home", class = "btn btn-sm btn-outline-secondary",
                                                                                                     onclick="location.href='https://uealtamira.shinyapps.io/altamiraadmin/';"
    ),
                                                                                        class = "dropdown")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Todos los cursos", tabName = "Todos", startExpanded = TRUE,icon = icon("globe")),
            menuItem("Cursos", tabName = "curso", startExpanded = TRUE,icon = icon("clipboard-list")),
            menuItem("Factura", tabName = "factura", startExpanded = TRUE,icon = icon("file-invoice-dollar")),
            menuItem("Pensiones", tabName = "pensiones", startExpanded = TRUE,icon = icon("file-invoice-dollar")),
            menuItem("Egresos", tabName = "egresos", startExpanded = TRUE,icon = icon("file-invoice-dollar")),
            
            menuItem("BD Pedidos temporales", tabName = "temp", startExpanded = TRUE,icon = icon("file-download")),
            menuItem("Cumpleaños", tabName = "cumple", startExpanded = TRUE,icon = icon("birthday-cake")),
            menuItem("Artes", tabName = "artes", startExpanded = TRUE,icon = icon("palette")),
            menuItem("ArchivosCSV", tabName = "csv", startExpanded = TRUE,icon = icon("print")),
            menuItem("Años lectivos anteriores", tabName = "anteriores", startExpanded = TRUE,icon = icon('archive')),
            # icons    https://fontawesome.com/v4/icons/
            fluidRow(align='center',
                     br(),
            img(src="https://github.com/JavierRojasC/JavierRCam/blob/master/Logo%20Nuevo%20Transparente.png?raw=true", width='50%')
            
        )
            
        )),
    
    dashboardBody( tags$head(tags$style(HTML('
    .small-box {
    border-radius: 25px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgba(0,0,0,.1);
    background-color: #113673!important;
  -webkit-box-shadow: 2px 2px 5px #de881e;
  -moz-box-shadow: 2px 2px 5px #de881e;
    }
    
  .small-box:hover { 
  transform: scale(1.05);
  background-color: #104EA2!important;
  }
  
  .highcharts-axis-labels.highcharts-xaxis-labels{
  font-family: inherit;
  font size="5" ;
  background-color: #104EA2;

  }
         .content-wrapper {
                                  /*background-image: url("https://github.com/JavierRojasC/JavierRCam/blob/master/backg3.png?raw=true");*/
                                    background-color: #eff5fb;
                                 }


                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #DADADA;
                                color: #2B1F57
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #A1A1A1;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #FFC40D;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #ECF5FF;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A8A8A8;
                                
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #8B8989;
                                color: #151515;
                                style:"font-family:verdana";
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6F6F6F;
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DDDDDD;
                                }

                             /* body */
                        
                                
                                .box.box-solid.box-primary>.box-header{
  background: rgb(0, 129, 201);
  color: #57A184;
    font-size: 18px;
  font-weight; bold;
  border-radius: 25px 25px 25px 0; /
}



                                '))),
                   tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                   tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                   
                   tabItems(
                       tabItem(tabName= "Todos",
                               fluidRow(
                                   valueBoxOutput("CategoriaEst"),
                                   valueBoxOutput("CategoriaEstNew"),
                                   valueBoxOutput("CategoriaEstAlt"),
                               ),
                               fluidRow(
                                   withSpinner(highchartOutput('todoscursos',  height = "450px"), type = 7, color='#C7D5EB'))),
                       tabItem(tabName= "curso",
                               fluidRow(
                                   selectInput('curso', "Ingrese el curso:",
                                               c("Educación Inicial 1",
                                                 "Educación Inicial 2",
                                                 "1º Grado EGB",
                                                 "2º Grado EGB",
                                                 "3º Grado EGB",
                                                 "4º Grado EGB",
                                                 "5º Grado EGB",
                                                 "6º Grado EGB",
                                                 "7º Grado EGB",
                                                 "8º Grado EGB",
                                                 "9º Grado EGB",
                                                 "10º Grado EGB",
                                                 "I Bachillerato",
                                                 "II Bachillerato",
                                                 "III Bachillerato"
                                               )),
                                   valueBoxOutput("CategoriaEstfilt"),
                                   valueBoxOutput("CategoriaEstNewfilt"),
                                   valueBoxOutput("CategoriaEstAltfilt"),
                                   DT::dataTableOutput("cursos"),
                                   box(width = 12,
                                       column(6,
                                              uiOutput('Estudiante')),
                                       
                                       column(6,
                                              selectInput('info','Seleccione:',
                                                          c('Estudiante','Padre','Madre','Representante')))),
                                   textOutput('Nametable'),
                                   tags$head(tags$style("#Nametable{color: #3E6590;
                                 font-size: 30px;
                                 font-style: bold;
                                 }"
                                   )
                                   ),
                                   DT::dataTableOutput("PMR")
                                   
                                   
                               )
                               
                       ),
                       tabItem(tabName= "factura",
                               fluidRow(
                                   dateRangeInput('dateRange',
                                                  label = 'Ingrese Fechas : yyyy-mm-dd',
                                                  start = '2021-04-06', end = Sys.Date()
                                   ),
                                   downloadLink('download_excel_Fact', 'Descarga de Datos de Facturación'),
                                   
                                   DT::dataTableOutput("factura")
                               )
                               
                       ),
                       tabItem(tabName= "pensiones",
                               fluidRow(
                                   selectInput('cursoPensiones', "Ingrese el curso:",
                                               c("Inicial 1",
                                                 "Inicial 2",
                                                 "1º Grado EGB",
                                                 "2º Grado EGB",
                                                 "3º Grado EGB",
                                                 "4º Grado EGB",
                                                 "5º Grado EGB",
                                                 "6º Grado EGB",
                                                 "7º Grado EGB",
                                                 "8º Grado EGB",
                                                 "9º Grado EGB",
                                                 "10º Grado EGB",
                                                 "I Bachillerato",
                                                 "II Bachillerato",
                                                 "III Bachillerato"
                                               )),
                                   withSpinner(highchartOutput('pensionesnumber',  height = "850px"), type = 7, color='#C7D5EB')                               )
                               
                       ),
                       tabItem(tabName= "egresos",
                            
                               
                               tags$hr(),
                               
                               column(width = 4,
                                      #textInput("idEgresos","No llenar este campo"),
                                      shinyjs::useShinyjs(),
                                      div(
                                        id = "form",
                                        selectInput("Categoria","Categoría", c("  ","SUELDOS","IESS PLANILLAS PATRONAL","FONDOS DE RESERVA","DÉCIMO TERCERO",
                                                                               "DÉCIMO CUARTO","FINIQUITOS","HONORARIOS PROFESIONALES WRB","HONORARIOS PROFESIONALES WRP",
                                                                               "HONORARIOS PROFESIONALES WRC", "HONORARIOS PROFESIONALES VARIOS","CAPACITACIÓN PERSONAL",
                                                                               "SRI IMPUESTOS","PLANILLAS LUZ COLEGIO","INTERNET","TELÉFONO FIJO","TELÉFONO CELULAR", 
                                                                               "MANTENIMIENTO LOCAL ASEO","MANTENIMIENTO LOCAL FERRETERÍAS","MANTENIMIENTO LOCAL MOBILIARIO",
                                                                               "MANTENIMINETO AREAS VERDES","MANTENIMINETO ELÉCTRICO","MANTENIMIENTO Y ADQUISICIÓN DE MATERIALES LABORATORIO",
                                                                               "MANTENIMIENTO Y ADQUISICIÓN DE EQUIPOS DE COMPUTACIÓN","MANTENIMIENTO LOCAL MANO DE OBRA","MANTENIMIENTO LOCAL PISCINA",
                                                                               "MANTENIMIENTO AIRES ACONDICIONADOS","CÁMARAS DE SEGURIDAD","SUMINISTROS DE COMPUTACIÓN",
                                                                               "SUMINISTROS DE OFICINA","SISTEMA CONTABLE FACTURACIÓN ELECTRÓNICA","PAGINA COLEGIO - PREDIOS ALTAMIRA SIST VISA",
                                                                               "IMPLEMENTOS DEPORTIVOS","IMPRENTA GRAFICARTE","MOVILIZACIÓN","PUBLICIDAD","MATRÍCULA MOTO","IMPREVISTOS",
                                                                               "EVENTOS","VACUNACIÓN","CAMBRIDGE")),
                                      textAreaInput("Comentario", "Comentario de egreso", ""),
                                      tags$hr(),
                                      
                               checkboxInput("Factura", "¿Se tiene factura?", FALSE),
                               tags$hr(),
                               dateInput("Fecha", "Fecha de egreso", Sys.time()),

                               numericInput("Monto", "Monto de egreso",
                                            0)),
                               textInput('idEgresos'," ", value = Sys.time(), width = 1),
                               actionButton("submit", "Ingresar Egreso")  ),
                               column(width = 8,
                                      shinyjs::useShinyjs(),
                                      div(
                                        id = "tableegresos",
                                      dt_output('server-side processing (editable = "row")', 'x5')
                              ) )
                       ),
                       tabItem(tabName= "cumple",
                               fluidRow(
                                   dataTableOutput("tablacumple"),
                                   textInput("nombre","Ingrese el nombre del estudiante:"),
                                   selectInput("colorselect","Seleccione sexo:",
                                               c("Hombre"="#1D3987",
                                                 "Mujer"="#BB37CD")),
                                   imageOutput("image", height = "400px")
                                   #downloadButton("descargacum","Descargar")
                               )
                               
                       ),
                       tabItem(tabName= "artes",
                               tags$div(class = "button",
                                        tags$a(href = "https://github.com/JavierRojasC/JavierRCam/raw/master/PruebaNombres.xlsx", 
                                               "Descargar plantilla", 
                                               target="_blank")
                               ),
                               fluidRow(
                                   align="center",
                                   dataTableOutput("estudiantes"),
                                   fileInput('excel', 'Cargar Excel con nombres de los estudiantes',
                                             accept = c(".xlsx")
                                   ),
                                   DT::dataTableOutput('nombres'),
                                   
                                   downloadButton("data_file", "Descargar zip con todos las menciones"),
                                   actionButton("submit2", label = "Enviar"),
                                   h3("Vista Previa"),
                                   imageOutput("imageartes", height = "400px")
                                   #downloadButton("descargacum","Descargar")
                               )
                               
                       ),
                       tabItem(tabName= "csv",
                               box(width=12,fluidRow(
                                   column(12,fileInput("file1", "Upload base in csv",
                                                       accept = c(
                                                           "text/csv",
                                                           "comma-separated-values,text/plain",
                                                           ".csv")
                                   ),
                                   checkboxInput("header", "Press if the first row contains the column names", TRUE),
                                   radioButtons(inputId="separador",label="Separador",
                                                choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                selected = ','))
                               )),
                               fluidRow(width=12,
                                        box(title="Viewer",
                                            width=12,
                                            DT::dataTableOutput("DTable")))
                               
                               
                               
                               
                       ),
                       tabItem(tabName = 'temp',
                               selectInput('curso2', "Ingrese el curso:",
                                           c("Educación Inicial 1",
                                             "Educación Inicial 2",
                                             "1º Grado EGB",
                                             "2º Grado EGB",
                                             "3º Grado EGB",
                                             "4º Grado EGB",
                                             "5º Grado EGB",
                                             "6º Grado EGB",
                                             "7º Grado EGB",
                                             "8º Grado EGB",
                                             "9º Grado EGB",
                                             "10º Grado EGB",
                                             "I Bachillerato",
                                             "II Bachillerato",
                                             "III Bachillerato"
                                           )),
                               selectInput('datos','Seleccionar datos:',
                                           c("Numero_matricula"              , "Nombre_est"   ,                 
                                             "cedula_estud"                   ,"Sexo_est"      ,                
                                             "Fecha_de_nacimiento"            ,"nacionalidad_est",              
                                             "est_vive_con"                   ,"Grado_Nombre_grado",            
                                             "nivel_ant_est"    ,           
                                             "IE_procedencia"                , "direccion_est" ,                
                                             "email_est"                     , "obs_medicas_Est" ,              
                                             "obs_varias_est"                , "correo_institucional",          
                                             "Password"                   ,                  
                                             "cedula_madre"                  , "cedula_padre" ,                 
                                             "cedula_representante"          , "MatriculadoPor",                
                                             "Fecha"                         , "lugar_born"     ,               
                                             "estado"                        , "nombre_representantes" ,        
                                             "cedula_representantes"         , "dir_representantes"     ,       
                                             "ciudad_representantes"         , "email_representantes"    ,      
                                             "telefono_representantes"       , "celular_representantes"   ,     
                                             "telefonotrabajo_representantes", "titulo_representantes"     ,    
                                             "nivelacademico_representantes" , "ocupacion_representantes"   ,   
                                             "relacion_est"                ,  
                                             "cont"                          , "MatriculadoPor.1"  ), 
                                           multiple = TRUE),
                               DT::dataTableOutput('cursos2')),
                       tabItem(tabName = "anteriores",
                               column(4,
                                      
                                      h3('Sistema de secretaría 2021-2022', style = "font-family: ‘Trebuchet MS’, Helvetica, sans-serif;
        font-weight: 500; line-height: 1.1; 
        color: #19004F;"),
                                      withAnim(),
                                      fluidRow(align='center',
                                      tags$div(id = 'dash2', modal_img('https://github.com/JavierRojasC/JavierRCam/blob/master/secre2021.png?raw=true',
                                                                       'https://javierrojascamp.shinyapps.io/Secretaria/')))))
                   )))

server = function(input, output,session) {
    autoInvalidate <- reactiveTimer(200000)

    Base <- reactive({
        autoInvalidate()
        
        
        data = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')
        dbSendQuery(data, "SET CHARACTER SET utf8mb4;")
        
        query = dbGetQuery(data, statement="SELECT * FROM ALTAMIRA_2022.Estudiantes LEFT JOIN  ALTAMIRA_2022.Financiero ON
Numero_matricula = matricula")
        BaseAnotaciones = data.frame(query)
        on.exit(dbDisconnect(data))
        
        # return the dataframe
        
        BaseAnotaciones
        
        
    })
    
    
    
    BasePadre <- reactive({
        autoInvalidate()
        
        
        data = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')
        dbSendQuery(data, "SET CHARACTER SET utf8mb4;")
        
        query = dbGetQuery(data, statement="SELECT * FROM ALTAMIRA_2022.Estudiantes LEFT JOIN  ALTAMIRA_2022.Padres ON
cedula_padres = cedula_padre")
        BaseAnotaciones = data.frame(query)
        on.exit(dbDisconnect(data))
        
        # return the dataframe
        
        BaseAnotaciones
        
        
    })
    
    BaseMadre <- reactive({
        autoInvalidate()
        
        
        data = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')
        dbSendQuery(data, "SET CHARACTER SET utf8mb4;")
        
        query = dbGetQuery(data, statement="SELECT * FROM ALTAMIRA_2022.Estudiantes LEFT JOIN  ALTAMIRA_2022.Madres ON
cedula_madres = cedula_madre")
        BaseAnotaciones = data.frame(query)
        on.exit(dbDisconnect(data))
        
        # return the dataframe
        
        BaseAnotaciones
        
        
    })
    
    BaseRepresentante <- reactive({
        autoInvalidate()
        
        
        data = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')
        dbSendQuery(data, "SET CHARACTER SET utf8mb4;")
        
        query = dbGetQuery(data, statement="SELECT * FROM ALTAMIRA_2022.Estudiantes LEFT JOIN  ALTAMIRA_2022.Representantes ON
cedula_representantes = cedula_representante")
        BaseAnotaciones = data.frame(query)
        on.exit(dbDisconnect(data))
        
        # return the dataframe
        
        BaseAnotaciones
        
        
    })
    
    output$Estudiante <- renderUI({
        
        x <- Base()
        curso <- input$curso
        #curso <- '6º Grado EGB'
        #x <- BaseAnotaciones
        
        NOM <- x%>%
            filter(Grado_Nombre_grado==curso)
        
        selectInput("nomest", "Estudiante:", choices=sort(unique(as.character(NOM$Nombre_est))))
        
        
        
    })
    
    output$todoscursos <- renderHighchart({
        Data <- Base()
        # Data <- BaseAnotaciones
        Tab <- data.frame(table(Data$Grado_Nombre_grado))
        rownames(Tab) <- Tab$Var1
        DataOrd <- Tab[c('Educación Inicial 1','Educación Inicial 2','1º Grado EGB',
                         '2º Grado EGB',
                         '3º Grado EGB',
                         '4º Grado EGB',
                         '5º Grado EGB',
                         '6º Grado EGB',
                         '7º Grado EGB',
                         '8º Grado EGB',
                         '9º Grado EGB',
                         '10º Grado EGB',
                         'I Bachillerato','II Bachillerato',
                         'III Bachillerato'),]
        DataOrd$Freq <- as.numeric(DataOrd$Freq)
        DataOrd$Var1 <- as.character(DataOrd$Var1)
        str(DataOrd)
        highchart()%>%
            hc_add_series(DataOrd,type='column',hcaes(x='Var1', y='Freq'), color='#1B2667', name='Cursos', dataLabels=list(enabled=TRUE))%>%
            hc_xAxis(categories=rownames(DataOrd))%>%
            hc_title(text='Número de estudiantes por curso')
        
    })
    
    output$CategoriaEst <- renderValueBox({
        Data <- Base()
        
        valueBox(
            paste0(nrow(Data)), "Número de estudiantes", icon = icon("user-check"),
            color = "blue"
        )
    })
    output$CategoriaEstNew <- renderValueBox({
        Data <- Base()
        DataAlt <- Data%>%
            filter(IE_procedencia=='ALTAMIRA')
        
        valueBox(
            paste0(nrow(Data)-nrow(DataAlt)), "Número de estudiantes nuevos", icon = icon("user-plus"),
            color = "blue"
        )
    })
    
    output$CategoriaEstAlt <- renderValueBox({
        Data <- Base()
        DataAlt <- Data%>%
            filter(IE_procedencia=='ALTAMIRA')
        
        valueBox(
            paste0(nrow(DataAlt)), "Número de estudiantes no nuevos", icon = icon("user-tag"),
            color = "blue"
        )
    })
    output$cursos <- DT::renderDataTable({
        Data <- Base()
        #  Data <- BaseAnotaciones
        Curs <- input$curso
        # Curs <- '9º Grado EGB'
        Datacurso <- Data%>%
            filter(Grado_Nombre_grado==Curs)
        names(Data)
        DatacursoDB <- Datacurso[,c('Nombre_est','correo_institucional')]
        DatacursoDB <- as.data.frame(DatacursoDB)
        colnames(DatacursoDB) <- c('Estudiante','Correo Institucional')
        
        DT::datatable(DatacursoDB,  # Agregar filtro en la parte superior de las columnas
                      filter = 'top', 
                      # Añadir los botones de descarga de tabla
                      rownames=FALSE,
                      
                      extensions = 'Buttons', 
                      options = list( 
                          autoWidth = TRUE, 
                          # Ubicación de los botones de descarga de tabla, y formatos disponibles
                          dom = 'Blfrtip', 
                          buttons = list(list(extend='pdf', filename = paste0('Nómina de  ',Curs,'.pdf'), title = paste0('Nómina de  ',Curs)),
                                         list(extend='excel', filename = paste0('Nómina de  ',Curs), title = paste0('Nómina de  ',Curs))),
                          pageLength = 50, 
                          # Ordenar la columna de Título de libro, de manera ascendente
                          order = list(0, 'asc') 
                      ), 
                      escape = FALSE 
        )
        
    })
    
    output$cursos2 <- DT::renderDataTable({
        Data <- BaseRepresentante()
        #  Data <- BaseAnotaciones
        
        Curs <- input$curso2
        #Curs <- '9º Grado EGB'
        Datacurso <- Data%>%
            filter(Grado_Nombre_grado==Curs)
        names(Datacurso)
        DatacursoDB <- Datacurso[,input$datos]
        DatacursoDB <- as.data.frame(DatacursoDB)
        
        DT::datatable(DatacursoDB,  # Agregar filtro en la parte superior de las columnas
                      filter = 'top', 
                      # Añadir los botones de descarga de tabla
                      rownames=FALSE,
                      
                      extensions = 'Buttons', 
                      options = list( 
                          autoWidth = TRUE, 
                          # Ubicación de los botones de descarga de tabla, y formatos disponibles
                          dom = 'Blfrtip', 
                          buttons = list(list(extend='pdf', filename = paste0('Nómina de contacto de ',Curs,'.pdf'), title = paste0('Nómina de contacto de ',Curs)),
                                         list(extend='excel', filename = paste0('Nómina de contacto de ',Curs), title = paste0('Nómina de contacto de ',Curs))),
                          pageLength = 50, 
                          # Ordenar la columna de Título de libro, de manera ascendente
                          order = list(0, 'asc') 
                      ), 
                      escape = FALSE 
        )
        
    })
    
    output$CategoriaEstNewfilt <- renderValueBox({
        Data <- Base()
        Curs <- input$curso
        
        DataC <- Data%>%
            filter(Grado_Nombre_grado==Curs)
        
        DataAlt <- DataC%>%
            filter(IE_procedencia=='ALTAMIRA')
        
        valueBox(
            paste0(nrow(DataC)-nrow(DataAlt)), "Número de estudiantes nuevos", icon = icon("user-plus"),
            color = "blue"
        )
    })
    
    output$CategoriaEstAltfilt <- renderValueBox({
        Data <- Base()
        Curs <- input$curso
        
        DataAlt <- Data%>%
            filter(Grado_Nombre_grado==Curs)%>%
            filter(IE_procedencia=='ALTAMIRA')
        
        
        valueBox(
            paste0(nrow(DataAlt)), "Número de estudiantes no nuevos", icon = icon("user-tag"),
            color = "blue"
        )
    })
    
    output$CategoriaEstfilt <- renderValueBox({
        Data1 <- Base()
        Curs <- input$curso
        
        Data <- Data1%>%
            filter(Grado_Nombre_grado==Curs)
        
        valueBox(
            paste0(nrow(Data)), "Número de estudiantes", icon = icon("user-check"),
            color = "blue"
        )
    })
    
    
    facturaBD <- reactive({
        Data1 <- Base()
        Tabla <- Data1[,c('Nombre_est',"Grado_Nombre_grado",'matricula','Fecha','idFactura','nombreFactura',
                          'rucFactura','direccionFactura',
                          'Correo')]
        
        Tabla$Fecha <- as.Date(Tabla$Fecha)
        Fecha1 <- as.Date(input$dateRange[1])
        Fecha2 <- as.Date(input$dateRange[2])
        #Fecha1 <- as.Date('2021-04-06')
        #Fecha2 <- as.Date('2021-04-07')   
        
        
        Tab1 <- Tabla%>%
            filter(Fecha >= Fecha1)%>%
            filter(Fecha <= Fecha2)
        
        colnames(Tab1) <- c('Nombre del estudiante',"Curso",'Número de matrícula','Fecha de matrícula','Id Factura','Nombre Factura',
                            'Cédula o RUC','Dirección Factura','Correo Factura')
        Tab1
    })
    
    output$factura <- DT::renderDataTable({
        Data1 <- Base()
        Tabla <- Data1[,c('Nombre_est',"Grado_Nombre_grado",'matricula','Fecha','idFactura','nombreFactura',
                          'rucFactura','direccionFactura',
                          'Correo')]
        
        Tabla$Fecha <- as.Date(Tabla$Fecha)
        Fecha1 <- as.Date(input$dateRange[1])
        Fecha2 <- as.Date(input$dateRange[2])
        #Fecha1 <- as.Date('2021-04-06')
        #Fecha2 <- as.Date('2021-04-07')   
        
        
        Tab1 <- Tabla%>%
            filter(Fecha >= Fecha1)%>%
            filter(Fecha <= Fecha2)
        
        colnames(Tab1) <- c('Nombre del estudiante',"Curso",'Número de matrícula','Fecha de matrícula','Id Factura','Nombre Factura',
                            'Cédula o RUC','Dirección Factura','Correo Factura')
        DT::datatable(Tab1,  options = list(
            pageLength = 30))
    })
    
    
    
    
    output$download_excel_Fact <- downloadHandler(
        
        
        filename = function() {
            paste0("Datos Factura.xlsx")
        },
        content = function(file){
            my_workbook <- createWorkbook(file)
            
            addWorksheet(wb = my_workbook,sheetName = "Datos Facturación")
            
            
            writeData(
                my_workbook,
                sheet = "Datos Facturación",
                facturaBD(),
                startRow = 1,
                startCol = 1
            )
            
            
            
            saveWorkbook(my_workbook, file)
            
        })
    output$Nametable <- renderText({
        Des <- input$info
        
        if (Des=='Padre'){
            text='Datos del Padre'
        } else if (Des=='Madre'){
            text='Datos de la Madre'
            
        } else if (Des=='Representante'){
            text='Datos del Representante'
            
        } else if (Des=='Estudiante'){
            text='Datos del Estudiante'
        }
        text
    })
    output$PMR <- DT::renderDataTable({
        
        Des <- input$info
        
        if (Des=='Padre'){
            Data=BasePadre()
            Datacurso <- Data%>%
                filter(Nombre_est==input$nomest)
            
            TabDat <- data.frame(Nombre=Datacurso$nombre_padres,Cédula=Datacurso$cedula_padre,Dirección=Datacurso$dir_padres,
                                 Ciudad=Datacurso$ciudad_padres,Teléfono=Datacurso$telefono_padres,Email=Datacurso$email_padres,
                                 Celular=Datacurso$celular_padres,Telf_Trabajo=Datacurso$telefonotrabajo_padres,
                                 Título=Datacurso$titulo_padres,Niv_Académico=Datacurso$nivelacademico_padres,Ocupación=Datacurso$ocupacion_padres)
            
            TabDat2 <- data.frame(t(TabDat))
            colnames(TabDat2)=c("")
        } else if (input$info=='Madre'){
            Data=BaseMadre()
            Datacurso <- Data%>%
                filter(Nombre_est==as.character(input$nomest))
            
            TabDat <- data.frame(Nombre=Datacurso$nombre_madres,Cédula=Datacurso$cedula_madre,Dirección=Datacurso$dir_madres,
                                 Ciudad=Datacurso$ciudad_madres,Teléfono=Datacurso$telefono_madres,Email=Datacurso$email_madres,
                                 Celular=Datacurso$celular_madres,Telf_Trabajo=Datacurso$telefonotrabajo_madres,
                                 Título=Datacurso$titulo_madres,Niv_Académico=Datacurso$nivelacademico_madres,Ocupación=Datacurso$ocupacion_madres)
            
            TabDat2 <- data.frame(t(TabDat))
            colnames(TabDat2)=c("")
        } else if (input$info=='Representante'){
            Data <- BaseRepresentante()
            
            Datacurso <- Data%>%
                filter(Nombre_est==as.character(input$nomest))
            
            TabDat <- data.frame(Nombre=Datacurso$nombre_representantes, Relación=Datacurso$relacion_est,Cédula=Datacurso$cedula_representante,Dirección=Datacurso$dir_representantes,
                                 Ciudad=Datacurso$ciudad_representantes,Teléfono=Datacurso$telefono_representantes,Email=Datacurso$email_representantes,
                                 Celular=Datacurso$celular_representantes,Telf_Trabajo=Datacurso$telefonotrabajo_representantes,
                                 Título=Datacurso$titulo_representantes,Niv_Académico=Datacurso$nivelacademico_representantes,Ocupación=Datacurso$ocupacion_representantes)
            
            TabDat2 <- data.frame(t(TabDat))
            colnames(TabDat2)=c("")
            
        } else if (input$info=='Estudiante'){
            Data=BaseMadre()
            Datacurso <- Data%>%
                filter(Nombre_est==as.character(input$nomest))
            
            TabDat <- data.frame(Nombre=Datacurso$Nombre_est,Cédula=Datacurso$cedula_estud,Dirección=Datacurso$direccion_est,
                                 Vive_con=Datacurso$est_vive_con,Grado=Datacurso$Grado_Nombre_grado,Email=Datacurso$correo_institucional,
                                 Fecha_nacimiento=Datacurso$Fecha_de_nacimiento,Nacionalidad=Datacurso$nacionalidad_est,
                                 IE_procedencia=Datacurso$IE_procedencia,Obs_Médicas=Datacurso$obs_medicas_Est,Obs_Varias=Datacurso$obs_varias_est)
            
            TabDat2 <- data.frame(t(TabDat))
            colnames(TabDat2)=c("")
        }
        
        Estud <- input$nomest
        colnames(TabDat2)=c(" ")
        
        DT::datatable(TabDat2,  # Agregar filtro en la parte superior de las columnas
                      filter = 'top', 
                      # Añadir los botones de descarga de tabla
                      rownames=TRUE,
                      
                      extensions = 'Buttons', 
                      options = list( 
                          autoWidth = TRUE, 
                          # Ubicación de los botones de descarga de tabla, y formatos disponibles
                          dom = 'Blfrtip', 
                          buttons = list(list(extend='pdf', filename = paste0('Datos de ',Des, ' de ',Estud), title = paste0('Datos de ',Des, ' de ',Estud)),
                                         list(extend='excel', filename = paste0('Datos de ',Des, ' de ',Estud), title = paste0('Datos de ',Des, ' de ',Estud)))
                          # Mostrar las primeras 20 entradas, por default despliega 10 pageLength = 5, 
                          # Ordenar la columna de Título de libro, de manera ascendente
                          #order = list(0, 'asc') 
                      ), 
                      escape = FALSE)
    })
    
    output$report <- downloadHandler(
        # Para la salida en PDF, usa "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copia el reporte a un directorio temporal antes de porcesarlo, en 
            #caso de que no tengamos permiso de escritura en el directorio actual
            #puede ocurrir un error
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # configurar los parametros para pasar al documento .Rmd
            params <- list(n = input$slider)
            
            #Copilar el documento con la lista de parametros, de tal manera que se 
            #evalue de la misma manera que el entorno de la palicacipon.
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )})
    
    output$pensionesnumber <- renderHighchart({
        options(gargle_oauth_cache = ".secrets")
        # check the value of the option, if you like
        gargle::gargle_oauth_cache()
        googlesheets4::sheets_auth()
        list.files(".secrets/")
        sheets_deauth()
        
        sheets_auth(
            cache = ".secrets",
            email = "secretaria@altamira.edu.ec"
        )
        
        options(
            gargle_oauth_cache = ".secrets",
            gargle_oauth_email = "secretaria@altamira.edu.ec"
        )
        # run sheets auth
        sheets_auth()
        
        Curs <- input$cursoPensiones
        #  Curs <- 'Inicial 1'
        #  
        #  if (Curs=="Educación Inicial 1"){
        #    
        #  }else if (Curs=="Educación Inicial 2"){
        #    Sheet = 'Inicial 1'
        #  }else if (Curs=="1º Grado EGB"){ 
        #    Sheet = 'Inicial 2'
        #    
        #  }else if (Curs=="2º Grado EGB"){ 
        #    
        #  }else if (Curs=="3º Grado EGB"){ 
        #    
        #  }else if (Curs=="4º Grado EGB"){ 
        #    
        #  }else if (Curs=="5º Grado EGB"){ 
        #    
        #  }else if (Curs=="6º Grado EGB"){ 
        #    
        #  }else if (Curs=="7º Grado EGB"){ 
        #    
        #  }else if (Curs=="8º Grado EGB"){ 
        #    
        #  }else if (Curs=="9º Grado EGB"){ 
        #    
        #  }else if (Curs=="10º Grado EGB"){ 
        #    
        #  }else if (Curs=="I Bachillerato"){ 
        #    
        #  }else if (Curs=="II Bachillerato"){ 
        #    
        #  }else if (Curs=="III Bachillerato"){ 
        #    
        #  }
        #    
        
        book <- read_sheet("https://docs.google.com/spreadsheets/d/1mMYk2SF7hq0Y9eHpHKgvg9IiOGGbF66JrQy_DOm9nbI/edit#gid=1", sheet = Curs)
        bok <- as.data.frame(book)
        Cuent <- c()
        for (i in 1:nrow(book)){
            Cuent[i] <- 10-sum(book[i,]=='0')
        }
        
        Matric <- c()
        for (i in 1:nrow(book)){
            
            if (bok$Matricula[i] == "No Pagado"){Matric[i]=0
            } else {Matric[i]=1}
        }
        
        
        
        
        
        DataGraf <- data.frame(Estudiante=book$Estudiante, Cuent)
        DataMatri <- data.frame(Estudiante=book$Estudiante, Matric)
        highchart()%>%
            hc_add_series(DataGraf, type='bar', hcaes(x='Estudiante',y='Cuent'), name='Pensiones', dataLabels=list(enabled=TRUE, pointFormat='<br> {point.y} de 10'), color='#1E2A6A')%>%
            hc_add_series(DataMatri, type='bar', hcaes(x='Estudiante',y='Matric'), name='Matrícula', dataLabels=list(enabled=TRUE), color='#F9B306')%>%
            
            hc_xAxis(categories=DataGraf$Estudiante)%>%
            hc_yAxis(max=10)%>%
            hc_title(text='Pago de pensiones')
        
    })
    
    cumplea <- reactive({
        tiger <- image_read('https://github.com/JavierRojasC/JavierRCam/blob/master/cumple2.png?raw=true')
        image_annotate(tiger,input$nombre,size = 90, location = "-90-300", 
                       gravity = "center", font = "Trebuchet", color = "#1A4E8A")
    })
    
    output$image <- renderImage({
        tiger <- image_read('https://github.com/JavierRojasC/JavierRCam/blob/master/cumple2.png?raw=true')
        temp <- image_annotate(tiger,input$nombre,size = 120, location = "+0-300", 
                               gravity = "center", font = "Trebuchet", color = input$colorselect)%>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        list(src = temp, contentType = "image/jpg", width = 600)
        
    }, deleteFile = FALSE)
    
    imagecum <- reactive({
        tiger <- image_read('https://github.com/JavierRojasC/JavierRCam/blob/master/cumple2.png?raw=true')
        temp <- image_annotate(tiger,input$nombre,size = 400, location = "-10-300", 
                               gravity = "center", font = "Trebuchet", color = "#1A4E8A")%>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        list(src = temp, contentType = paste('Cumpleaños', input$nombre, '/jpg', sep=''), width = 600)
        
    })
    
    output$descargacum <- downloadHandler(
        filename = function() {
            paste('Cumpleaños', input$nombre, '.png', sep='')
        },
        content = function(con) {
            imagecum(con)
        }
    )
    
    output$tablacumple <- renderDataTable({
        data=Base()
        #data=BaseAnotaciones
        #names(data)
        Fechas=as.Date(data$Fecha_de_nacimiento, format="%Y-%m-%d")
        betterDates <- as.Date(Fechas,
                               format = "%m")
        
        TabFechas <- data.frame(Cumpleañero=data$Nombre_est,Fecha=format(Fechas, format="%m-%d"),Correo=data$correo_institucional)
        Hoy=format(Sys.time(),format="%m-%d")
        TabFinal=TabFechas%>%
            filter(Fecha==Hoy)
        TabFinal
    })
    
    datacsv <- reactive({
        
        dataset=FALSE
        if (dataset == FALSE){
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)
            
            datacsv=read.csv2(inFile$datapath, sep=input$separador,header = input$header)
            datacsv
        } else {
            datacsv = dataset}
        
        
        
        
        
        
    })
    
    output$DTable <- DT::renderDataTable({
        Data <- datacsv()
        
        DT::datatable(Data,
                      extensions = 'Buttons', 
                      options = list( 
                          autoWidth = TRUE, 
                          # Ubicación de los botones de descarga de tabla, y formatos disponibles
                          dom = 'Blfrtip', 
                          buttons = list(list(extend='pdf', filename = paste0('Archivo1','.pdf'), title = paste0('Archivo1','.pdf')),
                                         list(extend='excel', filename = paste0('Archivo1'), title = paste0('Archivo1'))),
                          pageLength = nrow(Data)
                          # Ordenar la columna de Título de libro, de manera ascendente
                      ), 
                      escape = FALSE)
    })
    dataartes <- reactive({
        inFile <- input$excel
        
        if (is.null(inFile))
            return(NULL)
        
        data=read_excel(inFile$datapath,col_names = TRUE)
        data
    })
    
    output$nombres <- DT::renderDataTable({
        datatable(dataartes())
    })
    
    output$data_file <- downloadHandler(
        filename = paste0("Mención_de_Honor_Artes.zip"),
        content = function(fname) {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            print (tempdir())
            
            for (i in 1:length(dataartes()$ASISTENTES)){
                path <- paste0('Mención_de_Honor_ARTES_',dataartes()$ASISTENTES[i],'.pdf')
                fs <- c(fs,path)
                Certificado(dataartes()$ASISTENTES[i])}
            print (fs)
            zip(fname, files=fs)
            
        },
        contentType = "application/zip"
    )
    
    output$imageartes <- renderImage({
        tiger <- image_read("https://github.com/JavierRojasC/JavierRCam/blob/master/Certificado%20ARTES.png?raw=true")
        temp <- image_annotate(tiger,dataartes()$ASISTENTES[1],size = 120, location = "-0+320", 
                               gravity = "center", font = "Trebuchet", color = "#008cfa")%>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        list(src = temp, contentType = "image/jpg", width = 600)
        
    }, deleteFile = FALSE)
    
    imagecum <- reactive({
        tiger <- image_read("https://github.com/JavierRojasC/JavierRCam/blob/master/Certificado%20ARTES.png?raw=true")
        temp <- image_annotate(tiger,dataartes()$ASISTENTES[1],size = 120, location = "-0+320", 
                               gravity = "center", font = "Trebuchet", color = "#008cfa")%>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        list(src = temp, contentType = paste('Artes', dataartes()$ASISTENTES[1], '/jpg', sep=''), width = 600)
        
    })
    
    
    output$data_file <- downloadHandler(
        filename = paste0("Mención_de_Honor_Artes.zip"),
        content = function(fname) {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            print (tempdir())
            
            for (i in 1:length(dataartes()$ASISTENTES)){
                path <- paste0('Mención_de_Honor_ARTES_',dataartes()$ASISTENTES[i],'.pdf')
                fs <- c(fs,path)
                Certificado(dataartes()$ASISTENTES[i])}
            print (fs)
            zip(fname, files=fs)
            
        },
        contentType = "application/zip"
    )
    
    observe({ if(is.null(input$submit2) || input$submit2==0) return(NULL)
        
        for (i in 1:length(dataartes()$ASISTENTES)){
            
            Cert <- Certificado(dataartes()$ASISTENTES[i])
            img_string <- add_image(Cert)
            email <-
                compose_email(
                    body = md(glue::glue("{img_string}")),
                    footer = md(glue::glue(""))
                )
            
            smtp_send(email,
                      from = "cepwol@yahoo.com",
                      to = dataartes()$mail[i],
                      subject = "FELICIDADES. @AltamiraEsArte",
                      credentials = creds_key(id = "yahooAlt")
            )}
    })
    
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    observeEvent(input$submit, {
      dbWriteTable(connection, value = responses, name = "Egresos", append = TRUE, overwrite = FALSE,row.names = FALSE ) 
      
    })
    observeEvent(input$submit, {
      shinyjs::reset("form")
      
})

    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })    
    
    
    BaseEgreso <- reactive({
      # autoInvalidate()
      
      
      data = dbConnect(MySQL(), user = 'root', host='node209482-env-7629881.j.layershift.co.uk', password = 'OAAflq49580', dbname= 'ALTAMIRA_2022')
      dbSendQuery(data, "SET CHARACTER SET utf8mb4;")
      
      query = dbGetQuery(data, statement="SELECT * FROM ALTAMIRA_2022.Egresos")
      BaseAnotaciones = data.frame(query)
      on.exit(dbDisconnect(data))
      
      # return the dataframe
      if (is.numeric(BaseAnotaciones$Fecha) ){
      BaseAnotaciones$Fecha <- lubridate::as_date(as.numeric(BaseAnotaciones$Fecha))} else {
        BaseAnotaciones$Fecha <- lubridate::as_date(BaseAnotaciones$Fecha)
      }
      BaseAnotaciones
      
    })
    
    output$x5 = render_dt(BaseEgreso(), 'row')
    
    
    observeEvent(input$x5_cell_edit, {
      
      BaseEgreso2 <<- editData(BaseEgreso(), input$x5_cell_edit, 'x5')
      dbWriteTable(connection, value = BaseEgreso2, name = "Egresos", append = FALSE, overwrite = TRUE,row.names = FALSE ) 
      rs <- dbSendQuery(
        connection,
        "ALTER TABLE `ALTAMIRA_2022`.`Egresos` CHANGE COLUMN `idEgresos` `idEgresos` VARCHAR(20) NOT NULL , ADD PRIMARY KEY (`idEgresos`);"
      )
      dbFetch(rs)
      
    })

    observeEvent(input$submit, {
      shinyjs::refresh()
    })
}
# Run the application 
shinyApp(ui = ui, server = server, port = 5024, host="0.0.0.0")



