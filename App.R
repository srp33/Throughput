library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)
library(tidycwl)
library(tidyverse)
library(readr)
library(shinyFiles) # This is for getting the file path of a file (maybe a later implementation of the app?)
library(ymlthis)

# Global Functions ---------------------------------------------------------------------

# This is a UI Module for text input
listOutputUI <- function(id, type){
  ns <- NS(id)
  
  if (type == "Int"){
    tagList(
      h3(paste0("Input: ", id, sep = "")),
      numericInput(ns("numericText"),"Numeric Input", 10, width = '100%'),
      hr()
    )
    # These are file and directory searches. They're an eventual addition to the app, but they are too slow (3/11/2020)
  #} else if (type == "File") {
  #  tagList(
  #    h3(paste0("Input: ", id, sep = "")),
  #    shinyFilesButton(ns("Btn_GetFile"), "Choose a file" , title = "Please select a file:", multiple = FALSE, 
  #                     buttonType = "color: #fff; background-color: #337ab7; border-color: #2e6da4", class = NULL), br(), br(),
  #    uiOutput(ns("txt_file")),
  #    hr()
  #  )
  #} else if (type == "Directory"){
  #  tagList(
  #    h3(paste0("Input: ", id, sep = "")),
  #    shinyDirButton(ns("Btn_GetDir"), "Choose a directory" , title = "Please select a directory:", 
  #                     buttonType = "color: #fff; background-color: #337ab7; border-color: #2e6da4", class = NULL), br(), br(),
  #    uiOutput(ns("txt_file")),
  #    hr()
  #  )
  } else {
    tagList(
      h3(paste0("Input: ", id, sep = "")),
      textInput(ns("inputText"),"Text Input", width = '100%', placeholder = paste0("Please respond to the input type with a ", type, sep ="")),
      hr()
    )
  }
  
}

# This function converts a Dataframe to a YML String
parseDFToYaml <- function(OutputDF){
  FinalString = ""
  count = 1
  for (x in 1:nrow(OutputDF)){
    FinalString = paste0(FinalString, OutputDF[count,1], ":", sep = "")
    if(((OutputDF[count,2]) == "Directory")[[1]] || ((OutputDF[count,2]) == "File")[[1]]){
      FinalString = paste0(FinalString, "\n\tclass: ", OutputDF[count,2], "\n\t", 
                           "path: ", OutputDF[count,3], "\n", sep = "")
    } else{
      FinalString = paste0(FinalString, " ", OutputDF[count,3], "\n", sep = "" )
    }
    count = count + 1
  }
  return(FinalString)
}

# This function generates the string of inputs (based on users selection) for CWL File generation
inputStringGenerator <- function(checkedInputList){
  stringBuilder = ""
  for(i in checkedInputList){
    if(toString(i) == "fasta_dir"){
      stringBuilder = paste0(stringBuilder, toString(i), ":\n\t\ttype: Directory\n\t")
    } else if(toString(i) == "fasta_file1" | toString(i) == "fasta_file2"){
      stringBuilder = paste0(stringBuilder, toString(i), ":\n\t\ttype: File\n\t")
    } else if(toString(i) == "threads"){
      stringBuilder = paste0(stringBuilder, toString(i), ":\n\t\ttype: int\n\t")
    } else{
      stringBuilder = paste0(stringBuilder, toString(i), ":\n\t\ttype: string\n\t")
    }
  }
  stringBuilder <- trimws(stringBuilder, which = "right")
  return(stringBuilder)
}

# User Interface ---------------------------------------------------------------------------
ui <- fluidPage (
  navbarPage(id = "tabs", theme = shinytheme("sandstone"), title = "Throughput",
             # CWL FILE UI
             tabPanel("CWL File Generation", 
                      sidebarLayout(
                        sidebarPanel(
                          HTML('<center><img src="Throughput.png" width=100%></center>'),
                          h4("CWL File Generation"),
                          p("Welcome to Throughput, an interface for helping generate CWL and YML files.")
                        ),
                        mainPanel(
                          h4("Build your CWL File"),
                          p("CWL File Header:"),
                          verbatimTextOutput("CWLStart"),
                          p("Requirements"),
                          textInput("dockerImageID", "Docker Image Id:", width = "100%"),
                          textInput("dockerFileFROM", "Docker File FROM file:", width = "100%"),
                          textInput("dockerFileRUN", "Docker File RUN Command:", width = "100%"),
                          verbatimTextOutput("Requirements"),
                          p("Arguements"),
                          textAreaInput("ExecutionScript", "Execution Script", width = "1000px", height = "250px"),
                          verbatimTextOutput("Arguments"),
                          p("Inputs"),
                          checkboxGroupInput("inputType", "Input Type To Include:",
                                             c("fasta_dir","fasta_prefix","fasta_extension","fasta_file1",
                                               "fasta_file2","out_bam_file_name","threads")),
                          verbatimTextOutput("Inputs"),
                          p("Outputs"),
                          verbatimTextOutput("Outputs"),
                          hr(),
                          h4("Save CWL File"),
                          p("Once you're ready to save your file, use the text input to input a name for your CWL File (don't include the extension),
                            then download the file you've made. After you've downloaded the file, a button will appear that will allow you to continue 
                            to the next tab and generate a YML file."),
                          textInput("CWLFileName", "CWL File Name:", width = "100%", placeholder = "Please input the name of your CWL File"),
                          div(style="text-align: right;",downloadButton("downloadCWLFile", "Save as CWL File")),
                          uiOutput("justContinue")
                        )
                        
                      )),
             # YML FILE UI
             tabPanel("YML File Creation",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Load CWL File"),
                          uiOutput("sidebarGetCWLFile"),
                          width = 5  # width sidebar + width of main panel must equal 12 (5 + 7 = 12). Default is 4 & 6
                        ),
                        mainPanel(
                          uiOutput("dynamicUIInputList"),
                          width=7
                        )
                        
                      )),
             # CONTACT UI
             tabPanel("Contact", 
                      sidebarLayout(
                        sidebarPanel(HTML('<center><img src="BYULogo.png" width="170"></center>')
                        ),
                        mainPanel(
                          h4("Contact"),
                          HTML(paste('<div>For questions and comments, please visit', 
                                     '<a target="_blank", href="https://piccolo.byu.edu/Contact.aspx">https://piccolo.byu.edu/Contact.aspx</a>.',
                                     '<p>The source code for Throughput can be found at', 
                                     '<a target="_blank", href="https://github.com/elizabethcook21/Throughput">https://github.com/elizabethcook21/Throughput</a>.</p></div>'))
                        )
                        
                      ))
  )
)


# Server --------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  volumes = getVolumes()
  values <- reactiveValues(numRows = 0, types = NULL, names = NULL, userInput = NULL, CWLFileFinalString = "",
                           CWLFileStart = "#!/usr/bin/env cwl-runner\n\ncwlVersion: v1.0\nclass: CommandLineTool",
                           Requirements = "requirements:\n\tDockerRequirement:\n\t\tdockerImageId: ",
                           Requirements2 = "\n\t\tdockerFile: |-\n\t\t\tFROM ",
                           RequirementsRUN = "\n\t\t\tRUN ",
                           RequirementsEnd = "\n\tInlineJavascriptRequirement: {}\n\tShellCommandRequirement: {}",
                           Arguments = "arguments:\n\t- shellQuote: false\n\tvalueFrom: >\n\t\t",
                           Inputs = "inputs:\n\t",
                           Outputs = "outputs:\n\texample_out:\ntype: stdout\nout_files:\ntype:\ntype: array\nitems: File\noutputBinding:\nglob:stdout: output.txt",
                           finalRequirements = "", finalArguments = "", finalInputs = "", finalOutputs = "")
  
  
  
  # GENERATE CWL FILE -----------------------------------------------------
  output$CWLStart <- renderText({ values$CWLFileStart })
  
  output$Requirements <- renderText({ 
    values$finalRequirements <- paste0(values$Requirements, input$dockerImageID, values$Requirements2,
           input$dockerFileFROM, values$RequirementsRUN, input$dockerFileRUN,
           values$RequirementsEnd)
  })
  
  output$Arguments <- renderText({ 
    values$finalArguments <- paste0(values$Arguments, input$ExecutionScript)
  })
  
  output$Inputs <- renderText({
    values$finalInputs <- paste0(values$Inputs, inputStringGenerator(input$inputType))
  })
  
  output$Outputs <- renderText({
    values$finalOutputs <- paste0(values$Outputs)
  })
  
  output$downloadCWLFile <- downloadHandler(
    filename = function(){
      if(input$CWLFileName != ""){
        paste0(gsub(" ", "_", trimws(input$CWLFileName)), ".cwl", sep = "")
      }
      else{ "CWLFile.cwl" }
    },
    content = function(file){
      finalString <- paste0(values$CWLFileStart, "\n",
                            values$finalRequirements, "\n",
                            values$finalArguments, "\n",
                            values$finalInputs, "\n",
                            values$finalOutputs)
      write(finalString, file)
      output$justContinue <- renderUI({
        tagList(
          br(), 
          div(style="text-align: right;",actionButton("continue", "Continue to YML"))
        )
      })
    }
  )
  
  observeEvent(input$continue, {
    updateTabsetPanel(session, "tabs", selected = "YML File Creation")
  })
  
  # GENERATE YML FILE ----------------------------------------------------------------------------
  
  output$sidebarGetCWLFile <- renderUI({
    print(values$finalInputs)
    # If the user hasn't edited the input field on the CWL page, give them an opportunity to upload a file
    if(values$finalInputs == "inputs:\n\t"){
      tagList(
        p("Please upload a cwl file containing information about the desired inputs."),
        fileInput("cwlfile", "CWL File input:",
                  accept = ".cwl",
                  placeholder= "Find CWL files")
      )
      
    } else{
      radioButtons("uploadOrUsePreviousTab", "Would you rather upload another file or use the previous information?",
                   choices = c("Upload New File" = "new", "Use Exising Inputs from previous tab" = "existing"), selected = NULL,
                   inline = FALSE, width = "100%", choiceNames = NULL,
                   choiceValues = NULL)
    }
  })
  
  observeEvent(input$uploadOrUsePreviousTab, {
    if(input$uploadOrUsePreviousTab == "new"){
      # work here on Wednesday 
      print("made it to new button")
    }else{
      print("made it to existing button")
    }
  })
  
  
  # Functionality for Importing a preexisting CWL file 
  observeEvent(input$cwlfile, ignoreInit = T, {
    fileInput <- read_cwl(input$cwlfile$datapath, format = "yaml")
    
    Types = tools::toTitleCase(unname(unlist(fileInput %>% parse_inputs())))
    Names = tools::toTitleCase(names(fileInput$inputs))
    Values = rep("", length(Names))
    
    # Set reactive variables that can be used to build the YML file
    values[["DF"]] <- cbind(Names, Types, Values)
    values$numRows = nrow(values[["DF"]])
    values$types = Types
    values$names = Names
    values$userInput = rep("", values$numRows)
    names(values$userInput) <- 1:values$numRows
    
    
    # This function embeds the UI for our module so it's dynamic
    output$dynamicUIInputList <- renderUI({
      tagList(
        lapply(1:values$numRows, function(i) {
          listOutputUI(values$names[i], values$types[i] )
        }),
        hr(),
        h4("Save YML File"),
        textInput("FileName", "Choose a Name for your File",
                  width = "100%",
                  placeholder = "Do not include a filetype and please don't have spaces"), br(),
        div(style="text-align: right;",downloadButton("SaveYML", "Save as YML File"))
      )
    })
    
  })
  
  # This function will "CallModule" (which calls the server end of the module, using the same ID as the UI end)
  observe({
    lapply(1:values$numRows, function(i) {
      callModule(listOutput, values$names[i], i)
    })
  })
  
  # This function will service the module (must take in session). It is important to note that the input, output, and session here refer
  #   to the namespace from the module, not the Shiny App.
  listOutput <- function(input, output, session, modID){
    
    observeEvent(input$numericText,{
      values$userInput[modID] <- input$numericText
    })
    
    observeEvent(input$inputText,{
      values$userInput[modID] <- input$inputText
    })
    
    # These are file and directory searches. They're an eventual addition to the app, but they are too slow (3/11/2020)
    # observe({
    #   volumes = getVolumes()
    #   shinyFileChoose(input, "Btn_GetFile", roots = volumes, session = session)
    #   if(!is.null(input$Btn_GetFile)){
    #     file_selected <-parseFilePaths(volumes, input$Btn_GetFile)
    #     output$txt_file <- renderUI(
    #       p(paste0("Selected File Path: ",as.character(file_selected$datapath), sep = "" ))
    #     )
    #     if (nrow(file_selected) != 0){
    #       values$userInput <- c(file_selected$datapath, values$userInput)
    #       print(values$userInput)
    #     }
    #   }
    #   
    # })
    # 
    # observe({
    #   volumes = getVolumes()
    #   shinyDirChoose(input, "Btn_GetDir", roots = volumes, session = session)
    #   if(!is.null(input$Btn_GetDir)){
    #     dir_selected <- parseDirPath(volumes, input$Btn_GetDir)
    #     output$txt_file <- renderUI(
    #       p(paste0("Selected Dir Path: ",dir_selected, sep = ""))
    #     )
    #     #if (nrow(dir_selected) != 0){
    #     #  values$userInput <- c(dir_selected, values$userInput)
    #     #  print(values$userInput)
    #     #}
    #   }
    # })

   
  }
  
  
  # Save YML File
  output$SaveYML <- downloadHandler(
    filename = function(){
      if(!is.null(input$FileName)){
        paste0(gsub(" ", "_", trimws(input$FileName)), ".yaml", sep = "")
      }
      else{
       paste("YAMLDownload", ".yaml", sep="")
     }
    },
    content = function(file){
      OutputDF <- as_tibble(values[["DF"]])
      OutputDF$Values <- unname(values$userInput)
      # TODO Let user pick file name
      use_yml_file(parseDFToYaml(OutputDF), file)
      showNotification(type = "message", ui = "Your file was successfully saved.")
    }
  )
  
   
}

shinyApp(ui, server)