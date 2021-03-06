library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(tidycwl)
library(tidyverse)
library(shinyFiles) # This is for getting the file path of a file (maybe a later implementation of the app?)
library(ymlthis)

# Global Functions ---------------------------------------------------------------------
INPUT_TYPE_CHOICES = c("String", "Int", "Directory", "File")
BUTTON_CLICK_COUNT = 0

# This is a UI Module for text input
listOutputUI <- function(id, type){
  ns <- NS(id)
  
  if (type == "Int"){
    tagList(
      h3(paste0("Input: ", id, sep = "")),
      numericInput(ns("numericText"),"Numeric Input", 10, width = '100%'),
      hr()
    )
  } else {
    tagList(
      h3(paste0("Input: ", id, sep = "")),
      textInput(ns("inputText"),"Text Input", width = '100%', placeholder = paste0("Please respond to the input type with a ", type, sep ="")),
      hr()
    )
  }
  
}

editInputUI <- function(name, type){
  ns <- NS(name)
  tagList(
    fluidRow(
      column(width = 4, textInput(ns("textInput"), label = NULL, value = name)),
      column(width = 4, selectizeInput(ns("selectCorrectInput"), label = NULL, choices = INPUT_TYPE_CHOICES, selected = type)),
      column(width = 1, actionButton(ns("deleteButton"), label = NULL, icon = icon("trash"), style="color: #000000; background-color: #C8C7C2; border-color: #000000"))
    )
  )
}

# This function converts a Dataframe to a YML String (so it can save as a file)
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

# This function codes for the help button icon that a user can hover over
helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}

updateInputVerbatim <- function(DF){
  newVerbatim = ""
  if(nrow(DF) != 0){
    for(row in 1:nrow(DF)){
      newVerbatim <- paste0(newVerbatim, DF[row,1], ":\n\t\tType: ", DF[row,2], "\n\t")
    }
  }
  return(newVerbatim)
}

# User Interface ---------------------------------------------------------------------------
ui <- fluidPage (
  useShinyjs(),
  navbarPage(id = "tabs", title = "Throughput", theme = "sandstone.css",   
             #CWL Generation UI 
             tabPanel("CWL File Generation", 
                      sidebarLayout(
                        sidebarPanel(
                          HTML('<center><img src="ThroughputLogo.png" width=100%></center>'),
                          h4("Build your CWL File"),
                          p("Welcome to Throughput, an interface for helping generate CWL and YML files."),
                          p("A docker image is a read-only template for creating containers, 
                            and provides a filesystem based on an ordered union of multiple layers of files and directories, which can be shared with other images and containers."),
                          p("See https://windsock.io/explaining-docker-image-ids/")
                        ),
                        mainPanel(
                          h4("CWL File Header"), 
                          verbatimTextOutput("CWLStart"),
                          br(),
                          
                          # Requirements
                          h4("Requirements"),
                          textInput("dockerImageID",
                                    label = div("Docker Image Id:", helpButton("The image id that will be used for docker run.  May be a human-readable image name or the image identifier hash.")), 
                                    width = "100%"),
                          textInput("dockerFileFROM", 
                                    label = div("Docker File FROM file:", helpButton("Add text later")),
                                    width = "100%"),
                          textInput("dockerFileRUN",
                                    label = div("Docker Build Commands:", helpButton("Add text later")), 
                                    width = "100%"),
                          verbatimTextOutput("Requirements"),
                          br(),
                          
                          # Arguments
                          h4("Arguments"),
                          textAreaInput("ExecutionScript", "Execution Script", width = "1000px", height = "250px"),
                          verbatimTextOutput("Arguments"),
                          br(),
                          
                          # Inputs
                          h4("Inputs"),
                          fluidRow(
                            column(width = 4, textInput("typeInputName", "Add Input Name:")),
                            column(width = 4, selectizeInput("inputTypeSelection", "Select Input Type:", choices = INPUT_TYPE_CHOICES,
                                                             options = list(placeholder = 'Please select an input type'))),
                            column(width = 4, actionButton("addToInputList", "Add to Input List", style="color: #000000; background-color: #C8C7C2; border-color: #000000"))
                          ),
                          tags$style(type='text/css', "#addToInputList { width:100%; margin-top: 20px;}"),
                          uiOutput("editInputDynamicModule"),
                          verbatimTextOutput("Inputs"), 
                          br(),
                          
                          # Outputs
                          h4("Outputs"),
                          verbatimTextOutput("Outputs"),
                          hr(),
                          h4("Save CWL File"),
                          p("Once you're ready to save your file, use the text input to input a name for your CWL File (don't include the extension),
                            then download the file you've made. After you've downloaded the file, a button will appear that will allow you to continue 
                            to the next tab and generate a YML file."),
                          textInput("CWLFileName", "CWL File Name:", width = "100%", placeholder = "Please input the name of your CWL File"),
                          div(style="text-align: right;",downloadButton("downloadCWLFile", "Save as CWL File"), disabled(actionButton("continueYML", "Continue")))
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
                                     '<a target="_blank", href="https://github.com/srp33/Throughput">https://github.com/srp33/Throughput</a>.</p></div>'))
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
                           Inputs = "inputs:",
                           inputDF = data.frame(Name=as.character() , Type=as.character(), stringsAsFactors = FALSE),
                           Outputs = "outputs:\n\texample_out:\ntype: stdout\nout_files:\ntype:\ntype: array\nitems: File\noutputBinding:\nglob:stdout: output.txt",
                           finalRequirements = "", finalArguments = "", finalInputs = "", finalOutputs = "", 
                           deleteComplete = FALSE, countNumInputs = 0)
  
  # This function dynamically generates the GenerateYML page (it needs to be in the server because it accesses the reactive values)
  generateYMLValues <- function(namedList){
    Types = tools::toTitleCase(namedList)
    Names = tools::toTitleCase(names(namedList))
    Values = rep("", length(Names))
    
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
    
  }
  
  renderInputModule <- function(){
    output$editInputDynamicModule <- renderUI({
      tagList(
        p("Edit your input list:"),
        lapply(1:nrow(values$inputDF), function(i) {
          editInputUI(values$inputDF[i,1], values$inputDF[i,2])
        })
      )
    })    
  }
  
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
  
  observeEvent(input$addToInputList, {
    if(!is.na(input$typeInputName) && !is.na(input$inputTypeSelection)){
      # Save the new inputs to finalInputs to display it in the verbatim text output
      values$inputDF <- rbind(values$inputDF, data.frame(Name = input$typeInputName, Type = input$inputTypeSelection, stringsAsFactors = FALSE))
      values$finalInputs <- paste0("\n\t", updateInputVerbatim(values$inputDF))
      # Dynamic UI rendering for editable input list
      if(nrow(values$inputDF) > 0){
        renderInputModule()
      }
      
      updateTextInput(session, inputId = "typeInputName", value="")
    }
  })
  
  output$Inputs <- renderText({
    paste0(values$Inputs, values$finalInputs)
  })
  
  # Code to call Dynamic Input Module and get listeners ready 
  observe({
    if(nrow(values$inputDF) > 0){
      lapply(1:nrow(values$inputDF), function(i) {
        callModule(editInputListener, values$inputDF[i,1], i)
      })
    }
  })
  
  # Listener for input module (includes listeners for all buttons in the module)
  editInputListener <- function(input, output, session, modID){

    # OPTION 1 - uses eventReactive
    # deleteRow <- eventReactive(input$deleteButton ,{
    #   # Delete the row from the inputDF table (the verbatim text)
    #   if (values$deleteComplete == FALSE){
    #     values$inputDF = values$inputDF[-c(modID),]
    #     values$finalInputs <- paste0("\n\t", updateInputVerbatim(values$inputDF))
    #     output$Inputs <- renderText({
    #       paste0(values$Inputs, values$finalInputs)
    #     })
    #     values$deleteComplete = TRUE
    #   }
    #   # Stop the delete button from deleting anything else
    # })
    # 
    # observeEvent(input$deleteButton, {
    #   deleteRow()
    # })
    
    # OPTION 2
    observeEvent(input$deleteButton, {
      #browser()
      if (values$deleteComplete != TRUE){
        values$inputDF <- values$inputDF[!(row.names(values$inputDF)) %in% toString(modID), ]
        values$finalInputs <- paste0("\n\t", updateInputVerbatim(values$inputDF))
        output$Inputs <- renderText({
          paste0(values$Inputs, values$finalInputs)
        })
      }
      values$deleteComplete = TRUE
      values$countNumInputs = values$countNumInputs + 1
      print(values$countNumInputs)
    })
    
    observe({
      if(values$countNumInputs == nrow(values$inputDF) -1 && values$deleteComplete == -1){
        #browser()
        print("new row")
        values$deleteComplete = FALSE
        values$countNumInputs = 0
      }
    })
    
    # Observe changes to the drop down (on the input list)
    observeEvent(input$selectCorrectInput,{
      updateSelectInput(session, "selectCorrectInput", selected = input$selectCorrectInput)
      values$inputDF[modID,2] <- input$selectCorrectInput
      values$finalInputs <- paste0("\n\t", updateInputVerbatim(values$inputDF))
    })
    
    # Observe changes to the text input (on the input list)
    observeEvent(input$textInput,{
      values$inputDF[modID,1] <- input$textInput
      values$finalInputs <- paste0("\n\t", updateInputVerbatim(values$inputDF))
    })

  }
  
  # Output Listener
  output$Outputs <- renderText({
    values$finalOutputs <- values$Outputs
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
      enable("continueYML")
    }
  )
  
  observeEvent(input$continueYML, {
    updateTabsetPanel(session, "tabs", selected = "YML File Creation")
  })
  
  # GENERATE YML FILE ----------------------------------------------------------------------------
  
  output$sidebarGetCWLFile <- renderUI({
    # If the user hasn't edited the input field on the CWL page, give them an opportunity to upload a file
    if(values$finalInputs == ""){
      tagList(
        p("Please upload a cwl file containing information about the desired inputs."),
        fileInput("cwlfile", "CWL File input:",
                  accept = ".cwl",
                  placeholder= "Find CWL files")
      )
      
    } else{
      radioButtons("uploadOrUsePreviousTab", "Would you rather upload another file or use the inputs from the CWL file that you just created in the previous tab?",
                   choices = c("Upload new file" = "new", "Use exising inputs from previous tab" = "existing"), 
                   selected = "existing", inline = FALSE, width = "100%",
                   choiceNames = NULL, choiceValues = NULL)
    }
  })
  
  observeEvent(input$uploadOrUsePreviousTab, {
    # If the user hasn't edited the input field on the CWL page, give them an opportunity to upload a file
    if(input$uploadOrUsePreviousTab == "new"){
      output$sidebarGetCWLFile <- renderUI({
          tagList(
            radioButtons("uploadOrUsePreviousTab", "Would you rather upload another file or use the previous information?",
                         choices = c("Upload New File" = "new", "Use Exising Inputs from previous tab" = "existing"),
                         inline = FALSE, width = "100%", choiceNames = NULL, choiceValues = NULL),
            p("Please upload a cwl file containing information about the desired inputs."),
            fileInput("cwlfile", "CWL File input:", accept = ".cwl", placeholder= "Find CWL files")
          )
        })
    }else{
      # Convert input data frame to a named list
      namedListFromDF <- as.character(values$inputDF[,2])
      names(namedListFromDF) <- values$inputDF[,1]
      generateYMLValues(namedListFromDF)
    }
  })
  
  # Functionality for importing a preexisting CWL file 
  observeEvent(input$cwlfile, ignoreInit = T, {
    fileInput <- read_cwl(input$cwlfile$datapath, format = "yaml")
    
    # Parse the inputs from the file into a named vector. parse_inputs will just grab the type of input. "names" will grab the name of the input, then I combine the two values into the dataframe
    inputsToParse <- unlist(fileInput %>% parse_inputs())
    names(inputsToParse) <- names(fileInput$inputs)
    
    # Send this 
    generateYMLValues(inputsToParse)
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
      use_yml_file(parseDFToYaml(OutputDF), file)
      showNotification(type = "message", ui = "Your file was successfully saved.")
    }
  )
  
   
}

shinyApp(ui, server)
