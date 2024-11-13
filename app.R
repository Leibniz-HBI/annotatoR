# Load necessary libraries
library(shiny)
library(DT)

# Global reactive values to store data accessible across sessions
sharedData <- reactiveValues(
  data = NULL,
  annotations = data.frame(
    id = character(),
    text = character(),
    username = character(),
    label = character(),
    stringsAsFactors = FALSE
  ),
  class_labels = c("positive", "negative", "neutral"),
  stats = list(
    one_label = 0,
    two_labels = 0,
    three_labels = 0
  )
)

# UI definition
ui <- fluidPage(
  titlePanel("Text Annotation App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File",
                accept = c(".csv")),
      uiOutput("username_ui"),
      actionButton("settings", "Settings"),
      h3("Statistics"),
      verbatimTextOutput("stats_output"),
      downloadButton("downloadData", "Download Annotations")
    ),
    mainPanel(
      h3("Annotate the Text"),
      tags$blockquote(textOutput("text_to_annotate")),
      uiOutput("label_buttons"),
      h3("Annotations"),
      p(actionButton("delete_annotation", "Delete Selected Annotation")),
      DTOutput("annotations_table")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Store username per session
  user_name <- reactiveVal(NULL)
  
  # Prompt for username when the app is opened
  observe({
    if (is.null(user_name())) {
      showModal(modalDialog(
        title = "Enter Username",
        textInput("username_input", "Username:", ""),
        footer = tagList(
          actionButton("submit_username", "Submit")
        ),
        easyClose = FALSE
      ))
    }
  })
  
  # Save username and remove modal
  observeEvent(input$submit_username, {
    req(input$username_input)
    user_name(input$username_input)
    removeModal()
  })
  
  # Display current user's username
  output$username_ui <- renderUI({
    req(user_name())
    h4(paste("Logged in as:", user_name()))
  })
  
  # Load data when a file is uploaded
  observeEvent(input$file1, {
    req(input$file1)
    data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    if(!all(c("id", "text") %in% colnames(data))) {
      showModal(modalDialog(
        title = "Error",
        "The uploaded file must contain 'id' and 'text' columns.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    data$id <- as.character(data$id)
    sharedData$data <- data
  })
  
  # Determine next text to annotate
  next_text <- reactive({
    req(sharedData$data, user_name())
    data <- sharedData$data
    ann <- sharedData$annotations
    user_ann <- ann[ann$username == user_name(), ]
    data_to_annotate <- data[!(data$id %in% user_ann$id), ]
    id_counts <- table(ann$id)
    ids_with_less_than_three <- names(id_counts[id_counts < 3])
    data_to_annotate <- data_to_annotate[data_to_annotate$id %in% ids_with_less_than_three | !(data_to_annotate$id %in% ann$id), ]
    if(nrow(data_to_annotate) > 0) {
      data_to_annotate$text[1]
    } else {
      NULL  # Return NULL when no texts are left
    }
  })
  
  # Display text to annotate
  output$text_to_annotate <- renderText({
    text <- next_text()
    if (!is.null(text)) {
      text
    } else {
      "No more texts to annotate."
    }
  })
  
  # Generate label buttons
  output$label_buttons <- renderUI({
    req(sharedData$data)
    labels <- sharedData$class_labels
    text_available <- !is.null(next_text())
    buttons <- lapply(labels, function(label) {
      actionButton(session$ns(paste0("label_", label)), label, disabled = !text_available)
    })
    do.call(tagList, buttons)
  })
  
  # Handle label button clicks
  observe({
    req(sharedData$data, user_name())
    labels <- sharedData$class_labels
    lapply(labels, function(label) {
      observeEvent(input[[paste0("label_", label)]], {
        ann <- sharedData$annotations
        current_text <- next_text()
        if (is.null(current_text)) {
          showNotification("No more texts to annotate.", type = "error")
          return()
        }
        data <- sharedData$data
        current_row <- data[data$text == current_text, ]
        current_id <- current_row$id[1]
        if(any(ann$id == current_id & ann$username == user_name())) {
          showNotification("You have already annotated this text.", type = "error")
          return()
        }
        new_ann <- data.frame(
          id = current_id,
          text = current_text,
          username = user_name(),
          label = label,
          stringsAsFactors = FALSE
        )
        sharedData$annotations <- rbind(ann, new_ann)
        update_stats()
      })
    })
  })
  
  # Update statistics
  update_stats <- function() {
    ann <- sharedData$annotations
    counts <- table(ann$id)
    sharedData$stats$one_label <- sum(counts == 1)
    sharedData$stats$two_labels <- sum(counts == 2)
    sharedData$stats$three_labels <- sum(counts == 3)
  }
  
  # Display statistics
  output$stats_output <- renderText({
    paste(
      "Texts with one label:", sharedData$stats$one_label, "\n",
      "Texts with two labels:", sharedData$stats$two_labels, "\n",
      "Texts with three labels:", sharedData$stats$three_labels
    )
  })
  
  # Render annotations table with reversed order
  output$annotations_table <- renderDT({
    ann <- sharedData$annotations
    if (nrow(ann) > 0) {
      ann <- ann[rev(seq_len(nrow(ann))), ]  # Reverse the order
    }
    datatable(ann, selection = 'single')
  })
  
  # Handle deletion of annotations
  observeEvent(input$delete_annotation, {
    req(input$annotations_table_rows_selected)
    ann <- sharedData$annotations
    # Adjust the row index because the table is reversed
    selected_row <- nrow(ann) - input$annotations_table_rows_selected + 1
    ann <- ann[-selected_row, ]
    sharedData$annotations <- ann
    update_stats()
  })
  
  # Provide download functionality
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("annotations-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(sharedData$annotations, file, row.names = FALSE)
    }
  )
  
  # Settings modal for class labels
  observeEvent(input$settings, {
    showModal(modalDialog(
      title = "Configure Class Labels",
      textInput("class_labels_input", "Class Labels (comma-separated):", value = paste(sharedData$class_labels, collapse = ", ")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_class_labels", "Save")
      ),
      easyClose = TRUE
    ))
  })
  
  # Save new class labels
  observeEvent(input$save_class_labels, {
    labels_input <- input$class_labels_input
    labels <- unlist(strsplit(labels_input, "\\s*,\\s*"))
    if(length(labels) > 0) {
      sharedData$class_labels <- labels
      removeModal()
    } else {
      showNotification("Please enter at least one class label.", type = "error")
    }
  })
  
}

# Run the application
shinyApp(ui, server)
