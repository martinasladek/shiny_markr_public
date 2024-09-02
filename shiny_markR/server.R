library(ggplot2)
library(shiny)
library(shinyalert)

source("www/helpers.R")

rubric_font_size = 26
col_width = "2.75cm"

function(input, output, session) {
  
  # General setup  ----------------------------------------------------------
  rvs <- reactiveValues()
  
  observe({
    
    rvs$total_score <-  sum(c(input$points_analysis, input$points_theory, input$points_coding, input$points_org))
    rvs$all_points <- list(input$points_analysis, input$points_theory, input$points_coding, input$points_org)
    
    rvs$grade_df <- read_grade_df()
    
  })
  

  # Progress checking  ------------------------------------------------------

  
  output$progress_plot <- renderPlot({
    class_hist(bin_helper = bin_helper, col_pal = col_pal, grade_df = rvs$grade_df)
  })
  

  
  # Comments - Analysis plan ------------------------------------------------
  
  # Set up reactive values. Each section needs to have its own set of reactive 
  # values for this to work. Initiate section-specific "latest_selection_set"
  # within these reactive values (empty/NULL until something is checked)
  
  rvs_analysis <- reactiveValues()
  rvs_analysis$latest_selection_set <- NULL
  
  observeEvent(input$checked_comments_analysis, {
    
    add_comments(input = input, rvs = rvs_analysis, 
                 input_checked_comments = "checked_comments_analysis", 
                 text_area_input_id = "comments_editable_analysis")
    
  })
  
  # Comments - theory -------------------------------------------------------
  
  # See code description for the first section (Analysis Plan)
  rvs_theory <- reactiveValues()
  rvs_theory$latest_selection_set <- NULL
  
  observeEvent(input$checked_comments_theory, {
    
    add_comments(input = input, rvs = rvs_theory, 
                 input_checked_comments = "checked_comments_theory", 
                 text_area_input_id = "comments_editable_theory")
    
  })  
  
  
  # Comments - coding -------------------------------------------------------
  
  # See code description for the first section (Analysis Plan)
  rvs_coding <- reactiveValues()
  rvs_coding$latest_selection_set <- NULL
  
  observeEvent(input$checked_comments_coding, {
    
    add_comments(input = input, rvs = rvs_coding, 
                 input_checked_comments = "checked_comments_coding", 
                 text_area_input_id = "comments_editable_coding")
    
  })
  
  # Comments - organization  ------------------------------------------------
  
  # See code description for the first section (Analysis Plan)
  rvs_org <- reactiveValues()
  rvs_org$latest_selection_set <- NULL
  
  observeEvent(input$checked_comments_org, {
    
    add_comments(input = input, rvs = rvs_org, 
                 input_checked_comments = "checked_comments_org", 
                 text_area_input_id = "comments_editable_org")
    
  })  
  
  
  # Comments - overall ------------------------------------------------------
  
  # See code description for the first section (Analysis Plan)
  rvs_overall <- reactiveValues()
  rvs_overall$latest_selection_set <- NULL
  
  observeEvent(input$checked_comments_overall, {
    
    add_comments(input = input, rvs = rvs_overall, 
                 input_checked_comments = "checked_comments_overall", 
                 text_area_input_id = "comments_editable_overall")
    
  }) 
  
  
  
  
  # Toggle rubric -----------------------------------------------------------
  
  # Conditionally show rubric if the checkbox is selected (input$toggle_rubric)
  # The output needs to react to changes in any of the allocated points so the
  # numeric inputs are collected in rvs$all_points. The rubric is displayed in 
  # all of the comment sections. Shiny doesn't support multiple outputs with the 
  # same id, so the table needs to be generated 5 times -_- 
  
  observeEvent(rvs$all_points, {
    
    output$rubric_analysis <- function() {
      if(input$toggle_rubric){
        generate_rubric(grid = grid, rvs$all_points[[1]], rvs$all_points[[2]], rvs$all_points[[3]], rvs$all_points[[4]], rubric_font_size, col_width, 2, scrollbox = TRUE) 
      } else {
        return(NULL)
      }
    }
    
    output$rubric_theory <- function() {
      if(input$toggle_rubric){
        generate_rubric(grid = grid, rvs$all_points[[1]], rvs$all_points[[2]], rvs$all_points[[3]], rvs$all_points[[4]], rubric_font_size, col_width, 4, scrollbox = TRUE)
      } else {
        return(NULL)
      }
    }
    
    output$rubric_coding <- function() {
      if(input$toggle_rubric){
        generate_rubric(grid = grid, rvs$all_points[[1]], rvs$all_points[[2]], rvs$all_points[[3]], rvs$all_points[[4]], rubric_font_size, col_width, 6, scrollbox = TRUE)
      } else {
        return(NULL)
      }
    }
    
    output$rubric_org <- function() {
      if(input$toggle_rubric){
        generate_rubric(grid = grid, rvs$all_points[[1]], rvs$all_points[[2]], rvs$all_points[[3]], rvs$all_points[[4]], rubric_font_size, col_width, 8, scrollbox = TRUE)
      } else {
        return(NULL)
      }
    }
    
    output$rubric_overall <- function() {
      if(input$toggle_rubric){
        generate_rubric(grid = grid, rvs$all_points[[1]], rvs$all_points[[2]], rvs$all_points[[3]], rvs$all_points[[4]], rubric_font_size, col_width, 10, scrollbox = TRUE)
      } else {
        return(NULL)
      }
    }
    
    
  })
  
  
  # Generate feedback files -------------------------------------------------
  
  observeEvent(input$generate_feedback, {
    
    
    rvs <- reactiveValues()
    rvs$total_points <- sum(c(input$points_analysis, input$points_theory, input$points_coding, input$points_org))
    
    # Check whether file already exists
    current_html_file <- paste0(input$cand_no, ".html")
    current_html_file_regex <- paste0("^", input$cand_no, "\\",  ".html$")
    rvs$file_check <- list.files(path = paste0("student_feedback_files/html/"), pattern = current_html_file_regex)
    
    
    # Trigger pop-up alert if the candidate number is missing. 
    if(is.na(input$cand_no)){
      
      trigger_gullert(title = "Cadidate number missing!", text = "Enter the right candidate number to avoid doom.")
      
    } else if(
      rvs$total_points > 100 | 
      input$points_analysis > max_pts_analysis | 
      input$points_theory > max_pts_theory | 
      input$points_coding > max_pts_coding | 
      input$points_org > max_pts_org 
    ){
      
      trigger_gullert(
        title = "Incorrect number of points. Check values: ", 
        text = paste0(
          "Analysis: ", input$points_analysis, " (max ", max_pts_analysis, ")", "<br>", 
          "Theory: ", input$points_theory, " (max ", max_pts_theory, ")", "<br>", 
          "Coding: ", input$points_coding, " (max ", max_pts_coding, ")", "<br>", 
          "Organization: ", input$points_org, " (max ", max_pts_org, ")"
        )
      )
      
    } else if(
      (length(rvs$file_check) > 0) & ((is.null(input$overwrite_alert)) | isTRUE(input$overwrite_alert == FALSE))
    ){
      
      trigger_gullert(
        inputId = "overwrite_alert",
        title = "Careful - you're about to overwrite a file.", text = paste0("File ", current_html_file, " already exists. Click 'OK' and 'Generate feedback' again to overwrite it with new feedback."), 
        cancel_button = TRUE, 
        confirm_text = "OK"
      )
      
    } else if(
      (isTRUE(input$overwrite_alert == TRUE) & (length(rvs$file_check) > 0)) | (length(rvs$file_check) == 0)
    ){
      
      # Compile the YAML with the candidate number in the title 
      feedback_yaml <- readLines("feedback_yaml") |> 
        stringr::str_replace_all(string = _, "title:", paste0("title: 'Feedback for ", input$cand_no,"'")) |> 
        paste0(collapse = "\n")
      
      # Compile the text of the quarto doc by pulling static input from all  
      # text area input sections 
      qmd_lines <- paste0(
        feedback_yaml, "\n\n", 
        cc(
          "here::here('shiny_markR/www/helpers.R') |> source()", 
        ), "\n\n", 
        cc(other_cc_settings = ", message=FALSE",
          code_string = 
            paste0("grade_csv <- here::here('shiny_markR/student_feedback_files/csv/", input$cand_no, ".csv') |> readr::read_csv()")
        ), "\n\n",
        "## Marking rubric \n\n", 
        cc(
          paste0(
            "generate_rubric(grid = grid, ", 
            "grade_csv$points_analysis", ", ", 
            "grade_csv$points_theory", ", ",  
            "grade_csv$points_coding", ", ", 
            "grade_csv$points_org", 
            ")"
          )
        ), "\n\n", 
        "## Feedback", "\n\n", 
        "### Analysis \n\n", 
        input$comments_editable_analysis, "\n\n", 
        "### Theoretical understanding \n\n", 
        input$comments_editable_theory, "\n\n", 
        "### Coding \n\n", 
        input$comments_editable_coding, "\n\n",
        "### Organization \n\n", 
        input$comments_editable_org, "\n\n",
        "### Overall \n\n", 
        input$comments_editable_overall, "\n\n",
        cc("", other_cc_settings = ", child='../../rubric_files/rubric_text.qmd'")
      )
      
      # Set up version count to avoid overwriting
      
      # Set up file path for the feedback files and save the .qmd file
      file_path_qmd <- paste0("student_feedback_files/qmd/", input$cand_no, ".qmd")
      writeLines(qmd_lines, file_path_qmd)
      
      # Save file as a csv for editing
      edit_csv <- data.frame(
        cand_no = input$cand_no, 
        points_analysis = input$points_analysis, 
        points_theory = input$points_theory, 
        points_coding = input$points_coding, 
        points_org = input$points_org, 
        comments_analysis = input$comments_editable_analysis, 
        comments_theory = input$comments_editable_theory, 
        comments_coding = input$comments_editable_coding, 
        comments_org = input$comments_editable_org, 
        comments_overall = input$comments_editable_overall
      )
      
      write.csv(
        edit_csv,
        paste0("student_feedback_files/csv/", input$cand_no, ".csv"), 
        row.names = FALSE
      )
      
      # Render the .qmd file as html
      shiny::showNotification("Rendering HTML.")
      quarto::quarto_render(file_path_qmd)
      shiny::showNotification("Files ready.", type = "message")
      
      rvs$folder_name <- ""
      
      file.rename(
        from = paste0("student_feedback_files/qmd/", input$cand_no, ".html"),
        to = paste0("student_feedback_files/html/", rvs$folder_name, input$cand_no, ".html")
      )
      
      
      # Re-read
      
      rvs$grade_df <- read_grade_df()
      
    }
    
  })
  
  
  # Edit feedback files  ----------------------------------------------------
  
  observeEvent(input$edit_feedback_upload, {
    
    rvs$edit_feedback <- read.csv(input$edit_feedback_upload$datapath)
    
    # Update comment boxes: 
    updateTextAreaInput(
      inputId = "comments_editable_analysis",
      value = rvs$edit_feedback$comments_analysis
    )
    
    updateTextAreaInput(
      inputId = "comments_editable_theory",
      value = rvs$edit_feedback$comments_theory
    )
    
    updateTextAreaInput(
      inputId = "comments_editable_coding",
      value = rvs$edit_feedback$comments_coding
    )
    
    updateTextAreaInput(
      inputId = "comments_editable_org",
      value = rvs$edit_feedback$comments_org
    )
    
    updateTextAreaInput(
      inputId = "comments_editable_overall",
      value = rvs$edit_feedback$comments_overall
    )
    
    # Update point boxes: 
    updateNumericInput(
      inputId = "points_analysis", 
      value = rvs$edit_feedback$points_analysis
    )
    
    updateNumericInput(
      inputId = "points_theory", 
      value = rvs$edit_feedback$points_theory
    )
    
    updateNumericInput(
      inputId = "points_coding", 
      value = rvs$edit_feedback$points_coding
    )
    
    updateNumericInput(
      inputId = "points_org", 
      value = rvs$edit_feedback$points_org
    )
    
    # Update candidate number
    updateNumericInput(
      inputId = "cand_no", 
      value = rvs$edit_feedback$cand_no
    )
    
  })
  
  
  
  
  # Edit comments.md --------------------------------------------------------
  
  observe({
    
    updateTextAreaInput(
      inputId = "comments_md", label = "Comments:",
      value = paste0(comment_lines, collapse = "\n")
    )
    
  })
  
  observeEvent(input$save_comments_md, {
    
    writeLines(
      text = (input$comments_md), paste0("comment_files/", comment_file)
    )
    
    shiny::showNotification("Comments updated", type = "message")
    
  })
  
  
  
}

