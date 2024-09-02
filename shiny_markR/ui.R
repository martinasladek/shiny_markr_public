library(shiny)
library(shinydashboard)

source("www/helpers.R")

# General settings 

feedback_box_height = "400px"
default_comment = ""


dashboardPage(
  
  
  
  header = dashboardHeader(title = "Shiny MarkR"),
  
  sidebar = dashboardSidebar(
    
    includeCSS("www/css.css"),
    
    sidebarMenu(
      id = "sidebar_menu", 
      menuItem("Marking",                 tabName = "marking", icon = icon("marker"), selected = TRUE), 
      
      fluidRow(
        column(1), 
        column(
          10, 
          HTML(
            paste0(
              "<div class='toc-link'> <a href='#analysis'>", " ‣ Analysis </a><br>",
              "<a href='#coding'>", " ‣ Coding </a> <br>",
              "<a href='#theory'>", " ‣ Theory </a> <br>",
              "<a href='#organization'>", " ‣ Organization </a> <br>",
              "<a href='#overall'>", " ‣ Overall </a> </br> </div>"
            )
          )
        ), 
        column(1)
      ),
      
      menuItem("Edit comments .md",       tabName = "edit_comments_md", icon = icon("comment-dots")), 
      menuItem("Marking progress",        tabName = "marking_progress", icon = icon("chart-line")), 
      numericInput("cand_no", "Candidate number:", value = ""),
      fileInput("edit_feedback_upload", "Edit feedback file:"),
      checkboxInput("toggle_rubric", "Toggle rubric", value = TRUE) 
      
      
    )
  ),
  
  body = dashboardBody(
    
    includeCSS("www/css.css"),
    
    conditionalPanel(
      'input.sidebar_menu == "marking"', 
      fluidRow(
        box(width = 12, 
            title = HTML(paste0("<a name='analysis'>", "Analysis", "</a>")), 
            collapsible = TRUE, collapse = TRUE, solidHeader = TRUE, status = "primary",
            column(6,  checkboxGroupInput("checked_comments_analysis",  "Select comments:", choices = analysis_comments)),
            column(6, 
                   textAreaInput("comments_editable_analysis", "Feedback:", value = default_comment, height = feedback_box_height),
                   numericInput("points_analysis", "Points:", value = 0, min = 0, max = max_pts_analysis, step = 100), 
                   tableOutput("rubric_analysis")
            )
        ),
        box(width = 12, 
            title = HTML(paste0("<a name='coding'>", "Coding", "</a>")), 
            collapsible = TRUE, collapse = TRUE, solidHeader = TRUE, status = "primary",
            column(6,  checkboxGroupInput("checked_comments_coding", "Select comments:", choices = coding_comments)),
            column(6, 
                   textAreaInput("comments_editable_coding", "Feedback:", value = default_comment, height = feedback_box_height),
                   numericInput("points_coding", "Points:", value = 0, min = 0, max = max_pts_coding, step = 100), 
                   tableOutput("rubric_coding")
            )
        ), 
        box(width = 12, 
            title = HTML(paste0("<a name='theory'>", "Theoretical understanding", "</a>")), 
            collapsible = TRUE, collapse = TRUE, solidHeader = TRUE, status = "primary",
            column(6,  checkboxGroupInput("checked_comments_theory", "Select comments:", choices = theory_comments)),
            column(6, 
                   textAreaInput("comments_editable_theory", "Feedback:", value = default_comment, height = feedback_box_height),
                   numericInput("points_theory", "Points:", value = 0, min = 0, max = max_pts_theory, step = 100), 
                   tableOutput("rubric_theory")
                   )
        ),
        box(width = 12, 
            title = HTML(paste0("<a name='organization'>", "Organization", "</a>")), 
            collapsible = TRUE, collapse = TRUE, solidHeader = TRUE, status = "primary",
            column(6,  checkboxGroupInput("checked_comments_org", "Select comments:", choices = org_comments)),
            column(6,
                   textAreaInput("comments_editable_org", "Feedback:", value = default_comment, height = feedback_box_height),
                   numericInput("points_org", "Points:", value = 0, min = 0, max = max_pts_org, step = 100), 
                   tableOutput("rubric_org")
                   )
        ),
        box(width = 12, 
            title = HTML(paste0("<a name='overall'>", "Overall", "</a>")), 
            collapsible = TRUE, collapse = TRUE, solidHeader = TRUE, status = "primary",
            column(6,  checkboxGroupInput("checked_comments_overall", "Select comments:", choices = overall_comments)),
            column(6,
                   textAreaInput("comments_editable_overall", "Feedback:", value = default_comment, height = feedback_box_height),
                   tableOutput("rubric_overall")
            )
        ), 
        box(width = 12, align = "center", 
            column(10),
            column(2,
                   actionButton("generate_feedback", "Generate feedback", 
                                style="color: #fff; background-color: #2596be; border-color: #2e6da4; font-weight: bold; padding:10px; font-size:120%")
            )
        )
        
      )
    ), 
    conditionalPanel(
      'input.sidebar_menu == "edit_comments_md"', 
      fluidRow(
        box(width = 12, 
          textAreaInput("comments_md", "No file uploaded.", height = "900px"), 
          actionButton("save_comments_md", "Save comments (overwrites the original file)")
        )
      )
    ), 
    conditionalPanel(
      'input.sidebar_menu == "marking_progress"', 
      fluidRow(
        column(
          6,
          box(
            width = 12, 
            plotOutput("progress_plot")  
          )
        )
      )
    )
  )
)


