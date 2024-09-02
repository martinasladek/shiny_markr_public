
# Set up rubric grid ------------------------------------------------------

grid <- tibble::tibble(
  criterion = gl(4, 5, labels = c("Analysis", "Theory", "Coding", "Organization")),
  classification = gl(5, 1, 20, labels = c("Fail", "Marginal fail", "Pass", "Merit", "Distinction")),
  weight = c(rep(.25, 5), rep(.50, 5), rep(.10, 5), rep(.15, 5)),
  lower_bound = rep(c(0, 30, 47, 57, 74), 4),
) |> 
  dplyr::group_by(criterion) |> 
  dplyr::mutate(
    lower_mark = floor(weight*lower_bound),
    upper_mark = dplyr::lead(lower_mark)-1,
    upper_mark = ifelse(is.na(upper_mark), 100*weight, upper_mark),
  ) |> 
  dplyr::select(-c(weight, lower_bound)) |> 
  dplyr::bind_rows(
    tibble::tibble(
      criterion = "Overall", 
      classification = c("Fail", "Marginal fail", "Pass", "Merit", "Distinction"), 
      lower_mark = c(0,  45, 50, 60,  70), 
      upper_mark = c(44, 49, 59, 69, 100)
    )
  ) |> 
  dplyr::mutate(
    text_boundary = paste0(lower_mark, " - ", upper_mark)
  ) |> 
  dplyr::ungroup()

max_pts_analysis <- max(grid[grid$criterion == "Analysis", ]$upper_mark)
max_pts_theory <- max(grid[grid$criterion == "Theory", ]$upper_mark)
max_pts_coding <- max(grid[grid$criterion == "Coding", ]$upper_mark)
max_pts_org<- max(grid[grid$criterion == "Organization", ]$upper_mark)

# Process .md comment file ------------------------------------------------

# read in the .md file with the comments and pre-process 

comment_file <- "list_of_comments.md"
comment_path <- here::here("shiny_markR/comment_files", comment_file)
comment_lines <- comment_path |> readLines()

comments_df <- 
  tibble::tibble(
    comments = comment_lines
  ) |>
  # remove blank lines
  dplyr::filter(comments != "") |>
  dplyr::mutate(
    # identify section headings
    is_heading = stringr::str_detect(comments, "\\*\\*"),
    heading = ifelse(is_heading, comments, NA) |>
      stringr::str_replace_all(string = _, "\\*\\*", "")
  ) |>
  # create a column with section headings
  tidyr::fill(heading, .direction = "down") |>
  # Remove the rows that are just the heading
  dplyr::filter(!is_heading) |>
  dplyr::group_by(heading)

# Split into a list where each dataset corresponds to a section
comments_list <- comments_df |>
  dplyr::group_split(.keep = TRUE)

# Assign names to the list for subsetting in the UI
names(comments_list) <- comments_df$heading |> unique() |> sort()

# Prepare comments for sections up-front to avoid cluttering-up the UI:
analysis_comments <- comments_list[["Analysis Plan"]]$comments
theory_comments <- comments_list[["Results"]]$comments
coding_comments <- comments_list[["Coding"]]$comments
org_comments <- comments_list[["Organization"]]$comments
overall_comments <- comments_list[["Overall"]]$comments


# Rubric table function ---------------------------------------------------

# Placeholder function for adjusting font in the rubric. Can be used for adding
# html formatting to the rows that contain the marks. 
adj_font <- function(string){
  paste0(string)
}

## Rubric table settings 

stripe_col = "#f5f5f5"

# stripe_vec used for setting up conditional colours in the rubric table
stripe_vec = c(rep("white", 2), rep(stripe_col, 2)) |> rep(2)

# transparency of the conditional colours 
alpha = 1

# font colour for cells that are conditionally highlighted
highlight_font_col <- "white"

# font colour for rubric boundary rows
light_font_col <- "grey"

# Colours for conditional highlighting 
fail_col <- paste0("rgba(225,40,41,", alpha, ")")
margin_col <- paste0("rgba(242,115,37,", alpha, ")")
pass_col <- paste0("rgba(248,204,28,", alpha, ")")
merit_col <- paste0("rgba(114,177,67,", alpha, ")")
distinction_col <- paste0("rgba(3,126,78,", alpha, ")")

# Rows that contain the boundary rows 
bound_header <- c(1,3,5,7,9)

# Generate rubric table with knitr
generate_rubric <- function(
    grid, 
    analysis_pts = 0, theory_pts = 0, coding_pts = 0, org_pts = 0, font_size_selected = 27, 
    col_width = "3cm", 
    highlight_row = 0, 
    scrollbox = FALSE
    ){
  
  highlight_row_bg = ifelse(highlight_row == 0, "#f5f5f5", "#2596be")
  highlight_row_col = ifelse(highlight_row == 0, "black", "white")
  
  grid_wide <- grid |> 
    tidyr::pivot_wider(
      id_cols = criterion,
      names_from = classification,
      values_from = text_boundary
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(order = 1:length(criterion))
  
  total_pts = sum(c(analysis_pts, theory_pts, coding_pts, org_pts))
  
  grid_wide_i <- grid |> 
    dplyr::mutate(
      text_boundary = dplyr::case_when(
        .default = "", 
        criterion == "Analysis"     & lower_mark <= analysis_pts & upper_mark >= analysis_pts ~
          adj_font(analysis_pts), 
        criterion == "Theory"       & lower_mark <= theory_pts   &   upper_mark >= theory_pts ~
          adj_font(theory_pts), 
        criterion == "Coding"       & lower_mark <= coding_pts   &   upper_mark >= coding_pts ~ 
          adj_font(coding_pts), 
        criterion == "Organization" & lower_mark <= org_pts      & upper_mark >= org_pts ~ 
          adj_font(org_pts), 
        criterion == "Overall" & lower_mark <= total_pts      & upper_mark >= total_pts ~ 
          adj_font(total_pts)
      )
    ) |> 
    tidyr::pivot_wider(
      id_cols = criterion,
      names_from = classification,
      values_from = text_boundary
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(order = 1:length(criterion))
  
  grid_wide_full <- dplyr::bind_rows(
    grid_wide, grid_wide_i 
  ) |> 
    dplyr::arrange(order) |> 
    dplyr::group_by(criterion) |> 
    dplyr::mutate(
      criterion = as.character(criterion),
      criterion = ifelse(!duplicated(criterion), "", adj_font(criterion))
    ) |> 
    dplyr::select(-order) |> 
    dplyr::ungroup()
  
  index_fail <- setdiff(which(grid_wide_full$Fail != ""), bound_header)
  index_margin <- setdiff(which(grid_wide_full$`Marginal fail` != ""), bound_header)
  index_pass <- setdiff(which(grid_wide_full$Pass != ""), bound_header)
  index_merit <- setdiff(which(grid_wide_full$Merit != ""), bound_header)
  index_distinction <- setdiff(which(grid_wide_full$Distinction != ""), bound_header)
  
  grid_wide_helper <- grid_wide_full |> 
    dplyr::mutate(
      row_id = dplyr::row_number(),
      bg_fail = ifelse(row_id %in% c(index_fail, index_fail-1), fail_col, stripe_vec),
      bg_margin = ifelse(row_id %in% c(index_margin, index_margin-1), margin_col, stripe_vec),
      bg_pass = ifelse(row_id %in% c(index_pass, index_pass-1), pass_col, stripe_vec),
      bg_merit = ifelse(row_id %in% c(index_merit, index_merit-1), merit_col, stripe_vec),
      bg_distinction = ifelse(row_id %in% c(index_distinction, index_distinction-1), distinction_col, stripe_vec),
    )
  
  grid_wide_full_formatted <- grid_wide_full |> 
    knitr::kable(
      col.names = c("", "Fail", "Marginal fail", "Pass", "Merit", "Distinction"),
      align = c("l", rep("c", 5)), format = "html",  escape = FALSE) |>
    kableExtra::kable_styling() |> 
    
    kableExtra::column_spec(2:6, width = col_width, width_min = col_width) |> 
    
   
    
    kableExtra::row_spec(row = c(0, 3,4, 7,8), background = stripe_col) |> 
    kableExtra::row_spec(row = highlight_row, background = highlight_row_bg, color = highlight_row_col) |> 
    kableExtra::row_spec(row = c(2,4,6,8,10), font_size = font_size_selected, bold = TRUE) |> 
    kableExtra::row_spec(row = c(1,3,5,7, 9), font_size = 13, color = light_font_col) |> 
    
    kableExtra::column_spec(2, background = grid_wide_helper$bg_fail, 
                            color = ifelse(grid_wide_helper$bg_fail == fail_col, highlight_font_col, light_font_col)) |> 
    kableExtra::column_spec(3, background = grid_wide_helper$bg_margin, 
                            color = ifelse(grid_wide_helper$bg_margin == margin_col, highlight_font_col, light_font_col)) |> 
    kableExtra::column_spec(4, background = grid_wide_helper$bg_pass, 
                            color = ifelse(grid_wide_helper$bg_pass == pass_col, highlight_font_col, light_font_col)) |> 
    kableExtra::column_spec(5, background = grid_wide_helper$bg_merit, 
                            color = ifelse(grid_wide_helper$bg_merit == merit_col, highlight_font_col, light_font_col)) |> 
    kableExtra::column_spec(6, background = grid_wide_helper$bg_distinction, 
                            color = ifelse(grid_wide_helper$bg_distinction == distinction_col, highlight_font_col, light_font_col)) 
  
  if(scrollbox == TRUE){
  grid_wide_full_formatted |> 
    kableExtra::scroll_box(width = "100%")
  }
  else{
    grid_wide_full_formatted
  }
}


# Shiny helpers -----------------------------------------------------------

# Helper function for generating code-chunks as text: 
cc <- function(code_string, eval=TRUE, echo=FALSE, other_cc_settings=""){
  paste0(
    "```{r eval=", eval,", echo= ", echo, other_cc_settings, "}\n", code_string, "\n```"
  )
}

# Function for managing interactivity between check box input and text input
add_comments <- function(rvs, input = input, 
                         text_area_input_id = "comments_editable", 
                         input_checked_comments = "checked_comments"
) {
  
  # Save a list of currently checked comments 
  selected_comments <- input[[input_checked_comments]]
  
  # Prevent "latest selection set" from reacting. Use this frozen state to get 
  # the latest checked value by checking the difference between the reactive
  # up-to-date set and the frozen previous set
  new_selection <- setdiff(selected_comments, isolate(rvs[["latest_selection_set"]]))
  new_selection <- paste0(new_selection, "\n\n")
  new_selection <- gsub("\\|", "\n\n", new_selection)
  
  # Update latest selection set for the next time something new is checked.
  rvs[["latest_selection_set"]] <- selected_comments
  
  # Compile the "static" comments that comprise of the chain of latest selected
  # comments (in the order they are checked, not in the order they appear on 
  # the list) as well as manually written comments. 
  rvs[["comments_static"]] <- paste0(
    input[[text_area_input_id]], new_selection, collapse = "\n\n"
  )
  
  # Update the text box with the set of static comments
  updateTextAreaInput(
    inputId = text_area_input_id, label = "Feedback:",
    value = rvs[["comments_static"]]
  )
  
}

# Helper function to trigger pop-up alerts.
trigger_gullert <- function(
    title = "Stop right there!", 
    text = "You haven't included the candidate number", 
    cancel_button = FALSE, confirm_text = "OK", inputId = "shinyalert"){
  
  shinyalert::shinyalert(
    inputId = inputId,
    title = title,
    text = text,
    size = "xs",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    confirmButtonText = confirm_text,
    confirmButtonCol = "#004192",
    showCancelButton = cancel_button,
    imageUrl = "gullert.png", 
    imageWidth = "180",
    imageHeight = "180", 
    animation = TRUE
  )
  
}


# Plotting helpers --------------------------------------------------------

# just...don't judge me okay... I did my best here, I'm sure there's a much 
# more efficient code hiding behind that `get_hist()` function in the general
# feedback reports. 

col_pal = c("#39a4a0", "#9c6a68", "#3679a4", "#edddaf")

bin_helper <- tibble::tibble(
  points_total = 0:100, 
  grade_bin = ggplot2::cut_width(points_total, width = 10, boundary = 0, closed = "left") |> 
    as.character() |> 
    stringr::str_remove_all(string = _, pattern = "\\[|\\]|\\(|\\)") 
) |> 
  tidyr::separate(
    data = _, col = grade_bin, into = c("bin_min", "bin_max"), sep = ","
  ) |> 
  dplyr::mutate(
    bin_max = ifelse(bin_max == "100", "100", as.numeric(bin_max) - 1), 
    grade_bin = factor(paste0(bin_min, "-", bin_max)), 
    class = dplyr::case_when(
      points_total < 50 ~ "Fail", 
      dplyr::between(points_total, 50, 59) ~ "Pass", 
      dplyr::between(points_total, 60, 69) ~ "Merit", 
      points_total >= 70 ~ "Distinction"
    ) |> factor(x = _, levels = c("Fail", "Pass", "Merit", "Distinction")), 
  ) |> 
  dplyr::select(points_total, grade_bin, class)


read_grade_df <- function(all_csvs_filepath = here::here("shiny_markR/student_feedback_files/csv/")){

  # Get file paths of all currently existing csv files
  all_csvs <- list.files(all_csvs_filepath, full.names = TRUE)
  
  if(length(all_csvs) == 0){
    return("No grades found so far - back to work! \nOr don't. Why ruin a nice day... \nAlso, you'll need to reload to see this update \nbecause my reactive values are not reacting \nthe way they should.")
  }
  
  else{
    # Read paths and bind into a dataset. Compute total points. 
    grade_df <- 
      purrr::map(
        .x = all_csvs, 
        .f = read.csv
      ) |> 
      purrr::reduce(
        .x = _, 
        .f = rbind
      ) |> 
      dplyr::mutate(
        points_total = points_analysis + points_theory + points_coding + points_org
      )
    
    return(grade_df)
  }
  
}


class_hist <- function(
    grade_df = grade_df, 
    bin_helper = bin_helper, 
    col_pal = col_pal
){
  
  if(is.character(grade_df)){
    ggplot2::ggplot() + 
      annotate(geom = "text", x = 1, y = 1, label = grade_df) + 
    theme_void()
  }
  
  else{
    # merge with helper and create a plot
    grade_df |> 
      dplyr::left_join(bin_helper, by = "points_total") |> 
      ggplot2::ggplot(data = _, aes(x = grade_bin, color = class, fill = class)) + 
      # geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), alpha = 0.75) +
      geom_bar(alpha = 0.75) +
      annotate(geom = "text", x = 10, y = 15, label = format(mean(grade_df$points_total), digits = 2)) + 
      #scale_y_continuous(breaks = seq(0, 1, 0.02)) +
      scale_y_continuous(breaks = seq(0, 50, 1)) +
      scale_x_discrete(drop = FALSE) +
      coord_cartesian(ylim = c(0, 15)) + 
      scale_colour_manual(values = col_pal) + 
      scale_fill_manual(values = col_pal) + 
      labs(x = "Mark (%)", y = "Number of students", colour = "Classification", fill = "Classification") + 
      theme_minimal()
    
  }
}
