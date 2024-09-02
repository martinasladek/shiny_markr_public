# ShinyMarkR (code copy)

This repository contains the code for the app ShinyMarkR presented at RSS2024. Please note that this repository is not wrapped up as an R package, which is currently under development here: https://github.com/martinasladek/shinymarkr

You can either fork this repository or download a copy to customise the app to your needs. Some instructions for customisation are in this readme and I'm also happy to work with you to set up the app and add features - get in touch if interested. 

## Customisation notes 

The main things to customise are: 

- The feedback comments located in the file `shiny_markR/comment_files/list_of_comments.md`
- The rubric boundaries. The code for settings these up is in the file `shiny_markR/www/helpers.R`
- The rubric itself. This file is in `shiny_markR/rubric_files/rubric_text.qmd`
- The marking categories. These are the most complicated to customise as the current settings are hard-coded into the interface. You will need to be consistent in how you name these categories across files. 

The current marking categories are: Analysis Plan, Theoretical Understanding, Coding, and Organisation. If you're using different categories, you will need to replace all mention of these in the following files: 

- `ui.R`  
- `server.R`  
- `www/helpers.R`

Also pay attention to the function `generate_rubric()` in `www/helpers.R`. This function generates the marking tables in the app, and currently expects only 4 categories. This function, as well as `server.R` will need to be re-written if you're working with more than 4 categories. 

To further customise the the appearance of the feedback, edit the file `feedback_yaml`. 