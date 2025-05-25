# Doing some art from my Obsidian daily journal notes.
# Plotting concatenated random convex quadrilaterals which area is proportional to the length of the note.
#
# Author: Francesco Grassi
# Date: May 2025

# Libraries ----
library(tidyverse)
library(scico)
source("random_convex_quad.R")  # custom function to generate quadrilaterals

# Read data ----
# 1. Read all my daily journal notes from Obsidian 
# 2. Count each note length, in words
# 3. Create data.frame with note ID and note length

in_path <- "/Users/francescograssi/Library/Mobile Documents/iCloud~md~obsidian/Documents/Fra_vault/02 - Journal"
notes <- list.files(in_path, full.names = TRUE)

# Function to read note, count words and assign ID
read_notes <- function(note_path, idx) {
  
  this_note <- read_lines(note_path)  # read each note line as a character vector element
  this_note <- paste(this_note, collapse = " ")  # combine all lines in one character vector
  word_count <- str_count(this_note, pattern = "\\w+")  # "\\w+" for "word" character (letter, digit, underscore)
  
  note_df <- tibble(
    id = idx,
    word_count = word_count
  )
  
  return(note_df)
  
}

# Map function to all notes and automatically handle indexes
notes_df <- notes %>% 
  imap(\(note_path, idx) read_notes(note_path, idx)) %>% 
  list_rbind()

# Create quadrilaterals ----
# For each note:
# 1. Create a quadrilateral with area proportional to word count
# 2. Translate it so that one vertex coincides with one vertex of the previous quadrilateral.
#    Choice of the vertexes is made according to a custom probability distribution:
#     - high probability: 
#       - left vertex quad n with right vertex quad n-1 (left-to-right movement)
#       - top vertex quad n with bottom vertex quad n-1 (top-down movement)
#       - bottom vertex quad n with top vertex quad n-1 (bottom-up movement)
#     - intermediate probability: 
#       - right vertex quad n with left vertex quad n-1 (right-to-left movement)
#     - low probability: 
#       - random vertex quad n with random vertex quad n-1 (random movement)
# NOTE: should think of how to re-write following code to use recurrence approach (like purrr::accumulate())

quad_list <- vector(mode = "list", length = nrow(notes_df))  # pre-allocate list

# Define rules for vertex translation and corresponding probabilities
rule_names <- c("left-right", "top-bottom", "right-left", "bottom-top", "random")
rule_prob <- c(0.25, 0.13, 0.25, 0.25, 0.12)

for (i in 1:nrow(notes_df)) {
  
  # Get ID and word count of current note
  id <- notes_df[[i, "id"]]
  word_count <- notes_df[[i, "word_count"]]
  
  # Calculate vertexes of quadrilateral
  this_quad <- random_convex_quad(word_count)
  
  # Translate from the second quadrilateral on
  if (i > 1) {
    
    prev_quad <- quad_list[[i-1]]  # get previous quadrilateral
    
    rule <- sample(rule_names, 1, prob = rule_prob)  # select rule based on probability
    
    # Check rules and translate
    if (rule == "left-right") {
      # Left-most vertex of current quad with right-most vertex of previous one
      
      # Get coords of left-most vertex of current quadrilateral
      this_x <- min(this_quad$x)
      this_y <- this_quad[which.min(this_quad$x), "y"]
      
      # Get coords of right-most vertex of previous quadrilateral
      prev_x <- max(prev_quad$x)
      prev_y <- prev_quad[[which.max(prev_quad$x), "y"]]
      
    } else if (rule == "top-bottom") {
      # Top vertex of current quad with bottom vertex of previous one
      
      # Get coords of top vertex of current quadrilateral
      this_y <- max(this_quad$y)
      this_x <- this_quad[which.max(this_quad$y), "x"]
      
      # Get coords of bottom vertex of previous quadrilateral
      prev_y <- min(prev_quad$y)
      prev_x <- prev_quad[[which.min(prev_quad$y), "x"]]
      
    } else if (rule == "right-left") {
      # Right-most vertex of current quad with left-most vertex of previous one
      
      # Get coords of right-most vertex of current quadrilateral
      this_x <- max(this_quad$x)
      this_y <- this_quad[which.max(this_quad$x), "y"]
      
      # Get coords of left-most vertex of previous quadrilateral
      prev_x <- min(prev_quad$x)
      prev_y <- prev_quad[[which.min(prev_quad$x), "y"]]
    } else if (rule == "bottom-top") {
      # Bottom vertex of current quad with top vertex of previous one
      
      # Get coords of bottom vertex of current quadrilateral
      this_y <- min(this_quad$y)
      this_x <- this_quad[which.min(this_quad$y), "x"]
      
      # Get coords of top vertex of previous quadrilateral
      prev_y <- max(prev_quad$y)
      prev_x <- prev_quad[[which.max(prev_quad$y), "x"]]
    } else if (rule == "random") {
      # Random vertex of current quad with random vertex of previous one
      
      # Get coords of random vertex of current quadrilateral
      this_idx <- sample(1:nrow(this_quad), 1)
      this_x <- this_quad[this_idx, "x"]
      this_y <- this_quad[this_idx, "y"]
      
      # Get coords of random vertex of previous quadrilateral
      prev_idx <- sample(1:nrow(prev_quad), 1)
      prev_x <- prev_quad[[prev_idx, "x"]]
      prev_y <- prev_quad[[prev_idx, "y"]]
      
    }
    
    # Translate current quadrilateral by vertexes difference
    this_quad$x <- this_quad$x - (this_x - prev_x)
    this_quad$y <- this_quad$y - (this_y - prev_y)
    
  }
  
  # Return a df with coordinates, ID and word count
  quad_list[[i]] <- tibble(
    id = id,
    word_count = word_count,
    x = this_quad$x,
    y = this_quad$y
  )
  
}

# Plotting ----

# Define palette
colors <- scico(6, palette = "managua")

# 1) Turn list into df
# 2) Refine
# 3) Assign random color to each note
quad_df <- quad_list %>% 
  list_rbind() %>% 
  mutate(id = factor(id)) %>% 
  group_by(id) %>% 
  mutate(fill_col = sample(1:length(colors), 1)) %>% 
  mutate(fill_col = as.factor(fill_col))

# Plot
quad_plot <- quad_df %>% 
  ggplot(aes(x=x, y=y, group = id, fill = fill_col)) + 
  geom_polygon(alpha = 0.5) +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave("quad-note-art.png", quad_plot, width = 2000, height = 2000, unit = "px", dpi = 300, bg = "white")
