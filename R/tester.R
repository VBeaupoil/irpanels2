library(ggfittext)
library(dplyr)
library(ggplot2)

# Create a small sample dataset
set.seed(42)
df <- data.frame(
  item = sample(1:4, 200, replace = TRUE),
  by   = sample(1:3, 200, replace = TRUE),
  w    = runif(200, 0.5, 2)   # optional weights
)
# Load the function from R/ (adjust path if needed)


plot_groupbar_v <- function(data, item, by, weights, question,
                            lang = "DE", barpadding = 0.1, barposition = "dodge", 
                            legendtitle = "", textsize = 8, min_textsize = 5, ...){
  
  if(missing(barposition) || barposition == "dodge"){
    
    barposition <- position_dodge2(padding = barpadding)
    out <- TRUE
    percent_position <- "top"
    barwidth <- NULL
  } else if(barposition == "stack"){
    out <- FALSE
    percent_position <- "center"
    barwidth <- 0.5
  } else {
    out <- TRUE
    percent_position <- "right"
    barwidth <- NULL
  }
  
  if(missing(weights)){
    data$weight <- 1
  } else {
    data <- data %>% 
      mutate(weight = {{weights}})
  }
  
  if (missing(question)) {
    question <- NA
    question_text <- ""
    
  } else {
    
    if(grepl("Fragetext: «", question) || grepl("Question text: «", question)){
      question_text <- paste0(question, "\n")
    } else {
      question_text <- ifelse(lang == "DE",
                              paste0("Fragetext: «", question, "»\n"),
                              paste0("Question text: «", question, "»\n"))
    }
  }
  
  data %>%
    filter({{item}} > -1,
           {{by}} > -1) %>% 
    crosstab(x = {{item}}, y = {{by}}, weight = weight, format = "long") %>%
    ggplot(aes(x = fct_rev(as.factor({{item}})), y = .data$pct/100, 
               fill = as.factor({{by}}), 
               label = paste0(round(.data$pct, 1), "%"))) +
    geom_col(position = barposition, width = barwidth) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  position = barposition,
                  outside = out,
                  place = percent_position,
                  fullheight = TRUE,
                  color = "white",
                  contrast = TRUE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_fill_manual(...) +
    labs(fill = legendtitle,
         caption = paste(question_text,
                         n_par(data = data, item = ensym(item), lang = lang))) +
    theme_sep() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom",
          plot.caption = element_text(color = "grey")) +
    guides(fill = guide_legend(keywidth = 0.4, keyheight = 0.8))
  
}

plot_groupbar_v(df, item = item, by = by, question = "Test question", lang = "EN", legendtitle = "Group")



# Simple call (no weights)

# With weights (pass the name of the weight column)
plot_groupbar_v(df, item = item, by = by, weights = w, question = "Test question", lang = "EN")