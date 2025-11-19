#' Grouped vertical barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param by grouping variable
#' @param weights optional argument to weight output by survey weights
#' @param question optional argument to add question text in caption
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barpadding optional argument to adjust padding between bars
#' @param barposition optional argument to determine the positioning of the bars (default: "dodge")
#' @param legendtitle optional argument to define a legend title
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param ... further arguments of \code{scale_fill_manual} like \code{labels}
#'
#' @return a grouped vertical barplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggfittext
# @import pollster
#'
plot_groupbar_v <- function(data, item, by, weights, question,
                            lang = "DE", barpadding = 0.1, barposition = "dodge", 
                            legendtitle = "", textsize = 8, min_textsize = 5, ...){
  
  if(missing(barposition) | barposition == "dodge"){
    
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
    
    if(grepl("Fragetext: «", question) | grepl("Question text: «", question)){
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
