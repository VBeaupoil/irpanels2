#' Simple horizontal barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param weights optional argument to weight output by survey weights
#' @param question optional argument to add question text in caption
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barcolor optional argument to set the color of the bar
#' @param barwidth optional argument to define the width of the bar
#' @param texthjust optional argument to adjust the text's horizontal alignment
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#'
#'
#' @return a horizontal barplot
#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @import forcats
#' @import ggfittext
# @import pollster
#'
plot_bar_h <- function(data, item, weights, question,
                       lang = "DE", barcolor = "#1F407A", barwidth = 0.8, 
                       texthjust = 2, textsize = 8, min_textsize = 5){
  
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
    filter({{item}} > -1) %>% 
    topline(variable = {{item}}, weight = weight) %>%
    ggplot(aes(x = Response, y = `Valid Percent`/100, 
               label = paste0(round(`Valid Percent`, 1), "%"))) +
    geom_col(fill = barcolor, width = barwidth) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  padding.x = grid::unit(texthjust, "mm"),
                  outside = TRUE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(caption = paste(question_text,
                         n_par(data = data, item = ensym(item), lang = lang))) +
    coord_flip(clip = "off") +
    theme_sep() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          plot.caption = element_text(color = "grey"))

}
