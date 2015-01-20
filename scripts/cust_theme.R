#' Custom ggplot2 theme


cust_theme <- function() {
  res <- theme(
    text                = element_text(size = 12,family = 'serif', color = "#1E1E1E"),
    axis.line           = element_line(colour = '#1E1E1E'),
    axis.text           = element_text(color = '#1E1E1E'),
    panel.background    = element_rect(fill = 'white'),
    panel.grid.major.y  = element_blank(), 
    panel.grid.major.x  = element_blank(), 
    panel.grid.minor.y  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    strip.text.y        = element_text(angle = 0, hjust = 0.1),
    strip.background    = element_blank(),
    legend.key          = element_rect(fill = NA),
    legend.position     = 'none'
  )
  res
}
