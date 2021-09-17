#' Title
#'
#' @param yourdf
#' @param var_name
#' @param var_value
#'
#' @return
#' @export
#'
#' @examples
ggpetalplot <- function(yourdf, var_name, var_value) {
  varnum <- nrow(yourdf)
  x <- 1:(180 * varnum)
  y <- sin(x * pi / 180)


  mydf <- data.frame(
    var.x = x,
    var.y = abs(y),
    var = gl(varnum,
      180,
      labels = yourdf %>% pull(var_name)
    )
  ) %>%
    merge(yourdf, by.x = "var", by.y = var_name) %>%
    mutate(new_y = pull(., var.y) * pull(., var_value))


  p1 <- ggplot(data = mydf, aes(x = var.x, y = new_y)) +
    geom_area(aes(fill = var), show.legend = F) +
    coord_polar() +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_x_continuous(
      breaks = seq(90, 180 * varnum, 180),
      labels = yourdf %>% pull(var_name)
    ) +
    geom_text(data = yourdf, aes(
      x = seq(90, 180 * varnum, 180),
      y = (yourdf %>% pull(var_value)) + 1,
      label = yourdf %>% pull(var_value)
    )) +
    scale_fill_material_d()
  return(p1)
}
