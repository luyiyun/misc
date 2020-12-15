#' Organize tibble for output using Officer package
#'
#' @param tab tibble, output.
#'
#' @return Organized tibble viewed in Viewer window.
#' @export
#'
#' @import flextable
#'
#' @examples
#' read_docx() %>% body_add_flextable(format_tab(iris)) %>% print(target="./res.docx")
format_tab <- function(tab) {
  tab %>% flextable %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Times", part = "all") %>%
    bold(part = "header") %>%
    autofit(add_w = -0.1)
}

