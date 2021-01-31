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




#' convert the numeric vector and matrix to rounded strings
#'
#' @param x
#' @param digit
#' @param
#' @param nozero_pre
#'
#' @return
#'
#' @examples
.round_num <- function(x, digits = 2, zero_pre = "<", nonzero_pre = "", add_pre = FALSE) {
  # sprintf采取的是4舍6入5双留，即0.005并不会进到0.01，但0.0050001会被进到0.01
  if (add_pre) {
    zeros <- paste0("0.", paste0(rep("0", digits), collapse = ""))
    zeros_str <- paste0(zero_pre, zeros)
  }
  # attrs <- attributes(x)  # 使得我们可以保持matrix
  res <- sprintf(paste0("%.", digits, "f"), unclass(x))
  if (add_pre) {
    res <- ifelse(
      res == zeros,
      zeros_str,
      paste0(nonzero_pre, res)
    )
  }
  # attributes(res) <- attrs
  return(res)
}
