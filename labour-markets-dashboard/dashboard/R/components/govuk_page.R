# ------------------------------------------------------------
# 6) Blank Page Skeleton
# ------------------------------------------------------------ 

govuk_page <- function(title) {
  tags$div(
    class = "govuk-width-container",
    tags$main(
      class = "govuk-main-wrapper",
      id = "main-content",
      tags$h1(class = "govuk-heading-xl", title)
    )
  )
}