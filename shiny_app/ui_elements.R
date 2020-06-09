#------------------------------------------------------------------------------------------------------------------
# ui elements
#------------------------------------------------------------------------------------------------------------------

overall_design_options <- tags$head(
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Fira+Sans+Condensed');

                   h1 {
                   font-family: 'Calibri', bold;
                   font-weight: 500;
                   line-height: 1.1;
                   color: #aa1c7d;
                   }

                   h4 {
                   font-family: 'Calibri', bold;
                   font-weight: 500;
                   line-height: 1.1;
                   color: #3C5D70;
                   }

                   h5 {
                   font-family: 'Calibri', bold;
                   font-weight: 500;
                   line-height: 1.1;
                   color: #3C5D70;
                   }

                   .navbar-default {
                       color: #3C5D70;
                       background-color: #3C5D70;
                   }

                   body {
                       background-color: #FFFFFF;
                   }

                   .navbar-nav {
                        float: none !important;
                   }
                   .navbar-nav > li:nth-child(5) {
                        float: right;
                        right: 230px;
                   }
                   .navbar-nav > li:nth-child(6) {
                        float: right;
                   }
                   "))
)


metric_box <- function(title, value, value_text)
{
  wellPanel(style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
            h4(strong(title)),
            h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
            h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text))

}


methods_panel <- function(title, what_text, how_text, limit_text, style = "default")
{
  bsCollapsePanel(strong(title),
                  strong("What it measures:"),
                  br(),
                  p(what_text),
                  strong("How it was calculated:"),
                  br(),
                  p(how_text),
                  strong("Limitations:"),
                  br(),
                  p(limit_text),
                  style = style)
}
