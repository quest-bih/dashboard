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


methods_panel <- function(title, what_text, how_text, limit_text, style = "default")
{
  bsCollapsePanel(title, # JT test remove strong strong(title), in order for updateCollapse() to work
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
