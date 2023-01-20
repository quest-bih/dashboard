
make_datatable <- function(dataset)
{
  DT::datatable(data = dataset,
                extensions = 'Buttons',
                filter = 'top',
                options = list(dom = 'Blfrtip',
                               buttons =
                                 list(list(
                                   extend = "collection"
                                   , buttons = c("csv", "excel")
                                   , text = "Download"
                                 ) ),
                               orderClasses = TRUE,
                               pageLength = 20,
                               lengthMenu = list(c(10, 20, 50, 100, -1),
                                                 c(10, 20, 50, 100, "All"))
                ))
}

make_datatable_BSS <- function(dataset)
{
  DT::datatable(data = dataset,
                extensions = 'Buttons',
                filter = 'top',
                options = list(dom = 'Blfrtip',
                               buttons =
                                 list(list(
                                   extend = "collection"
                                   , buttons = c("csv", "excel")
                                   , text = "Download"
                                 ) ),
                               orderClasses = TRUE
                               ,
                               pageLength = 227
                               #,
                               # lengthMenu = list(c(-1),
                               #                   c("All"))
                ))
}
