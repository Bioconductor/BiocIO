## implement import/export
## Define CSVFile class

.CSVFile <- setClass("CSVFile", contains = "BiocFile")

#' @export
CSVFile <-
    function(resource)
{
    .CSVFile(resource = resource)
}

#' @export
setMethod("import", c(con = "CSVFile"),
    function(con, format, text, ...)
{
    read.csv(resource(con), ...)
})

#' @export
setMethod("export", c(object = "data.frame", con = "CSVFile"),
    function(object, con, format, ...)
{
    write.csv(object, resource(con), ...)
})

#' @export
setMethod("yield", "CSVFile",
    function(con, format, text, size = -1, ...)
{
    con <- resource(con)
    if (!is(con, "connection"))
        con <- file(con)
    if (!identical("opened", summary(con)$opened))
        tryCatch({
            open(con, "rt")
        }, error = function(e) {
            cond <- simpleCondition(e)
            class(cond) <- c("File not found", class(cond))
            signalCondition(cond)
        })
    tryCatch({
        read.csv(con, nrows = size, ...)
    }, error = function(e) {
        cond <- simpleCondition(e)
        class(cond) <- c("EOF", class(cond))
        signalCondition(cond)
    })
})
