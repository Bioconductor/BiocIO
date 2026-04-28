# Utilities ---------------------------------------------------------------

isURL <- function(uri) {
    if (!isSingleString(uri))
        return(FALSE)
    windowsDriveLetter <- .Platform$OS.type == "windows" &&
        grepl("^[A-Za-z]:[/\\]", uri)
    grepl("^[A-Za-z]+:", uri) && !windowsDriveLetter
}

.parseURI <- function(uri) {
    if (!isURL(uri))
        return(list(scheme = "", path = uri))

    m <- regmatches(uri, regexec("^([A-Za-z]+)://([^/]*)(.*)", uri))[[1L]]

    if (identical(length(m), 4L)) {
        scheme <- m[2L]
        host <- m[3L]
        path <- m[4L]
        if (identical(scheme, "file"))
            return(list(scheme = scheme, path = paste0(host, path)))
        else
            return(list(scheme = scheme, host = host, path = path))
    }

    list(scheme = "", path = uri)
}

resourceDescription <- function(x) {
    resource <- resource(x)
    if (is(resource, "connection"))
        summary(resource)$description
    else
        resource
}

.ConnectionManager <-
    setRefClass("ConnectionManager", fields = c(connections = "list"))

manager <- function() .ConnectionManager()

connectionForResource <- function(manager, x, open = "") {
    resource <- decompress(manager, x)
    if (is.character(resource)) {
        if (!nzchar(resource))
            stop("path cannot be an empty string")
        uri <- .parseURI(resource)
        if (uri$scheme != "")
            con <- url(resource)
        else
            con <- file(resource)
    } else {
        con <- resource
    }
    if (!isOpen(con) && nzchar(open)) {
        open(con, open)
        con <- manage(manager, con)
    }
    con
}

connection <- function(manager, x, open = "") {
    connectionForResource(manager, resource(x), open = open)
}

# Connection management (similar to memory management) --------------------

manage <- function(manager, con) {
    manager$connections <- unique(c(manager$connections, list(con)))
    attr(con, "manager") <- manager
    con
}

managed <- function(manager, con) {
    con %in% manager$connections
}

unmanage <- function(manager, con) {
    manager$connections <- setdiff(manager$connections, con)
    attr(con, "manager") <- NULL
    con
}

release <- function(manager, con) {
    if (managed(manager, con)) {
        unmanage(manager, con)
        close(con)
    }
    con
}
