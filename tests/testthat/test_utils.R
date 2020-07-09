test_that("Test .parseURI", {
    uri <- 'http://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "http", path = "/bar.txt"))
    
    uri <- 'file:////foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "file", path = "//foo/bar.txt"))

    uri <- 'file:///foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "file", path = "/foo/bar.txt"))

    uri <- 'https://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "https", path = "/bar.txt"))

    uri <- 'ftp://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "ftp", path = "/bar.txt"))

    uri <- 'smtp://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "smtp", path = "/bar.txt"))

    uri <- 'file://C:/foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "file", path = "/C:/foo/bar.txt"))
})
