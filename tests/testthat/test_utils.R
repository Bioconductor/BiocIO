test_that("Test .parseURI", {
    uri <- 'http://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "http", host = "foo.com", path = "/bar.txt")
    )

    uri <- 'http://www.example.com/foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "http", host = "www.example.com", path = "/foo/bar.txt")
    )

    uri <- 'file:////foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "file", path = "//foo/bar.txt"))

    uri <- 'file:///foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(parsed, list(scheme = "file", path = "/foo/bar.txt"))

    uri <- 'https://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "https", host = "foo.com", path = "/bar.txt")
    )

    uri <- 'ftp://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "ftp", host = "foo.com", path = "/bar.txt")
    )

    uri <- 'smtp://foo.com/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "smtp", host = "foo.com", path = "/bar.txt")
    )

    uri <- 'file://C:/foo/bar.txt'
    parsed <- .parseURI(uri)
    expect_equal(
        parsed,
        list(scheme = "file", path = "C:/foo/bar.txt")
    )
})
