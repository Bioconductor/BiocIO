test_that("export_import", {
  .CSVFile <- setClass("CSVFile", contains = "BiocFile")

  ## Constructor
  CSVFile <-
      function(resource)
  {
      .CSVFile(resource = resource)
  }

  ## Define import
  setMethod("import", "CSVFile",
      function(con, format, text, ...)
  {
      read.csv(resource(con), ...)
  })

  ## Define export
  setMethod("export", c("data.frame", "CSVFile"),
      function(object, con, format, ...)
  {
      write.csv(object, resource(con), ...)
  })

  temp <- tempfile(fileext = ".csv")
  csv <- CSVFile(temp)

  export(mtcars, csv)
  df <- import(csv, row.names = 1)

  mtcars <- as.character(mtcars)
  df <- as.character(df)

  expect_equivalent(df, mtcars)
})
