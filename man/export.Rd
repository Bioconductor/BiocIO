\name{io}

\alias{export}
\alias{export,ANY,connection,character-method}
\alias{export,ANY,connection,missing-method}
\alias{export,ANY,CompressedFile,missing-method}
\alias{export,ANY,missing,character-method}
\alias{export,ANY,character,missing-method}
\alias{export,ANY,character,character-method}

\alias{import}
\alias{import,connection,character,ANY-method}
\alias{import,connection,missing,ANY-method}
\alias{import,CompressedFile,missing,ANY-method}
\alias{import,character,missing,ANY-method}
\alias{import,character,character,ANY-method}
\alias{import,missing,ANY,character-method}

\alias{yield}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Import and export  }
\description{
  The functions \code{import} and \code{export} load and save
  objects from and to particular file formats. The rtracklayer package
  implements support for a number of annotation and sequence formats.
}
\usage{
export(object, con, format, ...)
import(con, format, text, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ The object to export. }
  \item{con}{ The connection from which data is loaded or to which data
    is saved. If this is a character vector, it is assumed to be a
    filename and a corresponding file connection is created and then
    closed after exporting the object. If a \code{\linkS4class{BiocFile}}
    derivative, the data is loaded from or saved to the underlying
    resource. If missing, the function will return the output as a
    character vector, rather than writing to a connection.
  }
  \item{format}{ The format of the output. If missing and \code{con} is
    a filename, the format is derived from the file extension. This
    argument is unnecessary when \code{con} is a derivative of
    \code{\linkS4class{BiocFile}}.
  }
  \item{text}{ If \code{con} is missing, this can be a character vector
    directly providing the string data to import. }
  \item{\dots}{ Parameters to pass to the format-specific method.
  }
}
\value{
  If \code{con} is missing, a character vector containing the string
  output. Otherwise, nothing is returned.
}
\author{ Michael Lawrence }

\seealso{
  Format-specific options for the popular formats:
  \acronym{\link[rtracklayer:GFFFile]{GFF}},
  \acronym{\link[rtracklayer:BEDFile]{BED}},
  \acronym{\link[rtracklayer:BED15File]{Bed15}},
  \acronym{\link[rtracklayer:BEDGraphFile]{bedGraph}},
  \acronym{\link[rtracklayer:WIGFile]{WIG}},
  \acronym{\link[rtracklayer:BigWigFile]{BigWig}}
}

\examples{
  ## To illustrate export(), import(), and yeild(), we create a class, CSVFILE
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

  ## Define yield
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

  ## Usage
  temp <- tempfile(fileext = ".csv")
  csv <- CSVFile(temp)

  export(mtcars, csv)
  df <- import(csv)

  yield(csv)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
