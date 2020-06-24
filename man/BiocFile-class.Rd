\name{BiocFile-class}
\docType{class}

%% Classes:
\alias{class:BiocFile}
\alias{BiocFile-class}
\alias{class:CompressedFile}
\alias{CompressedFile-class}
\alias{BiocFileList-class}

%% Accessors
\alias{path}
\alias{path,BiocFile-method}
\alias{resource}
\alias{resource<-}
\alias{fileFormat}
\alias{fileFormat,BiocFile-method}
\alias{fileFormat,CompressedFile-method}
\alias{fileFormat,character-method}

%% Consturctors
\alias{BiocFileList}

%% Show:
\alias{show,BiocFile-method}

%% Related functions:
\alias{FileForFormat}
\alias{bestFileFormat}
\alias{bestFileFormat,GRangesList-method}
\alias{bestFileFormat,GenomicRanges-method}
\alias{bestFileFormat,IntegerRangesList-method}
\alias{bestFileFormat,RleList-method}
\alias{decompress}
\alias{decompress,ANY-method}
\alias{decompress,CompressedFile-method}
\alias{decompress,GZFile-method}
\alias{decompress,character-method}

%% Coerce:
\alias{as.character,BiocFile-method}

\title{BiocFile objects}

\description{
  A \code{BiocFile} object is the base class for classes representing
  files accessible with rtracklayer. It wraps a resource (either a path,
  URL or connection). We can represent a list of \code{BiocFile} objects
  with a \code{BiocFileList}.
}

\section{Accessor Methods}{
  In the code snippets below, \code{x} represents a \code{BiocFile}
  object.
  
  \describe{
    \item{}{
      \code{path(x)}:
      Gets the path, as a \code{character} vector, to the resource
      represented by the \code{BiocFile} object, if possible.
    }
    \item{}{
      \code{resource(x)}:
      Gets the low-level resource, either a character vector (a path or
      URL) or a connection.
    }
    \item{}{
      \code{fileFormat(x)}: Gets a string identifying the file
      format. Can also be called directly on a character file path, in
      which case it uses a heuristic based on the file extension.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as.character(x)}:
      Returns the path of the file as a character vector.
    }
  }
}

\section{Related functions}{
  \describe{
    \item{}{
      \code{FileForFormat(path, format = file_ext(path))}:
      Determines the file type of \code{path} and returns
      a high-level file object such as BamFile, BEDFile,
      BigWigFile etc.. 
    }
  }
}

\author{Michael Lawrence}

\seealso{
  Implementing classes include: \code{\link[rtracklayer]{BigWigFile}},
  \code{\link[rtracklayer]{TwoBitFile}}, \code{\link[rtracklayer]{BEDFile}},
  \code{\link[rtracklayer]{GFFFile}}, and \code{\link[rtracklayer]{WIGFile}}.
}

\keyword{methods}
\keyword{classes}
