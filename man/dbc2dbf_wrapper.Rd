% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbc2dbf.R
\name{dbc2dbf_wrapper}
\alias{dbc2dbf_wrapper}
\title{Decompress a DBC (compressed DBF) file}
\source{
The internal C code for \code{dbc2dbf} is based on \code{blast} decompressor and \code{blast-dbf} (see \emph{References}).
}
\usage{
dbc2dbf_wrapper(input.file, output.file)
}
\arguments{
\item{input.file}{The name of the DBC file (including extension)}

\item{output.file}{The output file name (including extension)}
}
\value{
Return TRUE if succeded, FALSE otherwise.
}
\description{
This function allows you decompress a DBC file into its DBF counterpart. Please note that this is the file format used by the Brazilian Ministry of Health (DATASUS), and it is not related to the FoxPro or CANdb DBC file formats.
}
\details{
DBC is the extension for compressed DBF files (from the 'XBASE' family of databases). This is a proprietary file format used by the brazilian government to make available public healthcare datasets (by it's agency called DATASUS).

It uses internally the PKWare's Data Compression Library (DCL) "implode" compression algorithm. When decompressed, it becomes a regular DBF file.
}
\examples{
\dontrun{
# Input file name
in.f <- system.file("files/sids.dbc", package = "read.dbc")

# Output file name
out.f <- tempfile(fileext = ".dbc")

# The call return logi = TRUE on success
if (dbc2dbf(input.file = in.f, output.file = out.f)) {
  print("File decompressed!")
  file.remove(out.f)
}
}

}
\references{
The PKWare ZIP file format documentation (contains the "implode" algorithm specification) available at \url{https://support.pkware.com}, current version \url{https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT}.

\code{blast} source code in C: \url{https://github.com/madler/zlib/tree/master/contrib/blast}

\code{blast-dbf}, DBC to DBF command-line decompression tool: \url{https://github.com/eaglebh/blast-dbf}
}
\seealso{
\code{read.dbc}
}
\author{
Daniela Petruzalek, \email{daniela.petruzalek@gmail.com}
}
\keyword{dbc}
\keyword{dbf}
