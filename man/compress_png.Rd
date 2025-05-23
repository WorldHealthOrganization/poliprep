% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compress_png.R
\name{compress_png}
\alias{compress_png}
\title{Compress PNG Files in a Directory or a Single PNG File with pngquant}
\usage{
compress_png(path, png_overwrite = TRUE, speed = 1, verbosity = TRUE)
}
\arguments{
\item{path}{A string specifying either the path to a single PNG file or
a directory containing PNG files.}

\item{png_overwrite}{Logical. If TRUE, will overwrite existing files.
Default is TRUE}

\item{speed}{Integer. Speed/quality trade-off from 1 (brute-force) to 10
(fastest). Default is 3. Speed 10 has 5\% lower quality but is 8 times
faster.}

\item{verbosity}{Logical. Controls the amount of information displayed.
FALSE = minimal, TRUE = detailed. Default is TRUE.}
}
\value{
For single files, returns a list with compression statistics.
For directories, returns a data frame with statistics for all files.
The function also works by side effect, compressing PNG files.
}
\description{
This function compresses either a single PNG file or all PNG files in a
specified directory using pngquant optimization to reduce file size while
maintaining visual quality. pngquant is a lossy compression tool that can
reduce file sizes by up to 70\% while preserving full alpha transparency.
}
\examples{
# Compress all PNG files in a directory
# compress_png("path/to/your/folder")

# Compress a single PNG file
# compress_png("path/to/your/image.png")

# Compress with automatic installation if pngquant is not found
# compress_png("path/to/your/folder", auto_install = TRUE)

# Compress and overwrite existing files
# compress_png("path/to/your/folder", force = TRUE)

}
