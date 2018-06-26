#' Interface to \code{gs} to extract a range of pages from a PDF file
#'
#' Extract a range of pages from a PDF file. Preserves hyperlinks that fall
#' within the selected page range.
#'
#' @note This function is based on Ghostscript.
#'
#' @param fileName character string denoting the path/ name of the input PDF
#'   file
#'
#' @param firstP integer denoting the first extracted page
#'
#' @param lastP integer denoting the last extracted page
#'
#' @param outFileName character string denoting the name of the output PDF file.
#'   If not specified, it is
#'   "[\code{fileName}]_pages[\code{firstP}]_[\code{lastP}].pdf".
#'
#' @return Returns nothing, but as a side effect saves the extracted pages as a
#'   new .pdf file.
#'
#' @seealso \pkg{GPL Ghostscript} \url{https://www.ghostscript.com}
#'
#' @export
#'
#' @examples
#'
#' library(d3heatmap)
#' p <- d3heatmap(mtcars, scale = "column", colors = "Blues")
#' # pdf contains three pages (1 plot, 2 empty ones)
#' jsGraphic2Pdf(p, c(2500, 2000, 50, 50))
#' pdfPageExtract("JSgraphic.pdf", 1, 1)
#' pdfPageExtract("JSgraphic.pdf", 1, 1, "JSgraphic.pdf")
#'
#' file.remove("JSgraphic.pdf", "JSgraphic_pages1_1.pdf", "JSgraphic_extr.pdf")
#'
#' @author Christoph Schmidt <schmidtchristoph@@users.noreply.github.com>

# 23.03.17

pdfPageExtract <- function(fileName, firstP = NULL, lastP = NULL, outFileName = NULL){
   if(!file.exists(fileName)){ stop("The file corresponding to input argument 'fileName' does not exist.") }

   if( !stringr::str_detect(fileName, ".pdf") ){ stop("Input argument 'fileName' does not refer to a PDF file.") }



   if(is.null(outFileName)){
      ind          <- stringr::str_locate_all(fileName, stringr::fixed("."))
      nrMatch      <- length(ind)
      ind2         <- ind[[nrMatch]][1, 1]
      outFileName  <- paste(stringr::str_sub(fileName, 1L, ind2-1), "_pages", firstP, "_", lastP, ".pdf", sep="")
   }
   else {
      if( !stringr::str_detect(outFileName, ".pdf") ){
         outFileName <- paste(outFileName, ".pdf", sep="")
      }

      if(identical(fileName, outFileName)){ # overwriting the input fileName does not work, so appending _extr
         ind          <- stringr::str_locate_all(fileName, stringr::fixed("."))
         nrMatch      <- length(ind)
         ind2         <- ind[[nrMatch]][1, 1]
         outFileName  <- paste(stringr::str_sub(outFileName, 1L, ind2-1), "_extr.pdf", sep="")
      }
   }

   command <- c("-sDEVICE=pdfwrite", "-dNOPAUSE", "-dBATCH", "-dSAFER",
                paste("-dFirstPage=", firstP, sep=""), paste("-dLastPage=", lastP, sep=""),
                paste("-sOutputFile=", outFileName, sep=""), fileName)

   system2("gs", args = command, stdout = FALSE, stderr = FALSE)
}
