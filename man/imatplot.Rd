\name{imatplot}
\alias{imatplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create Interactive Line Plots
}
\description{
  Plots interactive SVG line charts.
}
\usage{
imatplot(data, metadata = rownames(data), grouping = seq(nrow(data)), outdir = getwd(), file = NULL,
         palette = c("rgb(255,128,0)", "rgb(255,0,0)", "rgb(255,0,128)", "rgb(255,0,255)", "rgb(128,0,255)", "rgb(0,0,255)", "rgb(0,128,255)", "rgb(0,255,255)"),
         hlwd = 2, opacity = 1.0,
         # passed to svg device
         width = 7, height = 7, pointsize = 12, 
         # passed to matplot
         ...)  
}
\arguments{
  \item{data}{
  \code{data.frame} containing data to be plotted, where each column corresponds to one line
}
  \item{metadata}{
  \code{data.frame} containing metadata associated with individual lines
}
  \item{grouping}{
  the grouping of the data
}
  \item{outdir}{
  output directory; dafaults to current working directory.
}
  \item{file}{
  a character string giving the output file name; if it is NULL, then no external file is created and the drawing occurs on screen.
}
  \item{palette}{
  colors used for highlighting groups.
}
  \item{hlwd}{
  highlighted line width.
}
  \item{opacity}{
  onmouseover line opacity.
}
  \item{width,}{
  the width and height of the graphics region in inches. The default values are 7.
}
  \item{height}{}
  \item{pointsize}{
  the point size to be used; defaults to 12.
}
  \item{...}{
  arguments passed to \code{matplot}.
}
}
\details{
  \code{imatplot} uses the \code{matplot} function for drawing. Plots are then garnished and exported to \code{SVG} using the \code{SVGAnnotation} package.
  
  Use Google Chrome browser for best viewing experience.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Andrzej Oles, andrzej.oles@embl.de
}
%\note{
%further notes
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%objects to See Also as \code{\link{help}},~
%}
\examples{
data(data)
df = as.data.frame(exprs(data))
metadata = fData(data)
points = c("tsc03PS_tu1", "tsc03PS_18w1r", "tsc03PS_6w1r", "tsc01PS_48h", "tsc03PS_6w2r", "tsc03PS_18w2r", "tsc03PS_tu2" )
samples = 1:2000 #dim(data)[1]

## this will be our input to the plotting function
d = df[samples, points] + 1e-7
m = metadata[samples, c("Gene.Symbol", "Entrez.Gene.ID")]
targets = m$Gene.Symbol

## do the plotting
imatplot(data      = d,
         metadata  = m,
         grouping  = targets,
         file = "imatplot.svg",
         hlwd = 3,
         pointsize = 10,
         # these are passed to matplot:
         lty="solid",
         col = c( "#a0a0a005", "#FF0000b0", "#3030FFb0", "#FF30FFb0" )[ (d[, "tsc03PS_18w2r"]>.01)*2 + (d[, "tsc03PS_18w1r"]>.01) + 1 ],
         main="TSC03", xlab="",
         ylab="log2 norm count",
         #xaxt="n",
         log="y" )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
