drawMetadataTable = function(names){
  names = paste(names, " ", sep = ":")
  
  # lineheight in inches
  lh = strheight("", units = "in") * 1.5
    
  # calculate plotting region coordinates in inches
  xlim = grconvertX(if(par()$xlog) 10^par()$usr[1:2] else par()$usr[1:2], "user", "in")
  ylim = grconvertY(if(par()$ylog) 10^par()$usr[3:4] else par()$usr[3:4], "user", "in")
  stopifnot( all.equal(c(xlim[2]-xlim[1], ylim[2]-ylim[1]), par()$pin) ) # make sure the calculations are correct
  
  offset = c(1, 1) # (x,y) offset in lineheight
  
  x = xlim[1] + offset[1] * lh
  y = ylim[1] + (offset[2] + rev(seq_along(names)-1)) * lh
  
  # metadata labels
  text(grconvertX(x, "in"), grconvertY(y, "in"), names, adj=c(0,0))
  
  # metadata values
  x = x + max(strwidth(names, units = "in"))
  text(grconvertX(x, "in"), grconvertY(y, "in"), "value", adj=c(0,0))
  
  invisible()
}

asTextNodeMy = function(node, id){
  ## this function could be integrated into a modified version of 'asTextNode'
  getTextAnchor = function (node){
    pos = xmlSApply(node, function(x) xmlAttrs(x)[c("x", "y")])
    attrs = as.list(pos[, which.min(pos["x",])])
    attrs$"text-anchor" = "start"
    attrs
  }
  asTextNode(node, "", replace = TRUE, addChildren = FALSE, .attrs = c(getTextAnchor(node), id = id) )
}


## group nodes by enclosing those which have the same parent into <g>...</g>
groupXMLNodes = function(nodes){
  p = sapply(nodes, xmlParent)
  up = unique(p)
  grouping = match(p, unique(p))
  sapply(seq_along(up), function(x) {
    children = nodes[grouping==x]
    removeNodes(children)
    newNode = newXMLNode("g", children, parent = up[x]) 
  })
  invisible()
}

imatplot = function(data, metadata = rownames(data), grouping = seq(nrow(data)), 
                    palette = c("rgb(255,128,0)", "rgb(255,0,0)", "rgb(255,0,128)", "rgb(255,0,255)", "rgb(128,0,255)", "rgb(0,0,255)", "rgb(0,128,255)", "rgb(0,255,255)"),
                    hlwd = 2, opacity = 1.0,
                    outdir = getwd(), file = NULL,
                    # passed to svg device
                    width = 7, height = 7, pointsize = 12, 
                    # passed to matplot
                    ...) { 
  
  ## assume data is a data.frame or matrix where each row corresponds to one line
  ## needs first to transpose the data frame for convenient plotting
  d = t(data)
  
  if(is.null(file)) # prototyping mode
    matplot(d, type="l", ...)
  else { # render int an SVG file and process afterwards
    if(is.vector(metadata))
      metadata = data.frame(Name = metadata)
    names = names(metadata)
    
    doc = svgPlot({
      matplot(d, type="l", ...)
      drawMetadataTable(names)
    }, width = width, height = height, pointsize = pointsize, addInfo = FALSE)
    svg = xmlRoot(doc)
    xmlAttrs(svg) <- list("font-size"=pointsize, "font-family"="Helvetica, Arial, FreeSans, Liberation Sans, Nimbus Sans L, sans-serif")
    #
    # highlight lines
    #
    lines = getMatplotSeries(doc)
    invisible(
      mapply(
        function(line, id) {
          actions = sprintf(c("toggleMetadata('show', %d)", "toggleMetadata('hide', %d)", "highlightLines(%d)"), id) 
          names(actions) = c("onmouseover", "onmouseout", "onclick")
          xmlAttrs(line) = c(class = "matplot.line", id = paste("matplot.line", id, sep="."), actions)
          convertCSSStylesToSVG(line)
        },
        lines,
        seq_along(lines)-1)
    )
    groupXMLNodes(lines)
    #
    ## get all horizontal textnodes
    #
    textNodes = getTextNodes(doc)
    textNodes = textNodes[sapply(textNodes, SVGAnnotation:::isHorizontalText)]
    ## and assume that the labels are the text nodes added last
    n = length(names)
    metaNodes = textNodes[seq(length.out = 2*n, to=length(textNodes))]
    ## ensure that the selected labels have expected length
    stopifnot( sapply(metaNodes, xmlSize) == c(nchar(paste(names, " ", sep = ":")), rep(5, n)) )
    #
    groupXMLNodes(metaNodes)
    #
    ids = paste(rep(c("meta.label", "meta.value"), each = n), seq(n)-1, sep=".")
    ## convert all glyph nodes to regular text nodes
    invisible(mapply(
      asTextNodeMy,
      metaNodes,
      ids
    ))
    #
    # process javascript file
    #
    ug = unique(grouping)
    groups = sapply(ug, function(x) which(grouping==x) - 1, USE.NAMES = FALSE)
    
    copySubstitute(src = system.file("javascript", "imatplot.js", package = "iSVGplots"), dest = outdir, recursive = TRUE,
                   symbolValues = sapply(
                     list( GROUPING  = sapply(grouping, function(x) which(ug==x) - 1, USE.NAMES = FALSE),
                           GROUPS    = groups,
                           METADATA  = metadata,
                           PALETTE   = rep(palette, length.out = max(sapply(groups, length))) ,
                           LINEWIDTH = hlwd,
                           OPACITY   = opacity),
                     toJSON, collapse = "")
    )
    
    addECMAScripts(doc, file.path(outdir, "imatplot.js"), insertJS = TRUE)
    #
    # SAVE the result
    #
    writeLines(saveXML(doc), file) # get SVG with line breaks
  }
  invisible()
}
