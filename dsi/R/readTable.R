readTable =
    #
    # vv = readTable(o0321, colPos = c(0, 630, 780, 990, 2990,3375, Inf))
    #
# x - tesseract result. Data frame with top, right, ... and text
function(x, numCols = 4, colPos = computeCols(x, numCols), offset = 14, splitVar = 3)
{
    cols = split(x, cut(x$right, colPos))

    bin = grepl("^[0-9]+$", cols[[3]]$text)
#        desc = split(cols[[4]], cut(cols[[4]]$top, c(0, cols[[3]]$top - 14, Inf)))
#    g = cut(cols[[4]]$top, c(0, sort(unique(cols[[3]]$top[bin])) - offset, Inf))
    rows = lapply(cols, function(x) split(x, cut(x$top, c(0, sort(unique(cols[[ splitVar ]]$top)) - offset, Inf))))
    cells = lapply(rows, function(x) unname(sapply(x, function(x) paste(x$text, collapse = " " ))))
    as.data.frame(do.call(cbind, cells), stringsAsFactors = FALSE)
    #    desc = split(cols[[4]], g)
    
}

computeCols =
function(x, numCols = 4)
{
    p = isPrice(x$text) | grepl("^[0-9]+$", x$text)

    pos = sort(kmeans(x$right[p], numCols)$centers[,1])
}
