`globalRecode` <-
function(x, breaks, labels){
  gr <- cut(x, breaks=breaks, labels=labels)
  invisible(gr)
}

