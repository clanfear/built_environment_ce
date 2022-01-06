

modelList = psem_original_data 
modelList = psem_new_data 
basis.set = NULL
direction = NULL
conserve = FALSE
conditioning = FALSE
.progressBar = TRUE


  if (is.null(basis.set)) {
    b <- basisSet(modelList, direction)
  } else b <- basis.set
  if (any(duplicated(names(b))) & conserve == FALSE & is.null(direction)) 
    dupOutput(b)
  if (length(b) == 0) {
    data.frame()
  } else {
    if (class(modelList) == "psem") 
      data <- modelList$data
    else data <- GetData(modelList)
    modelList <- piecewiseSEM:::removeData(modelList, formulas = 1)
    if (.progressBar == T & length(b) > 0) 
      pb <- txtProgressBar(min = 0, max = length(b), style = 3)
    ret <- do.call(rbind, lapply(1:length(b), function(i) piecewiseSEM:::testBasisSetElements(i, 
                                                                               b, modelList, data, conditioning, .progressBar, pb)))
    if (.progressBar == TRUE) 
      close(pb)
    rownames(ret) <- NULL
    if (conserve == TRUE) {
      ret <- do.call(rbind, lapply(unique(names(b)), function(i) {
        r <- ret[which(names(b) == i), ]
        r[which.min(r[, "P.Value"]), ]
      }))
    }
    return(ret)
  }

# testBasisSetElements <- function (i, b, modelList, data, conditioning, .progressBar, 
#           pb) 
# {

i <- 2
  formulaList <- lapply(piecewiseSEM:::listFormula(modelList, formulas = 1), 
                        piecewiseSEM:::all.vars_trans)
  bMod <- modelList[[which(sapply(formulaList, function(x) x[1] == 
                                    b[[i]][2]))]]
  if (any(class(bMod) %in% c("lmerMod", "merModLmerTest", 
                             "lmerModLmerTest", "glmerMod"))) {
    bNewMod <- suppressWarnings(update(bMod, formula(paste(". ~ ", 
                                                           paste(rev(b[[i]][-2]), collapse = " + "), " + ", 
                                                           piecewiseSEM:::onlyBars(formula(bMod)))), data = data))
  }
  else {
    bNewMod <- suppressWarnings(update(bMod, formula(paste(". ~ ", 
                                                           paste(rev(b[[i]][-2]), collapse = " + "))), 
                                       data = data))
  }
  ct <- unstdCoefs(bNewMod, data)
  ct$Test.Type <- ifelse(is.na(ct$Estimate) | grepl("=", 
                                                    ct$Predictor), "anova", "coef")
  ct <- ct[which(b[[i]][1] == ct$Predictor), , drop = FALSE]
  rhs <- paste0(b[[i]][-2], collapse = " + ")
  if (conditioning == FALSE) 
    rhs <- paste0(b[[i]][1], " + ...")
  ret <- data.frame(Independ.Claim = paste(b[[i]][2], "~", 
                                           rhs), ct[, c("Test.Type", "DF", "Crit.Value", 
                                                        "P.Value")])
  ret <- cbind(ret, piecewiseSEM:::isSig(ret[, "P.Value"]))
  names(ret)[ncol(ret)] <- ""
  if (.progressBar == TRUE) 
    setTxtProgressBar(pb, i)
  return(ret)
}
