standardize <- function(input, scale=1){
  x <- as.numeric(input)
  return((x- mean(x, na.rm=T))/(scale*sd(x, na.rm=T)))
}

valcheck <- function(df){
  numerics <- df[,sapply(df, is.numeric)]
  sapply(numerics, function(x){
    x_no_na <- x[!is.na(x)]
    c("min"     = min(x_no_na),
      "mean"    = mean(x_no_na),
      "median"  = median(x_no_na),
      "max"     = max(x_no_na),
      "sd"      = sd(x_no_na),
      "n_missing" = sum(is.na(x)),
      "pr_missing" = sum(is.na(x))/length(x))
  })
}


`%!in%` <- Negate(`%in%`)

x_notin_y <- function(x, y){
  setNames(list(
  unique(x)[unique(x) %!in% unique(y)],
  unique(y)[unique(y) %!in% unique(x)]
  ), 
  c(paste0(deparse1(substitute(x)), " not in ", deparse1(substitute(y))),
    paste0(deparse1(substitute(y)), " not in ", deparse1(substitute(x)))
    )
  )
}

x_in_y <- function(x, y){
  setNames(list(
    unique(x)[unique(x) %in% unique(y)],
    unique(y)[unique(y) %in% unique(x)]
  ), 
  c(paste0(deparse1(substitute(x)), " in ", deparse1(substitute(y))),
    paste0(deparse1(substitute(y)), " in ", deparse1(substitute(x)))
  )
  )
}

extract_EB_res <- function(model, varname, group = "TRACT"){
  eb_res <- HLMdiag::HLMresid(model, group, type="EB", standardize=F) %>% 
    tibble::rownames_to_column(var=group)
  names(eb_res)[2] <- varname
  return(eb_res)
}

list_missing <- function(x){
  missings <- lapply(x, function(x) sum(is.na(x)))
  data.frame(missing = unlist(missings)) %>% arrange(desc(missing))
}

# Not convinced this is the right reliability calculation
lme_reliability <- function(x){
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  n <- table(insight::get_random(x))
  J <- length(n) # number of neighbs
  return(sum(t00 / (t00 + s2 / n)) / J)
}

# Not convinced this is the right reliability calculation
lme_reliability_3lvl <- function(x){
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[["NC_ID"]] #t00 is between group variance
  s2  <- var_components$var.intercept[["RESP_ID:NC_ID"]] # s2 is within-group variance
  unique_ids <- insight::get_random(x) %>% distinct(RESP_ID, NC_ID)
  n <- table(unique_ids$NC_ID) # n is neighb sample size
  J <- length(n) # number of neighbs
  return(sum(t00 / (t00 + s2 / n)) / J)
}



ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

invert <- function (x) # This is from searchable package which is deprecated
{
  if (is.null(names(x))) 
    stop("vector does not have names.")
  v <- names(x)
  names(v) <- as.character(x)
  return(v)
}

str_detect_all <- function(x, patterns){
  # Look across a vector of characters, checking that all elements of a vector
  # of patterns exist inside at least one element.
  any(apply(sapply(patterns, function(pattern){str_detect(x, pattern)}), 1, all))
}
