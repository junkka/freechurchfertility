#' Diff in days
#'
#' @param x end date
#' @param y start date


diff_days <- function(x, y) {
  as.numeric(difftime(x, y, units="days"))
}

#' Format regression results table
#'
#' @param x data.frame

format_rt <- function(x) {
  # Create index
  x$index     <- seq_len(nrow(x))
  # add levels
  x$levs      <- paste('- ', x$levs)
  # Create data.frame for variable names
  vars        <- x[c(1:length(unique(x$var))), ]
  # Add uniwue variable names as levs
  vars$levs   <- unique(x$var)

  vars[ ,!(names(x) %in% c('levs'))] <- NA
  # add index, get lowest rowname of 
  indexs       <- x %>% group_by(var) %>% summarise(index = min(index))
  vars$index  <- indexs$index
  x$index     <- x$index + 0.1
  ret         <- rbind(x, vars)
  ret         <- ret[order(ret$index),c(2:(ncol(ret)-1))]
  ret[is.na(ret)] <- ''
  return(ret)
}

#' Make comment
#'
#' Make a comment for xtable
#'
#' @param x data.frame containing regression table
#' @param note character comment


comment <- function(x, note = "\\hline\n  \\emph{Note: * p$<$0.1, ** p$<$0.05} \\\\ \n") {
  comment <- list()
  comment$pos <- list()
  comment$pos[[1]] <- c(nrow(x))
  comment$command <- c(paste(note, sep=""))
  return(comment)
}

#' Summary statistic table
#'
#' Transforms summary stats to data.frame
#'

sum_stat_tbl <- function(x) {
  sums <- attr(x, 'summary')
  res <- ldply(sums, function(y) {
    y
  })
  res <- res %>% filter(var %in% c('Events', 'AIC', 'Likelihood ratio test', 'Wald test')) %>% 
    mutate(value = ifelse(var == 'Wald test', p, value), value = round(value, 3)) %>% 
    select(.id, var, value) %>% 
    spread(.id, value)
  res$levs <- res$var
  res$var <- "Summary"
  res$levs <- ordered(
    factor(res$levs), 
    levels = c("Events", "AIC","Likelihood ratio test", "Wald test"))
  res <- res %>% arrange(levs)
  return(res)
  
}


#' Regression results table
#'
#' Creates a regression results table from nested coxph objects
#'
#' @param ... coxph object/s
#' @param models character vector specifing name of objects to return
#' @param cilevel numeric confidence interval


res_table <- function(..., models = NULL, cilevel = 0.9) {
  resl <- coxph_to_long(..., cilevel = cilevel)
  if (!is.null(models))
    resl <- resl %>% filter(model %in% models)

  rest <- resl %>% 
    mutate(
      beta = ifelse(
        is.na(signif), 
        '1 (ref.)', 
        ifelse(
          p <= 0.05,
          paste(round(haz, 3), '**'),
          ifelse(
            p <=0.1,
            paste(round(haz, 3), '*'),
            paste(round(haz, 3))
          )
        )
      )
    ) %>% 
    select(model, var, levs, beta) %>% 
    tidyr::spread(model, beta) %>% 
    mutate(levs = ordered(levs, levels = rev(levs))) %>% 
    arrange(var, levs)
  # remove all var dublicates
  rest <- rbind(rest, sum_stat_tbl(resl))
  return(rest)
}

#' Transform coxph objects to long format table
#'
#' For nested models
#'
#' @param ... coxph objects or list of objects
#' @param cilevel confidence interval


coxph_to_long <- function(..., cilevel=0.95) {
  x <- list(...)
  if (any(as.logical(lapply(x, inherits, "list")))) {
    if (!inherits(x[[1]], 'list'))
      stop('No coxph or list of coxph')
    x <- x[[1]]
  } else  {
    names(x) <- obj_names(...)
  }

  dat <- ldply(x, .fun=function(y) coxph_df(y, cilevel))
  colnames(dat)[1] <- 'model'

  # get diff between unique model var and all var by model
  model_diff <- ddply(dat, .(model), .fun = function(a){
    var_names <- as.character(levels(dat$var))
    res <- length(levels(dat$var)) - sum(unique(as.character(a$var)) %in% var_names)
  })
  
  # get var where model = model with min V1
  full_model  <- model_diff$model[model_diff$V1 == min(model_diff$V1)][1]
  nm          <- unique(as.character(dat$levs[dat$model == full_model]))
  # Order levs by largest model
  dat$levs    <- ordered(dat$levs, levels=rev(nm))

  # Add summary stat by model
  attr(dat, "summary") <- llply(x, .fun = coxph_sum)
  class(dat)           <- c("cox_tb", class(dat))

  return(dat)
}

obj_names <- function(...) {
   x <- deparse(substitute(list(...)))
   x <- str_replace(str_replace(x, '^list\\(', ''), '\\)', '')
   x <- str_trim(unlist(str_split(x, ',')))
   return(x)
}

coxph_df <- function(x, cilevel = 0.9) {
  # by assign get xlevels
  ldply(names(x$assign), .fun = function(var) {

    s       <- summary(x)$coefficients
    beta    <- exp(coef(x))
    pvalue  <- s[ ,ncol(s)]
    loci    <- exp(confint(x, level = cilevel))[ ,1]
    hici    <- exp(confint(x, level = cilevel))[ ,2]
    n_coef  <- x$assign[[var]]

    if (str_detect(var, ':')){
      # is interaction
      vars <- str_split(var, ':')[[1]]
      lev1 <- x$xlevels[[vars[1]]]
      lev1 <- lev1[2:length(lev1)]
      lev2 <- x$xlevels[[vars[2]]]
      levs <- unlist(lapply(lev1, function(a) paste(a, lev2, sep = ' * ')))
    } else {
      levs <- x$xlevels[[var]]
      if (is.null(levs)) 
        levs <- var
    }
    if (length(levs) > 1) {
      data.frame(
        var     = rep(var, length(levs)),
        levs    = levs,
        haz     = c(1, beta[n_coef]),
        p       = c(NA, pvalue[n_coef]),
        ci_low  = c(NA, loci[n_coef]),
        ci_high = c(NA, hici[n_coef]),
        signif  = ifelse(c(NA, pvalue[n_coef]) < (1 - cilevel), TRUE, FALSE)
      )
    } else {
      data.frame(
        var     = rep(var, length(levs)),
        levs    = levs,
        haz     = beta[n_coef],
        p       = pvalue[n_coef],
        ci_low  = loci[n_coef],
        ci_high = hici[n_coef],
        signif  = ifelse(pvalue[n_coef] < (1 - cilevel), TRUE, FALSE)
      )
    }
  })
}

#' Summary stats for coxph object
#'
#' @param x coxph object



coxph_sum <- function(x) {
  coef       <- x$coef
  df         <- ifelse(is.null(x$df), sum(!is.na(coef)), round(sum(x$df),2)) 
  getp <- function(x) 1 - pchisq(x, df)
  data.frame(
    var = c("Events", "Observations", "AIC", "Likelihood ratio test", "Logrank score", "Wald test"),
    value = c(
      as.numeric(x$nevent),
      as.numeric(x$n),
      extractAIC(x)[2],
      (-2 * (x$loglik[1] - x$loglik[2])),
      as.numeric(x$score),
      as.numeric(x$wald.test)
      ),
    p = c(rep(NA, 3), getp(-2 * (x$loglik[1] - x$loglik[2])), getp(x$score), getp(x$wald))
  )
}