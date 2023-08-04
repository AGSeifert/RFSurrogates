#' Mutual Forest Impact (Corrected Mean Adjusted Agreement).
#'
#' This method corrects the mean adjusted agreement by a permutation approach
#' and generates the relation parameter mutual forest impact.
#' Subsequently p-values are determined and related variables are selected.
#'
#' @inheritParams RandomForestSurrogates
#' @inheritDotParams RandomForestSurrogates
#' @param variables Vector of variable names for **which related variables should be searched**.
#' @param candidates Vector of variable names that **are candidates to be related to the variables**.
#'
#' @returns A [`MutualForestImpact`] list object.
#'  * `REL`: The [`MeanAdjustedAgreement`] object.
#'  * `PERM`: The permutated [`MeanAdjustedAgreement`] object.
#'  * `relations`: Matrix of determined relations (rows: investigated variables, columns: candidate variables).
#'
#' @examples
#' \donttest {
#' data("SMD_example_data")
#' mfi <- MFI(
#'   x = SMD_example_data[, -1], y = SMD_example_data[, 1],
#'   s = 10, num.trees = 50, num.threads = 1,
#'   variables = c("X7", "X1"), candidates = colnames(SMD_example_data)[2:101]
#' )
#' }
#'
#' @seealso [MutualForestImpact()]
#' @keywords varrel shortcut
#' @export
MFI <- function(
  variables,
  candidates,
  num.threads = 1,
  ...
) {
  rels <- lapply(list(norm = FALSE, perm = TRUE), function(permutate) {
    MeanAdjustedAgreement(
      RandomForestSurrogates(
        num.threads = num.threads,
        permutate = permutate,
        ...
      ),
      # Relations are not calculated, leaving `t` unused.
      related = FALSE, t = NA_integer_,
      num.threads = num.threads
    )
  })

  MutualForestImpact(
    rels$norm,
    rels$perm,
    variables = variables,
    candidates = candidates
  )
}

#' MutualForestImpact.
#'
#' @param REL A [`MeanAdjustedAgreement`] object.
#' @param PERM A permutated [`MeanAdjustedAgreement`] object.
#' @param variables Vector of variable names for **which related variables should be searched**.
#' @param candidates Vector of variable names that **are candidates to be related to the variables**.
#'
#' @returns A [`MutualForestImpact`] list object.
#'  * `REL`: The [`MeanAdjustedAgreement`] object.
#'  * `PERM`: The permutated [`MeanAdjustedAgreement`] object.
#'  * `relations`: Matrix of determined relations (rows: investigated variables, columns: candidate variables).
#'
#' @seealso [MFI()]
#' @keywords varrel
#' @export
MutualForestImpact <- function(
  REL,
  PERM,
  variables,
  candidates
) {
  if (!inherits(REL, "MeanAdjustedAgreement")) {
    stop("`REL` must be a `MeanAdjustedAgreement` object.")
  }

  if (!inherits(PERM, "MeanAdjustedAgreement")) {
    stop("`PERM` must be a `MeanAdjustedAgreement` object.")
  }

  if (!all(
    identical(REL$RFS$ranger$forest$independent.variable.names, colnames(REL$relations)),
    identical(REL$RFS$ranger$forest$independent.variable.names, rownames(REL$relations)),
    identical(paste(REL$RFS$ranger$forest$independent.variable.names, "perm", sep = "_"), PERM$RFS$ranger$forest$independent.variable.names),
    identical(PERM$RFS$ranger$forest$independent.variable.names, colnames(PERM$relations)),
    identical(PERM$RFS$ranger$forest$independent.variable.names, rownames(PERM$relations))
  )) {
    stop("`REL`, `PERM` variable relations must have been created using all variables of `x`.")
  }

  nvar <- REL$RFS$ranger$num.independent.variables

  adj.agree.corr <- adj_agree_corr(
    nvar = REL$RFS$ranger$num.independent.variables,
    rel_surr = REL$relations,
    perm_surr = PERM$relations
  )

  diag(adj.agree.corr) <- NA
  adj.agree.corr.var <- adj.agree.corr[variables, candidates]

  result <- list(
    REL = REL,
    PERM = PERM,
    relations = adj.agree.corr.var
  )
  class(result) <- "MutualForestImpact"

  return(result)
}

#' @keywords internal
adj_agree_corr <- function(
  nvar,
  rel_surr, # rel$surr.res
  perm_surr # perm$surr.res
) {
  adj.agree <- rel_surr
  adj.agree.perm <- perm_surr

  diag(adj.agree) <- diag(adj.agree.perm) <- 1

  if (anyNA(adj.agree)) {
    no.na <- length(which(rowSums(is.na(adj.agree)) != 0))
    warning(paste0("Relations for ", no.na, " original variables were not calculated because they were never used as a primary split.
            Affected relations are set to 0. "))
    adj.agree[which(is.na(adj.agree))] <- 0
  }

  if (anyNA(adj.agree.perm)) {
    no.na <- length(which(rowSums(is.na(adj.agree.perm)) != 0))
    warning(paste0("Relations for ", no.na, " permuted variables were not calculated because they were not used as a primary split.
            Affected relations are set to 0. "))
    adj.agree.perm[which(is.na(adj.agree.perm))] <- 0
  }
  adj.agree.corr <- adj.agree - adj.agree.perm[1:nvar, 1:nvar]
  # Huh, isnt 1:nvar all of them?

  diag(adj.agree.corr) <- NA

  return(adj.agree.corr)
}

#' Variable selection for MutualForestImpact.
#'
#' @param MFI [`MutualForestImpact()`] object.
#' @param variables Vector of variable names for **which related variables should be searched**.
#' @param candidates Vector of variable names that **are candidates to be related to the variables**.
#' @param p.threshold (Default = 0.01) P-value threshold
#' @param method The method to use. One of: `"Janitza"` or `"Permutation"`.
#'
#' @returns A list:
#'  * `method`: The method used.
#'  * `selected`: A list of vectors containing selected candidates for each investigated variable.
#'  * `p.values`: A list of numeric vectors containing p-values for each candidate's relation to each investigated variable.
#'
#' @keywords varsel
#' @export
MutualForestImpactVariableSelection <- function(
  MFI,
  variables,
  candidates,
  p.threshold = 0.01,
  method = c("Janitza", "Permutation")
) {
  if (!inherits(MFI, "MutualForestImpact")) {
    stop("`MFI` must be a `MutualForestImpact` object.")
  }

  method <- match.arg(method, c("Janitza", "Permutation"))

  result <- switch(method,
    "Janitza" = MFI_VarSel_Janitza(
      MFI = MFI,
      variables = variables, candidates = candidates,
      p.threshold = 0.01
    ),
    "Permutation" = MFI_VarSel_Permutation(
      MFI = MFI,
      variables = variables, candidates = candidates,
      p.threshold = 0.01
    ),
  )
  result$method <- method

  return(result)
}

#' Janitza method for MFI.
#'
#' @param MFI [`MutualForestImpact()`] object.
#' @param variables Vector of variable names for **which related variables should be searched**.
#' @param candidates Vector of variable names that **are candidates to be related to the variables**.
#' @param p.threshold (Default = 0.01) P-value threshold
#'
#' @returns A list:
#'  * `selected`: A list of vectors containing selected candidates for each investigated variable.
#'  * `p.values`: A list of numeric vectors containing p-values for each candidate's relation to each investigated variable.
#'
#' @keywords varsel internal
MFI_VarSel_Janitza <- function(
  MFI,
  variables,
  candidates,
  p.threshold = 0.01
) {
  if (!inherits(MFI, "MutualForestImpact")) {
    stop("`MFI` must be a `MutualForestImpact` object.")
  }

  adj.agree.corr <- adj_agree_corr(
    nvar = MFI$REL$RFS$ranger$num.independent.variables,
    rel_surr = MFI$REL$relations,
    perm_surr = MFI$PERM$relations
  )

  adj.agree.corr.var <- adj.agree.corr[variables, candidates]

  janitza_mfi(
    adj.agree.corr,
    adj.agree.corr.var,
    candidates = candidates,
    variables = variables,
    p.t = p.threshold
  )
}

#' @keywords internal
janitza_mfi <- function(
    adj.agree.corr,
    adj.agree.corr.var,
    candidates,
    variables,
    p.t = 0.01
) {
  adj.agree.1 <- adj.agree.corr
  diag(adj.agree.1) <- 1
  ## Mirrored VIMP (# This part is taken from ranger function)
  m1 <- adj.agree.1[adj.agree.1 < 0]
  m2 <- adj.agree.1[adj.agree.1 == 0]
  null.rel <- c(m1, -m1, m2)

  if (length(m1) == 0) {
    stop("No negative importance values found for selection of related variables. Consider the 'permutation' approach.")
  }
  if (length(m1) < 100) {
    warning("Only few negative importance values found for selection of related variables, inaccurate p-values. Consider the 'permutation' approach.")
  }

  rel.p <- lapply(1:length(variables), p.relation,
                  null.rel = null.rel,
                  adj.agree.corr = adj.agree.corr.var,
                  candidates = candidates,
                  variables = variables
  )
  sel.rel <- lapply(
    1:length(variables), select.related,
    rel.p,
    p.t
  )

  names(rel.p) <- names(sel.rel) <- variables

  return(list(
    selected = sel.rel,
    p.values = rel.p
  ))
}

#' Permutation method for MFI.
#'
#' @param MFI [`MutualForestImpact()`] object.
#' @param variables Vector of variable names for **which related variables should be searched**.
#' @param candidates Vector of variable names that **are candidates to be related to the variables**.
#' @param p.threshold (Default = 0.01) P-value threshold
#'
#' @returns A list:
#'  * `selected`: A list of vectors containing selected candidates for each investigated variable.
#'  * `p.values`: A list of numeric vectors containing p-values for each candidate's relation to each investigated variable.
#'
#' @keywords varsel internal
MFI_VarSel_Permutation <- function(
  MFI,
  candidates,
  variables,
  p.threshold = 0.01
) {
  if (!inherits(MFI, "MutualForestImpact")) {
    stop("`MFI` must be a `MutualForestImpact` object.")
  }

  adj.agree.corr <- adj_agree_corr(
    nvar = MFI$REL$RFS$ranger$num.independent.variables,
    rel_surr = MFI$REL$relations,
    perm_surr = MFI$PERM$relations
  )

  adj.agree.corr.var <- adj.agree.corr[variables, candidates]

  adj.agree.perm <- MFI$PERM$relations
  diag(adj.agree.perm) <- NA

  permutation_mfi(
    adj.agree.corr.var,
    adj.agree.perm,
    candidates = candidates,
    variables = variables,
    p.t = p.threshold
  )
}

#' @keywords internal
permutation_mfi <- function(
    adj.agree.corr.var,
    adj.agree_perm,
    candidates,
    variables,
    p.t = 0.01
) {
  null.rel.plus <- as.vector(adj.agree_perm)
  null.rel.plus <- null.rel.plus[!is.na(null.rel.plus)]

  m1 <- null.rel.plus[null.rel.plus > 0]
  m2 <- null.rel.plus[null.rel.plus == 0]
  null.rel <- c(m1, -m1, m2)

  if (length(null.rel) < 100) {
    warning("Only few null relations used. P-values could be inaccurate.")
  }

  rel.p <- lapply(1:length(variables), p.relation,
                  null.rel = null.rel,
                  adj.agree.corr = adj.agree.corr.var,
                  candidates = candidates,
                  variables = variables
  )

  sel.rel <- lapply(
    1:length(variables), select.related,
    rel.p,
    p.t
  )

  names(rel.p) <- names(sel.rel) <- variables

  return(list(
    selected = sel.rel,
    p.values = rel.p
  ))
}
