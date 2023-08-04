#' Mutual Impurity Reduction (MIR)
#'
#' @param REL A [`MeanAdjustedAgreement`] or [`MutualForestImpract`] object.
#'
#' @returns A [`MutualImpurityReduction`] list object.
#'  * `REL`: The [`MeanAdjustedAgreement`] or [`MutualForestImpact`] object.
#'  * `MIR`: Mutual Impurity Reduction
#'  * `AIR`: Actual Impurity Reduction
#'
#' @keywords varrel mir
#' @export
MutualImpurityReduction <- function(REL) {
  # Combine getting RFS with checking object inheritance
  if (inherits(REL, "MutualForestImpact")) {
    RFS <- REL$REL$RFS
  } else if (inherits(REL, "MeanAdjustedAgreement")) {
    RFS <- REL$RFS
  } else {
    stop("`REL` must be a `MeanAdjustedAgreement` or `MutualForestImpact` object.")
  }

  if (!inherits(RFS, "RandomForestSurrogates")) {
    stop("`RFS` must be a `RandomForestSurrogates` object.")
  }

  if (RFS$ranger$importance.mode != "impurity_corrected") {
    stop(paste0("`RFS` must have been created with `importance = \"impurity_corrected\".` (Found: \"", RFS$ranger$importance.mode, "\")"))
  }

  adj.agree <- REL$relations
  diag(adj.agree) <- 1
  # This is okay because candidates==variables

  air <- RFS$ranger$variable.importance
  mir <- colSums(adj.agree * air)

  result <- list(
    REL = REL,
    MIR = mir,
    AIR = air
  )
  class(result) <- "MutualImpurityReduction"

  return(result)
}

#' Variable selection for Mutual Impurity Reduction.
#'
#' @param MIR [`MutualImpurityReduction()`] object.
#' @param p.threshold (Default = 0.01) P-value threshold
#' @param method The method to use. One of: `"Janitza"` or `"Permutation"`.
#' @param permutation.num (If method is `"Permutation"`) Number of AIR permutations to determine p-value. (Default: 100)
#' @param permutation.MeanAdjustedAgreement If method is `"Permutation"` and `MIR` used `MeanAdjustedAgreement`.
#'
#' @returns A list:
#'  * `method`: The method used.
#'  * `selected`: A list of vectors containing selected candidates for each investigated variable.
#'  * `p.values`: A list of numeric vectors containing p-values for each candidate's relation to each investigated variable.
#'
#' @keywords varsel mir
#' @export
MutualImpurityReductionVariableSelection <- function(
    MIR,
    p.threshold = 0.01,
    method = c("Janitza", "Permutation"),
    permutation.num = 100,
    permutation.MeanAdjustedAgreement = NULL
) {
  if (!inherits(MIR, "MutualImpurityReduction")) {
    stop("`MIR` must be a `MutualImpurityReduction` object.")
  }

  method <- match.arg(method, c("Janitza", "Permutation"))

  result <- switch(method,
    "Janitza" = MIR_VarSel_Janitza(
      MIR = MIR,
      p.threshold = 0.01
    ),
    "Permutation" = MIR_VarSel_Permutation(
      MIR = MIR,
      p.threshold = 0.01,
      perm = permutation.MeanAdjustedAgreement,
      num.permutations = permutation.num
    ),
  )
  result$method <- method

  return(result)
}

#' @keywords varsel mir janitza
MIR_VarSel_Janitza <- function(
    MIR,
    p.threshold = 0.01) {
  if (!inherits(MIR$REL, "MutualForestImpact")) {
    stop("Janitza approach should only be conducted with corrected relations (`MutualForestImpact` expected).")
  }

  Janitza_MIR(
    mir = MIR$MIR,
    # allvariables = get_rfs_from_mir(MIR)$ranger$forest$independent.variable.names,
    p.t.sel = p.threshold
  )
}

#' @keywords internal
Janitza_MIR <- function(mir, allvariables = names(mir), p.t.sel = 0.01) {
  m1 <- mir[mir < 0]
  m2 <- mir[mir == 0]

  if (length(m1) == 0) {
    stop("No negative importance values found for selection of important variables. Consider the 'permutation' approach.")
  } else if (length(m1) < 100) {
    warning("Only few negative importance values found for selection of important variables, inaccurate p-values. Consider the 'permutation' approach.")
  }

  null.rel <- c(m1, -m1, m2)

  pval <- 1 - ranger:::numSmaller(mir, null.rel) / length(null.rel)
  names(pval) <- allvariables
  selected <- pval <= p.t.sel
  names(selected) <- names(pval)

  return(list(
    # chr[] selected variable names
    selected = names(selected[which(selected)]),
    # dbl[] p values (for all variables)
    p.values = pval
  ))
}

#' @keywords internal
get_rfs_from_mir <- function(
    MIR) {
  if (inherits(MIR$REL, "MutualForestImpact")) {
    RFS <- MIR$REL$REL$RFS
  } else if (inherits(MIR$REL, "MeanAdjustedAgreement")) {
    RFS <- MIR$REL$RFS
  } else {
    stop("`MIR$REL` must be MutualForestImpact or MeanAdjustedAgreement")
  }
  return(RFS)
}

#' @keywords varsel mir permutation
MIR_VarSel_Permutation <- function(
    MIR,
    perm = NULL,
    num.permutations = 100,
    p.threshold = 0.01) {
  if (inherits(MIR$REL, "MutualForestImpact")) {
    adj.agree_perm <- MIR$REL$PERM$relations
  } else {
    if (!inherits(perm, "MeanAdjustedAgreement")) {
      stop("`perm` must be a `MeanAdjustedAgreement` object.")
    }

    adj.agree_perm <- perm$relations
  }

  diag(adj.agree_perm) <- 0

  Permutation_MIR(
    MIR$MIR,
    adj.agree_perm,
    MIR$AIR,
    # get_rfs_from_mir(MIR)$ranger$forest$independent.variable.names,
    num.permutations = num.permutations,
    p.t.sel = p.threshold
  )
}

#' @keywords internal
Permutation_MIR <- function(
    mir,
    adj.agree_perm,
    air,
    allvariables = names(mir),
    num.permutations = 100,
    p.t.sel = 0.01) {
  null.rel <- unlist(lapply(1:num.permutations, calculate.mir.perm,
    adj.agree_perm = adj.agree_perm,
    air = air,
    allvariables = allvariables
  ))

  pval <- 1 - ranger:::numSmaller(mir, null.rel) / length(null.rel)
  names(pval) <- allvariables
  selected <- pval <= p.t.sel
  names(selected) <- names(pval)

  return(list(
    selected = names(selected[which(selected)]),
    p.values = pval
    # p.thresh = p.t.sel # redundant
  ))
}
