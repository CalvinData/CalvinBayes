
#' Diagnostic Plots for MCMC samples
#'
#' This function mimics `diagMCMC()` from Kruschke's *Doing Bayesian Data Analysis*.
#' The options to save the plot to a file have been removed since there are other
#' general purpose ways of doing this, and it isn't needed if you are working in
#' R Markdown, which is recommended for reporting.
#'
#' @param object an MCMC object as per the coda package
#' @param pars the name of a parameter for which diagnostics are displayed
#' @param parName an alias for pars
#'
#' @rdname diag_mcmc
#' @export
diagMCMC <- function(object, parName = varnames(object)[1]) {
  DBDAplColors = c("skyblue", "black", "royalblue", "steelblue")
  par(mar = 0.5+c(3, 4, 1, 0), oma = 0.1 + c(0, 0, 2, 0), mgp = c(2.25, 0.7, 0),
       cex.lab = 1.5)
  layout(matrix(1:4, nrow = 2))
  # traceplot and gelman.plot are from CODA package:
  require(coda)
  coda::traceplot(object[, c(parName)], main = "", ylab = "Param. Value",
                   col = DBDAplColors)
  tryVal = try(
    coda::gelman.plot(object[, c(parName)], main = "", auto.layout = FALSE,
                       col = DBDAplColors)
  )
  # if it runs, gelman.plot returns a list with finite shrink values:
  if (class(tryVal) == "try-error") {
    plot.new()
    print(paste0("Warning: coda::gelman.plot fails for ", parName))
  } else {
    if (class(tryVal) == "list" & !is.finite(tryVal$shrink[1])) {
      plot.new()
      print(paste0("Warning: coda::gelman.plot fails for ", parName))
    }
  }
  DbdaAcfPlot(object, parName, plColors = DBDAplColors)
  DbdaDensPlot(object, parName, plColors = DBDAplColors)
  mtext(text = parName, outer = TRUE, adj = c(0.5, 0.5), cex = 2.0)
}

#' @rdname diag_mcmc
#' @export
diag_mcmc <- diagMCMC

#' Plot posterior samples
#'
#' This function mimics `plotPost()` from Kruschke's *Doing Bayesian Data Analysis*
#' and provides a highly customized plot of a vector of posterior samples for
#' a parameter.
#' The options to save the plot to a file have been removed since there are other
#' general purpose ways of doing this, and it isn't needed if you are working in
#' R Markdown, which is recommended for reporting.
#'
#' @rdname plot_post
#' @param samples a vector of (posterior) sampled values
#' @param cenTend measure of central tendency to use.
#'   One of `"mode"`, `"median"` or `"mean"`.
#' @param comparison_value, compVal number for "comparison value"
#' @param ROPE a vector of length 2 giving the ends of the region of practical equivalence
#' @param hdi_prob,credMass probability for HDI interval
#' @param hdi_text_place,HDItextPlace location of HDI text on plot.
#' @param quietly if TRUE, return summary invisibly.
#' @param xlab,xlim,yaxt,ylab,main,cex,cex.lab,col,border,showCurve,breaks arguments for
#'   [`graphics::hist()`]
#' @param ... additional arguments passed along to [graphics::hist()]
#' @importFrom coda effectiveSize
#' @export
#' @examples
#' samples <- rnorm(2000)    # fake a posterior sample
#' plot_post(samples)
#' plot_post(samples, hdi_prob = 0.90)
#' plot_post(samples, hdi_prob = 0.90, ROPE = c(-0.2, 0.2), comparison_value = 2)
#' plot_post(samples, hdi_prob = 0.90, ROPE = c(-0.2, 0.2), compVal = 2)
#'

plotPost <- function(
  samples,
  center = c("mode", "median", "mean"),
  cenTend = center,
  comparison_value = NULL, compVal = comparison_value,
  ROPE = NULL,
  hdi_prob = 0.95, credMass = hdi_prob,
  hdi_text_place = 0.7, HDItextPlace = hdi_text_place,
  quietly = FALSE,
  xlab = "Param. Val.", xlim = NULL,
  yaxt = "n", ylab = "", main = "",
  cex = 1.4, cex.lab = 1.5,
  col = "skyblue", border = "white",
  showCurve = FALSE, breaks = NULL,
  ...) {

  cenTend <- match.arg(cenTend, choices =  c("mode", "median", "mean"))

  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if (is.null(xlab)) xlab <- "Param. Val."
  if (is.null(cex.lab)) cex.lab <- 1.5
  if (is.null(cex)) cex <- 1.4
  if (is.null(xlim)) xlim <- range(c(compVal, ROPE, samples))
  if (is.null(main)) main <- ""
  if (is.null(yaxt)) yaxt <- "n"
  if (is.null(ylab)) ylab <- ""
  if (is.null(col)) col <- "skyblue"
  if (is.null(border)) border <- "white"

  # convert coda object to matrix:
  if (class(samples) == "mcmc.list") {
    samples = as.matrix(samples)
  }

  # summaryColNames = c("ESS", "mean", "median", "mode",
  #                     "hdiMass", "hdiLow", "hdiHigh",
  #                     "compVal", "pGtCompVal",
  #                     "ROPElow", "ROPEhigh", "pLtROPE", "pInROPE", "pGtROPE")
  # postSummary = matrix(NA, nrow = 1, ncol = length(summaryColNames),
  #                      dimnames = list(c(xlab), summaryColNames))
  # postSummary[, "ESS"] = coda::effectiveSize(samples)
  # postSummary[, "mean"] = mean(samples)
  # postSummary[, "median"] = median(samples)
  # mcmcDensity = density(samples)
  # postSummary[, "mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  # postSummary[, "hdiMass"] <- credMass
  # postSummary[, "hdiLow"] <- HDI[1]
  # postSummary[, "hdiHigh"] <- HDI[2]

  mcmcDensity <- density(samples)
  HDI = HDIofMCMC(samples, credMass)

  postSummary <- list(
    "posterior" =
      data.frame(check.names = FALSE,
        ESS =  coda::effectiveSize(samples),
        mean = mean(samples),
        median = median(samples),
        mode = mcmcDensity$x[which.max(mcmcDensity$y)]),
    "hdi" =
      data.frame(
        prob = credMass,
        lo = HDI[1],
        hi = HDI[2]
      )
  )

  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if (is.null(breaks)) {
    if (max(samples) > min(samples)) {
      breaks = c(seq(from = min(samples), to = max(samples),
                       by = (HDI[2]-HDI[1])/18), max(samples))
    } else {
      breaks = c(min(samples)-1.0E-6, max(samples)+1.0E-6)
      border = "skyblue"
    }
  }
  if (!showCurve) {
    par(xpd = NA)
    histinfo = hist(samples, xlab = xlab, yaxt = yaxt, ylab = ylab,
                     freq = F, border = border, col = col,
                     xlim = xlim, main = main, cex = cex, cex.lab = cex.lab,
                     breaks = breaks, ...)
  }
  if (showCurve) {
    par(xpd = NA)
    histinfo = hist(samples, plot = FALSE)
    densCurve = density(samples, adjust = 2)
    plot(densCurve$x, densCurve$y, type = "l", lwd = 5, col = col, bty = "n",
          xlim = xlim, xlab = xlab, yaxt = yaxt, ylab = ylab,
          main = main, cex = cex, cex.lab = cex.lab, ...)
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display central tendency:
  mn = mean(samples)
  med = median(samples)
  mcmcDensity = density(samples)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if (cenTend == "mode"){
    text(mo, cenTendHt,
          bquote(mode == .(signif(mo, 3))), adj = c(.5, 0), cex = cex)
  }
  if (cenTend == "median"){
    text(med, cenTendHt,
          bquote(median == .(signif(med, 3))), adj = c(.5, 0), cex = cex, col = cvCol)
  }
  if (cenTend == "mean"){
    text(mn, cenTendHt,
          bquote(mean == .(signif(mn, 3))), adj = c(.5, 0), cex = cex)
  }
  # Display the comparison value.
  if (!is.null(compVal)) {
    pGtCompVal = sum(samples > compVal) / length(samples)
    pLtCompVal = 1 - pGtCompVal
    postSummary[["comparison"]] <-
      data.frame(check.names = FALSE,
        "value" = compVal,
        "P(< comp. val.)"  = pLtCompVal,
        "P(> comp. val.)"  = pGtCompVal
      )
    lines(c(compVal, compVal), c(0.96 * cvHt, 0),
           lty = "dotted", lwd = 2, col = cvCol)
    text(compVal, cvHt,
          bquote(.(round(100*pLtCompVal, 1)) * "% < " *
                    .(signif(compVal, 3)) * " < " *
                    .(round(100*pGtCompVal, 1)) * "%"),
          adj = c(pLtCompVal, 0), cex = 0.8*cex, col = cvCol)

  }
  # Display the ROPE.
  if (!is.null(ROPE)) {
    pInROPE = (sum(samples > ROPE[1] & samples < ROPE[2])
                / length(samples))
    pGtROPE = (sum(samples >= ROPE[2]) / length(samples))
    pLtROPE = (sum(samples <= ROPE[1]) / length(samples))
    lines(c(ROPE[1], ROPE[1]), c(0.96*ROPEtextHt, 0), lty = "dotted", lwd = 2,
           col = ropeCol)
    lines(c(ROPE[2], ROPE[2]), c(0.96*ROPEtextHt, 0), lty = "dotted", lwd = 2,
           col = ropeCol)
    text(mean(ROPE), ROPEtextHt,
          bquote(.(round(100*pLtROPE, 1)) * "% < " * .(ROPE[1]) * " < " *
                    .(round(100*pInROPE, 1)) * "% < " * .(ROPE[2]) * " < " *
                    .(round(100*pGtROPE, 1)) * "%"),
          adj = c(pLtROPE+.5*pInROPE, 0), cex = 1, col = ropeCol)

    postSummary[["ROPE"]] <-
      data.frame(check.names = FALSE,
        lo  = ROPE[1],
        hi = ROPE[2],
        `P(< ROPE)`  = pLtROPE,
        `P(in ROPE)` = pInROPE,
        `P(> ROPE)`  = pGtROPE
      )
  }
  # Display the HDI.
  lines(HDI, c(0, 0), lwd = 4, lend = 1)
  text(mean(HDI), 0, bquote(.(100*credMass) * "% HDI"),
        adj = c(.5, -1.7), cex = cex)
  text(HDI[1], 0, bquote(.(signif(HDI[1], 3))),
        adj = c(HDItextPlace, -0.5), cex = cex)
  text(HDI[2], 0, bquote(.(signif(HDI[2], 3))),
        adj = c(1.0-HDItextPlace, -0.5), cex = cex)
  par(xpd = FALSE)

  if (quietly) {
    invisible(postSummary)
  } else {
    postSummary
  }
}

#' @rdname plot_post
#' @export

plot_post <- plotPost
