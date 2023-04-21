#### HOUSEKEEPING ####

# Required packages
library(ggplot2)
library(plyr)
library(smaa)
library(mvtnorm)
library(dplyr)
library(reshape2)
library(tidyr)

# Prevent scientific notation
options(scipen = 10L)

# Load data
etI <- read.csv("et - induction.csv", header = TRUE)
etM <- read.csv("et - maintenance.csv", header = TRUE)
etN <- read.csv("et - nail.csv", header = TRUE)
ppI <- read.csv("ppi - induction.csv", header = TRUE)
ppM <- read.csv("ppi - maintenance.csv", header = TRUE)
ppN <- read.csv("ppi - nail.csv", header = TRUE)
etI.ci <- read.csv("et - induction - ci.csv", header = TRUE)
ppI.ci <- read.csv("ppi - induction - ci.csv", header = TRUE)
etI.n <- read.csv("et - induction - n.csv", header = TRUE)

#### SWING WEIGHTS ####

# Function to calculate swing Weights
sw <- function(et, ppi) {
  out <- data.frame(matrix(nrow = ncol(et) - 1, ncol = 3))
  names(out) <- c("Attributes", "Weight", "sw")
  out$Attributes <- names(et[, 2:ncol(et)])
  ppi$abs.est <- abs(ppi$estimate)

  # Calculate the constrained Weights (Weights sum to 1)
  for (a in 1:nrow(out)) {
    out$Weight[a] <- max(ppi$abs.est[ppi$Attribute == out$Attributes[a]]) /
      sum(by(ppi$abs.est, ppi$Attribute, max))
  }

  # Calculate the swing Weight respondents would have provided
  # Attribute with largest constrained Weight set to a swing Weight of 100
  # All other Attributes have a proportional swing Weight
  for (a in 1:nrow(out)) {
    out$sw[a] <- ifelse(out$Weight[a] == max(out$Weight), 100,
                        out$Weight[a] * 100 /
                          out$Weight[which(out$Weight == max(out$Weight))])
  }

  return(out)
}

# Calculate swing Weights for each model
swI <- sw(etI, ppI)
swM <- sw(etM, ppM)
swN <- sw(etN, ppN)

#### PARTIAL VALUE FUNCTIONS FROM MNL ESTIMATES ####

# Function to calculate Partial value function from mnl estimates
pvf <- function(ppi) {
  ppi$Partial <- NA
  att <- names(table(ppi$Attribute))
  for (a in 1:length(att)) {
    tmp.ppi <- ppi[ppi$Attribute == att[a], ]
    if (tmp.ppi$type[1] == "ascending") {
      ppi$Partial[ppi$Attribute == att[a]] <-
        tmp.ppi$estimate / max(abs(tmp.ppi$estimate))
    } else {
      ppi$Partial[ppi$Attribute == att[a]] <-
        (tmp.ppi$estimate - min(tmp.ppi$estimate)) / max(abs(tmp.ppi$estimate))
    }
    for (i in 1:nrow(tmp.ppi)) {

    }
  }
  return(ppi$Partial)
}

# Calculate Partial value functions
ppI$Partial <- pvf(ppI)
ppM$Partial <- pvf(ppM)
ppN$Partial <- pvf(ppN)

# Plot Preference Functions
ppI.ci$Attribute <- factor(ppI.ci$Attribute,
                           levels = c("PASI75", "Infection", "SIB"))
gg <- ggplot(data = ppI.ci, aes (x = Level, y = Mean)) +
  geom_line(aes(col = Attribute)) +
  geom_point(aes(col = Attribute)) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, col = Attribute),
                width = c(rep((max(ppI.ci$Level[ppI.ci$Attribute == "PASI75"]) -
                                 min(ppI.ci$Level[ppI.ci$Attribute == "PASI75"])) * 0.05, 4),
                          rep((max(ppI.ci$Level[ppI.ci$Attribute == "Infection"]) -
                                 min(ppI.ci$Level[ppI.ci$Attribute == "Infection"])) * 0.05, 4),
                          rep((max(ppI.ci$Level[ppI.ci$Attribute == "SIB"]) -
                                 min(ppI.ci$Level[ppI.ci$Attribute == "SIB"])) * 0.05, 4)))
gg <- gg + facet_grid(. ~ Attribute, scales = "free_x") +
  theme(legend.position = "none") +
  labs(title = "Preference Estimates",
       subtitle = "Induction Phase Model") +
  ylab("Coefficients") +
  xlab("Level (percent of subjects)")
ggsave("pp_weights_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg)

#### PERFORMANCE SCORES ####

# Function to Calculate performance Scores
pfs <- function(et, ppi) {
  out <- et
  att <- names(out[, 2:ncol(out)])
  for (i in 2:ncol(out)) {
    tmp.ppi <- ppi[ppi$Attribute == att[i - 1], ]
    for (j in 1:nrow(out)) {
      if (is.na(et[j, i]) == TRUE) {
        out[j, i] <- 0
      } else {
        if (length(which(tmp.ppi$Level == et[j, i])) == 1) {
          out[j, i] <- tmp.ppi$Partial[which(tmp.ppi$Level == et[j, i])]
        } else {
          point <- et[j, i]
          level.below <- ifelse(et[j, i] < min(tmp.ppi$Level),
                                min(tmp.ppi$Level),
                                ifelse(et[j, i] > max(tmp.ppi$Level),
                                       tmp.ppi$Level[which(tmp.ppi$Level == max(tmp.ppi$Level[tmp.ppi$Level != max(tmp.ppi$Level)]))],
                                       max(tmp.ppi$Level[which(tmp.ppi$Level < et[j, i])])))
          level.above <- ifelse(et[j, i] < min(tmp.ppi$Level),
                                tmp.ppi$Level[which(tmp.ppi$Level == min(tmp.ppi$Level[tmp.ppi$Level != min(tmp.ppi$Level)]))],
                                ifelse(et[j, i] > max(tmp.ppi$Level),
                                       max(tmp.ppi$Level),
                                       min(tmp.ppi$Level[which(tmp.ppi$Level > et[j, i])])))
          partial.below <- ifelse(et[j, i] < min(tmp.ppi$Level),
                                  tmp.ppi$Partial[which(tmp.ppi$Level == min(tmp.ppi$Level))],
                                  ifelse(et[j, i] > max(tmp.ppi$Level),
                                         tmp.ppi$Partial[which(tmp.ppi$Level == max(tmp.ppi$Level[tmp.ppi$Level != max(tmp.ppi$Level)]))],
                                         tmp.ppi$Partial[which(tmp.ppi$Level == max(tmp.ppi$Level[which(tmp.ppi$Level < et[j, i])]))]))
          partial.above <- ifelse(et[j, i] < min(tmp.ppi$Level),
                                  tmp.ppi$Partial[which(tmp.ppi$Level == min(tmp.ppi$Level[tmp.ppi$Level != min(tmp.ppi$Level)]))],
                                  ifelse(et[j, i] > max(tmp.ppi$Level),
                                         tmp.ppi$Partial[which(tmp.ppi$Level == max(tmp.ppi$Level))],
                                         tmp.ppi$Partial[which(tmp.ppi$Level == min(tmp.ppi$Level[which(tmp.ppi$Level > et[j, i])]))]))
          rise = partial.above - partial.below
          run = level.above - level.below
          slope = rise / run
          out[j, i] <- partial.below  + slope * (point - level.below)
          rm(point, level.below, level.above, partial.below, partial.above, 
             rise, run, slope)
        }
      }
    }
  }  
  return(out)
}

# Calculate performance Scores
psI <- pfs(etI, ppI)
psM <- pfs(etM, ppM)
psN <- pfs(etN, ppN)
psI.lower <- pfs(etI.ci[5:8, c(1, 3:5)], ppI)
psI.upper <- pfs(etI.ci[9:12, c(1, 3:5)], ppI)

#### BENEFIT-RISK SCORE ####

# Function for benefit-risk Score calculation
brs <- function(ps, sw) {
  out <- ps
  out$Total <- NA
  for (a in 1:nrow(sw)) {
    out[, sw$Attributes[a]] <- ps[, sw$Attributes[a]] *
      sw$Weight[sw$Attributes == sw$Attributes[a]]
  }
  for (d in 1:nrow(out)) {
    out$Total[d] <- sum(out[d, 2:(ncol(out) - 1)])
  }
  return(out)
}

# Calculate benefit-risk Scores
brI <- brs(psI, swI)
brM <- brs(psM, swM)
brN <- brs(psN, swN)

# Function to restructure data for plotting
brs.plot <- function(br) {
  Attribute <- rep(names(br[, 2:(ncol(br) - 1)]), nrow(br))
  for (i in 1:nrow(br)) {
    if (i == 1) {
      Drug <- rep(br$Drug[i], (ncol(br) - 2))
      Score <- unlist(br[i, 2:(ncol(br) - 1)])
    } else {
      Drug <- c(Drug, rep(br$Drug[i], (ncol(br) - 2)))
      Score <- c(Score, unlist(br[i, 2:(ncol(br) - 1)]))
    }
  }
  out <- data.frame(Attribute, Drug, Score)
  return(out)
}

# Bar chart of Total Scores with breakdown to individual Attributes
brI.plot <- brs.plot(brI)
brI.plot$Attribute <- factor(brI.plot$Attribute,
                             levels = c("PASI75", "Infection", "SIB"))
brI.plot$Drug <- factor(brI.plot$Drug,
                        levels = c("BRO 140", "BRO 210", "UST", "PBO"))
gg <- ggplot(brI.plot, aes(fill = Attribute, y = Score, x = Drug)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Aggregate Benefit-Risk Score",
       subtitle = "Induction Phase Model") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
ggsave("brs_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg, brI.plot)
brM.plot <- brs.plot(brM)
brM.plot$Attribute <- factor(brM.plot$Attribute,
                             levels = c("sPGA",
                                        "Infection", "SIB", "Hypersensitivity"))
brM.plot$Drug <- factor(brM.plot$Drug,
                        levels = c("BRO 140", "BRO 210", "UST"))
gg <- ggplot(brM.plot, aes(fill = Attribute, y = Score, x = Drug)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Aggregate Benefit-Risk Score",
       subtitle = "Maintenance Phase Model") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
ggsave("brs_mM.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg, brM.plot)
brN.plot <- brs.plot(brN)
brN.plot$Attribute <- factor(brN.plot$Attribute,
                             levels = c("NAPSI", "Infection", "SIB"))
brN.plot$Drug <- factor(brN.plot$Drug,
                        levels = c("BRO 140", "BRO 210", "PBO"))
gg <- ggplot(brN.plot, aes(fill = Attribute, y = Score, x = Drug)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Aggregate Benefit-Risk Score",
       subtitle = "Nail Psoriasis Model") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))
ggsave("brs_mN.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg, brN.plot)

#### WEIGHTED NET CLINICAL BENEFIT ####

# Weights per percent
for (a in 1:nrow(swI)) {
  swI$wncb[a] <- swI$Weight[a] / 
    (max(ppI$Level[ppI$Attribute == swI$Attributes[a]]) - 
       min(ppI$Level[ppI$Attribute == swI$Attributes[a]]))
}
swI$wncb[2:3] <- -swI$wncb[2:3]
swI$wncb <- swI$wncb / swI$wncb[swI$Attributes == "PASI75"]

# PASI75 Responder Equivalents
wncbI <- data.frame(matrix(nrow = 3, ncol = 5))
names(wncbI) <- c("Drug", swI$Attributes, "Total")
wncbI$Drug <- brI$Drug[brI$Drug != "PBO"]
for (d in 1:nrow(wncbI)) {
  for (a in 1:nrow(swI)) {
    wncbI[d, a + 1] <- 
      (etI[etI$Drug == wncbI$Drug[d], swI$Attributes[a]] - 
         etI[etI$Drug == "PBO", swI$Attributes[a]]) * 
      swI$wncb[a]
  }
  wncbI$Total[d] <- sum(wncbI[d, 2:4])
}

# Barchart
wncbI$Drug <- factor(wncbI$Drug,
                     levels = c("BRO 140", "BRO 210", "UST"))
gg <- ggplot(wncbI, aes(fill = Drug, y = Total * 100, x = Drug)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Weighted Net Clinical Benefit vs Placebo",
       subtitle = "Induction Phase Model") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  ylab("PASI75 responder equivalents per 100 patients treated") +
  theme(legend.position = "none")
ggsave("wncb_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg)

#### BENEFT-RISK SCORE DIFFERENCE ####

# Base Model: BRO vs UST and BRO vs PBO
brdI <- data.frame(matrix(nrow = 4 * 4, ncol = 3))
names(brdI) <- c("comp", "Attribute", "score.diff")
brdI$comp <- c(rep("BRO 140 vs UST", 4), rep("BRO 210 vs UST", 4),
               rep("BRO 140 vs PBO", 4), rep("BRO 210 vs PBO", 4))
brdI$Attribute <- rep(names(brI[, 2:ncol(brI)]), 4)
brdI$score.diff[brdI$comp == "BRO 140 vs UST"] <-
  brI[brI$Drug == "BRO 140", 2:ncol(brI)] - brI[brI$Drug == "UST", 2:ncol(brI)]
brdI$score.diff[brdI$comp == "BRO 210 vs UST"] <-
  brI[brI$Drug == "BRO 210", 2:ncol(brI)] - brI[brI$Drug == "UST", 2:ncol(brI)]
brdI$score.diff[brdI$comp == "BRO 140 vs PBO"] <-
  brI[brI$Drug == "BRO 140", 2:ncol(brI)] - brI[brI$Drug == "PBO", 2:ncol(brI)]
brdI$score.diff[brdI$comp == "BRO 210 vs PBO"] <-
  brI[brI$Drug == "BRO 210", 2:ncol(brI)] - brI[brI$Drug == "PBO", 2:ncol(brI)]

# Bar chart of Score differences
brdI$Attribute[brdI$Attribute == "Total"] <- "Aggregate"
brdI$Attribute <- factor(brdI$Attribute,
                         levels = c("Aggregate", "SIB", "Infection", "PASI75"))
brdI$score.diff <- unlist(brdI$score.diff)
comps <- names(table(brdI$comp))
for (c in c(3)) {
  gg <- ggplot(brdI[brdI$comp == comps[c], ],
               aes(x = Attribute, y = score.diff, fill = score.diff < 0)) +
    scale_fill_manual(values = c("green", "red")) +
    ylab("\n\nBenefit-Risk Score Difference") +
    geom_bar(stat = "identity") +
    coord_flip(xlim = c(1, 4),
               ylim = c(min(min(brdI$score.diff), -max(brdI$score.diff) * 0.2),
                        max(brdI$score.diff)),
               clip = "off") +
    labs(title = comps[c], subtitle = "Induction Phase Model") +
    theme(legend.position = "none") +
    annotate(geom = "text", x = -0.3, y = 0 + max(brdI$score.diff) * 0.2,
             size = 3, label="Favors Brodalumab") +
    annotate(geom = "text", x = -0.3, y = 0 - max(brdI$score.diff) * 0.2,
             size = 3, label="Favors Comparator") +
    annotate(geom = "segment", x = -0.1, xend = -0.1,
             y = 0.05, yend = 0 + max(brdI$score.diff) * 0.3,
             arrow=arrow(length=unit(0.1, "cm"))) +
    annotate(geom = "segment", x = -0.1, xend = -0.1,
             y = -0.05, yend = 0 - max(brdI$score.diff) * 0.3,
             arrow=arrow(length=unit(0.1, "cm")))
  ggsave(paste0("brs_diff_mI_", gsub(" ", "-", comps[c]), ".png"), plot = gg,
         width = 6, height = 4, units = "in")
  rm(gg)
}
rm(comps, c)

#### PREDICTED CHOICE PROBABILITIES ####

set.seed(1911)
n.samples <- 1e4
n <- 3 # nr criteria
crit.names <- c('PASI75', 'Infec', 'SIB')
trt.names <- c('BRO 140', 'BRO 210', 'UST', 'PBO')

beta.sample <- function(r, n) {
  rbeta(n=1, shape1=r+1, shape2=n-r+1)
}

### Rank probabilities

sample.perf <- function() {
  matrix(c(
    ## PASI75
    beta.sample(round(etI$PASI75[etI$Drug == "BRO 140"] * 
                        etI.n$PASI75[etI$Drug == "BRO 140"]), 
                etI.n$PASI75[etI$Drug == "BRO 140"]),
    beta.sample(round(etI$PASI75[etI$Drug == "BRO 210"] * 
                        etI.n$PASI75[etI$Drug == "BRO 210"]), 
                etI.n$PASI75[etI$Drug == "BRO 210"]),
    beta.sample(round(etI$PASI75[etI$Drug == "UST"] * 
                        etI.n$PASI75[etI$Drug == "UST"]), 
                etI.n$PASI75[etI$Drug == "UST"]),
    beta.sample(round(etI$PASI75[etI$Drug == "PBO"] * 
                        etI.n$PASI75[etI$Drug == "PBO"]), 
                etI.n$PASI75[etI$Drug == "PBO"]),
    ## Infections
    beta.sample(round(etI$Infection[etI$Drug == "BRO 140"] * 
                        etI.n$Infection[etI$Drug == "BRO 140"]), 
                etI.n$Infection[etI$Drug == "BRO 140"]),
    beta.sample(round(etI$Infection[etI$Drug == "BRO 210"] * 
                        etI.n$Infection[etI$Drug == "BRO 210"]), 
                etI.n$Infection[etI$Drug == "BRO 210"]),
    beta.sample(round(etI$Infection[etI$Drug == "UST"] * 
                        etI.n$Infection[etI$Drug == "UST"]), 
                etI.n$Infection[etI$Drug == "UST"]),
    beta.sample(round(etI$Infection[etI$Drug == "PBO"] * 
                        etI.n$Infection[etI$Drug == "PBO"]), 
                etI.n$Infection[etI$Drug == "PBO"]),
    ## SIB - no uncertainty taken into account
    etI$SIB[etI$Drug == "BRO 140"],
    etI$SIB[etI$Drug == "BRO 210"],
    etI$SIB[etI$Drug == "UST"],
    etI$SIB[etI$Drug == "PBO"]
  ), ncol=3, byrow=FALSE)
}

meas <- raply(n.samples, sample.perf)
dimnames(meas)[[2]] <- trt.names
dimnames(meas)[[3]] <- crit.names

pv <- aaply(1:dim(meas)[3], 1, function(ind) { # index of criterion
    row.inds <- ((ind-1)*4+1):(ind*4)
    matrix(smaa.pvf(as.vector(meas[,,ind]),
                    cutoffs=ppI[row.inds, 'Level'],
                    values=ppI[row.inds, 'Partial'],
                    outOfBounds='interpolate'),
           ncol=4, byrow=FALSE)
})
pv <- aperm(pv, c(2, 3, 1)) # change order of dims to correct
dimnames(pv)[[2]] <- trt.names
dimnames(pv)[[3]] <- crit.names

n.pars <- nrow(ppI) - 3
dce.mle <- ppI$estimate[c(2:4,6:8,10:12)]
dce.se <- ppI.ci$SE[c(2:4,6:8,10:12)]

## Random vcov matrix
vcov <- matrix(rnorm(mean=0, sd=0.001, n=n.pars*n.pars), ncol=n.pars)
## make symmetric
vcov[lower.tri(vcov)] <- t(vcov)[lower.tri(vcov)]
diag(vcov) <- dce.se * dce.se

kr.dce.samples <- suppressWarnings(rmvnorm(n.samples, mean=dce.mle, sigma=vcov, 
                                           method='chol'))

my.et <- etI[,c(2,4,3)]

pcps <- laply(1:n.samples, function(idx) {
  my.mle <- kr.dce.samples[idx,]
  my.uf <- cbind(matrix(c(0, my.mle[1:3], 0, my.mle[4:6], 0, my.mle[7:9])),
                 ppI$Level)
  
  my.pvs <- aaply(1:dim(meas)[3], 1, function(ind) { # index of criterion
    row.inds <- ((ind-1)*4+1):(ind*4)
    matrix(smaa.pvf(as.vector(my.et[,ind]),
                    cutoffs=my.uf[row.inds, 2],
                    values=my.uf[row.inds, 1],
                    outOfBounds='interpolate'),
           ncol=4, byrow=FALSE)
  })
  
  my.uts <- colSums(my.pvs)
  exps <- exp(my.uts)
  laply(my.uts, function(x) {exp(x) / sum(exps)})
})

## Mean predicted choice probabilities - CIs using Krinsky-Robb
my.pvs <- aaply(1:dim(meas)[3], 1, function(ind) { # index of criterion
  row.inds <- ((ind-1)*4+1):(ind*4)
  matrix(smaa.pvf(as.vector(my.et[,ind]),
                  cutoffs=ppI[row.inds, 'Level'],
                  values=ppI[row.inds, 'estimate'],
                  outOfBounds='interpolate'),
         ncol=4, byrow=FALSE)
})
my.uts <- colSums(my.pvs)
exps <- exp(my.uts)
mean.pcp <- laply(my.uts, function(x) {exp(x) / sum(exps)})

pcps.est <- adply(pcps, 2, quantile, probs=c(0.025, 0.975))
rownames(pcps.est) <- trt.names
pcps.est$mean <- mean.pcp

## 95% CIs for the normalized weights 
kr.w.samples <- aaply(kr.dce.samples[,c(3, 6, 9)], 1, function(x) {abs(x) / sum(abs(x))})
w.ci.est <- adply(kr.w.samples, 2, quantile, probs=c(0.025, 0.975))[,c(2,3)]
rownames(w.ci.est) <- c('PASI75', 'Infection', 'SIB')
w.ci.est$mean <- abs(dce.mle[c(3, 6, 9)]) / sum(abs(dce.mle[c(3, 6, 9)]))

## Forest plot of predicted choice probabilities
pcps.est$Drug <- rownames(pcps.est)
pcps.est <- pcps.est[,c(5,4,2,3)]
colnames(pcps.est)[3:4] <- c('lower', 'upper')

pcps.est$Drug <- c("BRO 140", "BRO 210", "UST", "PBO")
pcps.est$Drug <- factor(pcps.est$Drug,
                        levels = c("BRO 140", "BRO 210", "UST", "PBO"))
gg <- ggplot(pcps.est, aes(x = mean, 
                           y = factor(Drug, levels = rev(levels(factor(Drug)))), 
                           color = Drug)) +
  geom_point() +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = 0.1)) +
  xlab("Mean predicted choice probability (95% confidence interval)") +
  ylab("Drug") +
  theme(legend.position = "none") +
  labs(title = "Predicted Choice Probability",
       subtitle = "Induction Phase Model") +
  xlim(0, max(pcps.est$upper))
ggsave("pcp_forest_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg)

#### MULTI-WAY SENSITIVITY ANALYSIS ####

## SMAA analysis with uncertainty both on weights and performance ##
val.samples.fullsmaa <- smaa.values(pv, kr.w.samples)
fullsmaa.ra <- smaa.ra(smaa.ranks(val.samples.fullsmaa))

## Bar chart of rank probabilities
fullsmaa.plot <- data.frame(matrix(nrow = 16, ncol = 7))
names(fullsmaa.plot) <- c("Drug", "Rank", "Probability", "Bottom", "Top", "Mean",
                          "Label")
fullsmaa.plot$Drug <- c(rep("BRO 140", 4), rep("BRO 210", 4), rep("UST", 4),
                        rep("PBO", 4))
fullsmaa.plot$Rank <- c("1st", "2nd", "3rd", "4th")
fullsmaa.plot$Probability <- c(fullsmaa.ra[1, ], fullsmaa.ra[2, ],
                               fullsmaa.ra[3, ], fullsmaa.ra[4, ])
fullsmaa.plot$Drug <- factor(fullsmaa.plot$Drug,
                             levels = c("BRO 140", "BRO 210", "UST", "PBO"))
fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "4th"] <- 0
fullsmaa.plot$Top[fullsmaa.plot$Rank == "4th"] <- 
  fullsmaa.plot$Probability[fullsmaa.plot$Rank == "4th"]
fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "3rd"] <- 
  fullsmaa.plot$Top[fullsmaa.plot$Rank == "4th"]
fullsmaa.plot$Top[fullsmaa.plot$Rank == "3rd"] <- 
  fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "3rd"] + 
  fullsmaa.plot$Probability[fullsmaa.plot$Rank == "3rd"]
fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "2nd"] <- 
  fullsmaa.plot$Top[fullsmaa.plot$Rank == "3rd"]
fullsmaa.plot$Top[fullsmaa.plot$Rank == "2nd"] <- 
  fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "2nd"] + 
  fullsmaa.plot$Probability[fullsmaa.plot$Rank == "2nd"]
fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "1st"] <- 
  fullsmaa.plot$Top[fullsmaa.plot$Rank == "2nd"]
fullsmaa.plot$Top[fullsmaa.plot$Rank == "1st"] <- 
  fullsmaa.plot$Bottom[fullsmaa.plot$Rank == "1st"] + 
  fullsmaa.plot$Probability[fullsmaa.plot$Rank == "1st"]
for (i in 1:nrow(fullsmaa.plot)) {
  fullsmaa.plot$Mean[i] <- mean(as.numeric(fullsmaa.plot[i, 4:5]))
}
fullsmaa.plot$Label <- round(fullsmaa.plot$Probability, 2)
fullsmaa.plot$Label[fullsmaa.plot$Label == 0.00] <- NA
gg <- ggplot(fullsmaa.plot, aes(fill = Rank, y = Probability, x = Drug)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Rank Probabilities", subtitle = "Induction Phase Model") +
  annotate("text", x = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), 
           y = fullsmaa.plot$Mean, label = fullsmaa.plot$Label)
suppressWarnings(ggsave("rank_prob_mI.png", plot = gg, width = 6, height = 4, 
                        units = "in"))
rm(gg, fullsmaa.plot)

## Distribution of benefit-risk scores
vals.molten <- as.data.frame(melt(unclass(val.samples.fullsmaa))[,c(2,3)])
colnames(vals.molten) <- c('Drug', 'BR.score')

gg <- vals.molten %>% ggplot(aes(x = BR.score, col = Drug)) + geom_density() +
  labs(title = "Distribution of Aggregate Benefit-Risk Score",
       subtitle = "Induction Phase Model") +
  xlab("Aggregate Benefit-Risk Score") +
  ylab("Density") +
  xlim(0, 1) 
ggsave("dist_brs_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg)

#### ONE-WAY SENSITIVITY TESTING: EFFECTS ####

# Function for one-way sensitivity plots for effects
one.eff <- function(et, sw, ppi, Attribute, Drug) {
  Level <- rep(ppi$Level[ppi$Attribute == Attribute], nrow(et))
  for (d in 1:nrow(et)) {
    if (d == 1) {
      Drugs <- rep(et$Drug[d], length(ppi$Level[ppi$Attribute == Attribute]))
    } else {
      Drugs <- c(Drugs, rep(et$Drug[d],
                            length(ppi$Level[ppi$Attribute == Attribute])))
    }
  }
  out <- data.frame(Drugs, Level)
  rm(Drugs, Level)
  out$Score <- NA
  Levels <- ppi$Level[ppi$Attribute == Attribute]
  for (l in 1:length(Levels)) {
    tmp.et <- et
    tmp.et[tmp.et$Drug == Drug, Attribute] <- Levels[l]
    tmp.ps <- pfs(tmp.et, ppi)
    tmp.br <- brs(tmp.ps, sw)
    for (d in 1:nrow(et)) {
      out$Score[out$Drugs == tmp.et$Drug[d] &
                  out$Level == Levels[l]] <-
        tmp.br$Total[tmp.br$Drug == et$Drug[d]]
    }
    rm(tmp.et, tmp.ps, tmp.br)
  }
  return(out)
}

# Sensitivity Analysis Plot (line chart)
Attributes <- swI$Attributes
Drugs <- etI$Drug
ci.y <- c(0, 0, 0)
ci.yend <- c(1, 1, 1)
ci.lt <- c(1, 2, 2)
for (a in 1:length(Attributes)) {
  for (d in 2) {
    sens.plot <- one.eff(etI, swI, ppI, Attributes[a], Drugs[d])
    sens.plot$Drugs <- factor(sens.plot$Drugs,
                              levels = c("BRO 140", "BRO 210", "UST", "PBO"))
    ci <- c(etI.ci[d, a + 2], etI.ci[d + 4, a + 2], etI.ci[d + 8, a + 2])
    gg <- ggplot(sens.plot,
                 aes(x = Level, y = Score, group = Drugs, color = Drugs)) +
      geom_line() +
      annotate(geom = "segment", x = ci[1], xend = ci[1], y = 0, yend = 1, 
               linetype = 1) +
      scale_x_continuous(breaks = sens.plot$Level, minor_breaks = NULL,
                         oob = function(x, ...) x) +
      scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                         oob = function(x, ...) x) +
      coord_cartesian(clip = "off") +
      annotate(geom = "text", x = etI.ci[d, a + 2], y = -0.02,
               label = ifelse(length(ci[which(ci >= min(sens.plot$Level) &
                                                ci <= max(sens.plot$Level))]) > 1,
                              paste0(Drugs[d], ": ", Attributes[a], ", 95% CI"),
                              paste0(Drugs[d], ": ", Attributes[a])),
               size = 2.5) +
      labs(title = paste0("Sensitivity to ", Attributes[a],
                          " Effect Estimate for ", Drugs[d]),
           subtitle = "Induction Phase Model", color = "Drug") +
      xlab(paste(Attributes[a], "Effect Estimate")) +
      ylab("Aggregate Benefit-Risk Score")
    if (min(ci) > min(sens.plot$Level) & max(ci) < max(sens.plot$Level)) {
      gg <- gg + annotate("rect", 
                          xmin = ci[2], xmax = ci[3], ymin = 0, ymax = 1,
                          fill = "grey50", colour = NA, alpha = 0.5)
    }
    ggsave(paste0("1way_eff_mI_", Attributes[a], "_",
                  gsub(" ", "-", Drugs[d]), ".png"), plot = gg,
           width = 6, height = 4, units = "in")
    rm(gg, sens.plot, ci)
  }
}
rm(Attributes, Drugs, a, d, ci.y, ci.yend, ci.lt)

# Prep for Tornado Plot
Attributes <- swI$Attributes
tpI <- data.frame(matrix(nrow = 3, ncol = 5))
names(tpI) <- c("Attributes", "Min", "Max", "Plot.Min", "Plot.Max")
tpI$Attributes <- Attributes
for (a in 1:length(Attributes)) {
  tmp.ps.lower <- psI
  tmp.ps.upper <- psI
  tmp.ps.lower[tmp.ps.lower$Drug == "BRO 210", a + 1] <- 
    psI.lower[psI$Drug == "BRO 210", a + 1]
  tmp.ps.upper[tmp.ps.upper$Drug == "BRO 210", a + 1] <- 
    psI.upper[psI$Drug == "BRO 210", a + 1]
  tmp.brs.lower <- brs(tmp.ps.lower, swI)
  tmp.brs.upper <- brs(tmp.ps.upper, swI)
  tpI$Min[tpI$Attributes == Attributes[a]] <- 
    tmp.brs.lower$Total[tmp.brs.lower$Drug == "BRO 210"] -
    tmp.brs.lower$Total[tmp.brs.lower$Drug == "PBO"]
  tpI$Max[tpI$Attributes == Attributes[a]] <- 
    tmp.brs.upper$Total[tmp.brs.upper$Drug == "BRO 210"] -
    tmp.brs.upper$Total[tmp.brs.upper$Drug == "PBO"]
}
for (a in 1:nrow(tpI)) {
  tpI$Plot.Min[a] <- min(tpI$Min[a], tpI$Max[a])
  tpI$Plot.Max[a] <- max(tpI$Min[a], tpI$Max[a])
}
rm(a)

# Tornado Plot
gg <- ggplot() +
  geom_rect(data = tpI, mapping = aes(xmin = Plot.Min, xmax = Plot.Max,
                                      ymin = 3:1 - 0.4, ymax = 3:1 + 0.4,
                                      fill = Attributes)) +
  xlim(min(0, min(tpI$Plot.Min)), max(0, tpI$Plot.Max)) + 
  theme(legend.position = "none") +
  labs(title = "Tornado Plot: Uncertainty in Effect Estimates", 
       subtitle = "Induction Phase Model") +
  xlab("Aggregate Benefit-Risk Score Difference: BRO 210 vs PBO") +
  scale_y_continuous(breaks = 3:1, labels = Attributes)
ggsave("tornado_eff_mI_BRO-210.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg)

#### ONE-WAY SENSITIVITY TESTING: PREFERENCES ####

# Function for one-way sensitivity plots for Weights
one.wei <- function(et, sw, ppi, Attribute) {
  Weight <- rep(c(0, 1), nrow(et))
  for (d in 1:nrow(et)) {
    if (d == 1) {
      Drugs <- rep(et$Drug[d], length(Weight) / nrow(et))
    } else {
      Drugs <- c(Drugs, rep(et$Drug[d], length(Weight) / nrow(et)))
    }
  }
  out <- data.frame(Drugs, Weight)
  rm(Drugs, Weight)
  out$Score <- NA
  for (w in c(0, 1)) {
    tmp.sw <- sw
    if (w == 1) {
      tmp.sw$sw[tmp.sw$Attributes == Attribute] <- 100
      tmp.sw$sw[tmp.sw$Attributes != Attribute] <- 0
      tmp.sw$Weight <- tmp.sw$sw / sum(tmp.sw$sw)
    } else {
      tmp.sw$sw[tmp.sw$Attributes == Attribute] <- (w / (1 - w)) *
        sum(tmp.sw$sw[tmp.sw$Attributes != Attribute])
      tmp.sw$Weight <- tmp.sw$sw / sum(tmp.sw$sw)
    }
    tmp.ps <- pfs(et, ppi)
    tmp.br <- brs(tmp.ps, tmp.sw)
    for (d in 1:nrow(et)) {
      out$Score[out$Drugs == et$Drug[d] & out$Weight == w] <-
        tmp.br$Total[tmp.br$Drug == et$Drug[d]]
    }
    rm(tmp.sw, tmp.ps, tmp.br)
  }
  return(out)
}

# Sensitivity Analysis Plot (line chart)
Attributes <- swI$Attributes
for (a in 1:length(Attributes)) {
  sens.plot <- one.wei(etI, swI, ppI, Attributes[a])
  sens.plot$Drugs <- factor(sens.plot$Drugs,
                            levels = c("BRO 140", "BRO 210", "UST", "PBO"))
  gg <- ggplot(sens.plot,
               aes(x = Weight, y = Score, group = Drugs, color = Drugs)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                       oob = function(x, ...) x) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                       oob = function(x, ...) x) +
    coord_cartesian(clip = "off") +
    annotate(geom = "segment", x = swI$Weight[a], xend = swI$Weight[a],
             y = 0, yend = 1) +
    annotate(geom = "text", x = swI$Weight[a], y = -0.02,
             label = paste(Attributes[a], "mean weight, 95% CI"), size = 2.5) +
    labs(title = paste0("Sensitivity to ", Attributes[a], " Weight"),
         subtitle = "Induction Phase Model", color = "Drug") +
    xlab(paste(Attributes[a], "Weight")) +
    ylab("Aggregate Benefit-Risk Score")
  gg <- gg + annotate("rect", 
                      xmin = w.ci.est[Attributes[a], 1], 
                      xmax = w.ci.est[Attributes[a], 2], 
                      ymin = 0, ymax = 1,
                      fill = "grey50", colour = NA, alpha = 0.5)
  ggsave(paste0("1way_wei_mI_", Attributes[a], ".png"), plot = gg,
         width = 6, height = 4, units = "in")
  rm(gg, sens.plot)
}
rm(Attributes, a)

# Prep for Tornado Plot
Attributes <- swI$Attributes
tpI <- data.frame(matrix(nrow = 3, ncol = 5))
names(tpI) <- c("Attributes", "Min", "Max", "Plot.Min", "Plot.Max")
tpI$Attributes <- Attributes
for (a in 1:length(Attributes)) {
  sens.plot <- one.wei(etI, swI, ppI, Attributes[a])
  sens.plot$Drugs <- factor(sens.plot$Drugs,
                            levels = c("BRO 140", "BRO 210", "UST", "PBO"))
  tpI$Min[tpI$Attributes == Attributes[a]] <-
    ((sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 1] -
        sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 0]) / 
       1 * w.ci.est[Attributes[a], 1] + 
       sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 0]) - 
    ((sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 1] -
        sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 0]) / 
       1 * w.ci.est[Attributes[a], 1] + 
       sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 0])
  tpI$Max[tpI$Attributes == Attributes[a]] <-
    ((sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 1] -
        sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 0]) / 
       1 * w.ci.est[Attributes[a], 2] + 
       sens.plot$Score[sens.plot$Drugs == "BRO 210" & sens.plot$Weight == 0]) - 
    ((sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 1] -
        sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 0]) / 
       1 * w.ci.est[Attributes[a], 2] + 
       sens.plot$Score[sens.plot$Drugs == "PBO" & sens.plot$Weight == 0])
}
for (a in 1:nrow(tpI)) {
  tpI$Plot.Min[a] <- min(tpI$Min[a], tpI$Max[a])
  tpI$Plot.Max[a] <- max(tpI$Min[a], tpI$Max[a])
}
rm(a)

# Tornado Plot
gg <- ggplot() +
  geom_rect(data = tpI, mapping = aes(xmin = Plot.Min, xmax = Plot.Max,
                                      ymin = 3:1 - 0.4, ymax = 3:1 + 0.4,
                                      fill = Attributes)) +
  xlim(min(0, min(tpI$Plot.Min)), max(0, tpI$Plot.Max)) + 
  theme(legend.position = "none") +
  labs(title = "Tornado Plot: Uncertainty in Weights", 
       subtitle = "Induction Phase Model") +
  xlab("Aggregate Benefit-Risk Score Difference: BRO 210 vs PBO") +
  scale_y_continuous(breaks = 3:1, labels = Attributes)
ggsave("tornado_wei_mI.png", plot = gg, width = 6, height = 4, units = "in")
rm(gg, tpI)