\name{survey4_analyze}
\alias{survey4_analyze}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analyzes survey data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
survey4_analyze(survey_data = "data/confounding_democratic_peace_4_sim.sav", military_data = "data/NMC_v4_0.csv", trade_data = "data/dyadic_trade_3.0.csv")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey_data}{
%%     ~~Describe \code{survey_data} here~~
}
  \item{military_data}{
%%     ~~Describe \code{military_data} here~~
}
  \item{trade_data}{
%%     ~~Describe \code{trade_data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (survey_data = "data/confounding_democratic_peace_4_sim.sav", 
    military_data = "data/NMC_v4_0.csv", trade_data = "data/dyadic_trade_3.0.csv") 
{
    rm(list = ls())
    source("survey4_functions.R")
    d4 <- read_spss(survey_data)
    d <- d4[d4$workerid != "", ]
    d$Z <- ifelse(d$d03 == 1 | d$d07 == 1 | d$d08 == 1 | d$d09 == 
        1 | d$d10 == 1 | d$d13 == 1 | d$d14 == 1, 1, 0)
    d$Z[is.na(d$Z)] <- 0
    d$Z <- factor(x = d$Z, labels = c("Non-democracy", "Democracy"))
    d$V <- c()
    d$V[d$d02 == 1 | d$d03 == 1] <- 1
    d$V[d$d04 == 1 | d$d05 == 1 | d$d06 == 1 | d$d07 == 1 | d$d08 == 
        1 | d$d09 == 1 | d$d10 == 1 | d$d11 == 1] <- 2
    d$V[d$d12 == 1 | d$d13 == 1 | d$d14 == 1 | d$d15 == 1] <- 3
    d$V <- factor(x = d$V, labels = c("Basic", "Controlled Details", 
        "ENE"))
    d <- d[!is.na(d$V), ]
    d$NoAm <- region.lab(d$regions_1_1_Group, d$regions_2_1_Group)
    d$WeEu <- region.lab(d$regions_1_4_Group, d$regions_2_4_Group)
    d$MiEa <- region.lab(d$regions_1_6_Group, d$regions_2_6_Group)
    d$SuAf <- region.lab(d$regions_1_7_Group, d$regions_2_7_Group)
    d$EaAs <- region.lab(d$regions_1_8_Group, d$regions_2_8_Group)
    d$CeAs <- region.lab(d$regions_1_9_Group, d$regions_2_9_Group)
    d$region <- d$NoAm + d$WeEu - d$MiEa - d$SuAf
    d$region.s <- vscale(var = d$region, vig = d$V)
    region.s <- myreg(Y = d$region.s)
    region.n <- myreg(Y = d$region)
    robustse(lm(formula = region ~ Z + V + Z:V, data = d))
    robustse(lm(formula = region.s ~ Z + V + Z:V, data = d))
    d$gdp <- ifelse(is.na(d$gdp_1), d$gdp_2, d$gdp_1)
    qog <- read.dta("data/qog_bas_ts_jan15.dta")
    r_gdp <- qog$gle_cgdpc[qog$year == 2005]
    d$gdp <- relab(old.var = d$gdp, old.labs = 1:7, new.labs = c(median(r_gdp[r_gdp < 
        500], TRUE), median(r_gdp[r_gdp > 500 & r_gdp < 1001], 
        TRUE), median(r_gdp[r_gdp > 1000 & r_gdp < 5001], TRUE), 
        median(r_gdp[r_gdp > 5000 & r_gdp < 10001], TRUE), median(r_gdp[r_gdp > 
            10000 & r_gdp < 20001], TRUE), median(r_gdp[r_gdp > 
            20000 & r_gdp < 40001], TRUE), median(r_gdp[r_gdp > 
            40000], TRUE)))
    d$gdp.s <- vscale(var = d$gdp, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$gdp, v = j, x.limits = c(0, 60000), 
            x.breaks = seq(0, 60000, 10000), x.labels = seq(0, 
                60000, 10000), title = levels(d$V)[j], xlab = "GDP per Capita (USD)")
        print(test)
        ggsave(paste("images/gdp", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    gdp.s <- myreg(Y = d$gdp.s)
    gdp.n <- myreg(Y = d$gdp)
    robustse(lm(formula = gdp ~ Z + V + Z:V, data = d))
    robustse(lm(formula = gdp.s ~ Z + V + Z:V, data = d))
    d$religion <- ifelse(is.na(d$religion_1), d$religion_2, d$religion_1)
    probint <- c(mean(0:20), mean(21:40), mean(41:60), mean(61:80), 
        mean(81:100))
    likelylab <- c("Very Unlikely", "Unlikely", "Odds About Even", 
        "Likely", "Very Likely")
    d$religion <- relab(old.var = d$religion, old.labs = 1:5, 
        new.labs = probint)
    d$religion.s <- vscale(var = d$religion, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$religion, v = j, x.limits = c(0, 
            100), x.breaks = probint, x.labels = likelylab, title = levels(d$V)[j], 
            xlab = "Likelihood of Being Majority Christian")
        print(test)
        ggsave(paste("images/religion", j, ".pdf", sep = ""), 
            width = 5, heigh = 3.5)
    }
    religion.s <- myreg(Y = d$religion.s)
    religion.n <- myreg(Y = d$religion)
    robustse(lm(formula = religion ~ Z + V + Z:V, data = d))
    robustse(lm(formula = religion.s ~ Z + V + Z:V, data = d))
    d$oil <- ifelse(is.na(d$oil_1), d$oil_2, d$oil_1)
    d$oil <- relab(old.var = d$oil, old.labs = 1:5, new.labs = rev(probint))
    d$oil.o <- relab(old.var = ifelse(is.na(d$oil_1), d$oil_2, 
        d$oil_1), old.labs = 1:5, new.labs = probint)
    d$oil.s <- vscale(var = d$oil.o, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$oil, v = j, x.limits = c(0, 100), 
            x.breaks = probint, x.labels = likelylab, title = levels(d$V)[j], 
            xlab = "Likelihood of Having Large Oil Reserves")
        print(test)
        ggsave(paste("images/oil", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    oil.s <- myreg(Y = d$oil.s)
    oil.n <- myreg(Y = d$oil)
    robustse(lm(formula = oil ~ Z + V + Z:V, data = d))
    robustse(lm(formula = oil.s ~ Z + V + Z:V, data = d))
    d$white <- ifelse(is.na(d$white_1), d$white_2, d$white_1)
    probint <- c(mean(0:20), mean(21:40), mean(41:60), mean(61:80), 
        mean(81:100))
    likelylab <- c("Very Unlikely", "Unlikely", "Odds About Even", 
        "Likely", "Very Likely")
    d$white <- relab(old.var = d$white, old.labs = 1:5, new.labs = probint)
    d$white.s <- vscale(var = d$white, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$white, v = j, x.limits = c(0, 100), 
            x.breaks = probint, x.labels = likelylab, title = levels(d$V)[j], 
            xlab = "Likelihood of Being Majorit White")
        print(test)
        ggsave(paste("images/white", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    white.s <- myreg(Y = d$white.s)
    white.n <- myreg(Y = d$white)
    robustse(lm(formula = white ~ Z + V + Z:V, data = d))
    robustse(lm(formula = white.s ~ Z + V + Z:V, data = d))
    d$force <- ifelse(is.na(d$force_1), d$force_2, d$force_1)
    nmc <- read.csv(military_data)
    r_exp <- nmc$milex[nmc$year == 2005]/1000
    d$force <- relab(old.var = d$force, old.labs = 1:5, new.labs = c(median(r_exp[r_exp >= 
        0 & r_exp <= 30], TRUE), median(r_exp[r_exp > 30 & r_exp <= 
        120], TRUE), median(r_exp[r_exp > 120 & r_exp <= 600], 
        TRUE), median(r_exp[r_exp > 600 & r_exp <= 3500], TRUE), 
        median(r_exp[r_exp > 3500], TRUE)))
    d$force.o <- ifelse(is.na(d$force_1), d$force_2, d$force_1)
    d$force.o <- relab(old.var = d$force.o, old.labs = 1:5, new.labs = 0:4)
    d$force.s <- vscale(var = d$force, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = log(d$force), v = j, title = levels(d$V)[j], 
            xlab = "Spending on Military (Log Millions USD)")
        print(test)
        ggsave(paste("images/force", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    force.s <- myreg(Y = d$force.s)
    force.n <- myreg(Y = d$force)
    force.o <- myreg(Y = d$force.o)
    robustse(lm(formula = force ~ Z + V + Z:V, data = d))
    robustse(lm(formula = force.s ~ Z + V + Z:V, data = d))
    d$allies <- ifelse(is.na(d$alliance_1), d$alliance_2, d$alliance_1)
    d$allies <- relab(old.var = d$allies, old.labs = 1:5, new.labs = probint)
    d$allies.s <- vscale(var = d$allies, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$allies, v = j, x.limits = c(0, 100), 
            x.breaks = probint, x.labels = likelylab, title = levels(d$V)[j], 
            xlab = "Likelihood of Military Alliance with the U.S")
        print(test)
        ggsave(paste("images/allies", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    allies.s <- myreg(Y = d$allies.s)
    allies.n <- myreg(Y = d$allies)
    robustse(lm(formula = allies ~ Z + V + Z:V, data = d))
    robustse(lm(formula = allies.s ~ Z + V + Z:V, data = d))
    d$trade <- ifelse(is.na(d$trade_1), d$trade_2, d$trade_1)
    cow_trade <- read.csv(trade_data)
    cow_trade <- cow_trade[cow_trade$importer1 == "United States of America" & 
        cow_trade$year == 2005, ]
    r_trade <- cow_trade$flow1 + cow_trade$flow2
    d$trade <- relab(old.var = d$trade, old.labs = 1:5, new.labs = c(median(r_trade[r_trade >= 
        0 & r_trade <= 100], TRUE), median(r_trade[r_trade > 
        100 & r_trade <= 350], TRUE), median(r_trade[r_trade > 
        350 & r_trade <= 1500], TRUE), median(r_trade[r_trade > 
        1500 & r_trade <= 10000], TRUE), median(r_trade[r_trade > 
        10000], TRUE)))
    d$trade.o <- ifelse(is.na(d$trade_1), d$trade_2, d$trade_1)
    d$trade.o <- relab(old.var = d$trade.o, old.labs = 1:5, new.labs = 0:4)
    d$trade.s <- vscale(var = d$trade, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = log(d$trade), v = j, title = levels(d$V)[j], 
            xlab = "Total Trade Flow (Log Millions USD)")
        print(test)
        ggsave(paste("images/trade", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    trade.s <- myreg(Y = d$trade.s)
    trade.n <- myreg(Y = d$trade)
    trade.o <- myreg(Y = d$trade.o)
    robustse(lm(formula = trade ~ Z + V + Z:V, data = d))
    robustse(lm(formula = trade.s ~ Z + V + Z:V, data = d))
    d$exercise <- ifelse(is.na(d$exercise_1), d$exercise_2, d$exercise_1)
    d$exercise <- relab(old.var = d$exercise, old.labs = 1:5, 
        new.labs = probint)
    d$exercise.s <- vscale(var = d$exercise, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$exercise, v = j, x.limits = c(0, 
            100), x.breaks = probint, x.labels = likelylab, title = levels(d$V)[j], 
            xlab = "Likelihood of Joint Military Exercise with U.S.")
        print(test)
        ggsave(paste("images/exercise", j, ".pdf", sep = ""), 
            width = 5, heigh = 3.5)
    }
    exercise.s <- myreg(Y = d$exercise.s)
    exercise.n <- myreg(Y = d$exercise)
    robustse(lm(formula = exercise ~ Z + V + Z:V, data = d))
    robustse(lm(formula = exercise.s ~ Z + V + Z:V, data = d))
    sizelab <- c("Very Low", "Low", "Medium", "High", "Very High")
    d$invest <- ifelse(is.na(d$invest_1), d$invest_2, d$invest_1)
    d$invest <- relab(old.var = d$invest, old.labs = 1:5, new.labs = 0:4)
    d$invest.s <- vscale(var = d$invest, vig = d$V)
    for (j in 1:3) {
        test <- myden(Y = d$invest, v = j, x.limits = c(0, 6), 
            x.breaks = 1:5, x.labels = sizelab, title = levels(d$V)[j], 
            xlab = "Level of Investment in U.S. Businesses")
        print(test)
        ggsave(paste("images/invest", j, ".pdf", sep = ""), width = 5, 
            heigh = 3.5)
    }
    invest.s <- myreg(Y = d$invest.s)
    invest.n <- myreg(Y = d$invest)
    robustse(lm(formula = invest ~ Z + V + Z:V, data = d))
    robustse(lm(formula = invest.s ~ Z + V + Z:V, data = d))
    res.s <- data.frame(rbind(region.s, gdp.s, religion.s, white.s, 
        oil.s, force.s, allies.s, trade.s, exercise.s, invest.s))
    names(res.s) <- c("coef", "se")
    res.s$v <- rep(levels(d$V), 10)
    res.s$v2 <- factor(res.s$v, levels = levels(factor(res.s$v)))
    res.s$v3 <- factor(res.s$v, levels = rev(levels(factor(res.s$v))))
    p.labs <- c("C: Most Likely Region", "D: GDP per Capita", 
        "E: Likelihood of Being Majority Christian", "F: Likelihood of Being Majority White", 
        "G: Likelihood of Having Large Oil Reserves", "H: Military Spending*", 
        "I: Likelihood of Military Alliance with U.S.*", "J: Trade with U.S.*", 
        "L: Likelihood of Joint Military Exercise with U.S.**", 
        "L: Level of Investment in U.S. Businesses**")
    p.labs.func <- function(x, times = 3) {
        rep(p.labs[x], times)
    }
    res.s$placebo <- unlist(lapply(1:length(p.labs), p.labs.func))
    f <- ggplot(res.s, aes(x = coef, y = v3, shape = v2, color = v2))
    f <- f + geom_vline(xintercept = 0, linetype = "longdash") + 
        geom_errorbarh(aes(xmax = coef + 2.576 * se, xmin = coef - 
            2.576 * se), size = 0.6, height = 0) + geom_errorbarh(aes(xmax = coef + 
        1.96 * se, xmin = coef - 1.96 * se), size = 1, height = 0) + 
        geom_point(stat = "identity", size = 3.5, fill = "white") + 
        scale_color_manual(name = "Vignette Type", values = c("firebrick3", 
            "forestgreen", "dodgerblue3")) + scale_shape_manual(name = "Vignette Type", 
        values = c(21, 22, 23))
    f + facet_wrap(~placebo, ncol = 1) + theme_bbtop() + xlab("Standardized Difference (Dem-NonDem)") + 
        ylab("") + scale_y_discrete(breaks = NULL)
    ggsave("images/coef_plot_main_s.pdf", height = 7, width = 5.5, 
        dpi = 600)
    r_c <- region.s ~ Z
    r_d <- gdp.s ~ Z
    r_e <- religion.s ~ Z
    r_f <- oil.s ~ Z
    r_g <- white.s ~ Z
    r_h <- force.s ~ Z
    r_i <- allies.s ~ Z
    r_j <- trade.s ~ Z
    r_k <- exercise.s ~ Z
    r_l <- invest.s ~ Z
    for (i in 1:3) {
        fitsur <- systemfit(list(r.c = r_c, r.d = r_d, r.e = r_e, 
            r.f = r_f, r.g = r_g, r.h = r_h, r.i = r_i, r.j = r_j, 
            r.k = r_k, r.l = r_l), data = d[d$V == levels(d$V)[i], 
            ])
        summary(fitsur)
        restriction <- c("r.c_ZDemocracy = 0", "r.d_ZDemocracy = 0", 
            "r.e_ZDemocracy = 0", "r.f_ZDemocracy = 0", "r.g_ZDemocracy = 0", 
            "r.h_ZDemocracy = 0", "r.i_ZDemocracy = 0", "r.j_ZDemocracy = 0", 
            "r.k_ZDemocracy = 0", "r.l_ZDemocracy = 0")
        print(linearHypothesis(fitsur, restriction, test = "Chisq", 
            white.adjust = TRUE))
        print(linearHypothesis(fitsur, restriction, test = "F", 
            white.adjust = TRUE))
    }
    d$R1_1 <- multiq(d$dem_a_1, d$dem_b_1, d$dem_c_1, d$dem2a_1, 
        d$dem2b_1, d$dem2c_1)
    d$R1_1 <- relab(old.var = d$R1_1, old.labs = 1:5, new.labs = probint)
    d$R1_2 <- multiq(d$dem_a_2, d$dem_b_2, d$dem_c_2, d$dem2a_2, 
        d$dem2b_2, d$dem2c_2)
    d$R1_2 <- relab(old.var = d$R1_2, old.labs = 1:5, new.labs = probint)
    d$R1_3 <- multiq(d$dem_a_3, d$dem_b_3, d$dem_c_3, d$dem2a_3, 
        d$dem2b_3, d$dem2c_3)
    d$R1_3 <- relab(old.var = d$R1_3, old.labs = 1:5, new.labs = probint)
    d$R1_4 <- multiq(d$dem_a_4, d$dem_b_4, d$dem_c_4, d$dem2a_4, 
        d$dem2b_4, d$dem2c_4)
    d$R1_4 <- relab(old.var = d$R1_4, old.labs = 1:5, new.labs = probint)
    d$R1_5 <- multiq(d$dem_a_5, d$dem_b_5, d$dem_c_5, d$dem2a_5, 
        d$dem2b_5, d$dem2c_5)
    d$R1_5 <- relab(old.var = d$R1_5, old.labs = 1:5, new.labs = probint)
    for (i in 1:nrow(d)) {
        s <- d$R1_1[i] + d$R1_2[i] + d$R1_3[i] + d$R1_4[i] + 
            d$R1_5[i]
        d$R1_1[i] <- d$R1_1[i]/s
        d$R1_2[i] <- d$R1_2[i]/s
        d$R1_3[i] <- d$R1_3[i]/s
        d$R1_4[i] <- d$R1_4[i]/s
        d$R1_5[i] <- d$R1_5[i]/s
    }
    d$R1_sum <- d$R1_1 + d$R1_2 + d$R1_3 + d$R1_4 + d$R1_5
    d$R1 <- 10 * d$R1_1 + mean(6:9) * d$R1_2 + mean(1:5) * d$R1_3 + 
        mean(-5:0) * d$R1_4 + mean(-10:-6) * d$R1_5
    R1 <- myreg(Y = d$R1, Z = d$Z, V = d$V)
    d$R2 <- psum(d$demchar_1, d$demchar_4, d$demchar_5, d$demchar_6, 
        d$demchar_7, d$demchar_8, na.rm = F)
    R2 <- myreg(Y = d$R2)
    d$support <- ifelse(is.na(d$support_1), d$support_2 - 1, 
        d$support_1 - 1)
    d$support[d$support > 4] <- NA
    support <- myreg(Y = d$support)
    d$support2 <- d$support
    d$support2[is.na(d$support2)] <- 2
    support2 <- myreg(Y = d$support2)
    myreg.iv(Y = d$support[!is.na(d$support)], D = d$R1[!is.na(d$support)], 
        Z = d$Z[!is.na(d$support)], V = d$V[!is.na(d$support)])
    myreg.iv(Y = d$support2[!is.na(d$support2)], D = d$R1[!is.na(d$support2)], 
        Z = d$Z[!is.na(d$support2)], V = d$V[!is.na(d$support2)])
    d$college <- ifelse(d$educ > 3, 1, 0)
    d$democrat <- ifelse(d$partyid < 3, 1, 0)
    d$age_num <- ifelse(d$age == "older than 100", 101, d$age + 
        18)
    d$male <- ifelse(d$sex == 1, 1, 0)
    for (i in 1:3) {
        robustse(lm(as.numeric(Z) - 1 ~ college, data = d[d$V == 
            levels(d$V)[i], ]))
        robustse(lm(as.numeric(Z) - 1 ~ democrat, data = d[d$V == 
            levels(d$V)[i], ]))
        robustse(lm(as.numeric(Z) - 1 ~ age_num, data = d[d$V == 
            levels(d$V)[i], ]))
        robustse(lm(as.numeric(Z) - 1 ~ male, data = d[d$V == 
            levels(d$V)[i], ]))
        robustse(lm(as.numeric(Z) - 1 ~ poliid_1, data = d[d$V == 
            levels(d$V)[i], ]))
        overall <- felm(as.numeric(Z) - 1 ~ college + democrat + 
            age_num + male + poliid_1, data = d[d$V == levels(d$V)[i], 
            ])
        summary(object = overall, robust = TRUE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
