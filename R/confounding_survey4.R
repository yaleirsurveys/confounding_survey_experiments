######################################
# DEMOCRATIC PEACE Survey 4 CODE

#The primary statement of our analysis plan is in our pre-analysis plan in Open
#Science Framework Storage. In this GitHub repo, we have also included
#partially complete code and simulated data related to our pre-analysis plan; 
#the pre-analysis plan supersedes the code in any areas of ambiguity or difference.

survey4_analyze <- function(images_directory = NULL, output_print = FALSE,
                            return_main = FALSE) {
  # save images by specifying a images directory
  # this is data simulated from Qualtric's "test survey" function
  if (output_print) {
    print(paste0("Complied Date and Time: ", Sys.time()))
  }
  d4 <- read_spss(system.file("extdata", "confounding_democratic_peace_4_sim.sav",
                              package = "confounding"))
  d <- d4[d4$workerid != "",] # only completed responses
  ##########################################3
  # Treatment Assignments Variables
  # Democracy Vignette
  d$Z <- ifelse(d$d03==1 | d$d07==1 | d$d08==1 |
                  d$d09==1 | d$d10==1 | d$d13==1 | d$d14==1, 1, 0)
  d$Z[is.na(d$Z)] <- 0 
  d$Z <- factor(x = d$Z, labels = c("Non-democracy", "Democracy"))
  # Vignette Types
  d$V <- c()
  # Basic 
  d$V[d$d02==1 | d$d03==1] <- 1
  # Controlled Details
  d$V[d$d04==1 | d$d05==1 | d$d06==1 |
        d$d07==1 | d$d08==1 | d$d09==1 |
        d$d10==1 | d$d11==1] <- 2
  # ENE
  d$V[d$d12==1 | d$d13==1 | d$d14==1 | d$d15==1] <- 3
  # make a factor for d$V
  d$V <- factor(x = d$V, labels = c("Basic", "Controlled Details", "ENE"))
  d <- d[!is.na(d$V),]
  # size of data set
  if (output_print) {
    dim(d)
  }
  
  ############################################
  # Placebo Test C: Regions of the World
  
  # 1) Dimension Reduction
  d$NoAm <- region.lab(d$regions_1_1_Group, d$regions_2_1_Group)
  d$WeEu <- region.lab(d$regions_1_4_Group, d$regions_2_4_Group)
  d$MiEa <- region.lab(d$regions_1_6_Group, d$regions_2_6_Group)
  d$SuAf <- region.lab(d$regions_1_7_Group, d$regions_2_7_Group)
  d$EaAs <- region.lab(d$regions_1_8_Group, d$regions_2_8_Group)
  d$CeAs <- region.lab(d$regions_1_9_Group, d$regions_2_9_Group)
  d$region <- d$NoAm+d$WeEu-d$MiEa-d$SuAf
  # standardized
  d$region.s <- vscale(var = d$region, vig = d$V)
  # regression: standardized
  region.s <- myreg(Y = d$region.s, Z = d$Z, V = d$V)
  colnames(region.s) <- c("Coef", "SE")
  # regression: non-standardized
  region.n <- myreg(Y = d$region, , Z = d$Z, V = d$V)
  colnames(region.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  region.1 <- robustse(lm(formula = region ~ Z + V + Z:V, data = d))
  region.2 <- robustse(lm(formula = region.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test C: Regions of the World")
    print(region.s)
    print(region.n)
    print(region.1)
    print(region.2)
  }
  
  ############################################
  # Placebo Test D: GDP per Capita
  d$gdp <- ifelse(is.na(d$gdp_1), d$gdp_2, d$gdp_1)
  # relabel 
  # real-world median values for each interval
  qog <- read.dta(system.file("extdata", "qog_bas_ts_jan15.dta",
                              package = "confounding"))
  r_gdp <- qog$gle_cgdpc[qog$year==2005]
  d$gdp <- relab(old.var = d$gdp, old.labs = 1:7, 
                 new.labs = c(median(r_gdp[r_gdp < 500], TRUE), 
                              median(r_gdp[r_gdp > 500 & r_gdp < 1001], TRUE),
                              median(r_gdp[r_gdp > 1000 & r_gdp < 5001], TRUE),
                              median(r_gdp[r_gdp > 5000 & r_gdp < 10001], TRUE),
                              median(r_gdp[r_gdp > 10000 & r_gdp < 20001], TRUE),
                              median(r_gdp[r_gdp > 20000 & r_gdp < 40001], TRUE),
                              median(r_gdp[r_gdp > 40000], TRUE)))
  # standardize
  d$gdp.s <- vscale(var = d$gdp, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$gdp, V=d$V, Z=d$Z, v = j, x.limits=c(0, 60000), 
                  x.breaks=seq(0,60000,10000), 
                  x.labels=seq(0,60000,10000),
                  title=levels(d$V)[j], xlab="GDP per Capita (USD)")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/gdp", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  gdp.s <- myreg(Y = d$gdp.s, Z = d$Z, V = d$V)
  colnames(gdp.s) <- c("Coef", "SE")
  # regression: non-standardized
  gdp.n <- myreg(Y = d$gdp, Z = d$Z, V = d$V)
  colnames(gdp.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  gdp.1 <- robustse(lm(formula = gdp ~ Z + V + Z:V, data = d))
  gdp.2 <- robustse(lm(formula = gdp.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test D: GDP per Capita")
    print(gdp.s)
    print(gdp.n)
    print(gdp.1)
    print(gdp.2)
  }
  
  ############################################
  # Placebo Test E: Religion
  d$religion <- ifelse(is.na(d$religion_1), d$religion_2, d$religion_1)
  # relabel 
  probint <- c(mean(0:20), mean(21:40), mean(41:60), mean(61:80), mean(81:100))
  likelylab <- c("Very Unlikely", "Unlikely", "Odds About Even", 
                 "Likely", "Very Likely")
  d$religion <- relab(old.var = d$religion, old.labs = 1:5, 
                      new.labs = probint)
  # standardize
  d$religion.s <- vscale(var = d$religion, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$religion, V=d$V, Z=d$Z, v = j, x.limits=c(0, 100), 
                  x.breaks=probint, 
                  x.labels=likelylab,
                  title=levels(d$V)[j], 
                  xlab="Likelihood of Being Majority Christian")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/religion", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  religion.s <- myreg(Y = d$religion.s, Z = d$Z, V = d$V)
  colnames(religion.s) <- c("Coef", "SE")
  # regression: non-standardized
  religion.n <- myreg(Y = d$religion, Z = d$Z, V = d$V)
  colnames(religion.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  religion.1 <- robustse(lm(formula = religion ~ Z + V + Z:V, data = d))
  religion.2 <- robustse(lm(formula = religion.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test E: Religion")
    print(religion.s)
    print(religion.n)
    print(religion.1)
    print(religion.2)
  }
  
  ############################################
  # Placebo Test F: Oil Reserves
  d$oil <- ifelse(is.na(d$oil_1), d$oil_2, d$oil_1)
  # relabel 
  d$oil <- relab(old.var = d$oil, old.labs = 1:5, 
                 new.labs = rev(probint))
  d$oil.o <- relab(old.var = ifelse(is.na(d$oil_1), d$oil_2, d$oil_1), 
                   old.labs = 1:5, 
                   new.labs = probint)
  # standardize
  d$oil.s <- vscale(var = d$oil.o, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$oil, V=d$V, Z=d$Z, v = j, x.limits=c(0, 100), 
                  x.breaks=probint, 
                  x.labels=likelylab,
                  title=levels(d$V)[j], 
                  xlab="Likelihood of Having Large Oil Reserves")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/oil", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  oil.s <- myreg(Y = d$oil.s, Z = d$Z, V = d$V)
  colnames(oil.s) <- c("Coef", "SE")
  # regression: non-standardized
  oil.n <- myreg(Y = d$oil, Z = d$Z, V = d$V)
  colnames(oil.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  oil.1 <- robustse(lm(formula = oil ~ Z + V + Z:V, data = d))
  oil.2 <- robustse(lm(formula = oil.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test F: Oil Reserves")
    print(oil.s)
    print(oil.n)
    print(oil.1)
    print(oil.2)
  }
  
  ############################################
  # Placebo Test G: White
  d$white <- ifelse(is.na(d$white_1), d$white_2, d$white_1)
  # relabel 
  probint <- c(mean(0:20), mean(21:40), mean(41:60), mean(61:80), mean(81:100))
  likelylab <- c("Very Unlikely", "Unlikely", "Odds About Even", 
                 "Likely", "Very Likely")
  d$white <- relab(old.var = d$white, old.labs = 1:5, 
                   new.labs = probint)
  # standardize
  d$white.s <- vscale(var = d$white, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$white, V=d$V, Z=d$Z, v = j, x.limits=c(0, 100), 
                  x.breaks=probint, 
                  x.labels=likelylab,
                  title=levels(d$V)[j], 
                  xlab="Likelihood of Being Majority White")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/white", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  white.s <- myreg(Y = d$white.s, Z = d$Z, V = d$V)
  colnames(white.s) <- c("Coef", "SE")
  # regression: non-standardized
  white.n <- myreg(Y = d$white, Z = d$Z, V = d$V)
  colnames(white.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  white.1 <- robustse(lm(formula = white ~ Z + V + Z:V, data = d))
  white.2 <- robustse(lm(formula = white.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test G: White")
    print(white.s)
    print(white.n)
    print(white.1)
    print(white.2)
  }
  
  ############################################
  # Placebo Test H: Military Capability
  d$force <- ifelse(is.na(d$force_1), d$force_2, d$force_1)
  # relabel 
  # real-world median values in each interval
  nmc <- read.csv(system.file("extdata", "NMC_v4_0.csv",
                              package = "confounding"))
  r_exp <- nmc$milex[nmc$year == 2005]/1000
  d$force <- relab(old.var = d$force, old.labs = 1:5, 
                   new.labs = c(median(r_exp[r_exp >= 0 & r_exp <= 30], TRUE),
                                median(r_exp[r_exp > 30 & r_exp <= 120], TRUE),
                                median(r_exp[r_exp > 120 & r_exp <= 600], TRUE),
                                median(r_exp[r_exp > 600 & r_exp <= 3500], TRUE),
                                median(r_exp[r_exp > 3500], TRUE)))
  # ordinal coding
  d$force.o <- ifelse(is.na(d$force_1), d$force_2, d$force_1)
  d$force.o <- relab(old.var = d$force.o, old.labs = 1:5, new.labs = 0:4)
  # standardize
  d$force.s <- vscale(var = d$force, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=log(d$force), V=d$V, Z=d$Z, v = j,
                  title=levels(d$V)[j], 
                  xlab="Spending on Military (Log Millions USD)")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/force", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  force.s <- myreg(Y = d$force.s, Z = d$Z, V = d$V)
  colnames(force.s) <- c("Coef", "SE")
  # regression: non-standardized
  force.n <- myreg(Y = d$force, Z = d$Z, V = d$V)
  colnames(force.n) <- c("Coef", "SE")
  # regression: ordinal coding
  force.o <- myreg(Y = d$force.o, Z = d$Z, V = d$V)
  colnames(force.o) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  force.1 <- robustse(lm(formula = force ~ Z + V + Z:V, data = d))
  force.2 <- robustse(lm(formula = force.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test H: Military Capability")
    print(force.s)
    print(force.n)
    print(force.o)
    print(force.1)
    print(force.2)
  }
  
  ############################################
  # Placebo Test I: Military Alliance
  d$allies <- ifelse(is.na(d$alliance_1), d$alliance_2, d$alliance_1)
  # relabel 
  d$allies <- relab(old.var = d$allies, old.labs = 1:5, 
                    new.labs = probint)
  # standardize
  d$allies.s <- vscale(var = d$allies, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$allies, V=d$V, Z=d$Z, v = j, x.limits=c(0, 100), 
                  x.breaks=probint, 
                  x.labels=likelylab,
                  title=levels(d$V)[j], 
                  xlab="Likelihood of Military Alliance with the U.S")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/allies", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  allies.s <- myreg(Y = d$allies.s, Z = d$Z, V = d$V)
  colnames(allies.s) <- c("Coef", "SE")
  # regression: non-standardized
  allies.n <- myreg(Y = d$allies, Z = d$Z, V = d$V)
  colnames(allies.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  allies.1 <- robustse(lm(formula = allies ~ Z + V + Z:V, data = d))
  allies.2 <- robustse(lm(formula = allies.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test I: Military Alliance")
    print(allies.s)
    print(allies.n)
    print(allies.1)
    print(allies.2)
  }
  
  ############################################
  # Placebo Test J: Trade with the U.S.
  d$trade <- ifelse(is.na(d$trade_1), d$trade_2, d$trade_1)
  # relabel 
  # get real-world median values for each interval
  cow_trade <- read.csv(system.file("extdata", "dyadic_trade_3.0.csv",
                                    package = "confounding"))
  cow_trade <- cow_trade[cow_trade$importer1=="United States of America" & 
                           cow_trade$year==2005,]
  r_trade <- cow_trade$flow1 + cow_trade$flow2
  d$trade <- relab(old.var = d$trade, old.labs = 1:5, 
                   new.labs = c(median(r_trade[r_trade >= 0 & r_trade <= 100], TRUE),
                                median(r_trade[r_trade > 100 & r_trade <= 350], TRUE),
                                median(r_trade[r_trade > 350 & r_trade <= 1500], TRUE),
                                median(r_trade[r_trade > 1500 & r_trade <= 10000], TRUE),
                                median(r_trade[r_trade > 10000], TRUE)))
  # ordinal coding 
  d$trade.o <- ifelse(is.na(d$trade_1), d$trade_2, d$trade_1)
  d$trade.o <- relab(old.var = d$trade.o, old.labs = 1:5, new.labs = 0:4)
  # standardize
  d$trade.s <- vscale(var = d$trade, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=log(d$trade), V=d$V, Z=d$Z, v = j,
                  title=levels(d$V)[j], 
                  xlab="Total Trade Flow (Log Millions USD)")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/trade", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  trade.s <- myreg(Y = d$trade.s, Z = d$Z, V = d$V)
  colnames(trade.s) <- c("Coef", "SE")
  # regression: non-standardized
  trade.n <- myreg(Y = d$trade, Z = d$Z, V = d$V)
  colnames(trade.n) <- c("Coef", "SE")
  # regression: ordinal coding
  trade.o <- myreg(Y = d$trade.o, Z = d$Z, V = d$V)
  colnames(trade.o) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  trade.1 <- robustse(lm(formula = trade ~ Z + V + Z:V, data = d))
  trade.2 <- robustse(lm(formula = trade.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test J: Trade with the U.S.")
    print(trade.s)
    print(trade.n)
    print(trade.o)
    print(trade.1)
    print(trade.2)
  }
  
  ############################################
  # Placebo Test K: Joint Military Exercise
  d$exercise <- ifelse(is.na(d$exercise_1), d$exercise_2, d$exercise_1)
  # relabel 
  d$exercise <- relab(old.var = d$exercise, old.labs = 1:5, 
                      new.labs = probint)
  # standardize
  d$exercise.s <- vscale(var = d$exercise, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$exercise, V=d$V, Z=d$Z, v = j, x.limits=c(0, 100), 
                  x.breaks=probint, 
                  x.labels=likelylab,
                  title=levels(d$V)[j], 
                  xlab="Likelihood of Joint Military Exercise with U.S.")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/exercise", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  exercise.s <- myreg(Y = d$exercise.s, Z = d$Z, V = d$V)
  colnames(exercise.s)  <- c("Coef", "SE")
  # regression: non-standardized
  exercise.n <- myreg(Y = d$exercise, Z = d$Z, V = d$V)
  colnames(exercise.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  exercise.1 <- robustse(lm(formula = exercise ~ Z + V + Z:V, data = d))
  exercise.2 <- robustse(lm(formula = exercise.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test K: Joint Military Exercise")
    print(exercise.s)
    print(exercise.n)
    print(exercise.1)
    print(exercise.2)
  }
  
  ############################################
  # Placebo Test L: Foreign Direct Investment
  sizelab <- c("Very Low", "Low", "Medium", "High", "Very High")
  d$invest <- ifelse(is.na(d$invest_1), d$invest_2, d$invest_1)
  # relabel 
  d$invest <- relab(old.var = d$invest, old.labs = 1:5, 
                    new.labs = 0:4)
  # standardize
  d$invest.s <- vscale(var = d$invest, vig = d$V)
  # graphing distribution
  for (j in 1:3) {
    my_ggplot <- myden(Y=d$invest, V=d$V, Z=d$Z, v = j, x.limits=c(0, 6), 
                  x.breaks=1:5, 
                  x.labels=sizelab,
                  title=levels(d$V)[j], 
                  xlab="Level of Investment in U.S. Businesses")
    if (!is.null(images_directory)) {
      ggsave(paste0(images_directory, "/invest", j, ".pdf"), width=5, heigh=3.5) 
    } else if (output_print) {
      print(my_ggplot)
    }
  }
  # regression: standardized 
  invest.s <- myreg(Y = d$invest.s, Z = d$Z, V = d$V)
  colnames(invest.s) <- c("Coef", "SE")
  # regression: non-standardized
  invest.n <- myreg(Y = d$invest, Z = d$Z, V = d$V)
  colnames(invest.n) <- c("Coef", "SE")
  # regression with dummy variables and interactions
  invest.1 <- robustse(lm(formula = invest ~ Z + V + Z:V, data = d))
  invest.2 <- robustse(lm(formula = invest.s ~ Z + V + Z:V, data = d))
  if (output_print) {
    print("Placebo Test L: Foreign Direct Investment")
    print(invest.s)
    print(invest.n)
    print(invest.1)
    print(invest.2)
  }
  
  #######################################
  # Make Coefficient Plot
  
  #### standardized
  res.s <- data.frame(rbind(region.s, gdp.s, religion.s, white.s,
                            oil.s, force.s, allies.s, trade.s, exercise.s, invest.s))
  names(res.s) <- c("coef", "se")
  res.s$v <- rep(levels(d$V), 10)
  # vignette labels
  res.s$v2 <- factor(res.s$v, levels=levels(factor(res.s$v)))
  res.s$v3 <- factor(res.s$v, levels=rev(levels(factor(res.s$v))))
  p.labs <- c("C: Most Likely Region", 
              "D: GDP per Capita",
              "E: Likelihood of Being Majority Christian",
              "F: Likelihood of Being Majority White",
              "G: Likelihood of Having Large Oil Reserves",
              "H: Military Spending*",
              "I: Likelihood of Military Alliance with U.S.*",
              "J: Trade with U.S.*",
              "L: Likelihood of Joint Military Exercise with U.S.**",
              "L: Level of Investment in U.S. Businesses**")
  p.labs.func <- function(x, times=3) {rep(p.labs[x],times)}
  res.s$placebo <- unlist(lapply(1:length(p.labs), p.labs.func))
  # ggplot 
  f <- ggplot(res.s, aes(x=coef,y=v3,shape=v2,color=v2))
  f <- f+geom_vline(xintercept=0, linetype="longdash")+
    geom_errorbarh(aes(xmax =  coef + 2.576*se, 
                       xmin = coef - 2.576*se),
                   size=0.6, height=0) +
    geom_errorbarh(aes(xmax =  coef + 1.96*se, 
                       xmin = coef - 1.96*se),
                   size=1.0, height=0)+
    geom_point(stat="identity",size=3.5,fill="white")+
    scale_color_manual(name="Vignette Type",
                       values=c("firebrick3","forestgreen","dodgerblue3"))+
    scale_shape_manual(name="Vignette Type",values=c(21,22,23))
  my_ggplot_main <- f + facet_wrap( ~ placebo, ncol=1)+
    theme_bbtop()+
    xlab("Standardized Difference (Dem-NonDem)")+
    ylab("")+scale_y_discrete(breaks=NULL)
  if (!is.null(images_directory)) {
    my_ggplot_main
    ggsave(paste0(images_directory, "/coef_plot_main_s", ".pdf"), 
           height=7, width=5.5,dpi = 600)
  }
  
  #######################################
  # Seemingly Unrelated Regression
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
                             r.f = r_f, r.g = r_g, r.h = r_h,
                             r.i = r_i, r.j = r_j, r.k = r_k,
                             r.l = r_l), data = d[d$V==levels(d$V)[i],])
    summary(fitsur)
    restriction <- c("r.c_ZDemocracy = 0", "r.d_ZDemocracy = 0",
                     "r.e_ZDemocracy = 0", "r.f_ZDemocracy = 0",
                     "r.g_ZDemocracy = 0", "r.h_ZDemocracy = 0",
                     "r.i_ZDemocracy = 0", "r.j_ZDemocracy = 0",
                     "r.k_ZDemocracy = 0", "r.l_ZDemocracy = 0")
    # joint tests
    joint <- linearHypothesis(fitsur, restriction, 
                           test = "Chisq", white.adjust = TRUE)  
    if (output_print) {
      print("Joint Test")
      print(joint)
    }
  }
  
  #######################################
  # Treatment Measure 1
  # full democracy
  d$R1_1 <- multiq(v1 = d$dem_a_1, v2 = d$dem_b_1, v3 = d$dem_c_1)
  d$R1_1 <- relab(old.var = d$R1_1, old.labs = 1:5, new.labs = probint)
  # democracy
  d$R1_2 <- multiq(d$dem_a_2, d$dem_b_2, d$dem_c_2)
  d$R1_2 <- relab(old.var = d$R1_2, old.labs = 1:5, new.labs = probint)
  # semi-democracy
  d$R1_3 <- multiq(d$dem_a_3, d$dem_b_3, d$dem_c_3)
  d$R1_3 <- relab(old.var = d$R1_3, old.labs = 1:5, new.labs = probint)
  # semi-autocracy
  d$R1_4 <- multiq(d$dem_a_4, d$dem_b_4, d$dem_c_4)
  d$R1_4 <- relab(old.var = d$R1_4, old.labs = 1:5, new.labs = probint)
  # full autocracy
  d$R1_5 <- multiq(d$dem_a_5, d$dem_b_5, d$dem_c_5)
  d$R1_5 <- relab(old.var = d$R1_5, old.labs = 1:5, new.labs = probint)
  # normalize
  for (i in 1:nrow(d)) {
    s <- d$R1_1[i] + d$R1_2[i] + d$R1_3[i] + d$R1_4[i] + d$R1_5[i]
    d$R1_1[i] <- d$R1_1[i]/s
    d$R1_2[i] <- d$R1_2[i]/s
    d$R1_3[i] <- d$R1_3[i]/s
    d$R1_4[i] <- d$R1_4[i]/s
    d$R1_5[i] <- d$R1_5[i]/s
  }
  # check the probabilities sum up to 1
  d$R1_sum <- d$R1_1 + d$R1_2 + d$R1_3 + d$R1_4 + d$R1_5
  # impute Polity score
  d$R1 <- 10*d$R1_1+mean(6:9)*d$R1_2+mean(1:5)*d$R1_3+
    mean(-5:0)*d$R1_4+mean(-10:-6)*d$R1_5
  # regression 
  R1 <- myreg(Y = d$R1, Z = d$Z, V = d$V)
  colnames(R1) <- c("Coef", "SE")
  
  # Treatment Measure 2: Characteristics of Democracy
  d$R2 <- psum(d$demchar_1, d$demchar_4, d$demchar_5, d$demchar_6,
               d$demchar_7, d$demchar_8, na.rm=F)
  # regression
  R2 <- myreg(Y = d$R2, Z = d$Z, V = d$V)
  colnames(R2) <- c("Coef", "SE")
  
  if (output_print) {
    print("Treatment Measures")
    print(R1)
    print(R2)
  }
  
  ###############################
  # ITT estimate of democracy on support for military action
  d$support <- ifelse(is.na(d$support_1), d$support_2-1, d$support_1-1)
  # coding #1: set "don't know" to NA
  d$support[d$support>4] <- NA
  support <- myreg(Y = d$support, Z = d$Z, V = d$V)
  colnames(support) <- c("Coef", "SE")
  # coding #2: set "don't know" to 2
  d$support2 <- d$support
  d$support2[is.na(d$support2)] <- 2
  support2 <- myreg(Y = d$support2, Z = d$Z, V = d$V)
  colnames(support2)  <- c("Coef", "SE")
  if (output_print){
    print(support)
    print(support2)
  }
  ###############################
  # IV estimate of democracy on support for military action
  # using coding 1 for support
  iv.res1 <- myreg.iv(Y = d$support[!is.na(d$support)], 
           D = d$R1[!is.na(d$support)], 
           Z = d$Z[!is.na(d$support)], 
           V = d$V[!is.na(d$support)])
  colnames(iv.res1) <- c("Coef", "SE")
  # using coding 2 for support
  iv.res2 <- myreg.iv(Y = d$support2[!is.na(d$support2)], 
           D = d$R1[!is.na(d$support2)], 
           Z = d$Z[!is.na(d$support2)], 
           V = d$V[!is.na(d$support2)])
  colnames(iv.res2) <- c("Coef", "SE")
  
  if (output_print) {
    print("IV: don't know as NA")
    print(iv.res1)
    print("IV: don't know as neither support nor oppose")
    print(iv.res2)
  }
  
  ############################################
  # Balance Tests
  
  # clean up
  d$college <- ifelse(d$educ > 3, 1, 0)
  d$democrat <- ifelse(d$partyid < 3, 1, 0)
  d$age_num <- ifelse(d$age=="older than 100", 101, d$age+18)
  d$male <- ifelse(d$sex == 1, 1, 0)
  
  # balance tests
  for (i in 1:3) {
    b1 <- robustse(lm(as.numeric(Z)-1 ~ college, data = d[d$V==levels(d$V)[i],]))
    b2 <- robustse(lm(as.numeric(Z)-1 ~ democrat, data = d[d$V==levels(d$V)[i],]))
    b3 <- robustse(lm(as.numeric(Z)-1 ~ age_num, data = d[d$V==levels(d$V)[i],]))
    b4 <- robustse(lm(as.numeric(Z)-1 ~ male, data = d[d$V==levels(d$V)[i],]))
    b5 <- robustse(lm(as.numeric(Z)-1 ~ poliid_1, data = d[d$V==levels(d$V)[i],]))  
    overall <- felm(as.numeric(Z)-1 ~ college + democrat + age_num +
                      male + poliid_1, data = d[d$V==levels(d$V)[i],])
    b_overall <- summary(object = overall, robust = TRUE)
    if (output_print) {
      print("Balance Tests")
      print(b1)
      print(b2)
      print(b3)
      print(b4)
      print(b5)
      print(b_overall)
    }
  }  
  
  if (return_main) {
    return(my_ggplot_main)
  } else{
    return("All the code ran without error.")
  }
}

