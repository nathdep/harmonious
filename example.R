library(harmonious)

seed <- abs(.Random.seed[4])
set.seed(seed)
coef_hyper=5
sd_hyper=.1
rhat_threshold=1.06
maxCycles=3
initFile="init_pi_corr.stan"
runFile="run_pi_corr.stan"
saveDir <- getwd()

aux_envir <- genData(
  P=250,
  I=75,
  J=3,
  K=3,
  isCorrI=TRUE
)

model <- CreateMod(
  initFile=initFile,
  runFile=runFile,
  aux_envir=aux_envir,
  coef_hyper=coef_hyper,
  sd_hyper=sd_hyper,
  nWarmup_init=1000,
  nSamples_init=1000,
  nWarmup_run=1000,
  nSamples_run=1000
)

fileDetails <- paste0(seed, "_P", P, "_I", I, "_J", J, "_K", K)

model$initialize()
model$sample()
rhatCheck()

genReport(saveDir=saveDir, fileDetails=fileDetails)
