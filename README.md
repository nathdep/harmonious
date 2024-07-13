
Listen! Sleds rushing past in a row,\
Rushing past in a row! \
Their tiny bells ringing, \
Their easy, silvery peal falls on our ears like ambrosia,\
Their humming and pinging whisper of oblivion.\
O how clearly, clearly, clearly --\
Indeed, with sonorous childlike laughter --\
In the clear night air\
_**They tell the tale,\
Of how deception and delusion\
Will be followed by renewal**_... 

\- lyrics from Колокола ("The Bells") Movement I by S. Rachmaninoff, translated to English by Ahmed E. Ismail ([source](https://www.lieder.net/lieder/assemble_translations.html?SongCycleId=1773&LanguageId=7&ContribId=))

## Installation

``` r
remotes::install_github("nathdep/NathansComps")
```

## Example Usage

``` r
library(NathansComps)

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
```

# Example Report

```
====================

SEED  324845772

====================

P:  250 
I:  75 
J:  3 
K:  3 
PROPORTION Σ[λ_i < 0]/I = 25% 
MAX RHAT: 1.04/u_items[2,9]

TRUE Ω ITEM RESIDUALS: 

       λ      τ
λ  1.000 -0.809
τ -0.809  1.000


TRUE Σ ITEM REISUDALS: 

       λ      τ
λ  3.338 -1.754
τ -1.754  1.407


EAP Ω ITEM RESIDUALS: 

       λ      τ
λ  1.000 -0.754
τ -0.754  1.000


TRUE Σ ITEM REISUDALS: 

       λ      τ
λ  3.074 -1.447
τ -1.447  1.194


MEAN POSTERIOR BIAS (EST - TRUE)


---------------------------------------------------------------------------------------------------------------
     variable         mean      median      sd        mad        q5        q95     rhat    ess_bulk   ess_tail 
------------------ ---------- ---------- --------- --------- ---------- --------- ------- ---------- ----------
 mean_bias_theta    -0.06393   -0.06384   0.09465   0.09231   -0.2211    0.09512   1.032    186.2       440    

  mean_bias_tau      0.1057     0.1066     0.107    0.1054    -0.06955   0.2786    1.031    189.9      478.5   

 mean_bias_lambda   0.006611   0.005976   0.06554   0.06628   -0.09831   0.1176    1.01      525        1436   
---------------------------------------------------------------------------------------------------------------


S.D. POSTERIOR BIAS (EST-TRUE)


------------------------------------------------------------------------------------------------------
    variable       mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
---------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 sd_bias_theta    0.3621   0.3617   0.01965   0.01901   0.3318   0.3962   1.002     1390       1687   

  sd_bias_tau     0.3445   0.3269   0.06749   0.04985   0.2681   0.4803   1.013    466.2      683.9   

 sd_bias_lambda   0.4561   0.4513   0.05238   0.04962   0.3805   0.5496   1.001     2071       2128   
------------------------------------------------------------------------------------------------------


POSTERIOR RMSD 


---------------------------------------------------------------------------------------------------
  variable      mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 rmsd_theta    0.2816   0.2764   0.0259    0.01986   0.2496   0.3299   1.014    555.8      695.5   

  rmsd_tau     0.2817   0.2625   0.06911   0.0489    0.2093   0.4233   1.017    392.3      570.5   

 rmsd_lambda   0.3313   0.3297   0.03039   0.02876   0.2856   0.3827     1       2363       2322   
---------------------------------------------------------------------------------------------------


MEAN POSTERIOR BIAS (EST. - TRUE)


---------------------------------------------------------------------------------------------------------------
     variable         mean      median      sd        mad        q5        q95     rhat    ess_bulk   ess_tail 
------------------ ---------- ---------- --------- --------- ---------- --------- ------- ---------- ----------
 mean_bias_theta    -0.06393   -0.06384   0.09465   0.09231   -0.2211    0.09512   1.032    186.2       440    

  mean_bias_tau      0.1057     0.1066     0.107    0.1054    -0.06955   0.2786    1.031    189.9      478.5   

 mean_bias_lambda   0.006611   0.005976   0.06554   0.06628   -0.09831   0.1176    1.01      525        1436   
---------------------------------------------------------------------------------------------------------------


S.D. POSTERIOR BIAS (EST. - TRUE)


------------------------------------------------------------------------------------------------------
    variable       mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
---------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 sd_bias_theta    0.3621   0.3617   0.01965   0.01901   0.3318   0.3962   1.002     1390       1687   

  sd_bias_tau     0.3445   0.3269   0.06749   0.04985   0.2681   0.4803   1.013    466.2      683.9   

 sd_bias_lambda   0.4561   0.4513   0.05238   0.04962   0.3805   0.5496   1.001     2071       2128   
------------------------------------------------------------------------------------------------------

```
