
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

\- lyrics from Колокола ("The Bells") Op. 35 Movement I by S. Rachmaninoff (1913), translated to English by Ahmed E. Ismail ([source](https://www.lieder.net/lieder/assemble_translations.html?SongCycleId=1773&LanguageId=7&ContribId=))

## Installation

``` r
remotes::install_github("nathdep/harmonious")
```

## Example Usage

``` r
library(harmonious)

seed <- abs(.Random.seed[4])
set.seed(seed)
coef_hyper=5
sd_hyper=.1
rhat_threshold=1.06
maxCycles=3
init_modname="init_pi"
run_modname="run_pi"
saveDir <- getwd()

aux_envir <- genData(
  P=250,
  I=75,
  J=3,
  K=3,
  isCorrI=TRUE
)

model <- CreateMod(
  init_modname=init_modname,
  run_modname=run_modname,
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

# Example Reports

## Unstructured/Uncorrelated Item Residiuals

```


====================

SEED  1528295356

====================

P:  250 
I:  75 
J:  3 
K:  3 

PROPORTION Σ[λ_i < 0]/I = 7% 
MAX RHAT: 1.011 / bias_beta_j_theta_est[2] 

# BURN-IN DRAWS (INIT): 2000 
# BURN-IN DRAWS (FREE): 2000 
# POSTERIOR DRAWS POST BURN-IN (INIT): 2000 
# POSTERIOR DRAWS POST BURN-IN (FREE): 2000 
# RHAT CYCLES: 1 



TRUE Ω ITEM RESIDUALS: 

  λ τ
λ 1 0
τ 0 1


TRUE Σ ITEM REISUDALS: 

      λ    τ
λ 0.893 0.00
τ 0.000 2.63





EAP Ω ITEM RESIDUALS: 

  λ τ
λ 1 0
τ 0 1


EAP Σ ITEM REISUDALS: 

      λ     τ
λ 0.995 0.000
τ 0.000 3.269



POSTERIOR RMSD 


---------------------------------------------------------------------------------------------------
  variable      mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 rmsd_theta    0.2644   0.2572   0.02872   0.02212   0.2311   0.3194   1.001     1077       1431   

  rmsd_tau     0.3021   0.2909   0.05058   0.0386    0.242     0.4       1       1428       1604   

 rmsd_lambda   0.2806   0.2784   0.02678   0.02477    0.24    0.3268   1.001     4549       4832   
---------------------------------------------------------------------------------------------------




MEAN POSTERIOR BIAS (EST. - TRUE)


-------------------------------------------------------------------------------------------------------------
     variable        mean      median      sd        mad       q5        q95     rhat    ess_bulk   ess_tail 
------------------ --------- ---------- --------- --------- --------- --------- ------- ---------- ----------
 mean_bias_theta    -0.0929   -0.09438   0.09423   0.09378   -0.2442   0.06176   1.003    649.5       1277   

  mean_bias_tau     0.04098   0.04447    0.1351    0.1328    -0.1808   0.2606    1.003    669.9       1433   

 mean_bias_lambda   0.03778   0.03473    0.07424   0.07303   -0.0788   0.1638    1.002     1478       3021   
-------------------------------------------------------------------------------------------------------------




S.D. POSTERIOR BIAS (EST. - TRUE)


------------------------------------------------------------------------------------------------------
    variable       mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
---------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 sd_bias_theta    0.313    0.3126   0.01506   0.01479   0.2893   0.3386     1       4854       5255   

  sd_bias_tau     0.3718   0.3685   0.04134   0.03941   0.3103   0.4448   1.001     1856       2407   

 sd_bias_lambda   0.3526   0.3511   0.03132   0.03005   0.3042   0.4065     1       5710       5775   
------------------------------------------------------------------------------------------------------




COEFFICIENT POSTERIOR BIAS


----------------------------------------------------------------------------------------------------------------------------
         variable              mean       median       sd        mad        q5         q95      rhat    ess_bulk   ess_tail 
--------------------------- ----------- ----------- --------- --------- ---------- ----------- ------- ---------- ----------
 bias_beta_j_theta_est[1]     -0.2594     -0.2595    0.1554    0.1563    -0.5129    -0.006979   1.007    609.6       1159   

 bias_beta_j_theta_est[2]    -0.007439   -0.008171   0.1553    0.1575    -0.2616     0.2538     1.011    767.4       1588   

 bias_beta_k_lambda_est[1]   -0.005771   -0.008401   0.1896    0.1905    -0.3127     0.3084     1.003     1306       2364   

 bias_beta_k_lambda_est[2]    -0.1658     -0.1693    0.2163    0.2133    -0.5165      0.202     1.003     1385       2206   

  bias_beta_k_tau_est[1]      -0.1114     -0.1226    0.3457    0.3432    -0.6654     0.4847     1.002    520.4       1178   

  bias_beta_k_tau_est[2]      0.4937      0.4999     0.3802    0.3693    -0.1471      1.114     1.004    799.9       1571   

 bias_beta_jk_eta_est[1,1]    0.1708      0.1707     0.1007     0.101    0.007016    0.3387       1      11566       6633   

 bias_beta_jk_eta_est[2,1]    0.1831      0.1834     0.1071    0.1073    0.004467    0.3601       1      10759       6491   

 bias_beta_jk_eta_est[1,2]   -0.06548    -0.06684    0.09833   0.09885   -0.2286     0.09634    1.001    12420       6796   

 bias_beta_jk_eta_est[2,2]    0.08803     0.08866    0.1059    0.1059    -0.08972    0.2601     1.001    13356       6744   
----------------------------------------------------------------------------------------------------------------------------

```

## Structured/Correlated Item Residuals

```


====================

SEED  1003184675

====================

P:  250 
I:  75 
J:  3 
K:  3 

PROPORTION Σ[λ_i < 0]/I = 21% 
MAX RHAT: 1.015 / Omega_itemsL[2,1] 

# BURN-IN DRAWS (INIT): 2000 
# BURN-IN DRAWS (FREE): 2000 
# POSTERIOR DRAWS POST BURN-IN (INIT): 2000 
# POSTERIOR DRAWS POST BURN-IN (FREE): 2000 
# RHAT CYCLES: 1 



TRUE Ω ITEM RESIDUALS: 

      λ     τ
λ 1.000 0.655
τ 0.655 1.000


TRUE Σ ITEM REISUDALS: 

      λ     τ
λ 2.643 1.247
τ 1.247 1.371





EAP Ω ITEM RESIDUALS: 

      λ     τ
λ 1.000 0.499
τ 0.499 1.000


EAP Σ ITEM REISUDALS: 

      λ     τ
λ 2.144 0.789
τ 0.789 1.133



POSTERIOR RMSD 


---------------------------------------------------------------------------------------------------
  variable      mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 rmsd_theta    0.2927   0.2892   0.02327   0.01948   0.2619   0.3356   1.003     1515       2022   

  rmsd_tau     0.252    0.2386   0.05174   0.03786   0.1945   0.3557   1.003     1313       1766   

 rmsd_lambda   0.2938   0.2928   0.02765   0.02754   0.2495   0.3407   1.001     3941       4975   
---------------------------------------------------------------------------------------------------




MEAN POSTERIOR BIAS (EST. - TRUE)


----------------------------------------------------------------------------------------------------------------
     variable         mean      median      sd        mad       q5         q95      rhat    ess_bulk   ess_tail 
------------------ ---------- ---------- --------- --------- --------- ----------- ------- ---------- ----------
 mean_bias_theta    0.04525    0.04673    0.09232   0.09085   -0.1099    0.1944     1.011    678.4       1324   

  mean_bias_tau     -0.04277   -0.04329   0.09589   0.09451   -0.2012    0.1167     1.009     733        1581   

 mean_bias_lambda   -0.09481   -0.09717   0.05687   0.05723   -0.1825   8.427e-06   1.007     1546       2421   
----------------------------------------------------------------------------------------------------------------




S.D. POSTERIOR BIAS (EST. - TRUE)


------------------------------------------------------------------------------------------------------
    variable       mean    median     sd        mad       q5      q95     rhat    ess_bulk   ess_tail 
---------------- -------- -------- --------- --------- -------- -------- ------- ---------- ----------
 sd_bias_theta    0.3797   0.379    0.02021   0.01975   0.3477   0.4142   1.001     3785       4672   

  sd_bias_tau     0.315    0.3035   0.05441   0.04266   0.2497   0.4209   1.002     1522       1846   

 sd_bias_lambda   0.4015   0.3999   0.04259   0.04381   0.3353   0.4746   1.001     5223       6124   
------------------------------------------------------------------------------------------------------




COEFFICIENT POSTERIOR BIAS


--------------------------------------------------------------------------------------------------------------------------
         variable              mean       median       sd        mad       q5        q95      rhat    ess_bulk   ess_tail 
--------------------------- ----------- ----------- --------- --------- --------- ---------- ------- ---------- ----------
 bias_beta_j_theta_est[1]     0.1107      0.1077     0.1625    0.1642    -0.1534    0.3802    1.005    876.8       1892   

 bias_beta_j_theta_est[2]     0.0461      0.04479    0.1437    0.1422    -0.1892    0.283     1.012    762.8       1242   

 bias_beta_k_lambda_est[1]    -0.5808     -0.584     0.2945    0.2857    -1.059    -0.08826   1.005    830.2       1595   

 bias_beta_k_lambda_est[2]    0.1332      0.1308     0.3098    0.3064    -0.3737    0.6475    1.003     1016       1835   

  bias_beta_k_tau_est[1]      -0.2231     -0.2244    0.2155    0.2137    -0.5735    0.1308    1.002     1090       1976   

  bias_beta_k_tau_est[2]      0.1193      0.1128     0.2863    0.2746    -0.347     0.5975    1.005    939.9       1260   

 bias_beta_jk_eta_est[1,1]    -0.1146     -0.114     0.08569   0.08648   -0.2561   0.02314      1      10460       6505   

 bias_beta_jk_eta_est[2,1]   -0.006861   -0.005843   0.08027   0.07922   -0.1402    0.1261    1.001    10673       5993   

 bias_beta_jk_eta_est[1,2]    -0.1504     -0.1497    0.1159    0.1138    -0.3407   0.03986      1      10289       6395   

 bias_beta_jk_eta_est[2,2]    -0.1307     -0.1323    0.1026    0.1032    -0.2961   0.03709      1       9941       5907   
--------------------------------------------------------------------------------------------------------------------------

```
