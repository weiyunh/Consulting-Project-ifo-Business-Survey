# to add a value contribution, first understand:
# 1.what did first group do?
# 2.their major takeaways?
# 3.->implications for us:what can we now do with it? Where is our approach different?
# 4.identify: where can we add more value and most value? and what is not relevant for us?

#Bayesian principle: focus on what adds most value, discard rest.

#---------summarize_dirs function: looks for files in subdirectories----------
#show_files = TRUE to see file names inside folders
#pattern = relevant document extensions we want to count
summarize_dirs <- function(path = ".",
                          pattern = "\\.(r|rmd|md|txt|pdf|docx|doc|csv|xls|xlsx|xlsm)$",
                          show_files = TRUE) {
  dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  n_dirs <- length(dirs)
  cat(sprintf("\nAnalyzing: %s\nFolders in path: %d\n", basename(path), n_dirs))
  if (!n_dirs) { cat("  (no subfolders)\n"); return(invisible(NULL)) }
  for (d in dirs) {
    docs <- list.files(file.path(path, d), pattern = pattern, ignore.case = TRUE)
    cat(sprintf("  - %s: %d docs\n", d, length(docs)))
    if (isTRUE(show_files) && length(docs)) for (f in docs) cat(sprintf("    %s\n", f))
  }
  top_files <- list.files(path, pattern = pattern, ignore.case = TRUE)
  if (length(top_files)) {
    cat("Files in path:\n")
    for (f in top_files) cat(sprintf("  - %s\n", f))
  }
  cat("\n")
}
# use function with absolute path:check folder structure on first sight:
summarize_dirs("C:/R_projects/Consulting-Project-ifo-Business-Survey-main (5)/Consulting-Project-ifo-Business-Survey-main/")
summarize_dirs("C:/R_projects/Consulting-Project-ifo-Business-Survey-main (5)/Consulting-Project-ifo-Business-Survey-main/data")
summarize_dirs("C:/R_projects/Consulting-Project-ifo-Business-Survey-main (5)/Consulting-Project-ifo-Business-Survey-main/data", show_files = FALSE)
summarize_dirs("C:/R_projects/Consulting-Project-ifo-Business-Survey-main (5)/Consulting-Project-ifo-Business-Survey-main/ifoConsulting")
summarize_dirs("C:/R_projects/Consulting-Project-ifo-Business-Survey-main (5)/Consulting-Project-ifo-Business-Survey-main/ifoConsulting", show_files = FALSE )

# see 1st groups report:
summarize_dirs("C:/R_projects/1stteam/")
summarize_dirs("C:/R_projects/1stteam/ifo-lmu-consulting-main", show_files = FALSE)
summarize_dirs("C:/R_projects/1stteam/ifo-lmu-consulting-main")
#function non recursive, so can run on subdirs separately:
summarize_dirs("C:/R_projects/1stteam/ifo-lmu-consulting-main/Report", show_files = FALSE)

#---------1.what did first group do?----------------------------------------------------------------------------------------------
#summary of first groups important repositories:
summarize_dirs("C:/R_projects/1stteam/ifo-lmu-consulting-main", show_files = FALSE)
#LagAnalysis:
# Identify lead–lag structures in time series. 
# goal: look for sectoral dynamics, turning points, and stationarity properties across industries, 
# both in full-sample and rolling-window settings.
  # Goal: Find the delay.
  # Logic: If Industry A peaks in January and the Main Index peaks in June, Industry A is a 5-month leader. 
  # They used "Rolling Windows" to see if that 5-month gap stays the same or changes during a crisis.

#Forecasting:  
#perform univariate and multivariate Granger causality analysis 
#identify leading indicators for the aggregate main-index.
  #Goal: Prove the lead isn't a coincidence (Granger Causality).
  #Logic: Does knowing the past of a specific sector actually 
  #help you predict the future of the whole index? that is a "winner"

#CCF-based Clustering:
#cluster individual series, 
#according to their CCF with the main index.
  # Goal: Group by movement, not by name.
  # Logic: Instead of grouping "Car Parts" with "Cars," 
  # group "Car Parts" with "Steel" if they both move 3 months ahead of the economy. 
  # It’s about behavior, not the product they make.

#TurningPointIndications :  
# identify leading indicators for bus.cycle turning points. 
# main goal: find sub-series that can predict turning points 
# in the main index (manufacturing business climate) in real-time.
  # Goal: Find who hits the brakes first.
  # Logic: Identify the specific survey questions that change 
  # direction (up-to-down) just before the entire economy crashes.

#---------2.major takeaways of first group?:-----------------------------------------
#----------Quotes from Abstract:------------------------
#...full potential of time series of the underlying sub-aggregates 
# remains largely unexplored
#....
# By analyzing the manufacturing industrys sub-aggregates over time, 
# we examine how their interactions with the manufacturing bus.climate evolve and 
# whether certain sub-aggregates can serve as early indicators of turning point.
#...(<now what they did:>) 
#1.comprehensive correlation analysis 
# to map relationships between between individual industries
# and the main manufacturing index. Then examine these relationships
# with Granger causality.
# <where they find that:>
# formally tested, almost all individual subseries are Granger causal,
# but their information gain is negligible.
#2.different approaches to identify subsets which seem to be 
# especially indicative, such as an elastic net, a scoring model and 
# clustering <of industries or subaggregates?> based on their <whose?> cross-correlation function with the main idex.
#3. we use Markov Switching models applied to the sub-aggregates,
# to see: can we predict economic turning points of the main index.
# but neither individual, nor grouped series were able to meaningfully 
# indicate the main turning points.

#----------Quotes from Conclusion:------------------------
#...rather than building a full forecasting model,the focus was:
#to identify small predictice subsets of industries which stick out as esp.
#important for the overall index. We find strong correlations with clear 
#lead-lag structures within +-6months that compress during crises,
#pointing to their role as early stress signals.
#..most industries are Granger causal, but the info gain is negligible,
#which empahasizes the importance of subsets and clusters.
#..time series that showed up in their grouping and clustering analysis,
#often contained industries investment related or
#industries rather early in the supply chain.
#!

#Correlation analysis: reveals strong dependencies between industry indicators 
#and the main manufacturing index, concentrated within a +-6 month window.
#Despite sectoral imbalance and non stationarity, the correlation structure
#remains coherent and econometrically meaningful.
#Consensus results further show that correlations compress during crises,
#suggesting that shifts in industry alignment may provide 
#early signals of systemic stress and warrant deeper investigation.
#!!!

#Granger causality: almost all manufacturing industries are Granger causal
#(simple and instantaneous) to the aggregate main-index.
#the contribution of any single industry is minimal 
#in terms of information gain measured by adjusted R^2
#..an implicict requirement for Granger causality is stationarity
#which is violated in some survey questions and industries. (see 2.1.3)
#Thus, we find this method to be rather unsuitable for futh er analysis..
#!!

#Elastic Net vs. Scoring Model:
#both methods improve on the benchmark of lagged main-index values.
#Compared to 500 random five-industry subsets, they perform better in 
#early prediction set, likely bc overfitting is harder there,
#while the general prediction set leaves more scope for spurious fit.
#Although the exact industry codes differ, both approaches consistently 
#select sectors linked to inputs, packaging, and investment goods.
#From an economic perspective, these sectors are upstream in the value chain
#and therefore tend to adjust production earlier, 
#making them natural candidates for anticipating aggregate manufacturing dynamics.
#!!!

#CCF Clustering: 
#cluster individual survey questions (level 2) 
#by their cross correlation with main index.
#to compare clusters, they fit linear models with lagged values of 
#cluster average cluster and lagged values of the main index itself to the main index.
#<? dont understand last line, anyway..>
#They combine it wiht visually assessing the cross-corr fct to identify
#potentially leading and interesting clusters. 
#Esp. the early prediction setting brought out some leading clusters.
#While many clusters are hard to interpret,
#<?which ones?>
#clearer examples feature intermediate goods and investment related activities.
#!!

#Turning points:
# separate evaluation of twofold: 
#indicate turning points explicitly vs. indicate the main index itself 
#<?indicate the main index itself,do they mean forecast?>
#<?so they emphasise: are turning points sth different from the main index?> 
#They use Bry-Boschan algorithm to identify turning points of the main index and
#Markov Switching models for potentially indicating turning points of the subseries.
#Neither individual nor grouped series were able to reliably indicate the 
#Bry-Boschan turning points of the main index, when modeling levels with Markov switching.
#Further smoothing the series and applying Markov switching models 
#to model the first differenes or other more sophisticated approches could be
#examined further
#!!!

#Outlook:
#results show, the underlying time series of the ifo business climate 
#are able to provide meaningful information beyond the main index alone.
#!
#future work could explore data in greater depth, 
#many of their results suggested points to further elaborate on.
#!!!
#also analysis was restricted to monthly questions of the manufacturing industry.
#taking all of 40000 time series of the survey into account,
#could lead to many more insights.
#!!

#---------3.what can we now do with that? to build on their work-------------------

#Apriori again:
#the time series are able to provide meaningful information beyond the main index alone.
#taking all of 40tsd time series into account, 
#(so also data outside manufacturing,)
#could lead to many more insights.
#see!!!: pointers to future work

#Correlation:
#strong dependencies between industry indicators and main manufacturing index
#within +-6 month window.
#during crises, correlations compress:
#!!!1-> use 'tightening' correlations as a systemic stress indicator

#(Granger causality):
#!->almost all manufacturing industries Granger-cause the main-index.
# so the signal is 'real' but (the add info is) too weak to be useful.
# iow: signal exists everywhere, but too diluted to use without aggregation. 
# or: we need no hero prediction t.series but an army of them.
#..stationarity is an implicit requirement for Granger causality 
#which is violated in some survey questions and industries. (see 2.1.3)
#-> so this method rather unsuitable for futher analysis..
#!!!2-> for strong signal: focus on subsets and clusters, not individual series
#!-> stationarity matters, Granger failed in this respect. 
#!-> need robust methods, that aggregate signals.

#Elastic Net vs. Scoring Model:
#both methods improve on the benchmark of lagged main-index values.
#Although exact industry codes differ, both approaches 
#!!-> consistently select sectors linked to 
#   inputs, packaging, and investment goods.
#these sectors are 
#!!-> upstream in the value chain (so adjust production earlier), 
#!!-> natural candidates for anticipating aggregate manufacturing dynamics.

#(CCF Clustering:) 
#cluster individual survey questions (level 2) 
#by their cross correlation with main index. 
#identify potentially leading and interesting clusters. 
#Esp the early prediction setting brought leading clusters.
#-> While many clusters are hard to interpret,
#<?which ones?>
#clearer examples feature 
#!-> intermediate goods and investment related activities.

#!!!3-> use "upstream/investment" clusters
#as our primary DFM inputs?
#!!-> supply chain logic: leaders are not random,
#they are consistently 'upstream' (packaging, steel,..) 

#Turning points:
# separate evaluation of twofold: 
#indicate turning points explicitly vs. indicate the main index itself 
#They use 
#-Bry-Boschan algorithm to identify turning points of the main index and
#-Markov Switching models for potentially indicating turning points of the subseries.
#!-> Neither individual nor grouped series able to reliably indicate the 
# Bry-Boschan turning points of the main index, when modeling levels with Markov switching.
#!!! -> Further smoothing the series and applying Markov switching models 
#!!! -> to model first differences or more sophisticated approaches could be examined further

#!!!------3.again in short, what they suggest, we should look into------------

#in prioriry order:
# !!!1. Crisis dynamics: Use "tightening" correlations as systemic stress indicator (±6 month window)
# Kohäsion als Stressindikator nutzen.
#       -> don't model trend but the Cohesion, the correlation structure itself.
#       -> cohesion = intensity and uniformity of correlation structure, Kohäsion, Zusammenhalt
#       -> divide normal times vs crisis times using cohesion measure.
#       -> normal times: low cohesion, industries move more independently, the network of economy is more flexible
#       -> crisis times: high cohesion, like major shock (COVID-19, energy crisis,..) industries tend to crash at same time
#       -> crisis times: industries move more in sync, correlations compress/tighten, network of economy is rigid, prone to shocks.
#       -> ?how can we model cohesion and compressing correlations?
#           a) First Factor Dominance (PCA): Run rolling PCA. If 1st Component explains >60% variance = Crisis Mode.
#           b) Rolling Average Correlation: Calculate mean of all pairwise correlations in sliding window (e.g., 12 months).
#           c) Network Density: Treat industries as nodes. If # of high-correlation links spikes -> "Hairball" = Systemic Stress.
#
# !!!2. Granger: Signal too weak individually; need robust aggregation methods
# Aggregation statt Einzelindikatoren.
#       -> focus entirely on subsets and clusters.
#       -> use DFM to aggregate signals robustly.
#       -> bundle weak signals of upstream industries into Factors.
#       -> ?any other robust methods that aggregate weak signals?
#           a) Partial Least Squares (PLS): Finds latent factors that specifically maximize covariance with the Target (unlike PCA which just maximizes Variance).
#           b) Diffusion Indexes: Simple count of % of upstream sectors growing. (Robust because it ignores outlier magnitude).
#           c) Bayesian Model Averaging (BMA): Don't pick one model; average thousands of models weighted by their probability.
#
# !!!3. Elastic Net/Scoring: Focus on "upstream/investment" sectors 
# Fokus auf Vorleistungssektoren.
#       -> inputs, packaging, investment goods
#       -> Supply chain is reflective of a timeline:
#       natural leaders are not random, they are consistently 'upstream',
#       they are at the beginning of the value chain and adjust production earlier,
#       they are the 'canaries in the coal mine', early indicators of crises.
#       -> primary DFM inputs: use "upstream/investment" clusters.
#
# !!!4. Turning Points: Differences over levels
# Modelliere Differenzen statt Niveaus.
#       -> Markov Switching models failed looking at levels (raw values) of data.
#       -> apply models to first differences instead of levels.
#       -> smooth series before Markov Switching to remove noise.
#         -> HP filter, moving average, Baxter-King filter (bandpass)?

#!!!------4.where can we add more value and most value? and what is not relevant for us?-------------

#The first group's approaches to Markov Switching and Granger Causality is our roadmap. 
#1.We will use Cohesion (Systemic Stress) to identify the Red Zones.
#2.build a Dynamic Factor Model based on Upstream Clusters using First Differences. 
#to catch the U-turns of boom and bust cycles and economic crises.
#This should solve their 'Information Gain' problem.

#?!!Model Stability: Red Zone Handling
#-> 1st group's linear models "broke" during structural shifts (COVID/Energy crisis).
#-> Value Add: Use DFMs specifically to maintain signal stability during "Red Zones."
#   Use cohesion measure as a "Switch" to tell the model,
#   when stress is detected, to adjust weights or parameters.

#!-------- Not Relevant / Discard ----------

#Granger Causality on individual series:
#-> Proven waste of time, information gain too low (R^2 stagnation).

#Static Industry Codes (SIC):
#-> group by 'Lead-Time Behavior', not by SIC (..,"Cars", "Steel", "Chemicals") 

#Modeling Levels for Turning Points:
#-> 1st group already proved raw levels + Markov Switching = Fail. Do not repeat.

#Simple Linear/Full-Sample Models:
#-> Cannot handle the "Red Zone" shifts found in the rolling window analysis.

#(Data Outside Manufacturing)
#1st group only analyzed monthly manufacturing data.
#40,000+ time series available (Services, Trade, Construction).
#Upstream logic (intermediate goods) likely applies to B2B Services too.
#-> !!!Value Add: Test if Service sector leaders move even earlier than Manufacturing upstream.

#---------notes:--------------------------------------------------

#Granger causality?: a statistical test for 
#predictive utility, not cause-effect.
#Does knowing the past values of time series B
#help me predict the future values of time series A,
#better than just knowing past values of A alone?
#-> if yes, B Granger-causes A.
