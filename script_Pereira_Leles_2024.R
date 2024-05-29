rm(list = ls()) # apaga todas as variáveis 
cat("\014") # limpa a tela (CTRL+L)     
setwd(dirname(sys.frame(1)$ofile)) # diretório corrente é o do script

# ------------------------------------------------------------------------------
# Data Loading and Initial Processing (Commented Out)
# 
# Description:
#   The original dataset is loaded into a data.table for ease of manipulation. 
#   Several profitability measures are calculated during this phase.
#
# Note:
#   This code block is commented out due to copyright issues of the original 
#   data. It serves only as an example of the pre-processing steps performed.
#
# Profitability Measures:
#   Gross Profitability (OP)   - Calculated as (REVT - COGS) / AT
#   Earnings (OP1)             - Calculated as EBIT / AT
#   Free Cash Flow (OP2)       - Calculated as FCF / AT
#   Fama-French Profitability (OP3) - Calculated as EBIT / BE.lag4
# 
# Functions:
#   psych::winsor() - Winsorizes the data to trim outliers based on the specified 
#                     level_winsor (1% in this case).
#   by              - Applies the function by groups (in this case, by firm).
# ------------------------------------------------------------------------------
# dataset = dataset[, OP  := psych::winsor((REVT - COGS) / AT, trim = level_winsor), by = firm]
# dataset = dataset[, OP1 := psych::winsor(EBIT / AT, trim = level_winsor), by = firm]
# dataset = dataset[, OP2 := psych::winsor(FCF / AT, trim = level_winsor), by = firm]
# dataset = dataset[, OP3 := psych::winsor(EBIT / BE.lag4, trim = level_winsor), by = firm]

# ------------------------------------------------------------------------------
# Load Required Libraries
# 
# Description:
#   This section loads the necessary libraries for data manipulation, 
#   time series analysis, and panel data modeling.
# ------------------------------------------------------------------------------
library(zoo)
library(plm)

# ------------------------------------------------------------------------------
# Source Additional Functions
# 
# Description:
#   This section sources additional custom functions from an external R script.
#
#  -> informar as funções usadas (PENDENTE)
# Functions:
#   save.charts()    - Saves the charts for the specified data range.
#   calc_models()    - 
# ------------------------------------------------------------------------------
source('./lib/extra_functions.R')

# ------------------------------------------------------------------------------
# Load Dataset
# 
# Description:
#   This section loads the pre-processed dataset required for the analysis.
# 
# Note:
#   The dataset is loaded from an RData file located in the results directory.
# ------------------------------------------------------------------------------
load('./data/dataset_github.RData')

# ------------------------------------------------------------------------------
# Step 1 
# Save charts showing the number of companies that report data annually  and 
# those that report quarterly
# 
# Description:
#   This step involves saving visual representations of the data to highlight 
#   the frequency of company reports. Charts are saved for both annual and 
#   quarterly reporting companies.
#
# Variables:
#   last_date_NEFIN  - The most recent date in the NEFIN factors dataset.
#   idx_last         - Index of the last date in the chart_data corresponding 
#                      to last_date_NEFIN.
#
# Functions:
#   as.yearqtr()     - Converts a date to a year-quarter format.
#   max()            - Returns the maximum value.
#   index()          - Retrieves the index (dates) of a data object.
#   which()          - Identifies the indices of TRUE values.
#   save.charts()    - Saves the charts for the specified data range.
# ------------------------------------------------------------------------------
last_date_NEFIN = as.yearqtr(max(index(nefin_factors)))
idx_last = which(index(chart_data) == last_date_NEFIN)
save.charts(chart_data[1:idx_last,], flag_font_latex = FALSE)

# ------------------------------------------------------------------------------
# Step 2 - Filter Data and Handle Negative Book Equity (BE)
#
# Description:
#   This section checks for observations with negative Book Equity (BE) and 
#   prints a warning message. It then filters out these observations from the 
#   dataset.
#
# Justification:
#   The removal of companies with negative book equity is common in finance 
#   studies, following a practice established in the literature, as demonstrated 
#   in prior research, such as Fama and French (1992). Hence, to maintain the 
#   integrity of our analysis, these observations are filtered out.
# ------------------------------------------------------------------------------

# Print a warning message about the number of observations with BE < 0
cat(sprintf('WARNING :: There are %d observations, out of a total of %d (~ %2.1f%%), containing BE < 0\n', 
            nrow(panel[panel$BE <= 0, ]), nrow(panel), 100*nrow(panel[panel$BE <= 0, ])/nrow(panel)))

# Filter out observations with BE <= 0
panel_regression = panel[panel$BE > 0, ]

###########################################################
# Step 3 - Demean variables by industry and time period
#
# Description:
# This step adjusts (demeans) the variables in the dataset by removing industry 
# and time-specific effects. By subtracting the industry average for each time 
# period, we normalize the data to focus on deviations from the industry mean.
# This process helps in isolating the unique performance attributes of each firm 
# relative to its industry and time period.
#
# Note:
# In this context, "industry" refers to the economic sector classification as 
# provided by the Economática database.
#
# Functions:
# - mean(): Calculate the mean of a given vector.
# - unique(): Extract unique elements from a vector.
# - which(): Identify indices of elements satisfying a condition.
# - View(): Display a data frame (optional, commented out).
###########################################################

# Create a copy of the dataset to hold the demeaned data
panel_regression_demeaned = panel_regression

# Get a list of unique industry identifiers
grp_industry = unique(panel_regression$industry)

# Get the unique time indices
dt_index = unique(panel_regression$Index)

# Loop through each unique industry
for (i in 1:length(grp_industry)) 
{
    # Loop through each unique time index
    for (j in 1:length(dt_index)) 
    {
        # Identify the indices for the current sub-panel based on the current industry and time index
        index_sub_panel = which((panel_regression_demeaned$Index == dt_index[j]) & (panel_regression_demeaned$industry == grp_industry[i]))
        
        # Extract the sub-panel data for the current industry and time index
        sub_panel = panel_regression_demeaned[index_sub_panel,]
        
        # Loop through each variable to be demeaned
        for (k in 1:length(variable_info$short))
        {
            # Calculate the industry average for the current variable within the sub-panel
            industry_avg = mean(panel_regression_demeaned[index_sub_panel, variable_info$short[k]])
            
            # Subtract the industry average from each observation in the sub-panel for the current variable
            panel_regression_demeaned[index_sub_panel, variable_info$short[k]] = panel_regression_demeaned[index_sub_panel, variable_info$short[k]] - industry_avg
        }
        # Optionally view the sub-panel before and after demeaning
        # View(sub_panel)
        # View(panel_regression_demeaned[index_sub_panel,])
    }
}

###########################################################
# Step 4 - Summary of regression dataset and information collection
#
# Description:
# This step provides a summary of the regression dataset and collects key 
# information about the data. It prints out the number of observations, firms, 
# and time periods, as well as the range of the data. Additionally, it creates a 
# list object to store important metadata about the dataset.
#
# Functions:
# - cat(): Concatenate and print objects.
# - sprintf(): Format and return a string.
# - nrow(): Get the number of rows in a data frame.
# - length(): Get the length of a vector.
# - unique(): Extract unique elements from a vector.
# - min(): Find the minimum value in a vector.
# - max(): Find the maximum value in a vector.
#
# Variables:
# - panel_regression: Data frame containing the regression data.
# - variable_info: Data frame with variable names and metadata.
# - winsorize_data: Logical indicating whether winsorization was applied.
# - level_winsor: Numeric value specifying the level of winsorization.
# - type_excess_ret: Character indicating the type of excess return used 
#   (linear: r-rf or discounted: (1+r)/(1+rf) - 1).
# - include_financials: Logical indicating whether financial firms are included.
###########################################################

# Print summary of the regression dataset
cat(sprintf('DATA (regression):: %d observations, %d firms, %d quarters (%s to %s)\n', 
            nrow(panel_regression), 
            length(unique(panel_regression$firm)), 
            length(unique(panel_regression$Index)), 
            min(panel_regression$Index), 
            max(panel_regression$Index)))

# Collect key information about the dataset into a list
info = list(
    observations = nrow(panel_regression), 
    num.firms = length(unique(panel_regression$firm)), 
    num.periods = length(unique(panel_regression$Index)), 
    first.date = min(panel_regression$Index), 
    last.date = max(panel_regression$Index),
    names = variable_info,
    winsor = list(
        is.winsorized = winsorize_data, 
        level = level_winsor
        ),
    type.excess.ret = type_excess_ret,
    include.financials = include_financials
)

###########################################################
# Step 4: Estimate and Summarize PMG Models
#
# Description:
# This step involves estimating multiple Pooled Mean Group (PMG) regression models 
# using the previously defined `calc_models` function. The results of these models 
# are then summarized using the `stargazer` package to generate a text-based summary 
# of the regression outputs.
#
# Functions:
# - cat(): Outputs text to the console.
# - calc_models(): Estimates multiple PMG models based on the input data.
# - stargazer::stargazer(): Generates summary tables for regression models.
#
# Notes:
# - The 'stargazer' package is used for creating well-formatted summaries of 
#   regression results. Ensure the package is installed and loaded before running this step.
# - The summary output is printed in the console as plain text.
###########################################################

# Print a header to indicate the start of the Fama-MacBeth (PMG) results section
cat('\n=====Fama-MacBeth (PMG)=====\n')

# Estimate the PMG models using the previously defined function (extra_function.R)
vector_models = calc_models(panel_regression)

# Summarize the estimated PMG models using stargazer and print the results to 
# the console
stargazer::stargazer(vector_models,
                     type = 'text')

###########################################################
# Step 5: Estimate and Summarize PMG Models (Demeaned by Industry)
#
# Description:
# This step involves estimating multiple Pooled Mean Group (PMG) regression models 
# using industry-demeaned data. The results of these models are then summarized 
# using the `stargazer` package to generate a text-based summary of the regression outputs.
#
# Functions:
# - cat(): Outputs text to the console.
# - calc_models(): Estimates multiple PMG models based on the input data.
# - stargazer::stargazer(): Generates summary tables for regression models.
#
# Notes:
# - This step specifically uses data that has been demeaned by industry, as defined in previous steps.
# - The 'stargazer' package is used for creating well-formatted summaries of regression results.
# - Ensure the 'stargazer' package is installed and loaded before running this step.
###########################################################

# Print a header to indicate the start of the Fama-MacBeth (PMG) results section for industry-demeaned data
cat('\n=====Fama-MacBeth (PMG) *demeaned by industry*=====\n')

# Estimate the PMG models using the industry-demeaned data
vector_models_demeaned = calc_models(panel_regression_demeaned)

# Summarize the estimated PMG models using stargazer and print the results to 
# the console
stargazer::stargazer(vector_models_demeaned,
                     type = 'text')

###########################################################
# Step 7: Calculate Variance Inflation Factors (VIF)
#
# Description:
# This step calculates the Variance Inflation Factors (VIF) for each of the PMG models 
# by fitting a simple linear model (lm) with the same predictor variables used in the PMG models.
# VIF is a measure of multicollinearity in the regression models, which helps diagnose 
# the presence of multicollinearity among the predictors.
#
# Functions:
# - vector(): Creates a list to store the VIF values for each model.
# - for loop: Iterates over each PMG model to calculate VIF values using lm.
# - car::vif(): Computes the Variance Inflation Factors for a given linear model.
# - lm(): Fits a simple linear model with the same predictor variables used in the PMG models.
# - update(): Updates the formula of the PMG models to include an intercept.
# - do.call(): Combines a list of VIF values into a data frame.
# - lapply(): Applies a function over a list or vector.
# - as.data.frame(): Converts a list to a data frame.
# - colnames(): Sets the column names of a data frame.
# - cat(): Outputs text to the console.
# - print(): Prints the VIF results to the console.
#
# Notes:
# - Multicollinearity can inflate the variance of regression coefficients, making them unstable.
# - High VIF values (typically above 10) indicate significant multicollinearity issues.
# - VIF is calculated using simple linear models (lm) instead of PMG models for ease of computation.
###########################################################

# Initialize a list to store VIF values for each PMG model
vif_models = vector('list', length = length(vector_models))

# Loop over each PMG model to calculate VIF values using simple linear models
for (i in 1:length(vif_models))
{
    # Calculate VIF for the i-th PMG model using a simple linear model (lm)
    vif_models[[i]] = car::vif(lm(update(vector_models[[i]]$formula, . ~ . + 1), 
                                  data = panel_regression))
}

# Get the names of the predictor variables (excluding the intercept)
colunas = names(vector_models[[i]]$model)[-1]

# Combine the VIF values into a data frame for easier interpretation
result_VIF = as.data.frame(do.call(rbind, lapply(vif_models, function(x) { x[colunas] })))

# Set the column names of the resulting data frame
colnames(result_VIF) = colunas

# Print a header to indicate the start of the VIF results section
cat('\n=====VIF(Variance Inflation Factors)=====\n\n')

# Print the VIF results to the console
print(result_VIF)


panel$log_BE_ME = log(panel$BE/panel$ME)
panel$log_ME_lag1 = log(panel$ME.lag1)
table_selected = panel[panel$BE > 0,]

short.alias = c('\\mathit{GP}/\\mathit{AT}',
                '\\mathit{EBIT}/\\mathit{AT}',
                '\\mathit{FCF}/\\mathit{AT}',
                '\\mathit{EBIT}/\\mathit{BE}_{t-4}', 
                '\\log(\\mathit{BE}/\\mathit{ME})', 
                '\\log(\\mathit{ME})',
                'r_{3,0}',
                'r_{12,4}')

descriptive_stats = gen_stats_table(table_selected[,c('OP','OP1','OP2','OP3','log_BE_ME','log_ME_lag1','r.3.0','r.12.4')],
                                    short.alias,
                                    cor.method = 'spearman')
descriptive_stats$period = c(table_selected$Index[1],tail(table_selected$Index,1))
descriptive_stats$nobs = nrow(table_selected)

# lista contendo os resultados referentes aos portfólios agrupados por 
# indices de lucratividade, bem como suas regressoes diante do modelo de 
# 3 fatores (FF3) para o mercado brasileiro
vector_tables = vector('list', length = length(info$names$short))

number_groups_univariate = 4
number_groups_bivariate_ME = 3
number_groups_bivariate_OP = 4

formula_j = r_j ~ Mkt.RF + SMB + HML # FF3
formula_jk = r_jk ~ Mkt.RF + SMB + HML # FF3  

cat('\n\nCalculating portfolios...\n')
for (i in 1:length(vector_tables)) 
{
    cat(sprintf('%s) Portfolios created with profitability index: %s\n',
                letters[i],
                info$names$short[i]))
    cat('gen_portfolio_table:: in\n')
    portfolio_data = gen_portfolio_table(excess_ret,
                                         panel[lubridate::quarter(panel$Index) == 2,
                                               c('Index','firm', info$names$short[i], 'log_BE_ME', 'ME', 'BE')],
                                         panel[lubridate::quarter(panel$Index) == 2,
                                               c('Index','firm','ME')],
                                         num_groups = number_groups_univariate,
                                         sort_type = 'profitability',
                                         avg_method = avg_method_portfolios)
    cat('gen_portfolio_table:: out\n')
    
    vector_tables[[i]]$data = portfolio_data$portfolios
    
    if (i == 1)
    {
        info$rebalancing$single = portfolio_data$info$rebalancing
    }    
    
    data_regress = merge(vector_tables[[i]]$data, nefin_factors, all = FALSE)

    if (i == 1)
    {
        short.alias.factors = c('(R_M-r_f)','\\mathit{SMB}','\\mathit{HML}','r_f')
        idx = which(colnames(data_regress) == 'Mkt.RF')
        
        nefin_factors_descriptive_stats = gen_stats_table(100*as.data.frame(data_regress[,-c(1:(idx-1), (ncol(data_regress)-1) )]),
                                                          short.alias.factors,
                                                          cor.method = 'pearson')

        nefin_factors_descriptive_stats$period = c(index(data_regress)[1],tail(index(data_regress),1))
        nefin_factors_descriptive_stats$nobs = nrow(data_regress)
    }
    
    
    coefs.value = NULL
    coefs.stats = NULL
    coefs.pvalue = NULL
    r.squared = NULL
    
    for (j in 1:(ncol(vector_tables[[i]]$data))) 
    {
        r_j = data_regress[,j]
        idx_valid = !is.na(r_j)
        r_j = r_j[idx_valid]
        
        model = lm(formula_j, 
                   data = data_regress[idx_valid,])
        model_summary = summary(model)
        
        coefs.value = rbind(coefs.value, coef(model_summary)[,1])
        coefs.stats = rbind(coefs.stats, coef(model_summary)[,3])
        coefs.pvalue = rbind(coefs.pvalue, coef(model_summary)[,4])
        r.squared = c(r.squared, model_summary$adj.r.squared)
    }
    rownames(coefs.value) = colnames(vector_tables[[i]]$data)
    rownames(coefs.stats) = colnames(vector_tables[[i]]$data)
    rownames(coefs.pvalue) = colnames(vector_tables[[i]]$data)
    coefs.value = cbind(coefs.value, r.squared)
    
    vector_tables[[i]]$panelA.1.value = cbind(colMeans(vector_tables[[i]]$data, na.rm = TRUE), 
                                              coefs.value)
    
    vector_tables[[i]]$panelA.1.stats = cbind(t(as.data.frame(sapply(vector_tables[[i]]$data, t.test)[1,])),
                                              coefs.stats)
    
    vector_tables[[i]]$panelA.1.pvalue = cbind(t(as.data.frame(sapply(vector_tables[[i]]$data, t.test)[3,])),
                                               coefs.pvalue)
    
    vector_tables[[i]]$panelA.2.value = portfolio_data$characteristics
    
    vector_tables[[i]]$panelA.2.DR.pvalue = portfolio_data$diversification.ratio.test$p.value
    
    vector_tables[[i]]$panelA.2.DR.market = portfolio_data$diversification.ratio.test$market
    
    cat('gen_portfolio_table_double_sort:: in\n')
    portfolio_data_double = gen_portfolio_table_double_sort(excess_ret,
                                                            panel[lubridate::quarter(panel$Index) == 2,
                                                                  c('Index','firm', info$names$short[i], 'log_BE_ME', 'ME', 'BE')],
                                                            panel[lubridate::quarter(panel$Index) == 2,
                                                                  c('Index','firm','ME')],
                                                            num_groups_ME = number_groups_bivariate_ME,
                                                            num_groups_OP = number_groups_bivariate_OP,
                                                            avg_method = avg_method_portfolios)
    cat('gen_portfolio_table_double_sort:: out\n')
    
    vector_tables[[i]]$data.bivariate = portfolio_data_double$portfolios
    
    if (i == 1)
    {
        info$rebalancing$double = portfolio_data_double$info$rebalancing
    }    
    
    vector_tables[[i]]$table4.panelA.1.value = colMeans(vector_tables[[i]]$data.bivariate, na.rm = TRUE)
    
    vector_tables[[i]]$table4.panelA.1.stats = as.data.frame(sapply(vector_tables[[i]]$data.bivariate, t.test)[1,])
    
    data_regress = merge(vector_tables[[i]]$data.bivariate, nefin_factors, all = FALSE)

    re.value = NULL
    re.stats = NULL
    re.pvalue = NULL
    coefs.value = NULL
    coefs.stats = NULL
    coefs.pvalue = NULL
    r.squared = NULL
    reg_names = NULL    
    for (k in 1:number_groups_bivariate_OP) 
    {
        r_jk = data_regress[,k+1] - data_regress[,(number_groups_bivariate_ME-1)*number_groups_bivariate_OP+k+1] 
        idx_valid = !is.na(r_jk)
        r_jk = r_jk[idx_valid]
        
        reg_names = c(reg_names, 
                      sprintf('%s-%s',
                              colnames(data_regress)[k+1], 
                              colnames(data_regress)[(number_groups_bivariate_ME-1)*number_groups_bivariate_OP+k+1]))
        model = lm(formula_jk, 
                   data = data_regress[idx_valid,])
        model_summary = summary(model)
        
        test.re = t.test(r_jk)
        re.value  = rbind(re.value , test.re$estimate) 
        re.stats  = rbind(re.stats , test.re$statistic)
        re.pvalue = rbind(re.pvalue, test.re$p.value)
        
        coefs.value = rbind(coefs.value, coef(model_summary)[,1])
        coefs.stats = rbind(coefs.stats, coef(model_summary)[,3])
        coefs.pvalue = rbind(coefs.pvalue, coef(model_summary)[,4])
        r.squared = c(r.squared, model_summary$adj.r.squared)
    }
    rownames(coefs.value) = reg_names
    rownames(coefs.stats) = reg_names
    rownames(coefs.pvalue) = reg_names
    coefs.value = cbind(coefs.value, r.squared)
    
    coefs.value = cbind(re.value, coefs.value)
    colnames(coefs.value)[1] = 'r.e'
    coefs.stats = cbind(re.stats, coefs.stats)
    colnames(coefs.stats)[1] = 'r.e'
    coefs.pvalue = cbind(re.pvalue, coefs.pvalue)
    colnames(coefs.pvalue)[1] = 'r.e'
    
    vector_tables[[i]]$table4.panelB.SMB.value = coefs.value
    vector_tables[[i]]$table4.panelB.SMB.stats = coefs.stats
    vector_tables[[i]]$table4.panelB.SMB.pvalue = coefs.pvalue
    
    re.value = NULL
    re.stats = NULL
    re.pvalue = NULL
    coefs.value = NULL
    coefs.stats = NULL
    coefs.pvalue = NULL
    r.squared = NULL
    reg_names = NULL    
    for (j in 1:number_groups_bivariate_ME) 
    {
        r_jk = data_regress[,(j-1)*number_groups_bivariate_OP+number_groups_bivariate_OP+1] - data_regress[,(j-1)*number_groups_bivariate_OP+2] 
        idx_valid = !is.na(r_jk)
        r_jk = r_jk[idx_valid]
        
        reg_names = c(reg_names, 
                      sprintf('%s-%s',
                              colnames(data_regress)[(j-1)*number_groups_bivariate_OP+number_groups_bivariate_OP+1], 
                              colnames(data_regress)[(j-1)*number_groups_bivariate_OP+2]))
        model = lm(formula_jk, 
                   data = data_regress[idx_valid,])
        model_summary = summary(model)
        
        test.re = t.test(r_jk)
        re.value  = rbind(re.value , test.re$estimate) 
        re.stats  = rbind(re.stats , test.re$statistic)
        re.pvalue = rbind(re.pvalue, test.re$p.value)
        
        coefs.value = rbind(coefs.value, coef(model_summary)[,1])
        coefs.stats = rbind(coefs.stats, coef(model_summary)[,3])
        coefs.pvalue = rbind(coefs.pvalue, coef(model_summary)[,4])
        r.squared = c(r.squared, model_summary$adj.r.squared)
    }
    rownames(coefs.value) = reg_names
    rownames(coefs.stats) = reg_names
    rownames(coefs.pvalue) = reg_names
    coefs.value = cbind(coefs.value, r.squared)
    
    coefs.value = cbind(re.value, coefs.value)
    colnames(coefs.value)[1] = 'r.e'
    coefs.stats = cbind(re.stats, coefs.stats)
    colnames(coefs.stats)[1] = 'r.e'
    coefs.pvalue = cbind(re.pvalue, coefs.pvalue)
    colnames(coefs.pvalue)[1] = 'r.e'
    
    vector_tables[[i]]$table4.panelB.HML.value = coefs.value
    vector_tables[[i]]$table4.panelB.HML.stats = coefs.stats
    vector_tables[[i]]$table4.panelB.HML.pvalue = coefs.pvalue
    
    vector_tables[[i]]$table4.panelC = portfolio_data_double$characteristics
    
    vector_tables[[i]]$table4.panelC.DR.pvalue = portfolio_data_double$diversification.ratio.test$p.value
    
    vector_tables[[i]]$table4.panelC.DR.market = portfolio_data_double$diversification.ratio.test$market
}

cat('gen_ME_characteristics:: in\n')

table_3 = gen_ME_characteristics(panel[,c('Index','firm',info$names$short,'log_BE_ME','ME','BE')],
                                 num_groups = number_groups_univariate,
                                 international_breakpoints = FALSE)
cat('gen_ME_characteristics:: out\n')

