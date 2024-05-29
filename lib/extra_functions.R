diversification_ratio = function(x,V)
{
    V[is.na(V)] = 0
    s = sqrt(diag(V))
    risk = sqrt(t(x)%*%V%*%x)
    return((t(x)%*%s)/risk)
}

save.charts = function(data.dfp, flag_font_latex = FALSE)
{
    output_dir = './figs/'
    if (!dir.exists(output_dir))
    {
        print(sprintf("save.charts:: The directory %s does not exist. Creating the directory to save generated graphs.", output_dir))
        dir.create(output_dir)
    } 
    
    library(ggplot2)
    library(zoo)
    
    library(extrafont)
    #font_install('fontcm') # deve ser instalada com sudo no prompt
    loadfonts(device = "pdf", quiet = TRUE)
    
    valid_lines = rowSums(is.na(data.dfp)) < 2
    
    dfp_period = c('Trimestral','Anual')
    df_plot = fortify.zoo(data.dfp[valid_lines,], melt = TRUE)
    df_plot$type = dfp_period[(lubridate::quarter(df_plot$Index) == 4)+1]
    df_plot$Index = as.Date(as.yearmon(df_plot$Index)+3/12)-1 # converte para o final do trimestre
    df_plot$type[df_plot$Series == 'ME'] = 'Negociadas'
    df_plot$type = factor(df_plot$type, levels = c('Trimestral','Anual','Negociadas'))
    
    for (var.name in unique(df_plot$Series)) 
    {
        p_i = ggplot(df_plot[(df_plot$Series == var.name | df_plot$Series == 'ME'),],
                     aes(x=Index, y=Value, color=type))+
            geom_line() +
            geom_point(aes(color=type,shape=type))+
            theme_bw()+
            ylim(0,ceiling(max(df_plot$Value/100,na.rm = TRUE))*100)+
            ylab("Número de empresas")+
            xlab(paste0("Período: ",format(min(index(data.dfp[valid_lines,])),"%qT-%Y")," a ",format(max(index(data.dfp[valid_lines,])),"%qT-%Y")))+
            ggtitle(sprintf('Comparativo da frequência de divulgação dos dados (%s)', var.name))
        
        
        if(flag_font_latex)
        {
            base_size = 16
            p_i = p_i+
                theme(#text = element_text(size = base_size, family = "CM Roman"),
                    strip.text.y = element_text(size = base_size, family = "CM Roman"),
                    axis.text=element_text(size = base_size - 2, family = "CM Roman"),
                    axis.title = element_text(size = base_size + 2, family = "CM Roman"),
                    plot.title = element_text(size = base_size, hjust = 0.5, family = "CM Roman"),
                    legend.position = 'bottom',
                    legend.text = element_text(size = base_size + 2, family = "CM Roman"),
                    legend.title = element_blank(),
                    legend.key.size = unit(2, "lines"))+
                guides(color = guide_legend(override.aes = list(size = 2.5)))+
                scale_x_date(date_labels = "%Y", date_breaks = "2 years")
            
        }
        else
        {
            p_i = p_i+
                theme(strip.text.y = element_text(size = 12),
                      axis.text=element_text(size=12),
                      axis.title = element_text(size=14),
                      plot.title = element_text(size=12, hjust = 0.5),
                      legend.position = 'bottom',
                      legend.title=element_blank())+
                scale_x_date(date_labels = "%Y")
        }    
        
        pdf_file_name = sprintf("%s%s.pdf", output_dir, var.name)
        
        ggsave(filename = pdf_file_name,
               plot=p_i,
               dpi=600,
               width = 183, 
               height = 183,
               units = 'mm',
               device = "pdf")        
        
        if (flag_font_latex)
        {
            embed_fonts(pdf_file_name)
        }
    }
}

###########################################################
# Function: calc_models
#
# Description:
# This auxiliary function estimates multiple Pooled Mean Group (PMG) regression models 
# using different profitability measures as independent variables. The models are 
# designed to analyze the impact of profitability, book equity, and market equity 
# on expected stock returns.
#
# Parameters:
# - panel_regression: A data frame containing the regression data, 
#   including variables such as profitability measures, book equity, market equity, and returns.
#
# Returns:
# - A list containing the estimated PMG models.
#
# Dependencies:
# - Requires the 'plm' package for panel data regression models.
# - Assumes the data frame 'panel_regression' contains the necessary variables:
#   OP, OP1, OP2, OP3, BE, ME, ME.lag1, r_i, r.3.0, and r.12.4.
#
# Functions:
# - pmg(): Estimate Pooled Mean Group regression models.
# - eval(): Parse and evaluate the text in the specified environment.
# - parse(): Parse text into an R expression.
# - sprintf(): Format and return a string.
#
# Notes:
# - It is recommended to use the Z-statistic from the summary() function for 
#   significance testing in PMG models, instead of using coeftest().
###########################################################

calc_models = function(panel_regression)
{
    # Define the independent variables for each model
    independent_vars_list = list(
        "OP + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP1 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP2 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP3 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP + OP2 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP1 + OP2 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP1 + OP3 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1",
        "OP + OP1 + OP2 + OP3 + log(BE/ME) + log(ME.lag1) + r.3.0 + r.12.4 - 1"
    )
    
    # Initialize a list to store the estimated models
    vector_models = vector('list', length = length(independent_vars_list))
    
    # Estimate each model and store it in the list
    for (i in seq_along(independent_vars_list)) 
    {
        formula_str = sprintf("r_i ~ %s", independent_vars_list[[i]])
        vector_models[[i]] = pmg(as.formula(formula_str), 
                                 data = panel_regression,
                                 index = c('Index', 'firm'))
    }
    
    return(vector_models)
}

gen_stats_table = function(dataset, 
                           alias, 
                           cor.method = "pearson",
                           language = 'PT-BR')
{
    table_stats = fBasics::basicStats(dataset)
    if (language == 'PT-BR')
    {
        table_stats = table_stats[c("Minimum","1. Quartile", "Mean", "Median","3. Quartile","Maximum","Stdev","Skewness","Kurtosis"),]
        rownames(table_stats) = c("Mínimo","1º quartil", "Média", "Mediana", "3º quartil", "Máximo","Desvio P.","Assimetria","Curtose")
    }
    else
    {
        table_stats = table_stats[c("Mean","Stdev","Minimum","Median","Maximum","Skewness","Kurtosis"),]
        rownames(table_stats) = c("Avg", "Std. dev.", "Minimum", "Median", "Maximum", "Skewness", "Kurtosis")
    }
    colnames(table_stats) = alias
    if (language == 'PT-BR')
    {
        table_stats["Coef. Vari.",] = table_stats["Desvio P.",]/table_stats["Média",]
    }
    else
    {
        table_stats["CV",] = table_stats["Std. dev.",]/table_stats["Avg",]
    }
    
    table_correlation = vector("list",3)
    table_correlation[[1]] = NA*cor(dataset)
    rownames(table_correlation[[1]]) = alias
    table_correlation[[2]] = NA*cor(dataset)
    table_correlation[[3]] = NA*cor(dataset)
    for (i in 1:ncol(dataset)) 
    {
        for (j in 1:ncol(dataset)) 
        {
            if (i != j)
            {
                test_cor = cor.test(dataset[,i],
                                    dataset[,j],
                                    method = cor.method)
                table_correlation[[1]][i,j] = test_cor$estimate
                table_correlation[[2]][i,j] = test_cor$p.value
                if (is.na(test_cor$estimate))
                {
                    stop('gen_stats_table:: association/correlation between paired samples is NA')
                }
                if (cor.method == 'pearson')
                {
                    table_correlation[[3]][i,j] = test_cor$statistic
                }
                else
                {
                    n = nrow(dataset)
                    Z_stats = (sqrt(n - 2) * test_cor$estimate) / sqrt(1 - test_cor$estimate^2)
                    table_correlation[[3]][i,j] = Z_stats
                }
            }
            else
            {
                table_correlation[[1]][i,j] = 1
                table_correlation[[2]][i,j] = 0
            }
        }
    }
    
    return(list(descriptive = table_stats,
                correlation = table_correlation,
                method = cor.method))
}

divide_quantile = function(variable, num_groups, valid_obs)
{
    groups = rep(num_groups, length(variable))
    limits = quantile(variable[valid_obs], 
                      probs = seq(0,100,100/num_groups)/100)[-1]
    for (i in (length(limits)-1):1) 
    {
        groups[variable <= limits[i]] = i
    }
    groups[!valid_obs] = 0
    return(groups)
}

gen_portfolio_table = function(assets, indicators,
                               market_value, 
                               num_groups = 3, 
                               avg_method = 'VW',
                               sort_type = 'size')
{
    info.frequency = min(diff(unique((indicators$Index))))
    if (info.frequency == 1)
    {
        rebalancing = 'anual'
    }
    else
    {
        if (info.frequency == 0.5)
        {
            rebalancing = 'semestral'
        }
        else
        {
            if (info.frequency == 0.25)
            {
                rebalancing = 'trimestral'
            }
            else
            {
                stop('gen_portfolio_table:: undetermined rebalancing period')
            }
        }
    }
    
    if (sort_type == 'size')
    {
        first_column = 'Small'
        last_column = 'Big'
        cat('gen_portfolio_table:: sorting by Size (market equity)\n')
    }
    else
    {
        first_column = 'Low'
        last_column = 'High'
        cat('gen_portfolio_table:: sorting by High-Low\n')
    }
    portfolio_table = zoo(matrix(NA,
                                 nrow = nrow(assets),
                                 ncol = num_groups+1),
                          order.by = index(assets))
    
    if (num_groups > 2)
    {
        colnames(portfolio_table) = c('neg',first_column, as.character(2:(num_groups-1)), last_column)
    }
    else
    {
        if (num_groups <= 1)
        {
            stop('gen_portfolio_table:: it is not possible to perform calculations with numbers of groups <= 1')
        }
        else
        {
            colnames(portfolio_table) = c('neg',first_column, last_column)    
        }
    }
    groups_names = colnames(portfolio_table)
    
    period = unique(indicators$Index)
    
    DR_table = zoo(matrix(NA,
                          nrow = nrow(assets),
                          ncol = num_groups+1),
                   order.by = period)
    colnames(DR_table) = groups_names
    
    DR_market = zoo(matrix(NA,
                           nrow = nrow(assets),
                           ncol = 1),
                    order.by = period)
    colnames(DR_market) = 'MKT'
    
    total_indicators = NULL
    for (i in 1:length(period))
    {
        subset_indicators = indicators[indicators$Index == period[i],]
        subset_indicators = subset_indicators[order(subset_indicators[,3]),] # coluna do indicador avaliado
        
        subset_indicators$group = divide_quantile(subset_indicators[,3], num_groups, subset_indicators$BE > 0)
        
        total_indicators = rbind(total_indicators, subset_indicators)
        
        first_date = as.yearmon(period[i])
        last_date = first_date + 1
        
        assets_ret  = assets[(index(assets) >= first_date) & (index(assets) < last_date),]
        
        market_value_group = market_value[market_value$Index == period[i],]
        
        market_value_group_wide = reshape(market_value_group, 
                                          idvar = "Index", 
                                          timevar = "firm", 
                                          direction = "wide")
        
        colnames(market_value_group_wide)[-1] = substring(colnames(market_value_group_wide)[-1],
                                                          4,
                                                          nchar(colnames(market_value_group_wide)[-1]))
        
        VM_i = market_value_group_wide[rep(1,nrow(assets_ret)),-1] # excluindo a coluna "Index" -> (data)
        
        assets_ret = assets_ret[,colnames(VM_i)]
        valid_assets = !is.na(assets_ret)
        
        x = VM_i*valid_assets/rowSums(VM_i*valid_assets, na.rm = TRUE)
        DR_market[period[i],'MKT'] = diversification_ratio(as.numeric(x[1,]),
                                                           cov(coredata(assets_ret)))                    
        
        for (j in 0:num_groups)
        {
            assets_groups = assets[(index(assets) >= first_date) & (index(assets) < last_date),
                                   as.character(subset_indicators$firm)[subset_indicators$group==j],
                                   drop = FALSE]
            if (ncol(assets_groups) == 0)
            {
                portfolio_table[index(assets_groups),groups_names[j+1]] = NA
            }
            else
            {            
                if (avg_method == 'EW')
                {
                    portfolio_table[index(assets_groups),groups_names[j+1]] = rowMeans(assets_groups,na.rm = TRUE)
                    
                    valid_assets = !is.na(assets_groups)
                    
                    x = valid_assets/rowSums(valid_assets, na.rm = TRUE) # 1/N (naive)
                    DR_table[period[i],groups_names[j+1]] = diversification_ratio(as.numeric(x[1,]),
                                                                                  cov(coredata(assets_groups)))                    
                }
                else
                {
                    VM_j = market_value_group_wide[,
                                                   as.character(subset_indicators$firm)[subset_indicators$group==j],
                                                   drop = FALSE]
                    VM_j = VM_j[rep(1,nrow(assets_groups)),]
                    
                    valid_assets = !is.na(assets_groups)
                    
                    num = rowSums(coredata(assets_groups)*VM_j*valid_assets, na.rm = TRUE)
                    den = rowSums(VM_j*valid_assets, na.rm = TRUE)
                    portfolio_table[index(assets_groups),groups_names[j+1]] = num/den
                    
                    x = VM_j*valid_assets/rowSums(VM_j*valid_assets, na.rm = TRUE)
                    
                    DR_table[period[i],groups_names[j+1]] = diversification_ratio(as.numeric(x[1,]), 
                                                                                  cov(coredata(assets_groups)))   
                }
            }
        }
    }
    
    characteristics = NULL
    for (i in 0:num_groups) 
    {
        characteristics_i = total_indicators[total_indicators$group == i,]
        characteristics = rbind(characteristics,
                                c(mean(characteristics_i[,3], na.rm = TRUE),
                                  mean(characteristics_i[,'BE']/characteristics_i[,'ME'], na.rm = TRUE),
                                  mean(characteristics_i[,'ME'], na.rm = TRUE)/1e6,
                                  nrow(characteristics_i)/length(unique(characteristics_i$Index)),
                                  mean(DR_table[,i+1], na.rm = TRUE)))
    }
    colnames(characteristics) = c(colnames(indicators)[3],'BE_ME','ME','n', 'DR')
    rownames(characteristics) = colnames(portfolio_table)
    
    DR.test = list(stats = rep(NA, nrow(characteristics)), 
                   p.value = rep(NA, nrow(characteristics)))
    for (i in 1:nrow(characteristics)) 
    {
        paired_test = t.test(coredata(DR_table[,i]), coredata(DR_market[,'MKT']), paired = TRUE, alternative = 'less')
        DR.test$stats[i] = paired_test$statistic
        DR.test$p.value[i] = paired_test$p.value
    }
    names(DR.test$stats) = rownames(characteristics)
    names(DR.test$p.value) = rownames(characteristics)
    DR.test$market = mean(coredata(DR_market[,'MKT']), na.rm = TRUE)
    
    portfolio_table = portfolio_table[(index(portfolio_table) >= as.yearmon(period[1])) & (index(portfolio_table) <= (as.yearmon(period[length(period)])+(info.frequency*12-1)/12)), ]
    portfolio_table$HighLow = portfolio_table[,last_column]-portfolio_table[,first_column]
    
    return(list(portfolios = portfolio_table, 
                characteristics = characteristics,
                diversification.ratio.test = DR.test,
                info = list(rebalancing = rebalancing)))
}

gen_portfolio_table_double_sort = function(assets, 
                                           indicators,
                                           market_value,
                                           num_groups_ME = 3,
                                           num_groups_OP = 3,
                                           avg_method = 'VW',
                                           international_breakpoints = FALSE)
{
    info.frequency = min(diff(unique((indicators$Index))))
    if (info.frequency == 1)
    {
        rebalancing = 'anual'
    }
    else
    {
        if (info.frequency == 0.5)
        {
            rebalancing = 'semestral'
        }
        else
        {
            if (info.frequency == 0.25)
            {
                rebalancing = 'trimestral'
            }
            else
            {
                stop('gen_portfolio_table_double_sort:: undetermined rebalancing period')
            }
        }
    }
    
    portfolio_table = zoo(matrix(NA,
                                 nrow = nrow(assets),
                                 ncol = num_groups_ME*num_groups_OP+1),
                          order.by = index(assets))
    groups_names = 'neg'
    for (i in 1:num_groups_ME) 
    {
        for (j in 1:num_groups_OP) 
        {
            groups_names = c(groups_names, sprintf('ME%d.OP%d',i,j))
        }
    }
    colnames(portfolio_table) = groups_names 
    
    period = unique(indicators$Index)
    
    DR_table = zoo(matrix(NA,
                          nrow = nrow(assets),
                          ncol = num_groups_ME*num_groups_OP+1),
                   order.by = period)
    colnames(DR_table) = groups_names
    
    DR_market = zoo(matrix(NA,
                           nrow = nrow(assets),
                           ncol = 1),
                    order.by = period)
    colnames(DR_market) = 'MKT'
    
    total_indicators = NULL
    for (i in 1:length(period))
    {
        subset_indicators = indicators[indicators$Index == period[i],]
        subset_indicators = subset_indicators[order(subset_indicators[,3]),] # coluna do indicador avaliado
        subset_indicators$group.OP = divide_quantile(subset_indicators[,3], num_groups_OP, subset_indicators$BE > 0)
        
        subset_indicators = subset_indicators[order(subset_indicators[,'ME']),] # coluna do valor de mercado (tamanho)
        subset_indicators$group.ME = divide_quantile(subset_indicators[,'ME'], num_groups_ME, subset_indicators$ME > 0)
        
        total_indicators = rbind(total_indicators, subset_indicators)
        
        first_date = as.yearmon(period[i])
        last_date = first_date + 1
        
        assets_ret  = assets[(index(assets) >= first_date) & (index(assets) < last_date),]
        
        market_value_group = market_value[market_value$Index == period[i],]
        
        market_value_group_wide = reshape(market_value_group, 
                                          idvar = "Index", 
                                          timevar = "firm", 
                                          direction = "wide")
        
        colnames(market_value_group_wide)[-1] = substring(colnames(market_value_group_wide)[-1],
                                                          4,
                                                          nchar(colnames(market_value_group_wide)[-1]))
        
        VM_i = market_value_group_wide[rep(1,nrow(assets_ret)),-1] # excluindo a coluna "Index" -> (data)
        
        assets_ret = assets_ret[,colnames(VM_i)]
        valid_assets = !is.na(assets_ret)
        
        x = VM_i*valid_assets/rowSums(VM_i*valid_assets, na.rm = TRUE)
        DR_market[period[i],'MKT'] = diversification_ratio(as.numeric(x[1,]),
                                                           cov(coredata(assets_ret)))                    
        
        for (j in 1:num_groups_ME)
        {
            for (k in 1:num_groups_OP) 
            {
                assets_groups = assets[(index(assets) >= first_date) & (index(assets) < last_date),
                                       as.character(subset_indicators$firm)[(subset_indicators$group.ME==j)&(subset_indicators$group.OP==k)],
                                       drop = FALSE]
                
                if (ncol(assets_groups) == 0)
                {
                    portfolio_table[index(assets_groups),groups_names[(j-1)*num_groups_OP+k+1]] = NA
                    browser()
                }
                else
                {            
                    if (avg_method == 'EW')
                    {
                        portfolio_table[index(assets_groups),groups_names[(j-1)*num_groups_OP+k+1]] = rowMeans(assets_groups,na.rm = TRUE)
                        
                        valid_assets = !is.na(assets_groups)
                        
                        x = valid_assets/rowSums(valid_assets, na.rm = TRUE) # 1/N (naive)
                        DR_table[period[i],groups_names[(j-1)*num_groups_OP+k+1]] = diversification_ratio(as.numeric(x[1,]),
                                                                                                          cov(coredata(assets_groups)))
                    }
                    else
                    {
                        VM_j = market_value_group_wide[,
                                                       as.character(subset_indicators$firm)[(subset_indicators$group.ME==j)&(subset_indicators$group.OP==k)],
                                                       drop = FALSE]
                        VM_j = VM_j[rep(1,nrow(assets_groups)),]
                        
                        valid_assets = !is.na(assets_groups)
                        
                        num = rowSums(coredata(assets_groups)*VM_j*valid_assets, na.rm = TRUE)
                        den = rowSums(VM_j*valid_assets, na.rm = TRUE)
                        portfolio_table[index(assets_groups),groups_names[(j-1)*num_groups_OP+k+1]] = num/den
                        
                        x = VM_j*valid_assets/rowSums(VM_j*valid_assets, na.rm = TRUE)
                        DR_table[period[i],groups_names[(j-1)*num_groups_OP+k+1]] = diversification_ratio(as.numeric(x[1,]),
                                                                                                          cov(coredata(assets_groups)))
                    }
                }
            }
        }    
        
        assets_groups = assets[(index(assets) >= first_date) & (index(assets) < last_date),
                               as.character(subset_indicators$firm)[subset_indicators$group.OP==0],
                               drop = FALSE]
        if (ncol(assets_groups) == 0)
        {
            portfolio_table[index(assets_groups),groups_names[1]] = NA
        }
        else
        {            
            if (avg_method == 'EW')
            {
                portfolio_table[index(assets_groups),groups_names[1]] = rowMeans(assets_groups,na.rm = TRUE)
                
                valid_assets = !is.na(assets_groups)
                
                x = valid_assets/rowSums(valid_assets, na.rm = TRUE) # 1/N (naive)
                DR_table[period[i],groups_names[1]] = diversification_ratio(as.numeric(x[1,]),
                                                                            cov(coredata(assets_groups)))        
                
            }
            else
            {
                market_value_group = market_value[market_value$Index == period[i],]
                
                market_value_group_wide = reshape(market_value_group, 
                                                  idvar = "Index", 
                                                  timevar = "firm", 
                                                  direction = "wide")
                
                colnames(market_value_group_wide)[-1] = substring(colnames(market_value_group_wide)[-1],
                                                                  4,
                                                                  nchar(colnames(market_value_group_wide)[-1]))
                VM_j = market_value_group_wide[,
                                               as.character(subset_indicators$firm)[(subset_indicators$group.OP==0)],
                                               drop = FALSE]
                VM_j = VM_j[rep(1,nrow(assets_groups)),]
                
                valid_assets = !is.na(assets_groups)
                
                num = rowSums(coredata(assets_groups)*VM_j*valid_assets, na.rm = TRUE)
                den = rowSums(VM_j*valid_assets, na.rm = TRUE)
                portfolio_table[index(assets_groups),groups_names[1]] = num/den
                
                x = VM_j*valid_assets/rowSums(VM_j*valid_assets, na.rm = TRUE)

                DR_table[period[i],groups_names[1]] = diversification_ratio(as.numeric(x[1,]),
                                                                            cov(coredata(assets_groups)))
            }
        }
    }
    
    portfolio_table = portfolio_table[(index(portfolio_table) >= as.yearmon(period[1])) & (index(portfolio_table) <= (as.yearmon(period[length(period)])+(info.frequency*12-1)/12)), ]
    
    characteristics = NULL
    characteristics_i = total_indicators[total_indicators$group.OP==0,]
    characteristics = rbind(characteristics,
                            c(mean(characteristics_i[,3], na.rm = TRUE),
                              mean(characteristics_i[,'BE']/characteristics_i[,'ME'], na.rm = TRUE),
                              mean(characteristics_i[,'ME'], na.rm = TRUE)/1e9,
                              nrow(characteristics_i)/length(unique(characteristics_i$Index)),
                              mean(DR_table[,1], na.rm = TRUE)))
    for (j in 1:num_groups_ME)
    {
        for (k in 1:num_groups_OP) 
        {
            characteristics_jk = total_indicators[(total_indicators$group.ME==j)&(total_indicators$group.OP==k),]
            characteristics = rbind(characteristics,
                                    c(mean(characteristics_jk[,3], na.rm = TRUE),
                                      mean(characteristics_jk[,'BE']/characteristics_jk[,'ME'], na.rm = TRUE),
                                      mean(characteristics_jk[,'ME'], na.rm = TRUE)/1e9,
                                      nrow(characteristics_jk)/length(unique(characteristics_jk$Index)),
                                      mean(DR_table[,(j-1)*num_groups_OP+k+1], na.rm = TRUE)))
        }
    }
    colnames(characteristics) = c(colnames(indicators)[3],'BE_ME','ME','n', 'DR')
    rownames(characteristics) = colnames(portfolio_table)
    
    DR.test = list(stats = rep(NA, nrow(characteristics)), 
                   p.value = rep(NA, nrow(characteristics)))
    for (i in 1:nrow(characteristics)) 
    {
        paired_test = t.test(coredata(DR_table[,i]), coredata(DR_market[,'MKT']), paired = TRUE, alternative = 'less')
        DR.test$stats[i] = paired_test$statistic
        DR.test$p.value[i] = paired_test$p.value
    }
    names(DR.test$stats) = rownames(characteristics)
    names(DR.test$p.value) = rownames(characteristics)
    
    DR.test$market = mean(coredata(DR_market[,'MKT']), na.rm = TRUE)
    
    return(list(portfolios = portfolio_table, 
                characteristics = characteristics,
                diversification.ratio.test = DR.test,
                info = list(rebalancing = rebalancing)))
}

gen_ME_characteristics = function(data,
                                  num_groups = 3,
                                  international_breakpoints)
{
    data = data[!is.na(data$log_BE_ME),]
    
    period = unique(data$Index)
    
    full_data = NULL
    for (i in 1:length(period)) 
    {
        subset_data = data[data$Index == period[i],]
        subset_data = subset_data[order(subset_data[,'ME']),]
        
        subset_data$group = divide_quantile(log(subset_data[,'ME']), num_groups, subset_data[,'ME'] > 0)
        subset_data$total_firms = length(unique(subset_data$firm))
        subset_data$total_cap = sum(subset_data[,'ME'])
        
        full_data = rbind(full_data, subset_data)
    }
    
    idx_profitability = which(grepl('OP', colnames(full_data)))
    
    characteristics = vector('list', length = num_groups)
    colnames_characteristics = c('tot_firms','perc_firms','avg_cap','total_cap','total_cap_perc','book_to_market', colnames(full_data)[idx_profitability])
    for (i in 1:num_groups) 
    {
        characteristics[[i]] = zoo(matrix(NA,
                                          ncol = length(colnames_characteristics),
                                          nrow = length(unique(full_data$Index))),
                                   order.by = unique(full_data$Index))
        colnames(characteristics[[i]]) = colnames_characteristics
        for (j in 1:length(period))
        {
            characteristics_ij = full_data[(full_data$Index == period[j]) & (full_data$group == i),]
            characteristics[[i]][period[j],'tot_firms'] = length(unique(characteristics_ij$firm))
            characteristics[[i]][period[j],'perc_firms'] = 100*length(unique(characteristics_ij$firm))/characteristics_ij[1,'total_firms']
            characteristics[[i]][period[j],'avg_cap'] = mean(characteristics_ij[,'ME'], na.rm = TRUE)/1e6
            characteristics[[i]][period[j],'total_cap'] = sum(characteristics_ij[,'ME'], na.rm = TRUE)/1e9
            characteristics[[i]][period[j],'total_cap_perc'] = 100*sum(characteristics_ij[,'ME'], na.rm = TRUE)/characteristics_ij[1,'total_cap']
            characteristics[[i]][period[j],'book_to_market'] = mean(characteristics_ij[,'BE']/characteristics_ij[,'ME'], na.rm = TRUE)
            for (k in idx_profitability) 
            {
                characteristics[[i]][period[j],colnames(full_data)[k]] = mean(characteristics_ij[,k], na.rm = TRUE)
            }
        }
    }
    
    result = NULL
    for (i in 1:num_groups)
    {
        result = rbind(result, colMeans(characteristics[[i]]))        
    }
    if (nrow(result) > 2)
    {
        rownames(result) = c('Small', as.character(2:(num_groups-1)), 'Big')   
    }
    else
    {
        if (nrow(result) <= 1)
        {
            stop('gen_ME_characteristics:: não é possível realizar cálculos com apenas números de grupos <= 1')
        }
        else
        {
            rownames(result) = c('Small', 'Big')    
        }
    }
    return(result)
}