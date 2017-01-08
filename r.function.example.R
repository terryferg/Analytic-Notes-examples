####Assumptions about the Raw data
### NOTE: Have this in the same directory as the datafiles and the demo.R. We make changes in demo.R for each project
##1. The data for test stores and potential control stores are in one excel file.
##2. Test stores is on the first sheet and potential control on the second.
##3. For every store, it has a unique ID which is given a variable name "store.id"
##4. The same variable has the same name in each data set


StoreMatch = function(test, control,
                      sale = NULL, sale.margin = NULL,
                      percent = NULL, percent.margin = NULL,
                      exact = NULL,
                      corr = NULL,
                      corr_threshold = NULL,
                      sortby = NULL, descending = TRUE,
                      state = "Out")
  ##test: a data.frame, Raw data of test stores
  ##control: a data.frame, Raw data of potential control stores
  ##sale: a character vector that contains the name of the SalesDollars variables
  ##sale.margin: a numeric vector that contains the corresponding margin of the variables in sale.
  ## When the number is equal or larger than 1, it is taken as a scalar: +/- margin;
  ## When the number is smaller than 1, then it is taken as a percent: * (1 +/- margin)
  ##percent: a character vector that contains the name of the DollarShare variables (characted since a list of variable names)
  ##percent.margin: a numeric vector that contains the corresponding margin of the variables in percent
  ##exact: a character vector that contains the variables that should be matched exactly
  ##state: denote the if we should search for potential control stores outside the state, 
  ## for state = "Out", only stores in Control file State B are considered; when state = 'In", the function will only search 
  ## stores in State A; For "Both", both all stores in both states are considered.
  ##corr: a character string that specifies the variable that will be used to calculate the correlation coef.
  ##sortby: match stores based on the descending order of the variable without duplication
{
  test.name = names(test)
  control.name = names(control)
  
  ##check variables
  tmp = c("store.id", sale, percent, exact, corr)
  if(sum(tmp %in% test.name)!=length(tmp))
    stop(paste0('Variable ', paste0(tmp[(tmp %in% test.name)==FALSE], collapse = ', '),
                ' not found in ', substitute(test)))
  if(sum(tmp %in% control.name)!=length(tmp))
    stop(paste0('Variable ', paste0(tmp[(tmp %in% control.name)==FALSE], collapse = ', '),
                ' not found in ', substitute(control)))
  ##check compare variables
  if(length(c(sale, percent, exact)) == 0)
    stop('No compare variable specified')
  ##check corr
  if(is.null(corr))
    stop('corr must be specified')
  if(length(corr) > 1)
    stop('more than one corr specified')
  ##check sortby
  if(length(sortby) > 1)
    stop('more than one sortby specified')
  if(is.null(sortby)==FALSE)
  {
    if(sortby %in% c(sale, percent)==FALSE)
      stop('sortby must be one of the variables specified in sale or percent')
  }
  ##check margin length
  if(length(sale)!=length(sale.margin))
    stop('sale and sale.margin have different length')
  if(length(percent)!=length(percent.margin))
    stop('percent and percent.margin have different length')
  ##check time span
  if(length(table(table(test$store.id)))!=1|length(table(table(control$store.id)))!=1)
  {
    tmp.test = table(test$store.id)
    tmp.test.count = table(tmp.test)
    tmp.test.mode = as.numeric(names(tmp.test.count)[which(tmp.test.count==max(tmp.test.count))])
    tmp.testID = names(tmp.test)[which(tmp.test!=tmp.test.mode)]
    
    tmp.control = table(control$store.id)
    tmp.control.count = table(tmp.control)
    tmp.control.mode = as.numeric(names(tmp.control.count)[which(tmp.control.count==max(tmp.control.count))])
    tmp.controlID = names(tmp.control)[which(tmp.control!=tmp.control.mode)]
    
    tmp.warn = list("Warning: Unequal time span found", testID = tmp.testID,
                    controlID = tmp.controlID)
    return(tmp.warn)
  }
  
  if(names(table(table(test$store.id))) != names(table(table(test$store.id))))
    stop(paste0('The time spans in ', substitute(test), ' and ', substitute(control), ' are not the same.'))
  
  ####calculate the 'unique' data frame 
  ### NOTE FROM TAF: creating an aggregate file for total Sale
  ### variables to make sure total category sales are within a certain percentage of each other in the test/control stores.
  ##test
  test$store.id = paste0(test$store.id, 't') ### TAF adds a "t" to test store.id so that not duplicate with control store id from differ state
  test.ID = unique(test$store.id)
  
  if(is.null(sale))
    test.sale = rep(NA, length(test.ID))
  else
  {
    test.sale = sapply(test.ID, function(x) {colSums(test[test$store.id == x, test.name %in% sale, drop = FALSE],
                                                     na.rm = TRUE)})
    if(length(sale) > 1)
      test.sale = t(test.sale)
  }

  if(is.null(exact))  ### TAF not sure what this is
    test.exact = rep(NA, length(test.ID))
  else
  {
    test.exact = sapply(test.ID, function(x) {test[test$store.id == x, test.name %in% exact, drop = FALSE][1, ]})
    if(length(exact) > 1)
      test.exact = t(test.exact)
  }

  
  if(is.null(percent))
    test.percent = rep(NA, length(test.ID))
  else
  {
    test.percent = sapply(test.ID,
                          function(x) {colMeans(test[test$store.id == x, test.name %in% percent, drop = FALSE],
                                                na.rm = TRUE)})
    if(length(percent) > 1)
      test.percent = t(test.percent)
  }

  
  test.unq <<- data.frame(test.ID, test.sale, test.exact, test.percent)
  names(test.unq) = c('store.id', sale, exact, percent)
  elim = apply(test.unq, 2, function(x) {sum(is.na(x))==length(test.ID)})  ### eliminate variables that user does not specify
  test.unq <<- test.unq[,elim==FALSE]
  
  ##control
  control$store.id = paste0(control$store.id, 'c')
  control.ID = unique(control$store.id)
  
  if(is.null(sale))
    control.sale = rep(NA, length(control.ID))
  else
  {
    control.sale = sapply(control.ID, function(x) {colSums(control[control$store.id == x, control.name %in% sale, drop = FALSE],
                                                           na.rm = TRUE)})
    if(length(sale) > 1)
      control.sale = t(control.sale)
  }
  
  if(is.null(exact))
    control.exact = rep(NA, length(control.ID))
  else
  {
    control.exact = sapply(control.ID, function(x) {control[control$store.id == x, control.name %in% exact, drop = FALSE][1, ]})
    if(length(exact) > 1)
      control.exact = t(control.exact)
  }
  
  
  if(is.null(percent))
    control.percent = rep(NA, length(control.ID))
  else
  {
    control.percent = sapply(control.ID,
                             function(x) {colMeans(control[control$store.id == x, control.name %in% percent, drop = FALSE],
                                                   na.rm = TRUE)})
    if(length(percent) > 1)
      control.percent = t(control.percent)
  }
  
  
  control.unq = data.frame(control.ID, control.sale, control.exact, control.percent)
  names(control.unq) = c('store.id', sale, exact, percent)
  elim = apply(control.unq, 2, function(x) {sum(is.na(x))==length(control.ID)})
  control.unq = control.unq[,elim==FALSE]
  
  ###### filtering
  if(state == 'Out')
    scope = control.unq   ## searching scope when match stores
  if(state == 'Both')
    scope = rbind(test.unq, control.unq)
  if(state == 'In')
    scope = test.unq
  
  ctrl1 = filter1(test.unq, scope, sale, sale.margin, percent, percent.margin, exact)  ##candidates
  
  if(length(ctrl1) == 0 & state == 'Out')
  {
    print('no match for any test store!')
    break
  }
  else if(length(unlist(ctrl1)) == length(test.ID) & (state == 'Both' | state == 'In'))
  {
    print('no match for any test store!')
    break
  }
  else
  {
    if(is.null(sortby))
    {
      matches = sapply(1:length(ctrl1),
                       function(x) {filter2(setdiff(ctrl1[[x]], test.ID[x]), test.ID[x], test, control, corr, corr_threshold)})
      matches = data.frame(test.ID, t(matches))
      names(matches) = c('Test store', 'Match', 'Corr')
    }
    else
    {
      srtindx = order(test.unq[,names(test.unq)==sortby], decreasing = descending)
      ctrl1 = ctrl1[srtindx]  ##sort by the variable specified in sortby
      test.ID = test.ID[srtindx]  ##adjust the order
      matched = NULL  ##store the ID of the control stores that have been matched
      matches = NULL
      for(i in 1:length(ctrl1))
      {
        ctrl_i = setdiff(ctrl1[[i]], c(matched, test.ID[i]))  ##eleminate matched stores and the test store itself
        match_i = c(test.ID[i], filter2(ctrl_i, test.ID[i], test, control, corr, corr_threshold))
        matches = rbind(matches, match_i)
        matched = c(matched, match_i[2])
      }
      matches = data.frame(matches, row.names = 1:length(ctrl1))
      names(matches) = c('Test store', 'Match', 'Corr')
    }
    matches$Corr = as.numeric(as.character(matches$Corr))
  }
  ###Calculate test/matched ratio of sale and percent
  if(state=='In')
    tmp1 = test.unq
  else
    tmp1 = rbind(test.unq, control.unq)
  
  ratio = apply(matches, 1,
        function(x) {if(is.na(x[2])==FALSE)
                       t(tmp1[tmp1$store.id==x[1], c(sale, percent)])/t(tmp1[tmp1$store.id==x[2], c(sale, percent)])
                     else
                       rep(NA, length(c(sale, percent)))
                    })
  if(length(c(sale, percent))>1)
  {
    ratio = as.data.frame(t(ratio))
    names(ratio) = paste0(c(sale, percent), '_ratio_test_over_control')
    matches = data.frame(matches, ratio)
  }
  else
  {
    matches[,paste0(c(sale, percent), '_ratio_test_over_control')] = ratio
  }
  
  ###calculate some statistics of the matches table
  stats_vb = c('Number of test stores', 'Number of matched test stores', 'Average correlation',
              paste0('Average ', names(matches)[-(1:3)]))
  stats = c(nrow(matches), sum(is.na(matches$Match)==FALSE), colMeans(matches[,-(1:2)], na.rm = TRUE))
  stats = data.frame(stats_vb, stats)
  
  ####output
  return(list(matches, stats))
}



##################filter1, nested in StoreMatch()
filter1 = function(Stst, Ssrch, sale, sale.margin, percent, percent.margin, exact)
{
  if(is.null(sale))
    I.sale = NULL
  else
  {
    I.sale = lapply(1:length(sale),
                    function(x) {sapply(Stst[, names(Stst)==sale[x]],
                                        function(y) {tmp = Ssrch[, names(Ssrch)==sale[x]];
                                        if(sale.margin[x]>=1) {tmp > y - sale.margin[x] & tmp < y + sale.margin[x]}
                                        else {tmp > y * (1 - sale.margin[x]) & tmp < y * (1 + sale.margin[x])}})})
  }
  
  if(is.null(percent))
    I.percent = NULL
  else
  {
    I.percent = lapply(1:length(percent),
                       function(x) {sapply(Stst[, names(Stst)==percent[x]],
                                           function(y) {tmp = Ssrch[, names(Ssrch)==percent[x]];
                                           tmp > y - percent.margin[x] & tmp < y + percent.margin[x]})})
  }

  if(is.null(exact))
    I.exact = NULL
  else
  {
    I.exact = lapply(1:length(exact),
                     function(x) {sapply(Stst[, names(Stst)==exact[x]],
                                         function(y) {Ssrch[, names(Ssrch)==exact[x]]==y})})
  }

  
  I.all = c(I.sale, I.percent, I.exact)
  elim = sapply(I.all, function(x) {is.null(x)==FALSE})  ##delete those variable that user does not specify
  I.all = I.all[elim]
  I.and = matrix(rep(FALSE, nrow(Ssrch)*nrow(Stst)), nrow = nrow(Ssrch))
  for(i in 1:length(I.all))
  {
    tmp = I.all[[i]]
    tmp[is.na(tmp)] = FALSE  ##NAs are taken as FALSE
    I.and = I.and + tmp
  }
  
  I.and = I.and == length(I.all)
  round1 = apply(I.and, 2, function(x) {Ssrch$store.id[x]})
  if(length(round1)!=0)
    names(round1) = Stst$store.id
  
  return(round1)
} ##output a list that shows those stores that satisfy all criterions


#########filter2, nested in StoreMatch()
filter2 = function(cddt, testID, test, control, corr, corr_threshold)
  ## find the store with the largest  es are matched in filter1()
  ## cddt: candidates
  ## output the StoreID and corresponding corr
{
  test[is.na(test[,corr]), corr] = 0
  control[is.na(control[,corr]), corr] = 0
  if(length(cddt)==0)
    return(c(NA, NA))
  
  if(length(cddt)>=1)
  {
    control.ID = unique(control$store.id)
    cddt.cor = sapply(cddt,
                      function(x) {if(x %in% control.ID) {test.tmp = test[test$store.id==testID, names(test)==corr];
                                                          control.tmp = control[control$store.id==x, names(control)==corr];
                                                          
                                                         if(var(test.tmp)>0 & var(control.tmp)>0)
                                                          cor(test.tmp, control.tmp)
                                                         else
                                                            -1}
                                   else {test.tmp = test[test$store.id==testID, names(test)==corr];
                                         control.tmp = test[test$store.id==x, names(test)==corr];
                                   
                                         if(var(test.tmp)>0 & var(control.tmp)>0)
                                           cor(test.tmp, control.tmp)
                                         else
                                           -1}})
    
    if(is.null(corr_threshold)==FALSE)
    {
      if(max(cddt.cor)>=corr_threshold)
        return(c(cddt[which(cddt.cor==max(cddt.cor))], max(cddt.cor)))
      else
        return(c(NA,NA))
    }
    else
      return(c(cddt[which(cddt.cor==max(cddt.cor))], max(cddt.cor)))
  }
}



