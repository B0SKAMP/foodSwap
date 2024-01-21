#
#
#

MY_KJ_REQ <- 10000

foodSwap_env <- new.env(parent=emptyenv())

#' 
#' @export
init <- function(targets_file, nutrientbank_file, intake_ndns_file, nutrientbank_field_names) {
    foodSwap_env$nutrientbank_field_names = nutrientbank_field_names
	t<-read.delim(targets_file, stringsAsFactors = F)
	TARGNA <- as.numeric(t[1,])
	names(TARGNA) <- names(t)
	w <- which(!is.na(TARGNA))
	foodSwap_env$TARGNA <- TARGNA[w]

	foodSwap_env$MY_KJ_REQ = 10000
	DIRECTIONS <- t[3,w]
	DIRECTIONS['KJ'] <- '='
	foodSwap_env$DIRECTIONS <- as.character(DIRECTIONS)

	foodSwap_env$NUTRIENT_MESSAGES <- t[4,w]

	NDNS <- read.csv(nutrientbank_file)
	fNDNS <- NDNS
	remove_cats <- c("Biscuits","Ice cream","Artificial sweeteners","Sugar confectionery","Chocolate confectionery","Soft drinks, not diet","Soft drinks, diet","Beer lager cider and perry","Wine","Alcoholic Beverages","Spirits and liqueurs","Dietary supplements","Commercial toddlers foods and drinks")
	fNDNS <- fNDNS[!fNDNS$NDNSmainFoodGroup %in% cats,]
	fNDNS <- fNDNS[!is.na(fNDNS$ProcessingCode),]
	fNDNS <- fNDNS[fNDNS$ProcessingCode  != 4,]

	
	foodSwap_env$ALL_FOODNUMBERS <- NDNS$FoodNumber
	targna_names = names(foodSwap_env$TARGNA)
	targna_names[targna_names == "GHGE"] <- nutrientbank_field_names$ghge
	targna_names[targna_names == "Average.supermarket.cost"] <- nutrientbank_field_names$cost
	foodSwap_env$NDNS <- NDNS[c('FoodNumber', nutrientbank_field_names$foodname, 'Base', 'Units', targna_names, nutrientbank_field_names$min, nutrientbank_field_names$max)]
	foodSwap_env$fNDNS <- fNDNS[c('FoodNumber', nutrientbank_field_names$foodname, 'Base', 'Units', targna_names, nutrientbank_field_names$min, nutrientbank_field_names$max)]
	
	foodSwap_env$NDNSE <- read.delim(intake_ndns_file)


	foodSwap_env$N_CONSTRAINTS <- length(w)
	
	names(foodSwap_env$TARGNA) <- names(foodSwap_env$NDNS)[5:(4+length(w))]

}

#' get foodnumbers with FoodNames.
#' used to update the FoodNumbers selectize
#' @export
getFoods <- function() {
	foods <- foodSwap_env$NDNS$FoodNumber
	names(foods) <- foodSwap_env$NDNS[,nutrientbank_field_names$foodname]
	foods
}

#' get foodnumbers with FoodNames.
#' used to update the FoodNumbers selectize
#' @export
onlyGoodFoods <- function() {
	
	foods <- foodSwap_env$fNDNS$FoodNumber
	names(foods) <- foodSwap_env$fNDNS[,nutrientbank_field_names$foodname]
	foods
}


#' get foodnumbers from NDNS, optionally filter away those with GHGE=NA and/or cost=NA
#' used to set rv_foodnumbers
#' @export
get_available_foodnumbers <- function(ghge=F, price=F){
  n <- foodSwap_env$NDNS
  if(ghge) {
    n <- dplyr::filter(n, !is.na(GHGE))
  }
  if(price){
    n <- dplyr::filter(n, !is.na(Average.supermarket.cost))
  }
  n$FoodNumber
}

#' reverse badness value into goodness value
#' used everywhere
#' @export
bad_to_good <- function(badness) {
  100-4*badness
}

#' get the items (and EnergykJ amounts) from ndnse.
#' @param random_s the seriali we are getting
#' @param available_fn vector of allowed FoodNumbers.
#' @return data.frame with FoodNumber and EnergykJ
#' @export
get_base_items <- function(random_s, available_fn) {
  # returns (FoodNumber, EnergykJ) or NULL
  print("get_base_items")
  print(random_s)
  # 
  if(is.null(random_s)) {
    NULL
  }else{
    bi <- dplyr::filter(foodSwap_env$NDNSE, seriali==random_s, FoodNumber %in% available_fn)[c(1,2)]
  }
}


#' choose  nextra items from choose_from.
#' @param choose_from vector of foodnumbers
#' @param nextra numbers of items to choose
#' @return data.frame with FoodNumber= the chosen items; EnergykJ=0
#' used in r_extra_items
#' @export
get_extra_items <- function(choose_from, nextra) {
  # choose_from must be a list of foodnumbers
  # return (FoodNumber, EnergykJ) or NULL
  print("get_extra_items")
  if(nextra == 0){
    NULL
  }else{
	chosen <- sample(choose_from,nextra)
	#print(chosen)
    data.frame(FoodNumber=chosen,grams=rep(0,nextra))
  }
}

#' join the accepted and the rejected foodnumbers.
#' @param accepted data.frame with FoodNumber= the accepted foodnumbers
#' @param rejected data.frame with FoodNumber= the rejected foodnumbers
#' @return the union of the FoodNumbers from both arguments.
#' @export
get_chosen_items <- function(accepted, rejected){
  # accepted & rejected are (FoodNumber, EnergykJ)
  # we return only FoodNumber
  print("get_chosen_items")
  
  union(accepted$FoodNumber, rejected$FoodNumber)
}

#' group and sum the items by FoodNumber; then merge them with all nutrient bank columns
#' @param items  data.frame with FoodNumber and EnergykJ
#' @return items grouped and summed by FoodNumber then merged with all nutrient bank columns. (data.frame)
#' @export
merge_items <- function(items){
  print("merge_items")
  grouped <- dplyr::group_by(items, FoodNumber)
  summarized <- dplyr::summarize(grouped, grams=sum(grams))
  merged <- merge(summarized, foodSwap_env$NDNS, by="FoodNumber")
  #if(is.null(merged$EnergykJ)) {
  #  merged$grams = merged$grams / merged$Base * merged$KJ
  #  names(merged)[names(merged)=="grams"]=="EnergykJ"
  #}
  rownames(merged) <- merged$FoodNumber
  merged
}

#' we merge new_energykJ from accepted into mbi, and override their energykJ with new_energykJ.
merge_accepted <- function(mbi, accepted) {
  print("merge_accepted")
  if(is.null(accepted)) {
    mbi
  }else{
    stripped_accepted <- dplyr::select(accepted, FoodNumber, new_g)
    firstmerge <- merge(mbi, stripped_accepted, by="FoodNumber", all.x=TRUE)
    #firstmerge$EnergykJ[!is.na(firstmerge$new_energykJ)] <- firstmerge$new_energykJ[!is.na(firstmerge$new_energykJ)]
	firstmerge$grams[!is.na(firstmerge$new_g)] <- firstmerge$new_g[!is.na(firstmerge$new_g)]
    firstmerge <- firstmerge[1:(length(firstmerge)-1)]
    dif <- setdiff(stripped_accepted$FoodNumber, mbi$FoodNumber)
    names(stripped_accepted) <- c('FoodNumber', 'grams')
    merged_new_items <- merge_items(stripped_accepted[stripped_accepted$FoodNumber %in% dif,])
    rbind(firstmerge, merged_new_items)
  }
}

find_impossible_targets <- function(t, swap_from, fixed_foodnumbers) {
  # calculate nutritional values of rejected choices for the nutrients with an upper limit.
  # if the sum of rejected (and therefore FIXED) nutrients exceed any of the <= targets, our
  # constraints are unsolvable
  # WE MAY have to add accepted to this: accepted changes are also fixed!
  # RETURN: the number of failed targets.
  merged_fixed <- swap_from[swap_from$FoodNumber %in% fixed_foodnumbers,]
  nutr_per_baseunit <- merged_fixed[, 6:(length(swap_from)-2)]
  nutr_amounts <- nutr_per_baseunit * merged_fixed$grams / merged_fixed$Base
  sum_nutr_amounts <- colSums(nutr_amounts)
  s <- which(sum_nutr_amounts >= t$targets & t$directions == "<=")
  print(paste("impossible targets: ", paste(names(s))))

  sum_nutr_amounts[s]*1.001
}

#' do the actual swap
#' @export
make_swap <- function(swap_from, targets, accepted, rejected, objf) {
  #make_swap <- function(swap_from, targets, maxf, ghge=FALSE, price=FALSE, accepted=NULL, rejected=NULL) {
  maxf = 2
  print("make_swap")
  
  
  myt <- targets$targets
  names(myt) <- names(foodSwap_env$TARGNA)
  
  it <- find_impossible_targets(targets, swap_from, union(accepted$FoodNumber, rejected$FoodNumber))
  if(length(it) > 0){
    myt[names(it)]  <- it
  }
  
  n_decision_vars <- length(swap_from[,1])
  
  Amat <- t(swap_from[,names(swap_from) %in% rownames(targets)])
  bought_amounts <- swap_from$grams/swap_from$Base
  
  current_nutr <- Amat %*% bought_amounts
  n <- colnames(Amat)
  Amat <-  cbind(Amat,-Amat)
  lp <- lpSolveAPI::make.lp(sum(names(swap_from) %in% rownames(targets)), 2*n_decision_vars)
  lpSolveAPI::lp.control(lp, sense="min", presolve="sensdual")
  for(i in 1:length(Amat[1,])){
    lpSolveAPI::set.column(lp,i,Amat[,i])
  }
  #cvec <- rep(1, 2*n_decision_vars)
  if(objf == "max"){
    cvec1 <- swap_from$Base / swap_from[,foodSwap_env$nutrientbank_field_names$max]
  }else if(objf == "min") {
    cvec1 <- swap_from$Base / swap_from[,foodSwap_env$nutrientbank_field_names$min]
  }else{
    cvec1 <- rep(1, n_decision_vars)
  }
  cvec <- c(cvec1, cvec1)
  rhs <- myt - current_nutr
  rhs[is.na(rhs)] <- 0
  lpSolveAPI::set.rhs(lp, rhs)
  lpSolveAPI::set.constr.type(lp,targets$directions)
  if(!is.null(accepted)){
    #print("accepted:")
    #print(accepted)
    #df <- data.frame(FoodNumber=accepted$FoodNumber, acceptedEnergykJ=accepted$EnergykJ, accepted=TRUE)
    df <- data.frame(FoodNumber=accepted$FoodNumber, acceptedgrams=accepted$new_g, accepted=TRUE)
    swap_from <- dplyr::left_join(swap_from, df, by="FoodNumber")
  }else{
    swap_from$accepted=NA
    swap_from$acceptedgrams=NaN
  }
  if(!is.null(rejected)) {
    swap_from <- dplyr::left_join(swap_from, data.frame(FoodNumber=rejected$FoodNumber, rejected=TRUE), by="FoodNumber")
  }else{
    swap_from$rejected=NA
  }
  
  lowerlim <- rep(0, 2*n_decision_vars)
  upper_firsthalf <- round(swap_from[,foodSwap_env$nutrientbank_field_names$max]/swap_from$Base-bought_amounts, digits=5)
  upperlim <- c(upper_firsthalf, bought_amounts)
  #print("upper")
  #print(upper_firsthalf)
  #print(bought_amounts)
  # lowerlim is 0 means the minimum change is 0.
  #upperlim <- c(swap_from$fourday_max/swap_from$Base  - bought_amounts, bought_amounts) 
  
  # divide fourdaymax by Base because unfortunately the portion numbers are in grams while everything else is in base units (100 grams) - we should change this
  # upperlim for 'add': max portion - bought_amount
  # upperlim for 'deduct': bought_amounts; meaning the minimum result is 0.
  wmr <- which(swap_from$rejected)
  wma <- which(swap_from$accepted)
  # rejected - limits for add: 0
  #print("setting upper limits to 0 for rejected swaps:")
  #print(wmr)
  #upperlim[wmr] <- 0
  #lowerlim[wmr] <- 0
  # rejected - limits for deduct: 0
  #upperlim[n_decision_vars + wmr] <- 0
  # try to penalize change in rejected changes by setting a high penalty in OBJF:
  cvec[wmr] <- 999999999
  cvec[n_decision_vars+wmr] <- 999999999
  #print("new upper")
  #print(upperlim[1:n_decision_vars])
  #print(upperlim[(n_decision_vars + 1):(2*n_decision_vars)])
  
  #lowerlim[n_decision_vars + wmr] <- 0
  # accepted - limit is 

  accepted_amounts <- swap_from$acceptedgrams / swap_from$Base
  accepted_limit <- accepted_amounts - bought_amounts
  #print("accepted limits")
  #print(accepted_limit[!is.null(accepted_limit)])
  #print(data.frame(acceptkJ = swap_from$acceptedEnergykJ, 
  #                 KJ=swap_from$KJ, 
  #                 ba=bought_amounts, 
  #                 accepted_amount=(swap_from$acceptedEnergykJ/swap_from$KJ), 
  #                 accepted_limit=(swap_from$acceptedEnergykJ/swap_from$KJ-bought_amounts)))
  acceptedgz <- which(swap_from$accepted & (accepted_limit > 0))
  acceptedsz <- which(swap_from$accepted & (accepted_limit < 0))
  lowerlim[acceptedgz] <- accepted_limit[acceptedgz]
  lowerlim[acceptedsz] <- 0
  lowerlim[n_decision_vars + acceptedgz] <- 0
  lowerlim[n_decision_vars + acceptedsz] <- abs(accepted_limit)[acceptedsz]
  #print("lower")
  #print(lowerlim[1:n_decision_vars])
  #print(lowerlim[(n_decision_vars+1):(2*n_decision_vars)])
  
  #upperlim[round(upperlim, 2)==0] <- 0

  #recheck_impossible_targets(it, swap_from, upperlim, accepted$FoodNumber, rejected$FoodNumber, directions, myt, current_nutr)
  
  lpSolveAPI::set.bounds(lp, lower=lowerlim, upper=upperlim)
  lpSolveAPI::set.objfn(lp,cvec)
  
  res<-lpSolveAPI::solve.lpExtPtr(lp)
  if(res != 0) {
    print("solve failed")
    print(res)
    
    c(sol=NULL, lp=lp)
    #lp
  }else{
    print("solve succeeded")
    sol <- lpSolveAPI::get.variables(lp)
    sol <- bought_amounts + sol[1:n_decision_vars] - sol[(n_decision_vars+1):(2*n_decision_vars)]
    sol <- swap_from$Base*sol
    list(sol=sol, lp=lp)
  }
}

#' calculates the number of days based on the energy in merged_base_items, then 
#' multiplies TARGNA targets by the number of days.
#' @param merged_base_items data.frame with items with FoodNumber and EnergykJ
#' @param price logical; do we constrain on price?
#' @param price_factor  if we constrain on price we aim to keep price under price_factor x original price
#' @param ghge logical; do we constrain on green house gas emissions?
#' @param ghge_factor if we constrain on ghge we aim to keep ghge under ghge_factor x original ghge
#' @return data.frame with directions=c("<=", "<=", ">=") and targets=numeric vector.
#' @export
get_targets <- function(merged_base_items, price, price_factor, ghge, ghge_factor) {
  print("get_targets")
  kj_total <- sum(merged_base_items$grams/merged_base_items$Base * merged_base_items$KJ)
  diet_days = kj_total / MY_KJ_REQ
  t <- foodSwap_env$TARGNA
  d <- foodSwap_env$DIRECTIONS
  names(d) <- names(t)
  t['KJ'] <- foodSwap_env$MY_KJ_REQ
  t['FAT'] <- foodSwap_env$TARGNA['FAT'] * foodSwap_env$MY_KJ_REQ / 3700
  t['CHO'] <- foodSwap_env$TARGNA['CHO'] * foodSwap_env$MY_KJ_REQ / 1700
  t['SATFA'] <- foodSwap_env$TARGNA['SATFA'] * foodSwap_env$MY_KJ_REQ / 3700
  t['FREESUG'] <- foodSwap_env$TARGNA['FREESUG'] * foodSwap_env$MY_KJ_REQ / 1700
  t <- t * diet_days
  bought_amounts <- merged_base_items$grams / merged_base_items$Base
  if(ghge){
    ghge_values <- merged_base_items$GHGE
    ghge_values[is.na(ghge_values)] <- 0
    t[foodSwap_env$nutrientbank_field_names$ghge] <- ghge_factor * sum(bought_amounts * ghge_values)
    d[foodSwap_env$nutrientbank_field_names$ghge] <-  "<="
  }
  if(price){
    price_values <- merged_base_items$Average.supermarket.cost
    price_values[is.na(price_values)] <- 0
    t[foodSwap_env$nutrientbank_field_names$cost] <- price_factor * sum(bought_amounts * price_values)
    d[foodSwap_env$nutrientbank_field_names$cost] <- "<="
  }

  data.frame(directions=d, targets=t, stringsAsFactors = FALSE)
}

#' lists the amounts of nutrients for sol and swap_from and shows the targets and badness for each nutrient.
#' @param sol solution amounts
#' @param t targets with directions
#' @param swap_from merged base items with accepted choices
#' @param mbi merged_base_items - the original food list.
#' @return data.frame the table showing the amounts of nutrients, the targets, and the badness for sol, mbi, and swap_from
#' @export
get_nutrient_matrix_with_badness <- function(sol, t, swap_from, mbi) {
  # mbi: merged base items; the original random food list
  # swap_from:  merged base items merged with accepted choices.
  # sol order is based on swap_from. MBI is shorter..
  if(is.null(sol)){
    NULL
  }else{
    print("get_nutrient_matrix_with_badness")
	last_nutr_col = dim(swap_from)[2]-2
    sol_changed_units <- sol / swap_from$Base
    sol_nutrition_values <- sol_changed_units * swap_from[6:last_nutr_col]
    swap_from_nutrition_values <- swap_from$grams / swap_from$Base * swap_from[6:last_nutr_col]
    mbi_nutrition_values <- mbi$grams / mbi$Base * mbi[6:last_nutr_col]
    sol_vals <- colSums(sol_nutrition_values)
    swap_from_vals <- colSums(swap_from_nutrition_values)
    mbi_vals <- colSums(mbi_nutrition_values)
    sol_badness <- calc_badness_per_target(sol_vals, t)
    swap_from_badness <- calc_badness_per_target(swap_from_vals, t)
    mbi_badness <- calc_badness_per_target(mbi_vals, t)
    data.frame(names=names(swap_from)[6:last_nutr_col], sol=sol_vals, solbad=sol_badness,  mbi=mbi_vals, mbibad=mbi_badness, swap_from=swap_from_vals, swap_frombad=swap_from_badness, targets=paste(t$directions,t$targets))
  }
}

#' calculate the badness vector
calc_badness_per_target <- function(nutri_vals, t){
  badness_score <- rep(0, length(t[,1]))
  b1 <- (t$targets / nutri_vals) - 1
  b2 <- (nutri_vals / t$targets) - 1
  b1[is.na(b1)] <- 0
  b2[is.na(b2)] <- 0
  
  badness_score[t$directions == "<=" & b2 > 0] <- b2[t$directions == "<=" & b2 > 0]
  badness_score[t$directions == ">=" & b1 > 0] <- b1[t$directions == ">=" & b1 > 0]
  badness_score[badness_score == Inf] <- 20
  badness_score
}

#' this calculates for each swap WHY it occurred and what the badness_diff is.
#' @param sol solution
#' @param t targets
#' @param m swap_from - this is base_items with the accepted_swaps added.
#' @return data.frame with swaps with bad, why, good, and ori_good
#' @export
get_badness_per_swap <- function(sol, t, m) {
  # m = swap_from; which is base_items plus accepted swaps.
  if(is.null(sol)){
    NULL
  }else{
    last_nutr_col = dim(m)[2]-2
    g <- m$grams
    diff <- abs(g - sol) # diff in grams
    s_base_units <- sol / m$Base # s was in grams. divide by Base to go back to base units
    ori_nutrition <- m$grams / m$Base * m[6:last_nutr_col]
    new_nutrition <- s_base_units * m[6:last_nutr_col]
    nutri_vals <- colSums(ori_nutrition)
    badness_per_swap <- rep(0,length(sol))
    goodness_per_swap <- rep(0, length(sol))
    better_desc = rep("", length(sol))
    ori_badnesses = calc_badness_per_target(nutri_vals, t)
    ori_badness <- sum(ori_badnesses)
    
    ori_goodness <- bad_to_good(ori_badness)
    for(i in 1:length(sol)){
      my_nutri_vals <- nutri_vals - ori_nutrition[i,] + new_nutrition[i,]
      my_badnesses = calc_badness_per_target(my_nutri_vals, t)
      max_badness_diff = max(ori_badnesses - my_badnesses)
      if(max_badness_diff != 0){
        most_improved_targets = which((ori_badnesses - my_badnesses) == max_badness_diff)
        better_desc[i] <- paste(names(foodSwap_env$TARGNA)[most_improved_targets], collapse=",")
      }
      b <- sum(calc_badness_per_target(my_nutri_vals, t))
      badness_per_swap[i] <- b
      goodness_per_swap[i] <- bad_to_good(b)
    }
    r <- data.frame(product=m[,nutrientbank_field_names$foodname], 
                    FoodNumber=m$FoodNumber,
                    kj=m$KJ, 
                    ori_g=g, 
                    units=m$Units, 
                    new_g=sol,
                    diff=diff,
                    bad=badness_per_swap,
                    why=better_desc,
                    good=goodness_per_swap,
                    ori_good=ori_goodness
    )
  }
  
}

#' make the swap_from list, which contains base items, 
#' @param bi
#' @param rejected
#' @param accepted
#' @param mei
#' @return merged
#' @export

get_swap_from <- function(bi, rejected, accepted, mei) {
  print("get_swap_from")
  if(!is.null(rejected)) {
    bi <- rbind(bi, data.frame(FoodNumber=rejected$FoodNumber, grams=0))
    # this way we make sure any rejects are there, without influencing quantities
  }
  mbi <- merge_items(bi)
  mbi <- merge_accepted(mbi, accepted)
  if(!is.null(mei)){
    mei <- dplyr::filter(mei, !FoodNumber %in% mbi$FoodNumber)
    mbi <- rbind(mbi, mei)
  }
  # rejected items should not be merged into swap_from - they are already in swap_from 
  # and will keep their original quantity because of bounds in make_swap.
  mbi
}

#' print an R var and its type
#' @export
inspect <- function(x) {
  print(paste(x, '(', typeof(x), ')'))
}

#' Using a single record from 'get_badness_per_swap' we create an english readable message about this swap.
#' @param c a single record from 'get_badness_per_swap' - data.frame with only 1 record. contains fields new_g, ori_g, product, why, good, ori_good.
#' @return data.frame the swap is described in 'swap'. the 'goodness' change is described in 'well'. the why (the value of the nutrient involved) is described in 'why'.
#' @export
make_swap_string <- function(c) {
  print("make_swap_string")
  print(c)
  if(!is.null(c) && length(c) != 0 && !is.na(c$ori_g)) {
    if(c$ori_g == 0){
      string <- paste("Add", round(c$new_g), "grams of", c$product)
    }else if(round(c$new_g,digits=1) == 0){
      string <- paste("Remove all", c$product)
    }else if(c$new_g > c$ori_g) {
      string <- paste("Increase the amount of", c$product, "to", round(c$new_g), "grams (from", round(c$ori_g), "grams)")
    }else{
      string <- paste("Reduce the amount of", c$product, "to", round(c$new_g), "grams (from", round(c$ori_g), "grams)")
    }
    if(c$why %in% names(NUTRIENT_MESSAGES)){
      why <- paste("This would ", NUTRIENT_MESSAGES[as.character(c$why)])
    }else{
      why = ""
    }
    well_string <- paste("This swap would add ", format(c$good-c$ori_good, digits=1), "improvement points.")
    data.frame(swap = string, well = well_string, why=why)
  }else{
    NULL
  }
}


#' @export
fourdaymax <- function(fnum) {
  foodSwap_env$NDNS[foodSwap_env$NDNS$FoodNumber == strtoi(fnum), foodSwap_env$nutrientbank_field_names$max]
}

#' @export
portion <- function(fdm) {
  signif(fdm/200,1)*100
}

#' @export
curval <- function(fnum) {
  portion(fourdaymax(fnum))
}

#' @export
minval <- function(fnum) {
  #portion(fourdaymax(fnum))/10
  0
}

#' @export
maxval <- function(fnum) {
  portion(fourdaymax(fnum))*10
}

#' @export
stepval <- function(fnum) {
  portion(fourdaymax(fnum))/2
}

#' @export
foodname <- function(fnum) {
  foodSwap_env$NDNS[foodSwap_env$NDNS$FoodNumber==strtoi(fnum),foodSwap_env$nutrientbank_field_names$foodname]
}



