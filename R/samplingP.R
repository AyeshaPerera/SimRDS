#' Extracts sample from generated populations
#' @description Extract samples from the genrated populations using 'SimRDS::population' function.
#' Can directly use this output to te RDS estimates in RDS package
#'
#' @usage samplingP (population, seeds, coupon, waves = NULL, size = NULL, prob=NULL, prop_sam = NULL,
#' showids = T, timeInterval = NULL)
#'
#' @param population Population generated using 'SimRDS::population' function. The list output
#' should be fed as the value for this parameter.
#' @param seeds Number of seeds. Should be a positive integer
#' @param coupon Number of coupons. Should be a positive
#' integer
#' @param waves Number of waves. Should be a positive integer
#' @param size Sample size. Should be a positive integer
#' @param prob Whether the seeds are selected probabilistically. If want to be selected randomly
#' then leave ot as 'NULL' If want to relate it to a variable add the variable name and the it
#' should be related. For example if the seeds selected should depend on the network size then 'network'
#' should be used. If it is inversly proportional then should enter as '1/network'.
#' @param prop_sam If the individuals from the network of an individual should be chosen randomly
#' use "NULL". If the selection should be done probabilistically state the variable and how it should
#' be related.
#' @param showids If TRUE displas the IDs beign recruited.
#' @param timeInterval Days between recruitment waves. zeroth wave is assumed to recruit at the present
#' day
#'
#' @inheritParams RDS::as.rds.data.frame
#' @inheritParams RDS::get.seed.id
#'
#' @usage network = network size; chara = character
#' @examples
#' samplingP(population, seeds = 10, coupon = 2, waves = 8, prob = '1/network', prob_sum = 'chara')
#' @export
samplingP = function(population,
                     seeds,
                     coupon,
                     waves = NULL,
                     size = NULL,
                     prob=NULL,
                     prop_sam = NULL,
                     showids = T,
                     timeInterval = NULL){
  if(is.null(waves)&&is.null(size)){ stop("Must provide number of waves or sample size")}
  data = population$Contact; row.names(data) = 1:nrow(data)
  degree = population$Degree; names(degree) = c("id", "total")
  pop = population$frame
  pop = pop[,names(pop)%in%c("s", "netSize", "network", "character", "response")]
  N = nrow(pop)
  network = pop$netSize; chara = pop$character

  y111=population$`Adjusent Matrix`
  l=sample(degree$id,seeds,replace=F,prob=eval(parse(text=prob)))
  y = y111

  t = data.frame(id = l, recruiter.id = "seed", network.size = pop[pop$s%in%l, "netSize"], wave = 0, response = pop[pop$s%in%l, "response"], character = pop[pop$s%in%l, "character"])

  if(!is.null(timeInterval)){ et = rep(Sys.time(), length(l))}

  l0 = nrow(t)
  w = 1
  size1 = ifelse(is.null(size), l0, size)
  w1 = ifelse(is.null(waves), w, waves + 1)


  lli = l
  while (w1 != w || l0 != size1) {
    qlq = numeric()
    for(i in 1:length(lli)){
      bond1 = data[data$respondent%in%lli[i],"connection"]
      bond = bond1[!bond1%in%(t$id)]

      if(length(bond)==0){
        next
      }else{
        c = ifelse(length(bond)<=coupon, length(bond),coupon)
        c1 = sample(x = 0:c, size = 1, prob = 1:(c+1))

        if(!is.null(prop_sam)){
          prop_sam = pop[pop$s%in%bond,"character"]
        }

        if(c1 == 0){
          next
        }else{
          while(T){
            a1 = sample(x = bond, replace = F, size = c1, prob = eval(parse(text=prop_sam)))
            if(sum(t$id%in%a1)==0){
              break
            }
          }

          t = rbind(t, data.frame(id = a1, recruiter.id = as.character(lli[i]), network.size = pop[pop$s%in%a1, "netSize"], wave = w, response = pop[pop$s%in%a1, "response"], character = pop[pop$s%in%a1, "character"]))
          qlq = c(qlq, a1)
        }
      }
      if(showids){ print(paste0(lli[i]))}
    }
    lli = qlq
    if(!is.null(timeInterval)){ et = c(et, rep(Sys.time()+60*60*24*timeInterval*w, length(qlq)))}
    w = w +1
    l0 = nrow(t)

    if(is.null(size)){
      size1 = l0
    }else{
      if(l0>size){ t = t[1:size,]; l0 = nrow(t) }
      size1 = size
    }
    w1 = ifelse(is.null(waves), w, waves + 1)
    #if(!is.null(timeInterval)){ et = rep(Sys.time(), length(l))}
    if(length(qlq)==0){
      print(paste("Haven't attained the required waves or sample size, waves=",w," siz=",l0))
      break()
    }

  }

  if(length(qlq)!=0){
    print(paste("waves=", w, "size=",nrow(t)))
  }
  if(!is.null(timeInterval)){ t$time = et}

  if(!is.null(timeInterval)){
    samp11=RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N, time = "time")
  }else{
    samp11=RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N)
  }
  samp11$seed=RDS::get.seed.id(samp11)

  return(samp11)
}
