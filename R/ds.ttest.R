
#' @export
ds.ttest <- function(formula, datasources) {

  # The variables names
  var1 <- unlist(strsplit(formula, split='~'))[1]
  var2 <- unlist(strsplit(formula, split='~'))[2]

  # check if the variables in the formula are defined in all the studies, if not defined a message is thrown and the process stops
  #defined <- isDefined(datasources, var1)
  #defined <- isDefined(datasources, var2)



  # Check that the variables in the formula are of the right type: 'numeric'/'integer' for the outcome and 'factor' for the covariate
  # typ1 <- checkClass(datasources, var1)
  # if(typ1 != "numeric" & typ1 != "integer"){
  #   stop(paste0(" Required ", var1, " as numeric vector!"), call.=FALSE)
  # }
  # typ2 <- checkClass(datasources, var2)
  # if(typ2 != "factor"){
  #   stop(paste0(" Required ", var2, " must be a factor vector!"), call.=FALSE)
  # }else{
  #
  #   # check levels (2 required)
  #   cally <- paste0("levels(", var2, ")")
  #   levels_all <- datashield.aggregate(datasources, as.symbol(cally))
  #   classes <- unique(unlist(levels_all))
  #   if(length(classes) != 2){
  #     stop(paste0(" ", var2 , " should have only two categories!"), call.=FALSE)
  #   }
  # }

  formula <- as.formula(formula)
  type <- 'gaussian'

  numstudies <- length(datasources)

  # start beta values
  beta.vect.next <- c(0,0)
  beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")

  # Iterations need to be counted. Start off with the count at 0
  # and increment by 1 at each new iteration
  iteration.count <- 0

  cally1 <- call('dsBase::glmDS1', formula, type, beta.vect=beta.vect.temp, NULL)

  study.summary <- datashield.aggregate(datasources, cally1)
  num.par.glm <- study.summary[[1]][[1]][[2]]
  return(num.par.glm)
}
