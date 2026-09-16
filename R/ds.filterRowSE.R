
#' @export
ds.filterRowSE <- function(se, threshold = 1, nsample = 1, percent = NULL, newobj_name = NULL, datasources = NULL){

  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }

  if(!is.null(nsample) & !is.null(percent)){

    stop(sprintf("Choose one filtering criteria."))
  }

  # Check if required new object name is missing
  if(is.null(newobj_name)){

    stop(sprintf("Missing new object name."))
  }

  # Check if required threshold is missing
  if(is.null(threshold)){

    stop(sprintf("Missing filtering threshold."))
  }

  # Check classes on server
  class_se <- dsBaseClient:::checkClass(datasources, se)

  # Check se object class on all the servers
  if(!all(class_se %in% "SummarizedExperiment")) {
    stop(sprintf("SE object not a SummarizedExperiment on all the servers."))
  }

  # Check on SE object dimensions
  dims <- dsBaseClient::ds.dim(x = se, datasources = datasources)
  if (is.list(dims)) {
    nrows <- vapply(dims, function(v) as.numeric(v[1]), numeric(1))
    ncols <- vapply(dims, function(v) as.numeric(v[2]), numeric(1))
  } else {
    nrows <- dims[1]
    ncols <- dims[2]
  }
  if (any(nrows == 0) || any(ncols == 0)) {
    stop(sprintf("SE object has zero rows or zero columns on at least one server."))
  }


  filtering_threshold  <- if (is.null(threshold)) "NULL" else threshold
  filtering_nsample  <- if (is.null(nsample))  "NULL" else nsample
  filtering_percentage  <- if (is.null(percent)) "NULL" else percent

  function_call <- paste0("filterRowSEDS(", se, ", ", filtering_threshold,", ", filtering_nsample, ", ", filtering_percentage,")")

  DSI::datashield.assign.expr(datasources, newobj_name, function_call)

}
