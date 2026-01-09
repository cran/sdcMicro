#' KI_applyAnonymization: Automatically apply anonymization strategy using LLM
#'
#' Applies an anonymization plan suggested by an LLM based on the structure and risk profile of an sdcMicro object.
#'
#' @param sdcObj An object of class sdcMicroObj.
#' @param auto If TRUE, automatically applies the suggested strategy.
#' @param generateReport If TRUE, generates internal and external reports.
#' @param k Desired k-anonymity level.
#' @param verbose If TRUE, prints LLM strategy.
#' @param model OpenAI model (default: 'gpt-4').
#' @param api_key Optional API key. Defaults to Sys.getenv("OPENAI_API_KEY").
#' @return Modified sdcMicroObj with anonymization applied (if auto = TRUE).
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   library(sdcMicro)
#'   data(testdata)
#'   sdc <- KI_createSdcObj(
#'     dat = testdata,
#'     policy = "open",
#'     model = "gpt-4",
#'     api_key = Sys.getenv("OPENAI_API_KEY")
#'   )
#'   sdc <- KI_applyAnonymization(
#'     sdcObj = sdc,
#'     k = 3,
#'     auto = TRUE,
#'     generateReport = FALSE,
#'     verbose = TRUE,
#'     model = "gpt-4",
#'     api_key = Sys.getenv("OPENAI_API_KEY")
#'   )
#'   sdc <- KI_applyAnonymization(
#'     sdcObj = sdc,
#'     k = 3,
#'     auto = TRUE,
#'     generateReport = FALSE,
#'     verbose = TRUE,
#'     model = "gpt-4-turbo",
#'     api_key = Sys.getenv("OPENAI_API_KEY")
#'   ) 
#' }
#' }
KI_applyAnonymization <- function(sdcObj, auto = TRUE, generateReport = TRUE, k = 3,
                                  verbose = TRUE, model = "gpt-4", api_key = Sys.getenv("OPENAI_API_KEY")) {
  # Extract variable info
  summary_info <- summarize_sdcObj_structure(sdcObj, k)
  prompt <- build_anonymization_prompt(summary_info, k)
  strategy <- query_llm_anonymization_plan(prompt, model = model, api_key = api_key)
  
  # Patch groupAndRename calls to include all factor levels
  strategy$code <- patch_groupAndRename_calls(sdcObj, strategy$code)
  
  # Replace incorrect globalRecode() usage on factor variables (e.g., "age")
  strategy$code <- patch_globalRecode_on_factors(sdcObj, strategy$code)
  
  strategy$code <- remove_globalRecode_on_numVars(strategy$code, sdcObj@numVars)
  
  # Validate and patch faulty groupAndRename() for 'age'
  age_levels <- levels(sdcObj@manipKeyVars[["age"]])
  age_bins <- c(20, 40, 60)  # your binning
  age_labels <- c("young", "middle", "older", "oldest")
  
  age_code <- build_groupAndRename_code(
    var = "age",
    levels = age_levels,
    bins = age_bins,
    labels = age_labels
  )
  
  strategy$code <- patch_groupAndRename_age(strategy$code, age_code)
  
  # # Fix invalid groupAndRename() for "age" if present
  # if (grepl('groupAndRename\\(.*var *= *"?age"?', strategy$code)) {
  #   age_levels <- levels(get.sdcMicroObj(sdcObj, type = "origData")[["age"]])
  #   bins <- c(20, 40, 60)
  #   labels <- c("young", "middle", "older", "oldest")
  #   
  #   if (length(bins) + 1 != length(labels)) {
  #     stop("Number of age bins does not match number of labels.")
  #   }
  #   
  #   age_code <- build_groupAndRename_code(
  #     var = "age",
  #     levels = age_levels,
  #     bins = bins,
  #     labels = labels
  #   )
  #   message("🔧 Replacing faulty groupAndRename() for 'age' with validated version.")
  #   
  #   # Completely remove all groupAndRename() for 'age', including multi-line fragments
  #   strategy$code <- gsub(
  #     "sdcObj <- groupAndRename\\([^\\)]*var *= *\"?age\"?[^\\)]*\\)[\\s\\\\n,]*", 
  #     "", 
  #     strategy$code
  #   )
  #   
  #   # Inject the correct groupAndRename(age) before localSuppression()
  #   lines <- unlist(strsplit(strategy$code, "\\\\n"))
  #   ls_idx <- grep("localSuppression\\(", lines)
  #   
  #   if (length(ls_idx) > 0) {
  #     lines <- append(lines, values = age_code, after = ls_idx[1] - 1)
  #   } else {
  #     warning("No localSuppression() found, appending age_code at end.")
  #     lines <- c(lines, age_code)
  #   }
  #   
  #   strategy$code <- paste(lines, collapse = "\\n")
  # }
  
  if (verbose) {
    message("LLM anonymization plan:\n")
    
    # Format explanation: break after periods and wrap at 75 characters
    explanation <- gsub("\\.\\s+", ".\n", strategy$explanation)
    explanation <- paste(strwrap(explanation, width = 75), collapse = "\n")
    cat(explanation, "\n\n")
    
    # Format code: replace literal "\\n" with line breaks, then indent
    if (!is.null(strategy$code)) {
      cat("Suggested R code:\n\n")
      code_lines <- unlist(strsplit(strategy$code, "\\\\n"))  # Split by literal \n
      code_lines <- trimws(code_lines)                        # Remove leading/trailing whitespace
      code_lines <- paste0("  ", code_lines)                  # Indent for readability
      cat(paste(code_lines, collapse = "\n"), "\n")
    }
  }
  
  # Validate the LLM-generated code before execution
  if (auto && !is.null(strategy$code)) {
    code_lines <- unlist(strsplit(strategy$code, "\n"))
    # Patch missing levels in groupAndRename
    code_lines <- patch_groupAndRename_missing_levels(code_lines, sdcObj)
    strategy$code <- paste(code_lines, collapse = "\n")
    invalid_vars <- character(0)
    
    # Collect all variables used in the code
    used_vars <- unique(unlist(regmatches(code_lines, gregexpr("(?<=column = ['\"])[^'\"]+", code_lines, perl = TRUE))))
    dat_names <- colnames(get.sdcMicroObj(sdcObj, type = "origData"))
    
    valid_keyVars <- dat_names[get.sdcMicroObj(sdcObj, type = "keyVars")]
    valid_pramVars <- dat_names[get.sdcMicroObj(sdcObj, type = "pramVars")]
    valid_numVars <- dat_names[get.sdcMicroObj(sdcObj, type = "numVars")]
    
    for (var in used_vars) {
      if (!(var %in% c(valid_keyVars, valid_pramVars, valid_numVars))) {
        warning(sprintf("Variable '%s' used in code is not defined in sdcObj as key/pram/num variable.", var))
        invalid_vars <- c(invalid_vars, var)
      }
    }
    
    if (length(invalid_vars) > 0) {
      stop("Aborting due to invalid variable references in LLM code: ", paste(invalid_vars, collapse = ", "))
    }
    
    # # Check if variable passed to groupAndRename is actually a factor
    # check_groupAndRename_validity <- function(sdcObj, code_string) {
    #   browser()
    #   manipVars <- get.sdcMicroObj(sdcObj, type = "manipKeyVars")
    #   matches <- gregexpr("groupAndRename\\(sdcObj, var = \\\"(.*?)\\\".*?before = c\\((.*?)\\)", code_string)
    #   extracted <- regmatches(code_string, matches)[[1]]
    #   
    #   for (expr in extracted) {
    #     var <- sub(".*var = \\\"(.*?)\\\".*", "\\1", expr)
    #     before_raw <- sub(".*before = c\\((.*?)\\).*", "\\1", expr)
    #     before_vals <- gsub("\"", "", strsplit(before_raw, ",\\s*")[[1]])
    #     
    #     if (!var %in% names(manipVars)) {
    #       warning(sprintf("Variable '%s' not found in manipKeyVars!", var))
    #       next
    #     }
    #     
    #     factor_levels <- levels(manipVars[[var]])
    #     if (!all(factor_levels %in% before_vals)) {
    #       missing <- setdiff(factor_levels, before_vals)
    #       stop(sprintf(
    #         "groupAndRename() for '%s' is missing level(s): %s",
    #         var, paste(missing, collapse = ", ")
    #       ))
    #     }
    #   }
    # }
    
    # Check that all before levels are complete
    check_groupAndRename_validity(sdcObj, strategy$code)
    
    # Convert literal "\\n" into line breaks before evaluation
    code_to_run <- clean_llm_code(strategy$code)
    
    # Execute validated code
    parsed_code <- tryCatch(
      parse(text = code_to_run),
      error = function(e) {
        stop("Error parsing LLM-generated code:\n", conditionMessage(e), "\n---\n", code_to_run)
      }
    )
    eval(parsed_code, envir = environment())
  }
  
  if (generateReport) {
    report(sdcObj, filename = "anonymization_internal.html", internal = TRUE)
    report(sdcObj, filename = "anonymization_external.html", internal = FALSE)
  }
  
  return(sdcObj)
}

