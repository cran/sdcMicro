#' Clean LLM-generated R code
#'
#' Replaces literal '\\n' with real line breaks, trims whitespace, and optionally indents.
#'
#' @param code_string Character string returned by LLM (with '\\n' line breaks).
#' @param indent Logical, whether to indent lines (for printing only).
#' @return Cleaned R code as a single string.
clean_llm_code <- function(code_string, indent = FALSE) {
  lines <- unlist(strsplit(gsub("\\\\n", "\n", code_string), "\n"))
  lines <- trimws(lines)
  if (indent) {
    lines <- paste0("  ", lines)
  }
  return(paste(lines, collapse = "\n"))
}

#' Read and summarize a codebook file for LLM prompt
read_codebook_snippet <- function(path) {
  if (!file.exists(path)) return("")
  ext <- tolower(tools::file_ext(path))
  out <- NULL
  
  if (ext %in% c("csv", "tsv")) {
    out <- tryCatch({
      cb <- read.csv(path, stringsAsFactors = FALSE)
      preview <- utils::capture.output(print(head(cb, 10)))
      paste("\nHere is additional codebook information (CSV preview):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  } else if (ext %in% c("yaml", "yml") && requireNamespace("yaml", quietly = TRUE)) {
    out <- tryCatch({
      cb <- yaml::read_yaml(path)
      preview <- utils::capture.output(str(cb, max.level = 2))
      paste("\nHere is additional codebook information (YAML structure):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  } else if (ext == "json" && requireNamespace("jsonlite", quietly = TRUE)) {
    out <- tryCatch({
      cb <- jsonlite::fromJSON(path)
      preview <- utils::capture.output(str(cb, max.level = 2))
      paste("\nHere is additional codebook information (JSON structure):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  } else if (ext %in% c("txt", "text")) {
    out <- tryCatch({
      cb <- readLines(path, n = 50)
      paste("\nHere is additional codebook information (ASCII preview):\n",
            paste(cb, collapse = "\n"))
    }, error = function(e) NULL)
  } else if (ext == "pdf" && requireNamespace("pdftools", quietly = TRUE)) {
    out <- tryCatch({
      cb <- pdftools::pdf_text(path)
      preview <- unlist(strsplit(cb[1:min(2, length(cb))], "\n"))[1:50]
      paste("\nHere is additional codebook information (extracted from PDF):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  if (is.null(out)) return("")
  return(out)
}

#' Summarize structure and k-anonymity violations from an sdcMicroObj
#' @noRd
summarize_sdcObj_structure <- function(sdcObj, k = 3) {
  dat_names <- colnames(get.sdcMicroObj(sdcObj, type = "origData"))
  
  keyVars   <- dat_names[get.sdcMicroObj(sdcObj, type = "keyVars")]
  pramVars  <- dat_names[get.sdcMicroObj(sdcObj, type = "pramVars")]
  numVars   <- dat_names[get.sdcMicroObj(sdcObj, type = "numVars")]
  
  # Compute k-anonymity violations
  k_risk <- report(sdcObj, internal = TRUE, verbose = FALSE)$risk$individual[
    , paste0("k_anon_violations_k", k)
  ]
  
  # Add new list element: keyVarLevels
  keyVarLevels <- lapply(keyVars, function(v) {
    var_data <- factor(get.sdcMicroObj(sdcObj, type = "origData")[[v]])
    levels(var_data)
  })
  names(keyVarLevels) <- keyVars
  
  # Return all info including new keyVarLevels
  structure <- list(
    keyVars       = keyVars,
    pramVars      = pramVars,
    numVars       = numVars,
    k_risk        = k_risk,
    keyVarLevels  = keyVarLevels  # new element
  )
  
  return(structure)
}

#' Build anonymization strategy prompt from summary info
#' Build anonymization strategy prompt from summary info
#'
#' @param summary_info A list containing structure info from summarize_sdcObj_structure()
#' @param k Desired k-anonymity level
#' @return A prompt string for LLM
#' Build anonymization strategy prompt from summary info
#'
#' Constructs a prompt to instruct an LLM to propose a valid anonymization plan
#' using sdcMicro functions with correct syntax and arguments.
#'
#' @param summary_info A list containing `keyVars`, `pramVars`, `numVars`, and `k_risk`.
#' @param k Desired k-anonymity level.
#' @return A character string prompt.
#' @keywords internal
#' Build anonymization strategy prompt from summary info
#'
#' Constructs a prompt to instruct an LLM to propose a valid anonymization plan
#' using sdcMicro functions with correct syntax and arguments.
#'
#' @param summary_info A list containing `keyVars`, `pramVars`, `numVars`, and `k_risk`.
#' @param k Desired k-anonymity level.
#' @return A character string prompt.
#' @keywords internal
build_anonymization_prompt <- function(summary_info, k) {
  generate_prompt_example_age <- function(levels_age) {
    numeric_levels <- sort(as.numeric(as.character(levels_age)))
    bin_labels <- cut(numeric_levels,
                      breaks = c(-Inf, 19, 39, 59, Inf),
                      labels = c("young", "middle", "older", "oldest"),
                      right = TRUE)
    
    before <- sprintf('\\"%s\\"', numeric_levels)
    after  <- sprintf('\\"%s\\"', bin_labels)
    
    before_str <- paste(before, collapse = ",")
    after_str  <- paste(after, collapse = ",")
    
    example_line <- sprintf(
      'sdcObj <- groupAndRename(sdcObj, var = \\"age\\", before = c(%s), after = c(%s))',
      before_str, after_str
    )
    
    return(example_line)
  }
  age_levels <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13",
                  "14","15","16","17","18","19","20","21","22","23","24","25",
                  "26","27","28","29","30","31","32","33","34","35","36","37",
                  "38","39","40","41","42","43","44","45","46","47","48","49",
                  "50","51","52","53","54","55","56","57","58","59","60","61",
                  "62","63","64","65","66","67","68","69","70","71","72","73",
                  "74","75","76","77","78","79","80","82","83","84","85","88",
                  "90","95")
  
  example_age_code <- generate_prompt_example_age(age_levels)
  txt <- paste(
    "You are an expert in data anonymization using the R package sdcMicro.",
    "",
    "The sdcMicro object has the following structure. You must only use variable",
    "names listed below in the anonymization plan. Do not invent variable names.",
    sprintf("- Key variables: %s", paste(summary_info$keyVars, collapse = ", ")),
    sprintf("- PRAM variables: %s", 
            if (length(summary_info$pramVars) > 0) 
              paste(summary_info$pramVars, collapse = ", ") 
            else 
              "(none defined - do not use PRAM)"),
    sprintf("- Numerical variables: %s", paste(summary_info$numVars, collapse = ", ")),
    sprintf("- Number of k-anonymity violations (k = %d): %d", k, summary_info$k_risk),
    "",
    "For your convenience, here are the exact levels of all key variables:",
    paste(sprintf("- %s: c(%s)", names(summary_info$keyVarLevels),
                  sapply(summary_info$keyVarLevels, function(x) paste(sprintf('"%s"', x), collapse = ", "))),
          collapse = "\n"),
    "",
    "These must match exactly in any call to groupAndRename().",
    "",
    "- IMPORTANT: Always inspect and include **all levels** of a categorical variable when using groupAndRename().",
    "- Example: if the variable `roof` has levels 1, 2, 4, 5, 6, 9, and you group only levels 2, 4, 5, 6, 9 into '1', then you must still include level '1' and map it to itself. Failing to do so causes an error.",
    "- correct: sdcObj <- groupAndRename(sdcObj, var = \"roof\", before = c(\"1\",\"2\",\"4\",\"5\",\"6\",\"9\"), after = c(\"1\",\"1\",\"1\",\"1\",\"1\",\"1\"))",
    "- incorrect: sdcObj <- groupAndRename(sdcObj, var = \"roof\", before = c(\"2\",\"4\",\"5\",\"6\",\"9\"), after = c(\"1\",\"1\",\"1\",\"1\",\"1\")) - This will fail because level \"1\" is not listed",
    "",
    "Your task is to create **valid and executable R code** for a sensible anonymization plan using only functions from the sdcMicro package.",
    "",
    "Use the following methods:",
    "- Recode categorical key variables using `groupAndRename()`. **Never use `globalRecode()` on factor or character variables!**",
    "- Use `globalRecode()` only for numeric variables.",
    "- Applying `globalRecode()` to categorical variables will cause an error: it expects numeric input, not character or factor.",
    "- Example of incorrect usage (this will crash): `globalRecode(sdcObj, column = \"roof\", breaks = c(\"poor\", \"good\"), ...)`",
    "- Correct usage: `groupAndRename(sdcObj, var = \"roof\", before = c(\"1\", \"2\", \"3\", \"4\"), after = c(\"1\", \"1\", \"3\", \"4\"))`",
    "- This combines levels 1 and 2, and leaves levels 3 and 4 unchanged.",
    "- ! All levels of the factor **must be listed** in `before`. If any level is missing, the code will fail.",
    "- ! If you want to leave levels unchanged, include them with the same value in `after`.",
    "- ! Do NOT use `groupAndRename()` on variables that are numeric or integer.",
    "- For example, the variables `income`, `expend`, and `savings` are continuous numerical variables and are listed in `numVars`. Do not apply `globalRecode()` to them. These variables should be anonymized using methods such as `microaggregation()` or `addNoise()`. Only use `globalRecode()` on variables that are categorical and included in `keyVars`. If in doubt, assume these numerical variables are not eligible for grouping. For example, instead of writing:\n  sdcObj <- globalRecode(sdcObj, column = \"income\", breaks = c(0, 1000, 10000), labels = c(\"low\", \"medium\", \"high\"))\nyou should use:\n  sdcObj <- microaggregation(sdcObj, numVars = c(\"income\"), method = \"mdav\")",
    "- The function argument to pass the variables for microaggregation is NOT numVars, but it is called variables",
    "- Example: `sdcObj <- microaggregation(sdcObj, variables = c(\"income\"), method = \"mdav\")`",
    "- Example: `sdcObj <- globalRecode(sdcObj, column = \"income\", breaks = c(0, 1000, 10000, 30000, Inf), labels = c(\"low\", \"middle\", \"high\", \"very high\"))`",
    "- You may also apply `microaggregation()` or `addNoise()` to such variables if they are declared as numerical.",
    "- If you apply microaggregation, use the default method mdav unless the data is larger than 20000 rows, then use method onedims",
    "- `localSuppression()` has no function argument called keyVars, it automatically takes all keyVars from the sdcMicroObj",
    "IMPORTANT RULES for groupAndRename():",
    "- Use `groupAndRename()` **only** for categorical variables (key variables that are factors).",
    "- The argument `before` must contain **all existing levels** of the variable.",
    "- The argument `after` must have the same length as `before`.",
    "- Do **not omit** any level, or the function will fail.",
    "- Generally, `groupAndRename()` must not be applied on all keyVars. Rather apply it to a few categories of selected keyVars",
    "- Try to group meaningful categories and concentrate on categories with low frequencies to be combined with another reasonable category",
    "",
    "- IMPORTANT: Always inspect and include **all levels** of a categorical variable when using groupAndRename().",
    "- Example: if the variable `roof` has levels 1, 2, 4, 5, 6, 9, and you group only levels 2, 4, 5, 6, 9 into '1', then you must still include level '1' and map it to itself. Failing to do so causes an error.",
    "- correct: sdcObj <- groupAndRename(sdcObj, var = \"roof\", before = c(\"1\",\"2\",\"4\",\"5\",\"6\",\"9\"), after = c(\"1\",\"1\",\"1\",\"1\",\"1\",\"1\"))",
    "- incorrect: sdcObj <- groupAndRename(sdcObj, var = \"roof\", before = c(\"2\",\"4\",\"5\",\"6\",\"9\"), after = c(\"1\",\"1\",\"1\",\"1\",\"1\")) - This will fail because level \"1\" is not listed",
    "IMPORTANT RULES for globalRecode():",
    "- Use `globalRecode()` **only** for numeric variables like `income`, `savings`, `expend`.",
    "- Do **NOT** use `globalRecode()` on the variable `age` if it is listed as a key variable (in this case, it is treated as a factor).",
    "- If `age` is a key variable (i.e., a factor), you must use `groupAndRename()` instead of `globalRecode()`.",
    "- Example (correct): `groupAndRename(sdcObj, var = \"age\", before = c(\"18\", \"19\", ..., \"40\"), after = c(\"young\", ..., \"middle\"))`",
    "- Example (incorrect): `globalRecode(sdcObj, column = \"age\", ...)` - This will crash if `age` is a factor.",
    "- Never apply `globalRecode()` to factor variables like `roof`, `walls`, `sex`, `age`, etc.",
    "- Do not apply globalRecode() to numerical variables (numVars) such as `income`, `expend`, or `savings`. These variables should either be protected using microaggregation() or addNoise() if they are continuous",
    "",
    "! SPECIAL WARNING:",
    "- If the variable `age` is listed as a key variable, it is a factor. Do NOT apply `globalRecode()` to it.",
    "- Use `groupAndRename()` instead. Otherwise, your code will fail.",
    "- Example when age is in keyVars (incorrect): `sdcObj <- globalRecode(sdcObj, column = \\\"age\\\", breaks = c(0, 20, 40, 60, Inf), labels = c(\\\"young\\\", \\\"middle\\\", \\\"older\\\", \\\"oldest\\\"))`",
    
    "- Example when age is in keyVars (correct): `sdcObj <- groupAndRename(sdcObj, var = \\\"age\\\", before = c(\\\"0\\\",\\\"1\\\",\\\"2\\\",\\\"3\\\",\\\"4\\\",\\\"5\\\",\\\"6\\\",\\\"7\\\",\\\"8\\\",\\\"9\\\",\\\"10\\\",\\\"11\\\",\\\"12\\\",\\\"13\\\",\\\"14\\\",\\\"15\\\",\\\"16\\\",\\\"17\\\",\\\"18\\\",\\\"19\\\",\\\"20\\\",\\\"21\\\",\\\"22\\\",\\\"23\\\",\\\"24\\\",\\\"25\\\",\\\"26\\\",\\\"27\\\",\\\"28\\\",\\\"29\\\",\\\"30\\\",\\\"31\\\",\\\"32\\\",\\\"33\\\",\\\"34\\\",\\\"35\\\",\\\"36\\\",\\\"37\\\",\\\"38\\\",\\\"39\\\",\\\"40\\\",\\\"41\\\",\\\"42\\\",\\\"43\\\",\\\"44\\\",\\\"45\\\",\\\"46\\\",\\\"47\\\",\\\"48\\\",\\\"49\\\",\\\"50\\\",\\\"51\\\",\\\"52\\\",\\\"53\\\",\\\"54\\\",\\\"55\\\",\\\"56\\\",\\\"57\\\",\\\"58\\\",\\\"59\\\",\\\"60\\\",\\\"61\\\",\\\"62\\\",\\\"63\\\",\\\"64\\\",\\\"65\\\",\\\"66\\\",\\\"67\\\",\\\"68\\\",\\\"69\\\",\\\"70\\\",\\\"71\\\",\\\"72\\\",\\\"73\\\",\\\"74\\\",\\\"75\\\",\\\"76\\\",\\\"77\\\",\\\"78\\\",\\\"79\\\",\\\"80\\\",\\\"82\\\",\\\"83\\\",\\\"84\\\",\\\"85\\\",\\\"88\\\",\\\"90\\\",\\\"95\\\"), after = c(rep(\\\"young\\\", 20),rep(\\\"middle\\\", 20),rep(\\\"older\\\", 20),rep(\\\"oldest\\\", 28)))`", 
    sprintf("- Example when age is a factor (e.g. in keyVars): `%s`", example_age_code),
    "",
    "Allowed functions and use cases:",
    "- `groupAndRename()` - for recoding categorical key variables (all levels must be covered)",
    "- `globalRecode()` - for binning numeric variables",
    "- `localSuppression()` - for k-anonymity of all key variables",
    "",
    "- Use `localSuppression()` on key variables.",
    sprintf("- %s PRAM using `PRAM()`.", 
            if (length(summary_info$pramVars) > 0) 
              "Apply" else "Do not apply (no PRAM variables defined, skip this step)"),
    "- Use `microaggregation()` or `addNoise()` on numerical variables.",
    "",
    "**RULES you must follow:**",
    "1. Only use the variable names listed above - no made-up names like `var1`, `col1`, etc.",
    "2. Do NOT use column numbers (e.g. column = 1).",
    "3. For categorical key variables, only use `groupAndRename()`.",
    "4. For numeric variables, you may use `globalRecode()`.",
    "5. Do NOT include any markdown formatting, backticks, triple backticks, or comments in the code.",
    "6. When using `groupAndRename()`, include **all levels** in the `before` vector.",
    "   Do not omit any factor levels. Even unchanged levels must be mapped.",
    "",
    "**Output Format:**",
    "Return a valid JSON object with two fields:",
    "  - 'explanation': plain-text summary (1-3 sentences).",
    "  - 'code': a single-line string of executable R code operating on `sdcObj`, use `\\n` to indicate line breaks.",
    "",
    "Example JSON:",
    "{",
    '  "explanation": "Applied recoding and suppression.",',
    '  "code": "sdcObj <- groupAndRename(sdcObj, var = \\"roof\\", before = c(\\"1\\",\\"2\\",\\"3\\"), after = c(\\"1\\",\\"1\\",\\"3\\"))\\n',
    'sdcObj <- localSuppression(sdcObj, keyVars = c(\\"roof\\",\\"sex\\"), k = 3)"',
    "}",
    "",
    "Ensure your code runs **without errors** in R with sdcMicro."
  )
  txt <- paste(txt,
               "",
               "For your convenience, here are the levels of all key variables:",
               paste(sprintf("- %s: c(%s)", 
                             names(summary_info$keyVarLevels),
                             sapply(summary_info$keyVarLevels, function(x) paste(sprintf('"%s"', x), collapse = ", "))),
                     collapse = "\n"),
               "",
               "Use these levels exactly in your groupAndRename() calls - no level should be omitted.",
               sep = "\n"
  )
  return(txt)
}

#' Query LLM to suggest anonymization code
#' @noRd
query_llm_anonymization_plan <- function(prompt, model = "gpt-4", api_key = NULL) {
if (is.null(api_key) || api_key == "") api_key <- Sys.getenv("OPENAI_API_KEY")
if (api_key == "") {
  api_key <- readline(prompt = "Please enter your OpenAI API key: ")
  if (api_key == "") stop("No API key provided.")
  Sys.setenv(OPENAI_API_KEY = api_key)
}


res <- httr::POST(
  url = "https://api.openai.com/v1/chat/completions",
  httr::add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  ),
  body = jsonlite::toJSON(list(
    model = model,
    messages = list(
      list(role = "system", content = "You are an anonymization expert."),
      list(role = "user", content = prompt)
    ),
    temperature = 0
  ), auto_unbox = TRUE)
)

res_content <- httr::content(res, as = "parsed")

if (!is.null(res_content$error)) {
  msg <- res_content$error$message
  if (grepl("quota", tolower(msg))) {
    stop(paste0(
      "OpenAI API quota exceeded or unavailable.\n",
      "Please visit https://platform.openai.com/account/usage to check your usage.\n",
      "Error message: ", msg
    ))
  } else {
    stop("OpenAI API error: ", msg)
  }
}

content_raw <- httr::content(res, as = "parsed")

# Defensive check
if (is.null(content_raw$choices) || length(content_raw$choices) == 0) {
  stop("LLM returned an empty or malformed response:\n", jsonlite::toJSON(content_raw, pretty = TRUE))
}

full_content <- content_raw$choices[[1]]$message$content

# Defensive check
if (is.na(full_content) || full_content == "NA") {
  stop("LLM returned an invalid response: NA")
}

full_content <- gsub("^```json\\s*|\\s*```$", "", full_content)

parsed <- tryCatch({
  jsonlite::fromJSON(full_content)
}, error = function(e) {
  # Try to extract code manually if JSON fails
  fallback_code <- regmatches(full_content, regexpr("(?s)```R(.*?)```", full_content, perl = TRUE))
  fallback_code <- gsub("^```R\\s*|\\s*```$", "", fallback_code)
  if (length(fallback_code) > 0) {
    warning("! LLM returned raw code block. Falling back to manual parsing.")
    return(list(explanation = "Fallback: explanation missing", code = fallback_code))
  }
  stop("Failed to parse LLM response: ", e$message, "\nRaw response:\n", full_content)
})

return(parsed)
}
# query_llm_anonymization_plan <- function(prompt, model = "gpt-4", api_key = NULL) {
#   if (is.null(api_key) || api_key == "") api_key <- Sys.getenv("OPENAI_API_KEY")
#   if (api_key == "") {
#     api_key <- readline(prompt = "Please enter your OpenAI API key: ")
#     if (api_key == "") stop("No API key provided.")
#     Sys.setenv(OPENAI_API_KEY = api_key)
#   }
#   res <- httr::POST(
#     url = "https://api.openai.com/v1/chat/completions",
#     httr::add_headers(
#       Authorization = paste("Bearer", api_key),
#       `Content-Type` = "application/json"
#     ),
#     body = jsonlite::toJSON(list(
#       model = model,
#       messages = list(
#         list(role = "system", content = "You are an anonymization expert."),
#         list(role = "user", content = prompt)
#       ),
#       temperature = 0
#     ), auto_unbox = TRUE)
#   )
#   
#   full_content <- httr::content(res, as = "parsed")$choices[[1]]$message$content
#   full_content <- gsub("^```json\\s*|\\s*```$", "", full_content)
#   parsed <- tryCatch(jsonlite::fromJSON(full_content), error = function(e) {
#     stop("Failed to parse LLM response: ", e$message, "\nRaw response:\n", full_content)
#   })
#   return(parsed)
# }


# KI-based anonymization support patch for sdcMicro

#' Automatically generate groupAndRename() code for a factor variable like age
#' when globalRecode() is suggested incorrectly
# build_groupAndRename_code <- function(var, levels, bins, labels) {
# # Remove NAs from levels and ensure it's character
# levels <- na.omit(as.character(levels))
# 
# # Try coercing to numeric, suppress warnings
# numeric_levels <- suppressWarnings(as.numeric(levels))
# 
# # Check if coercion failed (non-numeric detected)
# if (any(is.na(numeric_levels))) {
#   message(sprintf(
#     "! Skipping groupAndRename for '%s': non-numeric levels detected: %s",
#     var,
#     paste(unique(levels[is.na(numeric_levels)]), collapse = ", ")
#   ))
#   return(NULL)
# }
# 
# # Bin numeric levels
# bins_cut <- cut(numeric_levels, breaks = bins, labels = labels, right = FALSE, include.lowest = TRUE)
# grouped <- as.character(bins_cut)
# 
# # Format before/after vectors
# before_str <- paste0('"', levels, '"', collapse = ", ")
# after_str  <- paste0('"', grouped, '"', collapse = ", ")
# 
# # Return the R code as a string
# return(sprintf(
#   'sdcObj <- groupAndRename(sdcObj, var = "%s", before = c(%s), after = c(%s))',
#   var, before_str, after_str
# ))
# }
# build_groupAndRename_code <- function(var, levels, bins, labels) {
#   # Skip if already grouped (assumes string labels have been applied)
#   if (any(grepl("^[a-zA-Z]+$", levels))) {
#     message(sprintf("! Skipping groupAndRename for '%s': already grouped into labels like %s", var, paste(head(levels, 3), collapse = ", ")))
#     return(NULL)
#   }
# 
#   stopifnot(length(bins) + 1 == length(labels))
# 
#   # Convert levels to numeric for binning
#   levels <- trimws(levels)  # Remove leading/trailing whitespace
#   numeric_levels <- suppressWarnings(as.numeric(levels))
#   if (any(is.na(numeric_levels))) {
#     invalid <- levels[is.na(numeric_levels)]
#     stop(sprintf(
#       "Non-numeric factor levels detected in variable '%s': %s",
#       var, paste(shQuote(invalid), collapse = ", ")
#     ))
#   }
# 
#   # Assign labels to bins
#   full_breaks <- c(-Inf, bins, Inf)
#   if (length(full_breaks) - 1 != length(labels)) {
#     stop("Number of cut intervals does not match number of labels.")
#   }
#   breaks <- cut(numeric_levels, breaks = full_breaks, labels = labels,
#                 right = FALSE, include.lowest = TRUE)
# 
#   after <- as.character(breaks)
# 
#   before_str <- sprintf('c(%s)', paste0('"', levels, '"', collapse = ", "))
#   after_str <- sprintf('c(%s)', paste0('"', after, '"', collapse = ", "))
# 
#   code_line <- sprintf('sdcObj <- groupAndRename(sdcObj, var = "%s", before = %s, after = %s)',
#                        var, before_str, after_str)
#   return(code_line)
# }
#' Group and rename factor variable levels into bins
#' @noRd
build_groupAndRename_code <- function(var, levels, bins, labels) {
  # Remove NAs and trim
  levels <- trimws(na.omit(as.character(levels)))
  
  # Early exit if already grouped
  if (any(grepl("^[a-zA-Z]+$", levels))) {
    message(sprintf("! Skipping groupAndRename for '%s': already grouped into labels like %s", var, paste(head(levels, 3), collapse = ", ")))
    return(NULL)
  }
  
  # Check bin-label count
  if (length(bins) + 1 != length(labels)) {
    stop("Number of labels must be exactly one more than the number of bins.")
  }
  
  # Coerce to numeric with error if any non-numeric remain
  numeric_levels <- suppressWarnings(as.numeric(levels))
  if (any(is.na(numeric_levels))) {
    invalid <- levels[is.na(numeric_levels)]
    stop(sprintf(
      "Non-numeric factor levels detected in variable '%s': %s",
      var, paste(shQuote(invalid), collapse = ", ")
    ))
  }
  
  # Cut into intervals: ensure full range
  full_breaks <- c(-Inf, bins, Inf)
  grouped <- cut(numeric_levels, breaks = full_breaks, labels = labels, right = FALSE, include.lowest = TRUE)
  grouped <- as.character(grouped)
  
  # Format vectors as R code strings
  before_str <- sprintf('c(%s)', paste0('"', levels, '"', collapse = ", "))
  after_str  <- sprintf('c(%s)', paste0('"', grouped, '"', collapse = ", "))
  
  # Return the complete code line
  sprintf('sdcObj <- groupAndRename(sdcObj, var = "%s", before = %s, after = %s)', var, before_str, after_str)
}

#' Validate and patch incorrect globalRecode calls on factor keyVars
#' @noRd
patch_groupAndRename_calls <- function(sdcObj, code_string) {
  # Parse all groupAndRename calls
  lines <- unlist(strsplit(code_string, "\\n"))
  patched_lines <- sapply(lines, function(line) {
    if (grepl("groupAndRename\\(", line)) {
      var <- sub('.*var\\s*=\\s*"([^"]+)".*', '\\1', line)
      levels <- levels(get.sdcMicroObj(sdcObj, type = "origData")[[var]])
      before_str <- paste0('c("', paste(levels, collapse = '","'), '")')
      after_str <- paste0('c("', paste(rep("1", length(levels)), collapse = '","'), '")') # dummy example
      sub("before = c\\([^\\)]*\\)", paste0("before = ", before_str),
          sub("after = c\\([^\\)]*\\)", paste0("after = ", after_str), line))
    } else {
      line
    }
  })
  paste(patched_lines, collapse = "\\n")
}

#' Validate and patch incorrect globalRecode calls on factor keyVar age
#' @noRd
patch_globalRecode_on_factors <- function(sdcObj, code) {
  code_lines <- unlist(strsplit(code, "\n"))
  var_types <- sapply(get.sdcMicroObj(sdcObj, type = "origData")[get.sdcMicroObj(sdcObj, type = "keyVars")], class)
  
  new_lines <- lapply(code_lines, function(line) {
    for (var in names(var_types[var_types %in% c("factor", "character")])) {
      pattern <- paste0('globalRecode\\(.*column *= *"?', var, '"?')
      if (grepl(pattern, line)) {
        message(sprintf("! Replacing invalid globalRecode() on factor variable '%s' with groupAndRename()", var))
        # Replace with a no-op comment or actual valid code if you want
        return(paste0("# Removed: ", line))
      }
    }
    return(line)
  })
  
  paste(unlist(new_lines), collapse = "\n")
}

#' Patch faulty groupAndRename() for 'age' in LLM-generated strategy code
#'
#' @param code Character string containing the anonymization plan code
#' @param age_code Validated replacement code for groupAndRename(var = "age", ...)
#' @return Cleaned character string with faulty code removed and valid code inserted
patch_groupAndRename_age <- function(code, age_code) {
  lines <- unlist(strsplit(code, "\\\\n"))
  
  # Identify and remove broken or multi-line groupAndRename() blocks for 'age'
  remove_idx <- c()
  in_age_block <- FALSE
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    if (grepl("groupAndRename\\(.*var *= *\"?age\"?", line)) {
      in_age_block <- TRUE
      remove_idx <- c(remove_idx, i)
    } else if (in_age_block) {
      remove_idx <- c(remove_idx, i)
      if (grepl("\\)", line)) {
        in_age_block <- FALSE
      }
    }
  }
  
  if (length(remove_idx) > 0) {
    lines <- lines[-remove_idx]
  }
  
  # Insert valid groupAndRename(age) code just before localSuppression()
  ls_idx <- grep("localSuppression\\(", lines)
  if (length(ls_idx) > 0) {
    lines <- append(lines, values = age_code, after = ls_idx[1] - 1)
  } else {
    warning("No localSuppression() found in code; appending age_code at the end.")
    lines <- c(lines, age_code)
  }
  
  return(paste(lines, collapse = "\\n"))
}

remove_globalRecode_on_numVars <- function(code, numVars) {
  for (var in numVars) {
    code <- gsub(
      pattern = paste0("sdcObj <- globalRecode\\(sdcObj, column = \"", var, "\"[^\n]*\n?"),
      replacement = "",
      x = code
    )
  }
  return(code)
}


check_openai_quota <- function(api_key = Sys.getenv("OPENAI_API_KEY")) {
  res <- httr::GET(
    url = "https://api.openai.com/v1/models",
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )
  
  if (httr::status_code(res) != 200) {
    msg <- httr::content(res, as = "text", encoding = "UTF-8")
    stop("OpenAI quota or access error:\n", msg)
  }
}


patch_groupAndRename_missing_levels <- function(code_lines, sdcObj) {
  patched_lines <- lapply(code_lines, function(line) {
    if (grepl("groupAndRename\\(", line)) {
      # Extract the variable name
      var_match <- regmatches(line, regexpr('var *= *"[^"]+"', line))
      var_name <- gsub('var *= *"|"', "", var_match)
      
      # Get all existing levels
      var_levels <- levels(sdcObj@manipKeyVars[[var_name]])
      if (is.null(var_levels)) return(line)  # not a factor? skip
      
      # Extract 'before' list
      before_match <- regmatches(line, regexpr('before *= *c\\([^)]+\\)', line))
      if (length(before_match) == 0) return(line)  # malformed? skip
      before_values <- gsub('before *= *c\\(|\\)|"', "", before_match)
      before_values <- trimws(unlist(strsplit(before_values, ",")))
      
      # Add any missing levels and extend after values
      missing_levels <- setdiff(var_levels, before_values)
      if (length(missing_levels) == 0) return(line)
      
      message(sprintf("! Patching groupAndRename() for '%s': adding missing levels: %s",
                      var_name, paste(missing_levels, collapse = ", ")))
      
      after_match <- regmatches(line, regexpr('after *= *c\\([^)]+\\)', line))
      after_values <- gsub('after *= *c\\(|\\)|"', "", after_match)
      after_values <- trimws(unlist(strsplit(after_values, ",")))
      
      # Add identity mapping for missing
      before_all <- c(before_values, missing_levels)
      after_all  <- c(after_values, missing_levels)
      
      # Format new line
      new_line <- sprintf(
        'sdcObj <- groupAndRename(sdcObj, var = "%s", before = c(%s), after = c(%s))',
        var_name,
        paste(sprintf('"%s"', before_all), collapse = ", "),
        paste(sprintf('"%s"', after_all), collapse = ", ")
      )
      return(new_line)
    } else {
      return(line)
    }
  })
  return(unlist(patched_lines))
}


check_groupAndRename_validity <- function(sdcObj, code_string) {
  keyVars <- get.sdcMicroObj(sdcObj, type = "keyVars")
  origData <- get.sdcMicroObj(sdcObj, type = "origData")
  var_names <- colnames(origData)[keyVars]
  
  # Extract groupAndRename() lines
  matches <- gregexpr('groupAndRename\\(sdcObj, var = \\"(.*?)\\".*?before = c\\((.*?)\\)', code_string)
  extracted <- regmatches(code_string, matches)[[1]]
  
  for (expr in extracted) {
    var <- sub('.*var = \\"(.*?)\\".*', '\\1', expr)
    before_raw <- sub('.*before = c\\((.*?)\\).*', '\\1', expr)
    before_vals <- gsub('"', '', strsplit(before_raw, ',\\s*')[[1]])
    
    if (!var %in% var_names) {
      warning(sprintf("Variable '%s' not found in original keyVars!", var))
      next
    }
    
    # Get the full set of levels from original data
    factor_levels <- levels(origData[[var]])
    
    if (!all(factor_levels %in% before_vals)) {
      missing <- setdiff(factor_levels, before_vals)
      stop(sprintf(
        "groupAndRename() for '%s' is missing level(s): %s",
        var, paste(missing, collapse = ", ")
      ))
    }
  }
}