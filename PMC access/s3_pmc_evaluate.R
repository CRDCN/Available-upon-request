## File to get the OA subset data

library(aws.s3)
library(fun)
library(parallel)
library(pbapply)
library(readr)
library(stringr)
library(tidypmc)
library(tidyverse)
library(writexl)
library(xml2)

#0. Setup - user to modify the following lines:
## Step 1: setup a multi-threaded workload - requires a minimum 4-core CPU
## if you are working on a low-power computer, do not comment out this part of the workload, simply change the cluster size to single-core
pcl<-makeCluster(detectCores()-1)

## Step 2: create an output file 'xlsx' format and place the path at line 428 to save the output data

## Step 3: Modify algorithm parameters if necessary
# List of academic institutions: line 94
# Exact match phrases: line 134
# Near match parameters: line 137-151
# Proximity match distance & breaks: line 154

#File processes - modify at your own risk
#1. Define algorithms and parameters

# CLEAN TEXT FUNCTION - removes all punctuation
clean_text <- function(text_vector) {
  text_vector <- tolower(text_vector)
  # Remove common punctuation and compress spaces
  text_vector <- stringr::str_replace_all(text_vector, "[^[:alnum:][:space:].]", " ")
  text_vector <- stringr::str_squish(text_vector)
  return(text_vector)
}

## POLISH TEXT FUNCTION - removes all but sentence breaks
polish_text<- function(text_vector){
  text_vector<- tolower(text_vector)
  text_vector<- stringr::str_replace_all(text_vector, "[[:punct:]&&[^!?.]]","")
  text_vector<-stringr::str_squish(text_vector)
  return(text_vector)
}

# ALGORITHM FUNCTION - searches for exact matches from MATCH STRINGS (see line #128) then looks for proximity matches (see line #131) 
keyphrase_search <- function(raw_text){
  input_text<-polish_text(raw_text)
  output<-list(flag="NOT_FOUND", phrase=NA_character_)
  
  # Primary scan for exact phrase matches
  for (phrase in phrase_matches){
    if(grepl(phrase, input_text, fixed=FALSE, ignore.case=TRUE)){
      output$flag<-"RESTRICTED/UPON_REQUEST"
      output$phrase<-phrase
      output$message<-"NA"
      return(output)
    }
  }
  # Secondary scan for proximity between request (or requested/requesting),data,code etc...
  for (option in proximity_matches){
    term_pattern <- paste0("\\b", option$term, "\\b")
    pattern_forward <- paste0("\\brequest\\w{0,3}\\b",separator, term_pattern)
    pattern_backward <- paste0(term_pattern, separator, "\\brequest\\w{0,3}\\b")
  ##Search for matches backwards & forwards: NB code ignores backward matches if both are found  
    m_f <- regexpr(pattern_forward, input_text, ignore.case = TRUE, perl = TRUE)
    m_b <- regexpr(pattern_backward, input_text, ignore.case = TRUE, perl = TRUE)
    
    if (m_f[1] != -1L) {
      matched <- substr(input_text, m_f[1], m_f[1] + attr(m_f, "match.length")[1] - 1L)
      message("FORWARD match for ", option$term, ": [", matched, "]")
      output$flag  <- "RESTRICTED/UPON_REQUEST"
      output$phrase <- paste0("Proximity", option$output)
      output$message <- paste0("FWD match for ", option$term, ": [", matched, "]")
      return(output)
    }
    
    if (m_b[1] != -1L) {
      matched <- substr(input_text, m_b[1], m_b[1] + attr(m_b, "match.length")[1] - 1L)
      message("BACKWARD match for ", option$term, ": [", matched, "]")
      output$flag  <- "RESTRICTED/UPON_REQUEST"
      output$phrase <- paste0("Proximity", option$output)
      output$message <- paste0("BWD match for ", option$term, ": [", matched, "]")
      return(output)
    }
  }
return(output)
}

## ALGORITHM PARAMETERS:

## LIST OF CANADIAN ACADEMIC INSTITUTIONS
institution_list <- c(
  "memorial university of newfoundland", "university of prince edward island",
  "acadia university", "acadia divinity college", "atlantic school of theology",
  "cape breton university", "dalhousie university", "university of king's college",
  "mount saint vincent university", "nova scotia college of art and design university",
  "université sainte-anne", "st. francis xavier university", "saint mary's university",
  "mount allison university", "university of new brunswick", "université de moncton",
  "st. thomas university", "bishop's university", "mcgill university",
  "université de montréal", "polytechnique montréal", "école des hautes études commerciales",
  "université laval", "université de sherbrooke", "concordia university",
  "université du québec", "brock university", "concordia lutheran theological seminary",
  "carleton university", "university of guelph", "lakehead university",
  "laurentian university of sudbury", "université laurentienne de sudbury",
  "mcmaster university", "nipissing university", "university of ottawa",
  "saint-paul university", "université saint-paul", "queen's university",
  "ryerson university", "university of toronto", "st. augustine's seminary",
  "university of st. michael's college", "university of trinity college",
  "victoria university", "toronto metropolitan university", "knox college", "wycliffe college", "regis college",
  "trent university", "university of waterloo", "st. jerome's university",
  "renison university college", "conrad grebel university college",
  "university of western ontario", "huron university college", "king's college",
  "wilfred laurier university", "university of windsor", "york university",
  "ontario college of art and design", "university of ontario institute of technology",
  "algoma university college", "university of sudbury", "université de hearst",
  "huntington university", "thorneloe university", "brandon university",
  "canadian mennonite university", "university of manitoba", "université de saint-boniface",
  "university of winnipeg", "university of regina", "campion college",
  "luther college", "university of saskatchewan", "college of emmanuel and st chad",
  "lutheran theological seminary", "st andrew's college", "st thomas more college",
  "horizon college & seminary", "university of alberta", "athabasca university",
  "university of calgary", "burman university", "concordia university of edmonton",
  "university of lethbridge", "the king's university college", "ambrose university",
  "grant macewan university", "mount royal university", "university of british columbia",
  "university of northern british columbia", "royal roads university",
  "simon fraser university", "university of victoria", "thompson rivers university",
  "capilano university", "vancouver island university", "emily carr university",
  "kwantlen polytechnic university", "university of the fraser valley",
  "yukon university"
)
## MATCH STRINGS (FOR EXACT MATCHES)
phrase_matches <- c("Available upon request", "available on request", "Data can be obtained by contacting the author", "reasonable request", "upon request", "The raw data supporting the conclusions of this article will be made available by the authors without undue reservation.")

## PROXIMITY STRINGS (FOR PROXIMITY MATCHES)
proximity_matches <- list(
  # Data (no wildcards to avoid matching "database" which will false flag scoping reviews)
  list(term = "data", output = "DATA (proximity match)"),
  list(term = "datasets",output="DATASETS (proximity match)"),
  list(term = "dataset", output="DATASET (proximity match)"),
  # Code
  list(term = "code", output = "SOFTWARE (proximity match)"),
  list(term = "software", output = "SOFTWARE (proximity match)"),
  # Sample(s)/Specimen(s)
  list(term = "sample\\w{0,1}", output = "SAMPLE/SPECIMEN (proximity match)"),
  list(term = "specimen\\w{0,1}", output = "SAMPLE/SPECIMEN (proximity match)"),
  # Scripts/Supplementary
  list(term = "script\\w{0,1}", output = "SCRIPT/SUPPLEMENTARY (proximity match)"),
  list(term = "supplementary", output = "SCRIPT/SUPPLEMENTARY (proximity match)")
)
##Parameter for proximity matches (what appears between list term and request*)
##Currently a 50 word distance, disallowing sentence breaks
separator <- "(?:\\s+[^\\s.!?\\n]+){0,49}\\s+"

# 2. Get cleaned institution list for pattern matching (done once per execution)
institution_list_cleaned <- clean_text(institution_list) #redundant, but if issues exist in the list of institutions, this fixes them with essentially zero cost
search_pattern <- paste0("\\b(", paste(institution_list_cleaned, collapse = "|"), ")\\b")

start_time <- Sys.time()

# 3. Set up PMC S3 system parameters and obtain a list of PMC IDs

bucket<- "pmc-oa-opendata"

pmc_get<- function(prefix, max=Inf){
  objs<- get_bucket(bucket=bucket, prefix=prefix, max=max)
  keys<- vapply(objs, function(x) x[["Key"]], character(1))
  keys[grepl("\\.xml",keys)]
}

keys_commercial<- pmc_get("oa_comm/xml/all/")
keys_noncommercial<- pmc_get("oa_noncomm/xml/all/")
full_keys<- c(keys_commercial, keys_noncommercial)


## 4 Function to obtain article XML
xml_proc <- function(obj_key, institution_list){
pmc_id <- sub("\\.xml$", "", basename(obj_key))
  # 4.0.b fetch raw XML from S3

xml_dat <- NULL
  xml_dat <- tryCatch(
      {
        aws.s3::s3read_using(
          FUN    = xml2::read_xml,
          bucket = bucket,
          object = obj_key
        )
      },
      error = function(e) NULL
    )
    
  if (is.null(xml_dat)){
    warning(paste("No XML for", pmc_id))
    return(NULL)
  }
  
  # 4.1 read in the text from xml_dat then confirm year is part of sample.
  pmc_id_flag <- xml_find_first(xml_dat, "//article-id[@pub-id-type='pmc']")
  pmc_id_xml  <- xml_text(pmc_id_flag)
  
  if (is.na(pmc_id_xml) || pmc_id_xml == "") {
    warning(paste("PMC_ID not found for", pmc_id))
    return(NULL)
  }
  pub_year_nodes <- xml_find_all(
    xml_dat,
    "//*[local-name()='article-meta']//*[local-name()='pub-date']"
  )
  
  years <- suppressWarnings(as.integer(xml_text(xml_find_all(pub_year_nodes, ".//*[local-name()='year']"))))
  years <- years[!is.na(years)]
  
  pub_year <- if (length(years)) max(years) else NA_integer_
  
  if (is.na(pub_year) || pub_year < 2016) return(NULL)

  
  # Initialize variables for the results
  aff_raw_text        <- NA_character_
  author_email        <- NA_character_
  email_fallback_flag <- FALSE 
  data_availability_flag  <- "NO_KEYWORD_MATCH"
  matched_data_phrase     <- NA_character_  
  matched_data_message     <- NA_character_  
  
  # 4.2 corresponding author with matching affiliation
  corr_contrib_nodes <- xml_find_all(
    xml_dat,
    "//*[local-name()='contrib' and @contrib-type='author' and @corresp='yes']"
  )
  
  # Fallback: any author contrib that has an xref to a corresp element
  if (length(corr_contrib_nodes) == 0) {
    corr_contrib_nodes <- xml_find_all(
      xml_dat,
      "//*[local-name()='contrib' and @contrib-type='author'][.//xref[@ref-type='corresp']]"
    )
  }
  match_found <- FALSE
  for (contrib_node in corr_contrib_nodes) {
    
    aff_xref_node <- xml_find_first(contrib_node, "./*[local-name()='xref' and @ref-type='aff']")
    if (is.null(aff_xref_node)) next
    
    aff_id  <- xml_attr(aff_xref_node, "rid")
    aff_node <- xml_find_first(xml_dat, paste0("//*[local-name()='aff' and @id='", aff_id, "']"))
    if (is.null(aff_node)) next
    
    # 4.3 institution match
    current_aff_raw_text <- xml_text(aff_node)
    aff_cleaned_text     <- clean_text(current_aff_raw_text)
    
    if (grepl(search_pattern, aff_cleaned_text, fixed = FALSE)) {
      
      match_found  <- TRUE
      aff_raw_text <- current_aff_raw_text
# 4.4 corresponding author email

# Look for an xref to a corresp element within this contributor
      corresp_xref <- xml_find_first(
        contrib_node,
        ".//*[local-name()='xref' and @ref-type='corresp']"
      )
      
      author_email <- NA_character_
      
      if (!is.null(corresp_xref)) {
        corresp_id_vec <- xml_attr(corresp_xref, "rid")
        corresp_id_vec <- corresp_id_vec[!is.na(corresp_id_vec) & corresp_id_vec != ""]
        corresp_id     <- if (length(corresp_id_vec) > 0) corresp_id_vec[1] else NA_character_
        
        if (!is.na(corresp_id) && nzchar(corresp_id)) {
          corresp_node <- xml_find_first(
            xml_dat,
            paste0("//*[local-name()='corresp' and @id='", corresp_id, "']")
          )
          if (!is.null(corresp_node)) {
            email_node <- xml_find_first(corresp_node, ".//*[local-name()='email']")
            if (!is.null(email_node)) {
              author_email <- xml_text(email_node)
            }
          }
        }
      }
    #if there's no corresp within the node, look in the address path
      if (is.na(author_email) || author_email == "") {
        addr_email_node <- xml_find_first(
          contrib_node,
          ".//*[local-name()='address']//*[local-name()='email']"
        )
        if (!is.null(addr_email_node)) {
          author_email <- xml_text(addr_email_node)
        }
      }
      
      # Fallback if we still didn't get an email
      if (is.na(author_email) || author_email == "") {
        fallback_email_node <- xml_find_first(
          xml_dat,
          "//p[local-name()='corresp']//*[local-name()='email'] | //*[local-name()='corresp']//*[local-name()='email']"
        )
        if (!is.null(fallback_email_node)) {
          author_email        <- xml_text(fallback_email_node)
          email_fallback_flag <- TRUE
        }
      }
      break
    }
  }
  
  if (!match_found) return(NULL)
  
  # 4.5 data availability & full article search
  
  # 1) Prefer 'data availability' / 'accessibility' sections
  das_nodes <- xml_find_all(
    xml_dat,
    "//*[local-name()='sec' and (@sec-type='data-availability' or @sec-type='data-accessibility')]"
  )
  
  # 2) If none, look for sections whose title looks like a DAS (case-insensitive)
  if (length(das_nodes) == 0) {
    das_nodes <- xml_find_all(
      xml_dat,
      "//*[local-name()='sec'][translate(normalize-space(./*[local-name()='title']),
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')
      = 'data availability'
      or
      contains(translate(normalize-space(./*[local-name()='title']),
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),
      'data availability statement')
      or
      contains(translate(normalize-space(./*[local-name()='title']),
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),
      'data accessibility')
    ]"
    )
  }
  
  # collapse all candidate DAS sections’ text, if any
  if (length(das_nodes) > 0) {
    das_text_raw  <- paste(xml_text(das_nodes), collapse = "\n\n")
    search_result <- keyphrase_search(das_text_raw)
    
    if (search_result$flag != "NOT_FOUND") {
      data_availability_flag  <- "RESTRICTED/UPON_REQUEST IN DAS"
      matched_data_phrase     <- search_result$phrase
      matched_data_message <- search_result$message
    }
  }
  
  # 3) Fallback: search full article if still no match
  if (data_availability_flag == "NO_KEYWORD_MATCH") {
    article_body_node <- xml_find_first(xml_dat, "//*[local-name()='article']")
    if (!is.null(article_body_node)) {
      article_text_raw <- xml_text(article_body_node)
      search_result    <- keyphrase_search(article_text_raw)
      
      if (search_result$flag != "NOT_FOUND") {
        data_availability_flag <- "RESTRICTED/UPON_REQUEST IN ARTICLE BODY"
        matched_data_phrase    <- search_result$phrase
        matched_data_message <- search_result$message
      } 
      else {
        data_availability_flag <- "NOT_FOUND_IN_PHRASES"
      }
    } else {
      data_availability_flag <- "COULD NOT PROCESS ARTICLE : UNKNOWN STRUCTURE"
    }
  }
  
  c(
    PMC_ID                  = pmc_id_xml,
    Matched_Affiliation     = aff_raw_text,
    Data_Availability_Flag  = data_availability_flag,
    Data_Availability_Match = matched_data_phrase,
    author_email            = author_email,
    email_fallback_flag     = email_fallback_flag,
    matched_data_message    = matched_data_message
  )

}

##invoke the cluster and apply the functions to the PMC database
clusterExport(
  pcl, varlist=c("xml_proc","institution_list","bucket","clean_text","polish_text","keyphrase_search","phrase_matches","proximity_matches","separator","search_pattern"),
  envir=environment()
)
clusterEvalQ(pcl,{
  library(xml2)
  library(stringr)
  library(aws.s3)
})

result_list <- pblapply(
  X   = full_keys,
  FUN = xml_proc,
  institution_list = institution_list,
  cl=pcl
)

matched_data <- result_list[!sapply(result_list, is.null)]

if (length(matched_data) > 0) {
  final_df <- as.data.frame(do.call(rbind, matched_data), stringsAsFactors = FALSE)
  print(head(final_df))
} else {
  print("No matching articles found.")
}

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print("Total Elapsed Time:")
print(elapsed_time)

##5. Post-data acquisition processing
# drop rows with UK/NZ/AU academic emails
bad_email_pattern <- "\\.ac\\.(uk|nz|au)$"
bad_email <- grepl(bad_email_pattern, final_df$author_email, ignore.case = TRUE)

# drop rows where affiliation contains "New York"
bad_affil <- grepl("New York", final_df$Matched_Affiliation, ignore.case = TRUE)

final_df <- final_df[!(bad_email | bad_affil), ] 

write_xlsx(final_df, path = "user to set path")

##Shutdown command
shutdown(wait=1000)