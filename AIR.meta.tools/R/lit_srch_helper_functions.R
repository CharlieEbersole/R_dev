#Helper functions for conducting the literature search including:
#
# 1. Using litsearchr to extract key terms
# 2. Writing the search string
# 3. Reading data and deduplicating
# 4. Database-specific cleaning (e.g., ERIC, PsycINFO)
# 5. Creating search strings that omit or add one term at a time
# 6. Exporting to Abstrackr

##############################################
## 1. USING LITSEARCHR TO EXTRACT KEY TERMS ##
##############################################

#gets the document-feature matrix from an input of documents (character vector)
#and keywords (character vector)
create_dfm_keyword = function(documents, keywords) {
  
  #convert to lower case
  documents = tolower(documents)
  keywords = tolower(keywords)
  
  #separate by document, count number of times the keywords match (creates list of vector)
  counts_by_doc = documents %>% lapply(stringr::str_count, keywords) 
  
  #make list of vectors into a matrix
  dfm = t(matrix(unlist(counts_by_doc), nrow = length(keywords)))
  colnames(dfm) = keywords
  dfm
}

#wrapper function that takes in a character vector and returns a data 
#frame of the automatically extracted keyword, along with relevant summary
#statistics like number of documents each keyword appears in (keywords), and the 
#documentary feature matrix (dfm)
extract_keywords = function(documents, ...) {
  
  #convert NA's to empty strings
  documents = replace_na(as.character(documents), "")
  
  #extract phrases two words or longer (default setting)
  keywords = extract_terms(text = documents, method = "fakerake", ...)
  
  #get the DFM matrix
  dfm = create_dfm_keyword(documents, keywords)
  
  #get statistics
  n_times = colSums(dfm)
  n_docs = colSums(dfm > 0)
  
  #get node importance and cutoff from co-occurence matrix
  network = create_network(search_dfm = (dfm>0)*1, min_studies = 0, min_occ = 0)
  node_imp = igraph::strength(network)
  cutoff = find_cutoff(network, method = "cumulative")
  auto_select = 1*(node_imp >= cutoff)
  
  #make a wordle
  #network %>% reduce_graph(cutoff_strength = cutoff) %>% get_keywords(makewordle = TRUE)
  
  #return results
  list(data = data.frame(text = keywords, n_docs, n_times, node_imp, auto_select) %>% arrange(-n_docs),
       network=network, cutoff=cutoff, dfm=dfm)
}

##################################
## 2. WRITING THE SEARCH STRING ##
##################################

#loads the keywords and makes into a formatted list (OLDER CODE)
read_keywords = function(filename) {
  read.csv(filename) %>% 
    mutate_all(as.character) %>% na_if("") %>% 
    as.list() %>% lapply(na.omit) %>% lapply(as.vector)
}

#write grouped keywords as a list of combined OR strings (NEWER CODE)
format_keywords = function(data, ...) {
  data %>% 
    filter(include == 1) %>%
    plyr::dlply("group", pull, "keyword") %>%
    lapply(write_group, ...)
}

#helper function to write the string for one group of keywords
write_group = function(group_words, quotes = TRUE, brackets = FALSE) {
  if (!quotes) srch_string = group_words %>% paste0(collapse = " OR ")
  if (quotes)  srch_string = paste0('"',group_words,'"') %>% paste0(collapse = " OR ")
  if (brackets)  srch_string = paste0('{',group_words,'}') %>% paste0(collapse = " OR ")
  glue::glue("{srch_string}")
}

#allows to search for specific fields (e.g., title, abstract, keywords)
specific_fields = function(search_string, fields = c("TI", "AB", "KW"), 
                           psyc=FALSE, scopus = FALSE) {
  
  #use broadest possible fields for the age string...
  if (substr(search_string, 2, nchar(ignore_if)+1) == ignore_if) {
    fields = ""
    if (psyc) fields = c("", "AG")
    if (scopus) fields = c("TITLE-ABS-KEY")
  }
  
  #enclose in parentheses and combine with OR's
  result = paste0(fields, "(", search_string, ")") %>% paste0(collapse = " OR ")
  glue::glue("{result}")
}

#combine all search fields into one field
combine_field = function(groups, field = "") {
  paste0(field, "(", unlist(groups), ")") %>% paste0(collapse = " AND ") %>% glue()
}

#combines and copies to the clipboard using clipr package
combine_copy = function(x) combine_field(x) %>% write_clip()


#######################################
## 3. READING DATA AND DEDUPLICATING ##
#######################################

#function that merges duplicates filling in missing values 
#from other rows whenever possible, using an order of sources
merge_duplicates = function(data, dupVar, srcOrder = NULL) {
  
  #ignore merging where the duplicate variable is missing
  ignoreData = data[ is.na(data[dupVar]), ]
  data = data[ !is.na(data[dupVar]), ]
  
  #sort by the inputed order of sources in which to gather information
  if (!is.null(srcOrder)) data = data[order(factor(data$source, srcOrder)), ]
  
  #taken from @Jon Harmon's answer on Stack Overflow
  #https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row
  combine_rows <- function(df) coalesce(!!! as.list(df))
  
  #group by the duplicates variables and combine rows
  data %>% group_by_at(dupVar) %>% summarise_all(combine_rows) %>% rbind(ignoreData) %>% data.frame()
}

#deduplicates a dataset
dedup = function(data, srcOrder = c("old", "psyc", "wos", "asp", "erc", "scop", "eric", "es", 
                                    "soc", "gwatch", "proq", "ebsco", "scop_f", "scop_b", "gs_f"))
{
  
  #combine the two but add an ID specific to the "old" dataset, which we don't want to deduplicate
  message("Step 0: Formatting data...")
  step0 = data %>% 
    mutate_all(as.character) %>%
    
    #create a simplified title and extract first author's last name
    mutate(title_simp = title %>% stri_trans_general("latin-ascii") %>% str_replace_all("-|:", " ") %>% str_remove_all("[^[:alnum:][:space:]]") %>% str_squish() %>% str_to_lower() ,
           first_author = author %>% stri_trans_general("latin-ascii") %>% str_to_lower() %>% str_extract("[:graph:]+(?=(,|$|;))") %>% str_remove("\\.\\.\\."))
  
  #step1: first dedup by DOI
  message("Step 1: Deduplicating by DOI...")
  step1 = step0 %>% merge_duplicates("doi", srcOrder)
  
  #step2: dedup on similar title (but require exact matching first author's last name)
  message("Step 2: Deduplicating by title...")
  step2 = step1 %>% mutate(match = find_duplicates(., match_variable = "title_simp", group_variables = c("old_label", "first_author"), 
                                                   remove_punctuation = TRUE)) %>%
    merge_duplicates("match", srcOrder)
  
  #step 3: return result
  message("Step 3: Finalizing formatting...")
  
  #prefer the ERIC file (direct PDF link) to others
  if (is.null(step2$fulltext_url)) step2$fulltext_url = NA
  if (!is.null(step2$eric_fulltext_url)) step2$fulltext_url = coalesce(step2$eric_fulltext_url, step2$fulltext_url)
  
  #other formatting
  step2 %>%
    select(-first_author, -title_simp, -match) %>%
    mutate(label = 1:nrow(step2))
}

#specify names of variables to keep - just the list of those in the cleaned file now
keep_vars = c("doi", "label", "type", "author", "year", "title", "title_secondary", 
              "journal", "volume", "issue", "pages", "abstract", "keywords", 
              "email_first", "email_all", "issn", "publisher", "pub_location", 
              "booktitle", "editor", "database", "database_url", "database_id", 
              "eric_fulltext_url", "fulltext_url", "apa_citation_auto", "source", 
              "asp_id", "ebsco_id", "erc_id", "eric_id", "es_id", "gwatch_id", 
              "proq_id", "psyc_id", "scop_id", "soc_id", "wos_id", "gs_refid", 
              "gs_id", "gs_pub_url", "gs_times_cited", "scop_f_id", "scop_f_refid", 
              "scop_b_id", "scop_b_refid", "url_html", "cite")

#helper function for noting the data source of the search file
#also does some light pre-processing
add_source = function(data, stem, full_text_ids = NULL) {
  
  #rename fields if needed
  if ("n1" %in% names(data)) data = data %>% rename(notes = n1)
  if ("url" %in% names(data)) data = data %>% rename(database_url = url)
  
  #database-specific formatting
  if (stem == "eric") data = clean_eric(data)
  if (stem == "asp") data = clean_asp(data)
  if (stem == "soc") data = clean_soc(data)
  if (stem == "psyc") data = clean_psyc(data)
  if (stem == "wos") data = clean_wos(data)
  if (stem == "es") data = clean_es(data)
  if (stem == "scop") data = clean_scop(data)
  
  #use the database URL as a full-text URL if the IDs match
  if (!is.null(full_text_ids)) {
    has_ft = data$database_id %in% full_text_ids
    data$fulltext_url = ifelse(has_ft, data$database_url, NA)
  }
  
  #get emails
  d_email = data %>% mutate_all(as.character)
  if (stem == "eric") d_email$notes = NULL
  data$email_first = d_email %>% mutate_all(extract_email_first) %>% do.call(coalesce, .)
  data$email_all   = d_email %>% mutate_all(extract_email_all)   %>% do.call(coalesce, .)
  
  #nitpicky preference - I prefer separating multiple entries using "; " rather than " and "
  vars = intersect(c("author", "editor", "issn", "keywords"), names(data))
  data = data %>% mutate_at(vars, str_replace_all, " and ", "; ")
  
  #keep only the most important variables
  keep_these = intersect(names(data), keep_vars)
  exclude_these = setdiff(names(data), keep_vars)
  data = data[keep_these]
  message(glue("Excluding these variables: {paste0(exclude_these, collapse=', ')}"))
  
  #other data formatting
  if ("keywords" %in% names(data)) data[glue("{stem}_keywords")] = data$keywords
  data$keywords = NULL
  data$source = stem
  
  #keep the database-specific id
  data[glue("{stem}_id")] = data$database_id
  
  #return data
  data 
}

#helper functions for extracting emails
extract_email_all = function(string) {
  string %>% 
    str_extract_all("[:graph:]+@[^;[:space:]]+") %>% 
    sapply(paste0, collapse="; ") %>% 
    na_if("") %>% na_if("NA")
}
extract_email_first = function(string) {
  string %>% str_extract("[:graph:]+@[^;[:space:]]+")
}

#helper function for combining keywords across literature databases
combine_keywords = function(data) {
  key_vars = names(data) %>% str_subset("_keywords")
  data = data %>% mutate_at(key_vars, replace_na, "")
  
  #combine keywords across databases, split the keywords, extract the unique ones, then recombine
  recombine_keywords = function(x) unique(x) %>% setdiff("") %>% sort() %>% paste0(collapse = "; ")
  data$keywords = data[key_vars] %>% 
    apply(1, paste0, collapse = "; ") %>% 
    str_to_lower() %>% 
    str_split("; ") %>% 
    sapply(recombine_keywords) 
  
  #format data
  data[key_vars] = NULL
  data
}

#############################################
## 4. DATABASE-SPECIFIC CLEANING FUNCTIONS ##
#############################################

#helper function for cleaning Education Source RIS search files from EBSCOhost
clean_es = function(data) {
  data %>% mutate(
    year = year %>% str_extract("[0-9]+"),
    database_id = notes %>% str_extract("(?<=Accession Number: )[A-Z0-9-]+"),
    database = "Education Source",
    journal = j1 %>% str_remove("\\.$"),
    doi = l3)
}

#helper function for cleaning Academic Search Premier RIS search files from EBSCOhost
clean_asp = function(data) {
  data %>% mutate(
    database_id = notes %>% str_extract("(?<=Accession Number: )[A-Z0-9-]+"),
    database = "Academic Search Premier",
    doi = l3)
}

#helper function for cleaning SocINDEX RIS search files from EBSCOhost
clean_soc = function(data) {
  data %>% mutate(
    year = year %>% str_extract("[0-9]+"),
    database_id = notes %>% str_extract("(?<=Accession Number: )[A-Z0-9-]+"),
    database = "SocINDEX with Full Text",
    doi = l3)
}

#helper function for cleaning ERIC RIS search files from EBSCOhost
#adds a full text URL if available
clean_eric = function(data) {
  data$database_id = data$notes %>% str_extract("E[A-Z][0-9]+")
  data$database = "Education Resources Information Library (ERIC)"
  data$doi = data$database_url %>% str_extract("(?<=doi.org/)[:graph:]+")
  data$database_url = word(data$database_url)
  avail = data$notes %>% str_detect("Level of Availability: Available")
  url = ifelse(avail, paste0("https://files.eric.ed.gov/fulltext/", data$database_id, ".pdf"), NA)
  data$eric_fulltext_url = as.character(url)
  data
}

#helper function for cleaning PsycINFO RIS files from EBSCOhost
clean_psyc = function(data) {
  data = data %>% 
    mutate(journal = coalesce(journal_secondary, journal),
           journal_secondary = NULL,
           database = "PsycINFO",
           database_url = word(database_url))
  if ("ID" %in% names(data)) data = data %>% rename(database_id = ID)
  if ("institution" %in% names(data)) data = data %>% rename(pub_location = institution)
  data
}

#helper function for cleaning Scopus RIS files 
clean_scop = function(data) {
  data %>%
    rename(pub_location = institution,
           title_secondary = ST) %>%
    mutate(database_id = database_url,
           pages = coalesce(pages, c7),
           database = "Scopus")
}


#helper function for cleaning Web of Science Bibtex files 
clean_wos = function(data) {
  data %>%
    rename(issue = number) %>% 
    mutate(type = recode(type, "article" = "JOUR", "inproceedings" = "CONF"),
           doi = word(doi),
           journal = coalesce(journal, note) %>% str_to_title(),
           database_id = unique_id %>% str_remove("ISI:"),
           database = "Web of Science Core Collection",
           type = type %>% recode("incollection" = "CHAP"),
           pages = coalesce(pages, article_number))
}

####################################################################
## 5. CREATING SEARCH STRINGS THAT OMIT OR ADD ONE TERM AT A TIME ##
####################################################################

#function for dropping one word at a time from search string

#INPUT: keywords = dataframe of keywords including columns "keyword", "group", "include", and "notes"
#OUTPUT: outputs dropped_terms.csv, with a column saying which term is dropped
#        and the search string needed to examine differences between including 
#        vs. excluding each term in multiple databases

subtract_one = function(keywords){
  
  keywords = subset(keywords, include == "1")
  WebOfScienceAll <- rep(1, length(keywords$keyword))
  EBSCOAll <- rep(1, length(keywords$keyword))
  SCOPUSAll <- rep(1, length(keywords$keyword))
  ProquestAll <- rep(1, length(keywords$keyword))
  PsychInfoAll <- rep(1, length(keywords$keyword))
  
  WebOfScienceDrop1 <- rep(1, length(keywords$keyword))
  EBSCODrop1 <- rep(1, length(keywords$keyword))
  SCOPUSDrop1 <- rep(1, length(keywords$keyword))
  ProquestDrop1 <- rep(1, length(keywords$keyword))
  PsychInfoDrop1 <- rep(1, length(keywords$keyword))
  
  WebOfScience <- rep(1, length(keywords$keyword))
  EBSCO <- rep(1, length(keywords$keyword))
  SCOPUS <- rep(1, length(keywords$keyword))
  Proquest <- rep(1, length(keywords$keyword))
  PsychInfo <- rep(1, length(keywords$keyword))
  
  #Loop to remove one term at a time
  for(i in 1:length(keywords$keyword)){
    keywords_i = keywords
    dropone = keywords[-i,]
    grps = format_keywords(keywords)
    grps2 = format_keywords(dropone)
    
    #Web of Science
    WebOfScienceAll[i] = combine_field(grps)
    WebOfScienceDrop1[i] = combine_field(grps2)
    WebOfScience = glue("({WebOfScienceAll}) NOT ({WebOfScienceDrop1})")
    
    #EBSCO
    EBSCOAll[i] = grps %>% lapply(specific_fields) %>% combine_field()
    EBSCODrop1[i] = grps2 %>% lapply(specific_fields) %>% combine_field()
    EBSCO = glue("({EBSCOAll}) NOT ({EBSCODrop1})")
    
    #SCOPUS
    SCOPUSAll[i] = grps %>% lapply(specific_fields, fields=c("TITLE-ABS", "AUTHKEY"), scopus=TRUE) %>% combine_field()
    SCOPUSDrop1[i] = grps2 %>% lapply(specific_fields, fields=c("TITLE-ABS", "AUTHKEY"), scopus=TRUE) %>% combine_field()
    SCOPUS = glue("({SCOPUSAll}) NOT ({SCOPUSDrop1})")
    
    #Proquest
    ProquestAll[i] = grps %>% lapply(specific_fields, fields=c("NOFT")) %>% combine_field()
    ProquestDrop1[i] = grps2 %>% lapply(specific_fields, fields=c("NOFT")) %>% combine_field()
    Proquest = glue("({ProquestAll}) NOT ({ProquestDrop1})")
    
    #PsychInfo
    PsychInfoAll[i] = grps %>% lapply(specific_fields, fields=c("TI", "AB", "KW", "SU"), psyc=TRUE) %>% 
      combine_field()
    PsychInfoDrop1[i] = grps2 %>% lapply(specific_fields, fields=c("TI", "AB", "KW", "SU"), psyc=TRUE) %>% 
      combine_field()
    PsychInfo = glue("({PsychInfoAll}) NOT ({PsychInfoDrop1})")
  }
  
  DroppedWord <- keywords$keyword
  export<-as.data.frame(cbind(DroppedWord,WebOfScience,EBSCO,SCOPUS,Proquest,PsychInfo))
  write.csv(export,"dropped_terms.csv")
}

#function for adding one word at a time from search string

#INPUTS: keywords = dataframe of keywords including columns 
#        "keyword", "group", "include", and "notes"
#        new_keywords = dataframe of keywords to be added including columns 
#        "keyword", "group", "include", and "notes"

#OUTPUT: outputs added_terms.csv, with a column saying which term is added
#        and the search string needed to examine differences between including 
#        vs. excluding each term in multiple databases


add_one <- function(keywords, new_keywords){
  
  new_keywords = subset(new_keywords, include == "1")
  WebOfScienceBase <- rep(1, length(new_keywords$keyword))
  EBSCOBase <- rep(1, length(new_keywords$keyword))
  SCOPUSBase <- rep(1, length(new_keywords$keyword))
  ProquestBase <- rep(1, length(new_keywords$keyword))
  PsychInfoBase <- rep(1, length(new_keywords$keyword))
  
  WebOfScienceAdd1 <- rep(1, length(new_keywords$keyword))
  EBSCOAdd1 <- rep(1, length(new_keywords$keyword))
  SCOPUSAdd1 <- rep(1, length(new_keywords$keyword))
  ProquestAdd1 <- rep(1, length(new_keywords$keyword))
  PsychInfoAdd1 <- rep(1, length(new_keywords$keyword))
  
  WebOfScience <- rep(1, length(new_keywords$keyword))
  EBSCO <- rep(1, length(new_keywords$keyword))
  SCOPUS <- rep(1, length(new_keywords$keyword))
  Proquest <- rep(1, length(new_keywords$keyword))
  PsychInfo <- rep(1, length(new_keywords$keyword))
  
  for(i in 1:length(new_keywords$keyword)){
    keywords_i = keywords
    new_keywords_i = new_keywords
    new_row<-new_keywords[i,]
    addone = rbind(keywords,new_row)
    grps = format_keywords(keywords)
    grps2 = format_keywords(addone)
    
    #Web of Science
    WebOfScienceBase[i] = combine_field(grps)
    WebOfScienceAdd1[i] = combine_field(grps2)
    WebOfScience = glue("({WebOfScienceAdd1}) NOT ({WebOfScienceBase})")
    
    #EBSCO
    EBSCOBase[i] = grps %>% lapply(specific_fields) %>% combine_field()
    EBSCOAdd1[i] = grps2 %>% lapply(specific_fields) %>% combine_field()
    EBSCO = glue("({EBSCOAdd1}) NOT ({EBSCOBase})")
    
    #SCOPUS
    SCOPUSBase[i] = grps %>% lapply(specific_fields, fields=c("TITLE-ABS", "AUTHKEY"), scopus=TRUE) %>% combine_field()
    SCOPUSAdd1[i] = grps2 %>% lapply(specific_fields, fields=c("TITLE-ABS", "AUTHKEY"), scopus=TRUE) %>% combine_field()
    SCOPUS = glue("({SCOPUSAdd1}) NOT ({SCOPUSBase})")
    
    #Proquest
    ProquestBase[i] = grps %>% lapply(specific_fields, fields=c("NOFT")) %>% combine_field()
    ProquestAdd1[i] = grps2 %>% lapply(specific_fields, fields=c("NOFT")) %>% combine_field()
    Proquest = glue("({ProquestAdd1}) NOT ({ProquestBase})")
    
    #PsychInfo
    PsychInfoBase[i] = grps %>% lapply(specific_fields, fields=c("TI", "AB", "KW", "SU"), psyc=TRUE) %>% 
      combine_field()
    PsychInfoAdd1[i] = grps2 %>% lapply(specific_fields, fields=c("TI", "AB", "KW", "SU"), psyc=TRUE) %>% 
      combine_field()
    PsychInfo = glue("({PsychInfoAdd1}) NOT ({PsychInfoBase})")
  }
  
  AddedWord<-new_keywords$keyword
  export<-as.data.frame(cbind(AddedWord,
                              WebOfScience,
                              EBSCO,
                              SCOPUS,
                              Proquest,
                              PsychInfo))
  
  write.csv(export,"added_terms.csv")}

###############################
## 6. EXPORTING TO ABSTRACKR ##
###############################

#turn the search terms into regex expressions
get_expr = function(x) {
  x %>% 
    paste0("(?<![:graph:])", .) %>% 
    str_replace_all("\\*", "[:alpha:]*") %>% 
    paste0(collapse = "(?![:alpha:])|") %>% 
    regex(ignore_case=TRUE)
}


#function for adding colors
add_color = function(x, keywords, colors) {
  x = as.character(x)
  exprs = keywords %>% lapply(get_expr)
  for (i in 1:length(colors)) {
    add_span = function(x) glue("<span style='color:{colors[i]}'><strong>{x}</strong></span>")
    find_expr = exprs[[names(colors)[i]]]
    x = x %>% str_replace_all(find_expr, add_span)
  }
  x
}

#helper function for pre-export formatting to Abstrackr
export_format = function(data, highlight_terms, colors) {
  
  #add the HTML for the URLs
  if (is.null(data$url_html)) data$url_html = ""
  data$fulltext_url = ifelse(is.na(data$fulltext_url) & !is.na(data$doi), 
                             glue("https://sci-hub.se/{data$doi}"), 
                             data$fulltext_url)
  data$url_html = ifelse(is.na(data$fulltext_url), "", glue("<h1><a href='{data$fulltext_url}' target='_blank'>Link to Full Text</a></h1>"))
  data$url_html = glue('{data$url_html}<h1><a href="https://scholar.google.com/scholar?hl=en&q={data$title}" target="_blank">Search Google Scholar</a></h1>')
  data$url_html = glue('{data$url_html}<h1><a href="https://www.google.com/search?q={data$title}" target="_blank">Search Google</a></h1>')
  data$url_html = ifelse(is.na(data$database_url), data$url_html, glue('{data$url_html}<h1><a href="{data$database_url}" target="_blank">Literature Database Page</a></h1>'))
  
  #other formatting
  data %>% mutate_all(as.character) %>%
    mutate(
      keywords = add_color(keywords, highlight_terms, colors) %>% paste0(url_html),
      authors = author %>% str_remove_all(","),
      authors = glue("{authors} (Year: {year})"),
      abstract = add_color(abstract, highlight_terms, colors),
      journal = journal %>% replace_na(""),
      journal = glue("{journal} (Database: {database})")) %>%
    rename(id = label) %>%
    select(id, title, abstract, keywords, authors, journal) 
}
