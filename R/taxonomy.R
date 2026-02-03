#' Taxonomic Rank Determination Function
#'
#' @description `get_taxon_rank()` generates a new column in your selected data set called taxonRank that will show the taxonomic rank of the most specific name in the given scientific name column. This is a required column in the Simple Darwin Core rule set and guidelines. This function will be useful in creating and auto populating a required Simple Darwin Core field.
#'
#' @details Define your species data set name and the column name with the scientific names of your species (if you are following a Simple Darwin Core naming format, this column should be scientificName, but any column name is fine).
#'
#' The function will read the various strings in your species name column and identify them as either a family, genus, species, or subspecies. This function only works with cleaned and parsed scientific names. If the scientific name is higher than family, the function will not work correctly. Subfamily and Tribe names (which, similar to family names end in "ae*") will be designated Family.
#'
#' @param df - The name of your data frame containing species observations
#' @param sciName_col - The name of the column within your data frame containing the scientific names of the species.
#'
#' @return The function returns a new column in the given data frame named taxonRank with the taxonomic rank of the corresponding scientific name in each column. If there is no name in a row, then it returns as NA for that row.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mydf <- get_taxon_rank(df = mydf, sciName_col = "scientificName")
#' }
#'
get_taxon_rank <- function(df, sciName_col) {
  sciName_col <- df[[sciName_col]]
  dplyr::mutate(df, taxonRank = dplyr::case_when(
    stringr::str_detect(sciName_col, "\\s[^\\s]*\\s") ~ "subspecies", #regex says match a space, followed by any number of characters, followed by another space
    stringr::str_detect(sciName_col, "\\s.*") ~ "species", #regex says match a space followed by any number of character
    stringr::str_detect(sciName_col, "ae$") ~ "family", #regex says match to any word that has ae at the end of it
    stringr::str_detect(sciName_col, "^\\S*$") ~ "genus")) #regex says match to any number of characters that DO NOT have a space in front and then ends
}

#' Threatened Or Endangered Species Checker Function
#'
#' @description `check_te()` generates a list of species you should consider removing from your dataset before making it public by matching the scientific names within your data set to the Federal Conservation List. `check_te()` should be considered a helpful tool for identifying federally listed endangered and threatened species in your data. Each National Park has a park-specific Protected Data Memo that outlines which data should be restricted. Threatened and endangered species are often - although not always - listed on these Memos. Additional species (from state conservation lists) or non-threatened and non-endangered species of concern or other biological or non-biological resources may be listed on Memos. Consult the relevant park-specific Protected Data Memo prior to making decisions on restricting or releasing data.
#'
#' @details Define your species data set name, column name with the scientific names of your species, and your four letter park code.
#'
#' The `check_te()` function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your data frame. Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed. Because the odata API service is not publicly available, you must be logged in to the NPS VPN or in the office to use this function.
#'
#'  For the default, expansion = FALSE, the function will perform an exact match between the taxa in your scientificName column and the federal Conservation List and then filter the results to keep only species that are listed as endangered, threatened, or considered for listing. If your scientificName column contains information other than the binomial (genus and species), no matches will be returned. For instance, if you have an Order or just a genus listed, these will not be matched to the Federal Conservation List.
#'
#'  If you set expansion = TRUE, the function will truncate each item in your scientificName column to the first word in an attempt to extract a genus name. If you only have genera listed, these will be retained. If you have have higher-order taxa listed such as Family, Order, or Phyla again the first word will be retained. This first word (typically a genus) will be matched to just the generic name of species from the Federal Conservation List. All matches, regardless of listing status, are retained. The result is that for a given species in your scientificName column, all species within that genus that are on the Federal Conservation List will be returned (along with their federal conservation listing codes and a column indicating whether the species is actually in your data or is part of the expanded search).
#'
#' @param x - The name of your data frame containing species observations
#' @param species_col - The name of the column within your data frame containing the scientific names of the species (genus and specific epithet).
#' @param park_code -  A four letter park code. Or a list of park codes.
#' @param expansion - Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your data set and the federal match list. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the match list that match any genera listed in your data set, regardless of whether a given species is actually in your data set. An additional column indicating whether the species returned is in your data set ("In your Data") or has been expanded to ("Expansion") is generated.
#'
#' @return The function returns a (modified) data frame with the names of all the species that fall under the federal conservation list. The resulting data frame may have multiple instances of a given species if it is listed in multiple parks (park codes for each listing are supplied). Technically it is a huxtable, but it should function identically to a data frame for downstream purposes.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' #for individual parks:
#' check_te(x = my_species_dataframe, species_col = "scientificName", park_code = "BICY")
#' list<-check_te(data, "scientificName", "ROMO", expansion=TRUE)
#' # for a list of parks:
#' park_code<-c("ROMO", "YELL", "SAGU")
#' list<-check_te(data, "scientificName", park_code, expansion=TRUE)
#' }
#'
check_te <- function(x, species_col, park_code, expansion=FALSE) {
  #generate URL for odata services:
  url<-"https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27"
  for(i in seq_along(park_code)){
    url <- paste0(url, park_code[i], "%27%20or%20ParkCode%20eq%20%27")
  }
  odata_url <- paste0(url, "All%27")
  #trycatch for VPN connections:
  tryCatch(
    {
      fedlist <- ODataQuery::retrieve_data(odata_url)},
    error = function(e) {
      cat(crayon::red$bold("ERROR: "),
          "Your connection timed out.", sep="")
      stop()
    })
  #subset incoming data:
  fedspp <- as.data.frame(fedlist$value$ProtectedSci)
  fedspp <- cbind(fedspp, fedlist$value$MatchListStatus)
  fedspp <- cbind(fedspp, fedlist$value$ParkCode)
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code", "park_code")
  # add column explaining Fed T and E codes. From:
  # https://ecos.fws.gov/ecp0/html/db-status.html
  #---- code folding ----
  fedspp<-fedspp %>% dplyr::mutate(status = dplyr::case_when(
    status_code == "Fed-E" ~ "Endangered",
    status_code == "Fed-T" ~ "Threatened",
    status_code == "Fed-EmE" ~ "Emergency Listing, Endangered",
    status_code == "Fed-EmT" ~ "Emergency Listing Threatened",
    status_code == "Fed-EXPE" ~ "Experimental Population, Essential",
    status_code == "Fed-XE" ~ "Experimental Population, Essential",
    status_code == "Fed-EXPN" ~ "Experimental Population, Non-Essential",
    status_code == "XN" ~ "Experimental Population, Non-Essential",
    status_code == "Fed-SAE" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-E(S/A)" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-SAT" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-T(S/A)" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-SC" ~ "Species of Concern",
    status_code == "Fed-SU" ~ "Status Undefined",
    status_code == "Fed-UR" ~ "Under Review in the Candidate or Petition Process",
    status_code == "Fed-PE" ~ "Proposed Endangered",
    status_code == "Fed-PT" ~ "Proposed Threatened",
    status_code == "Fed-PEXPE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PXE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PEXPN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PXN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PSAE" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PE(S/A)" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PSAT" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-PT(S/A)" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-RT" ~ "Resolved Taxon",
    status_code == "Fed-C" ~ "Candidate Taxon, Ready for Proposal",
    status_code == "Fed-C2" ~ "Heritage Program Taxon of Concern, FWS NOR Taxon before 1996",
    status_code == "Fed-D3A" ~ "Delisted Taxon, Evidently Extinct",
    status_code == "Fed-D3B" ~ "Delisted Taxon, Invalid Name in Current Scientific Opinion",
    status_code == "Fed-D3C" ~ "Delisted Taxon, Recovered",
    status_code == "Fed-DA" ~ "Delisted Taxon, Amendment of the Act",
    status_code == "Fed-DM" ~ "Delisted Taxon, Recovered, Being Monitored First Five Years",
    status_code == "Fed-DO" ~ "Delisted Taxon, Original Commercial Data Erroneous",
    status_code == "Fed-DP" ~ "Delisted Taxon, Discovered Previously Unknown Additional Populations and/or Habitat",
    status_code == "Fed-DR" ~ "Delisted Taxon, Taxonomic Revision (Improved Understanding)",
    status_code == "Fed-AD" ~ "Proposed Delisting",
    status_code == "Fed-AE" ~ "Proposed Reclassification to Endangered",
    status_code == "Fed-AT" ~ "Proposed Reclassification to Threatened",
    status_code == "Fed-DNS" ~ "Original Data in Error - Not a listable entity",
    status_code == "Fed-NL" ~ "Not Listed",
    status_code == "Fed-Unlist" ~ "Pre-Act Delisting (or clearance--removal from the Lists) (code no longer in use)",
    status_code == "Fed-E*" ~ "Endangered Genus (code no longer in use)"
  ))
  #---- end code folding ----
  #get date data were pulled from FWS:
  fed_date <- fedlist$value$DateListImported[1]
  fed_date <- substr(fed_date, 1, 10)
  #get URL data were accessed from:
  url <- fedlist$value$DataSource[1]
  #get just species from user data frame:
  species_col_grepl <- paste0('\\b', species_col, '\\b')
  Species <- x[grepl(species_col_grepl, colnames(x))]
  colnames(Species) <- "species_col"
  # if any member of a genera in the dataset is protected, return all members
  # of that genera, even if they aren't in the observational data:
  if (expansion == TRUE) {
    #get genus name in input dataframe:
    Species$genus_col <- gsub(" .*$", "", Species$species_col)
    #get genus name in fedspp:
    fedspp$genus_col <- gsub(" .*$", "", fedspp$species_col)
    #inner join based on genera:
    TorE <- dplyr::inner_join(Species, fedspp, by = "genus_col")

    #if no species in the list:
    if (nrow(TorE) * ncol(TorE) == 0) {
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".", sep = "")
      return()
    }
    #if there are species returned:
    if (nrow(TorE) * ncol(TorE) > 0) {
      #add a column indicating if entry was in the original dataset or just shares genus name)
      TorE <- TorE %>%
        dplyr::mutate(InData = ifelse(species_col.x == species_col.y,
                                      "In your Data", "Expansion"))
      #clean up dataframe:
      TorE <- TorE[, c(5, 3, 7, 4, 6)]
      colnames(TorE) <- c("Park_code",
                          "Species",
                          "In_data",
                          "status_code",
                          "status_explanation")
      #format output for easy digestion:
      TorE <- huxtable::as_hux(TorE)
      TorE <- huxtable::map_text_color(TorE,
                                     huxtable::by_values("In your Data" = "green",
                                                         "Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE <- huxtable::theme_basic(TorE)
      #print data source and date:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
  }
  #if expansion = FALSE:
  if (expansion == FALSE) {
    #find all T&E species
    TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
    #keep only rows with Fed-E, Fed-T, Fed-C and Fed-C2 status codes
    TorE <- TorE[which(TorE$status_code == "Fed-C" |
                         TorE$status_code == "Fed-T" |
                         TorE$status_code == "Fed-E" |
                         TorE$status_code == "Fed-C2"),]
    #If no species of concern, state that and exit function.
    if (nrow(TorE) * ncol(TorE) == 0) {
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
    #if there are species in the list, return list (and data source/date):
    if (nrow(TorE) * ncol(TorE) > 0) {
      TorE <- TorE[, c(3,1,2,4)]
      colnames(TorE) <- c("Park_code", "Species", "status_code",
                          "status_explanation")
      TorE <- huxtable::as_hux(TorE)
      TorE <- huxtable::map_text_color(TorE,
                                     huxtable::by_values("Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE <- huxtable::theme_basic(TorE)
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep = "")
      return(TorE)
    }
  }
}

#' Threatened Or Endangered Species Checker Function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated in favor of `check_te()`. The function name was changed to promote constancy in function naming across the package and to conform with tidyverse style guides. `te_check()` is no longer updated and may not reference the latest version of the federal endangered and threatened species listings.
#'
#' `te_check()` generates a list of species you should consider removing from your dataset before making it public by matching the scientific names within your data set to the Federal Conservation List. `te_check()` should be considered a helpful tool for identifying federally listed endangered and threatened species in your data. Each National Park has a park-specific Protected Data Memo that outlines which data should be restricted. Threatened and endangered species are often - although not always - listed on these Memos. Additional species (from state conservation lists) or non-threatened and non-endangered species of concern or other biological or non-biological resources may be listed on Memos. Consult the relevant park-specific Protected Data Memo prior to making decisions on restricting or releasing data.
#'
#' @details Define your species data set name, column name with the scientific names of your species, and your four letter park code.
#'
#' The `te_check()` function downloads the Federal Conservation list using the IRMA odata API service and matches this species list to the list of scientific names in your data frame. Keep in mind that this is a Federal list, not a state list. Changes in taxa names may also cause some species to be missed. Because the odata API service is not publicly available, you must be logged in to the NPS VPN or in the office to use this function.
#'
#'  For the default, expansion = FALSE, the function will perform an exact match between the taxa in your scientificName column and the federal Conservation List and then filter the results to keep only species that are listed as endangered, threatened, or considered for listing. If your scientificName column contains information other than the binomial (genus and species), no matches will be returned. For instance, if you have an Order or just a genus listed, these will not be matched to the Federal Conservation List.
#'
#'  If you set expansion = TRUE, the function will truncate each item in your scientificName column to the first word in an attempt to extract a genus name. If you only have genera listed, these will be retained. If you have have higher-order taxa listed such as Family, Order, or Phyla again the first word will be retained. This first word (typically a genus) will be matched to just the generic name of species from the Federal Conservation List. All matches, regardless of listing status, are retained. The result is that for a given species in your scientificName column, all species within that genus that are on the Federal Conservation List will be returned (along with their federal conservation listing codes and a column indicating whether the species is actually in your data or is part of the expanded search).
#'
#' @param x - The name of your data frame containing species observations
#' @param species_col - The name of the column within your data frame containing the scientific names of the species (genus and specific epithet).
#' @param park_code -  A four letter park code. Or a list of park codes.
#' @param expansion - Logical. Defaults to FALSE. The default setting will return only exact matches between your the scientific binomial (genera and specific epithet) in your data set and the federal match list. Setting expansion = TRUE will expand the list of matches to return all species (and subspecies) that from the match list that match any genera listed in your data set, regardless of whether a given species is actually in your data set. An additional column indicating whether the species returned is in your data set ("In your Data") or has been expanded to ("Expansion") is generated.
#'
#' @return The function returns a (modified) data frame with the names of all the species that fall under the federal conservation list. The resulting data frame may have multiple instances of a given species if it is listed in multiple parks (park codes for each listing are supplied). Technically it is a huxtable, but it should function identically to a data frame for downstream purposes.
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #for individual parks:
#' te_check(x = my_species_dataframe, species_col = "scientificName", park_code = "BICY")
#' list<-te_check(data, "scientificName", "ROMO", expansion=TRUE)
#' # for a list of parks:
#' park_code<-c("ROMO", "YELL", "SAGU")
#' list<-te_check(data, "scientificName", park_code, expansion=TRUE)
#' }
#'
te_check <- function(x, species_col, park_code, expansion = FALSE) {
  lifecycle::deprecate_soft(when = "0.1.0.3", "te_check()", "check_te()")
  #generate URL for odata services:
  url<-"https://irmadev.nps.gov/PrototypeCSVtoAPI/odata/FederalConservationListTaxaforDataProtection2272462?$filter=ParkCode%20eq%20%27"
  for(i in seq_along(park_code)){
    url <- paste0(url, park_code[i], "%27%20or%20ParkCode%20eq%20%27")
  }
  odata_url <- paste0(url, "All%27")
  #trycatch for VPN connections:
  tryCatch(
    {
      fedlist <- ODataQuery::retrieve_data(odata_url)},
    error = function(e){
      cat(crayon::red$bold("ERROR: "),
          "Your connection timed out.\n",
          "Make sure you are logged on to the VPN before running ",
          crayon::green$bold("te_check()"),
          ".", sep="")
      stop()
    })
  #subset incoming data:
  fedspp <- as.data.frame(fedlist$value$ProtectedSci)
  fedspp <- cbind(fedspp, fedlist$value$MatchListStatus)
  fedspp <- cbind(fedspp, fedlist$value$ParkCode)
  #rename columns
  colnames(fedspp)<-c("species_col", "status_code", "park_code")
  # add column explaining Fed T and E codes. From:
  # https://ecos.fws.gov/ecp0/html/db-status.html
  #---- code folding ----
  fedspp<-fedspp %>% dplyr::mutate(status = dplyr::case_when(
    status_code == "Fed-E" ~ "Endangered",
    status_code == "Fed-T" ~ "Threatened",
    status_code == "Fed-EmE" ~ "Emergency Listing, Endangered",
    status_code == "Fed-EmT" ~ "Emergency Listing Threatened",
    status_code == "Fed-EXPE" ~ "Experimental Population, Essential",
    status_code == "Fed-XE" ~ "Experimental Population, Essential",
    status_code == "Fed-EXPN" ~ "Experimental Population, Non-Essential",
    status_code == "XN" ~ "Experimental Population, Non-Essential",
    status_code == "Fed-SAE" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-E(S/A)" ~ "Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-SAT" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-T(S/A)" ~ "Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-SC" ~ "Species of Concern",
    status_code == "Fed-SU" ~ "Status Undefined",
    status_code == "Fed-UR" ~ "Under Review in the Candidate or Petition Process",
    status_code == "Fed-PE" ~ "Proposed Endangered",
    status_code == "Fed-PT" ~ "Proposed Threatened",
    status_code == "Fed-PEXPE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PXE" ~ "Proposed Experimental Population, Essential",
    status_code == "Fed-PEXPN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PXN" ~ "Proposed Experimental Population, Non-Essential",
    status_code == "Fed-PSAE" ~ "Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PE(S/A)" ~"Proposed Similarity of Appearance to an Endangered Taxon",
    status_code == "Fed-PSAT" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-PT(S/A)" ~ "Proposed Similarity of Appearance to a Threatened Taxon",
    status_code == "Fed-RT" ~ "Resolved Taxon",
    status_code == "Fed-C" ~ "Candidate Taxon, Ready for Proposal",
    status_code == "Fed-C2" ~ "Heritage Program Taxon of Concern, FWS NOR Taxon before 1996",
    status_code == "Fed-D3A" ~ "Delisted Taxon, Evidently Extinct",
    status_code == "Fed-D3B" ~ "Delisted Taxon, Invalid Name in Current Scientific Opinion",
    status_code == "Fed-D3C" ~ "Delisted Taxon, Recovered",
    status_code == "Fed-DA" ~ "Delisted Taxon, Amendment of the Act",
    status_code == "Fed-DM" ~ "Delisted Taxon, Recovered, Being Monitored First Five Years",
    status_code == "Fed-DO" ~ "Delisted Taxon, Original Commercial Data Erroneous",
    status_code == "Fed-DP" ~ "Delisted Taxon, Discovered Previously Unknown Additional Populations and/or Habitat",
    status_code == "Fed-DR" ~ "Delisted Taxon, Taxonomic Revision (Improved Understanding)",
    status_code == "Fed-AD" ~ "Proposed Delisting",
    status_code == "Fed-AE" ~ "Proposed Reclassification to Endangered",
    status_code == "Fed-AT" ~ "Proposed Reclassification to Threatened",
    status_code == "Fed-DNS" ~ "Original Data in Error - Not a listable entity",
    status_code == "Fed-NL" ~ "Not Listed",
    status_code == "Fed-Unlist" ~ "Pre-Act Delisting (or clearance--removal from the Lists) (code no longer in use)",
    status_code == "Fed-E*" ~ "Endangered Genus (code no longer in use)"
  ))
  #---- end code folding ----
  #get date data were pulled from FWS:
  fed_date <- fedlist$value$DateListImported[1]
  fed_date <- substr(fed_date, 1, 10)
  #get URL data were accessed from:
  url <- fedlist$value$DataSource[1]
  #get just species from user data frame:
  species_col_grepl<-paste0('\\b', species_col, '\\b')
  Species<-x[grepl(species_col_grepl, colnames(x))]
  colnames(Species)<-"species_col"
  # if any member of a genera in the dataset is protected, return all members
  # of that genera, even if they aren't in the observational data:
  if(expansion == TRUE){
    #get genus name in input dataframe:
    Species$genus_col <- gsub(" .*$", "", Species$species_col)
    #get genus name in fedspp:
    fedspp$genus_col <- gsub(" .*$", "", fedspp$species_col)
    #inner join based on genera:
    TorE <- dplyr::inner_join(Species, fedspp, by="genus_col")

    #if no species in the list:
    if(nrow(TorE)*ncol(TorE)==0){
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".", sep="")
      return()
    }
    #if there are species returned:
    if(nrow(TorE)*ncol(TorE) > 0){
      #add a column indicating if entry was in the original dataset or just shares genus name)
      TorE <- TorE %>%
        dplyr::mutate(InData = ifelse(species_col.x == species_col.y,
                                      "In your Data", "Expansion"))
      #clean up dataframe:
      TorE<-TorE[, c(5,3,7,4,6)]
      colnames(TorE) <- c("Park_code",
                          "Species",
                          "In_data",
                          "status_code",
                          "status_explanation")
      #format output for easy digestion:
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::map_text_color(TorE,
                                     huxtable::by_values("In your Data" = "green",
                                                         "Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE<-huxtable::theme_basic(TorE)
      #print data source and date:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
  }
  #if expansion = FALSE:
  if(expansion == FALSE){
    #find all T&E species
    TorE <- dplyr::inner_join(Species, fedspp, by = "species_col")
    #keep only rows with Fed-E, Fed-T, Fed-C and Fed-C2 status codes
    TorE <- TorE[which(TorE$status_code == "Fed-C" |
                         TorE$status_code == "Fed-T" |
                         TorE$status_code == "Fed-E" |
                         TorE$status_code == "Fed-C2"),]
    #If no species of concern, state that and exit function.
    if(nrow(TorE)*ncol(TorE)==0){
      cat("No T&E species found in your dataset.\n")
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
    #if there are species in the list, return list (and data source/date):
    if(nrow(TorE)*ncol(TorE) > 0){
      TorE<-TorE[, c(3,1,2,4)]
      colnames(TorE)<-c("Park_code", "Species", "status_code", "status_explanation")
      TorE<-huxtable::as_hux(TorE)
      TorE<-huxtable::map_text_color(TorE,
                                     huxtable::by_values("Threatened" = "darkorange2",
                                                         "Endangered" = "red",
                                                         "Concern" = "yellow3",
                                                         "Candidate" = "yellow3"))
      TorE<-huxtable::theme_basic(TorE)
      #print date and source of data:
      cat("Your T&E check used data pulled from: ",
          crayon::bold$red(url), " on ",
          crayon::bold$red(fed_date), ".\n", sep="")
      return(TorE)
    }
  }
}

#' Load a full list of threatened and endangered species from the USFWS
#'
#' @description `load_te_species()` gets the USFWS's catalog of Threatened and Endangered species and
#' resolves scientific names and record IDs using two taxonomic databases: Integrated Taxonomic Information System (ITIS) and
#' Global Biodiversity Information Facility (GBIF). It then stores this data as a csv file in the user's cache. It also returns the dataframe.
#'
#' @details `load_te_species()` relies on an API call to the Environmental Conservation Online System (ECOS) at https://ecos.fws.gov/ecp/ and
#' the Global Names Verifier (GNV) https://verifier.globalnames.org/ from Global Names Architecture. It writes the resulting dataframe as a csv
#' entitled "resolved_te_species.csv" to the user's cache. In order to ensure that the species list is current, the function checks if there is
#' already a file named "resolved_te_species.csv" in the user's cache, and if there is, it checks if it was last modified over 30 days ago. If the
#' existing data is over 30 days old, it overwrites it with new data. Data can be updated at any time using force_refresh.
#'
#' @param force_refresh
#'     (logical) Defaults to FALSE. If set to TRUE, gets the current list of T&E species. Data is automatically refreshed if it is over one month old.
#' @param domestic_only
#'     (logical) Defaults to TRUE, meaning that the function loads data only for species that occur domestically. If set to FALSE, it returns all species.
#' @param return_object
#'     (logical) Defaults to FALSE.If set to TRUE, returns a data frame object of T&E species and resolved taxonomy.
#'
#' @return The function returns a data frame with all listed domestic (or foreign and domestic species, if requested by the user) in the ECOS database, and taxonomic names are record IDs resolved using ITIS and GBIF.
#'
#' @export
#' @examples
#' \dontrun{
#' load_te_species(force_refresh = TRUE, domestic_only = TRUE)

load_te_species <- function(force_refresh = FALSE, domestic_only = TRUE, return_object = FALSE) {

  refresh <- force_refresh

  # check if te species list exists in user's cache
  if (file.exists(paste0(rappdirs::user_cache_dir(), "/", "resolved_te_species.csv"))) {
    # check when file was last modified
    last_modified <- file.info(paste0(rappdirs::user_cache_dir(), "/", "resolved_te_species.csv"))$mtime
    message(paste0("T&E species file with resolved taxonomy found in cache.\nFile last modified on ",
                   last_modified, "."))
    if (difftime(Sys.time(), last_modified, units = c("days")) > 30) {
      # set refresh to true if file is over 30 days old
      message("File was last modified over 30 days ago. Refreshing data.")
      refresh <- TRUE
    } else {
      user_input <- readline(prompt = ("File was last modified within the last 30 days.\nType 'R' to refresh data or Enter to skip refresh."))
      if (user_input %in% c("R", "r")) {
        refresh <- TRUE
      } else {
        refresh <- FALSE
      }
    }
  } else {
    # if a resolved_te_species.csv file does not exist, set refresh to true to get data
    refresh <- TRUE
  }

  if (refresh) {
    message("Loading ECOS database.")

    # pull whole list of t&e species
    te_list_url <- "https://ecos.fws.gov/ecp/pullreports/catalog/species/report/species/export?format=json&columns=%2Fspecies%40sn%2Ccn%2Csid%3B%2Fspecies%2Ftaxonomy%40group%3B%2Fspecies%40status%2Cstatus_category%2Cdesc%2Cis_foreign"
    te_query <- httr::GET(te_list_url)

    te_response <- jsonlite::fromJSON(httr::content(te_query, as = "text"))$data

    te_response_cleaned <- purrr::map_depth(te_response, 2, ~ifelse(is.null(.x), NA, .x))

    te_response_cleaned <- lapply(X = te_response_cleaned, FUN = unlist)

    all_te_species <- do.call(rbind.data.frame, te_response_cleaned)

    colnames(all_te_species) <- c("ECOS_scientificName", "ECOS_commonName", "ECOS_ID", "ECOS_taxonomicGroup",
                                  "ECOS_listingStatus", "ECOS_statusCategory", "ECOS_whereListed", "ECOS_isForeign")

    # keep only listed, proposed, candidate, and species of concern
    all_te_species <- all_te_species |>
      dplyr::filter(ECOS_listingStatus %in% c("Species of Concern") |
                      ECOS_statusCategory %in% c("Listed", "Proposed for Listing", "Candidate"))

    # filter to domestic only based on domestic_only
    if (domestic_only) {
      all_te_species <- all_te_species |>
        dplyr::filter(ECOS_isForeign == "FALSE") |>
        dplyr::select(-ECOS_isForeign)
    }

    # edit scientific names to better match ITIS
    cleaned_sci_names <- all_te_species$ECOS_scientificName

    # fix known misspellings of scientific names
    cleaned_sci_names <- gsub("Acalyptera", "Acalypta", cleaned_sci_names)
    cleaned_sci_names <- gsub("Alopecoenas", "Pampusana", cleaned_sci_names)
    cleaned_sci_names <- gsub("Arundinax", "Iduna", cleaned_sci_names)
    cleaned_sci_names <- gsub("Cyanecula", "Luscinia", cleaned_sci_names)
    cleaned_sci_names <- gsub("Hemipachnolia subporphyria subporphyria", "Hemipachnobia subporphyrea", cleaned_sci_names)
    cleaned_sci_names <- gsub("Itodacnus", "Itodacne", cleaned_sci_names)
    cleaned_sci_names <- gsub("Larvivora", "Luscinia", cleaned_sci_names)
    cleaned_sci_names <- gsub("Microcylleopus", "Microcylloepus", cleaned_sci_names)
    cleaned_sci_names <- gsub("Nucombia plicata", "Newcombia cumingi", cleaned_sci_names)
    cleaned_sci_names <- gsub("Plejebus", "Plebejus", cleaned_sci_names)
    cleaned_sci_names <- gsub("Pseudanopthalmus", "Pseudanophthalmus", cleaned_sci_names)
    cleaned_sci_names <- gsub("Spartiniphaga", "Photedes", cleaned_sci_names)
    cleaned_sci_names <- gsub("Toltecus", "Comanchelus", cleaned_sci_names)

    # remove synonyms (between parentheses)
    cleaned_sci_names <- gsub(R"{\s*\([^\)]+\)}", "", cleaned_sci_names)

    # add cleaned scientific names to all_te_species df (for later joins)
    all_te_species[["scientificNameCleaned"]] <- cleaned_sci_names

    # split names into batches of 50 to pass through GNV
    name_batches <- split(cleaned_sci_names, ceiling(seq_along(cleaned_sci_names)/50))

    # initiate empty list to store resolved names; use ITIS and GBIF
    resolved_te_names <- NULL

    message("Resolving taxonomic data.")

    for (i in seq_along(name_batches)) {
      # paste all names in batch into 1 string to search on
      temp_names <- paste(name_batches[[i]], collapse = "%7C")
      temp_names <- gsub(" ", "%20", temp_names)

      gnv_url <- paste0("https://verifier.globalnames.org/api/v1/verifications/",
                        temp_names,
                        "?&data_sources=3%7C11&capitalize=true")

      tryCatch({
        gnv_query <- httr::GET(gnv_url)
      },
      error=function(e) {
        print(e)
        stop("Connection to Global Names Verifier failed.")
      })

      gnv_response <- jsonlite::fromJSON(httr::content(gnv_query, as = "text", encoding = "UTF-8"))$names

      # unnest results into one dataframe; rename and select relevant columns
      temp_results <- tidyr::unnest(gnv_response,
                                    cols = c(bestResult),
                                    names_sep = "_") |>
        dplyr::rename(submittedName = name,
                      taxonSource = bestResult_dataSourceTitleShort,
                      currentTaxonID = bestResult_currentRecordId,
                      currentName = bestResult_currentCanonicalSimple) |>
        dplyr::select(submittedName, currentName, currentTaxonID, taxonSource)
      resolved_te_names <- rbind(resolved_te_names, temp_results)
    }

    # get list of species that weren't resolved
    no_match <- resolved_te_names$submittedName[which(is.na(resolved_te_names$currentName))]

    if (length(no_match) > 0) {
      # print message with names that were not resolved
      cli::cli_alert_danger(paste("The following names were not resolved:", paste0("'", no_match, "'", collapse = " | ")))
    }

    # keep only distinct rows in resolved names df
    resolved_te_names <- resolved_te_names |>
      dplyr::distinct()

    # join to TE species list from ECOS
    all_te_species_resolved <- all_te_species |>
      dplyr::left_join(resolved_te_names, by = dplyr::join_by(scientificNameCleaned == submittedName),
                       relationship = "many-to-many") |>
      dplyr::select(-scientificNameCleaned)

    # write file to user's cache
    message("Writing T&E species file with resolved taxonomy to cache.")
    readr::write_csv(all_te_species_resolved, paste0(rappdirs::user_cache_dir(), "/resolved_te_species.csv"))

  } else {
    # if species list is not refreshed, read species list from cache
    all_te_species_resolved <- suppressMessages(readr::read_csv(paste0(rappdirs::user_cache_dir(), "/", "resolved_te_species.csv")))
  }

  # return dataframe with te species, if requested
  if (return_object) {
    return(all_te_species_resolved)
  }
}

#' Get a list of threatened or endangered species in a data set
#'
#' @description `check_te_species()` generates a data frame with threatened or endangered species in the input data set by matching scientific names to the USFWS's catalog of T&E species.
#'
#' @details `check_te_species()` relies on a file cached in load_te_species(). If this file does not exist, the function will exit with an error.
#'
#' @param x
#'     (data frame) A data frame containing scientific names.
#' @param sciname_col
#'     (character or numeric) The name or index of the column containing scientific names (genus and specific epithet).
#' @param resolve_taxonomy
#'     (logical) Defaults to TRUE. TRUE means taxonomic names will be resolved to current names and record IDs using ITIS and GBIF.
#'     Set to FALSE if taxonomy has already been resolved using one or both of these databases.
#' @param listing_status
#'     (character) Defaults to "all". A user may choose from "listed" to view only listed species, "listed or proposed" to view listed species and species proposed for listing
#'     or "all" to view listed, proposed, and species of concern.
#' @param viewer_table
#'     (logical) Defaults to FALSE. If set to TRUE, shows the returned table in the plot viewer in R, complete with links to the species profiles in the ECOS and ITIS/GBIF databases.
#'
#' @return The function returns a data frame with the name of each submitted record that matched a threatened or endangered species,
#' the matched scientific name, common name, taxonomic group, listing status under the Endangered Species Act, listing status category,
#' a description of where the species is considered threatened or endangered and its ECOS profile ID.
#'
#' @export
#' @examples
#' \dontrun{
#' check_te_species(x = sfcn_mammals, species_col = "scientificName", resolve_taxonomy = FALSE, viewer_table = TRUE)
#' check_te_species(x = sfcn_mammals, species_col = "scientificName", listing_status = "listed")

check_te_species <- function(x,
                             sciname_col,
                             resolve_taxonomy = TRUE,
                             listing_status = "all",
                             viewer_table = FALSE) {
  # check that x is a data frame and species_col exists in x
  if (!any(class(x) == "data.frame")) {
    stop("Input must be a data frame.")
  }

  if (is.null(x[[sciname_col]])) {
    stop("Scientific name column specified must exist in input data frame.")
  }

  # get path to cached te species list
  cached_species_list <- paste0(rappdirs::user_cache_dir(), "/", "resolved_te_species.csv")

  # check that cached file exists
  if (file.exists(cached_species_list)) {
    # check date cached file was last modified
    last_modified <- file.info(cached_species_list)$mtime
    if (difftime(Sys.time(), last_modified, units = c("days")) > 30) {
      # if cached species list was last modifed over 30 days ago, prompt user to refresh data
      stop("T&E species file with resolved taxonomy was last modified over 30 days ago. Use QCkit::load_te_species() to refresh data.")
    } else {
      # read in cached species list
      te_list <- suppressMessages(readr::read_csv(cached_species_list))
    }
  } else {
    # if cached species list does not exist, prompt user to load data
    stop("T&E species file with resolved taxonomy not found in cache. Use QCkit::load_te_species() to load data.")
  }

  # message with data source and date loaded
  message("Your T&E check used data pulled from: ",
          crayon::bold$green("https://ecos.fws.gov/ecp/"), " on ",
          crayon::bold$green(last_modified), sep = "")


  if (listing_status == "listed") {
    te_list <- te_list |>
      dplyr::filter(ECOS_statusCategory == "Listed")
  } else if (listing_status == "listed or proposed") {
    te_list <- te_list |>
      dplyr::filter(ECOS_statusCategory %in% c("Listed", "Proposed for Listing"))
  }

  # get scientific names from input as a character vector; remove NAs
  scinames <- unique(x[[sciname_col]])
  scinames <- scinames[!is.na(scinames)]

  # if resolved taxonomy is requested, use Global Names Verifier (GNV)
  if (resolve_taxonomy) {

    # split names into batches of 50 to pass through GNV
    input_name_batches <- split(scinames, ceiling(seq_along(scinames)/50))

    # initiate empty list to store resolved names; use ITIS and GBIF
    input_names_resolved <- NULL

    message("Resolving taxonomic data.")

    for (i in seq_along(input_name_batches)) {
      # paste all names in batch into 1 string to search on
      temp_names <- paste(input_name_batches[[i]], collapse = "%7C")
      temp_names <- gsub(" ", "%20", temp_names)

      gnv_url <- paste0("https://verifier.globalnames.org/api/v1/verifications/",
                        temp_names,
                        "?&data_sources=3%7C11&capitalize=true")

      tryCatch({
        gnv_query <- httr::GET(gnv_url)
      },
      error=function(e) {
        print(e)
        stop("Connection to Global Names Verifier failed.")
      })

      gnv_response <- jsonlite::fromJSON(httr::content(gnv_query, as = "text", encoding = "UTF-8"))$names

      # unnest results into one dataframe; rename and select relevant columns
      temp_results <- tidyr::unnest(gnv_response,
                                    cols = c(bestResult),
                                    names_sep = "_") |>
        dplyr::rename(INPUT_scientificName = name,
                      INPUT_currentName = bestResult_currentCanonicalSimple) |>
        dplyr::select(INPUT_scientificName, INPUT_currentName)
      input_names_resolved <- rbind(input_names_resolved, temp_results)
    }

    # get list of species that weren't resolved
    no_match <- input_names_resolved$INPUT_scientificName[which(is.na(input_names_resolved$INPUT_currentName))]

    if (length(no_match) > 0) {
      # Print message with names that were not resolved
      cli::cli_alert_danger(paste("The following names were not resolved:", paste0("'", no_match, "'", collapse = " | ")))
    }

    # keep only distinct rows in resolved names df
    input_names_resolved <- input_names_resolved |>
      dplyr::distinct()

    # join with te species list by resolved/current name for species that were resolved
    found_te_species <- input_names_resolved |>
      dplyr::filter(!is.na(INPUT_currentName)) |>
      dplyr::inner_join(te_list, by = dplyr::join_by(INPUT_currentName == currentName))

    # special case--check if any species in ECOS list that *weren't* resolved are in the input data frame
    te_unresolved <- te_list |>
      dplyr::filter(is.na(currentName))

    unresolved_te_species <- input_names_resolved[which(input_names_resolved$INPUT_scientificName
                                                        %in% te_unresolved$ECOS_scientificName), ] |>
      dplyr::inner_join(te_list, by = dplyr::join_by(INPUT_scientificName == ECOS_scientificName)) |>
      dplyr::mutate(ECOS_scientificName = INPUT_scientificName, .after = INPUT_currentName) |>
      dplyr::select(-currentName)

    full_te_species <- rbind(found_te_species, unresolved_te_species)
  } else {
    # if resolved taxonomy is not requested, use sciname_col to filter results
    te_species_ECOS_name <- te_list |>
      dplyr::filter(ECOS_scientificName %in% scinames) |>
      dplyr::mutate(INPUT_scientificName = ECOS_scientificName, .before = 1)

    te_species_current_name <- te_list |>
      dplyr::filter(currentName %in% scinames) |>
      dplyr::mutate(INPUT_scientificName = currentName, .before = 1)

    full_te_species <- rbind(te_species_ECOS_name, te_species_current_name) |>
      dplyr::distinct()
  }
  # if no species in the list:
  if (nrow(full_te_species) == 0) {
    message("No T&E species found in your dataset.")
  } else  if (nrow(full_te_species) > 0) {
    message(paste0(nrow(full_te_species), " T&E species were found in your dataset."))
  }

  if (viewer_table) {
    te_species_table_gt <- full_te_species |>
      dplyr::mutate(ECOS_ID = paste0("https://ecos.fws.gov/ecp/species/", ECOS_ID)) |>
      dplyr::mutate(currentTaxonID = as.character(currentTaxonID)) |>
      dplyr::mutate(currentTaxonID = dplyr::case_when(taxonSource == "ITIS" ~
                                                        paste0("https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=", currentTaxonID),
                                                      taxonSource == "GBIF" ~
                                                        paste0("https://www.gbif.org/species/", currentTaxonID),
                                                      TRUE ~ "")) |>
      dplyr::mutate(ECOS_listingStatus = factor(ECOS_listingStatus, levels = c("Species of Concern",
                                                                               "Proposed Endangered",
                                                                               "Proposed Threatened",
                                                                               "Proposed Similarity of Appearance (Threatened)",
                                                                               "Endangered",
                                                                               "Threatened",
                                                                               "Experimental Population, Non-Essential",
                                                                               "Similarity of Appearance (Threatened)")))


    print(te_species_table_gt |>
            gt::gt() |>
            gt::fmt_url(columns = ECOS_ID,
                        label = gsub("https://ecos.fws.gov/ecp/species/", "", te_species_table_gt$ECOS_ID)) |>
            gt::fmt_url(columns = currentTaxonID,
                        label = stringr::str_extract_all(te_species_table_gt$currentTaxonID, "[[:digit:]]+")) |>
            gt::data_color(
              columns = ECOS_listingStatus,
              method = "factor",
              palette = c("yellow", "orange", "orange", "orange", "red4", "red4", "red4", "red4")
            ))
  }

  return(full_te_species)
}