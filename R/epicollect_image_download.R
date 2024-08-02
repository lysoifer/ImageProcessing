#library(httr)
#library(jsonlite) # if needing json format

#' epi_image_dl
#'
#' Download images from an epicollect project
#'
#' @param slug project slug
#' @param form.ref form ref
#' @param access whether epicollect project is "pulic" or "private" (currently only supports public projects)
#' @param cID client ID if project is private
#' @param secret secret if project is private
#' @param cname name of column that photos are stored in
#' @param path file directory to save images to
#' @param df_path file path to save dataframe (must be .csv)
#'
#' @details
#' Slug and form ref can be found in epicollect project details. Use the project slug and the form ref. Code adapted from https://gist.github.com/mirko77/3f4a101cd4a77e2ae3e760d44d18d901
#'
#' @keywords epicollect, download
#'
#' @examples
#' epi_image_dl(slug = "test-photo-api", form.ref = "ead5f161866447249c9c57b255dae5c7_66abc8fcd2c06", access = "public", cname = "photo", path = "/path/to/photos/")
#'
#' @export
epi_image_dl = function(slug, form.ref, access, cID = NA, secret = NA, cname, path, df_path = NA) {
  if(access == "private") {
    res <- httr::POST("https://five.epicollect.net/api/oauth/token",
            body = list(grant_type = "client_credentials",
                        client_id = cID,
                        client_secret = secret))
    httr::http_status(res)
    token <- httr::content(res)$access_token
  }

  url.form<- paste("https://five.epicollect.net/api/export/entries/", slug, "?map_index=0&form_ref=", form.ref, "&format=csv&headers=true", sep= "")
  #url.form<- paste("https://five.epicollect.net/api/export/entries/", proj.slug, "?map_index=0&form_ref=", form.ref, "&format=json", sep= "") ## if using json

  if(access == "private") {
    res1<- httr::GET(url.form, add_headers("Authorization" = paste("Bearer", token)))
  } else {
    res1<- httr::GET(url.form)
  }

  # ct1 is a dataframe of the data from epicollect
  ct1<- read.csv(res1$url)
  #ct1<- fromJSON(rawToChar(content(res1))) ## if using json
  #ct1 = ct1$data$entries ## if using json

  # rename columns
  nms = colnames(ct1)[5:ncol(ct1)]
  nms = unlist(lapply(strsplit(nms, split = "_"), "[[", 2))

  colnames(ct1)[5:ncol(ct1)] = nms

  # add photo shortname to dataframe
  photo_short = unlist(lapply(strsplit(ct1$photo, split = "="), "[[", 4))
  ct1$photo_short = photo_short

  # download images
  for(i in 1:nrow(ct1)) {
    #fname = strsplit(ct1[i, which(grepl(cname, colnames(ct1)))], split = "=")[[1]][4]
    url = ct1[i, which(grepl(cname, colnames(ct1)))][1,1]
    fname = strsplit(url, split = "=")[[1]][4]
    dest = paste0(path, fname)
    download.file(url, destfile = dest, mode = "wb")
  }

  if(!is.na(df_path)) {
    write.csv(ct1, file = df_path)
  }

  # return dataframe
  return(ct1)
}




slug = "test-photo-api"
form.ref = "ead5f161866447249c9c57b255dae5c7_66abc8fcd2c06"
access = "public"
cname = "photo"
path = "examples/data/orig_imgs/"
df_path = "examples/output/dataframes/leaf_images.csv"






