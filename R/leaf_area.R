#' Leaf area
#' Calculate leaf area using pliman package
#'
#' @param imgpath vector of file paths to images
#' @param bg_file file path to an image of the background behind leaves (see details in pliman package)
#' @param area area of reference square
#' @param dir_processed directory to save processed images
#' @param prefix prefix of filenames for processed images
#' @param outdir filepath to save output dataframe
#' @param ... arguments supplied to analyze_objects() from pliman package
#'
#' @details
#' Reference area must be a square. It is recommended that you follow instructions in pliman package to identify the best index for your analysis.
#' The default used here is "NB". From testing, this appears to work well for a green leaf on a royal blue background with a white reference square.
#' For more instructions on calculating leaf area, see pliman reference at https://tiagoolivoto.github.io/pliman/
#'
#' @export
leaf_area = function(imgpath, bg_file, area, outdir, ...) {
  out = foreach::foreach(i = 1:length(imgpath), .combine = "rbind") %do% {
    im_i = imgpath[i]

    # get image file name
    im_name = strsplit(im_i, split = "/")[[1]]
    im_name = im_name[length(im_name)]
    im_name2 = strsplit(im_name, split = "\\.")[[1]][1]

    im = image_import(im_i)
    count = pliman::analyze_objects(im, marker = "id", index = "NB",
                            watershed = F, background = bg_file,
                            save_image = T, dir_processed = dir_processed, prefix = paste0(im_name2, "_"), ...)

    # get reference square
    asp_ratio = count$results$asp_ratio
    marker = count$results[which(round(asp_ratio, 1) == 1.0), "id"]

    # calculate area
    calc = pliman::get_measures(count, id = marker, area ~ area)
    morpho = calc[,c("area", "perimeter", "length", "width", "asp_ratio")]
    morpho$img_name = im_name
    morpho
  }
  return(out)
}
