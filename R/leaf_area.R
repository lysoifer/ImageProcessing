library(pliman)
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
leaf_area = function(imgpath, bg_file, area, outdir, ...) {
  wd = getwd()
  setwd(saveimg_dir)
  for(i in 1:length(imgpath)) {
    im_i = imgpath[i]
    im = import_image(im_i)
    count = analyze_objects(img, marker = "id", index = "NB",
                            watershed = F, background = bg_file,
                            save_image = T, dir_processed = dir_processed, prefix = prefix, ...)
    asp_ratio = count$results$asp_ratio
    marker = which(round(asp_ratio, 1) == 1.0)
    area = get_measures(count, id = marker, area ~ area)
    morpho = area[,c("area", "perimeter", "length", "width", "asp_ratio")]

    # get image file name
    im_i = strsplit(im_i, split = "/")[[1]]
    im_i = im_i[length(im_i)]
  }
  setwd(wd)
}
