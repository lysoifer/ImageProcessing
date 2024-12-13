#' Leaf area
#' Calculate leaf area using pliman package
#'
#' @param imgpath vector of file paths to images
#' @param bg_file file path to an image of the background behind leaves (see details in pliman package)
#' @param area area of reference square
#' @param dir_processed directory to save processed images
#' @param outdir if specified, filepath to save output dataframe
#' @param ... arguments supplied to analyze_objects() from pliman package
#'
#' @details
#' Reference area must be smaller than the leaf.
#' It is recommended that you follow instructions in pliman package to identify the best index for your analysis.
#' The default used here is "NB". From testing, this appears to work well for a green leaf on a royal blue background with a white reference object.
#' For more instructions on calculating leaf area, see pliman reference at https://tiagoolivoto.github.io/pliman/
#' The function will automatically append observations to an existing dataframe
#'
#' @export
leaf_area = function(imgpath, bg_file, area, dir_processed, outdir = NA, ...) {

  # read in output file if it exists
  if(file.exists(outdir)) {
    dat = read.csv(outdir)
  } else {
    dat = data.frame(area = double(), img_name = character())
  }


  for(i in 1:length(imgpath)) {
    im_i = imgpath[i]

    # get image file name
    im_name = strsplit(im_i, split = "/")[[1]]
    im_name = im_name[length(im_name)]
    im_name2 = strsplit(im_name, split = "\\.")[[1]][1]

    # run area calculation if it has not been run previously
    if (!file.exists(paste0(dir_processed, im_name2, "_img.png"))) {
      im = image_import(im_i)
      # initially use small lower noise for larger leaf pieces
      count = pliman::analyze_objects(im, marker = "id",
                                      index = "NB", watershed = F, background = bg_file,
                                      save_image = T, dir_processed = dir_processed,
                                      prefix = paste0(im_name2, "_"), reference_smaller = T,
                                      reference_area = area, reference = F, lower_noise = 0.01,
                                      marker_col = "red")
      marker = count$results[which(count$results$eccentricity ==
                                     min(count$results$eccentricity)), "id"]
      calc = pliman::get_measures(count, id = marker,
                                  area ~ area)
      # if any area is less than 0.01, there was probably noise, so recalculate with larger lower noise threshold for smaller leaves
      if(min(calc$area) < 0.05) {
        count = pliman::analyze_objects(im, marker = "id",
                                        index = "NB", watershed = F, background = bg_file,
                                        save_image = T, dir_processed = dir_processed,
                                        prefix = paste0(im_name2, "_"), reference_smaller = T,
                                        reference_area = area, reference = F, lower_noise = 0.1,
                                        marker_col = "red")
        marker = count$results[which(count$results$eccentricity ==
                                       min(count$results$eccentricity)), "id"]
        calc = pliman::get_measures(count, id = marker,
                                    area ~ area)
      }

      morpho = data.frame(area = sum(calc$area))
      morpho$img_name = im_name
      morpho$marker_id = marker
      dat = rbind(dat, morpho)
    }
  }

  if(!is.na(outdir)) {
    write.csv(dat, file = outdir, row.names = F)
  }

  return(dat)
}
