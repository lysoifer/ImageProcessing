#' canopy_cover_ml
#' Wrapper function for canopy cover calculation using imageseg package, a machine learning method of canopy cover analysis
#'
#' @param imageDir directory to images to analyze
#' @param outDir_resized directory to resized images
#' @param outDir_binary directory to binarized images
#' @param canopy_model filepath to canopy model downloaded from imageseg (work on integrating this into the package so download isn't necessary)
#'
#' @details
#' Follow instructions in imageseg documentation to ensure keras is properly installed. Canopy model should be downloaded from imageseg and saved to be provided to the function
#'
#'
#' @export

canopy_cover_ml = function(imageDir, outDir_resized, outDir_binary, canopy_model) {

  check = tf$constant("Hello Tensorflow")
  if(class(check)[1] != "tensorflow.tensor") {
    stop("Tensorflow is not installed properly. Please follow instructions in imageseg pacakge documentation to install keras and tensorflow")
  }

  resizeImages(imageDir = imageDir,
               type = "canopy",
               outDir = outDir)

  images = loadImages(imageDir = outDir)

  # convert images to arrays for keras
  x = imagesToKerasInput(images)

  # load the pre-trained model
  model_file = canopy_model
  model = loadModel(modelFile = model_file)

  results = imageSegmentation(model = model, x = x)

  fnames = sapply(strsplit(results$summary[,1], split = "/"), "[[", 4)
  fnames = substr(fnames, 1, nchar(fname)-4)
  for(i in 1:nrow) {
    img_result = image_append(c(results$image[i], results$prediction_binary[i]))
    image_write(img_result, paste0(outDir_binary, fnames[i], ".png"))
  }

  results$summary$img_name = fnames

  return(results)
}

#imageDir = "examples/data/orig_imgs/"
#outDir_resized = "examples/output/imageseg_resized/"
#outDir_binary = "examples/output/imageseg_binary/"
#canopy_model = "./../../../field_work/summer_2023/panama_epiphytes/imageseg_canopy_model/imageseg_canopy_model.hdf5" # path to pre-trained canopy model

