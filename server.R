library(ggplot2)
library(magick)
library(knitr)
# testPlumb.R


makeImage = function(text = 'Test',size = 60){
    if(nchar(text)>=1000){
        return('Must be shorter than 1000 charters')
    }
    img = image_read('white.png')
    img = image_annotate(img,gravity = 'center', size = size, boxcolor = 'gray98',text =text)
    img = image_trim(img)
    return(img)
}

#* @get /text2img
#* @serializer contentType list(type='image/png')
text2img = function(text='Test'){
    file = tempfile(fileext = '.png')
    img = makeImage(text = text)
    image_write(img,file)
    base::readBin(file,'raw',n = file.info(file)$size)
    
}


#* @get /t2i
#* @serializer contentType list(type='image/png')
t2i = function(t='Test'){
    text2img(t)
}

#* @get /t2img
t2img = function(res, t = 'Test'){
    file = tempfile(fileext = '.png')
    img = makeImage(text = t)
    image_write(img,file)
    image = knitr::imgur_upload(file = file)
    image = attributes(image)
    res$setHeader(name = 'Location',image$XML$data$link[[1]])
    res$status = 301
}
