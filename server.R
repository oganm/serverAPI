library(ggplot2)
library(magick)
# testPlumb.R


#* @get /text2img
#* @serializer contentType list(type='image/png')
text2img = function(text='Test'){
    if(nchar(text)>=1000){
        return('Must be shorter than 1000 charters')
    }
    img = image_read('white.png')
    file = tempfile(fileext = '.png')
    img = image_annotate(img,gravity = 'center', size = 60, boxcolor = 'gray98',text =text)
    img = image_trim(img)
    
    image_write(img,file)
    readBin(file,'raw',n = file.info(file)$size)
    
}


#* @get /t2i
#* @serializer contentType list(type='image/png')
t2i = function(t='Test'){
    text2img(t)
}