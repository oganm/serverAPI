library(ggplot2)
library(magick)
library(knitr)
library(magrittr)
library(import5eChar)
library(jsonlite)
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
text2img = function(text='Test', size = 60){
    file = tempfile(fileext = '.png')
    img = makeImage(text = text, size = size)
    image_write(img,file)
    base::readBin(file,'raw',n = file.info(file)$size)
    
}


#* @get /t2i
#* @serializer contentType list(type='image/png')
t2i = function(t='Test', s = 60){
    text2img(t, s)
}

#* @get /t2img
t2img = function(res, t = 'Test', s = 60){
    file = tempfile(fileext = '.png')
    img = makeImage(text = t, size = s)
    image_write(img,file)
    image = knitr::imgur_upload(file = file)
    image = attributes(image)
    res$setHeader(name = 'Location',image$XML$data$link[[1]])
    res$status = 301
}


#* @post /walterXML2iiJSON
#* @serializer contentType list(type='text/json')
walterXML2iiJSON = function(req){
    startLine = grep(pattern = '<?xml',req$postBody,fixed = TRUE)
    endLine = grep(pattern = '</character',req$postBody, fixed = TRUE)
    char = req$postBody[startLine:endLine] %>% paste(collapse = '\n')
    ogChar = import5eChar:::processCharacter(char)
    ogChar %>% improvedInitiativeJSON()
}


#* @post /walterXML2ogJSON
#* @serializer contentType list(type='text/json')
walterXML2ogJSON = function(req){
    startLine = grep(pattern = '<?xml',req$postBody,fixed = TRUE)
    endLine = grep(pattern = '</character',req$postBody, fixed = TRUE)
    char = req$postBody[startLine:endLine] %>% paste(collapse = '\n')
    ogChar = import5eChar:::processCharacter(char)
    ogChar %>% toJSON(pretty = TRUE)
}