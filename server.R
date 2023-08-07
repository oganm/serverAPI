library(ggplot2)
library(magick)
library(knitr)
library(magrittr)
library(import5eChar)
library(jsonlite)
library(digest)

# testPlumb.R

#* @apiTitle Ogan's API endpoints

dir.create('images',showWarnings = FALSE)

makeImage = function(text = 'Test',size = 60){
    if(nchar(text)>=1000){
        return('Must be shorter than 1000 charters')
    }
    img = image_read('white.png')
    img = image_annotate(img,gravity = 'center', size = size, boxcolor = 'gray98',text =text)
    img = image_trim(img)
    return(img)
}

#* Turn text into an image
#* @param size:int Font size
#* @param text:character text to write
#* @get /text2img
#* @serializer contentType list(type='image/png')
text2img = function(text='Test', size = 60){
    # file = tempfile(fileext = '.png')
    file = file.path('images',paste0(digest::sha1(paste(text,size)),'.png'))
    img = makeImage(text = text, size = size)
    image_write(img,file)
    on.exit(gc())
    base::readBin(file,'raw',n = file.info(file)$size)

}

#* Turn text into an image with less characters
#* @param s:int Font size
#* @param t:character text to write
#* @get /t2i
#* @serializer contentType list(type='image/png')
t2i = function(t='Test', s = 60){
    text2img(t, s)
}

#* Turn text into image, redirect to imgur
#* @param s:int Font size
#* @param t:character text to write
#* @get /t2img
t2img = function(res, t = 'Test', s = 60){
    # file = tempfile(fileext = '.png')
    on.exit(gc())
    file = file.path('images',paste0(digest::sha1(paste(t,s)),'.png'))
    img = makeImage(text = t, size = s)
    image_write(img,file)
    image = knitr::imgur_upload(file = file)
    image = attributes(image)
    res$setHeader(name = 'Location',image$XML$link[[1]])
    res$status = 301
}


#* @post /walterXML2iiJSON
#* @get /walterXML2iiJSON
#* @serializer contentType list(type='text/json')
walterXML2iiJSON = function(req){
    startLine = grep(pattern = '<?xml',req$postBody,fixed = TRUE)
    endLine = grep(pattern = '</character',req$postBody, fixed = TRUE)
    if(length(startLine)==1 && length(endLine)==1 && endLine > startLine){
        char = req$postBody[startLine:endLine] %>% paste(collapse = '\n')
        ogChar = import5eChar:::processCharacter(char)
        jsonOut = ogChar %>% improvedInitiativeJSON()
        return(jsonOut)
    } else{
        characterFile <- system.file("Tim_Fighter5", package = "import5eChar")
        jsonOut = import5eChar::importCharacter(file = characterFile) %>% improvedInitiativeJSON()
        return(jsonOut)
    }
}


#* @post /walterXML2ogJSON
#* @get /walterXML2ogJSON
#* @serializer contentType list(type='text/json')
walterXML2ogJSON = function(req){
    startLine = grep(pattern = '<?xml',req$postBody,fixed = TRUE)
    endLine = grep(pattern = '</character',req$postBody, fixed = TRUE)
    if(length(startLine)==1 && length(endLine)==1 && endLine > startLine){
        char = req$postBody[startLine:endLine] %>% paste(collapse = '\n')
        ogChar = import5eChar:::processCharacter(char)
        jsonOut = ogChar %>% toJSON(pretty = TRUE,keep_vec_names = TRUE)
        return(jsonOut)
    } else{
        characterFile <- system.file("Tim_Fighter5", package = "import5eChar")
        jsonOut = import5eChar::importCharacter(file = characterFile) %>% toJSON(pretty = TRUE, keep_vec_names= TRUE)
        return(jsonOut)
    }
}

#* @post /walterXML2ogPDF
#* @get /walterXML2ogPDF
#* @serializer contentType list(type='application/pdf')
walterXML2ogPDF = function(req){
    file = tempfile(fileext = '.pdf')
    startLine = grep(pattern = '<?xml',req$postBody,fixed = TRUE)
    endLine = grep(pattern = '</character',req$postBody, fixed = TRUE)
    if(length(startLine)==1 && length(endLine)==1 && endLine > startLine){
        char = req$postBody[startLine:endLine] %>% paste(collapse = '\n')
        ogChar = import5eChar:::processCharacter(char)
        ogChar %>% prettyPDF(file = file)
    } else{
        characterFile <- system.file("Tim_Fighter5", package = "import5eChar")
        import5eChar::importCharacter(file = characterFile) %>% prettyPDF(file = file)
    }
    base::readBin(file,'raw',n = file.info(file)$size)
}


#* Create a card based on a pdf template
#* @param name:character Name of the card
#* @param description:character card description
#* @param imgur:logical set true to send to imgur
#* @get /text2cardpdf
#* @serializer contentType list(type='image/png')
text2cardpdf = function(res , name = 'name', description = 'description',imgur = TRUE){
    # magick package used to have a memory leak. this addresses that. 
    # i am unsure if it's been fixed since
    on.exit(gc())
    
    # read the fields of the pdf and fill them in a temporary file
    fields = staplr::get_fields('card template.pdf')
    fields$CardName$value = name
    fields$Description$value = description
    
    # fill the pdf form and save it
    tmp_pdf = tempfile(fileext = '.pdf')
    staplr::set_fields('card template.pdf',tmp_pdf,fields)
    
    # read the pdf and save it as an image
    img = magick::image_read_pdf(tmp_pdf)
    tmp_img = tempfile(fileext = '.png')
    magick::image_write(img, tmp_img)
    
    
    # either save to imgur and redirect or serve the image
    if(imgur){
        image = knitr::imgur_upload(file = tmp_img)
        image = attributes(image)
        res$setHeader(name = 'Location',image$XML$link[[1]])
        res$status = 301
    } else{
        base::readBin(tmp_img,'raw',n = file.info(tmp_img)$size)
    }
    
}


#* Create a card based on a pdf template
#* @param name:character Name of the card
#* @param description:character card description
#* @param imgur:logical set true to send to imgur
#* @get /text2cardpng
#* @serializer contentType list(type='image/png')
text2cardpng = function(res , name = 'name', description = 'description',imgur = TRUE){
    # magick package used to have a memory leak. this addresses that. 
    # i am unsure if it's been fixed since
    on.exit(gc())
    
    
    
    img = magick::image_read('card template.png')
    
    # write the text on the specific locations of the image
    img = magick::image_annotate(img,location = '+0+0', size = 120,text =name)
    img = magick::image_annotate(img,location = '+0+630', size = 90,text =description)
    
    # save the image to a file
    tmp_img = tempfile(fileext = '.png')
    magick::image_write(img, tmp_img)
    
    
    # either save to imgur and redirect or serve the image
    if(imgur){
        image = knitr::imgur_upload(file = tmp_img)
        image = attributes(image)
        res$setHeader(name = 'Location',image$XML$link[[1]])
        res$status = 301
    } else{
        base::readBin(tmp_img,'raw',n = file.info(tmp_img)$size)
    }    
}
