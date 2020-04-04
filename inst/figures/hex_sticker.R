library(hexSticker)
imgurl <- paste0("https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/",
                 "Tools-spanner-hammer.svg/",
                 "2000px-Tools-spanner-hammer.svg.png")
sticker(imgurl, package="CIDAtools",
        s_x = 1,
        s_y = 1,
        s_width = 0.75,
        p_y = 1,
        p_size = 9.25,
        h_fill = '#CFB87C',
        h_color = "#000000",
        p_color = "#000000",
        filename="inst/figures/CIDAtoolshex.png",
        dpi = 1500)
