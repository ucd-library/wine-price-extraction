isPrice =
function(x, ...)
   grepl("(^\\$?[0-9]+[.,][0-9]+$|^\\$[0-9]+$)", x, ...)    

GetBinaryBoxes =
function(f, threshold = 220)
{
    pb = thresholdImage(f, threshold)
    bbb = GetBoxes(pb)    
}

thresholdImage =
function(f, threshold = 220)    
{
    pix = pixRead(f)
    p8 = pixConvertTo8(pix)    
    pb = pixThresholdToBinary(p8, threshold)
}

isColorChecker =
function(box, text = box$text)
{
    all(c("Color", "Rendition") %in% text) & any(grepl("Checker|Macbeth", text) ) ||
        all(c("Macbeth", "ColorChecker") %in% text)
}


isNumber =
function(x)
{
  grepl("^([0-9]+)([.,][0-9]+)$", x)
}


`[[.PageResults` =
function(x, i, ...)
{
    if(is.character(i) && grepl("^[0-9]+$", i))
        i = sprintf("UCD_Lehmann_%s.jpg", i)

    NextMethod()
}

wrong = c("]", "{", "}", "§", "@", "°", "©", "®", "<", "=", ">", "|", "~", "¢", "£", "¥")
if(FALSE) {
    col = sapply(full, isColorChecker)
    tt = table(unlist(lapply(full[color], `[[`, "text")))
    library(Aspell)    
    ok = aspell(names(tt))
    spelledWrong = names(tt)[!ok]
}   

spelledWrong =  c("_—", "-_—", "-_—.", "-—", "-\"", "—_", "——", 
"——l", "—.", "—~", "._", "..", "\"S\\N", "\"x.", "(a.", 
"(AY", ")!", "[']", "[w)", "§F", "\\'", "©:", "©»", "®;", 
"<~)", "<3", "==", "=e", "=f", "=n", "|g", "|H", "~@", "~~", 
"~~.", "~N", "£1", "£14", "£2", "£8", "00", "2.", "2]", "2%:", 
"23", "27", "28", "2h", "3F", "3H", "41%", "4183", "48", "51", 
"52", "53", "7)", "a!", "a.", "A.", "a)", "a]", "aa", "AACE", 
"acbeth", "adir", "ae", "Ae", "AE", "AGEE", "ai", "Aig", "aii", 
"Aim\"", "al", "Ani", "AO", "aoe", "ar", "ARoSUE", "Ars", "aS", 
"ASSL", "ay", "AY", "az", "Bae", "BER", "BERS,", "bes", "Bln", 
"bo", "Bo", "br", "BRNEREEE", "bs", "BT", "BW", "cbe", "CEH", 
"CEmpate", "ColorChecker™", "CY", "Da", "DDN", "Ea.", "eae", 
"EAH", "Ean", "ee", "Ee", "EE", "eed", "EEE", "Eh:", "ei", "Ei", 
"ein", "EK", "el", "El", "ely", "en]", "EO", "EQ", "eR", "erent", 
"eS", "ESSA", "et", "ey", "EY", "fai", "fe", "Fh", "FH", "fhe", 
"Fi", "FI", "Fifi", "Fiz", "fo", "Fo", "Fs", "FSR", "Fy", "GC", 
"gg]", "gh", "gl", "gr;", "Hd", "hea", "Hebi", "HERES", "hg", 
"hh", "HH", "i,", "I~", "i232", "iat]", "ie", "ig", "ine", "isi", 
"it)", "J;", "Jb", "je", "Jian", "Kh", "le", "Lior", "lL", "Lm", 
"m—", "m=", "maa", "meee", "ml)", "mr", "N\\", "ne", "Ng", 
"nl", "NL", "nn", "Nn", "NN", "nr", "nS", "NWN", "ny", "Ny", 
"o-", "o\\®", "o\\d", "oa", "od", "ol®1®", "olorChecker", 
"ON)", "oo", "oO", "oS", "ot", "ou", "OU?", "pe", "Pe", "Pes", 
"plese", "PN", "Poa", "pone,", "pS", "py", "QO", "QP", "QS", 
"R—", "Raa", "re]", "REE", "REN", "rg", "Rh.", "Rl", "rn", 
"Roa", "Rr", "rT", "ry", "S\\=\\te", "sa", "Sd", "SER!", "Shera", 
"Sih", "So.", "srelel®", "SRG", "SRN", "ss", "Ss", "th", "Tk", 
"™~", "TT", "TW", "ven", "vo", "vO", "Vo", "Vy", "wa)", "wd", 
"wip", "wn", "wo", "X-", "x)", "XR", "Xt", "y-", "z=", "zl", 
"Zo")

wrong = c(wrong, spelledWrong)





assembleRow =
function(x)
{
    bin = setdiff(unique(x[[1]]), "")
    
    #    low = toupper(x[[2]]) != x[[2]]
    desc = x[[2]] [ x[[2]] != "" ]
    low =  pctUpperCase(desc) < .7 
    desc = paste(desc[!low], collapse = " ")

    prices = lapply(x[-(1:2)], function(x) { w = isPrice(x); if(sum(w) == 0) NA else x[w]})
    as.data.frame(c(bin = bin, desc = desc, prices))
}

pctUpperCase =
function(x)
{
  sapply(strsplit(x, ""), function(x) sum(toupper(x) == x)/length(x))
}
