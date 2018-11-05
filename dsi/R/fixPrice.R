fixPrice =
    #
    # fixPrice("249")
    # fixPrice("12,49")
function(x)
{
    x = gsub(",", ".", x)
    gsub("^([0-9]+)([0-9]{2})$", "\\1.\\2", x)
}
