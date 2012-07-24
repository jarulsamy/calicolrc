## assumes all strings end in \0

## match: search for regexp anywhere in text ##
def match(regexp, text):
    regexp += "\0"
    text += "\0"
    if (regexp[0] == '^'):
        return match_here(regexp[1:], text)
    while text:
        ## must look even if string is empty ##
        if (match_here(regexp, text)):
            return True
        text = text[1:]
    return False

## match_here: search for regexp at beginning of text ##
def match_here(regexp, text):
  if (regexp[0] == '\0'):
    return True
  if (regexp[1] == '*'):
    return match_star(regexp[0], regexp[2:], text)
  if (regexp[0] == '$' and regexp[1] == '\0'):
    return (text == '\0')
  if (text != '\0' and (regexp[0] == '.' or regexp[0] == text[0])):
    return match_here(regexp[1:], text[1:])
  return False

## match_star: search for c*regexp at beginning of text ##
def match_star(c, regexp, text):
    do = True
    pos = 0
    while do:
        ## a * matches zero or more instances ##
        if (match_here(regexp, text[pos:])):
            return True
        do = (text[pos] != '\0' and (text[pos] == c or c == '.'))
        pos += 1
    return False

