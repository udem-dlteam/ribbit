import sys

names = {}

reserved = ["exch","def","length","add","sub","array","put",
            "putinterval","get","getinterval","ne","eq","false","true","exit",
            "if","ifelse","mul","ge","le","and","gt","lt","or","string","flush",
            "print","forall","repeat","type","div","cvi","stop","loop","idiv","exec",
            "stopped","\{\}","for","file","read","not","null", "/arraytype"]

alphabet = [] 
literal = False
output_stream = "%!PS\n"

for i in range(26) : # make alphabet
    alphabet.append(chr(65+i))
    alphabet.append(chr(97+i))

def make_new_name(var_name):
    index = len(names)
    name = ''
    while index >= 0:
        name = alphabet[index % 52] + name
        index = index // 52 - 1
    names[var_name] = name

def isnumber(num):
    try :
        val = int(num)
    except :
        return False
    return True

def verify(word):
    global output_stream
    global literal
    
    if(literal):
        if(word[-1] == ")"):
            literal = False
            output_stream += word + " "
            return
        else:
            output_stream += word + " "
            return
    elif word in reserved : 
        output_stream += (word + " ")
    elif word[0] == "/" :
        if word[1:] not in names :
            make_new_name(word[1:])
        output_stream += ("/" + names[word[1:]] + " ")
    elif word[0] == "(" :
        if(word[-1] != ")"):
            literal = True
        output_stream += word  + " "
    elif word[0] in ["[","{","-"]:
        output_stream += (word[0])
        if len(word) > 1 :
            verify(word[1:])
    elif isnumber(word):
        output_stream += (word + " ")
    elif word in names.keys() :
        output_stream += (names[word] + " ")
    elif word[-1] in ["]","}"]:
        if len(word) > 1 :
            verify(word[:-1])
        if output_stream[-1] == " " :
            output_stream = output_stream[:-1] 
        output_stream += word[-1]
    else :

        print("error")
        exit()
        
   

if __name__=="__main__":        
    input_stream = sys.stdin.read()

    for line in input_stream.splitlines() :
        for word in line.split() :
            if word[0] == '%' :
                break
            verify(word)
            
    print(output_stream)
              
