def eliminarTexto(texto:str)->list[str]:
    numerosOnus:list[str]=[]
    for i,j in texto:
        if i == " ":
            while j != " ":
                numerosOnus.append(texto[i])
    return numerosOnus
t = "faohj 2113189 goael 3332312 reviion"
#print(eliminarTexto(t))


def eliminarTexto(texto:str)->list[str]:
    numerosOnus:list[str]=[]
    numerosTexto= ["1","2","3","4","5","6","7","8","9","0"]
    letras = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","Ñ","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","ñ","o","p","q","r","s","t","u","v","w","x","y","z"]
    for i in range(len(texto)):
        if texto[i]+texto[i+1]+texto[i+2]+texto[i+3] == "2841":
            while texto[i] in numerosTexto or texto[i] in letras:
                numerosOnus.append(texto[i])
                i+=1
                texto.replace(texto[i],"")

        elif texto[i]+texto[i+1]+texto[i+2]+texto[i+3] == "5443":
            while texto[i] in numerosTexto or texto[i] in letras:
                numerosOnus.append(texto[i])
                i+=1

        elif texto[i]+texto[i+1]+texto[i+2]+texto[i+3] == "A031":
            while texto[i] in numerosTexto or texto[i] in letras:
                numerosOnus.append(texto[i])
                i+=1
                
    return numerosOnus
txt = """Reempaque	2841ECAB975D	→ Revision Tecnica
	Reempaque	2841ECABA79D"""

def eliminaPalabras(texto:str)->list[str]:
    palabrasEliminadas:list[str]=[]
    for caracter in texto:
        if caracter 

