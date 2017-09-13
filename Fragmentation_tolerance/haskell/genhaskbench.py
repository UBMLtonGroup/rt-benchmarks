def genCode():
    code = ("import Data.Array\nmain = do\n        let ")
    for i in range(0,194280):
        variable = "a"+str(i)
        decl = variable+" = array (1, 1) [(1, "+str(i)+")]"
        code  = code + decl + "\n            " #8+4 spaces let bindings need to be aligned with spaces
    code = code.rstrip()
    code = code + "\n        print (\"Done\")" #8 spaces align with let
    fi = open ("fragger3.hs","w")
    fi.write(code)
    fi.close()

def main():
    genCode()

main()        
