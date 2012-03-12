stack = [] 

def evaluate(exp):
    current = ""
    for c in exp:
        if c == "(":
            if current != "":
                stack.append(current)
                current = ""
        elif c == ")":
            v2 = current
            v1 = stack.pop()
            op = stack.pop()
            current = apply(op, v1, v2)
        elif c == " ":
            stack.append(current)
            current = ""
        else:
            current += c
    return current

def apply(op, v1, v2):
    if op == "+":
        return str(int(v1) + int(v2))
    elif op == "-":
        return str(int(v1) - int(v2))
