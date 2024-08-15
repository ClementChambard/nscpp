LEFT_ASSOC = 0
RIGHT_ASSOC = 1
NOT_ASSOC = 2


def op_add(left, right):
    return left + right

def op_sub(left, right):
    return left - right

def op_eq(left, right):
    return left == right

def op_and(left, right):
    return left and right

def op_not(left):
    return not left

# TODO: All operators (find a way to make unary '-' work)

OPERATORS = [
    (NOT_ASSOC, [("!", 1, op_not)]),
    (LEFT_ASSOC, [("+", 2, op_add), ("-", 2, op_sub)]),
    (LEFT_ASSOC, [("==", 2, op_eq)]),
    (LEFT_ASSOC, [("&&", 2, op_and)]),
]


def find_op(s):
    for i, (a, l) in enumerate(OPERATORS):
        for S, n, o in l:
            if S == s:
                return (i, a, n, o)
    print("Unknown operator '{}'".format(s))
    exit(1)


def op_eval(tokens):
    num_stack = []
    op_stack = []
    for t in tokens:
        if type(t) != str:
            num_stack.append(t)
            continue
        prio, assoc, n, op = find_op(t)
        while len(op_stack) != 0:
            l_prio, l_assoc, l_n, l_op = op_stack[-1]
            if l_prio > prio or (l_prio == prio and l_assoc == RIGHT_ASSOC):
                break
            op_stack = op_stack[:-1]
            if l_n == 1:
                assert len(num_stack) > 0
                num = num_stack[-1]
                num_stack.pop()
                num_stack.append(l_op(num))
            elif l_n == 2:
                assert len(num_stack) > 1
                num1 = num_stack[-2]
                num2 = num_stack[-1]
                num_stack.pop()
                num_stack.pop()
                num_stack.append(l_op(num1, num2))
        op_stack.append((prio, assoc, n, op))
    while len(op_stack) != 0:
        l_prio, l_assoc, l_n, l_op = op_stack[-1]
        op_stack = op_stack[:-1]
        if l_n == 1:
            assert len(num_stack) > 0
            num = num_stack[-1]
            num_stack.pop()
            num_stack.append(l_op(num))
        elif l_n == 2:
            assert len(num_stack) > 1
            num1 = num_stack[-2]
            num2 = num_stack[-1]
            num_stack.pop()
            num_stack.pop()
            num_stack.append(l_op(num1, num2))
    assert len(num_stack) == 1
    return num_stack[0]








