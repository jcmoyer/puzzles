module Day07

include("sf.jl")
using .Supafast

isconst(s) = all(isdigit(c) for c in s)
isreg(s) = !isconst(s)

# Yes. This is overengineered.

abstract type Op end

struct OpConst <: Op
    val::UInt16
end

eval(op::OpConst, ::Dict, ::Dict) = op.val

struct OpName <: Op
    sym::Symbol
end

function eval(op::OpName, assigns::Dict, env::Dict)
    if op.sym in keys(env)
        # already assigned
        return env[op.sym]
    else
        # otherwise we have to perform the assignment
        return eval(assigns[op.sym], assigns, env)
    end
end

struct OpAssign <: Op
    name::OpName
    val::Op
end

function eval(op::OpAssign, assigns::Dict, env::Dict)
    env[op.name.sym] = eval(op.val, assigns, env)
    return env[op.name.sym]
end

struct OpOr <: Op
    left::Op
    right::Op
end

eval(op::OpOr, ops::Dict, env::Dict) = eval(op.left, ops, env) | eval(op.right, ops, env)

struct OpAnd <: Op
    left::Op
    right::Op
end

eval(op::OpAnd, ops::Dict, env::Dict) = eval(op.left, ops, env) & eval(op.right, ops, env)

struct OpLShift <: Op
    left::Op
    right::Op
end

eval(op::OpLShift, ops::Dict, env::Dict) = eval(op.left, ops, env) << eval(op.right, ops, env)

struct OpRShift <: Op
    left::Op
    right::Op
end

eval(op::OpRShift, ops::Dict, env::Dict) = eval(op.left, ops, env) >> eval(op.right, ops, env)

struct OpNot <: Op
    child::Op
end

eval(op::OpNot, ops::Dict, env::Dict) = ~eval(op.child, ops, env)

function parseatom(s)
    if isconst(s)
        OpConst(parse(UInt16, s))
    else
        OpName(Symbol(s))
    end
end

const binops = Dict("AND" => OpAnd, "OR" => OpOr, "LSHIFT" => OpLShift, "RSHIFT" => OpRShift)

function parseop(s)
    # binop
    m = match(r"([\w\d]+) (\w+) ([\w\d]+)", s)
    if m !== nothing
        a, op, b = m
        return binops[op](parseatom(a), parseatom(b))
    else
        # unop
        m = match(r"(\w+) ([\w\d]+)", s)
        if m !== nothing
            op, a = m
            if op == "NOT"
                return OpNot(parseatom(a))
            else
                error("unrecognized unary op: $op")
            end
        else
            # not a binop or unop
            return parseatom(s)
        end
    end
end

function solve(text)
    ops = Dict()
    env = Dict()

    for line in eachline(IOBuffer(text))
        opbody, namestr = split(line, " -> ")
        name = OpName(Symbol(namestr))
        ops[name.sym] = OpAssign(name, parseop(opbody))
    end

    p1 = eval(OpName(:a), ops, env)
    empty!(env)
    ops[:b] = OpConst(p1)
    p2 = eval(OpName(:a), ops, env)

    println(p1)
    println(p2)
end

solve(getinput(2015, 7))

end
