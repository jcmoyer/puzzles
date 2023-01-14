module Day23

include("sf.jl")
using .Supafast

@enum Opcode hlf tpl inc jmp jie jio

struct Instruction
    op::Opcode
    param1::Int
    param2::Int
end

reg2index(s) = s == "a" ? 1 : 2

mutable struct Cpu
    ip::Int
    reg::Vector{Int}
end
Cpu() = Cpu(1, Int[0, 0])

function interpret(cpu::Cpu, prog)
    while cpu.ip <= length(prog)
        instr = prog[cpu.ip]
        if instr.op == hlf
            cpu.reg[instr.param1] ÷= 2
            cpu.ip += 1
        elseif instr.op == tpl
            cpu.reg[instr.param1] *= 3
            cpu.ip += 1
        elseif instr.op == inc
            cpu.reg[instr.param1] += 1
            cpu.ip += 1
        elseif instr.op == jmp
            cpu.ip += instr.param1
        elseif instr.op == jie
            if cpu.reg[instr.param1] % 2 == 0
                cpu.ip += instr.param2
            else
                cpu.ip += 1
            end
        elseif instr.op == jio
            if cpu.reg[instr.param1] == 1
                cpu.ip += instr.param2
            else
                cpu.ip += 1
            end
        else
            error("unhandled opcode $(instr.op)")
        end
    end
end

function solve(text::AbstractString)
    program = Instruction[]

    matchlines(
        text,
        r"hlf (a|b)" => r -> push!(program, Instruction(hlf, reg2index(r), 0)),
        r"tpl (a|b)" => r -> push!(program, Instruction(tpl, reg2index(r), 0)),
        r"inc (a|b)" => r -> push!(program, Instruction(inc, reg2index(r), 0)),
        r"jmp ([\+\-]\d+)" => offset -> push!(program, Instruction(jmp, parseint(offset), 0)),
        r"jie (a|b), ([\+\-]\d+)" => (r, offset) -> push!(program, Instruction(jie, reg2index(r), parseint(offset))),
        r"jio (a|b), ([\+\-]\d+)" => (r, offset) -> push!(program, Instruction(jio, reg2index(r), parseint(offset))),
    )

    display(program)

    cpu = Cpu()
    interpret(cpu, program)
    display(cpu)

    cpu = Cpu()
    cpu.reg[reg2index("a")] = 1
    interpret(cpu, program)
    display(cpu)
end

solve(getinput(2015, 23))

end
