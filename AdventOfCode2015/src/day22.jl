module Day22

using ..Supafast

abstract type Spell end
abstract type Effect <: Spell end

heal(::Spell) = 0
damage(::Spell) = 0

struct MagicMissile <: Spell end
manacost(::MagicMissile) = 53
damage(::MagicMissile) = 4

struct Drain <: Spell end
manacost(::Drain) = 73
damage(::Drain) = 2
heal(::Drain) = 2

struct Shield <: Effect end
manacost(::Shield) = 113
effecttime(::Shield) = 6

struct Poison <: Effect end
manacost(::Poison) = 173
effecttime(::Poison) = 6

struct Recharge <: Effect end
manacost(::Recharge) = 229
effecttime(::Recharge) = 5

struct Player
    hp::Int
    mp::Int
end

Player() = Player(50, 500)
canafford(p::Player, spell) = p.mp >= manacost(spell)

struct Boss
    hp::Int
    damage::Int
end

struct EffectTimers
    shield::Int
    poison::Int
    recharge::Int
end

EffectTimers() = EffectTimers(0, 0, 0)

active(t::EffectTimers, ::Shield) = t.shield > 0
active(t::EffectTimers, ::Poison) = t.poison > 0
active(t::EffectTimers, ::Recharge) = t.recharge > 0
activate(t::EffectTimers, ::Spell) = t
activate(t::EffectTimers, e::Shield) = EffectTimers(effecttime(e), t.poison, t.recharge)
activate(t::EffectTimers, e::Poison) = EffectTimers(t.shield, effecttime(e), t.recharge)
activate(t::EffectTimers, e::Recharge) = EffectTimers(t.shield, t.poison, effecttime(e))
tick(t::EffectTimers) = EffectTimers(t.shield - 1, t.poison - 1, t.recharge - 1)

struct State
    player::Player
    boss::Boss
    effects::EffectTimers

    mpspent::Int
    playerdef::Int
    stepid::Int
    hardmode::Bool
end

State(boss::Boss; hardmode) = State(Player(), boss, EffectTimers(), 0, 0, 0, hardmode)

finished(s::State) = s.player.hp <= 0 || s.boss.hp <= 0

function castspell(s::State, spell)
    State(
        Player(s.player.hp + heal(spell), s.player.mp - manacost(spell)),
        Boss(s.boss.hp - damage(spell), s.boss.damage),
        activate(s.effects, spell),
        s.mpspent + manacost(spell),
        s.playerdef,
        s.stepid + 1,
        s.hardmode
    )
end

function trycast!(buf, s::State, spell::Spell)
    canafford(s.player, spell) && push!(buf, castspell(s, spell))
end

function trycast!(buf, s::State, eff::Effect)
    !active(s.effects, eff) && canafford(s.player, eff) && push!(buf, castspell(s, eff))
end

function doeffects(s::State)
    mpregen = active(s.effects, Recharge()) ? 101 : 0
    defbuff = active(s.effects, Shield()) ? 7 : 0
    bossdegen = active(s.effects, Poison()) ? 3 : 0
    playerdegen = s.hardmode && s.stepid % 4 == 0 ? 1 : 0
    State(
        Player(s.player.hp - playerdegen, s.player.mp + mpregen),
        Boss(s.boss.hp - bossdegen, s.boss.damage),
        tick(s.effects),
        s.mpspent,
        defbuff,
        s.stepid + 1,
        s.hardmode,
    )
end

function doplayer(s::State)
    states = State[]
    trycast!(states, s, MagicMissile())
    trycast!(states, s, Drain())
    trycast!(states, s, Shield())
    trycast!(states, s, Poison())
    trycast!(states, s, Recharge())
    states
end

function doboss(s::State)
    State(
        Player(s.player.hp - max(s.boss.damage - s.playerdef, 1), s.player.mp),
        s.boss,
        s.effects,
        s.mpspent,
        s.playerdef,
        s.stepid + 1,
        s.hardmode,
    )
end

function adjacent(s::State)
    which = s.stepid % 4
    if which == 0
        return State[doeffects(s)]
    elseif which == 1
        return doplayer(s)
    elseif which == 2
        return State[doeffects(s)]
    elseif which == 3
        return State[doboss(s)]
    end
end

function findminmp(init::State)
    open = State[init]
    scores = Int[]
    while length(open) > 0
        next = pop!(open)
        for n in adjacent(next)
            if finished(n)
                n.player.hp > 0 && n.boss.hp <= 0 && push!(scores, n.mpspent)
            else
                push!(open, n)
            end
        end
    end
    return minimum(scores)
end

function solve(text::AbstractString)
    boss = Boss(0, 0)
    matchlines(
        text,
        r"Hit Points: (\d+)" => x -> boss = Boss(parseint(x), boss.damage),
        r"Damage: (\d+)" => x -> boss = Boss(boss.hp, parseint(x)),
    )
    return findminmp(State(boss; hardmode=false)), findminmp(State(boss; hardmode=true))
end

end
