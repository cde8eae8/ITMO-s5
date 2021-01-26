//
// Created by nikita on 1/24/21.
//

#include <algorithm>
#include <memory>
#include <numeric>
#include <utility>

#include "parsing_table_generator.h"
#include "debug/debug.h"
#include "parsing_rule_structures.h"
#include "code_generator.h"


std::string name(TerminalGrammarSymbol const &g) {
    return std::visit([](auto &v) { return v.name(); }, g);
}


const TerminalGrammarSymbol EpsilonTerminal = TerminalGrammarSymbol{Epsilon{}};
const TerminalGrammarSymbol EndOfTextTerminal = TerminalGrammarSymbol{EndOfText{}};

struct ParserItem {
public:
    NonterminalGrammarSymbol head;
    const Rule *rule;
    size_t pos;
    TerminalGrammarSymbol LL1Next;

    [[nodiscard]] GrammarSymbol nextSymbol() const {
        return rule->body[pos];
    }

    [[nodiscard]] bool atEnd() const {
        return pos == rule->body.size();
    }
};

struct HashLR1 {
    std::size_t operator()(ParserItem const &item) const noexcept {
        return std::hash<const NonterminalDescription *>{}(item.head.internal()) ^
               std::hash<const Rule *>{}(item.rule) ^
               std::hash<TerminalGrammarSymbol>{}(item.LL1Next) ^
               std::hash<size_t>{}(item.pos);
    }
};

struct HashLALR {
    std::size_t operator()(ParserItem const &item) const noexcept {
        return std::hash<const NonterminalDescription *>{}(item.head.internal()) ^
               std::hash<const Rule *>{}(item.rule) ^
               std::hash<size_t>{}(item.pos);
    }
};

bool eqLR1(ParserItem const &li, ParserItem const &ri) noexcept {
    return li.pos == ri.pos && li.head.name() == ri.head.name() && li.LL1Next == ri.LL1Next &&
           li.rule == ri.rule;
}

bool eqLALR(ParserItem const &li, ParserItem const &ri) noexcept {
    return li.pos == ri.pos && li.head.name() == ri.head.name() && li.rule == ri.rule;
}

struct EqLALR {
    bool operator()(ParserItem const &lhs, ParserItem const &rhs) const {
        return eqLALR(lhs, rhs);
    }
};

struct Kernel {
    std::vector<ParserItem> items;
    mutable std::unordered_map<GrammarSymbol, const Kernel *> gotoFunction;
    // std::vector<Kernel*> terminalsGoto;
};

struct LALRKernel;

std::ostream &operator<<(std::ostream &out, Kernel const &kernel);

std::ostream &operator<<(std::ostream &out, LALRKernel const &kernel);

std::ostream &operator<<(std::ostream &out, ParserItem const &item);


std::ostream &operator<<(std::ostream &out, const Rule &rule) {
    for (GrammarSymbol const &g : rule.body) {
        out << g << std::string(" ");
    }
    return out;
}


std::ostream &operator<<(std::ostream &out, const NonterminalDescription &nonterminal) {
    out << nonterminal.name << std::string(" -> \n");
    for (Rule const &rule : nonterminal.rules) {
        out << std::string("\t") << rule << std::string("\n");
    }
    return out;
}

std::ostream &operator<<(std::ostream &out, const NonterminalGrammarSymbol &nonterminal) {
    out << nonterminal.name();
    return out;
}

std::ostream &operator<<(std::ostream &out, const TerminalGrammarSymbol &terminal) {
//    std::string name = std::visit([](auto const &t) { return t.name(); }, terminal);
    std::string name = std::visit(overloaded{
            [](UserDefinedTerminalGrammarSymbol const &t) { return t.name(); },
            [](EndOfText const &t) { return t.name(); /*std::string("EndOfTextTerminal{}");*/ },
            [](Epsilon const &t) { return t.name(); /*std::string("$@");*/ },
    }, terminal);
    out << name;
    return out;
}

std::ostream &operator<<(std::ostream &out, GrammarSymbol const &sym) {
    std::visit([&](auto const &v) { out << v; }, sym);
    return out;
}

Rule parseRule(const std::vector<std::string> &tokens,
               std::unordered_map<std::string, NonterminalDescription> const &descriptions) {
    Rule rule{nullptr, SIZE_MAX, {}};
    if (tokens.empty()) {
        return Rule({nullptr, SIZE_MAX, {Epsilon{}}});
    }
    for (std::string const &token : tokens) {
        if (isupper(token.front())) {
            rule.body.emplace_back(NonterminalGrammarSymbol{&descriptions.at(token)});
        } else {
            rule.body.emplace_back(UserDefinedTerminalGrammarSymbol{token});
        }
    }
    return rule;
}


std::pair<size_t, ParsingTable> generateParsingTable(std::unordered_set<GrammarSymbol> const &grammar);

void convertGrammar(std::string const& file,
                    std::string const& header,
                    std::unordered_map<std::string, std::vector<RawRule>> const& rawGrammar,
                    std::map<std::string, std::string> const& types) {
    std::unordered_map<std::string, NonterminalDescription> descriptions;

    size_t i = 0;
    for (auto const&[name, rules] : rawGrammar) {
        descriptions.insert(
                std::make_pair(name, NonterminalDescription{i++, name, {}, {}}));
    }

    size_t ruleId = 0;
    std::unordered_set<GrammarSymbol> symbols;
    for (auto const&[name, rules] : rawGrammar) {
        auto descriptionIt = descriptions.find(name);
        symbols.insert(NonterminalGrammarSymbol{&descriptionIt->second});
        descriptionIt->second.rules.resize(rules.size());
        std::transform(rules.begin(), rules.end(), descriptionIt->second.rules.begin(),
                       [&](RawRule const &rule) {
                           Rule r = parseRule(rule.body, descriptions);
                           r.id = ruleId++;
                           r.description = &descriptionIt->second;
                           r.code = rule.code;
                           return r;
                       });
        for (Rule const &rule : descriptionIt->second.rules) {
            symbols.insert(rule.body.begin(), rule.body.end());
        }
    }
    std::cout << symbols << std::endl;

    // TODO: check epsilon rules
    for (auto const &nonterminal : descriptions) {
        std::cout << nonterminal.second << std::endl;
    }
    bool changed = true;
    while (changed) {
        changed = false;
        for (auto &[_, description] : descriptions) {
            for (Rule const &rule : description.rules) {
                size_t oldSize = description.firstSet.size();
                auto tokenIt = rule.body.begin();
                for (; tokenIt != rule.body.end(); ++tokenIt) {
                    bool continueRuleProcessing = false;
                    std::visit(overloaded{
                            [&, description = std::ref(description)](NonterminalGrammarSymbol const &nonterminal) {
                                continueRuleProcessing = nonterminal.firstSet().contains(EpsilonTerminal);
                                bool descriptionContainsEps = description.get().firstSet.contains(EpsilonTerminal);
                                description.get().firstSet.insert(nonterminal.firstSet().begin(),
                                                                  nonterminal.firstSet().end());
                                if (!descriptionContainsEps) description.get().firstSet.erase(EpsilonTerminal);
                            },
                            [&, description = std::ref(description)](TerminalGrammarSymbol const &terminal) {
                                description.get().firstSet.insert(terminal);
                            }
                    }, *tokenIt);
                    changed = changed || (oldSize != description.firstSet.size());
                    if (!continueRuleProcessing) break;
                }
                if (tokenIt == rule.body.end()) {
                    description.firstSet.insert(EpsilonTerminal);
                }
            }
        }
    }

    for (auto const &[_, nonterminal] : descriptions) {
        std::cout << nonterminal.name << " " << nonterminal.firstSet << std::endl;
    }

    auto[startIdx, table] = generateParsingTable(symbols);

    generateCode(file, header, table, descriptions, startIdx, types);
}

template<typename Iterator>
std::unordered_set<TerminalGrammarSymbol> first(Iterator begin, Iterator end, TerminalGrammarSymbol const &afterEnd) {
    std::unordered_set<TerminalGrammarSymbol> firstSet;
    bool stopProcessing = false;
    auto it = begin;
    for (; it != end && !stopProcessing; ++it) {
        GrammarSymbol const &symbol = *it;
        std::visit(overloaded{
                [&](NonterminalGrammarSymbol const &nonterminal) {
                    firstSet.insert(nonterminal.firstSet().begin(), nonterminal.firstSet().end());
                    firstSet.erase(EpsilonTerminal);
                    stopProcessing = !nonterminal.firstSet().contains(EpsilonTerminal);
                },
                [&](TerminalGrammarSymbol const &terminal) {
                    firstSet.insert(terminal);
                    stopProcessing = true;
                }
        }, symbol);
    }
    if (!stopProcessing) {
        firstSet.insert(afterEnd);
    }
    return firstSet;
}

void makeClosure(std::vector<ParserItem> &items) {
    bool changed = true;
    auto eq = [](auto const &l, auto const &r) { return eqLR1(l, r); };
    std::unordered_set<ParserItem, HashLR1, decltype(eq)> itemsSet(items.begin(), items.end(), 8, HashLR1{}, eq);
    std::unordered_set<ParserItem, HashLR1, decltype(eq)> added(8, HashLR1{}, eq);
    while (changed) {
        changed = false;
        for (ParserItem const &item : itemsSet) {
            if (item.pos == item.rule->body.size()) continue;
            GrammarSymbol sym = item.nextSymbol();
            if (!std::holds_alternative<NonterminalGrammarSymbol>(sym))
                continue;
            NonterminalGrammarSymbol closureBase = std::get<NonterminalGrammarSymbol>(sym);
            std::vector<Rule> const &rules = closureBase.rules();
            for (Rule const &rule : rules) {
                size_t nextTokenInRule = std::min(item.rule->body.size(), item.pos + 1);
                /* TODO: check here begin index */
                std::unordered_set<TerminalGrammarSymbol> const &firstSetsTmp =
                        first(item.rule->body.begin() + nextTokenInRule, item.rule->body.end(),
                              item.LL1Next);// closureBase.firstSet();
                for (TerminalGrammarSymbol const &terminal : firstSetsTmp) {
                    changed = true;
                    added.emplace(closureBase, &rule, 0, terminal);
                }
            }
        }
        size_t oldSize = itemsSet.size();
        itemsSet.insert(added.begin(), added.end());
        changed = itemsSet.size() != oldSize;
        added.clear();
    }
    items.clear();
    items.insert(items.end(), itemsSet.begin(), itemsSet.end());
    std::sort(items.begin(), items.end(), [](ParserItem const &lhs, ParserItem const &rhs) {
        if (lhs.pos != rhs.pos) return lhs.pos < rhs.pos;
        if (lhs.rule != rhs.rule) return lhs.rule < rhs.rule;
        return name(lhs.LL1Next) < name(rhs.LL1Next);
    });
}
// TODO: проверь не итерируешься ли ты где-то по контейнеру одновременно вставляя в него, особенно проверь это в FIRST

Kernel makeGoto(Kernel const &kernel, GrammarSymbol const &expectedSymbol) {
    Kernel newKernel{};
    for (ParserItem const &item : kernel.items) {
        if (item.atEnd()) continue;
        GrammarSymbol sym = item.nextSymbol();
        if (sym == expectedSymbol) {
            ParserItem shiftedItem = item;
            shiftedItem.pos++;
            if (shiftedItem.pos > shiftedItem.rule->body.size()) continue;
            newKernel.items.emplace_back(std::move(shiftedItem));
        }
    }
    makeClosure(newKernel.items);
    return newKernel;
}

struct LALRKernel {
    explicit LALRKernel(std::vector<ParserItem> const &items) : kernelItems(8, HashLALR{}, EqLALR{}), id{0} {
        for (ParserItem const &item : items) {
            kernelItems[item] = {};
        }
        addKernel(items);
    }

    void addKernel(std::vector<ParserItem> const &items) {
        for (ParserItem const &item : items) {
            kernelItems.at(item).emplace(item.LL1Next);
        }
    }

    std::unordered_map<ParserItem, std::unordered_set<TerminalGrammarSymbol>, HashLALR, EqLALR> kernelItems;
//    std::unordered_set<TerminalGrammarSymbol> continuation;
    std::unordered_map<GrammarSymbol, const LALRKernel *> gotoFunction;
    size_t id;
};

std::pair<size_t, ParsingTable> generateParsingTable(std::unordered_set<GrammarSymbol> const &grammar) {
    auto hashState = [](const Kernel *kern) {
        return std::accumulate(kern->items.begin(), kern->items.end(), 0, [](size_t headHash, ParserItem const &item) {
            return headHash ^ HashLR1{}(item);
        });
    };
    auto equalStates = [](const Kernel *lhs, const Kernel *rhs) {
        if (lhs->items.size() != rhs->items.size()) {
            return false;
        }
        for (size_t i = 0; i < lhs->items.size(); ++i) {
            if (!eqLR1(lhs->items[i], rhs->items[i])) return false;
        }
        return true;
    };
    std::unordered_set<const Kernel *, decltype(hashState), decltype(equalStates)> statesKernels(grammar.size(),
                                                                                                 hashState,
                                                                                                 equalStates);
    auto start = std::get<NonterminalGrammarSymbol>(
            *std::find_if(grammar.begin(), grammar.end(), [](GrammarSymbol const &s) {
                if (!std::holds_alternative<NonterminalGrammarSymbol>(s)) return false;
                std::cout << std::get<NonterminalGrammarSymbol>(s).name() << " ";
                return std::get<NonterminalGrammarSymbol>(s).name() == "S'";
            }));

    /*TODO:...*/
    Kernel st{{{ParserItem{start, &start.rules()[0], 0, EndOfTextTerminal}}},
              {}};
    makeClosure(st.items);
    std::vector<std::unique_ptr<Kernel>> kernelsStorage;
    kernelsStorage.push_back(std::make_unique<Kernel>(st));
    Kernel *startKernel = kernelsStorage.back().get();
    statesKernels.insert(kernelsStorage.front().get());

    bool changed = true;
//    std::unordered_set<Kernel *, decltype(hashState), decltype(equalStates)> addedSet(8, hashState, equalStates);
    std::vector<std::unique_ptr<Kernel>> addedStorage;

    while (changed) {
        changed = false;
        for (std::unique_ptr<Kernel> &state : kernelsStorage) {
            for (GrammarSymbol const &symbol : grammar) {
                if (state->gotoFunction.contains(symbol)) continue;
                Kernel newKernel = makeGoto(*state, symbol);
                if (newKernel.items.empty()) continue;
                auto kernel = std::make_unique<Kernel>(std::move(newKernel));
//                std::cout << "from " << *state << " by " << symbol << " to \n" << *kernel << std::endl;
                auto[it, inserted] = statesKernels.insert(kernel.get());
                if (inserted) {
                    changed = true;
                    addedStorage.emplace_back(std::move(kernel));
                }
                state->gotoFunction[symbol] = *it;
            }
        }
//        kernelsStorage.insert(kernelsStorage.end(), addedStorage.begin(), addedStorage.end());
//        size_t oldSize = kernelsStorage.size();
        for (std::unique_ptr<Kernel> &kernel : addedStorage) {
//            auto[it, inserted] = statesKernels.insert(kernel.get());
//            if (inserted) {
            kernelsStorage.push_back(std::move(kernel));
//            }
        }
//        changed = kernelsStorage.size() != oldSize;
        addedStorage.clear();
//        addedSet.clear();
    }
    std::cout << statesKernels.size() << std::endl;
    std::cout << kernelsStorage.size() << std::endl;


    // debug
    std::ofstream out("kernels.dot");
    for (auto &v : kernelsStorage) {
        out << reinterpret_cast<size_t>(v.get()) << " [label=\"" << *v << "\"]\n";
        for (auto &&p : v->gotoFunction) {
            out << reinterpret_cast<size_t>(v.get()) << " -> " << reinterpret_cast<size_t>(p.second) << " [label=\""
                << p.first << "\"]\n";

        }
    }
    out.close();

    auto lalrHash = [](std::vector<ParserItem> const &items) {
        size_t hash = HashLALR{}(items.front());
        auto prev = items.begin();
        for (auto it = prev + 1; it != items.end(); prev = it++) {
            if (prev->rule == it->rule && prev->pos == it->pos) {
                continue;
            }
            hash ^= HashLALR{}(*it);
        }
        return hash;
//        return std::accumulate(items.begin(), items.end(), 0, [](size_t headHash, ParserItem const &item) {
//            return headHash ^ HashLALR{}(item);
//        });
    };
    auto lalrKernelEq = [](std::vector<ParserItem> const &lhs, std::vector<ParserItem> const &rhs) {
        if (!eqLALR(lhs.front(), rhs.front())) return false;
        auto lprev = lhs.begin();
        auto rprev = rhs.begin();
        auto lit = lprev + 1;
        auto rit = rprev + 1;
        while (true) {
            while (lit != lhs.end() && lprev->rule == lit->rule && lprev->pos == lit->pos) {
                lit++;
                lprev++;
            }
            while (rit != rhs.end() && rprev->rule == rit->rule && rprev->pos == rit->pos) {
                rit++;
                rprev++;
            }
            if (rit == rhs.end() && lit == lhs.end()) return true;
            if (rit == rhs.end() || lit == lhs.end()) return false;
            if (!eqLALR(*lit, *rit)) return false;
            lprev = lit++;
            rprev = rit++;
        }
        return true;
    };

    std::unordered_map<const Kernel *, LALRKernel *> kernelsToLalrMap;
    std::unordered_map<std::vector<ParserItem>, LALRKernel *, decltype(lalrHash), decltype(lalrKernelEq)> lalrKernelsSet(
            8, lalrHash, lalrKernelEq);
    std::vector<std::unique_ptr<LALRKernel>> lalrKernelsStorage;
    for (std::unique_ptr<Kernel> const &kernel : kernelsStorage) {
        auto it = lalrKernelsSet.find(kernel->items);
        std::cout << kernel->items << std::endl;
        if (it != lalrKernelsSet.end()) {
            it->second->addKernel(kernel->items);
        } else {
            lalrKernelsStorage.emplace_back(std::make_unique<LALRKernel>(kernel->items));
            it = lalrKernelsSet.insert(std::make_pair(kernel->items, lalrKernelsStorage.back().get())).first;
        }
        kernelsToLalrMap[kernel.get()] = it->second;
    }

    for (std::unique_ptr<Kernel> const &kernel : kernelsStorage) {
        LALRKernel *lalr = kernelsToLalrMap[kernel.get()];
        for (auto const &pair : kernel->gotoFunction) {
            lalr->gotoFunction[pair.first] = kernelsToLalrMap[pair.second];
        }
    }

    std::cout << lalrKernelsStorage.size() << std::endl;
    std::cout << "--- split ---" << std::endl;
    for (auto &v : lalrKernelsStorage) {
        std::cout << *v << std::endl;
        for (auto &&p : v->gotoFunction) {
            std::cout << "==| by " << p.first << " to " << p.second << std::endl << *p.second;
        }
    }

    for (size_t i = 0; i < lalrKernelsStorage.size(); ++i) {
        lalrKernelsStorage[i]->id = i;
    }


    out = std::ofstream("lalrkernels.dot");
    for (auto &v : lalrKernelsStorage) {
        out << reinterpret_cast<size_t>(v.get()) << " [label=\"" << v->id << " " << *v << "\"]\n";
        for (auto &&p : v->gotoFunction) {
            out << reinterpret_cast<size_t>(v.get()) << " -> " << reinterpret_cast<size_t>(p.second) << " [label=\""
                << p.first << "\"]\n";

        }
    }
    out.close();

    std::vector<std::unordered_map<TerminalGrammarSymbol, Action>> actions(lalrKernelsStorage.size());
    for (std::unique_ptr<LALRKernel> const &kernel : lalrKernelsStorage) {
        for (GrammarSymbol sym : grammar) {
            if (std::holds_alternative<TerminalGrammarSymbol>(sym)) {
                TerminalGrammarSymbol terminal = std::get<TerminalGrammarSymbol>(sym);
                auto it = kernel->gotoFunction.find(terminal);
                /* TODO: start state */
                if (it != kernel->gotoFunction.end()) {
                    if (actions[kernel->id].contains(terminal)) {
                        throw std::runtime_error("bad grammar");
                    }
                    actions[kernel->id][terminal] = Shift{it->second->id};
                }
            }
        }
        for (auto const &item : kernel->kernelItems) {
            if (!item.first.atEnd()) continue;
            if (item.first.head.name() == "S'") {
                if (actions[kernel->id].contains(EndOfTextTerminal)) {
                    throw std::runtime_error("bad grammar");
                }
                actions[kernel->id][EndOfTextTerminal] = Accept{};
            } else {
                for (TerminalGrammarSymbol const &terminal : item.second) {
                    if (actions[kernel->id].contains(terminal)) {
                        throw std::runtime_error("bad grammar");
                    }
//                    NonterminalGrammarSymbol head = item.first.head;
                    actions[kernel->id][terminal] = Reduce{item.first.rule};
                }
            }
        }
    }

    std::vector<std::unordered_map<NonterminalGrammarSymbol, size_t>> gotoActions(lalrKernelsStorage.size());

    for (std::unique_ptr<LALRKernel> const &kernel : lalrKernelsStorage) {
        for (GrammarSymbol sym : grammar) {
            if (std::holds_alternative<NonterminalGrammarSymbol>(sym)) {
                auto nonterminal = std::get<NonterminalGrammarSymbol>(sym);
                auto it = kernel->gotoFunction.find(nonterminal);
                if (it != kernel->gotoFunction.end()) {
                    gotoActions[kernel->id][nonterminal] = it->second->id;
                }
            }
        }
    }

    for (size_t i = 0; i < actions.size(); ++i) {
        std::cout << lalrKernelsStorage[i]->id << std::endl;
        for (auto &p : actions[i]) {
            std::cout << p.first << ": " << p.second << ";  ";
        }
        std::cout << std::endl;
    }

    for (size_t i = 0; i < gotoActions.size(); ++i) {
        std::cout << i << " -> ";
        for (auto &p : gotoActions[i]) {
            std::cout << p.first << ": " << p.second << ";  ";
        }
        std::cout << std::endl;
    }

    return std::make_pair(kernelsToLalrMap[startKernel]->id, ParsingTable{std::move(actions), std::move(gotoActions)});

//    for (Kernel const &state : statesKernels) {
//        for (GrammarSymbol const &symbol : grammar) {
//
//        }
//    }


//    for (auto const &s : statesKernels) { std::cout << s << std::endl; }

//    auto hashKernel = [](const Kernel &i) {};
//    auto equalKernels = [](const Kernel &lhs, const Kernel &rhs) {};
//    std::unordered_set<LALR_Kernel, decltype(hashKernel), decltype(equalKernels)> unitedKernels(grammar.size(),
//                                                                                                hashKernel,
//                                                                                                equalKernels);
//    for (Kernel const &state : statesKernels) {
//        if (auto it = unitedKernels.find(state); it != unitedKernels.end()) {
//            LALR_Kernel kern = mergeKernels(std::move(*it), state);
//            unitedKernels.erase(it);
//            unitedKernels.insert(kern);
//        } else {
//            unitedKernels.insert(LALR_Kernel{state});
//        }
//    }
}

std::ostream &operator<<(std::ostream &out, const LALRKernel &kernel) {
    out << std::string("kernel for ") << std::string(":\n");
//    std::vector<ParserItem> items = kernel.kernelItems;
//    std::sort(items.begin(), items.end(), [](ParserItem const &lhs, ParserItem const &rhs) {
//        if (lhs.pos != rhs.pos) return lhs.pos > rhs.pos;
//        return lhs.head.name() < rhs.head.name();
//    });
    for (auto const &item : kernel.kernelItems) {
        out << "\t" << item.first << " " << item.second << std::endl;
    }
    return out;
}

std::ostream &operator<<(std::ostream &out, const Kernel &kernel) {
    out << std::string("kernel for ") << std::string(":\n");
    std::vector<ParserItem> items = kernel.items;
    std::sort(items.begin(), items.end(), [](ParserItem const &lhs, ParserItem const &rhs) {
//        if (lhs.pos != rhs.pos) return lhs.pos > rhs.pos;
//        if (lhs.head.name() != rhs.head.name()) return lhs.head.name() != rhs.head.name();
//        if (lhs.rule != rhs.rule) return lhs.rule != rhs.rule;
//        return name(lhs.LL1Next) < name(rhs.LL1Next);

        if (lhs.pos != rhs.pos) return lhs.pos < rhs.pos;
        if (lhs.rule != rhs.rule) return lhs.rule < rhs.rule;
        return name(lhs.LL1Next) < name(rhs.LL1Next);
    });
    for (auto const &item : items) {
        out << "\t" << item << "\n";
    }
    return out;
}

std::ostream &operator<<(std::ostream &out, const ParserItem &item) {
    size_t i = 0;
    out << item.head << "->";
    for (; i < item.pos; ++i) {
        out << item.rule->body[i] << " ";
    }
    out << ".";
    for (; i < item.rule->body.size(); ++i) {
        out << item.rule->body[i] << " ";
    }
    out << "| ";
    out << std::visit([](auto &t) { return t.name(); }, item.LL1Next);
    return out;
}
