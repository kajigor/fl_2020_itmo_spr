from dataclasses import dataclass
from typing import List


class Grammar:

    def __init__(self, rules):
        self.rules = rules

    def check_llness(self) -> bool:
        raise NotImplementedError()

    def __repr__(self):
        rep_string = []
        for rule in self.rules:
            rep_string.append(f"{rule.nonterm} -> {self.rhs_to_string(rule.rhs)}")
        return "\n".join(rep_string)

    @staticmethod
    def rhs_to_string(rhs):
        return " | ".join(map(lambda x: " ".join(x) if len(x) != 0 else "EPSILON",
                            rhs))


@dataclass
class Rule:
    nonterm: str
    rhs: List[str]
    nullable: bool = None
