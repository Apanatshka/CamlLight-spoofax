module analysis/eq

imports
  include/CamlLight
  extra-constructors

type rules
  Prefix("<") : FuncType(TypeVar("'a"), FuncType(TypeVar("'a"), ConstrType(None(), "bool")))
  Prefix("=") : FuncType(TypeVar("'a"), FuncType(TypeVar("'a"), ConstrType(None(), "bool")))