module analysis/eq

imports
  include/CamlLight
  extra-constructors

type rules
  Prefix("<") : FuncType(TypeVar("'a"), FuncType(TypeVar("'a"), ConstrType(None(), GlobalName("bool"))))
  Prefix("=") : FuncType(TypeVar("'a"), FuncType(TypeVar("'a"), ConstrType(None(), GlobalName("bool"))))