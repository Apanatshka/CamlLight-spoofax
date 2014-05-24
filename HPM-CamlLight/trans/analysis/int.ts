module analysis/int

imports
  include/CamlLight
  extra-constructors

type rules
  Prefix("+") : FuncType(ConstrType(None(), "int"), FuncType(ConstrType(None(), "int"), ConstrType(None(), "int")))
  Prefix("-") : FuncType(ConstrType(None(), "int"), FuncType(ConstrType(None(), "int"), ConstrType(None(), "int")))