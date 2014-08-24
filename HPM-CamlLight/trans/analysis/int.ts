module analysis/int

imports
  include/CamlLight
  extra-constructors

type rules
  Prefix("+") : FuncType(ConstrType(None(), GlobalName("int")), FuncType(ConstrType(None(), GlobalName("int")), ConstrType(None(), GlobalName("int"))))
  Prefix("-") : FuncType(ConstrType(None(), GlobalName("int")), FuncType(ConstrType(None(), GlobalName("int")), ConstrType(None(), GlobalName("int"))))