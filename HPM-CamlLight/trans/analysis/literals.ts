module analysis/literals

imports
  include/CamlLight

type rules
  Int(_)    : ConstrType(None(), "int")
  Float(_)  : ConstrType(None(), "float")
  Char(_)   : ConstrType(None(), "char")
  String(_) : ConstrType(None(), "string")
  Unit()    : ConstrType(None(), "unit")