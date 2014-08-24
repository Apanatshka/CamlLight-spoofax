module analysis/literals

imports
  include/CamlLight

type rules
  Int(_)    : ConstrType(None(), GlobalName("int"))
  Float(_)  : ConstrType(None(), GlobalName("float"))
  Char(_)   : ConstrType(None(), GlobalName("char"))
  String(_) : ConstrType(None(), GlobalName("string"))
  Unit()    : ConstrType(None(), GlobalName("unit"))