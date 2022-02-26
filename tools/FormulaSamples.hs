module FormulaSamples where

import FormulaDeclaration

-- пример атомов
atom1 = Atom "p1"                                       -- p1
atom2 = Atom "p2"                                       -- p2
atom3 = Atom "p3"                                       -- p3
atom4 = Atom "p4"                                       -- p4


-- формулы с "and"
and1 = atom1 `And` atom2                                -- p1 & p2
and2 = atom3 `And` atom4                                -- p3 & p4
and3 = atom1 `And` (and1 `And` atom4)                   -- p1 & ((p1 & p2) & p4) 


-- формулы с "or"
or1 = atom1 `Or` atom2                                  -- p1 | p2
or2 = atom2 `Or` atom4                                  -- p2 | p4
or3 = (or1 `Or` atom4) `Or` (or1 `Or` or2)              -- ((p1 | p2) | p4) | ((p1 | p2) | (p2 | p4))


-- формулы из "and" "or"
andOr1 = atom1 `And` or1                                -- p1 & (p1 | p2)
andOr2 = or2 `And` atom3                                -- (p2 | p4) & p3
andOr3 = or1 `And` (atom2 `Or` atom2)                   -- (p1 | p2) & (p2 | p2)
andOr4 = (atom1 `Or` atom3) `And` (atom1 `Or` atom4)    -- (p1 | p3) & (p1 | p4)
orAnd1 = and1 `Or` atom2                                -- (p1 & p2) | p2
orAnd2 = atom1 `Or` and2                                -- p1 | (p3 & p4)
orAnd3 = (atom1 `And` atom1) `Or` (atom1 `And` atom2)   -- (p1 & p1) | (p1 & p2)
orAnd4 = (atom2 `And` atom3) `Or` (atom4 `And` atom3)   -- (p2 & p3) | (p4 & p3)
orAnd5 = (atom1 `Or` (atom2 `Or` atom2))                -- (p1 | (p2 | p2)) & (p2 | (p2 | p2))
   `And` (atom2 `Or` (atom2 `Or` atom2))                 


-- "not" от атомов
not1 = Not atom1                                        -- !p1
not2 = Not atom2                                        -- !p2
not3 = Not atom3                                        -- !p3
not4 = Not atom4                                        -- !p4
notnot1 = Not not1                                      -- !!p1


-- формулы из "and" "or" "not"
orNot1 = not1 `Or` atom4                                -- !p1 | p4
orNot2 = not1 `Or` not2                                 -- !p1 | !p2
orAnd = not2 `And` not4                                 -- !p2 & !p4
andOrNot1 = (not1 `Or` atom3) `And` (not3 `Or` atom1)   -- (!p1 | p3) & (!p3 | p1)
andOrNot2 = orNot2 `Or` and2                            -- (!p1 | !p2) | (p3 & p4) 


-- формулы из "->"
arrow1 = atom1 :->: atom4                               -- p1 -> p4
arrow2 = arrow1 :->: arrow1                             -- (p1 -> p4) -> (p1 -> p4)


-- формулы из "<->"
darrow1 = atom1 :<->: atom3                             -- p1 <-> p3
darrow2 = atom4 :<->: darrow1                           -- p4 <-> (p1 <-> p3)


-- "not" от формул
notAnd1 = Not and1                                      -- !(p1 & p2)
notOr1 = Not or2                                        -- !(p2 | p4)
notArrow1 = Not arrow1                                  -- !(p1 -> p4)
notDarrow1 = Not darrow1                                -- !(p1 <-> p3)


-- формулы из "->" "and" "or"
arrowAndAnd = and1 :->: and2                            -- (p1 & p2) -> (p3 & p4)
arrowAndOr = and1 :->: or1                              -- (p1 & p2) -> (p1 | p2)
arrowOrAnd = or2 :->: and2                              -- (p2 | p4) -> (p3 & p4)
orArrowAnd = and1 `Or` (not2 :->: atom2)                -- (p1 & p2) | (!p2 -> p2)


-- формулы из "<->" "and" "or"
darrowAndAnd = and1 :<->: and2                          -- (p1 & p2) <-> (p3 & p4)
darrowAndOr = and1 :<->: or1                            -- (p1 & p2) <-> (p1 | p2)
darrowOrAnd = or2 :<->: and2                            -- (p2 | p4) <-> (p3 & p4)


-- произвольные потенциально опасные формулы
mixed1 = Not (Not (Not darrow1))                                -- !!!(p1 <-> p3)
mixed2 = Not (not1 `And` not2)                                  -- !(!p1 & !p2)
mixed3 = Not (arrow1 `And` darrow1)                             -- !((p1 -> p4) & (p1 <-> p3))
mixed4 = Not (andOr1 `And` (Not andOr1))                        -- !((p1 & (p1 | p2)) & !(p1 & (p1 | p2)))
mixed5 = atom1 `Or` (atom2 `And` (Not or1))                     -- p1 | (p2 & !(p1 | p2))
mixed6 = atom1 `Or` (atom2 `And` (not1 `And` not2))             -- p1 | (p2 & (!p1 & !p2))
mixed7 = (atom1 `And` not3) `Or` (atom3 `And` not1)             -- (p1 & !p3) | (p3 | !p1)
mixed8 = ((not1 `And` not3) `Or` (atom3 `And` not3))            -- ((!p1 & !p3) | (p3 & !p3)) | ((!p1 & p3) | (!p3 & p1))
   `Or` ((not1 `And` atom1) `Or` (atom3 `And` atom1))
mixed9 = (atom1 `And` not4)                                     -- (p1 & !p4) | ((p1 & !p3) | (p3 | !p1))
   `Or` ((atom1 `And` not3) `Or` (atom3 `And` not1))
mixed10 = (not1 `Or` (not1 `And` not2))                         -- (!p1 | (!p1 & !p2)) | ((p1 & p1) | (p1 & p2))
   `Or` ((atom1 `And` atom1) `Or` (atom1 `And` atom2)) 
mixed11 = (((not1 `Or` not2) `Or` atom3)                        -- (((!p1 | !p2) | p3) & ((!p1 | !p2) | p4)) & (((!p3 | !p4) | p1) & ((!p3 | !p4) | p2))
   `And`  ((not1 `Or` not2) `Or` atom4))
   `And`  (((not3 `Or` not4) `Or` atom1)
   `And`  ((not3 `Or` not4) `Or` atom2))
mixed12 = or1 `And` ((atom1 `Or` not1) `And` (atom1 `Or` not2)) -- (p1 | p2) & ((p1 | !p1) & (p1 | !p2))