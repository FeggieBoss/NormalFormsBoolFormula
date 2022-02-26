# Перевод булевой формулы в одну из нормальных форм
[![Haskell CI](https://github.com/FeggieBoss/NormalFormsBoolFormula/actions/workflows/haskell.yml/badge.svg)](https://github.com/FeggieBoss/NormalFormsBoolFormula/actions/workflows/haskell.yml)
### Запуск:  
``` 
$> stack build  
$> stack run -- "put formula here"  
```
### Пример:
```
$> stack build  
$> stack run -- "(p0 & p1) | !p2" 
Your formula - p0 & p1 | !p2
to NNF - p0 & p1 | !p2
to DNF - p0 & p1 | !p2
to CNF - (p0 | !p2) & (p1 | !p2)
```

### Тестирование:  
``` 
$> stack test
```
### Пример:
```
$> stack test
QuickCheck:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
UnitTests:
  NNF Tests
    already in NNF:    OK
    arrow destruction: OK
    lows:              OK
    special cases:     OK
  DNF Tests
    already in DNF:    OK
    lows:              OK
    special cases:     OK
  CNF Tests
    already in CNF:    OK
    lows:              OK
    special cases:     OK

All 10 tests passed (0.03s)
```
