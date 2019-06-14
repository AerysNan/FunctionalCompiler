import AST
import Helper
import EvalValue
import Parser
import Data.Map

------------------------------------------------------------

maybeADT = ADT "Maybe" [("Just", [TInt]), ("Nothing", [])]

listADT = ADT "List" [("Con", [TInt, TData "List"]), ("Nil", [])]

adtContext = ADTContext [] empty

------------------------------------------------------------

safeDiv_demo = makeFun ("safeDiv", TData "Maybe")
  [("x", TInt), ("y", TInt)] $
  EIf (EEq (EVar "y") (EIntLit 0))
    (EVar "Nothing")
    (callFun (EVar "Just") [EDiv (EVar "x") (EVar "y")])

map_demo = makeFun ("map", TData "List")
  [("f", TArrow TInt TInt), ("list", TData "List")] $
  ECase (EVar "list") [
    (PData "Nil" [], EVar "Nil"),
    (PData "Con" [PVar "x", PVar "xs"],
      callFun (EVar "Con") [
        callFun (EVar "f") [EVar "x"],
        callFun (EVar "map") [EVar "f", EVar "xs"]
      ])
  ]

filter_demo = makeFun ("filter", TData "List")
  [("f", TArrow TInt TBool), ("list", TData "List")] $
  ECase (EVar "list") [
    (PData "Nil" [], EVar "Nil"),
    (PData "Con" [PVar "x", PVar "xs"],
      ELet ("ys", callFun (EVar "filter") [EVar "f", EVar "xs"]) $
        EIf (callFun (EVar "f") [EVar "x"])
          (callFun (EVar "Con") [EVar "x", EVar "ys"])
          (EVar "ys"))
  ]

lseq_demo = makeFun ("lseq", TBool)
  [("list1", TData "List"), ("list2", TData "List")] $
  ECase (EVar "list1") [
    (PData "Nil" [],
      ECase (EVar "list2") [
        (PData "Nil" [], EBoolLit True),
        (PData "Con" [PVar "y", PVar "ys"], EBoolLit False)
      ]
    ),
    (PData "Con" [PVar "x", PVar "xs"],
      ECase (EVar "list2") [
        (PData "Nil" [], EBoolLit False),
        (PData "Con" [PVar "y", PVar "ys"],
          EIf (ENeq (EVar "x") (EVar "y"))
            (EBoolLit False)
            (callFun (EVar "lseq") [EVar "xs", EVar "ys"]))
      ]
    )
  ]

square_demo = ELambda ("x", TInt) $ EMul (EVar "x") (EVar "x")

even_demo = ELambda ("x", TInt) $ EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0)

------------------------------------------------------------

list_1 = callFun (EVar "Con") [
    EIntLit 1, EVar "Nil"
  ]

list_1_2 = callFun (EVar "Con") [
    EIntLit 1, callFun (EVar "Con") [
      EIntLit 2, EVar "Nil"
  ]]

list_1_3 = callFun (EVar "Con") [
    EIntLit 1, callFun (EVar "Con") [
      EIntLit 3, EVar "Nil"
  ]]

list_1_2_3 = callFun (EVar "Con") [
    EIntLit 1, callFun (EVar "Con") [
      EIntLit 2, callFun (EVar "Con") [
        EIntLit 3, EVar "Nil"
  ]]]

------------------------------------------------------------

expr_1 = safeDiv_demo $ callFun (EVar "safeDiv") [EIntLit 7, EIntLit 2]
program_1 = Program [maybeADT] expr_1
result_1 = evalProgramValue program_1

expr_2 = safeDiv_demo $ callFun (EVar "safeDiv") [EIntLit 7, EIntLit 0]
program_2 = Program [maybeADT] expr_2
result_2 = evalProgramValue program_2

expr_3 = lseq_demo $ callFun (EVar "lseq") [list_1_2, list_1_2]
program_3 = Program [listADT] expr_3
result_3 = evalProgramValue program_3

expr_4 = lseq_demo $ callFun (EVar "lseq") [list_1_2, list_1_3]
program_4 = Program [listADT] expr_4
result_4 = evalProgramValue program_4

expr_5 = lseq_demo $ callFun (EVar "lseq") [list_1_2, list_1]
program_5 = Program [listADT] expr_5
result_5 = evalProgramValue program_5

expr_6 = map_demo $ callFun (EVar "map") [square_demo, list_1_2_3]
program_6 = Program [listADT] expr_6
result_6 = evalProgramValue program_6

expr_7 = filter_demo $ callFun (EVar "filter") [even_demo, list_1_2_3]
program_7 = Program [listADT] expr_7
result_7 = evalProgramValue program_7

------------------------------------------------------------

pstr_1 = "True;"
presult_1 = inputParser adtContext pstr_1

pstr_2 = "40 + 2;"
presult_2 = inputParser adtContext pstr_2

pstr_3 = "'@' != '@';"
presult_3 = inputParser adtContext pstr_3

pstr_4 = "if False then 42 else 233;"
presult_4 = inputParser adtContext pstr_4

pstr_5 = "lambda x::Int . x + 1;"
presult_5 = inputParser adtContext pstr_5

pstr_6 = "let even = (lambda x::Int . x % 2 == 0) in even 42;"
presult_6 = inputParser adtContext pstr_6

pstr_7 = "let fact = lambda x::Int . (if x == 0 then 1 else x * fact (x - 1))::Int in fact 5;"
presult_7 = inputParser adtContext pstr_7