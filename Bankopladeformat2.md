Bankopladeformat 2.0
====================

BNF-notation
------------

      <bankopladefil> ::= <bankopladeliste>
    <bankopladeliste> ::= <bankoplade> <linjeskift> <bankopladeliste>
                        | <bankoplade> <linjeskift>
         <bankoplade> ::= <bankopladelinje> <bankopladelinje> <bankopladelinje>
    <bankopladelinje> ::= <felt> " " <felt> " " <felt> " " <felt> " " <felt> 
" " <felt> " " <felt> " " <felt> " " <felt> <linjeskift>
         <linjeskift> ::= "\n"
               <felt> ::= <etciferheltal> <etciferheltal>
      <etciferheltal> ::= 0-9

Eksempel
--------

    00 00 20 00 43 51 61 00 84
    00 00 00 33 45 52 00 70 89
    02 10 00 35 00 00 64 00 90
    
    02 00 21 30 00 00 64 00 81
    00 00 24 00 41 50 00 75 87
    00 12 25 32 00 53 66 00 00
    
    00 10 00 00 40 00 61 73 81
    03 00 00 32 43 50 00 00 84
    06 00 21 00 46 52 00 75 00
    

Noter
-----

Det er væsentligt at hver bankoplade afsluttes med to linjeskift, også den
sidste bankoplade.
