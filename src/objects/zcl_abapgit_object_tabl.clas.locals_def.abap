TYPES ty_dd03p_tt TYPE STANDARD TABLE OF dd03p.

TYPES: BEGIN OF ty_internal,
         dd02v TYPE dd02v,
         dd09l TYPE dd09l,
         dd03p TYPE ty_dd03p_tt,
         dd05m TYPE TABLE OF dd05m,
         dd08v TYPE TABLE OF dd08v,
         dd12v TYPE dd12vtab,
         dd17v TYPE dd17vtab,
         dd35v TYPE TABLE OF dd35v,
         dd36m TYPE dd36mttyp,
       END OF ty_internal.
