TYPES: BEGIN OF ty_dd02_text,
         ddlanguage TYPE dd02t-ddlanguage,
         ddtext     TYPE dd02t-ddtext,
       END OF ty_dd02_text.

TYPES ty_dd02_texts TYPE STANDARD TABLE OF ty_dd02_text.

TYPES ty_dd03p_tt TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

TYPES: BEGIN OF ty_internal,
         dd02v      TYPE dd02v,
         dd09l      TYPE dd09l,
         dd03p      TYPE ty_dd03p_tt,
         dd05m      TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY,
         dd08v      TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
         dd12v      TYPE dd12vtab,
         dd17v      TYPE dd17vtab,
         dd35v      TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
         dd36m      TYPE dd36mttyp,
         dd02_texts TYPE ty_dd02_texts,
         i18n_langs TYPE STANDARD TABLE OF langu WITH DEFAULT KEY,
       END OF ty_internal.
