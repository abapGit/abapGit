CLASS zcl_abapgit_syntax_txt DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS process_line
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_syntax_txt IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " No rules for plain text files

  ENDMETHOD.


  METHOD process_line.

    rv_line = apply_style(
      iv_line  = iv_line
      iv_class = '' ).

  ENDMETHOD.
ENDCLASS.
