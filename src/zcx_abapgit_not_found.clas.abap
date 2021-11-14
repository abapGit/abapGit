CLASS zcx_abapgit_not_found DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_abapgit_not_found IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      textid   = textid
      previous = previous ).
  ENDMETHOD.
ENDCLASS.
