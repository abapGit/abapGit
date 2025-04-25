CLASS zcx_abapgit_type_not_supported DEFINITION
  PUBLIC
  INHERITING FROM zcx_abapgit_exception
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        log      TYPE REF TO zif_abapgit_log OPTIONAL
        msgv1    TYPE symsgv OPTIONAL
        msgv2    TYPE symsgv OPTIONAL
        msgv3    TYPE symsgv OPTIONAL
        msgv4    TYPE symsgv OPTIONAL
        longtext TYPE csequence OPTIONAL
        obj_type TYPE trobjtype.

    METHODS get_text REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_obj_type TYPE trobjtype.
ENDCLASS.



CLASS zcx_abapgit_type_not_supported IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor(
        textid   = textid
        previous = previous
        log      = log
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4
        longtext = longtext ).

    mv_obj_type = obj_type.

  ENDMETHOD.


  METHOD get_text.
    result = |Object type { mv_obj_type } is not supported by this system|.
  ENDMETHOD.

ENDCLASS.
