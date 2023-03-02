CLASS lcl_cua_interface DEFINITION INHERITING FROM zcl_abapgit_objects_program FINAL.
  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_cua_interface.
    METHODS get_own_cua
      RETURNING
        VALUE(rs_cua) TYPE ty_cua
      RAISING
        zcx_abapgit_exception.
    METHODS put_own_cua
      IMPORTING
        is_cua TYPE ty_cua
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_cua_interface IMPLEMENTATION.

  METHOD new.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    SELECT SINGLE devclass object obj_name INTO (ls_item-devclass, ls_item-obj_type, ls_item-obj_name)
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND object   = 'PROG'
      AND obj_name = sy-cprog.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

  ENDMETHOD.

  METHOD get_own_cua.

    rs_cua = serialize_cua( iv_program_name = sy-cprog ).

  ENDMETHOD.

  METHOD put_own_cua.

    DATA li_log TYPE REF TO zif_abapgit_log.

    deserialize_cua(
      is_cua          = is_cua
      iv_program_name = ms_item-obj_name ).

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    zcl_abapgit_objects_activation=>activate( ii_log = li_log ).
    zcl_abapgit_objects_activation=>clear( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_own_cua_provider DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(rs_cua) TYPE zcl_abapgit_objects_program=>ty_cua.
ENDCLASS.

CLASS lcl_own_cua_provider IMPLEMENTATION.
  METHOD get.
    " @@abapmerge include-cua zabapgit.prog.xml > rs_cua
  ENDMETHOD.
ENDCLASS.
