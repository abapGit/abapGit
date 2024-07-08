CLASS zcl_abapgit_object_bgqc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS cv_bgqc_name         TYPE string VALUE 'BGQCNAME'.
    CONSTANTS cv_bgqc_wbi_p        TYPE string VALUE 'CL_BGQC_WBI_P'.
    CONSTANTS cv_select_changed_by TYPE string VALUE 'IF_BGQC_WBI_P~SELECT_CHANGED_BY'.

ENDCLASS.


CLASS zcl_abapgit_object_bgqc IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.
    DATA lo_bgqc_wbi_p   TYPE REF TO object.
    DATA lvref_bgqc_name TYPE REF TO data.
    DATA lv_changed_by   TYPE syuname.
    DATA lv_subrc        TYPE sy-subrc.
    DATA lx_root         TYPE REF TO cx_root.

    TRY.

        CREATE DATA lvref_bgqc_name TYPE (cv_bgqc_name).
        lvref_bgqc_name->* = me->ms_item-obj_name.

        CREATE OBJECT lo_bgqc_wbi_p TYPE (cv_bgqc_wbi_p).

        CALL METHOD lo_bgqc_wbi_p->(cv_select_changed_by)
          EXPORTING iv_bgqc_name  = lvref_bgqc_name->*
          IMPORTING ev_changed_by = lv_changed_by
                    ev_subrc      = lv_subrc.

        IF lv_subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        rv_user = lv_changed_by.

      CATCH cx_root INTO lx_root.

        zcx_abapgit_exception=>raise_with_text( ix_previous = lx_root ).

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
