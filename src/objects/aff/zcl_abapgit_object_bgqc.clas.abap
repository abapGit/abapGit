CLASS zcl_abapgit_object_bgqc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_bgqc_name         TYPE string VALUE 'BGQCNAME'.
    CONSTANTS c_bgqc_wbi_p        TYPE string VALUE 'CL_BGQC_WBI_P'.
    CONSTANTS c_select_changed_by TYPE string VALUE 'IF_BGQC_WBI_P~SELECT_CHANGED_BY'.

ENDCLASS.


CLASS zcl_abapgit_object_bgqc IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.
    DATA lo_bgqc_wbi_p    TYPE REF TO object.
    DATA lv_ref_bgqc_name TYPE REF TO data.
    DATA lv_changed_by    TYPE syuname.
    DATA lv_subrc         TYPE sy-subrc.
    DATA lx_root          TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lv_ref_bgqc_name> TYPE any.

    TRY.

        CREATE DATA lv_ref_bgqc_name TYPE (c_bgqc_name).
        ASSIGN lv_ref_bgqc_name->* TO <lv_ref_bgqc_name>.
        ASSERT sy-subrc = 0.

        me->ms_item-obj_name = <lv_ref_bgqc_name>.

        CREATE OBJECT lo_bgqc_wbi_p TYPE (c_bgqc_wbi_p).

        CALL METHOD lo_bgqc_wbi_p->(c_select_changed_by)
          EXPORTING
            iv_bgqc_name  = <lv_ref_bgqc_name>
          IMPORTING
            ev_changed_by = lv_changed_by
            ev_subrc      = lv_subrc.

        IF lv_subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        rv_user = lv_changed_by.

      CATCH cx_root INTO lx_root.

        zcx_abapgit_exception=>raise_with_text( lx_root ).

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
