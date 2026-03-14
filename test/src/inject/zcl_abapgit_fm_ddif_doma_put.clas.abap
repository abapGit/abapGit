CLASS zcl_abapgit_fm_ddif_doma_put DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
    CLASS-DATA gs_dd01v TYPE dd01v.
    CLASS-DATA gt_dd07v TYPE dd07v_tab.
ENDCLASS.

CLASS zcl_abapgit_fm_ddif_doma_put IMPLEMENTATION.
  METHOD if_ftd_invocation_answer~answer.

    DATA lv_ref TYPE REF TO data.

    FIELD-SYMBOLS <lv_data> TYPE any.

    lv_ref = arguments->get_importing_parameter( 'DD01V_WA' ).
    ASSIGN lv_ref->* TO <lv_data>.
    IF sy-subrc = 0.
      gs_dd01v = <lv_data>.
    ENDIF.

    lv_ref = arguments->get_table_parameter( 'DD07V_TAB' ).
    ASSIGN lv_ref->* TO <lv_data>.
    IF sy-subrc = 0.
      gt_dd07v = <lv_data>.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
