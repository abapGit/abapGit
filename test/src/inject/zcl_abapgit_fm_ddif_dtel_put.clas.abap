CLASS zcl_abapgit_fm_ddif_dtel_put DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
ENDCLASS.

CLASS zcl_abapgit_fm_ddif_dtel_put IMPLEMENTATION.
  METHOD if_ftd_invocation_answer~answer.

    " ZCL_ABAPGIT_OBJECT_DTEL reads the data element back from DD04L and DD04T
    " instead of calling DDIF_DTEL_GET, so persist it here

    DATA lv_ref   TYPE REF TO data.
    DATA ls_dd04v TYPE dd04v.
    DATA ls_dd04l TYPE dd04l.
    DATA ls_dd04t TYPE dd04t.

    FIELD-SYMBOLS <lv_data> TYPE any.

    lv_ref = arguments->get_importing_parameter( 'DD04V_WA' ).
    ASSIGN lv_ref->* TO <lv_data>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    ls_dd04v = <lv_data>.

    lv_ref = arguments->get_importing_parameter( 'NAME' ).
    ASSIGN lv_ref->* TO <lv_data>.
    IF sy-subrc = 0.
      ls_dd04v-rollname = <lv_data>.
    ENDIF.

    MOVE-CORRESPONDING ls_dd04v TO ls_dd04l.
    ls_dd04l-as4local = 'A'.
    ls_dd04l-as4vers  = '0000'.
    MODIFY dd04l FROM ls_dd04l.

    MOVE-CORRESPONDING ls_dd04v TO ls_dd04t.
    ls_dd04t-as4local = 'A'.
    ls_dd04t-as4vers  = '0000'.
    MODIFY dd04t FROM ls_dd04t.

  ENDMETHOD.
ENDCLASS.
