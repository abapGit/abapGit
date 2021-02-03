CLASS zcl_abapgit_object_srfc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_srfc IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          lx_error        TYPE REF TO cx_root,
          lv_text         TYPE string.


    IF zif_abapgit_object~exists( ) = abap_false.
* the SRFC might already have been deleted with the function module
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->delete( p_object_key = |{ ms_item-obj_name }|
                                 p_version    = 'A' ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          li_object_data  TYPE REF TO if_wb_object_data_model,
          lv_text         TYPE string,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.


    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uname.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-datum.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uzeit.
        ENDIF.

        io_xml->read(
          EXPORTING
            iv_name = 'SRFC'
          CHANGING
            cg_data = <lg_srfc_data> ).

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
        CREATE OBJECT li_object_data TYPE ('CL_UCONRFC_OBJECT_DATA').

        li_object_data->set_data( <lg_srfc_data> ).

        li_srfc_persist->save( li_object_data ).

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name    " Object Name
        object_type         = ms_item-obj_type    " Object Type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root,
          lv_text         TYPE string.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.


    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

        li_object_data->get_data( IMPORTING p_data = <lg_srfc_data> ).


        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

    io_xml->add( iv_name = 'SRFC'
                 ig_data = <lg_srfc_data> ).

  ENDMETHOD.
ENDCLASS.
