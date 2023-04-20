CLASS zcl_abapgit_object_odso DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      clear_field
        IMPORTING
          iv_fieldname TYPE string
        CHANGING
          cg_metadata  TYPE any.
ENDCLASS.



CLASS zcl_abapgit_object_odso IMPLEMENTATION.


  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_dsonam  TYPE c LENGTH 30,
          ls_return  TYPE bapiret2,
          lr_details TYPE REF TO data.

    FIELD-SYMBOLS: <lg_details> TYPE any,
                   <lg_tstpnm>  TYPE any.

    TRY.
        CREATE DATA lr_details TYPE ('BAPI6116').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject = lv_dsonam
      IMPORTING
        details   = <lg_details>
        return    = ls_return.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when geting changed by of ODSO: { ls_return-message }| ).
    ENDIF.

    ASSIGN COMPONENT 'TSTPNM' OF STRUCTURE <lg_details> TO <lg_tstpnm>.

    rv_user = <lg_tstpnm>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_odsonam    TYPE c LENGTH 30,
          lv_objname    TYPE sobj_name,
          lo_collection TYPE REF TO object,
          lt_msg        TYPE STANDARD TABLE OF bal_s_msg,
          ls_msg        TYPE bal_s_msg.

    TRY.
        CREATE OBJECT lo_collection TYPE ('CL_RSD_ODSO_COLLECTION').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    lv_odsonam = ms_item-obj_name.
    lv_objname = ms_item-obj_name.

    TRY.
        CALL METHOD lo_collection->('ADD_TLOGO')
          EXPORTING
            i_objnm  = lv_objname
            i_modify = abap_true
            i_delete = abap_true.

        CALL METHOD lo_collection->('DELETE').

        CALL METHOD ('CL_RSO_APPLICATION_LOG')=>('APPL_LOG_MSG_READ')
          IMPORTING
            e_t_msg = lt_msg.

        READ TABLE lt_msg WITH KEY msgty = 'E' INTO ls_msg.
        IF sy-subrc = 0.
          zcx_abapgit_exception=>raise(
          |Error when deleting ODSO: { ms_item-obj_name } { ls_msg-msgv1 } { ls_msg-msgv2 }| ).
        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Canceled deletion of ODSO: { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_dsonam      TYPE c LENGTH 30,
          lr_details     TYPE REF TO data,
          lr_infoobjects TYPE REF TO data,
          lr_navigation  TYPE REF TO data,
          lr_indexes     TYPE REF TO data,
          lr_index_iobj  TYPE REF TO data,
          lt_return      TYPE STANDARD TABLE OF bapiret2,
          ls_return      TYPE bapiret2.

    FIELD-SYMBOLS:
      <lg_details>     TYPE any,
      <lg_odsobject>   TYPE any,
      <lt_infoobjects> TYPE STANDARD TABLE,
      <lt_navigation>  TYPE STANDARD TABLE,
      <lt_indexes>     TYPE STANDARD TABLE,
      <lt_index_iobj>  TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details     TYPE ('BAPI6116').
        CREATE DATA lr_infoobjects TYPE STANDARD TABLE OF ('BAPI6116IO').
        CREATE DATA lr_navigation  TYPE STANDARD TABLE OF ('BAPI6116NA').
        CREATE DATA lr_indexes     TYPE STANDARD TABLE OF ('BAPI6116IN').
        CREATE DATA lr_index_iobj  TYPE STANDARD TABLE OF ('BAPI6116II').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->*     TO <lg_details>.
    ASSIGN lr_infoobjects->* TO <lt_infoobjects>.
    ASSIGN lr_navigation->*  TO <lt_navigation>.
    ASSIGN lr_indexes->*     TO <lt_indexes>.
    ASSIGN lr_index_iobj->*  TO <lt_index_iobj>.

    io_xml->read( EXPORTING iv_name = 'ODSO'
                  CHANGING  cg_data = <lg_details> ).

    io_xml->read( EXPORTING iv_name = 'INFOOBJECTS'
                  CHANGING  cg_data =  <lt_infoobjects> ).

    io_xml->read( EXPORTING iv_name = 'NAVIGATION'
                  CHANGING  cg_data =  <lt_navigation> ).

    io_xml->read( EXPORTING iv_name = 'INDEXES'
                  CHANGING  cg_data =  <lt_indexes> ).

    io_xml->read( EXPORTING iv_name = 'INDEX_IOBJ'
                  CHANGING  cg_data =  <lt_index_iobj> ).
    TRY.

        ASSIGN COMPONENT 'ODSOBJECT' OF STRUCTURE <lg_details> TO <lg_odsobject>.
        ASSERT sy-subrc = 0.

        IF zif_abapgit_object~exists( ) = abap_false.
          CALL FUNCTION 'BAPI_ODSO_CREATE'
            EXPORTING
              details              = <lg_details>
            IMPORTING
              odsobject            = lv_dsonam
            TABLES
              infoobjects          = <lt_infoobjects>
              navigationattributes = <lt_navigation>
              indexes              = <lt_indexes>
              indexesinfoobjects   = <lt_index_iobj>
              return               = lt_return.
        ELSE.
          CALL FUNCTION 'BAPI_ODSO_CHANGE'
            EXPORTING
              odsobject            = <lg_odsobject>
              details              = <lg_details>
            TABLES
              infoobjects          = <lt_infoobjects>
              navigationattributes = <lt_navigation>
              indexes              = <lt_indexes>
              indexesinfoobjects   = <lt_index_iobj>
              return               = lt_return.
        ENDIF.

      CATCH  cx_sy_dyn_call_illegal_func.
        zcx_abapgit_exception=>raise( |Necessary BW function modules not found or object not supported| ).
    ENDTRY.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Error when creating ODSO: { ls_return-message }| ).
    ENDIF.

    CALL FUNCTION 'BAPI_ODSO_ACTIVATE'
      EXPORTING
        odsobject = <lg_odsobject>
      TABLES
        return    = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Error when activating ODSO: { ls_return-message }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE c LENGTH 30.

    SELECT SINGLE odsobject
    FROM ('RSDODSO')
    INTO lv_iobjnm
    WHERE odsobject = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA: lv_dsona TYPE c LENGTH 30,
          lo_odso  TYPE REF TO object,
          lv_isact TYPE abap_bool.

    lv_dsona = ms_item-obj_name.

    CALL METHOD ('CL_RSD_ODSO')=>('FACTORY')
      EXPORTING
        i_odsobject = lv_dsona
      RECEIVING
        r_r_odso    = lo_odso.

    CALL METHOD lo_odso->('IS_ACTIVE')
      RECEIVING
        r_is_active = lv_isact.

    rv_active = lv_isact.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object =  ms_item-obj_name.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'RSD_S_PROV'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_dsonam      TYPE c LENGTH 30,
          lr_details     TYPE REF TO data,
          lr_infoobjects TYPE REF TO data,
          lr_navigation  TYPE REF TO data,
          lr_indexes     TYPE REF TO data,
          lr_index_iobj  TYPE REF TO data,
          ls_return      TYPE bapiret2.

    FIELD-SYMBOLS:
      <lg_details>     TYPE any,
      <lt_infoobjects> TYPE STANDARD TABLE,
      <lt_navigation>  TYPE STANDARD TABLE,
      <lt_indexes>     TYPE STANDARD TABLE,
      <lt_index_iobj>  TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details     TYPE ('BAPI6116').
        CREATE DATA lr_infoobjects TYPE STANDARD TABLE OF ('BAPI6116IO').
        CREATE DATA lr_navigation  TYPE STANDARD TABLE OF ('BAPI6116NA').
        CREATE DATA lr_indexes     TYPE STANDARD TABLE OF ('BAPI6116IN').
        CREATE DATA lr_index_iobj  TYPE STANDARD TABLE OF ('BAPI6116II').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->*     TO <lg_details>.
    ASSIGN lr_infoobjects->* TO <lt_infoobjects>.
    ASSIGN lr_navigation->*  TO <lt_navigation>.
    ASSIGN lr_indexes->*     TO <lt_indexes>.
    ASSIGN lr_index_iobj->*  TO <lt_index_iobj>.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject            = lv_dsonam
      IMPORTING
        details              = <lg_details>
        return               = ls_return
      TABLES
        infoobjects          = <lt_infoobjects>
        navigationattributes = <lt_navigation>
        indexes              = <lt_indexes>
        indexesinfoobjects   = <lt_index_iobj>.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when geting details of ODSO: { ls_return-message }| ).
    ENDIF.

    clear_field( EXPORTING iv_fieldname = 'TSTPNM'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'TIMESTMP'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'CONTTIMESTMP'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'OWNER'
                 CHANGING  cg_metadata  = <lg_details> ).

    io_xml->add( iv_name = 'ODSO'
                 ig_data = <lg_details> ).

    io_xml->add( iv_name = 'INFOOBJECTS'
                 ig_data = <lt_infoobjects> ).

    io_xml->add( iv_name = 'NAVIGATION'
                 ig_data = <lt_navigation> ).

    io_xml->add( iv_name = 'INDEXES'
                 ig_data = <lt_indexes> ).

    io_xml->add( iv_name = 'INDEX_IOBJ'
                 ig_data = <lt_index_iobj> ).

  ENDMETHOD.
ENDCLASS.
