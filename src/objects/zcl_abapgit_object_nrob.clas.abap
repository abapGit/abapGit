CLASS zcl_abapgit_object_nrob DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      delete_intervals IMPORTING iv_object TYPE inri-object
                       RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_nrob IMPLEMENTATION.


  METHOD delete_intervals.

    DATA: lv_error    TYPE c LENGTH 1,
          ls_error    TYPE inrer,
          lt_list     TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY,
          lt_error_iv TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        object                     = iv_object
      TABLES
        interval                   = lt_list
      EXCEPTIONS
        nr_range_nr1_not_found     = 1
        nr_range_nr1_not_intern    = 2
        nr_range_nr2_must_be_space = 3
        nr_range_nr2_not_extern    = 4
        nr_range_nr2_not_found     = 5
        object_not_found           = 6
        subobject_must_be_space    = 7
        subobject_not_found        = 8
        OTHERS                     = 9.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lines( lt_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CLEAR <ls_list>-nrlevel.
      <ls_list>-procind = 'D'.
    ENDLOOP.

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        object           = iv_object
      IMPORTING
        error            = ls_error
        error_occured    = lv_error
      TABLES
        error_iv         = lt_error_iv
        interval         = lt_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0 OR lv_error = abap_true.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = iv_object
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_objectid TYPE cdhdr-objectid,
          lt_cdhdr    TYPE cdhdr_tab.

    FIELD-SYMBOLS: <ls_cdhdr> LIKE LINE OF lt_cdhdr.


    lv_objectid = ms_item-obj_name.

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        objectclass                = 'NRKROBJ'
        objectid                   = lv_objectid
      TABLES
        i_cdhdr                    = lt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    SORT lt_cdhdr BY udate DESCENDING utime DESCENDING.

    READ TABLE lt_cdhdr INDEX 1 ASSIGNING <ls_cdhdr>.
    ASSERT sy-subrc = 0.

    rv_user = <ls_cdhdr>-username.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_object TYPE tnro-object.


    lv_object = ms_item-obj_name.

    delete_intervals( lv_object ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_DELETE'
      EXPORTING
        language           = mv_language
        object             = lv_object
      EXCEPTIONS
        delete_not_allowed = 1
        object_not_found   = 2
        wrong_indicator    = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING cg_data = ls_text ).

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      GET TIME STAMP FIELD <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
      EXPORTING
        indicator                 = 'I'
        object_attributes         = ls_attributes
        object_text               = ls_text
      TABLES
        errors                    = lt_errors
      EXCEPTIONS
        object_already_exists     = 1
        object_attributes_missing = 2
        object_not_found          = 3
        object_text_missing       = 4
        wrong_indicator           = 5
        OTHERS                    = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).
    corr_insert( iv_package ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSNRO'.
    ls_bcdata-dynpro   = '0150'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'NRIV-OBJECT'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DISP'.
    APPEND ls_bcdata TO lt_bcdata.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SNRO'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = mv_language
        object            = lv_object
      IMPORTING
        object_attributes = ls_attributes
        object_text       = ls_text
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_text ).

  ENDMETHOD.
ENDCLASS.
