*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_NROB
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      delete_intervals IMPORTING iv_object TYPE inri-object
                       RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_nrob DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-late_deser = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


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
      lcx_exception=>raise( 'error from NUMBER_RANGE_OBJECT_READ' ).
    ENDIF.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_text ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING cg_data = ls_text ).

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
      lcx_exception=>raise( 'error from NUMBER_RANGE_OBJECT_UPDATE' ).
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from NUMBER_RANGE_OBJECT_CLOSE' ).
    ENDIF.

    tadir_insert( iv_package ).

  ENDMETHOD.                    "deserialize

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
      lcx_exception=>raise( 'error from NUMBER_RANGE_INTERVAL_LIST' ).
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
      lcx_exception=>raise( 'error from NUMBER_RANGE_INTERVAL_UPDATE' ).
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = iv_object
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from NUMBER_RANGE_UPDATE_CLOSE' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~delete.

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
      lcx_exception=>raise( 'error from NUMBER_RANGE_OBJECT_DELETE' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    lcx_exception=>raise( 'todo' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_nrob IMPLEMENTATION
