CLASS zcl_abapgit_object_sots DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_sots,
        header  TYPE sotr_headu,
        entries TYPE sotr_textl_tt,
      END OF ty_sots,
      ty_sots_tt TYPE STANDARD TABLE OF ty_sots
                    WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      read_sots
        RETURNING
          VALUE(rt_sots) TYPE ty_sots_tt,

      create_sots
        IMPORTING
          is_sots    TYPE ty_sots
          iv_package TYPE devclass
          iv_object  TYPE trobjtype
        RAISING
          zcx_abapgit_exception,

      get_raw_text_filename
        IMPORTING
          is_entry           TYPE sotr_textl
        RETURNING
          VALUE(rv_filename) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_object_sots IMPLEMENTATION.


  METHOD create_sots.

    " Reimplementation of SOTR_STRING_CREATE_CONCEPT because we can't supply
    " concept and it would then be generated.

    DATA: lv_subrc                 TYPE sy-subrc,
          lv_source_langu          TYPE spras,
          ls_header                TYPE btfr_head,
          lv_flag_is_string        TYPE btfr_flag VALUE abap_true,
          lt_text_tab              TYPE sotr_text_tt,
          lv_concept_default       TYPE sotr_conc,
          lt_entries               TYPE sotr_textl_tt,
          lv_concept               LIKE is_sots-header-concept,
          lv_flag_correction_entry TYPE abap_bool VALUE abap_true.

    lt_entries = is_sots-entries.

    ls_header-paket          = iv_package.
    ls_header-crea_lan       = mv_language.
    ls_header-alias_name     = is_sots-header-alias_name.
    lv_source_langu          = mv_language.
    lv_concept               = is_sots-header-concept.

    PERFORM btfr_create
      IN PROGRAM saplsotr_db_string
      USING iv_object
            lv_source_langu
            lv_flag_correction_entry
            lv_flag_is_string
      CHANGING lt_text_tab
               lt_entries
               ls_header
               lv_concept
               lv_concept_default
               lv_subrc.

    CASE lv_subrc.
      WHEN 1.
        zcx_abapgit_exception=>raise( |No entry found| ).
      WHEN 2.
        zcx_abapgit_exception=>raise( |OTR concept not found| ).
      WHEN 3.
        zcx_abapgit_exception=>raise( |Enter a permitted object type| ).
      WHEN 4.
        "The concept will be created in the non-original system (not an error)
        RETURN.
      WHEN 5.
        zcx_abapgit_exception=>raise( |Invalid alias| ).
      WHEN 6.
        zcx_abapgit_exception=>raise( |No correction entry has been created| ).
      WHEN 7.
        zcx_abapgit_exception=>raise( |Error in database operation| ).
      WHEN 9.
        zcx_abapgit_exception=>raise( |Action canceled by user| ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_raw_text_filename.

    DATA lv_langu TYPE string.

    " Lower case language codes can cause duplicate filenames therefore add suffix to make them unique
    " Note: Using ISO code would be better but is not compatible with existing files
    lv_langu = is_entry-langu.
    IF lv_langu = to_lower( lv_langu ).
      lv_langu = lv_langu && '-'.
    ENDIF.

    rv_filename =
        to_lower( |{ is_entry-concept }_|
               && |{ lv_langu         }_|
               && |{ is_entry-object  }_|
               && |{ is_entry-lfd_num }| ).

  ENDMETHOD.


  METHOD read_sots.

    DATA: lt_sotr_head TYPE STANDARD TABLE OF sotr_headu,
          lt_objects   TYPE sotr_objects,
          lv_object    LIKE LINE OF lt_objects,
          ls_sots      LIKE LINE OF rt_sots.

    FIELD-SYMBOLS: <ls_sotr_head> TYPE sotr_head,
                   <ls_entry>     LIKE LINE OF ls_sots-entries.


    SELECT * FROM sotr_headu
             INTO TABLE lt_sotr_head
             WHERE paket = ms_item-obj_name
             ORDER BY PRIMARY KEY.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr_head>-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      " Handled by object serializer
      CHECK lv_object <> 'SICF' AND lv_object <> 'CPUB'.

      CLEAR: ls_sots.

      CALL FUNCTION 'SOTR_STRING_GET_CONCEPT'
        EXPORTING
          concept        = <ls_sotr_head>-concept
        IMPORTING
          header         = ls_sots-header
          entries        = ls_sots-entries
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR:
        ls_sots-header-paket,
        ls_sots-header-crea_name,
        ls_sots-header-crea_tstut,
        ls_sots-header-chan_name,
        ls_sots-header-chan_tstut.

      LOOP AT ls_sots-entries ASSIGNING <ls_entry>.
        CLEAR: <ls_entry>-version,
               <ls_entry>-crea_name,
               <ls_entry>-crea_tstut,
               <ls_entry>-chan_name,
               <ls_entry>-chan_tstut.
      ENDLOOP.

      INSERT ls_sots INTO TABLE rt_sots.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE chan_name FROM sotr_headu INTO rv_user
      WHERE paket = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots> TYPE ty_sots.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.
      " Remove any usage to ensure deletion, see function module BTFR_CHECK
      DELETE FROM sotr_useu WHERE concept = <ls_sots>-header-concept.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept             = <ls_sots>-header-concept
          flag_string         = abap_true
        EXCEPTIONS
          text_not_found      = 1
          invalid_package     = 2
          text_not_changeable = 3
          text_enqueued       = 4
          no_correction       = 5
          parameter_error     = 6
          OTHERS              = 7.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lt_sots    TYPE ty_sots_tt,
          lt_objects TYPE sotr_objects,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> LIKE LINE OF <ls_sots>-entries.

    io_xml->read(
      EXPORTING
        iv_name = 'SOTS'
      CHANGING
        cg_data = lt_sots ).

    tadir_insert( iv_package ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      CLEAR: lt_objects.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sots>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from SOTR_OBJECT_GET_OBJECTS' ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        TRY.
            <ls_entry>-text = zif_abapgit_object~mo_files->read_string(
              iv_extra = get_raw_text_filename( <ls_entry> )
              iv_ext   = 'txt' ).

          CATCH zcx_abapgit_exception.
            " Most probably file not found -> ignore
            CONTINUE.
        ENDTRY.

      ENDLOOP.

      create_sots(
          is_sots    = <ls_sots>
          iv_package = iv_package
          iv_object  = lv_object ).

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_object_type TYPE trobjtype,
          lv_object_name TYPE trobj_name.

    lv_object_type = ms_item-obj_type.
    lv_object_name = ms_item-obj_name.

    CALL FUNCTION 'SOTR_WBO_OBJECTS_CHECK'
      EXPORTING
        pgmid          = 'R3TR'
        object         = lv_object_type
        obj_name       = lv_object_name
      IMPORTING
        object_exist   = rv_bool
      EXCEPTIONS
        unknown_object = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      rv_bool = abap_false.
    ENDIF.

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
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
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

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> TYPE sotr_textl.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        zif_abapgit_object~mo_files->add_string(
          iv_extra  = get_raw_text_filename( <ls_entry> )
          iv_ext    = 'txt'
          iv_string = <ls_entry>-text ).

        CLEAR: <ls_entry>-text.

      ENDLOOP.

    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).

  ENDMETHOD.
ENDCLASS.
