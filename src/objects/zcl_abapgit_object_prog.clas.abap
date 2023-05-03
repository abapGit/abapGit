CLASS zcl_abapgit_object_prog DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tpool_i18n,
        language TYPE langu,
        textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
      END OF ty_tpool_i18n .
    TYPES:
      ty_tpools_i18n TYPE STANDARD TABLE OF ty_tpool_i18n .

    CONSTANTS c_longtext_id_prog TYPE dokil-id VALUE 'RE' ##NO_TEXT.

    METHODS deserialize_with_ext
      IMPORTING
        !is_progdir TYPE ty_progdir
        !it_source  TYPE abaptxt255_tab
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS is_program_locked
      RETURNING
        VALUE(rv_is_program_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_prog IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.


    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = ms_item-obj_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_with_ext.

    " Special treatment for extensions
    " If the program name exceeds 30 characters it is not a usual ABAP program but might be
    " some extension, which requires the internal addition EXTENSION TYPE
    " https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm?file=abapinsert_report_internal.htm
    " This e.g. occurs in case of transportable Code Inspector variants (ending with ===VC)

    INSERT REPORT is_progdir-name
      FROM it_source
      STATE 'I'
      EXTENSION TYPE is_progdir-name+30
      PROGRAM TYPE is_progdir-subc.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from INSERT REPORT .. EXTENSION TYPE' ).
    ENDIF.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = is_progdir
        i_progname   = is_progdir-name
        i_state      = 'I'
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error updating program directory' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add(
      iv_type = 'REPS'
      iv_name = is_progdir-name ).

  ENDMETHOD.


  METHOD is_program_locked.

    rv_is_program_locked = exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                                    iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lt_tpool_i18n      TYPE ty_tpools_i18n,
          lt_tpool           TYPE textpool_table,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

    zcl_abapgit_lxe_texts=>add_iso_langs_to_lang_filter(
      EXPORTING it_iso_filter      = ii_xml->i18n_params( )-translation_languages
      CHANGING  ct_language_filter = lt_language_filter ).

    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = ms_item-obj_name
      AND language <> mv_language
      AND language IN lt_language_filter ##TOO_MANY_ITAB_FIELDS.

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL ms_item-obj_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_program  LIKE sy-repid,
      lv_obj_name TYPE e071-obj_name.

    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        corrnumber                 = iv_transport
        program                    = lv_program
        suppress_popup             = abap_true
        mass_delete_call           = abap_true
        tadir_devclass             = iv_package
        force_delete_used_includes = abap_true
      EXCEPTIONS
        enqueue_lock               = 1
        object_not_found           = 2
        permission_failure         = 3
        reject_deletion            = 4
        OTHERS                     = 5.
    IF sy-subrc = 2.
      " Drop also any inactive code that is left in REPOSRC
      DELETE REPORT lv_program ##SUBRC_OK.

      " Remove inactive objects from work area
      lv_obj_name = lv_program.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPS'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPT'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    delete_longtexts( c_longtext_id_prog ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_program_name TYPE programm,
          ls_progdir      TYPE ty_progdir,
          lt_tpool        TYPE textpool_table,
          lt_dynpros      TYPE ty_dynpro_tt,
          lt_tpool_ext    TYPE zif_abapgit_definitions=>ty_tpool_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE abaptxt255_tab.

    " Add R3TR PROG to transport first, otherwise we get several LIMUs
    corr_insert( iv_package ).

    lv_program_name = ms_item-obj_name.

    lt_source = zif_abapgit_object~mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data  = ls_progdir ).

    IF strlen( lv_program_name ) > 30.

      " Objects with extension for example transportable Code Inspector variants (ending with ===VC)
      deserialize_with_ext( is_progdir = ls_progdir
                            it_source  = lt_source ).

    ELSE.

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

      io_xml->read( EXPORTING iv_name = 'DYNPROS'
                    CHANGING cg_data  = lt_dynpros ).
      deserialize_dynpros( lt_dynpros ).

      io_xml->read( EXPORTING iv_name = 'CUA'
                    CHANGING cg_data  = ls_cua ).
      deserialize_cua( iv_program_name = lv_program_name
                       is_cua = ls_cua ).

      " Texts deserializing (English)
      deserialize_textpool( iv_program = lv_program_name
                            it_tpool   = lt_tpool ).

      " Texts deserializing (translations)
      deserialize_texts( io_xml ).
      deserialize_lxe_texts( io_xml ).

      deserialize_longtexts( ii_xml         = io_xml
                             iv_longtext_id = c_longtext_id_prog ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_progname TYPE reposrc-progname.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name.
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
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    IF is_program_locked( ) = abap_true
        OR is_any_dynpro_locked( ms_item-obj_name ) = abap_true
        OR is_cua_locked( ms_item-obj_name ) = abap_true
        OR is_text_locked( ms_item-obj_name ) = abap_true.

      rv_is_locked = abap_true.

    ENDIF.

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

* see SAP note 1025291, run report DELETE_TADIR_FOR_EIMP_INCLUDE to clean bad TADIR entries
    ASSERT NOT ms_item-obj_name CP '*=E'.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = zif_abapgit_object~mo_files ).

    " Texts serializing (translations)
    IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
      serialize_texts( io_xml ).
    ELSE.
      serialize_lxe_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_prog ).

  ENDMETHOD.
ENDCLASS.
