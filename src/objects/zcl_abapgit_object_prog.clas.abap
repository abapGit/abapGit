CLASS zcl_abapgit_object_prog DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tpool_i18n,
             language TYPE langu,
             textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
           END OF ty_tpool_i18n,
           ty_tpools_i18n TYPE STANDARD TABLE OF ty_tpool_i18n,
           BEGIN OF ty_lxe_i18n,
             source_lang TYPE lxeisolang,
             target_lang TYPE lxeisolang,
             custmnr     TYPE lxecustmnr,
             objtype     TYPE trobjtype,
             objname     TYPE lxeobjname,
             text_pairs  TYPE lxe_tt_pcx_s1,
           END OF ty_lxe_i18n,
           ty_tlxe_i18n TYPE STANDARD TABLE OF ty_lxe_i18n WITH DEFAULT KEY.

    CONSTANTS: c_longtext_id_prog TYPE dokil-id VALUE 'RE'.

    METHODS:
      serialize_texts
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      deserialize_texts
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      is_program_locked
        RETURNING
          VALUE(rv_is_program_locked) TYPE abap_bool
        RAISING
          zcx_abapgit_exception,
      get_lang_iso4
        IMPORTING
          iv_src         TYPE spras
        RETURNING
          VALUE(rv_iso4) TYPE lxeisolang ,
      get_lxe_object_list
        RETURNING
          VALUE(rt_obj_list) TYPE lxe_tt_colob,
      get_lxe_texts
        RETURNING VALUE(rt_lxe_texts) TYPE ty_tlxe_i18n,
      deserialize_lxe_texts
        IMPORTING
          it_lxe_texts TYPE ty_tlxe_i18n.
    TYPES:
      ty_languages TYPE STANDARD TABLE OF langu WITH DEFAULT KEY.
    METHODS get_installed_languages
      RETURNING
        VALUE(rt_installed_languages) TYPE ty_languages.

ENDCLASS.



CLASS zcl_abapgit_object_prog IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA:
      lt_tpool_i18n TYPE ty_tpools_i18n,
      lt_tpool      TYPE textpool_table,
      lt_lxe_texts  TYPE ty_tlxe_i18n.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.


    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = ms_item-obj_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.
* -->    https://github.com/abapGit/abapGit/issues/2424
    ii_xml->read( EXPORTING iv_name = 'LXE_TEXTS'
                  CHANGING  cg_data = lt_lxe_texts ).
    deserialize_lxe_texts( lt_lxe_texts ).
* <--    https://github.com/abapGit/abapGit/issues/2424
  ENDMETHOD.


  METHOD is_program_locked.

    rv_is_program_locked = exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                                    iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table,
          lt_lxe_texts  TYPE STANDARD TABLE OF ty_lxe_i18n.

    FIELD-SYMBOLS:
      <ls_tpool>         LIKE LINE OF lt_tpool_i18n.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = ms_item-obj_name
      AND language <> mv_language ##TOO_MANY_ITAB_FIELDS.

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

* -->    https://github.com/abapGit/abapGit/issues/2424
    lt_lxe_texts = get_lxe_texts(  ).

    IF lines( lt_lxe_texts ) > 0.
      ii_xml->add( iv_name = 'LXE_TEXTS'
                   ig_data = lt_lxe_texts ).
    ENDIF.
* <--    https://github.com/abapGit/abapGit/issues/2424
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
      lv_program    LIKE sy-repid,
      lv_obj_name   TYPE e071-obj_name,
      lv_corrnumber TYPE e071-trkorr.

    lv_program = ms_item-obj_name.
    lv_corrnumber = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        corrnumber                 = lv_corrnumber
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
      zcx_abapgit_exception=>raise( |Error from RS_DELETE_PROGRAM: { sy-subrc }| ).
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

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data  = ls_progdir ).
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

    deserialize_longtexts( io_xml ).

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

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

* see SAP note 1025291, run report DELETE_TADIR_FOR_EIMP_INCLUDE to clean bad TADIR entries
    ASSERT NOT ms_item-obj_name CP '*=E'.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = mo_files ).

    " Texts serializing (translations)
    serialize_texts( io_xml ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_prog ).

  ENDMETHOD.
  METHOD get_lang_iso4.
    CALL FUNCTION 'LXE_T002_CONVERT_2_TO_4'
      EXPORTING
        old_r3_lang = iv_src
      IMPORTING
        new_lang    = rv_iso4.
  ENDMETHOD.


  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = ms_item-obj_name.
    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid    = 'R3TR'
        object   = 'PROG'
        obj_name = lv_object_name
      TABLES
        ex_colob = rt_obj_list.

  ENDMETHOD.
  METHOD get_lxe_texts.
    DATA:
      lt_obj_list            TYPE lxe_tt_colob,
      lt_installed_languages TYPE TABLE OF langu,

      ls_lxe_text_item       TYPE ty_lxe_i18n.

    FIELD-SYMBOLS:
      <lv_language>      TYPE langu,
      <lv_lxe_object>       TYPE lxe_colob.

    lt_obj_list = get_lxe_object_list( ).

    lt_installed_languages = get_installed_languages( ).

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT lt_installed_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = get_lang_iso4( mv_language ).
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        CLEAR ls_lxe_text_item-text_pairs.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = ls_lxe_text_item-source_lang
            t_lang    = ls_lxe_text_item-target_lang
            custmnr   = ls_lxe_text_item-custmnr
            objtype   = ls_lxe_text_item-objtype
            objname   = ls_lxe_text_item-objname
          TABLES
            lt_pcx_s1 = ls_lxe_text_item-text_pairs.

        DELETE ls_lxe_text_item-text_pairs WHERE t_text IS INITIAL. " No Target Text, no translation to be transported
        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO rt_lxe_texts.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD deserialize_lxe_texts.
    DATA: ls_lxe_item       TYPE ty_lxe_i18n,
          lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    LOOP AT it_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill
      CLEAR: lt_text_pairs_tmp.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
          read_only = abap_false
        TABLES
          lt_pcx_s1 = lt_text_pairs_tmp.

      "Call actual Write FM
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
        TABLES
          lt_pcx_s1 = ls_lxe_item-text_pairs.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_installed_languages.

    DATA lv_index TYPE i.
    DATA lv_length TYPE i.
    DATA lv_char TYPE c.
    DATA lv_installed_languages TYPE string.

    CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
      IMPORTING
        languages       = lv_installed_languages
      EXCEPTIONS
        sapgparam_error = 1                " Error requesting profile parameter
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    lv_length = strlen( lv_installed_languages ).
    lv_index = 0.
    WHILE lv_index < lv_length.
      lv_char = lv_installed_languages+lv_index(1).
      APPEND lv_char TO rt_installed_languages.
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
