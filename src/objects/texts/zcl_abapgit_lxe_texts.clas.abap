CLASS zcl_abapgit_lxe_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_lxe_texts .

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_translation_languages
      IMPORTING
        !iv_main_language   TYPE spras
        !it_i18n_languages  TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS get_installed_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS convert_lang_string_to_table
      IMPORTING
        !iv_langs              TYPE string
        !iv_skip_main_language TYPE spras OPTIONAL
      RETURNING
        VALUE(rt_languages)    TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS convert_table_to_lang_string
      IMPORTING
        !it_languages   TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rv_langs) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS detect_unsupported_languages
      IMPORTING
        !it_languages                   TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_unsupported_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS trim_saplangu_by_iso
      IMPORTING
        it_iso_filter TYPE zif_abapgit_definitions=>ty_languages
      CHANGING
        ct_sap_langs  TYPE zif_abapgit_definitions=>ty_sap_langu_tab
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS trim_tab_w_saplang_by_iso
      IMPORTING
        it_iso_filter       TYPE zif_abapgit_definitions=>ty_languages
        iv_lang_field_name  TYPE abap_compname
        iv_keep_master_lang TYPE sy-langu OPTIONAL
      CHANGING
        ct_tab              TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_custmnr TYPE lxecustmnr VALUE '999999'.
    " The value for ABAP system translation is always 999999 (from lxecustmnr docs)

    TYPES:
      BEGIN OF ty_lxe_translation,
        source_lang TYPE lxeisolang,
        target_lang TYPE lxeisolang,
        custmnr     TYPE lxecustmnr,
        objtype     TYPE trobjtype,
        objname     TYPE lxeobjname,
        text_pairs  TYPE zif_abapgit_lxe_texts=>ty_text_pairs,
      END OF ty_lxe_translation.
    TYPES:
      ty_lxe_translations TYPE STANDARD TABLE OF ty_lxe_translation WITH DEFAULT KEY .

    CLASS-DATA gt_installed_languages_cache TYPE zif_abapgit_definitions=>ty_languages.
    CLASS-DATA gt_supported_obj_types TYPE STANDARD TABLE OF tadir-object.

    DATA mo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA mi_xml         TYPE REF TO zif_abapgit_xml_output.
    DATA mo_files       TYPE REF TO zcl_abapgit_objects_files.

    METHODS serialize_xml
      IMPORTING
        !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
        !iv_object_type   TYPE tadir-object
        !iv_object_name   TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_as_po
      IMPORTING
        !iv_object_type   TYPE tadir-object
        !iv_object_name   TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception .

    METHODS get_lang_iso4
      IMPORTING
        iv_src         TYPE laiso
      RETURNING
        VALUE(rv_iso4) TYPE lxeisolang
      RAISING
        zcx_abapgit_exception.
    METHODS get_lxe_object_list
      IMPORTING
        iv_object_type     TYPE trobjtype
        iv_object_name     TYPE sobj_name
      RETURNING
        VALUE(rt_obj_list) TYPE lxe_tt_colob .
    METHODS read_lxe_object_text_pair
      IMPORTING
        iv_s_lang                TYPE lxeisolang
        iv_t_lang                TYPE lxeisolang
        iv_custmnr               TYPE lxecustmnr
        iv_objtype               TYPE trobjtype
        iv_objname               TYPE lxeobjname
        iv_read_only             TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_text_pairs_tmp) TYPE ty_lxe_translation-text_pairs
      RAISING
        zcx_abapgit_exception.
    METHODS write_lxe_object_text_pair
      IMPORTING
        iv_s_lang  TYPE lxeisolang
        iv_t_lang  TYPE lxeisolang
        iv_custmnr TYPE lxecustmnr
        iv_objtype TYPE trobjtype
        iv_objname TYPE lxeobjname
        it_pcx_s1  TYPE ty_lxe_translation-text_pairs
      RAISING
        zcx_abapgit_exception.
    METHODS read_text_items
      IMPORTING
        iv_object_type       TYPE tadir-object
        iv_object_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_text_items) TYPE ty_lxe_translations
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS langu_to_laiso_safe
      IMPORTING
        iv_langu        TYPE sy-langu
      RETURNING
        VALUE(rv_laiso) TYPE laiso
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS iso4_to_iso2
      IMPORTING
        iv_lxe_lang     TYPE lxeisolang
      RETURNING
        VALUE(rv_laiso) TYPE laiso
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS check_langs_versus_installed
      IMPORTING
        it_languages    TYPE zif_abapgit_definitions=>ty_languages
        it_installed    TYPE zif_abapgit_definitions=>ty_languages
      EXPORTING
        et_intersection TYPE zif_abapgit_definitions=>ty_languages
        et_missfits     TYPE zif_abapgit_definitions=>ty_languages.
ENDCLASS.



CLASS ZCL_ABAPGIT_LXE_TEXTS IMPLEMENTATION.


  METHOD check_langs_versus_installed.

    DATA lt_installed_hash TYPE HASHED TABLE OF laiso WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <lv_lang> LIKE LINE OF it_languages.

    CLEAR: et_intersection, et_missfits.
    lt_installed_hash = it_installed.

    LOOP AT it_languages ASSIGNING <lv_lang>.
      READ TABLE lt_installed_hash WITH KEY table_line = <lv_lang> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lv_lang> TO et_intersection.
      ELSE.
        APPEND <lv_lang> TO et_missfits.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    APPEND 'CLAS' TO gt_supported_obj_types.
    APPEND 'DOMA' TO gt_supported_obj_types.
    APPEND 'DTEL' TO gt_supported_obj_types.
    APPEND 'FUGR' TO gt_supported_obj_types.
    APPEND 'MSAG' TO gt_supported_obj_types.
    APPEND 'PARA' TO gt_supported_obj_types.
    APPEND 'PROG' TO gt_supported_obj_types.
    APPEND 'SHI3' TO gt_supported_obj_types.
    APPEND 'TABL' TO gt_supported_obj_types.
    APPEND 'TRAN' TO gt_supported_obj_types.
    APPEND 'VIEW' TO gt_supported_obj_types.

  ENDMETHOD.


  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str          TYPE string_table,
      lv_laiso              TYPE laiso,
      lv_skip_main_lang_iso TYPE laiso.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).
      APPEND lv_laiso TO rt_languages.
    ENDLOOP.

    IF iv_skip_main_language IS NOT INITIAL.
      lv_skip_main_lang_iso = langu_to_laiso_safe( iv_skip_main_language ).
      DELETE rt_languages WHERE table_line = lv_skip_main_lang_iso.
    ENDIF.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.


  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table.

    FIELD-SYMBOLS:
      <lv_lang> LIKE LINE OF it_languages,
      <lv_str>  TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_lang>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_lang> = '*'.
        CLEAR lt_langs_str.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = <lv_lang>.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.


  METHOD detect_unsupported_languages.

    check_langs_versus_installed(
      EXPORTING
        it_languages = it_languages
        it_installed = get_installed_languages( )
      IMPORTING
        et_missfits = rt_unsupported_languages ).

  ENDMETHOD.


  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_langu               TYPE sy-langu,
      lv_laiso               TYPE laiso,
      lv_installed_languages TYPE string,
      lt_language_filter     TYPE zif_abapgit_environment=>ty_system_language_filter.

    IF gt_installed_languages_cache IS INITIAL.
      CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
        IMPORTING
          languages       = lv_installed_languages
        EXCEPTIONS
          sapgparam_error = 1                " Error requesting profile parameter
          OTHERS          = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Fail to get system SYSTEM_INSTALLED_LANGUAGES' ).
      ENDIF.

      lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

      DO strlen( lv_installed_languages ) TIMES.
        lv_index = sy-index - 1.
        lv_langu = lv_installed_languages+lv_index(1).

        IF lv_langu NOT IN lt_language_filter.
          CONTINUE.
        ENDIF.

        lv_laiso = langu_to_laiso_safe( lv_langu ).
        APPEND lv_laiso TO gt_installed_languages_cache.
      ENDDO.
    ENDIF.

    rt_languages = gt_installed_languages_cache.

  ENDMETHOD.


  METHOD get_lang_iso4.

    DATA lv_lang_iso639 TYPE laiso.
    DATA lv_country     TYPE land1.

    cl_i18n_languages=>sap2_to_iso639_1(
      EXPORTING
        im_lang_sap2   = iv_src
      IMPORTING
        ex_lang_iso639 = lv_lang_iso639
        ex_country     = lv_country
      EXCEPTIONS
        no_assignment  = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to convert [{ iv_src }] lang to iso639| ).
    ENDIF.

    CONCATENATE lv_lang_iso639 lv_country INTO rv_iso4.

  ENDMETHOD.


  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = iv_object_type
        obj_name        = lv_object_name
      TABLES
        ex_colob        = rt_obj_list
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN. " Ignore error and return empty list
    ENDIF.

  ENDMETHOD.


  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    DATA lv_main_lang_laiso TYPE laiso.

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        check_langs_versus_installed(
          EXPORTING
            it_languages = it_i18n_languages
            it_installed = get_installed_languages( )
          IMPORTING
            et_intersection = rt_languages ).
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    lv_main_lang_laiso = langu_to_laiso_safe( iv_main_language ).
    DELETE rt_languages WHERE table_line = lv_main_lang_laiso.

  ENDMETHOD.


  METHOD iso4_to_iso2.
    rv_laiso = iv_lxe_lang+0(2).
  ENDMETHOD.


  METHOD langu_to_laiso_safe.

    cl_i18n_languages=>sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_langu
      RECEIVING
        re_lang_sap2  = rv_laiso
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Could not convert lang [{ iv_langu }] to ISO| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.
        IF lv_error IS NOT INITIAL.
          zcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.

    ENDTRY.

  ENDMETHOD.


  METHOD read_text_items.

    DATA:
      lt_obj_list      TYPE lxe_tt_colob,
      lv_main_lang     TYPE lxeisolang,
      ls_lxe_text_item LIKE LINE OF rt_text_items.

    FIELD-SYMBOLS:
      <lv_language>   LIKE LINE OF mo_i18n_params->ms_params-translation_languages,
      <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( mo_i18n_params->ms_params-main_language ) ).

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT mo_i18n_params->ms_params-translation_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = lv_main_lang.
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        ls_lxe_text_item-text_pairs = read_lxe_object_text_pair(
          iv_s_lang    = ls_lxe_text_item-source_lang
          iv_t_lang    = ls_lxe_text_item-target_lang
          iv_custmnr   = ls_lxe_text_item-custmnr
          iv_objtype   = ls_lxe_text_item-objtype
          iv_objname   = ls_lxe_text_item-objname ).

        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO rt_text_items.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_as_po.

    DATA lt_lxe_texts TYPE ty_lxe_translations.
    DATA lo_po_file TYPE REF TO zcl_abapgit_po_file.
    DATA lv_lang LIKE LINE OF mo_i18n_params->ms_params-translation_languages.
    FIELD-SYMBOLS <ls_translation> LIKE LINE OF lt_lxe_texts.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    LOOP AT mo_i18n_params->ms_params-translation_languages INTO lv_lang.
      lv_lang = to_lower( lv_lang ).
      CREATE OBJECT lo_po_file
        EXPORTING
          iv_lang = lv_lang.
      LOOP AT lt_lxe_texts ASSIGNING <ls_translation>.
        IF iso4_to_iso2( <ls_translation>-target_lang ) = lv_lang.
          lo_po_file->push_text_pairs(
            iv_objtype    = <ls_translation>-objtype
            iv_objname    = <ls_translation>-objname
            it_text_pairs = <ls_translation>-text_pairs ).
        ENDIF.
      ENDLOOP.
      mo_files->add_i18n_file( lo_po_file ).
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_xml.

    DATA lt_lxe_texts TYPE ty_lxe_translations.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    IF lines( lt_lxe_texts ) > 0.
      mi_xml->add(
        iv_name = iv_lxe_text_name
        ig_data = lt_lxe_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD trim_saplangu_by_iso.

    DATA lv_langu TYPE sy-langu.
    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    IF it_iso_filter IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_sap_langs INTO lv_langu.
      lv_index = sy-tabix.

      cl_i18n_languages=>sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = lv_langu
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE it_iso_filter TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD trim_tab_w_saplang_by_iso.

    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.
    FIELD-SYMBOLS <lv_langu> TYPE sy-langu.

    IF it_iso_filter IS INITIAL OR iv_lang_field_name IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_tab ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ASSIGN COMPONENT iv_lang_field_name OF STRUCTURE <ls_i> TO <lv_langu>.
      ASSERT sy-subrc = 0.

      IF <lv_langu> = iv_keep_master_lang.
        CONTINUE. " Just keep it
      ENDIF.

      cl_i18n_languages=>sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = <lv_langu>
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE it_iso_filter TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD write_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = it_pcx_s1.
        IF lv_error IS NOT INITIAL.
          zcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          TABLES
            lt_pcx_s1 = it_pcx_s1.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~deserialize.

    DATA:
      lt_lxe_texts      TYPE ty_lxe_translations,
      ls_lxe_item       LIKE LINE OF lt_lxe_texts,
      lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    ii_xml->read(
      EXPORTING iv_name = iv_lxe_text_name
      CHANGING  cg_data = lt_lxe_texts ).

    LOOP AT lt_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill

      lt_text_pairs_tmp = read_lxe_object_text_pair(
        iv_s_lang    = ls_lxe_item-source_lang
        iv_t_lang    = ls_lxe_item-target_lang
        iv_custmnr   = ls_lxe_item-custmnr
        iv_objtype   = ls_lxe_item-objtype
        iv_objname   = ls_lxe_item-objname
        iv_read_only = abap_false ).

      "Call actual Write FM
      write_lxe_object_text_pair(
        iv_s_lang  = ls_lxe_item-source_lang
        iv_t_lang  = ls_lxe_item-target_lang
        iv_custmnr = ls_lxe_item-custmnr
        iv_objtype = ls_lxe_item-objtype
        iv_objname = ls_lxe_item-objname
        it_pcx_s1  = ls_lxe_item-text_pairs ).

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~deserialize_from_po.

    DATA lv_lang LIKE LINE OF is_i18n_params-translation_languages.
    DATA li_po TYPE REF TO zif_abapgit_i18n_file.
    DATA lt_text_pairs_tmp TYPE ty_lxe_translation-text_pairs.
    DATA lt_obj_list TYPE lxe_tt_colob.
    DATA lv_main_lang TYPE lxeisolang.
    DATA lv_target_lang TYPE lxeisolang.

    FIELD-SYMBOLS <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( is_i18n_params-main_language ) ).

    LOOP AT is_i18n_params-translation_languages INTO lv_lang.
      lv_target_lang = get_lang_iso4( lv_lang ).

      LOOP AT it_po_files INTO li_po.
        IF li_po->lang( ) = to_lower( lv_lang ). " Not quite efficient but the list is presumably very short
          EXIT.
        ELSE.
          CLEAR li_po.
        ENDIF.
      ENDLOOP.

      CHECK li_po IS BOUND. " Ignore missing files, missing translation is not a crime

      LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.

        lt_text_pairs_tmp = read_lxe_object_text_pair(
          iv_s_lang    = lv_main_lang
          iv_t_lang    = lv_target_lang
          iv_custmnr   = <lv_lxe_object>-custmnr
          iv_objtype   = <lv_lxe_object>-objtype
          iv_objname   = <lv_lxe_object>-objname
          iv_read_only = abap_false ).

        li_po->translate( CHANGING ct_text_pairs = lt_text_pairs_tmp ).
        " TODO maybe optimize, check if values have changed

        write_lxe_object_text_pair(
          iv_s_lang  = lv_main_lang
          iv_t_lang  = lv_target_lang
          iv_custmnr = <lv_lxe_object>-custmnr
          iv_objtype = <lv_lxe_object>-objtype
          iv_objname = <lv_lxe_object>-objname
          it_pcx_s1  = lt_text_pairs_tmp ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~serialize.

    READ TABLE gt_supported_obj_types TRANSPORTING NO FIELDS WITH KEY table_line = iv_object_type.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    mo_i18n_params = io_i18n_params.
    mi_xml         = ii_xml.
    mo_files       = io_files.

    " MAYBE TODO
    " if other formats are needed, including the old in-XML approach
    " here is the place to implement it. Supposed architecture:
    " I18N_PARAMS should contain an option which format to use
    " The option should be originally maintained in dot_abapgit structures (e.g. `translation_storage_format`)
    " Consequently it comes here
    " The serialize method can read it and call a corresponding submethod,
    " e.g. serialize_xml or serialize_as_po or ...
    " both ii_xml and io_files are accessible intentionally to enable both XML based or file based formats
    " access to json can be easily added too,
    " or maybe (maybe) some kind of zif_ag_object_ctl with all DAO instead

    serialize_as_po(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

  ENDMETHOD.
ENDCLASS.
