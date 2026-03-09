CLASS zcl_abapgit_objects_generic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras DEFAULT sy-langu
        !io_field_rules TYPE REF TO zif_abapgit_field_rules OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_package   TYPE devclass
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize
      IMPORTING
        !iv_package   TYPE devclass
        !io_xml       TYPE REF TO zif_abapgit_xml_input
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS exists
      RETURNING
        VALUE(rv_bool) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS serialize
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    DATA ms_object_header TYPE objh .
    DATA:
      mt_object_table TYPE STANDARD TABLE OF objsl WITH DEFAULT KEY .
    DATA:
      mt_object_method TYPE STANDARD TABLE OF objm WITH DEFAULT KEY .
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
    DATA mv_language TYPE spras .

    METHODS after_import .
    METHODS before_export .
    METHODS corr_insert
      IMPORTING
        !iv_package   TYPE devclass
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_data
      IMPORTING
        !io_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS get_key_fields
      IMPORTING
        !iv_table      TYPE objsl-tobj_name
      RETURNING
        VALUE(rt_keys) TYPE ddfields
      RAISING
        zcx_abapgit_exception .
    METHODS get_primary_table
      RETURNING
        VALUE(rv_table) TYPE objsl-tobj_name
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_data
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS validate
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .

    TYPES: BEGIN OF ty_where,
             line TYPE sychar72,
           END OF ty_where.

    TYPES: ty_where_tab TYPE STANDARD TABLE OF ty_where WITH DEFAULT KEY,
           ty_e071_tab  TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           ty_e071k_tab TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    CONSTANTS: c_feature_object LIKE ms_item-obj_type VALUE 'PMKC'.

    DATA: mo_i18n_params    TYPE REF TO zcl_abapgit_i18n_params,
          mt_resolved_e071  TYPE ty_e071_tab,
          mt_resolved_e071k TYPE ty_e071k_tab.

    METHODS resolve_logical_object
      EXPORTING et_resolved_e071  TYPE ty_e071_tab
                et_resolved_e071k TYPE ty_e071k_tab
      RAISING   zcx_abapgit_exception.
    METHODS get_where_clause
      IMPORTING
        iv_tabname      TYPE tabname
        iv_tabkey       TYPE trobj_name
      RETURNING
        VALUE(rt_where) TYPE ty_where_tab
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.

    DATA mo_field_rules TYPE REF TO zif_abapgit_field_rules .

    METHODS apply_clear_logic
      IMPORTING
        !iv_table TYPE objsl-tobj_name
      CHANGING
        !ct_data  TYPE STANDARD TABLE .
    METHODS apply_fill_logic
      IMPORTING
        !iv_table   TYPE objsl-tobj_name
        !iv_package TYPE devclass
      CHANGING
        !ct_data    TYPE STANDARD TABLE .
ENDCLASS.



CLASS zcl_abapgit_objects_generic IMPLEMENTATION.


  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
          lv_suppress_call    TYPE abap_bool VALUE abap_false,
          li_exit             TYPE REF TO zif_abapgit_exit.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    lv_suppress_call = li_exit->suppress_gen_obj_after_import( ).

    IF lv_suppress_call <> abap_true.
      ls_cts_object_entry-pgmid    = 'R3TR'.
      ls_cts_object_entry-object   = ms_item-obj_type.
      ls_cts_object_entry-obj_name = ms_item-obj_name.
      INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

      READ TABLE mt_object_method ASSIGNING <ls_object_method>
        WITH KEY
          objectname = ms_item-obj_type
          objecttype = 'L'
          method = 'AFTER_IMP'.
      IF sy-subrc = 0.
* client is actually optional for most AIM, but let's supply it and hope
* that those client-independent-ones just ignore it
        CALL FUNCTION <ls_object_method>-methodname
          EXPORTING
            iv_tarclient  = sy-mandt
            iv_is_upgrade = abap_false
          TABLES
            tt_e071       = lt_cts_object_entry
            tt_e071k      = lt_cts_key.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD apply_clear_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_clear_logic( EXPORTING iv_table = |{ iv_table }|
                                         CHANGING  ct_data  = ct_data ).
    ENDIF.
  ENDMETHOD.


  METHOD apply_fill_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_fill_logic(
        EXPORTING
          iv_table   = |{ iv_table }|
          iv_package = iv_package
        CHANGING
          ct_data    = ct_data ).
    ENDIF.
  ENDMETHOD.


  METHOD before_export.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
          lv_client           TYPE trclient.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method     = 'BEFORE_EXP'.
    IF sy-subrc = 0.
      lv_client = sy-mandt.

      ls_cts_object_entry-pgmid    = 'R3TR'.
      ls_cts_object_entry-object   = ms_item-obj_type.
      ls_cts_object_entry-obj_name = ms_item-obj_name.
      INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_client = lv_client
        TABLES
          tt_e071   = lt_cts_object_entry
          tt_e071k  = lt_cts_key.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CONSTANTS lc_logical_transport_object TYPE c LENGTH 1 VALUE 'L'.

    SELECT SINGLE * FROM objh INTO ms_object_header
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDIF.

    " object tables
    SELECT * FROM objsl INTO CORRESPONDING FIELDS OF TABLE mt_object_table
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      AND tobject = 'TABU'
      ORDER BY PRIMARY KEY.
    IF mt_object_table IS INITIAL.
      zcx_abapgit_exception=>raise( |Obviously corrupted object-type { is_item-obj_type }: No tables defined| ).
    ENDIF.

    " remove duplicate table/table-key entries
    " same table with different keys is ok
    SORT mt_object_table BY tobj_name tobjkey.
    DELETE ADJACENT DUPLICATES FROM mt_object_table COMPARING tobj_name tobjkey.

    " object methods
    SELECT * FROM objm INTO TABLE mt_object_method
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      ORDER BY PRIMARY KEY.

    ms_item = is_item.
    mv_language = iv_language.
    mo_field_rules = io_field_rules.
    mo_i18n_params = io_i18n_params.

    resolve_logical_object( IMPORTING et_resolved_e071  = mt_resolved_e071
                                      et_resolved_e071k = mt_resolved_e071k ).

  ENDMETHOD.


  METHOD corr_insert.

* this will also insert into TADIR
    zcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object    = ms_item-obj_type
      iv_obj_name  = ms_item-obj_name
      iv_package   = iv_package
      iv_transport = iv_transport
      iv_language  = mv_language ).

  ENDMETHOD.


  METHOD delete.

    DATA: lt_where   TYPE ty_where_tab,
          lv_primary TYPE objsl-tobj_name.

    FIELD-SYMBOLS: <ls_e071>  TYPE e071,
                   <ls_e071k> TYPE e071k.

    lv_primary = get_primary_table( ).

    "Each logical object table listed once.
    LOOP AT mt_resolved_e071 ASSIGNING <ls_e071>.
      "Multiple keys can be specified per table.
      LOOP AT mt_resolved_e071k ASSIGNING <ls_e071k> WHERE mastername = <ls_e071>-obj_name.

        lt_where = get_where_clause( iv_tabname = <ls_e071k>-objname
                                     iv_tabkey  = <ls_e071k>-tabkey ).

        ASSERT lt_where IS NOT INITIAL.

        DELETE FROM (<ls_e071k>-objname) WHERE (lt_where).

        "Object PMKC has more than 1 entry in the primary table
        IF <ls_e071>-obj_name = lv_primary AND ms_item-obj_type <> c_feature_object.
          ASSERT sy-dbcnt <= 1. "Just to be on the very safe side
        ENDIF.

        CLEAR lt_where.
      ENDLOOP.
    ENDLOOP.

    corr_insert( iv_package   = iv_package
                 iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD deserialize.

    validate( io_xml ).

    delete( iv_package   = iv_package
            iv_transport = iv_transport ).

    deserialize_data(
      io_xml     = io_xml
      iv_package = iv_package ).

    after_import( ).

    corr_insert( iv_package   = iv_package
                 iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD deserialize_data.

    DATA: lr_ref   TYPE REF TO data,
          lv_table TYPE sobj_name.

    FIELD-SYMBOLS: <lt_data>          TYPE STANDARD TABLE,
                   <ls_resolved_e071> TYPE e071.

    LOOP AT mt_resolved_e071 ASSIGNING <ls_resolved_e071>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_resolved_e071>-obj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      io_xml->read( EXPORTING iv_name = <ls_resolved_e071>-obj_name
                    CHANGING  cg_data = <lt_data> ).

      lv_table = <ls_resolved_e071>-obj_name.

      apply_fill_logic( EXPORTING iv_table   = lv_table
                                  iv_package = iv_package
                        CHANGING  ct_data    = <lt_data> ).

      INSERT (<ls_resolved_e071>-obj_name) FROM TABLE <lt_data>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error inserting data, { <ls_resolved_e071>-obj_name }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD exists.

    DATA: lt_where      TYPE ty_where_tab,
          ls_e071k      TYPE e071k,
          lr_table_line TYPE REF TO data,
          lv_primary    TYPE objsl-tobj_name.

    FIELD-SYMBOLS: <lg_table_line> TYPE any.

    lv_primary = get_primary_table( ).

    READ TABLE mt_resolved_e071k INTO ls_e071k WITH KEY objname = lv_primary.
    IF sy-subrc = 0.

      lt_where = get_where_clause( iv_tabname = ls_e071k-objname
                                   iv_tabkey  = ls_e071k-tabkey ).

      CREATE DATA lr_table_line TYPE (lv_primary).
      ASSIGN lr_table_line->* TO <lg_table_line>.

      SELECT SINGLE * FROM (lv_primary) INTO <lg_table_line> WHERE (lt_where).
      rv_bool = boolc( sy-dbcnt > 0 ).

    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD get_key_fields.

    DATA: lv_table TYPE ddobjname.


    lv_table = iv_table.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_table
      TABLES
        dfies_tab = rt_keys
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE rt_keys WHERE keyflag = abap_false.

  ENDMETHOD.


  METHOD get_primary_table.

    DATA: ls_object_table LIKE LINE OF mt_object_table.
    DATA: lt_object_table LIKE mt_object_table.

    " There might be several tables marked as "primary"
    " Sort by DB key so we get first one in the list
    lt_object_table = mt_object_table.
    SORT lt_object_table.

    READ TABLE lt_object_table INTO ls_object_table WITH KEY prim_table = abap_true.
    IF sy-subrc <> 0.
      " Fallback. For some objects, no primary table is explicitly flagged
      " Then, the one with only one key field shall be chosen
      READ TABLE lt_object_table INTO ls_object_table WITH KEY tobjkey = '/&'. "#EC CI_SUBRC
    ENDIF.
    IF ls_object_table IS INITIAL.
      zcx_abapgit_exception=>raise( |Object { ms_item-obj_type } has got no defined primary table| ).
    ENDIF.

    rv_table = ls_object_table-tobj_name.

  ENDMETHOD.


  METHOD get_where_clause.

    "Based on RKE_TRANSPORT_BUILD_WHERETAB
    DATA: ls_dfies        TYPE dfies,
          lv_strlen_ch    TYPE i,
          lv_langu_key(1) TYPE c,
          lv_first(1)     TYPE c VALUE 'X',
          lt_wheretab     TYPE TABLE OF vimwheretb,
          lt_ftab         TYPE TABLE OF vimwheretb,
          ls_value        TYPE vimwheretb,
          ls_wline        TYPE vimwheretb,
          ls_fline        TYPE vimwheretb,
          lv_offset_ch    TYPE i,
          lv_wline_ch     TYPE i,
          lv_ap_count     TYPE i,
          lt_dfies        TYPE TABLE OF dfies,
          lv_charsize     TYPE i,
          lv_table        TYPE sobj_name.

    lv_charsize = cl_abap_char_utilities=>charsize.

    lv_table = iv_tabname.
    lt_dfies = get_key_fields( lv_table ).

    lv_strlen_ch = strlen( iv_tabkey ).

    CLEAR: lv_langu_key.

    LOOP AT lt_dfies INTO ls_dfies.

      lv_offset_ch = ls_dfies-offset / lv_charsize.

      " Quit if string is completely processed
      IF lv_offset_ch >= lv_strlen_ch.
        EXIT.
      ENDIF.

      ls_value-line = iv_tabkey+lv_offset_ch(ls_dfies-leng).

      IF lv_first IS INITIAL.
        ls_wline-line = 'AND'.
      ENDIF.

      CLEAR lv_first.

      CONCATENATE ls_wline-line ls_dfies-fieldname INTO ls_wline-line SEPARATED BY space.
      IF ls_dfies-datatype = 'LANG'.

        ls_fline-line = ls_dfies-fieldname.
        APPEND ls_fline TO lt_ftab.
        CONCATENATE ls_wline-line 'EQ' '''' INTO ls_wline-line SEPARATED BY space.
        CONCATENATE ls_wline-line ls_value-line '''' INTO ls_wline-line.
        lv_langu_key = 'X'.

      ELSEIF ls_value-line CS '*'. " Handle generic keys

        CONCATENATE ls_wline-line 'LIKE' '''' INTO ls_wline-line SEPARATED BY space.
        TRANSLATE ls_value-line USING '*%'.
        CONCATENATE ls_wline-line ls_value-line '''' INTO ls_wline-line.

      ELSEIF ls_value-line IS INITIAL. " Key field is empty

        CONCATENATE ls_wline-line 'EQ' '''' '''' INTO ls_wline-line SEPARATED BY space.

      ELSEIF ls_dfies-datatype = 'CHAR'.

        lv_wline_ch = strlen( ls_wline-line ).
        FIND ALL OCCURRENCES OF '''' IN ls_value-line MATCH COUNT lv_ap_count.
        lv_ap_count = lv_ap_count + strlen( ls_value-line ).

        IF sy-subrc = 0.
          REPLACE ALL OCCURRENCES OF '''' IN ls_value-line WITH ''''''.
        ENDIF.
        lv_wline_ch = lv_wline_ch + lv_ap_count.

        " Check for string longer that 72 char of wline
        IF lv_wline_ch > 66.
          IF lv_ap_count > 70.
            CLEAR lv_langu_key.
            EXIT.
          ELSE.

            CONCATENATE ls_wline-line 'EQ' INTO ls_wline-line SEPARATED BY space.
            APPEND ls_wline TO lt_wheretab.
            CONCATENATE ls_wline-line 'EQ' INTO ls_wline-line SEPARATED BY space.
            CONCATENATE '''' ls_value-line '''' INTO ls_wline-line.

          ENDIF.
        ELSE.

          CONCATENATE ls_wline-line 'EQ' '''' INTO ls_wline-line SEPARATED BY space.
          CONCATENATE ls_wline-line ls_value-line '''' INTO ls_wline-line.

        ENDIF.
      ELSE. " Other Types

        CONCATENATE ls_wline-line 'EQ' '''' INTO ls_wline-line SEPARATED BY space.
        CONCATENATE ls_wline-line ls_value-line '''' INTO ls_wline-line.

      ENDIF.

      APPEND ls_wline TO lt_wheretab.
    ENDLOOP.

    rt_where = lt_wheretab.

  ENDMETHOD.


  METHOD serialize.

    before_export( ).

    serialize_data( io_xml ).

  ENDMETHOD.


  METHOD serialize_data.

    DATA: lr_ref   TYPE REF TO data,
          lt_where TYPE ty_where_tab.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_e071>  TYPE e071,
                   <ls_e071k> TYPE e071k.

    " Each logical object table listed once.
    LOOP AT mt_resolved_e071 ASSIGNING <ls_e071>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_e071>-obj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      " Multiple keys can be specified per master table.
      LOOP AT mt_resolved_e071k ASSIGNING <ls_e071k> WHERE objname = <ls_e071>-obj_name.

        lt_where = get_where_clause( iv_tabname = <ls_e071k>-objname
                                     iv_tabkey  = <ls_e071k>-tabkey ).

        SELECT *
               FROM (<ls_e071k>-objname)
               APPENDING TABLE <lt_data>
               WHERE (lt_where)
               ORDER BY PRIMARY KEY.

      ENDLOOP.

      apply_clear_logic( EXPORTING iv_table = |{ <ls_e071k>-objname }|
                         CHANGING  ct_data  = <lt_data> ).

      io_xml->add( iv_name = |{ <ls_e071k>-objname }|
                   ig_data = <lt_data> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD validate.

    DATA: lt_where   TYPE ty_where_tab,
          lr_ref     TYPE REF TO data,
          ls_e071k   TYPE e071k,
          lv_primary TYPE objsl-tobj_name.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    lv_primary = get_primary_table( ).

    READ TABLE mt_resolved_e071k INTO ls_e071k WITH KEY objname = lv_primary.
    IF sy-subrc = 0.

      lt_where = get_where_clause( iv_tabname = ls_e071k-objname
                                   iv_tabkey  = ls_e071k-tabkey ).

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (lv_primary).
      ASSIGN lr_ref->* TO <lt_data>.

      io_xml->read(
        EXPORTING
          iv_name = lv_primary
        CHANGING
          cg_data = <lt_data> ).

      IF lines( <lt_data> ) = 0.
        zcx_abapgit_exception=>raise( |Primary table { lv_primary } not found in imported container| ).
      ELSEIF lines( <lt_data> ) <> 1.
        zcx_abapgit_exception=>raise( |Primary table { lv_primary } contains more than one instance!| ).
      ENDIF.

*  validate that max one local instance was affected by the import
      SELECT COUNT(*) FROM (lv_primary) WHERE (lt_where).
      IF sy-dbcnt > 1 AND ms_item-obj_type <> c_feature_object. " Primary table has more than 1 entry for PMKC
        zcx_abapgit_exception=>raise( |More than one instance exists locally in primary table { lv_primary }| ).
      ENDIF.
    ELSE.
      zcx_abapgit_exception=>raise( |Primary table could not be determined { lv_primary }| ).
    ENDIF.

  ENDMETHOD.


  METHOD resolve_logical_object.

    DATA: ls_e071                  TYPE e071,
          lv_lang_string           TYPE c LENGTH 50,
          lt_langu                 TYPE STANDARD TABLE OF spras,
          lt_translation_languages LIKE mo_i18n_params->ms_params-translation_languages.

    ls_e071-object = ms_item-obj_type.
    ls_e071-obj_name = ms_item-obj_name.

    IF mo_i18n_params IS BOUND.
      lt_translation_languages = mo_i18n_params->ms_params-translation_languages.

      IF mo_i18n_params->ms_params-translation_languages IS NOT INITIAL.
        SELECT spras INTO TABLE lt_langu
               FROM t002
               FOR ALL ENTRIES IN lt_translation_languages
               WHERE laiso = lt_translation_languages-table_line.
      ENDIF.

      APPEND mo_i18n_params->ms_params-main_language TO lt_langu.
      CONCATENATE LINES OF lt_langu INTO lv_lang_string.

      CONDENSE lv_lang_string.

      CALL FUNCTION 'RESOLVE_LOGICAL_OBJECT'
        EXPORTING
          e071_entry                = ls_e071
          iv_languages              = lv_lang_string
        TABLES
          e071k_tab                 = et_resolved_e071k
          e071_tab                  = et_resolved_e071
        EXCEPTIONS
          no_logical_object         = 1
          logical_object_with_tadir = 2
          OTHERS                    = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error Resolving Logical Object { ms_item-obj_name }| ).
      ENDIF.
    ELSE.
      "No i18n object passed to constructor
      CALL FUNCTION 'RESOLVE_LOGICAL_OBJECT'
        EXPORTING
          e071_entry                = ls_e071
        TABLES
          e071k_tab                 = et_resolved_e071k
          e071_tab                  = et_resolved_e071
        EXCEPTIONS
          no_logical_object         = 1
          logical_object_with_tadir = 2
          OTHERS                    = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error Resolving Logical Object { ms_item-obj_name }| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
