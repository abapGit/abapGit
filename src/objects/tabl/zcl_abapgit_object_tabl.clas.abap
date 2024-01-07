CLASS zcl_abapgit_object_tabl DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

    "! get additional data like table authorization group
    "! @parameter iv_tabname | name of the table
    METHODS read_extras IMPORTING iv_tabname            TYPE ddobjname
                        RETURNING VALUE(rs_tabl_extras) TYPE zif_abapgit_object_tabl=>ty_tabl_extras.

    "! Update additional data
    "! @parameter iv_tabname | name of the table
    "! @parameter is_tabl_extras | additional table data
    METHODS update_extras IMPORTING iv_tabname     TYPE ddobjname
                                    is_tabl_extras TYPE zif_abapgit_object_tabl=>ty_tabl_extras.

    "! Delete additional data
    "! @parameter iv_tabname | name of the table
    METHODS delete_extras IMPORTING iv_tabname TYPE ddobjname.

    "! Serialize IDoc Segment type/definition if exits
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS serialize_idoc_segment CHANGING cs_internal TYPE zif_abapgit_object_tabl=>ty_internal
                                   RAISING  zcx_abapgit_exception.

    "! Deserialize IDoc Segment type/definition if exits
    "! @parameter iv_package | Target package
    "! @parameter rv_deserialized | It's a segment and was desserialized
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS deserialize_idoc_segment IMPORTING is_internal            TYPE zif_abapgit_object_tabl=>ty_internal
                                               iv_transport           TYPE trkorr
                                               iv_package             TYPE devclass
                                     RETURNING VALUE(rv_deserialized) TYPE abap_bool
                                     RAISING   zcx_abapgit_exception.
    "! Delete the IDoc Segment type if exists
    "! @parameter rv_deleted | It's a segment and was deleted
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS delete_idoc_segment RETURNING VALUE(rv_deleted) TYPE abap_bool
                                RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.
    CONSTANTS c_longtext_id_tabl TYPE dokil-id VALUE 'TB' ##NO_TEXT.

    METHODS deserialize_indexes
      IMPORTING
        !is_internal TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS clear_dd03p_fields
      CHANGING
        !ct_dd03p TYPE zif_abapgit_object_tabl=>ty_dd03p_tt .
    "! Check if structure is an IDoc segment
    "! @parameter rv_is_idoc_segment | It's an IDoc segment or not
    METHODS is_idoc_segment
      RETURNING
        VALUE(rv_is_idoc_segment) TYPE abap_bool .
    METHODS clear_dd03p_fields_common
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS clear_dd03p_fields_dataelement
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS serialize_texts
      CHANGING
        !cs_internal TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      CHANGING
        !cs_internal TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS is_db_table_category
      IMPORTING
        !iv_tabclass               TYPE dd02l-tabclass
      RETURNING
        VALUE(rv_is_db_table_type) TYPE dd02l-tabclass .
ENDCLASS.



CLASS zcl_abapgit_object_tabl IMPLEMENTATION.


  METHOD clear_dd03p_fields.

    CONSTANTS lc_comptype_dataelement TYPE comptype VALUE 'E'.

    DATA: lv_masklen TYPE c LENGTH 4.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

* remove nested structures
    DELETE ct_dd03p WHERE depth <> '00'.
* remove fields from .INCLUDEs
    DELETE ct_dd03p WHERE adminfield <> '0'.

    LOOP AT ct_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.

      clear_dd03p_fields_common( CHANGING cs_dd03p = <ls_dd03p> ).

      lv_masklen = <ls_dd03p>-masklen.
      IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
* make sure the field contains valid data, or the XML will dump
        CLEAR <ls_dd03p>-masklen.
      ENDIF.

      IF <ls_dd03p>-comptype = lc_comptype_dataelement.
        clear_dd03p_fields_dataelement( CHANGING cs_dd03p = <ls_dd03p> ).
      ENDIF.

      IF <ls_dd03p>-shlporigin = 'D'.
* search help from domain
        CLEAR: <ls_dd03p>-shlpfield,
               <ls_dd03p>-shlpname.
      ENDIF.

* XML output assumes correct field content
      IF <ls_dd03p>-routputlen = '      '.
        CLEAR <ls_dd03p>-routputlen.
      ENDIF.

    ENDLOOP.

    " Clear position to avoid issues with include structures that contain different number of fields
    LOOP AT ct_dd03p ASSIGNING <ls_dd03p>.
      CLEAR: <ls_dd03p>-position, <ls_dd03p>-tabname, <ls_dd03p>-ddlanguage.
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_dd03p_fields_common.

    CLEAR: cs_dd03p-ddlanguage,
           cs_dd03p-dtelmaster,
           cs_dd03p-logflag,
           cs_dd03p-ddtext,
           cs_dd03p-reservedte,
           cs_dd03p-reptext,
           cs_dd03p-scrtext_s,
           cs_dd03p-scrtext_m,
           cs_dd03p-scrtext_l.

  ENDMETHOD.


  METHOD clear_dd03p_fields_dataelement.

* type specified via data element
    CLEAR: cs_dd03p-domname,
           cs_dd03p-inttype,
           cs_dd03p-intlen,
           cs_dd03p-mask,
           cs_dd03p-memoryid,
           cs_dd03p-headlen,
           cs_dd03p-scrlen1,
           cs_dd03p-scrlen2,
           cs_dd03p-scrlen3,
           cs_dd03p-datatype,
           cs_dd03p-leng,
           cs_dd03p-outputlen,
           cs_dd03p-deffdname,
           cs_dd03p-convexit,
           cs_dd03p-entitytab,
           cs_dd03p-dommaster,
           cs_dd03p-domname3l,
           cs_dd03p-decimals,
           cs_dd03p-lowercase,
           cs_dd03p-signflag.

  ENDMETHOD.


  METHOD delete_extras.

    DELETE FROM tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD delete_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.

    IF is_idoc_segment( ) = abap_false.
      rv_deleted = abap_false.
      RETURN. "previous XML version or no IDoc segment
    ENDIF.

    rv_deleted = abap_true.
    lv_segment_type = ms_item-obj_name.

    CALL FUNCTION 'SEGMENT_DELETE'
      EXPORTING
        segmenttyp = lv_segment_type
      IMPORTING
        result     = lv_result
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD deserialize_idoc_segment.

    DATA lv_result              LIKE sy-subrc.
    DATA lv_package             TYPE devclass.
    DATA lv_uname               TYPE sy-uname.
    DATA lv_transport           TYPE trkorr.
    DATA ls_edisdef             TYPE edisdef.
    DATA ls_segment_definition  TYPE zif_abapgit_object_tabl=>ty_segment_definition.
    FIELD-SYMBOLS <ls_segment_definition> TYPE zif_abapgit_object_tabl=>ty_segment_definition.

    rv_deserialized = abap_false.

    IF lines( is_internal-segment_definitions ) = 0.
      RETURN. "no IDoc segment
    ENDIF.

    rv_deserialized = abap_true.

    lv_package = iv_package.
    lv_transport = iv_transport.

    LOOP AT is_internal-segment_definitions ASSIGNING <ls_segment_definition>.
      ls_segment_definition = <ls_segment_definition>.
      <ls_segment_definition>-segmentheader-presp = sy-uname.
      <ls_segment_definition>-segmentheader-pwork = sy-uname.

      CALL FUNCTION 'SEGMENT_READ'
        EXPORTING
          segmenttyp = <ls_segment_definition>-segmentdefinition-segtyp
        IMPORTING
          result     = lv_result
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0 OR lv_result <> 0.
        CALL FUNCTION 'SEGMENT_CREATE'
          IMPORTING
            segmentdefinition = <ls_segment_definition>-segmentdefinition
          TABLES
            segmentstructure  = <ls_segment_definition>-segmentstructures
          CHANGING
            segmentheader     = <ls_segment_definition>-segmentheader
            devclass          = lv_package
          EXCEPTIONS
            OTHERS            = 1.
      ELSE.

        CALL FUNCTION 'SEGMENT_MODIFY'
          CHANGING
            segmentheader = <ls_segment_definition>-segmentheader
            devclass      = lv_package
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'SEGMENTDEFINITION_MODIFY'
            TABLES
              segmentstructure  = <ls_segment_definition>-segmentstructures
            CHANGING
              segmentdefinition = <ls_segment_definition>-segmentdefinition
            EXCEPTIONS
              OTHERS            = 1.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF ls_segment_definition-segmentdefinition-closed = abap_true.
        IF lv_transport IS NOT INITIAL.
          CALL FUNCTION 'SEGMENTDEFINITION_CLOSE'
            EXPORTING
              segmenttyp = ls_segment_definition-segmentdefinition-segtyp
            CHANGING
              order      = lv_transport
            EXCEPTIONS
              OTHERS     = 1.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise_t100( ).
          ENDIF.
        ENDIF.

        " SEGMENTDEFINITION_CLOSE saves current release but it should be same as in repo
        SELECT SINGLE * FROM edisdef INTO ls_edisdef
          WHERE segtyp  = ls_segment_definition-segmentdefinition-segtyp
            AND version = ls_segment_definition-segmentdefinition-version.
        ls_edisdef-released = ls_segment_definition-segmentdefinition-released.
        ls_edisdef-applrel  = ls_segment_definition-segmentdefinition-applrel.
        ls_edisdef-closed   = ls_segment_definition-segmentdefinition-closed.
        UPDATE edisdef FROM ls_edisdef.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error updating IDOC segment {
            <ls_segment_definition>-segmentdefinition-segtyp }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    lv_uname = sy-uname.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = lv_uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_set_edtflag      = abap_true
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD deserialize_indexes.

    DATA:
      lv_name      TYPE ddobjname,
      lv_subrc     TYPE sy-subrc,
      lt_dd12v_db  LIKE is_internal-dd12v,
      ls_dd12v     LIKE LINE OF is_internal-dd12v,
      ls_dd17v     LIKE LINE OF is_internal-dd17v,
      lt_secondary LIKE is_internal-dd17v.

    lv_name = ms_item-obj_name.

    " Get existing indexes and drop the ones that are not included in remote
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      TABLES
        dd12v_tab     = lt_dd12v_db
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    LOOP AT lt_dd12v_db INTO ls_dd12v.
      READ TABLE is_internal-dd12v TRANSPORTING NO FIELDS WITH KEY
        sqltab    = ls_dd12v-sqltab
        indexname = ls_dd12v-indexname.
      IF sy-subrc <> 0.
        CALL FUNCTION 'DD_INDX_DEL'
          EXPORTING
            sqltab    = ls_dd12v-sqltab
            indexname = ls_dd12v-indexname
            del_state = 'M'     "all states
          IMPORTING
            rc        = lv_subrc.
        IF lv_subrc <> 0.
          zcx_abapgit_exception=>raise( |Error deleting index { ls_dd12v-sqltab }~{ ls_dd12v-indexname }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Create new or update existing indexes
    LOOP AT is_internal-dd12v INTO ls_dd12v.

      CLEAR lt_secondary.
      LOOP AT is_internal-dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      " Secondary indexes are automatically activated as part of R3TR TABL
      " So there's no need to add them to activation queue
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA: lv_name      TYPE ddobjname,
          ls_dd02v_tmp TYPE dd02v.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF cs_internal-i18n_langs,
                   <ls_dd02_text> LIKE LINE OF cs_internal-dd02_texts.

    lv_name = ms_item-obj_name.

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = cs_internal-i18n_langs ).

    SORT cs_internal-i18n_langs.
    SORT cs_internal-dd02_texts BY ddlanguage. " Optimization

    LOOP AT cs_internal-i18n_langs ASSIGNING <lv_lang>.

      " Table description
      ls_dd02v_tmp = cs_internal-dd02v.
      READ TABLE cs_internal-dd02_texts ASSIGNING <ls_dd02_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |DD02_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd02_text> TO ls_dd02v_tmp.
      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v_tmp
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_db_table_category.

    " values from domain TABCLASS
    rv_is_db_table_type = boolc( iv_tabclass = 'TRANSP'
                              OR iv_tabclass = 'CLUSTER'
                              OR iv_tabclass = 'POOL' ).

  ENDMETHOD.


  METHOD is_idoc_segment.

    DATA lv_segment_type TYPE edilsegtyp.

    lv_segment_type = ms_item-obj_name.

    SELECT SINGLE segtyp
           FROM edisegment
           INTO lv_segment_type
           WHERE segtyp = lv_segment_type.
    rv_is_idoc_segment = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD read_extras.

    SELECT SINGLE * FROM tddat INTO rs_tabl_extras-tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD serialize_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.
    DATA lv_devclass            TYPE devclass.
    DATA lt_segmentdefinitions  TYPE STANDARD TABLE OF edisegmdef.
    DATA ls_segment_definition  TYPE zif_abapgit_object_tabl=>ty_segment_definition.

    FIELD-SYMBOLS: <ls_segemtndefinition> TYPE edisegmdef.

    IF is_idoc_segment( ) = abap_false.
      RETURN.
    ENDIF.

    lv_segment_type = ms_item-obj_name.
    CALL FUNCTION 'SEGMENT_READ'
      EXPORTING
        segmenttyp        = lv_segment_type
      IMPORTING
        result            = lv_result
      TABLES
        segmentdefinition = lt_segmentdefinitions
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_segmentdefinitions ASSIGNING <ls_segemtndefinition>.
      CLEAR ls_segment_definition.
      CALL FUNCTION 'SEGMENTDEFINITION_READ'
        EXPORTING
          segmenttyp           = <ls_segemtndefinition>-segtyp
        IMPORTING
          result               = lv_result
          devclass             = lv_devclass
          segmentheader        = ls_segment_definition-segmentheader
          segmentdefinition    = ls_segment_definition-segmentdefinition
        TABLES
          segmentstructure     = ls_segment_definition-segmentstructures
        CHANGING
          version              = <ls_segemtndefinition>-version
        EXCEPTIONS
          no_authority         = 1
          segment_not_existing = 2
          OTHERS               = 3.
      IF sy-subrc <> 0 OR lv_result <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      zcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentdefinition ).
      zcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentheader ).

      APPEND ls_segment_definition TO cs_internal-segment_definitions.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name            TYPE ddobjname,
          lv_index           TYPE i,
          ls_dd02v           TYPE dd02v,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF cs_internal-i18n_langs,
                   <ls_dd02_text> LIKE LINE OF cs_internal-dd02_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE cs_internal-i18n_langs
      FROM dd02v
      WHERE tabname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT cs_internal-i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd02v_wa      = ls_dd02v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd02v-ddlanguage IS INITIAL.
        DELETE cs_internal-i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO cs_internal-dd02_texts ASSIGNING <ls_dd02_text>.
      MOVE-CORRESPONDING ls_dd02v TO <ls_dd02_text>.
    ENDLOOP.

    SORT cs_internal-i18n_langs ASCENDING.
    SORT cs_internal-dd02_texts BY ddlanguage ASCENDING.

  ENDMETHOD.


  METHOD update_extras.

    IF is_tabl_extras-tddat IS INITIAL.
      delete_extras( iv_tabname ).
    ELSE.
      MODIFY tddat FROM is_tabl_extras-tddat.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_data,
             as4user TYPE dd02l-as4user,
             as4date TYPE dd02l-as4date,
             as4time TYPE dd02l-as4time,
           END OF ty_data.

    DATA: lt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
          ls_data LIKE LINE OF lt_data.


    SELECT as4user as4date as4time
      FROM dd02l INTO TABLE lt_data
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd09l
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd12l
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SORT lt_data BY as4date DESCENDING as4time DESCENDING.

    READ TABLE lt_data INDEX 1 INTO ls_data.
    IF sy-subrc = 0.
      rv_user = ls_data-as4user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_no_ask  TYPE abap_bool,
          lv_subrc   TYPE sy-subrc,
          BEGIN OF ls_dd02l,
            tabname  TYPE dd02l-tabname,
            tabclass TYPE dd02l-tabclass,
            sqltab   TYPE dd02l-sqltab,
          END OF ls_dd02l.

    IF zif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_objname = ms_item-obj_name.

    IF delete_idoc_segment( ) = abap_false.

      lv_no_ask = abap_true.
      SELECT SINGLE tabname tabclass sqltab FROM dd02l
        INTO CORRESPONDING FIELDS OF ls_dd02l
        WHERE tabname = ms_item-obj_name
        AND as4local = 'A'
        AND as4vers = '0000'.
      IF sy-subrc = 0 AND is_db_table_category( ls_dd02l-tabclass ) = abap_true.

        CALL FUNCTION 'DD_EXISTS_DATA'
          EXPORTING
            reftab          = ls_dd02l-sqltab
            tabclass        = ls_dd02l-tabclass
            tabname         = ls_dd02l-tabname
          IMPORTING
            subrc           = lv_subrc
          EXCEPTIONS
            missing_reftab  = 1
            sql_error       = 2
            buffer_overflow = 3
            unknown_error   = 4
            OTHERS          = 5.

        IF sy-subrc = 0 AND lv_subrc = 0.
          lv_no_ask = abap_false.
        ENDIF.

      ENDIF.

      delete_ddic( iv_objtype = 'T'
                   iv_no_ask  = lv_no_ask ).

      delete_longtexts( c_longtext_id_tabl ).

      delete_extras( lv_objname ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name     TYPE ddobjname,
          ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    FIELD-SYMBOLS: <ls_dd03p>      TYPE dd03p,
                   <ls_dd05m>      TYPE dd05m,
                   <ls_dd08v>      TYPE dd08v,
                   <ls_dd35v>      TYPE dd35v,
                   <ls_dd36m>      TYPE dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name. " type conversion

    ls_internal = lcl_tabl_xml=>read( io_xml ).

    IF deserialize_idoc_segment( is_internal  = ls_internal
                                 iv_transport = iv_transport
                                 iv_package   = iv_package ) = abap_false.

      ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_internal-dd09l TO <lg_roworcolst>.
      IF sy-subrc = 0 AND <lg_roworcolst> IS INITIAL.
        <lg_roworcolst> = 'C'. "Reverse fix from serialize
      ENDIF.

      " Number fields sequentially and fill table name
      LOOP AT ls_internal-dd03p ASSIGNING <ls_dd03p>.
        <ls_dd03p>-position   = sy-tabix.
        <ls_dd03p>-tabname    = lv_name.
        <ls_dd03p>-ddlanguage = mv_language.
      ENDLOOP.

      LOOP AT ls_internal-dd05m ASSIGNING <ls_dd05m>.
        <ls_dd05m>-tabname = lv_name.
      ENDLOOP.
      LOOP AT ls_internal-dd08v ASSIGNING <ls_dd08v>.
        <ls_dd08v>-tabname = lv_name.
        <ls_dd08v>-ddlanguage = mv_language.
      ENDLOOP.
      LOOP AT ls_internal-dd35v ASSIGNING <ls_dd35v>.
        <ls_dd35v>-tabname = lv_name.
      ENDLOOP.
      LOOP AT ls_internal-dd36m ASSIGNING <ls_dd36m>.
        <ls_dd36m>-tabname = lv_name.
      ENDLOOP.

      corr_insert( iv_package = iv_package
                   ig_object_class = 'DICT' ).

      CALL FUNCTION 'DD_TABL_EXPAND'
        EXPORTING
          dd02v_wa          = ls_internal-dd02v
        TABLES
          dd03p_tab         = ls_internal-dd03p
          dd05m_tab         = ls_internal-dd05m
          dd08v_tab         = ls_internal-dd08v
          dd35v_tab         = ls_internal-dd35v
          dd36m_tab         = ls_internal-dd36m
        EXCEPTIONS
          illegal_parameter = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_internal-dd02v
          dd09l_wa          = ls_internal-dd09l
        TABLES
          dd03p_tab         = ls_internal-dd03p
          dd05m_tab         = ls_internal-dd05m
          dd08v_tab         = ls_internal-dd08v
          dd35v_tab         = ls_internal-dd35v
          dd36m_tab         = ls_internal-dd36m
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      zcl_abapgit_objects_activation=>add_item( ms_item ).

      deserialize_indexes( ls_internal ).

      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_texts( CHANGING cs_internal = ls_internal ).
      ENDIF.

      deserialize_longtexts( ii_xml         = io_xml
                             iv_longtext_id = c_longtext_id_tabl ).

      update_extras( iv_tabname     = lv_name
                     is_tabl_extras = ls_internal-extras ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    lv_tabname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_tabname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for new, inactive, or modified versions that might not be in nametab
      SELECT SINGLE tabname FROM dd02l INTO lv_tabname
        WHERE tabname = lv_tabname.                     "#EC CI_NOORDER
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

    DATA: li_local_version_output TYPE REF TO zif_abapgit_xml_output,
          li_local_version_input  TYPE REF TO zif_abapgit_xml_input.


    CREATE OBJECT li_local_version_output TYPE zcl_abapgit_xml_output.

    zif_abapgit_object~serialize( li_local_version_output ).

    CREATE OBJECT li_local_version_input
      TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = li_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = li_local_version_input.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name     TYPE ddobjname,
          lv_state    TYPE ddgotstate,
          ls_internal TYPE zif_abapgit_object_tabl=>ty_internal,
          lv_index    LIKE sy-index.

    FIELD-SYMBOLS: <ls_dd12v>      LIKE LINE OF ls_internal-dd12v,
                   <ls_dd05m>      LIKE LINE OF ls_internal-dd05m,
                   <ls_dd08v>      LIKE LINE OF ls_internal-dd08v,
                   <ls_dd35v>      LIKE LINE OF ls_internal-dd35v,
                   <ls_dd36m>      LIKE LINE OF ls_internal-dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd02v_wa      = ls_internal-dd02v
        dd09l_wa      = ls_internal-dd09l
      TABLES
        dd03p_tab     = ls_internal-dd03p
        dd05m_tab     = ls_internal-dd05m
        dd08v_tab     = ls_internal-dd08v
        dd12v_tab     = ls_internal-dd12v
        dd17v_tab     = ls_internal-dd17v
        dd35v_tab     = ls_internal-dd35v
        dd36m_tab     = ls_internal-dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    " Check if any active version was returned
    IF lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_internal-dd02v-as4user,
           ls_internal-dd02v-as4date,
           ls_internal-dd02v-as4time.

* reset numeric field, so XML does not crash
    IF ls_internal-dd02v-prozpuff = ''.
      CLEAR ls_internal-dd02v-prozpuff.
    ENDIF.
    IF ls_internal-dd02v-datmin = ''.
      CLEAR ls_internal-dd02v-datmin.
    ENDIF.
    IF ls_internal-dd02v-datmax = ''.
      CLEAR ls_internal-dd02v-datmax.
    ENDIF.
    IF ls_internal-dd02v-datavg = ''.
      CLEAR ls_internal-dd02v-datavg.
    ENDIF.

    CLEAR: ls_internal-dd09l-as4user,
           ls_internal-dd09l-as4date,
           ls_internal-dd09l-as4time.

    ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_internal-dd09l TO <lg_roworcolst>.
    IF sy-subrc = 0 AND <lg_roworcolst> = 'C'.
      CLEAR <lg_roworcolst>. "To avoid diff errors. This field doesn't exists in all releases
    ENDIF.


    LOOP AT ls_internal-dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time,
             <ls_dd12v>-dbindex.
    ENDLOOP.

    clear_dd03p_fields( CHANGING ct_dd03p = ls_internal-dd03p ).

* remove foreign keys inherited from .INCLUDEs
    DELETE ls_internal-dd08v WHERE noinherit = 'N'.
    LOOP AT ls_internal-dd05m ASSIGNING <ls_dd05m>.
      CLEAR <ls_dd05m>-tabname.
      CLEAR <ls_dd05m>-leng.
      lv_index = sy-tabix.
      READ TABLE ls_internal-dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE ls_internal-dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    LOOP AT ls_internal-dd08v ASSIGNING <ls_dd08v>.
      CLEAR: <ls_dd08v>-tabname, <ls_dd08v>-ddlanguage.
    ENDLOOP.
    LOOP AT ls_internal-dd35v ASSIGNING <ls_dd35v>.
      CLEAR <ls_dd35v>-tabname.
    ENDLOOP.

* remove inherited search helps
    DELETE ls_internal-dd35v WHERE shlpinher = abap_true.
    LOOP AT ls_internal-dd36m ASSIGNING <ls_dd36m>.
      CLEAR <ls_dd36m>-tabname.
      lv_index = sy-tabix.
      READ TABLE ls_internal-dd35v WITH KEY fieldname = <ls_dd36m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE ls_internal-dd36m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( CHANGING cs_internal = ls_internal ).
    ENDIF.

    ls_internal-longtexts = zcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name = ms_item-obj_name
      iv_longtext_id = c_longtext_id_tabl
      io_i18n_params = mo_i18n_params
      ii_xml         = io_xml ).

    serialize_idoc_segment( CHANGING cs_internal = ls_internal ).

    ls_internal-extras = read_extras( lv_name ).

    lcl_tabl_xml=>add(
      io_xml      = io_xml
      is_internal = ls_internal ).

  ENDMETHOD.
ENDCLASS.
