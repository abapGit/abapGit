CLASS zcl_abapgit_object_tabl DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_segment_definition,
             segmentheader     TYPE edisegmhd,
             segmentdefinition TYPE edisegmdef,
             segmentstructures TYPE STANDARD TABLE OF edisegstru WITH DEFAULT KEY,
           END OF ty_segment_definition.
    TYPES: ty_segment_definitions TYPE STANDARD TABLE OF ty_segment_definition WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tabl_extras,
             tddat TYPE tddat,
           END OF ty_tabl_extras.

    "! get additional data like table authorization group
    "! @parameter iv_tabname | name of the table
    "! @parameter is_tabl_extras | additional table data
    METHODS read_extras IMPORTING iv_tabname            TYPE ddobjname
                        RETURNING VALUE(rs_tabl_extras) TYPE ty_tabl_extras.

    "! Update additional data
    "! @parameter iv_tabname | name of the table
    "! @parameter is_tabl_extras | additional table data
    METHODS update_extras IMPORTING iv_tabname     TYPE ddobjname
                                    is_tabl_extras TYPE ty_tabl_extras.

    "! Delete additional data
    "! @parameter iv_tabname | name of the table
    METHODS delete_extras IMPORTING iv_tabname TYPE ddobjname.

    "! Serialize IDoc Segment type/definition if exits
    "! @parameter io_xml | XML writer
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS serialize_idoc_segment IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
                                   RAISING   zcx_abapgit_exception.

    "! Deserialize IDoc Segment type/definition if exits
    "! @parameter io_xml | XML writer
    "! @parameter iv_package | Target package
    "! @parameter rv_deserialized | It's a segment and was desserialized
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS deserialize_idoc_segment IMPORTING io_xml                 TYPE REF TO zcl_abapgit_xml_input
                                               iv_package             TYPE devclass
                                     RETURNING VALUE(rv_deserialized) TYPE abap_bool
                                     RAISING   zcx_abapgit_exception.
    "! Delete the IDoc Segment type if exists
    "! @parameter rv_deleted | It's a segment and was deleted
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS delete_idoc_segment RETURNING VALUE(rv_deleted) TYPE abap_bool
                                RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.

    TYPES:
      ty_dd03p_tt TYPE STANDARD TABLE OF dd03p .

    CONSTANTS c_longtext_id_tabl TYPE dokil-id VALUE 'TB' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_s_dataname,
        segment_definition TYPE string VALUE 'SEGMENT_DEFINITION',
        tabl_extras        TYPE string VALUE 'TABL_EXTRAS',
      END OF c_s_dataname.


    METHODS clear_dd03p_fields
      CHANGING
        !ct_dd03p TYPE ty_dd03p_tt .
    "! Check if structure is an IDoc segment
    "! @parameter rv_is_idoc_segment | It's an IDoc segment or not
    METHODS is_idoc_segment
      RETURNING
        VALUE(rv_is_idoc_segment) TYPE abap_bool.
    METHODS clear_dd03p_fields_common
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS clear_dd03p_fields_dataelement
      CHANGING
        !cs_dd03p TYPE dd03p .
ENDCLASS.



CLASS zcl_abapgit_object_tabl IMPLEMENTATION.


  METHOD clear_dd03p_fields.

    CONSTANTS lc_comptype_dataelement TYPE comptype VALUE 'E' ##NO_TEXT.

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
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    DATA lv_package             TYPE devclass.
    DATA lv_uname               TYPE sy-uname.
    FIELD-SYMBOLS <ls_segment_definition> TYPE ty_segment_definition.

    rv_deserialized = abap_false.

    TRY.

        io_xml->read( EXPORTING iv_name = c_s_dataname-segment_definition
                      CHANGING  cg_data = lt_segment_definitions ).

      CATCH zcx_abapgit_exception.
        RETURN. "previous XML version or no IDoc segment
    ENDTRY.

    IF lines( lt_segment_definitions ) = 0.
      RETURN. "no IDoc segment
    ENDIF.

    rv_deserialized = abap_true.

    lv_package = iv_package.

    LOOP AT lt_segment_definitions ASSIGNING <ls_segment_definition>.
      <ls_segment_definition>-segmentheader-presp =
        <ls_segment_definition>-segmentheader-pwork = cl_abap_syst=>get_user_name( ).

      CALL FUNCTION 'SEGMENT_READ'
        EXPORTING
          segmenttyp = <ls_segment_definition>-segmentheader-segtyp
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

    ENDLOOP.

    lv_uname = cl_abap_syst=>get_user_name( ).

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
      zcx_abapgit_exception=>raise( 'error from TR_TADIR_INTERFACE' ).
    ENDIF.
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


  METHOD serialize_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.
    DATA lv_devclass            TYPE devclass.
    DATA lt_segmentdefinitions  TYPE STANDARD TABLE OF edisegmdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
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

      APPEND ls_segment_definition TO lt_segment_definitions.
    ENDLOOP.

    io_xml->add( iv_name = c_s_dataname-segment_definition
                 ig_data = lt_segment_definitions ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_data,
             as4user TYPE as4user,
             as4date TYPE as4date,
             as4time TYPE as4time,
           END OF ty_data.

    DATA: lt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
          ls_data LIKE LINE OF lt_data.


    SELECT as4user as4date as4time
      FROM dd02l INTO TABLE lt_data
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd09l
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd12l
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SORT lt_data BY as4date DESCENDING as4time DESCENDING.

    READ TABLE lt_data INDEX 1 INTO ls_data.
    IF sy-subrc = 0.
      rv_user = ls_data-as4user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname  TYPE rsedd0-ddobjname,
          lv_tabclass TYPE dd02l-tabclass,
          lv_no_ask   TYPE abap_bool,
          lv_subrc    TYPE sy-subrc,
          lr_data     TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>  TYPE any.

    IF zif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_objname = ms_item-obj_name.

    IF delete_idoc_segment( ) = abap_false.

      lv_no_ask = abap_true.
      SELECT SINGLE tabclass FROM dd02l INTO lv_tabclass
        WHERE tabname = ms_item-obj_name
        AND as4local = 'A'
        AND as4vers = '0000'.
      IF sy-subrc = 0 AND lv_tabclass = 'TRANSP'.

        " Avoid dump in dynamic SELECT in case the table does not exist on database
        CALL FUNCTION 'DB_EXISTS_TABLE'
          EXPORTING
            tabname = lv_objname
          IMPORTING
            subrc   = lv_subrc.
        IF lv_subrc = 0.
          " it cannot delete table with data without asking
          CREATE DATA lr_data TYPE (lv_objname).
          ASSIGN lr_data->* TO <lg_data>.
          SELECT SINGLE * FROM (lv_objname) INTO <lg_data>.
          IF sy-subrc = 0.
            lv_no_ask = abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      delete_ddic( iv_objtype = 'T'
                   iv_no_ask  = lv_no_ask ).

      delete_longtexts( c_longtext_id_tabl ).

      delete_extras( iv_tabname = lv_objname ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name      TYPE ddobjname,
          lv_tname     TYPE trobj_name,
          ls_dd02v     TYPE dd02v,
          ls_dd09l     TYPE dd09l,
          lt_dd03p     TYPE TABLE OF dd03p,
          lt_dd05m     TYPE TABLE OF dd05m,
          lt_dd08v     TYPE TABLE OF dd08v,
          lt_dd12v     TYPE dd12vtab,
          lt_dd17v     TYPE dd17vtab,
          ls_dd17v     LIKE LINE OF lt_dd17v,
          lt_secondary LIKE lt_dd17v,
          lt_dd35v     TYPE TABLE OF dd35v,
          lt_dd36m     TYPE dd36mttyp,
          ls_dd12v     LIKE LINE OF lt_dd12v,
          lv_refs      TYPE abap_bool,
          ls_extras    TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

    IF deserialize_idoc_segment( io_xml     = io_xml
                                 iv_package = iv_package ) = abap_false.

      io_xml->read( EXPORTING iv_name = 'DD02V'
                    CHANGING cg_data = ls_dd02v ).
      io_xml->read( EXPORTING iv_name = 'DD09L'
                    CHANGING cg_data = ls_dd09l ).
      io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                    CHANGING cg_data = lt_dd03p ).

      " DDIC Step: Replace REF TO class/interface with generic reference to avoid cyclic dependency
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE datatype = 'REF'.
        IF iv_step = zif_abapgit_object=>gc_step_id-ddic.
          <ls_dd03p>-rollname = 'OBJECT'.
        ELSE.
          lv_refs = abap_true.
        ENDIF.
      ENDLOOP.

      io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                    CHANGING cg_data = lt_dd05m ).
      io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                    CHANGING cg_data = lt_dd08v ).
      io_xml->read( EXPORTING iv_name = 'DD12V'
                    CHANGING cg_data = lt_dd12v ).
      io_xml->read( EXPORTING iv_name = 'DD17V'
                    CHANGING cg_data = lt_dd17v ).
      io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                    CHANGING cg_data = lt_dd35v ).
      io_xml->read( EXPORTING iv_name = 'DD36M'
                    CHANGING cg_data = lt_dd36m ).

      " DDIC Step: Remove referenced to search helps
      IF iv_step = zif_abapgit_object=>gc_step_id-ddic.
        CLEAR: lt_dd35v, lt_dd36m.
      ENDIF.

      IF iv_step = zif_abapgit_object=>gc_step_id-late AND lv_refs = abap_false AND lines( lt_dd35v ) = 0.
        RETURN. " already active
      ENDIF.

      corr_insert( iv_package = iv_package
                   ig_object_class = 'DICT' ).

      lv_name = ms_item-obj_name. " type conversion

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v
          dd09l_wa          = ls_dd09l
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from DDIF_TABL_PUT' ).
      ENDIF.

      zcl_abapgit_objects_activation=>add_item( ms_item ).

* handle indexes
      LOOP AT lt_dd12v INTO ls_dd12v.

* todo, call corr_insert?

        CLEAR lt_secondary.
        LOOP AT lt_dd17v INTO ls_dd17v
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
          zcx_abapgit_exception=>raise( 'error from DDIF_INDX_PUT' ).
        ENDIF.

        CALL FUNCTION 'DD_DD_TO_E071'
          EXPORTING
            type     = 'INDX'
            name     = ls_dd12v-sqltab
            id       = ls_dd12v-indexname
          IMPORTING
            obj_name = lv_tname.

        zcl_abapgit_objects_activation=>add( iv_type = 'INDX'
                                             iv_name = lv_tname ).

      ENDLOOP.

      deserialize_longtexts( io_xml ).

      io_xml->read( EXPORTING iv_name = c_s_dataname-tabl_extras
                    CHANGING cg_data = ls_extras ).
      update_extras( iv_tabname     = lv_name
                     is_tabl_extras = ls_extras ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

    DATA: lo_local_version_output TYPE REF TO zcl_abapgit_xml_output,
          lo_local_version_input  TYPE REF TO zcl_abapgit_xml_input.


    CREATE OBJECT lo_local_version_output.
    me->zif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        io_local = lo_local_version_input.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name   TYPE ddobjname,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE ty_dd03p_tt,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd12v  TYPE dd12vtab,
          lt_dd17v  TYPE dd17vtab,
          lt_dd35v  TYPE TABLE OF dd35v,
          lv_index  LIKE sy-index,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd12v>      LIKE LINE OF lt_dd12v,
                   <ls_dd05m>      LIKE LINE OF lt_dd05m,
                   <ls_dd36m>      LIKE LINE OF lt_dd36m,
                   <lg_roworcolst> TYPE any.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.
    IF ls_dd02v IS INITIAL.
      RETURN. " object does not exits
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

* reset numeric field, so XML does not crash
    IF ls_dd02v-prozpuff = ''.
      CLEAR ls_dd02v-prozpuff.
    ENDIF.
    IF ls_dd02v-datmin = ''.
      CLEAR ls_dd02v-datmin.
    ENDIF.
    IF ls_dd02v-datmax = ''.
      CLEAR ls_dd02v-datmax.
    ENDIF.
    IF ls_dd02v-datavg = ''.
      CLEAR ls_dd02v-datavg.
    ENDIF.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
    IF sy-subrc = 0 AND <lg_roworcolst> = 'C'.
      CLEAR <lg_roworcolst>. "To avoid diff errors. This field doesn't exists in all releases
    ENDIF.


    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time.
    ENDLOOP.

    clear_dd03p_fields( CHANGING ct_dd03p = lt_dd03p ).

* remove foreign keys inherited from .INCLUDEs
    DELETE lt_dd08v WHERE noinherit = 'N'.
    LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
      lv_index = sy-tabix.
      READ TABLE lt_dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

* remove inherited search helps
    DELETE lt_dd35v WHERE shlpinher = abap_true.
    LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
      lv_index = sy-tabix.
      READ TABLE lt_dd35v WITH KEY fieldname = <ls_dd36m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd36m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    io_xml->add( iv_name = 'DD02V'
                 ig_data = ls_dd02v ).
    IF NOT ls_dd09l IS INITIAL.
      io_xml->add( iv_name = 'DD09L'
                   ig_data = ls_dd09l ).
    ENDIF.
    io_xml->add( ig_data = lt_dd03p
                 iv_name = 'DD03P_TABLE' ).
    io_xml->add( ig_data = lt_dd05m
                 iv_name = 'DD05M_TABLE' ).
    io_xml->add( ig_data = lt_dd08v
                 iv_name = 'DD08V_TABLE' ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = lt_dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = lt_dd17v ).
    io_xml->add( ig_data = lt_dd35v
                 iv_name = 'DD35V_TALE' ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = lt_dd36m ).

    serialize_longtexts( io_xml         = io_xml
                         iv_longtext_id = c_longtext_id_tabl ).

    serialize_idoc_segment( io_xml ).

    ls_extras = read_extras( iv_tabname = lv_name ).
    io_xml->add( iv_name = c_s_dataname-tabl_extras
                 ig_data = ls_extras ).

  ENDMETHOD.

  METHOD delete_extras.

    DELETE FROM tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD read_extras.

    SELECT SINGLE * FROM tddat INTO rs_tabl_extras-tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD update_extras.

    IF is_tabl_extras-tddat IS INITIAL.
      delete_extras( iv_tabname = iv_tabname ).
    ELSE.
      MODIFY tddat FROM is_tabl_extras-tddat.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
