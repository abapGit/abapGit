CLASS zcl_abapgit_object_tabl DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TABL IMPLEMENTATION.


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


  METHOD zif_abapgit_object~compare_to_remote_version.
    DATA: lo_table_validation     TYPE REF TO zcl_abapgit_object_tabl_valid,
          lo_local_version_output TYPE REF TO zcl_abapgit_xml_output,
          lo_local_version_input  TYPE REF TO zcl_abapgit_xml_input,
          lv_validation_text      TYPE string.

    CREATE OBJECT lo_local_version_output.
    me->zif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->render( ).

    CREATE OBJECT lo_table_validation.

    lv_validation_text = lo_table_validation->validate(
      io_remote_version = io_remote_version_xml
      io_local_version  = lo_local_version_input ).
    IF lv_validation_text IS NOT INITIAL.
      lv_validation_text = |Database Table { ms_item-obj_name }: { lv_validation_text }|.
      CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_object_tabl_dialog
        EXPORTING
          iv_message = lv_validation_text.
    ELSE.
      CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname  TYPE rsedd0-ddobjname,
          lv_tabclass TYPE dd02l-tabclass,
          lv_no_ask   TYPE abap_bool,
          lv_subrc    TYPE sy-subrc,
          lr_data     TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>  TYPE any.


    lv_objname = ms_item-obj_name.

    lv_no_ask = abap_true.
    SELECT SINGLE tabclass FROM dd02l INTO lv_tabclass
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc = 0 AND lv_tabclass = 'TRANSP'.

* Avoid dump in dynamic SELECT in case the table does not exist on database
      CALL FUNCTION 'DB_EXISTS_TABLE'
        EXPORTING
          tabname = lv_objname
        IMPORTING
          subrc   = lv_subrc.
      IF lv_subrc = 0.
* it cannot delete table with table wihtout asking
        CREATE DATA lr_data TYPE (lv_objname).
        ASSIGN lr_data->* TO <lg_data>.
        SELECT SINGLE * FROM (lv_objname) INTO <lg_data>.
        IF sy-subrc = 0.
          lv_no_ask = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = lv_no_ask
        objname              = lv_objname
        objtype              = 'T'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, TABL' ).
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
          ls_dd12v     LIKE LINE OF lt_dd12v.


    io_xml->read( EXPORTING iv_name = 'DD02V'
                  CHANGING cg_data = ls_dd02v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                  CHANGING cg_data = lt_dd03p ).
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

    corr_insert( iv_package ).

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

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.


    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    DATA: lv_date    TYPE dats,
          lv_time    TYPE tims,
          lt_indexes TYPE STANDARD TABLE OF dd09l.

    FIELD-SYMBOLS <ls_index> LIKE LINE OF lt_indexes.

    SELECT SINGLE as4date as4time FROM dd02l " Table
      INTO (lv_date, lv_time)
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    rv_changed = check_timestamp(
      iv_timestamp = iv_timestamp
      iv_date      = lv_date
      iv_time      = lv_time ).
    IF rv_changed = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE as4date as4time FROM dd09l " Table tech settings
      INTO (lv_date, lv_time)
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    rv_changed = check_timestamp(
      iv_timestamp = iv_timestamp
      iv_date      = lv_date
      iv_time      = lv_time ).
    IF rv_changed = abap_true.
      RETURN.
    ENDIF.

    SELECT as4date as4time FROM dd12l " Table tech settings
      INTO CORRESPONDING FIELDS OF TABLE lt_indexes
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_indexes ASSIGNING <ls_index>.
      rv_changed = check_timestamp(
        iv_timestamp = iv_timestamp
        iv_date      = <ls_index>-as4date
        iv_time      = <ls_index>-as4time ).
      IF rv_changed = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

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

    DATA: lv_name    TYPE ddobjname,
          ls_dd02v   TYPE dd02v,
          ls_dd09l   TYPE dd09l,
          lt_dd03p   TYPE TABLE OF dd03p,
          lt_dd05m   TYPE TABLE OF dd05m,
          lt_dd08v   TYPE TABLE OF dd08v,
          lt_dd12v   TYPE dd12vtab,
          lt_dd17v   TYPE dd17vtab,
          lt_dd35v   TYPE TABLE OF dd35v,
          lv_index   LIKE sy-index,
          lv_masklen TYPE c LENGTH 4,
          lt_dd36m   TYPE dd36mttyp.

    FIELD-SYMBOLS: <ls_dd12v> LIKE LINE OF lt_dd12v,
                   <ls_dd05m> LIKE LINE OF lt_dd05m,
                   <ls_dd36m> LIKE LINE OF lt_dd36m,
                   <ls_dd03p> LIKE LINE OF lt_dd03p.


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

    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time.
    ENDLOOP.

* remove nested structures
    DELETE lt_dd03p WHERE depth <> '00'.
* remove fields from .INCLUDEs
    DELETE lt_dd03p WHERE adminfield <> '0'.

    LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.
      CLEAR: <ls_dd03p>-ddlanguage,
        <ls_dd03p>-dtelmaster,
        <ls_dd03p>-logflag,
        <ls_dd03p>-ddtext,
        <ls_dd03p>-reservedte,
        <ls_dd03p>-reptext,
        <ls_dd03p>-scrtext_s,
        <ls_dd03p>-scrtext_m,
        <ls_dd03p>-scrtext_l.

      lv_masklen = <ls_dd03p>-masklen.
      IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
* make sure the field contains valid data, or the XML will dump
        CLEAR <ls_dd03p>-masklen.
      ENDIF.

      IF <ls_dd03p>-comptype = 'E'.
* type specified via data element
        CLEAR: <ls_dd03p>-domname,
          <ls_dd03p>-inttype,
          <ls_dd03p>-intlen,
          <ls_dd03p>-mask,
          <ls_dd03p>-memoryid,
          <ls_dd03p>-headlen,
          <ls_dd03p>-scrlen1,
          <ls_dd03p>-scrlen2,
          <ls_dd03p>-scrlen3,
          <ls_dd03p>-datatype,
          <ls_dd03p>-leng,
          <ls_dd03p>-outputlen,
          <ls_dd03p>-deffdname,
          <ls_dd03p>-convexit,
          <ls_dd03p>-entitytab,
          <ls_dd03p>-dommaster,
          <ls_dd03p>-domname3l,
          <ls_dd03p>-decimals,
          <ls_dd03p>-lowercase,
          <ls_dd03p>-signflag.
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

  ENDMETHOD.
ENDCLASS.
