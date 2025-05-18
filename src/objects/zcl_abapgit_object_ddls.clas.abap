CLASS zcl_abapgit_object_ddls DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.

  PROTECTED SECTION.
    METHODS open_adt_stob
      IMPORTING
        iv_ddls_name TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mo_ddl_handler TYPE REF TO object. " CL_DD_DDL_HANDLER
    METHODS is_baseinfo_supported
      RETURNING
        VALUE(rv_supported) TYPE abap_bool .
    METHODS read_baseinfo
      RETURNING
        VALUE(rv_baseinfo_string) TYPE string.
    METHODS format_source_before_serialize
      CHANGING
        cv_string TYPE string.
    METHODS clear_fields
      CHANGING
        !cg_data TYPE any
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS zcl_abapgit_object_ddls IMPLEMENTATION.


  METHOD clear_fields.

    DATA:
      BEGIN OF ls_fields_to_clear,
        as4user            TYPE c,
        as4date            TYPE d,
        as4time            TYPE t,
        actflag            TYPE c,
        chgflag            TYPE c,
        abap_langu_version TYPE c,
      END OF ls_fields_to_clear.

    FIELD-SYMBOLS:
      <lg_abap_language_version> TYPE any.

    MOVE-CORRESPONDING ls_fields_to_clear TO cg_data.

    ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE cg_data TO <lg_abap_language_version>.
    IF sy-subrc = 0.
      <lg_abap_language_version> = get_abap_language_version( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = mo_ddl_handler.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD format_source_before_serialize.

    DATA:
      lv_len       TYPE i,
      lv_lastchar1 TYPE c,
      lv_lastchar2 TYPE c.

    " New line included in 751+ by CL_DD_DDL_HANDLER=>ADD_BASEOBJS_INFO_TO_DDLS
    " Change for 750-

    lv_len = strlen( cv_string ) - 1.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar1 = cv_string+lv_len(1).

    lv_len = strlen( cv_string ) - 2.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar2 = cv_string+lv_len(1).

    " only add a line break, if the last character is unequal to cr_lf and newline !
    IF lv_lastchar1 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar1 <> cl_abap_char_utilities=>newline AND
        lv_lastchar1 <> space OR
        ( lv_lastchar1 = space AND
          ( lv_lastchar2 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar2 <> cl_abap_char_utilities=>newline ) ).
      cv_string = |{ cv_string }{ cl_abap_char_utilities=>cr_lf }|.
    ENDIF.

  ENDMETHOD.


  METHOD is_baseinfo_supported.

    DATA:
      lr_data_baseinfo TYPE REF TO data.

    TRY.
        CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
        rv_supported = abap_true.
      CATCH cx_root.
        rv_supported = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD open_adt_stob.

    DATA: lr_data  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_ddnames>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lt_entity_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lg_ddnames>     TYPE any.
    FIELD-SYMBOLS: <lg_entity_view> TYPE any.
    FIELD-SYMBOLS: <lg_ddname>      TYPE any.
    FIELD-SYMBOLS: <lg_ddlname>     TYPE any.


    CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_DDOBJ').
    ASSIGN lr_data->* TO <lt_ddnames>.

    CREATE DATA lr_data LIKE LINE OF <lt_ddnames>.
    ASSIGN lr_data->* TO <lg_ddnames>.

    CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_ENTITY_OF_VIEW').
    ASSIGN lr_data->* TO <lt_entity_view>.

    CREATE DATA lr_data LIKE LINE OF <lt_entity_view>.
    ASSIGN lr_data->* TO <lg_entity_view>.

    CLEAR <lt_ddnames>.
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_ddnames> TO <lg_ddname>.
    <lg_ddname> = iv_ddls_name.
    INSERT <lg_ddnames> INTO TABLE <lt_ddnames>.

    CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~GET_VIEWNAME_FROM_ENTITYNAME')
      EXPORTING
        ddnames        = <lt_ddnames>
      IMPORTING
        view_of_entity = <lt_entity_view>.

    READ TABLE <lt_entity_view> ASSIGNING <lg_entity_view> INDEX 1.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_entity_view> TO <lg_ddlname>.

      zcl_abapgit_adt_link=>jump( iv_obj_name = <lg_ddlname>
                                  iv_obj_type = 'DDLS' ).

    ENDIF.

  ENDMETHOD.


  METHOD read_baseinfo.

    TRY.
        rv_baseinfo_string = mo_files->read_string( 'baseinfo' ).

      CATCH zcx_abapgit_exception.
        " File not found. That's ok, as the object could have been created in a
        " system where baseinfo wasn't supported.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data  TYPE REF TO data,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.

    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ENDIF.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lt_deltab TYPE TABLE OF dcdeltb,
      ls_deltab TYPE dcdeltb,
      lt_gentab TYPE TABLE OF dcgentb,
      lv_rc     TYPE sy-subrc.

    " CL_DD_DDL_HANDLER->DELETE does not work for CDS views that reference other views
    " To drop any views regardless of reference, we use delnoref = false
    ls_deltab-objtyp  = 'DDLS'.
    ls_deltab-objname = ms_item-obj_name.
    APPEND ls_deltab TO lt_deltab.

    CALL FUNCTION 'DD_MASS_ACT_C3'
      EXPORTING
        ddmode         = 'O'
        inactive       = abap_true
        write_log      = abap_false
        delall         = abap_true
        delnoref       = abap_false
        prid           = -1
      IMPORTING
        act_rc         = lv_rc
      TABLES
        gentab         = lt_gentab
        deltab         = lt_deltab
      EXCEPTIONS
        access_failure = 1
        no_objects     = 2
        locked         = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

    " rebuild object list to delete remaining TADIR entry
    CALL FUNCTION 'WB_TREE_UPDATE_OBJECTLIST'
      EXPORTING
        p_object_type = 'DF'
        p_object_name = ms_item-obj_name
        p_operation   = 'DELETE'
      EXCEPTIONS
        OTHERS        = 0.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lr_data          TYPE REF TO data,
      lr_data_baseinfo TYPE REF TO data,
      lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lg_data>                  TYPE any,
      <lg_data_baseinfo>         TYPE any,
      <lg_source>                TYPE any,
      <lg_baseinfo_string>       TYPE any,
      <lg_baseinfo_ddlname>      TYPE any,
      <lg_abap_language_version> TYPE any.

    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        io_xml->read( EXPORTING iv_name = 'DDLS'
                      CHANGING cg_data  = <lg_data> ).

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.
        <lg_source> = mo_files->read_string( 'asddls' ).

        ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <lg_data> TO <lg_abap_language_version>.
        IF sy-subrc = 0.
          set_abap_language_version( CHANGING cv_abap_language_version = <lg_abap_language_version> ).
        ENDIF.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
          ASSIGN lr_data_baseinfo->* TO <lg_data_baseinfo>.

          ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_string>.
          ASSERT sy-subrc = 0.

          <lg_baseinfo_string> = read_baseinfo( ).

          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_ddlname>.
          ASSERT sy-subrc = 0.
          <lg_baseinfo_ddlname> = ms_item-obj_name.

          TRY.
              CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~SAVE')
                EXPORTING
                  name                  = ms_item-obj_name
                  put_state             = 'N'
                  ddddlsrcv_wa          = <lg_data>
                  baseinfo_string       = <lg_data_baseinfo>
                  save_language_version = abap_true.
            CATCH cx_sy_dyn_call_param_not_found.
              CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~SAVE')
                EXPORTING
                  name            = ms_item-obj_name
                  put_state       = 'N'
                  ddddlsrcv_wa    = <lg_data>
                  baseinfo_string = <lg_data_baseinfo>.
          ENDTRY.
        ELSE.
          CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~SAVE')
            EXPORTING
              name         = ms_item-obj_name
              put_state    = 'N'
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

        CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~WRITE_TADIR')
          EXPORTING
            objectname = ms_item-obj_name
            devclass   = iv_package
            prid       = 0.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        " Attempt clean-up but catch error if it doesn't work
        TRY.
            CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~DELETE')
              EXPORTING
                name = ms_item-obj_name
                prid = 0.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_state TYPE objstate.

    TRY.
        CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name      = ms_item-obj_name
          IMPORTING
            got_state = lv_state.
        rv_bool = boolc( NOT lv_state IS INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-lxe TO rt_steps.
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

    DATA: lv_typename   TYPE typename.
    DATA: lv_ddtypekind TYPE ddtypekind.

    lv_typename = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = lv_typename
      IMPORTING
        typekind = lv_ddtypekind.

    IF lv_ddtypekind = 'STOB'.
      open_adt_stob( ms_item-obj_name ).
      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lr_data          TYPE REF TO data,
          lr_data_baseinfo TYPE REF TO data,
          lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>          TYPE any,
                   <lg_field>         TYPE any,
                   <lt_data_baseinfo> TYPE ANY TABLE,
                   <lg_data_baseinfo> TYPE any,
                   <lg_ddlname>       TYPE any,
                   <lg_as4local>      TYPE any.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_T_BASEINFO_STRING').
          ASSIGN lr_data_baseinfo->* TO <lt_data_baseinfo>.

          CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name            = ms_item-obj_name
              get_state       = 'A'
            IMPORTING
              ddddlsrcv_wa    = <lg_data>
              baseinfo_string = <lt_data_baseinfo>.

          LOOP AT <lt_data_baseinfo> ASSIGNING <lg_data_baseinfo>.
            ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_ddlname>.
            ASSERT sy-subrc = 0.

            ASSIGN COMPONENT 'AS4LOCAL' OF STRUCTURE <lg_data_baseinfo> TO <lg_as4local>.
            ASSERT sy-subrc = 0.

            IF <lg_ddlname> = ms_item-obj_name AND <lg_as4local> = 'A'.
              ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_field>.
              ASSERT sy-subrc = 0.
              mo_files->add_string(
                iv_ext    = 'baseinfo'
                iv_string = <lg_field> ).
              EXIT.
            ENDIF.
          ENDLOOP.
        ELSE.
          CALL METHOD mo_ddl_handler->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name         = ms_item-obj_name
              get_state    = 'A'
            IMPORTING
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    clear_fields( CHANGING cg_data = <lg_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.

    format_source_before_serialize( CHANGING cv_string = <lg_field> ).

    mo_files->add_string(
      iv_ext    = 'asddls'
      iv_string = <lg_field> ).

    CLEAR <lg_field>.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = <lg_data> ).

  ENDMETHOD.
ENDCLASS.
