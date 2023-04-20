CLASS zcl_abapgit_object_aifc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !iv_language TYPE spras
        !is_item     TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_aif_key_s,
        ns     TYPE c LENGTH 6,
        ifname TYPE c LENGTH 10,
        ifver  TYPE c LENGTH 5,
      END OF ty_aif_key_s.
    TYPES:
      BEGIN OF ty_icd_data_key_s,
        depl_scenario TYPE c LENGTH 20,
        ns            TYPE c LENGTH 6,
        ifname        TYPE c LENGTH 10,
        ifver2        TYPE c LENGTH 4,
      END OF ty_icd_data_key_s.
    TYPES:
      BEGIN OF ty_icd_data_key,
        depl_scenario TYPE c LENGTH 20,
        ns            TYPE c LENGTH 6,
        ifname        TYPE c LENGTH 10,
        ifver2        TYPE c LENGTH 4,
        ifver         TYPE c LENGTH 5,
      END OF ty_icd_data_key.
    TYPES:
      BEGIN OF ty_table_data_s,
        tabname    TYPE tabname,
        table_data TYPE REF TO data,
      END OF ty_table_data_s.
    TYPES:
      ty_table_data_t TYPE SORTED TABLE OF
           ty_table_data_s WITH UNIQUE KEY tabname.

    DATA ms_icd_data_key TYPE ty_icd_data_key_s.

    METHODS handle_table_data
      IMPORTING
        !iv_tabname TYPE tabname
        !it_data    TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception.
    METHODS clear_client
      CHANGING
        !ct_data TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception.
    METHODS authorization_check
      IMPORTING
        !io_log           TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS get_content_compress
      IMPORTING
        !io_log     TYPE REF TO zif_abapgit_log
        !is_ifkeys  TYPE ty_aif_key_s
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.
    METHODS validate_interface
      IMPORTING
        !is_ifkeys        TYPE ty_aif_key_s
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS compress_interface
      IMPORTING
        !is_ifkeys        TYPE ty_aif_key_s
        !io_log           TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS execute_checks
      IMPORTING
        !io_xml           TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_content_s,
        tabname TYPE tabname,
      END OF ty_content_s.
    TYPES:
      ty_content_t TYPE STANDARD TABLE OF ty_content_s WITH NON-UNIQUE DEFAULT KEY.

    DATA mo_abapgit_util TYPE REF TO object.
ENDCLASS.



CLASS zcl_abapgit_object_aifc IMPLEMENTATION.


  METHOD authorization_check.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~AUTHORIZATION_CHECK')
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.


  METHOD compress_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~COMPRESS_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    DATA: lx_exc_ref TYPE REF TO cx_sy_dyn_call_error.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_icd_data_key = is_item-obj_name.

    TRY.
        CALL METHOD ('/AIF/CL_ABAPGIT_AIFC_UTIL')=>('GET_INSTANCE')
          RECEIVING
            rr_abapgit_aifc_util = mo_abapgit_util.

      CATCH cx_sy_dyn_call_error INTO lx_exc_ref.
        zcx_abapgit_exception=>raise( 'AIFC not supported' ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_content_compress.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lo_log TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_log TYPE ('/AIF/CL_ABAPGIT_BAL_LOG')
          EXPORTING ir_git_log = io_log
                    is_item = ms_item.

        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~INITIALIZE_CONTENT_COMPRESS')
          EXPORTING
            ir_bal     = lo_log
            is_ifkey   = is_ifkeys
            iv_package = iv_package
            iv_depl_id = ms_icd_data_key-depl_scenario.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_table_data.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~HANDLE_TABLE_DATA')
          EXPORTING
            iv_tabname = iv_tabname
            it_data    = it_data.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.


  METHOD validate_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~VALIDATE_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA  lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~CHANGED_BY')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_user = rv_user.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    zcx_abapgit_exception=>raise( 'Delete not supported.' ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lt_content TYPE ty_content_t.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.

    DATA ls_ifkey TYPE ty_aif_key_s.
    DATA lr_content TYPE REF TO ty_content_s.

    DATA lx_abap_not_a_table TYPE REF TO cx_abap_not_a_table.

    DATA lv_tablename TYPE string.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    IF iv_step <> zif_abapgit_object=>gc_step_id-abap.
      RETURN.
    ENDIF.

    TRY.
        IF execute_checks( io_xml ) = abap_false.
          zcx_abapgit_exception=>raise( 'AIF interface checks failed' ).
        ENDIF.

        io_xml->read( EXPORTING
                        iv_name = `Content_table`
                      CHANGING
                        cg_data = lt_content ).


        LOOP AT lt_content REFERENCE INTO lr_content.
          TRY.
              lv_tablename = cl_abap_dyn_prg=>check_table_name_str( val = lr_content->tabname
                                                                    packages = '' ).
            CATCH cx_abap_not_a_table INTO lx_abap_not_a_table.
              zcx_abapgit_exception=>raise_with_text( lx_abap_not_a_table ).
            CATCH cx_abap_not_in_package.
              "thats fine
          ENDTRY.

          CLEAR lr_tabledescr.
          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = lr_content->tabname ).
          lr_tabledescr =  cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

          CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
          ASSIGN lr_table->* TO <lt_table>.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
          ENDIF.

          io_xml->read( EXPORTING
                          iv_name = lr_content->tabname
                        CHANGING
                          cg_data = <lt_table> ).

          handle_table_data( iv_tabname = lr_content->tabname
                             it_data = <lt_table> ).

          IF lr_content->tabname = '/AIF/T_FINF'.
            READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.

            ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ns = <lv_value>.
              UNASSIGN  <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifname = <lv_value>.
              UNASSIGN  <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifver = <lv_value>.
              UNASSIGN  <lv_value>.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF ls_ifkey IS INITIAL.
          RETURN.
        ENDIF.

        get_content_compress( io_log = ii_log
                              is_ifkeys = ls_ifkey
                              iv_package = iv_package ).


        IF authorization_check( ii_log ) = abap_false.
          RETURN.
        ENDIF.

        IF validate_interface( ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

        IF compress_interface( is_ifkeys = ls_ifkey
                               io_log    = ii_log ) = abap_false.
          RETURN.
        ENDIF.

      CATCH cx_root INTO lx_root.
        ii_log->add_exception( ix_exc = lx_root
                               is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.

    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    rv_bool = abap_false.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXISTS')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_bool = rv_bool.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
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
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_false.
    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    TYPES: ty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.

    DATA lv_report TYPE progname VALUE '/AIF/CONTENT_DISPLAY'.
    DATA lt_params TYPE ty_rsparamsl_255_t.
    DATA ls_param LIKE LINE OF lt_params.

    ls_param-selname = 'P_DEPL'.
    ls_param-kind = 'P'.
    ls_param-sign = 'I'.
    ls_param-option = 'EQ'.
    ls_param-low = ms_icd_data_key-depl_scenario.
    APPEND ls_param TO lt_params.

    SUBMIT (lv_report) WITH SELECTION-TABLE lt_params AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    DATA lt_ifdata TYPE ty_table_data_t.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA lt_content TYPE ty_content_t.
    DATA ls_content TYPE ty_content_s.
    DATA lr_ifdata TYPE REF TO ty_table_data_s.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    TRY.

        ASSIGN  lr_data TO <ls_data>.
        IF NOT <ls_data> IS ASSIGNED.
          RETURN.
        ENDIF.

        ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
        ls_icd_data_key-ns = ms_icd_data_key-ns.
        ls_icd_data_key-ifname = ms_icd_data_key-ifname.
        ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

        TRY.
            CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~GET_IF_DATA')
              EXPORTING
                is_key    = ls_icd_data_key
              RECEIVING
                rt_ifdata = lt_ifdata.

          CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
            zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                          ix_previous = lx_dyn_call_error ).
        ENDTRY.

        LOOP AT lt_ifdata REFERENCE INTO lr_ifdata.

          UNASSIGN <lt_table>.
          ASSIGN lr_ifdata->table_data->* TO <lt_table>.
          IF <lt_table> IS NOT ASSIGNED.
            CONTINUE.
          ENDIF.

          clear_client( CHANGING ct_data = <lt_table> ).

          io_xml->add( iv_name = lr_ifdata->tabname
                       ig_data = <lt_table> ).

          ls_content-tabname = lr_ifdata->tabname.
          APPEND ls_content TO lt_content.

        ENDLOOP.

        io_xml->add( iv_name = `Content_table`
                     ig_data = lt_content ).

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise( iv_text = 'Serialize not possible'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD clear_client.
    DATA:
      BEGIN OF ls_data_to_clear,
        mandt  TYPE sy-mandt,
        client TYPE sy-mandt,
      END OF ls_data_to_clear.

    FIELD-SYMBOLS:
      <ls_data> TYPE any.

    LOOP AT ct_data ASSIGNING <ls_data>.
      MOVE-CORRESPONDING ls_data_to_clear TO <ls_data>.
    ENDLOOP.
  ENDMETHOD.


  METHOD execute_checks.
    DATA ls_ifkeys TYPE ty_aif_key_s.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = '/AIF/T_FINF' ).
    lr_tabledescr =  cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <lt_table>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
    ENDIF.

    TRY.
        io_xml->read( EXPORTING
                    iv_name = '/AIF/T_FINF'
                  CHANGING
                    cg_data = <lt_table> ).

        READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ns = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifname = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifver = <lv_value>.
          ENDIF.

          CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXECUTE_CHECKS')
            EXPORTING
              is_ifkeys  = ls_ifkeys
              is_finf    = <ls_table>
            RECEIVING
              rv_success = rv_success.
        ENDIF.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
