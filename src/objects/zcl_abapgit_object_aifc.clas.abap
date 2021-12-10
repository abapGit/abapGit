CLASS zcl_abapgit_object_aifc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    TYPES:
      BEGIN OF ty_content_s,
        tabname TYPE tabname,
      END OF ty_content_s.
    TYPES:
      ty_content_t TYPE STANDARD TABLE OF ty_content_s WITH NON-UNIQUE DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !iv_language TYPE spras
        !is_item     TYPE zif_abapgit_definitions=>ty_item .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_aif_key_s,
        ns     TYPE c LENGTH 6,
        ifname TYPE c LENGTH 10,
        ifver  TYPE c LENGTH 5,
      END OF ty_aif_key_s .
    TYPES:
******************************************************************
      BEGIN OF ty_icd_data_key_s,
        depl_scenario TYPE c LENGTH 20,
        ns            TYPE c LENGTH 6,
        ifname        TYPE c LENGTH 10,
        ifver2        TYPE c LENGTH 4,
      END OF ty_icd_data_key_s .
    TYPES:
****************************************************************
      BEGIN OF ty_icd_data_key,
        depl_scenario TYPE c LENGTH 20,
        ns            TYPE c LENGTH 6,
        ifname        TYPE c LENGTH 10,
        ifver2        TYPE c LENGTH 4,
        ifver         TYPE c LENGTH 5,
      END OF ty_icd_data_key .
    TYPES:
*****************************************************************
      BEGIN OF ty_table_data_s,
        tabname    TYPE tabname,
        table_data TYPE REF TO data,
      END OF ty_table_data_s .
    TYPES:
      ty_table_data_t TYPE SORTED TABLE OF
         ty_table_data_s WITH UNIQUE KEY tabname .

    DATA ms_icd_data_key TYPE ty_icd_data_key_s .

    METHODS handle_table_data
      IMPORTING
        !iv_tabname TYPE tabname
        !it_data    TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception .
    METHODS authorization_check
      IMPORTING
        !ir_log           TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS get_content_compress
      IMPORTING
        !ir_log     TYPE REF TO zif_abapgit_log
        !is_ifkeys  TYPE ty_aif_key_s
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS validate_interface
      IMPORTING
        !is_ifkeys        TYPE ty_aif_key_s
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS compress_interface
      IMPORTING
        !is_ifkeys        TYPE ty_aif_key_s
        !ir_log           TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    DATA mr_abapgit_util TYPE REF TO object.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AIFC IMPLEMENTATION.


  METHOD authorization_check.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

    rv_success = abap_false.

    TRY.
        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~AUTHORIZATION_CHECK'.

        ls_parameter-name = 'RV_SUCCESS'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF rv_success INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        ir_log->add_exception( ix_exc  = lr_root
                               is_item = ms_item ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD compress_interface.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

    TRY.
        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~COMPRESS_INTERFACE'.

        ls_parameter-name = 'IS_IFKEYS'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF is_ifkeys INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'RV_SUCCESS'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF rv_success INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).

      CATCH cx_root INTO lr_root.
        ir_log->add_exception( ix_exc  = lr_root
                               is_item = ms_item ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    DATA: lv_meth       TYPE string,
          lv_class      TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_exc_ref TYPE REF TO cx_sy_dyn_call_error.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CLEAR ms_icd_data_key.
    ms_icd_data_key = is_item-obj_name.

    TRY.

        lv_class    =  '/AIF/CL_ABAPGIT_AIFC_UTIL'.
        lv_meth     = 'GET_INSTANCE'.

        ls_parameter-name = 'RR_ABAPGIT_AIFC_UTIL'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF mr_abapgit_util INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD (lv_class)=>(lv_meth)
          PARAMETER-TABLE
          lt_parameters.
      CATCH cx_sy_dyn_call_error INTO lr_exc_ref.
    ENDTRY.

  ENDMETHOD.


  METHOD get_content_compress.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.
    DATA: lr_log TYPE REF TO object.

    TRY.

        CREATE OBJECT lr_log TYPE ('/AIF/CL_ABAPGIT_BAL_LOG')
          EXPORTING ir_git_log = ir_log
                    is_item = ms_item.

        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~INITIALIZE_CONTENT_COMPRESS'.

        ls_parameter-name = 'IR_BAL'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF lr_log INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'IS_IFKEY'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF is_ifkeys INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'IV_PACKAGE'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF iv_package INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'IV_DEPL_ID'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF ms_icd_data_key-depl_scenario INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.


        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        ir_log->add_exception( ix_exc  = lr_root
                               is_item = ms_item ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD handle_table_data.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

    TRY.
        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~HANDLE_TABLE_DATA'.

        ls_parameter-name = 'IV_TABNAME'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF iv_tabname INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'IT_DATA'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF it_data INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        RAISE EXCEPTION TYPE zcx_abapgit_exception
          EXPORTING
            previous = lr_root.
    ENDTRY.

  ENDMETHOD.


  METHOD validate_interface.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

    rv_success = abap_false.

    TRY.

        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~VALIDATE_INTERFACE'.

        ls_parameter-name = 'IS_IFKEYS'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF is_ifkeys INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'RV_SUCCESS'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF rv_success INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA  lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    CLEAR rv_user.

    TRY.
        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~CHANGED_BY'.

        ls_parameter-name = 'IS_KEY'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF ls_icd_data_key INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'RV_USER'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF rv_user INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    zcx_abapgit_exception=>raise( 'Delete not supported.'(001) ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA: lr_root TYPE REF TO cx_root.
    DATA: lt_content TYPE ty_content_t.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.
    FIELD-SYMBOLS <ls_entry> TYPE any.
    FIELD-SYMBOLS <lv_spras> TYPE any.

    DATA ls_ifkey TYPE ty_aif_key_s.
    DATA lr_content TYPE REF TO ty_content_s.

    DATA lv_tablename TYPE string.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    IF iv_step <> zif_abapgit_object=>gc_step_id-abap.
      RETURN.
    ENDIF.

    TRY.
        io_xml->read( EXPORTING
                        iv_name = `Content_table`
                      CHANGING
                        cg_data = lt_content ).


        LOOP AT lt_content REFERENCE INTO lr_content.
          TRY.
              lv_tablename = cl_abap_dyn_prg=>check_table_name_str( val = lr_content->tabname
                                                                    packages = '' ).
            CATCH cx_abap_not_a_table.
              RAISE EXCEPTION TYPE zcx_abapgit_exception.
            CATCH cx_abap_not_in_package.
              "thats fine
          ENDTRY.

          CLEAR lr_tabledescr.
          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = lr_content->tabname ).
          lr_tabledescr =  cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

          CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
          ASSIGN lr_table->* TO <lt_table>.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_abapgit_exception.
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

        get_content_compress( ir_log = ii_log
                              is_ifkeys = ls_ifkey
                              iv_package = iv_package ).


        IF authorization_check( ir_log = ii_log ) = abap_false.
          RETURN.
        ENDIF.

        IF validate_interface( is_ifkeys = ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

        IF compress_interface( is_ifkeys = ls_ifkey
                               ir_log    = ii_log ) = abap_false.
          RETURN.
        ENDIF.

      CATCH cx_root INTO lr_root.
        ii_log->add_exception( ix_exc = lr_root
                               is_item = ms_item ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.

    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    rv_bool = abap_false.

    TRY.
        lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~EXISTS'.

        ls_parameter-name = 'IS_KEY'.
        ls_parameter-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF ls_icd_data_key INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        ls_parameter-name = 'RV_BOOL'.
        ls_parameter-kind = cl_abap_objectdescr=>returning.
        GET REFERENCE OF rv_bool INTO ls_parameter-value.
        INSERT ls_parameter INTO TABLE lt_parameters.

        CALL METHOD mr_abapgit_util->(lv_meth)
          PARAMETER-TABLE
          lt_parameters.

      CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
        zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                      ix_previous = lr_dyn_call_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    CLEAR rs_metadata.

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

    DATA lv_report TYPE progname VALUE '/AIF/CONTENT_DISPLAY'.

    TYPES: ty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.
    DATA lt_params TYPE ty_rsparamsl_255_t.
    DATA ls_param LIKE LINE OF lt_params.

    ls_param-selname = 'P_DEPL'.
    ls_param-kind = 'P'.
    ls_param-sign = 'I'.
    ls_param-option = 'EQ'.
    ls_param-low = ms_icd_data_key-depl_scenario.
    APPEND ls_param TO lt_params.

    SUBMIT (lv_report) WITH SELECTION-TABLE lt_params AND RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: lr_root TYPE REF TO cx_root.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    DATA lt_ifdata TYPE ty_table_data_t.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA lt_content TYPE ty_content_t.
    DATA ls_content TYPE ty_content_s.
    DATA lr_ifdata TYPE REF TO ty_table_data_s.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

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
            lv_meth     = '/AIF/IF_ABAPGIT_AIFC_UTIL~GET_IF_DATA'.

            ls_parameter-name = 'IS_KEY'.
            ls_parameter-kind = cl_abap_objectdescr=>exporting.
            GET REFERENCE OF ls_icd_data_key INTO ls_parameter-value.
            INSERT ls_parameter INTO TABLE lt_parameters.

            ls_parameter-name = 'RT_IFDATA'.
            ls_parameter-kind = cl_abap_objectdescr=>returning.
            GET REFERENCE OF lt_ifdata INTO ls_parameter-value.
            INSERT ls_parameter INTO TABLE lt_parameters.

            CALL METHOD mr_abapgit_util->(lv_meth)
              PARAMETER-TABLE
              lt_parameters.

          CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
            zcx_abapgit_exception=>raise( iv_text = TEXT-003
                                          ix_previous = lr_dyn_call_error ).
        ENDTRY.

        LOOP AT lt_ifdata REFERENCE INTO lr_ifdata.

          UNASSIGN <lt_table>.
          ASSIGN lr_ifdata->table_data->* TO <lt_table>.
          IF <lt_table> IS NOT ASSIGNED.
            CONTINUE.
          ENDIF.

          io_xml->add( iv_name = lr_ifdata->tabname
                       ig_data = <lt_table> ).

          ls_content-tabname = lr_ifdata->tabname.
          APPEND ls_content TO lt_content.

        ENDLOOP.

        io_xml->add( iv_name = `Content_table`
                     ig_data = lt_content ).

      CATCH cx_root INTO lr_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
