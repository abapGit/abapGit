class ZCL_ABAPGIT_OBJECT_AIFC definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .

  types:
    BEGIN OF mty_content_s,
        tabname TYPE tabname,
      END OF mty_content_s .
  types:
    mty_content_t TYPE STANDARD TABLE OF mty_content_s WITH NON-UNIQUE DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IV_LANGUAGE type SPRAS
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM .
protected section.

  types:
    BEGIN OF mty_aif_key_s,
      ns     TYPE char6,
      ifname TYPE char10,
      ifver  TYPE char5,
    END OF mty_aif_key_s .
  types:
******************************************************************
    BEGIN OF mty_icd_data_key_s,
      depl_scenario TYPE char20,
      ns            TYPE char6,
      ifname        TYPE char10,
      ifver2        TYPE char4,
    END OF mty_icd_data_key_s .
  types:
****************************************************************
    BEGIN OF mty_icd_data_key,
           depl_scenario TYPE char20,
           ns            TYPE char6,
           ifname        TYPE char10,
           ifver2        TYPE char4,
           ifver         TYPE char5,
         END OF mty_icd_data_key .
  types:
*****************************************************************
    BEGIN OF mty_table_data_s,
      tabname    TYPE tabname,
      table_data TYPE REF TO data,
    END OF mty_table_data_s .
  types:
    mty_table_data_t TYPE SORTED TABLE OF
     mty_table_data_s WITH UNIQUE KEY tabname .

  data MS_ICD_DATA_KEY type MTY_ICD_DATA_KEY_S .
  constants MC_TECHNICAL_LANGUAGE_1Q type SPRAS value '늑' ##NO_TEXT.
  constants MC_TECHNICAL_LANGUAGE_2Q type SPRAS value '닱' ##NO_TEXT.

  methods HANDLE_TABLE_DATA
    importing
      !IV_TABNAME type TABNAME
      !IT_DATA type STANDARD TABLE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods RAISE_T100_ABAPGIT
    importing
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MSGV1 type SYMSGV
      !IV_MSGV2 type SYMSGV
      !IV_MSGV3 type SYMSGV
      !IV_MSGV4 type SYMSGV
      !IR_PREVIOUS type ref to CX_ROOT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods AUTHORIZATION_CHECK
    importing
      !IR_LOG type ref to ZIF_ABAPGIT_LOG
    returning
      value(RV_SUCCESS) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_CONTENT_COMPRESS
    importing
      !IR_LOG type ref to ZIF_ABAPGIT_LOG
      !IS_IFKEYS type MTY_AIF_KEY_S
      !IV_PACKAGE type DEVCLASS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods VALIDATE_INTERFACE
    importing
      !IS_IFKEYS type MTY_AIF_KEY_S
    returning
      value(RV_SUCCESS) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods COMPRESS_INTERFACE
    importing
      !IS_IFKEYS type MTY_AIF_KEY_S
      !IR_LOG type ref to ZIF_ABAPGIT_LOG
    returning
      value(RV_SUCCESS) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
private section.

  data MR_ABAPGIT_UTIL type ref to OBJECT .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AIFC IMPLEMENTATION.


  METHOD AUTHORIZATION_CHECK.

    rv_success = abap_false.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab..

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

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
        zcx_abapgit_exception=>raise( iv_text = TEXT-003 ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        ir_log->add_exception( ix_exc  = lr_root
                               is_item = ms_item ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD COMPRESS_INTERFACE.
    TRY.
        DATA: lv_meth       TYPE string,
              ls_parameter  TYPE abap_parmbind,
              lt_parameters TYPE abap_parmbind_tab.

        DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
        DATA: lr_root TYPE REF TO cx_root.

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
        zcx_abapgit_exception=>raise( iv_text = TEXT-003 ix_previous = lr_dyn_call_error ).

      CATCH cx_root INTO lr_root.
        ir_log->add_exception( ix_exc  = lr_root
                               is_item = ms_item ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( is_item     = is_item
                    iv_language = iv_language ).

    CLEAR ms_icd_data_key.
    ms_icd_data_key = is_item-obj_name.

        DATA: lv_meth TYPE string,
              lv_class TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: exc_ref TYPE REF TO cx_sy_dyn_call_error.

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
      CATCH cx_sy_dyn_call_error INTO exc_ref.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_CONTENT_COMPRESS.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.
    DATA: lr_log TYPE REF TO object.

    TRY.

        CREATE OBJECT lr_log TYPE ('/AIF/CL_ABAPGIT_BAL_LOG')
          EXPORTING ir_git_log = ir_log
                    is_item = ms_item .

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
        zcx_abapgit_exception=>raise( iv_text = TEXT-003 ix_previous = lr_dyn_call_error ).
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
        zcx_abapgit_exception=>raise( iv_text = TEXT-003 ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        RAISE EXCEPTION TYPE zcx_abapgit_exception
          EXPORTING
            previous = lr_root.
    ENDTRY.

  ENDMETHOD.


  METHOD raise_t100_abapgit.
    zcx_abapgit_exception=>raise( iv_text = TEXT-002 ix_previous = ir_previous ).
  ENDMETHOD.


  METHOD VALIDATE_INTERFACE.

    rv_success = abap_false.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lr_root TYPE REF TO cx_root.

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
        zcx_abapgit_exception=>raise( iv_text = text-003 ix_previous = lr_dyn_call_error ).
      CATCH cx_root INTO lr_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~CHANGED_BY.

    CLEAR rv_user.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA  lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE mty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-NS = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

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
        zcx_abapgit_exception=>raise( iv_text = text-003 ix_previous = lr_dyn_call_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~DELETE.

    zcx_abapgit_exception=>raise( 'Delete not supported.'(001) ).

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~DESERIALIZE.
    DATA: lr_root TYPE REF TO cx_root.

    IF iv_step <> zif_abapgit_object=>gc_step_id-abap.
      RETURN.
    ENDIF.


    TRY.

        DATA: lt_content TYPE mty_content_t.
        io_xml->read( EXPORTING
                        iv_name = `Content_table`  ##no_text
                      CHANGING
                        cg_data = lt_content ).

        DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
        DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
        DATA lr_table TYPE REF TO data.
        FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
        FIELD-SYMBOLS <ls_table> TYPE any.
        FIELD-SYMBOLS <ls_entry> TYPE any.
        FIELD-SYMBOLS <lv_spras> TYPE any .

        DATA ls_ifkey TYPE mty_aif_key_s.
        DATA lr_content TYPE REF TO mty_content_s.
        LOOP AT lt_content REFERENCE INTO lr_content.

          TRY.
              DATA lv_tablename TYPE string.
              lv_tablename = cl_abap_dyn_prg=>check_table_name_str( val = lr_content->tabname packages = '' ).
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

*   this is for wrongly delivered texts with a technical language
          IF lr_content->tabname = '/AIF/T_NST' OR
             lr_content->tabname = '/AIF/T_NS_LBLT' OR
             lr_content->tabname = '/AIF/T_FINFT'.

            LOOP AT <lt_table> ASSIGNING <ls_entry>.

              ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <ls_entry> TO <lv_spras>.
              IF sy-subrc <> 0. CONTINUE. ENDIF.

              IF <lv_spras> = mc_technical_language_1q OR <lv_spras> = mc_technical_language_2q.
                DELETE TABLE <lt_table> FROM  <ls_entry>.
              ENDIF.
            ENDLOOP.
          ENDIF.

          handle_table_data( iv_tabname = lr_content->tabname
                             it_data = <lt_table> ).

          IF lr_content->tabname = '/AIF/T_FINF'.
            FIELD-SYMBOLS: <lv_value> TYPE any.

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

*        rv_complete_status = c_complete_status-partly.  / zif_abapgit_object=>c_complete_status-partly

        IF ls_ifkey IS INITIAL.
          RETURN.
        ENDIF.


        get_content_compress( ir_log = ii_log
                              is_ifkeys = ls_ifkey
                              iv_package = iv_package ).


        IF authorization_check( ir_log = ii_log ) = abap_false.
          RETURN.
        ENDIF.

        "mr_content_compress->content_select( ).
        IF validate_interface( is_ifkeys = ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

        IF compress_interface( is_ifkeys = ls_ifkey
                               ir_log    = ii_log ) = abap_false.
          RETURN.
        ENDIF.

*        rv_complete_status = zif_abapgit_object=>c_complete_status-complete.

      CATCH cx_root INTO lr_root.
        ii_log->add_exception( ix_exc = lr_root is_item = ms_item ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~EXISTS.

    rv_bool = abap_false.

    DATA: lv_meth       TYPE string,
          ls_parameter  TYPE abap_parmbind,
          lt_parameters TYPE abap_parmbind_tab.

    DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE mty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-NS = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

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
        zcx_abapgit_exception=>raise( iv_text = text-003 ix_previous = lr_dyn_call_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_METADATA.

    CLEAR rs_metadata.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~IS_ACTIVE.

    rv_active = abap_false.
    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.
    rv_active = abap_true.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~IS_LOCKED.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~JUMP.

    DATA lv_report TYPE progname VALUE '/AIF/CONTENT_DISPLAY'.

    TYPES: lty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.
    DATA lt_params TYPE lty_rsparamsl_255_t.
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

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    TRY.

        DATA: lv_meth       TYPE string,
              ls_parameter  TYPE abap_parmbind,
              lt_parameters TYPE abap_parmbind_tab.

        DATA: lr_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

        DATA ls_icd_data_key TYPE mty_icd_data_key.
        DATA lt_ifdata TYPE mty_table_data_t.

        DATA lr_data TYPE REF TO data.
        FIELD-SYMBOLS <ls_data> TYPE any.


        ASSIGN  lr_data TO <ls_data>.
        CHECK <ls_data> IS ASSIGNED.

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
*            ls_parameter-value = lr_data.
            INSERT ls_parameter INTO TABLE lt_parameters.

            CALL METHOD mr_abapgit_util->(lv_meth)
              PARAMETER-TABLE
              lt_parameters.

          CATCH cx_sy_dyn_call_error INTO lr_dyn_call_error.
            zcx_abapgit_exception=>raise( iv_text = TEXT-003 ix_previous = lr_dyn_call_error ).
        ENDTRY.

        DATA lt_content TYPE mty_content_t.
        DATA ls_content TYPE mty_content_s.
        DATA lr_ifdata TYPE REF TO mty_table_data_s.
        FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

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

        io_xml->add( iv_name = `Content_table` ##no_text
                     ig_data = lt_content ).

      CATCH cx_root INTO lr_root.
*        ii_log->add_exception( ix_exc = lr_root
*                               is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
