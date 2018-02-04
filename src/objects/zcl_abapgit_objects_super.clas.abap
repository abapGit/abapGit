CLASS zcl_abapgit_objects_super DEFINITION PUBLIC ABSTRACT.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

    CLASS-METHODS:
      jump_adt
        IMPORTING i_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name
                  i_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type
        RAISING   zcx_abapgit_exception.

    CONSTANTS: c_user_unknown TYPE xubname VALUE 'UNKNOWN'.

  PROTECTED SECTION.

    DATA: ms_item     TYPE zif_abapgit_definitions=>ty_item,
          mv_language TYPE spras.

    METHODS:
      check_timestamp
        IMPORTING
          iv_timestamp      TYPE timestamp
          iv_date           TYPE datum
          iv_time           TYPE uzeit
        RETURNING
          VALUE(rv_changed) TYPE abap_bool,
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata,
      corr_insert
        IMPORTING iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      tadir_insert
        IMPORTING iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      jump_se11
        IMPORTING iv_radio TYPE string
                  iv_field TYPE string
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS:
      is_adt_jump_possible
        IMPORTING io_object                     TYPE REF TO cl_wb_object
                  io_adt                        TYPE REF TO object
        RETURNING VALUE(r_is_adt_jump_possible) TYPE abap_bool
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "zcl_abapgit_objects_super DEFINITION

CLASS zcl_abapgit_objects_super IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.                    "constructor

  METHOD jump_se11.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSD_ENTRY'.
    <ls_bdcdata>-dynpro   = '1000'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=WB_DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_radio.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_field.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE11'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD jump_adt.

    DATA: adt_link          TYPE string,
          obj_type          TYPE trobjtype,
          obj_name          TYPE trobj_name,
          li_object         TYPE REF TO cl_wb_object,
          li_adt            TYPE REF TO object,
          li_adt_uri_mapper TYPE REF TO object,
          li_adt_objref     TYPE REF TO object ##needed.

    FIELD-SYMBOLS: <uri> TYPE string.

    obj_name = i_obj_name.
    obj_type = i_obj_type.

    TRY.
        cl_wb_object=>create_from_transport_key( EXPORTING  p_object = obj_type p_obj_name = obj_name
                                                 RECEIVING  p_wb_object = li_object
                                                 EXCEPTIONS OTHERS   = 1 ).
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
          RECEIVING
            result = li_adt.

        IF is_adt_jump_possible( io_object = li_object
                                 io_adt    = li_adt ) = abap_false.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD li_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
          RECEIVING
            result = li_adt_uri_mapper.

        CALL METHOD li_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
          EXPORTING
            wb_object = li_object
          RECEIVING
            result    = li_adt_objref.

        ASSIGN ('li_adt_objref->ref_data-uri') TO <uri>.
        ASSERT sy-subrc = 0.

        CONCATENATE 'adt://' sy-sysid <uri> INTO adt_link.

        cl_gui_frontend_services=>execute( EXPORTING  document = adt_link
                                           EXCEPTIONS OTHERS   = 1 ).

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_timestamp.

    DATA: lv_ts TYPE timestamp.

    IF sy-subrc = 0 AND iv_date IS NOT INITIAL AND iv_time IS NOT INITIAL.
      cl_abap_tstmp=>systemtstmp_syst2utc(
        EXPORTING syst_date = iv_date
                  syst_time = iv_time
        IMPORTING utc_tstmp = lv_ts ).
      IF lv_ts < iv_timestamp.
        rv_changed = abap_false. " Unchanged
      ELSE.
        rv_changed = abap_true.
      ENDIF.
    ELSE. " Not found? => changed
      rv_changed = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD get_metadata.
    rs_metadata-class =
      cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
    rs_metadata-version = 'v1.0.0' ##no_text.
  ENDMETHOD.                    "get_metadata

  METHOD tadir_insert.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from TR_TADIR_INTERFACE' ).
    ENDIF.

  ENDMETHOD.

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = ms_item-obj_type.
    ls_object-objname = ms_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      zcx_abapgit_exception=>raise( 'Cancelled' ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT' ).
    ENDIF.

  ENDMETHOD.                    "corr_insert


  METHOD is_adt_jump_possible.

    DATA: li_wb_request         TYPE REF TO cl_wb_request,
          li_adt_uri_mapper_vit TYPE REF TO object,
          is_vit_wb_request     TYPE abap_bool.

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = io_object
      RECEIVING
        p_wb_request      = li_wb_request
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        CALL METHOD io_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER_VIT')
          RECEIVING
            result = li_adt_uri_mapper_vit.

        CALL METHOD li_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
          EXPORTING
            wb_request = li_wb_request
          RECEIVING
            result     = is_vit_wb_request.

        IF is_vit_wb_request = abap_true.
          r_is_adt_jump_possible = abap_false.
        ELSE.
          r_is_adt_jump_possible = abap_true.
        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_objects_super IMPLEMENTATION
