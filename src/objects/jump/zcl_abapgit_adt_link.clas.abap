CLASS zcl_abapgit_adt_link DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS jump
      IMPORTING
        !iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_line_number  TYPE i OPTIONAL
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS link_transport
      IMPORTING
        iv_transport TYPE trkorr
      RETURNING
        VALUE(rv_link) TYPE string.

  PROTECTED SECTION.

    CLASS-METHODS generate
      IMPORTING
        !iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_line_number  TYPE i OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS get_adt_objects_and_names
      IMPORTING
        iv_obj_name       TYPE zif_abapgit_definitions=>ty_item-obj_name
        iv_obj_type       TYPE zif_abapgit_definitions=>ty_item-obj_type
      EXPORTING
        eo_adt_uri_mapper TYPE REF TO object
        eo_adt_objectref  TYPE REF TO object
        ev_program        TYPE progname
        ev_include        TYPE progname
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS is_adt_jump_possible
      IMPORTING
        io_object                      TYPE REF TO cl_wb_object
        io_adt                         TYPE REF TO object
      RETURNING
        VALUE(rv_is_adt_jump_possible) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_adt_link IMPLEMENTATION.

  METHOD link_transport.
* call to CL_CTS_ADT_TM_URI_BUILDER=>CREATE_ADT_URI replaced with logic that works on all systems,
    rv_link = |adt://{ sy-sysid }/sap/bc/adt/cts/transportrequests/{ iv_transport }|.
  ENDMETHOD.

  METHOD generate.

    DATA: lv_adt_link       TYPE string.
    DATA: lo_adt_uri_mapper TYPE REF TO object.
    DATA: lo_adt_objref     TYPE REF TO object.
    DATA: lo_adt_sub_objref TYPE REF TO object.
    DATA: lv_program        TYPE progname.
    DATA: lv_include        TYPE progname.

    FIELD-SYMBOLS: <lv_uri> TYPE string.

    get_adt_objects_and_names(
      EXPORTING
        iv_obj_name       = iv_obj_name
        iv_obj_type       = iv_obj_type
      IMPORTING
        eo_adt_uri_mapper = lo_adt_uri_mapper
        eo_adt_objectref  = lo_adt_objref
        ev_program        = lv_program
        ev_include        = lv_include ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          CALL METHOD lo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_INCLUDE_TO_OBJREF')
            EXPORTING
              program     = lv_program
              include     = lv_include
              line        = iv_line_number
              line_offset = 0
              end_line    = iv_line_number
              end_offset  = 1
            RECEIVING
              result      = lo_adt_sub_objref.
          IF lo_adt_sub_objref IS NOT INITIAL.
            lo_adt_objref = lo_adt_sub_objref.
          ENDIF.

        ENDIF.

        ASSIGN ('LO_ADT_OBJREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CONCATENATE 'adt://' sy-sysid <lv_uri> INTO lv_adt_link.

        rv_result = lv_adt_link.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_adt_objects_and_names.

    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    DATA lo_object         TYPE REF TO cl_wb_object.
    DATA lo_adt            TYPE REF TO object.

    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    TRY.
        cl_wb_object=>create_from_transport_key(
          EXPORTING
            p_object    = lv_obj_type
            p_obj_name  = lv_obj_name
          RECEIVING
            p_wb_object = lo_object
          EXCEPTIONS
            OTHERS      = 1 ).
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
          RECEIVING
            result = lo_adt.

        IF is_adt_jump_possible( io_object = lo_object
                                 io_adt    = lo_adt ) = abap_false.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD lo_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
          RECEIVING
            result = eo_adt_uri_mapper.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
          EXPORTING
            wb_object = lo_object
          RECEIVING
            result    = eo_adt_objectref.

        ASSIGN ('EO_ADT_OBJECTREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_OBJREF_TO_INCLUDE')
          EXPORTING
            uri     = <lv_uri>
          IMPORTING
            program = ev_program
            include = ev_include.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.


  METHOD is_adt_jump_possible.

    DATA: lo_wb_request         TYPE REF TO cl_wb_request,
          lo_adt_uri_mapper_vit TYPE REF TO object,
          lv_vit_wb_request     TYPE abap_bool.

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = io_object
      RECEIVING
        p_wb_request      = lo_wb_request
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
            result = lo_adt_uri_mapper_vit.

        CALL METHOD lo_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
          EXPORTING
            wb_request = lo_wb_request
          RECEIVING
            result     = lv_vit_wb_request.

        rv_is_adt_jump_possible = boolc( NOT lv_vit_wb_request = abap_true ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.


  METHOD jump.

    DATA lv_adt_link TYPE string.

    TRY.
        lv_adt_link = generate(
          iv_obj_name     = iv_obj_name
          iv_obj_type     = iv_obj_type
          iv_sub_obj_name = iv_sub_obj_name
          iv_line_number  = iv_line_number ).

        zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
