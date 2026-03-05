CLASS zcl_abapgit_object_http DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_uconservhttphandler,
             id             TYPE c LENGTH 30,
             version        TYPE c LENGTH 1,
             serviceorder   TYPE n LENGTH 2,
             servicehandler TYPE c LENGTH 30,
           END OF ty_uconservhttphandler.
    TYPES: BEGIN OF ty_uconhttpservtext,
             id        TYPE c LENGTH 30,
             version   TYPE c LENGTH 1,
             lang      TYPE c LENGTH 1,
             shorttext TYPE c LENGTH 255,
           END OF ty_uconhttpservtext.
    TYPES: BEGIN OF ty_handler,
             id             TYPE c LENGTH 30,
             version        TYPE c LENGTH 1,
             serviceorder   TYPE n LENGTH 2,
             servicehandler TYPE c LENGTH 30,
           END OF ty_handler.
    TYPES: BEGIN OF ty_gs_object_version,
             id           TYPE c LENGTH 1,
             object_state TYPE c LENGTH 1,
           END OF ty_gs_object_version.

    TYPES: BEGIN OF ty_icf_node,
             icfname    TYPE c LENGTH 15,
             icfparguid TYPE c LENGTH 25,
           END OF ty_icf_node.

ENDCLASS.



CLASS zcl_abapgit_object_http IMPLEMENTATION.

  METHOD constructor.

    DATA: lr_dummy TYPE REF TO data.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    TRY.
        CREATE DATA lr_dummy TYPE ('UCONHTTPSERVHEAD').
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM ('UCONHTTPSERVHEAD') INTO rv_user WHERE id = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lv_name TYPE c LENGTH 30.

    lv_name = ms_item-obj_name.

    CALL METHOD ('CL_UCON_API_FACTORY')=>('DELETE_HTTP_SERVICE')
      EXPORTING
        name     = lv_name
        devclass = iv_package.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_http_servid       TYPE c LENGTH 30,
          lt_handler           TYPE TABLE OF ty_handler,
          ls_handler           LIKE LINE OF lt_handler,
          ls_description       TYPE ty_uconhttpservtext,
          lv_check_object_name TYPE c LENGTH 40,
          lx_root              TYPE REF TO cx_root,
          lo_http              TYPE REF TO object,
          ls_abap_lang         TYPE ty_gs_object_version,
          lo_instance          TYPE REF TO object,
          lv_icfnode           TYPE ty_icf_node.

    TRY.
        io_xml->read(
          EXPORTING iv_name = 'HTTPID'
          CHANGING  cg_data = lv_http_servid ).
        io_xml->read(
          EXPORTING iv_name = 'HTTPTEXT'
          CHANGING  cg_data = ls_description ).
        io_xml->read(
          EXPORTING iv_name = 'HTTPHDL'
          CHANGING  cg_data = lt_handler ).

        TRY.
            "link to icf node (in releases older than 757, a http service requires a icf node to function)
            io_xml->read(
              EXPORTING iv_name = 'HTTPICFNODE'
              CHANGING  cg_data = lv_icfnode ).
          CATCH cx_root.
        ENDTRY.

        SELECT COUNT(*) FROM ('UCONHTTPSERVHEAD') WHERE id = lv_http_servid.
        IF sy-dbcnt > 0.
          "update
          CALL METHOD ('CL_UCON_API_FACTORY')=>('GET_HTTP_SERVICE')
            EXPORTING
              name          = lv_http_servid
              no_auth_check = abap_true
            RECEIVING
              http_service  = lo_http.
        ELSE.
          "create
          CALL METHOD ('CL_UCON_API_FACTORY')=>('NEW_HTTP_SERVICE')
            EXPORTING
              name         = lv_http_servid
            RECEIVING
              http_service = lo_http.
        ENDIF.

        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_HANDLER')
          EXPORTING
            handler = lt_handler.
        IF lt_handler IS NOT INITIAL.
          READ TABLE lt_handler INTO ls_handler INDEX 1.
          "get language version from abap class

          lv_check_object_name = ls_handler-servicehandler.
          IF lv_check_object_name IS NOT INITIAL.
            TRY.
                CALL METHOD ('CL_ABAP_LANGUAGE_VERSION')=>('GET_INSTANCE')
                  RECEIVING
                    ro_version_handler = lo_instance.
                CALL METHOD lo_instance->('IF_ABAP_LANGUAGE_VERSION~GET_VERSION_OF_OBJECT')
                  EXPORTING
                    iv_object_type    = 'CLAS'
                    iv_object_name    = lv_check_object_name
                  RECEIVING
                    rs_object_version = ls_abap_lang.

                IF ls_abap_lang-id = 'X'. "language version X not supported, use space instead
                  ls_abap_lang-id = space.
                ENDIF.

                CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_LANGUAGE_VERSION')
                  EXPORTING
                    iv_langu_version = ls_abap_lang-id.
              CATCH cx_root ##NO_HANDLER.
                " ABAP language version not supported in this system
            ENDTRY.
          ENDIF.
        ENDIF.

        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_DESCRIPTION')
          EXPORTING
            texts = ls_description.
        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_ICF_SERVICE')
          EXPORTING
            iv_icfservice = lv_icfnode.
        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~ACTIVATE').
        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SAVE')
          EXPORTING
            run_dark  = abap_true
            dev_class = iv_package
            korrnum   = iv_transport.
        CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~FREE').

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT COUNT(*) FROM ('UCONHTTPSERVHEAD') WHERE id = ms_item-obj_name.
    rv_bool = boolc( sy-dbcnt > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_http_srv_id TYPE c LENGTH 30,
          lo_serv        TYPE REF TO object, "if_ucon_api_http_service
          lt_handler     TYPE TABLE OF ty_uconservhttphandler,
          ls_description TYPE ty_uconhttpservtext,
          lx_root        TYPE REF TO cx_root,
          lv_icfnode     TYPE ty_icf_node,
          lv_name        TYPE c LENGTH 30.

    TRY.
        lv_http_srv_id = ms_item-obj_name.
        "read http service object
        CALL METHOD ('CL_UCON_API_FACTORY')=>('GET_HTTP_SERVICE')
          EXPORTING
            name          = lv_http_srv_id
            no_auth_check = abap_true
          RECEIVING
            http_service  = lo_serv.

        CALL METHOD lo_serv->('IF_UCON_API_HTTP_SERVICE~GET_HANDLER') RECEIVING handler = lt_handler.
        CALL METHOD lo_serv->('IF_UCON_API_HTTP_SERVICE~GET_DESCRIPTION')
          EXPORTING
            lang = sy-langu
          RECEIVING
            text = ls_description.

        "add data to output
        CALL METHOD lo_serv->('IF_UCON_API_HTTP_SERVICE~GET_NAME') RECEIVING name = lv_name.

        io_xml->add(
          iv_name = 'HTTPID'
          ig_data = lv_name ).

        io_xml->add(
          iv_name = 'HTTPTEXT'
          ig_data = ls_description ).

        io_xml->add(
          iv_name = 'HTTPHDL'
          ig_data = lt_handler ).

        TRY.
            "link to icf node (in releases older than 757, a http service requires a icf node to function)
            CALL METHOD lo_serv->('IF_UCON_API_HTTP_SERVICE~GET_ICF_SERVICE') IMPORTING ev_icfservice = lv_icfnode.
            io_xml->add(
              iv_name = 'HTTPICFNODE'
              ig_data = lv_icfnode ).
          CATCH cx_root.
        ENDTRY.

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
