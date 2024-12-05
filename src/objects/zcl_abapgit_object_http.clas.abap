CLASS zcl_abapgit_object_http DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF s_uconservhttphandler,
             id             TYPE c LENGTH 30,
             version        TYPE c LENGTH 1,
             serviceorder   TYPE n LENGTH 2,
             servicehandler TYPE c LENGTH 30,
           END OF s_uconservhttphandler.
    TYPES: BEGIN OF s_uconhttpservtext,
             id        TYPE c LENGTH 30,
             version   TYPE c LENGTH 1,
             lang      TYPE lang,
             shorttext TYPE c LENGTH 255,
           END OF s_uconhttpservtext.
    TYPES: BEGIN OF s_handler,
             id             TYPE c LENGTH 30,
             version        TYPE c LENGTH 1,
             serviceorder   TYPE n LENGTH 2,
             servicehandler TYPE c LENGTH 30,
           END OF s_handler.
    TYPES: BEGIN OF s_ty_gs_object_version   ,
             id           TYPE c LENGTH 1,
             object_state TYPE c LENGTH 1,
           END OF s_ty_gs_object_version.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_HTTP IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    TRY.
        SELECT SINGLE changedby FROM ('uconhttpservhead') INTO rv_user WHERE id = ms_item-obj_name.
        IF sy-subrc <> 0.
          rv_user = c_user_unknown.
        ENDIF.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'HTTP not supported' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA lx TYPE REF TO cx_root.
    DATA lo_name TYPE c LENGTH 30.
    lo_name = ms_item-obj_name.
    TRY.
        CALL METHOD ('CL_UCON_API_FACTORY')=>('DELETE_HTTP_SERVICE')
          EXPORTING
            name     = lo_name
            devclass = iv_package.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'HTTP not supported' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    TRY.
        DATA: lv_http_servid TYPE c LENGTH 30.
        DATA: lt_handler TYPE TABLE OF s_handler.

        DATA: ls_handler LIKE LINE OF lt_handler.
        DATA: ls_description TYPE s_uconhttpservtext.
        DATA: ls_korr TYPE trkorr.
        DATA: lv_check_object_name TYPE c LENGTH 40.
        DATA: lx           TYPE REF TO cx_root,
              lv_id        TYPE c LENGTH 30,
              lo_http      TYPE REF TO object,
              lv_abap_lang TYPE s_ty_gs_object_version.
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
            DATA lo_transport TYPE REF TO zcl_abapgit_default_transport.
            CREATE OBJECT lo_transport.
            ls_korr = lo_transport->zif_abapgit_default_transport~get( )-ordernum.

          CATCH zcx_abapgit_exception INTO lx. " Exception
            zcx_abapgit_exception=>raise( iv_text     = lx->get_text( )
                                 ix_previous = lx->previous ).
        ENDTRY.

        SELECT SINGLE id FROM ('uconhttpservhead') INTO lv_id WHERE id = lv_http_servid AND version = 'A'.

        TRY.
            IF sy-subrc = 0.
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
                    DATA lv_instance TYPE REF TO object.
                    CALL METHOD ('CL_ABAP_LANGUAGE_VERSION')=>('GET_INSTANCE') RECEIVING ro_version_handler = lv_instance.
                    CALL METHOD lv_instance->('IF_ABAP_LANGUAGE_VERSION~GET_VERSION_OF_OBJECT')
                      EXPORTING
                        iv_object_type    = 'CLAS'
                        iv_object_name    = lv_check_object_name
                      RECEIVING
                        rs_object_version = lv_abap_lang.

                    IF lv_abap_lang-id = 'X'. "language version X not supported, use space instead
                      lv_abap_lang-id = space.
                    ENDIF.

                    CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_LANGUAGE_VERSION') EXPORTING iv_langu_version = lv_abap_lang-id.
                  CATCH cx_root INTO lx.
                    zcx_abapgit_exception=>raise( iv_text       = lx->get_text( )
                                                  ix_previous   = lx ).
                ENDTRY.
              ENDIF.
            ENDIF.
            CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SET_DESCRIPTION')
              EXPORTING
                texts = ls_description.
            CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~SAVE')
              EXPORTING
                run_dark  = abap_true
                dev_class = iv_package
                korrnum   = ls_korr.
            CALL METHOD lo_http->('IF_UCON_API_HTTP_SERVICE~FREE').
          CATCH cx_root INTO lx.
            zcx_abapgit_exception=>raise( iv_text     = lx->get_text( )
                                 ix_previous   = lx->previous ).
        ENDTRY.

        DATA: lv_tadir_name TYPE tadir-obj_name.
        DATA: lt_ret TYPE bapiret2_t.
        lv_tadir_name = lv_http_servid.
        CALL METHOD ('CL_AUTH_START_TOOLS')=>('SUSH_CREATE')
          EXPORTING
            iv_type   = 'HTTP'
            iv_name   = lv_tadir_name
            iv_silent = abap_true
            iv_task   = ls_korr
          IMPORTING
            et_log    = lt_ret.
      CATCH cx_root INTO lx.
        zcx_abapgit_exception=>raise( lx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    TRY.
        DATA lv_id TYPE c LENGTH 30 .
        SELECT SINGLE id FROM ('uconhttpservhead') INTO lv_id WHERE id = ms_item-obj_name AND version = 'A'.

        IF sy-subrc = 0.
          rv_bool = abap_true.
        ELSE.
          rv_bool = abap_false.
        ENDIF.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'HTTP not supported' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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


  METHOD zif_abapgit_object~jump.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_http_srv_id TYPE c LENGTH 30,
          lo_serv        TYPE REF TO object, "if_ucon_api_http_service
          lt_handler     TYPE TABLE OF s_uconservhttphandler,
          ls_description TYPE s_uconhttpservtext,
          lv_text        TYPE string,
          lx             TYPE REF TO cx_root.
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
        DATA lv_name TYPE c LENGTH 30.
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
      CATCH cx_root INTO lx.
        lv_text = lx->get_text( ).
        "ii_log->add_error( iv_msg = lv_text is_item = ms_item ). " Exception
      CATCH cx_ucon_api_http_service INTO lx.
        lv_text = lx->get_text( ).
      CATCH cx_root INTO lx.
        zcx_abapgit_exception=>raise( 'HTTP not supported' ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
ENDCLASS.
