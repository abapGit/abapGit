CLASS zcl_abapgit_object_rvbc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_rvb TYPE c LENGTH 30.

    METHODS get_rvb_abap_git_api
      RETURNING VALUE(rv_result) TYPE REF TO object.
    METHODS get_rvb_conf_api
      RETURNING VALUE(rv_result) TYPE REF TO object.
ENDCLASS.



CLASS zcl_abapgit_object_rvbc IMPLEMENTATION.


  METHOD get_rvb_abap_git_api.
    DATA lv_factory TYPE REF TO object.


    CALL METHOD ('CL_REVIEW_BOOKLET_CONF_FACTORY')=>('GET_INSTANCE')
      RECEIVING
        result = lv_factory.

    CALL METHOD lv_factory->('IF_REVIEW_BOOKLET_CONF_FACTORY~GET_ABAP_GIT_API')
      RECEIVING
        result = rv_result.

  ENDMETHOD.


  METHOD get_rvb_conf_api.

    DATA lr_factory TYPE REF TO object.
    data lv_booklet_id TYPE ty_rvb.

    CALL METHOD ('CL_REVIEW_BOOKLET_CONF_FACTORY')=>('GET_INSTANCE')
      RECEIVING
        result = lr_factory.

    lv_booklet_id = ms_item-obj_name.

    CALL METHOD lr_factory->('IF_REVIEW_BOOKLET_CONF_FACTORY~GET_REVIEW_BOOKLET_CONF_API')
      EXPORTING
        iv_review_booklet_id = lv_booklet_id
      RECEIVING
        result               = rv_result.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA: lr_rvb_abap_git_api TYPE REF TO object.
    lr_rvb_abap_git_api = get_rvb_abap_git_api( ).

    CALL METHOD lr_rvb_abap_git_api->('IF_RVB_ABAPGIT_API~CHANGED_BY')
      EXPORTING
        review_booklet_id =  ms_item-obj_name
      RECEIVING
        rv_user           = rv_user.


  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: lr_rvb_conf_api TYPE REF TO object,
          lx_exception TYPE REF TO cx_static_check.
    TRY.
        lr_rvb_conf_api = get_rvb_conf_api( ).

        CALL METHOD lr_rvb_conf_api->('IF_REVIEW_BOOKLET_CONF_API~DELETE_BOOKLET').
      CATCH cx_static_check INTO lx_exception.
        raise exception type zcx_abapgit_exception
					exporting
						previous = lx_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA lr_header   TYPE REF TO data.
    DATA lr_messages TYPE REF TO data.
    DATA: lv_serialized TYPE string,
          lv_is_error TYPE abap_bool,
          lr_rvb_abap_git_api TYPE REF TO object,
          lr_exception TYPE REF TO cx_static_check.

    CREATE DATA lr_header TYPE ('IF_RVB_ABAPGIT_API=>HEADER').
    CREATE DATA lr_messages TYPE ('IF_RVB_ABAPGIT_API=>MESSAGES_TYPE').

    FIELD-SYMBOLS: <messages> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <message> TYPE REF TO if_abap_behv_message.

    lv_serialized = mo_files->read_string( iv_ext = 'RVBC' ).
    io_xml->read( EXPORTING
                    iv_name = 'ReviewBooklet'
                  CHANGING
                    cg_data = lr_header->* ).

    lv_is_error = abap_false.
    TRY.

        lr_rvb_abap_git_api = get_rvb_abap_git_api( ).

        CALL METHOD lr_rvb_abap_git_api->('IF_RVB_ABAPGIT_API~DESERIALIZE_AND_SAVE')
          EXPORTING
            review_booklet_id = CONV ty_rvb( ms_item-obj_name )
            data              = lv_serialized
            package           = iv_package
            header            = lr_header->*
          CHANGING
            messages          = lr_messages->*.

      CATCH cx_static_check INTO lr_exception.
        ii_log->add_exception( lr_exception ).
        lv_is_error = abap_true.
    ENDTRY.

    ASSIGN lr_messages->* TO <messages>.

    LOOP AT <messages> ASSIGNING <message>.
      IF <message>->m_severity = if_abap_behv_message=>severity-error.
        ii_log->add_error( iv_msg  = <message>->if_message~get_text( )
                           is_item = ms_item ).
        lv_is_error = abap_true.
      ELSE.

        ii_log->add_warning( iv_msg  = <message>->if_message~get_text( )
                             is_item = ms_item ).
      ENDIF.
    ENDLOOP.

    IF lv_is_error = abap_true.
      return.
    ELSE.
      corr_insert( iv_package ).
      zcl_abapgit_objects_activation=>add_item( ms_item ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: lr_rvb_abap_git_api TYPE REF TO object.

    lr_rvb_abap_git_api = get_rvb_abap_git_api( ).

    CALL METHOD lr_rvb_abap_git_api->('IF_RVB_ABAPGIT_API~EXISTS_REVIEW_BOOKLET')
      EXPORTING
        review_booklet_id = ms_item-obj_name
      RECEIVING
        result            = rv_bool.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND INITIAL LINE TO rt_steps ASSIGNING FIELD-SYMBOL(<step>).
    <step> = zif_abapgit_object=>gc_step_id-ddic.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = zif_abapgit_object~exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: lr_rvb_abap_git_api TYPE REF TO object,
          lx_exception TYPE REF TO cx_static_check,
          lv_rvb_id TYPE ty_rvb.


    lv_rvb_id = ms_item-obj_name.
    lr_rvb_abap_git_api = get_rvb_abap_git_api( ).
    DATA serialized_model TYPE REF TO data.

    TRY.
        CREATE DATA serialized_model TYPE ('if_rvb_abapgit_api=>serialized').

        CALL METHOD lr_rvb_abap_git_api->('IF_RVB_ABAPGIT_API~SERIALIZE')
          EXPORTING
            review_booklet_id = lv_rvb_id
          RECEIVING
            result            = serialized_model->*.

      CATCH cx_static_check INTO lx_exception.
        zcx_abapgit_exception=>raise( iv_text     = lx_exception->get_text( )
                                      ix_previous = lx_exception ).
    ENDTRY.

    mo_files->add_string( iv_ext    = 'RVBC'
                                                iv_string = serialized_model->('data') ).

    io_xml->add( iv_name = 'ReviewBooklet'
                 ig_data = serialized_model->('header') ).
  ENDMETHOD.
  METHOD zif_abapgit_object~get_deserialize_order.

  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.

  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.

  ENDMETHOD.

ENDCLASS.
