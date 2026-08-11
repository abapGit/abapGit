class ZCL_ABAPGIT_OBJECT_RVBC definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .
  PROTECTED SECTION.
  PRIVATE SECTION.
    types l_rvb type c length 30.

    methods get_rvb_abap_git_api
     RETURNING VALUE(result) type REF to object.
    methods get_rvb_conf_api
     RETURNING VALUE(result) type REF to object.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_RVBC IMPLEMENTATION.


  method get_rvb_abap_git_api.
    data factory type ref to object.


    call method ('CL_REVIEW_BOOKLET_CONF_FACTORY')=>('GET_INSTANCE')
      receiving
        result = factory.

    call method factory->('IF_REVIEW_BOOKLET_CONF_FACTORY~GET_ABAP_GIT_API')
      receiving
        result = result.

  endmethod.


  METHOD GET_RVB_CONF_API.

    data factory type ref to object.

    call method ('CL_REVIEW_BOOKLET_CONF_FACTORY')=>('GET_INSTANCE')
      receiving
        result = factory.

    final(booklet_id) = conv l_rvb( ms_item-obj_name ).

    call method factory->('IF_REVIEW_BOOKLET_CONF_FACTORY~GET_REVIEW_BOOKLET_CONF_API')
      exporting iv_review_booklet_id = booklet_id
      receiving
        result = result.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    data(rvb_abap_git_api) = get_rvb_abap_git_api( ).

    CALL  METHOD rvb_abap_git_api->('IF_RVB_ABAPGIT_API~CHANGED_BY')
      EXPORTING
        review_booklet_id = conv  l_rvb( ms_item-obj_name )
      RECEIVING
        rv_user            = rv_user.


  ENDMETHOD.


  method zif_abapgit_object~delete.
    try.
        data(rvb_conf_api) = get_rvb_conf_api( ).

        call method rvb_conf_api->('IF_REVIEW_BOOKLET_CONF_API~DELETE_BOOKLET').
      catch cx_static_check into data(exception).
        raise exception new zcx_abapgit_exception( previous = exception ).
    endtry.
  endmethod.


  method zif_abapgit_object~deserialize.
    data header   type ref to data.
    data messages type ref to data.

    CREATE DATA header TYPE ('IF_RVB_ABAPGIT_API=>HEADER').
    CREATE DATA messages TYPE ('IF_RVB_ABAPGIT_API=>MESSAGES_TYPE').

    FIELD-SYMBOLS: <messages> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <message> TYPE ref to if_abap_behv_message.

    data(serialized) = me->mo_files->read_string(
*                                                                    iv_extra =
                                                                    iv_ext = 'RVBC' ).
    io_xml->read( exporting
                    iv_name = 'ReviewBooklet'
                  changing
                    cg_data = header->* ).

    data(is_error) = abap_false.
    try.

        data(rvb_abap_git_api) = get_rvb_abap_git_api( ).

        call  method rvb_abap_git_api->('IF_RVB_ABAPGIT_API~DESERIALIZE_AND_SAVE')
          exporting
            review_booklet_id = conv l_rvb( ms_item-obj_name )
            data              = serialized
            package           = iv_package
            header            = header->*
          changing
            messages          = messages->*.

      catch cx_static_check into data(exception).
        ii_log->add_exception( exception ).
        is_error = abap_true.
    endtry.

    ASSIGN messages->* TO <messages>.

    loop at <messages> ASSIGNING <message>.
      if <message>->m_severity = if_abap_behv_message=>severity-error.
        ii_log->add_error( iv_msg  = <message>->if_message~get_text( )
                           is_item = ms_item ).
        is_error = abap_true.
      else.

        ii_log->add_warning( iv_msg  = <message>->if_message~get_text( )
                             is_item = ms_item ).
      endif.
    endloop.

    if is_error = abap_true.
      rollback entities.
    else.
      corr_insert( iv_package ).
      zcl_abapgit_objects_activation=>add_item( ms_item ).
    endif.
  endmethod.


  METHOD zif_abapgit_object~exists.

     data(rvb_abap_git_api) = get_rvb_abap_git_api( ).

    CALL  METHOD rvb_abap_git_api->('IF_RVB_ABAPGIT_API~EXISTS_REVIEW_BOOKLET')
      EXPORTING
        review_booklet_id = conv  l_rvb( ms_item-obj_name )
      RECEIVING
        result            = rv_bool.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    rt_steps = value #( ( zif_abapgit_object=>gc_step_id-ddic ) ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = me->zif_abapgit_object~exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  method zif_abapgit_object~serialize.
    final(rvb_id) = conv l_rvb( ms_item-obj_name ).


    data(rvb_abap_git_api) = get_rvb_abap_git_api( ).
    data serialized_model type ref to data.

    try.
        create data serialized_model type ('if_rvb_abapgit_api=>serialized').

        call  method rvb_abap_git_api->('IF_RVB_ABAPGIT_API~SERIALIZE')
          exporting
            review_booklet_id = rvb_id
          receiving
            result            = serialized_model->*.

      catch cx_static_check into data(exception).
        zcx_abapgit_exception=>raise( iv_text     = exception->get_text( )
                                      ix_previous = exception ).
    endtry.

    me->mo_files->add_string( iv_ext    = 'RVBC'
                                                iv_string = serialized_model->('data') ).

    io_xml->add( iv_name = 'ReviewBooklet'
                 ig_data = serialized_model->('header') ).
  endmethod.
  METHOD ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_ORDER.

  ENDMETHOD.

  METHOD ZIF_ABAPGIT_OBJECT~MAP_FILENAME_TO_OBJECT.

  ENDMETHOD.

  METHOD ZIF_ABAPGIT_OBJECT~MAP_OBJECT_TO_FILENAME.

  ENDMETHOD.

ENDCLASS.
