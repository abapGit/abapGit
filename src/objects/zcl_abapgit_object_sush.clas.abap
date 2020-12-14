class zcl_abapgit_object_sush definition
  public
  inheriting from zcl_abapgit_objects_super
  final
  create public .

  public section.

    interfaces zif_abapgit_object .
    aliases mo_files for zif_abapgit_object~mo_files.

  protected section.
*
*  data MS_ITEM type IF_ABAPGIT_DEFINITIONS=>TY_ITEM .
*  data MV_LANGUAGE type SPRAS .
private section.

  methods CHECK_EXIST_AND_NAME_SPACE
    importing
      !IO_SU22 type ref to CL_SU22_ADT_OBJECT optional
      !IS_HEAD type IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD
      !IV_PACKAGE type DEVCLASS
    raising
      CX_SU2N_RAISE_EVENTS .
  methods ALLOWN_OBJECTS_FOR_SUSH
    importing
      !IS_HEAD type IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD
      !IT_USOBX_EXT type IF_SU22_ADT_OBJECT=>TT_SU2X_X
      !IT_USOBT_EXT type IF_SU22_ADT_OBJECT=>TT_SU2X_T
      !IV_PACKAGE type DEVCLASS
    exporting
      !ET_MESSAGES type BAPIRET2_T
      !ET_OBJECT_NOT_TO_BE_USED type IF_ARS_ABAP_OBJECT_CHECK=>TY_GTS_OBJECT_NOT_TO_BE_USED
    raising
      CX_SU2N_RAISE_EVENTS .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SUSH IMPLEMENTATION.


  method allown_objects_for_sush.
    data:
      ls_data         type cl_su22_data_model=>ty_object_data,
      lv_value_string type su22_value_string,
      lv_item_string  type su22_value_string.

    if cl_cos_utilities=>is_cloud( ) = abap_true.
      try.
          data(lv_language_version) = cl_abap_language_version=>get_instance( )->get_default_version( exporting iv_object_type = 'SUSH'
                                                                                                                iv_package     = iv_package ).
          if lv_language_version = if_abap_language_version=>gc_version-sap_cloud_platform.

            ls_data-content       = corresponding #( is_head ).
            ls_data-content-name  = is_head-name.
            move-corresponding it_usobx_ext to ls_data-content-auth_objects.

            loop at ls_data-content-auth_objects assigning field-symbol(<ls_auth_object>).
              if <ls_auth_object>-okflag ca 'YI'.
                try.
                    cl_su21_adt_object=>create_instance( )->select(
                      exporting
                        iv_objct      = <ls_auth_object>-object
                      importing
                        et_auth_field = data(lt_object_field) ).
                  catch cx_su2n_raise_events.
                    raise exception type cx_su2n_raise_events
                      exporting
                        textid = cl_su2x=>convert_to_exception( ).
                endtry.
              endif.

              loop at lt_object_field assigning field-symbol(<ls_object_field>).
                clear lv_value_string.
                append initial line to <ls_auth_object>-auth_fields assigning field-symbol(<ls_auth_field>).

                <ls_auth_field>-field_name = <ls_object_field>-name.
                loop at it_usobt_ext assigning field-symbol(<ls_auth_value>)
                    where object = <ls_auth_object>-object and field = <ls_auth_field>-field_name
                    and not ( low is initial and high is initial ).
                  append <ls_auth_value> to <ls_auth_field>-field_values.
                  clear lv_item_string.
                  if <ls_auth_value>-low  is not initial.
                    lv_item_string = lv_item_string && ` ` && <ls_auth_value>-low.
                    if <ls_auth_value>-high is not initial.
                      lv_item_string = lv_item_string && |..| && <ls_auth_value>-high.
                    endif.
                  endif.

                  if lv_item_string is not initial.
                    lv_value_string = lv_value_string && lv_item_string && |,|.
                  endif.

                endloop.

                if lv_value_string cp '*,'.
                  shift lv_value_string right deleting trailing space.
                  shift lv_value_string right by 1 places.
                  shift lv_value_string left deleting leading space.
                  "condense lv_value_string.
                endif.

                <ls_auth_field>-field_value_string = lv_value_string.
              endloop.
            endloop.

            try.
                cl_susr_wbo_util=>check_whitelisting_for_sush(
                  exporting
                    is_data                  = ls_data
                    iv_abap_language_version = lv_language_version
                importing
                  et_object_not_to_be_used = et_object_not_to_be_used
              ).

                loop at et_object_not_to_be_used assigning field-symbol(<ls_object_not_to_be_used>).
                  append value #( type = <ls_object_not_to_be_used>-message-msgty
                                  id   = <ls_object_not_to_be_used>-message-msgid
                                  number = <ls_object_not_to_be_used>-message-msgno
                                  message_v1 = <ls_object_not_to_be_used>-message-msgv1
                                  message_v2 = <ls_object_not_to_be_used>-message-msgv2
                                  message_v3 = <ls_object_not_to_be_used>-message-msgv3
                                  message_v4 = <ls_object_not_to_be_used>-message-msgv4 ) to et_messages.
                endloop.
              catch cx_swb_exception.
                raise exception type cx_su2n_raise_events
                  exporting
                    textid = cl_su2x=>convert_to_exception( ).
            endtry.
          endif.
        catch cx_abap_language_version.
      endtry.
    endif.

  endmethod.


  method check_exist_and_name_space.
    data:
      lo_appl      type ref to cl_su22_appl,
      ls_key       type usobkey,
      ld_msg       type string,
      lv_text(100) type c,
      lv_text1     type symsgv,
      lv_text2     type symsgv,
      ls_head      type cl_su2x=>ts_head,
      lo_su22      type ref to cl_su22_adt_object.

    create object lo_su22.
    data(ls_su22_head) = is_head.
    try.
        lo_su22->if_su22_adt_object~check( exporting id_mode   = '02'
                                           changing  cs_head   = ls_su22_head ).
      catch cx_su2n_raise_events.
        lv_text = 'Lead application of object &1 does not exist'(001).
        replace '&1' with is_head-name into lv_text.
        lv_text1 = lv_text(50).
        lv_text2 = lv_text+50(50).
        message s471(s#) with lv_text1 lv_text2 into ld_msg.
        raise exception type cx_su2n_raise_events exporting textid = cl_su2x=>convert_to_exception( ).
    endtry.

    move-corresponding is_head to ls_key.
    create object lo_appl.
    lo_appl->get_data( exporting is_key   = ls_key
                       importing es_head  = ls_head ).
    if ls_head-devclass <> iv_package.
      lv_text = 'Lead application of object &1 does not exist package &2'(002).
      replace '&1' with is_head-name into lv_text.
      replace '&2' with iv_package into lv_text.
      lv_text1 = lv_text(50).
      lv_text2 = lv_text+50(50).
      message s471(s#) with lv_text1 lv_text2 into ld_msg.
      raise exception type cx_su2n_raise_events exporting textid = cl_su2x=>convert_to_exception( ).
    else.
      if cl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ). "[A4C_AGIT]
        " Lock Objects
        if ls_head-obj_name(1) eq 'Y' or
           ls_head-obj_name(1) eq 'Z' or
           ls_head-obj_name(1) eq '/' or
           ls_head-obj_name(1) eq 'J'.
        else.
          lv_text = 'Namespace of object &1 (type SUSH) not allowed in SAP Cloud Platform'(003).
          replace '&1' with is_head-name into lv_text.
          lv_text1 = lv_text(50).
          lv_text2 = lv_text+50(50).
          message s471(s#) with lv_text1 lv_text2 into ld_msg.
          raise exception type cx_su2n_raise_events exporting textid = cl_su2x=>convert_to_exception( ).
        endif.
      endif.
    endif.

  endmethod.


  method zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  endmethod.


  method zif_abapgit_object~delete.
    data:
      lo_su22 type ref to cl_su22_adt_object,
      ls_key  type        usobkey,
      lx_msg  type ref to cx_su2n_raise_events.

    assert not ms_item-obj_name is initial.
    create object lo_su22.

    ls_key = ms_item-obj_name.

    try.
        lo_su22->if_su22_adt_object~delete( exporting iv_key       = ls_key
                                                      iv_cleanup   = abap_true   ).
      catch cx_su2n_raise_events into lx_msg.
    endtry.

  endmethod.


  method zif_abapgit_object~deserialize.

    data:
      ls_key            type usobkey,
      lo_su22           type ref to object,
      lr_data_head      type ref to data,
      lt_usobx          type usobx_t,
      lt_usobt          type usobt_t,
      lr_data_usobx     type ref to data,
      lr_data_usobt     type ref to data,
      lr_data_usobx_ext type ref to data,
      lr_data_usobt_ext type ref to data,
      lv_package        type devclass.

    field-symbols: <lt_data_head>      type any table,
                   <ls_data_head>      type any,
                   <lt_data_usobx_ext> type any table,
                   <ls_data_usobx_ext> type any,
                   <lt_data_usobt_ext> type any table,
                   <ls_data_usobt_ext> type any,
                   <lt_field>          type any.

    field-symbols: <ls_head_source>      type any,
                   <ls_usobx_ext_source> type any,
                   <ls_usobt_ext_source> type any.

    assert not ms_item-obj_name is initial.

    try.
        create data lr_data_head type ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        assign lr_data_head->* to <ls_data_head>.

        create data lr_data_usobx_ext type ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        assign lr_data_usobx_ext->* to <lt_data_usobx_ext>.
        assign lr_data_usobx_ext->* to <ls_data_usobx_ext>.

        create data lr_data_usobt_ext type ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        assign lr_data_usobt_ext->* to <lt_data_usobt_ext>.
        assign lr_data_usobt_ext->* to <ls_data_usobt_ext>.

        "HEAD
        io_xml->read( exporting iv_name = 'HEAD'
                      changing  cg_data = <ls_data_head> ).

        "USOBX
        io_xml->read( exporting iv_name = 'USOBX'
                      changing  cg_data = lt_usobx ).

        "USOBT
        io_xml->read( exporting iv_name = 'USOBT'
                      changing  cg_data = lt_usobt ).

        "USOBX_EXT
        io_xml->read( exporting iv_name = 'USOBX_EXT'
                      changing  cg_data = <lt_data_usobx_ext> ).

        "USOBT_EXT
        io_xml->read( exporting iv_name = 'USOBT_EXT'
                      changing  cg_data = <lt_data_usobt_ext> ).

        create object lo_su22
          type ('CL_SU22_ADT_OBJECT').

        if iv_package is initial.
          assign component 'DEVCLASS' of structure <ls_data_head> to field-symbol(<ls_devclass>).
          assert sy-subrc = 0.
          lv_package = <ls_devclass>.
        else.
          lv_package = iv_package.
        endif.

        " check the existence and name space for Cloud Platform ABAP Environment in customer system
        try.
            check_exist_and_name_space( exporting is_head    = <ls_data_head>
                                                  iv_package = lv_package ).
          catch cx_su2n_raise_events into data(lr_err).
            data(ltext) = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
            return.
        endtry.

        try.
            allown_objects_for_sush( exporting is_head                  = <ls_data_head>
                                               it_usobx_ext             = <lt_data_usobx_ext>
                                               it_usobt_ext             = <lt_data_usobt_ext>
                                               iv_package               = lv_package
                                     importing et_object_not_to_be_used = data(lt_object_not_to_be_used) ).

            loop at lt_object_not_to_be_used reference into data(lr_object_not_to_be_used).
              delete lt_usobx  where object = lr_object_not_to_be_used->object-name.
              delete lt_usobt  where object = lr_object_not_to_be_used->object-name.
              message id lr_object_not_to_be_used->message-msgid
               type lr_object_not_to_be_used->message-msgty
             number lr_object_not_to_be_used->message-msgno
               with lr_object_not_to_be_used->message-msgv1 lr_object_not_to_be_used->message-msgv2 lr_object_not_to_be_used->message-msgv3 lr_object_not_to_be_used->message-msgv4
               into data(lv_message).
              ii_log->add_warning( iv_msg = lv_message is_item = ms_item ).
              rv_complete_status = if_abapgit_object=>c_complete_status-partly.
            endloop.

          catch cx_su2n_raise_events into lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).

        endtry.

        try.
            call method lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              exporting
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          catch cx_su2n_raise_events into lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
        endtry.

        if iv_unittest is initial.
          corr_insert( iv_package ).
        endif.

      catch cx_root into data(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    endtry.

  endmethod.


  method zif_abapgit_object~exists.
    data: ls_usobhash type usobhash.

    select single * from usobhash into ls_usobhash "#EC CI_ALL_FIELDS_NEEDED
        where name = ms_item-obj_name.                "#EC CI_SGLSELECT

    rv_bool = boolc( sy-subrc = 0 ).

  endmethod.


  method zif_abapgit_object~get_comparator.
  endmethod.


  method zif_abapgit_object~get_deserialize_steps.
    append zif_abapgit_object=>gc_step_id-abap to rt_steps.
  endmethod.


  method zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  endmethod.


  method zif_abapgit_object~is_active.
    rv_active = is_active( ).
  endmethod.


  method zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = conv #( ms_item-obj_name )
                                          iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  endmethod.


  method zif_abapgit_object~jump.
    data: ls_key          type usobkey.

    ls_key-type = ms_item-obj_type.
    ls_key-name = ms_item-obj_name.

    call function 'SU2X_DIALOG_SNGL'
      exporting
        is_key       = ls_key
        id_area      = 'SU22'
        id_actvt     = cl_suso=>gc_show
        id_disp_only = abap_false.

  endmethod.


  method zif_abapgit_object~serialize.

    data:
      ls_key            type usobkey,
      lo_su22           type ref to object,
      lt_clr_comps      type standard table of fieldname with default key,
      lt_usobx          type usobx_t,
      lt_usobt          type usobt_t,
      lr_data_head      type ref to data,
      lr_data_usobx     type ref to data,
      lr_data_usobt     type ref to data,
      lr_data_usobx_ext type ref to data,
      lr_data_usobt_ext type ref to data.

    field-symbols: <ls_data_head>      type any,
                   <lt_data_usobx_ext> type any table,
                   <ls_data_usobx_ext> type any,
                   <lt_data_usobt_ext> type any table,
                   <ls_data_usobt_ext> type any,
                   <lt_field>          type any,
                   <lv_comp>           like line of lt_clr_comps.

    ls_key = ms_item-obj_name.

    try.
        create data lr_data_head type ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        assign lr_data_head->* to <ls_data_head>.

        create data lr_data_usobx_ext type ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        assign lr_data_usobx_ext->* to <lt_data_usobx_ext>.
        assign lr_data_usobx_ext->* to <ls_data_usobx_ext>.

        create data lr_data_usobt_ext type ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        assign lr_data_usobt_ext->* to <lt_data_usobt_ext>.
        assign lr_data_usobt_ext->* to <ls_data_usobt_ext>.

        create object lo_su22
          type ('CL_SU22_ADT_OBJECT').

        try.
            call method lo_su22->('IF_SU22_ADT_OBJECT~SELECT')
              exporting
                iv_key       = ls_key
              importing
                es_head      = <ls_data_head>
                et_usobx     = lt_usobx
                et_usobt     = lt_usobt
                et_usobx_ext = <lt_data_usobx_ext>
                et_usobt_ext = <lt_data_usobt_ext>.
          catch cx_su2n_raise_events into data(lr_err).
            data(mtext) = lr_err->get_text( ).
            ii_log->add_error( iv_msg = mtext is_item = ms_item ).
        endtry.

        "HEAD
        io_xml->add( iv_name = 'HEAD'
                     ig_data = <ls_data_head> ).

        "USOBX
        io_xml->add( iv_name = 'USOBX'
                     ig_data = lt_usobx ).

        "USOBT
        io_xml->add( iv_name = 'USOBT'
                     ig_data = lt_usobt ).

        "USOBX_EXT
        io_xml->add( iv_name = 'USOBX_EXT'
                     ig_data = <lt_data_usobx_ext> ).

        "USOBT_EXT
        io_xml->add( iv_name = 'USOBT_EXT'
                     ig_data = <lt_data_usobt_ext> ).

      catch cx_root into data(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    endtry.

  endmethod.
ENDCLASS.
