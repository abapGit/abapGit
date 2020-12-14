CLASS zcl_abapgit_object_sush DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS check_exist_and_name_space
      IMPORTING
        !io_su22    TYPE REF TO cl_su22_adt_object OPTIONAL
        !is_head    TYPE if_su22_adt_object=>ts_su2x_head
        !iv_package TYPE devclass
      RAISING
        cx_su2n_raise_events .
    METHODS allown_objects_for_sush
      IMPORTING
        !is_head                  TYPE if_su22_adt_object=>ts_su2x_head
        !it_usobx_ext             TYPE if_su22_adt_object=>tt_su2x_x
        !it_usobt_ext             TYPE if_su22_adt_object=>tt_su2x_t
        !iv_package               TYPE devclass
      EXPORTING
        !et_messages              TYPE bapiret2_t
        !et_object_not_to_be_used TYPE if_ars_abap_object_check=>ty_gts_object_not_to_be_used
      RAISING
        cx_su2n_raise_events .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SUSH IMPLEMENTATION.


  METHOD allown_objects_for_sush.
    DATA:
      ls_data         TYPE cl_su22_data_model=>ty_object_data,
      lv_value_string TYPE su22_value_string,
      lv_item_string  TYPE su22_value_string.

    IF cl_cos_utilities=>is_cloud( ) = abap_true.
      TRY.
          DATA(lv_language_version) = cl_abap_language_version=>get_instance( )->get_default_version( EXPORTING iv_object_type = 'SUSH'
                                                                                                                iv_package     = iv_package ).
          IF lv_language_version = if_abap_language_version=>gc_version-sap_cloud_platform.

            ls_data-content       = CORRESPONDING #( is_head ).
            ls_data-content-name  = is_head-name.
            MOVE-CORRESPONDING it_usobx_ext TO ls_data-content-auth_objects.

            LOOP AT ls_data-content-auth_objects ASSIGNING FIELD-SYMBOL(<ls_auth_object>).
              IF <ls_auth_object>-okflag CA 'YI'.
                TRY.
                    cl_su21_adt_object=>create_instance( )->select(
                      EXPORTING
                        iv_objct      = <ls_auth_object>-object
                      IMPORTING
                        et_auth_field = DATA(lt_object_field) ).
                  CATCH cx_su2n_raise_events.
                    RAISE EXCEPTION TYPE cx_su2n_raise_events
                      EXPORTING
                        textid = cl_su2x=>convert_to_exception( ).
                ENDTRY.
              ENDIF.

              LOOP AT lt_object_field ASSIGNING FIELD-SYMBOL(<ls_object_field>).
                CLEAR lv_value_string.
                APPEND INITIAL LINE TO <ls_auth_object>-auth_fields ASSIGNING FIELD-SYMBOL(<ls_auth_field>).

                <ls_auth_field>-field_name = <ls_object_field>-name.
                LOOP AT it_usobt_ext ASSIGNING FIELD-SYMBOL(<ls_auth_value>)
                    WHERE object = <ls_auth_object>-object AND field = <ls_auth_field>-field_name
                    AND NOT ( low IS INITIAL AND high IS INITIAL ).
                  APPEND <ls_auth_value> TO <ls_auth_field>-field_values.
                  CLEAR lv_item_string.
                  IF <ls_auth_value>-low  IS NOT INITIAL.
                    lv_item_string = lv_item_string && ` ` && <ls_auth_value>-low.
                    IF <ls_auth_value>-high IS NOT INITIAL.
                      lv_item_string = lv_item_string && |..| && <ls_auth_value>-high.
                    ENDIF.
                  ENDIF.

                  IF lv_item_string IS NOT INITIAL.
                    lv_value_string = lv_value_string && lv_item_string && |,|.
                  ENDIF.

                ENDLOOP.

                IF lv_value_string CP '*,'.
                  SHIFT lv_value_string RIGHT DELETING TRAILING space.
                  SHIFT lv_value_string RIGHT BY 1 PLACES.
                  SHIFT lv_value_string LEFT DELETING LEADING space.
                  "condense lv_value_string.
                ENDIF.

                <ls_auth_field>-field_value_string = lv_value_string.
              ENDLOOP.
            ENDLOOP.

            TRY.
                cl_susr_wbo_util=>check_whitelisting_for_sush(
                  EXPORTING
                    is_data                  = ls_data
                    iv_abap_language_version = lv_language_version
                IMPORTING
                  et_object_not_to_be_used = et_object_not_to_be_used
              ).

                LOOP AT et_object_not_to_be_used ASSIGNING FIELD-SYMBOL(<ls_object_not_to_be_used>).
                  APPEND VALUE #( type = <ls_object_not_to_be_used>-message-msgty
                                  id   = <ls_object_not_to_be_used>-message-msgid
                                  number = <ls_object_not_to_be_used>-message-msgno
                                  message_v1 = <ls_object_not_to_be_used>-message-msgv1
                                  message_v2 = <ls_object_not_to_be_used>-message-msgv2
                                  message_v3 = <ls_object_not_to_be_used>-message-msgv3
                                  message_v4 = <ls_object_not_to_be_used>-message-msgv4 ) TO et_messages.
                ENDLOOP.
              CATCH cx_swb_exception.
                RAISE EXCEPTION TYPE cx_su2n_raise_events
                  EXPORTING
                    textid = cl_su2x=>convert_to_exception( ).
            ENDTRY.
          ENDIF.
        CATCH cx_abap_language_version.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD check_exist_and_name_space.
    DATA:
      lo_appl      TYPE REF TO cl_su22_appl,
      ls_key       TYPE usobkey,
      ld_msg       TYPE string,
      lv_text(100) TYPE c,
      lv_text1     TYPE symsgv,
      lv_text2     TYPE symsgv,
      ls_head      TYPE cl_su2x=>ts_head,
      lo_su22      TYPE REF TO cl_su22_adt_object.

    CREATE OBJECT lo_su22.
    DATA(ls_su22_head) = is_head.
    TRY.
        lo_su22->if_su22_adt_object~check( EXPORTING id_mode   = '02'
                                           CHANGING  cs_head   = ls_su22_head ).
      CATCH cx_su2n_raise_events.
        lv_text = 'Lead application of object &1 does not exist'(001).
        REPLACE '&1' WITH is_head-name INTO lv_text.
        lv_text1 = lv_text(50).
        lv_text2 = lv_text+50(50).
        MESSAGE s471(s#) WITH lv_text1 lv_text2 INTO ld_msg.
        RAISE EXCEPTION TYPE cx_su2n_raise_events EXPORTING textid = cl_su2x=>convert_to_exception( ).
    ENDTRY.

    MOVE-CORRESPONDING is_head TO ls_key.
    CREATE OBJECT lo_appl.
    lo_appl->get_data( EXPORTING is_key   = ls_key
                       IMPORTING es_head  = ls_head ).
    IF ls_head-devclass <> iv_package.
      lv_text = 'Lead application of object &1 does not exist package &2'(002).
      REPLACE '&1' WITH is_head-name INTO lv_text.
      REPLACE '&2' WITH iv_package INTO lv_text.
      lv_text1 = lv_text(50).
      lv_text2 = lv_text+50(50).
      MESSAGE s471(s#) WITH lv_text1 lv_text2 INTO ld_msg.
      RAISE EXCEPTION TYPE cx_su2n_raise_events EXPORTING textid = cl_su2x=>convert_to_exception( ).
    ELSE.
      IF cl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ). "[A4C_AGIT]
        " Lock Objects
        IF ls_head-obj_name(1) EQ 'Y' OR
           ls_head-obj_name(1) EQ 'Z' OR
           ls_head-obj_name(1) EQ '/' OR
           ls_head-obj_name(1) EQ 'J'.
        ELSE.
          lv_text = 'Namespace of object &1 (type SUSH) not allowed in SAP Cloud Platform'(003).
          REPLACE '&1' WITH is_head-name INTO lv_text.
          lv_text1 = lv_text(50).
          lv_text2 = lv_text+50(50).
          MESSAGE s471(s#) WITH lv_text1 lv_text2 INTO ld_msg.
          RAISE EXCEPTION TYPE cx_su2n_raise_events EXPORTING textid = cl_su2x=>convert_to_exception( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA:
      lo_su22 TYPE REF TO cl_su22_adt_object,
      ls_key  TYPE        usobkey,
      lx_msg  TYPE REF TO cx_su2n_raise_events.

    ASSERT NOT ms_item-obj_name IS INITIAL.
    CREATE OBJECT lo_su22.

    ls_key = ms_item-obj_name.

    TRY.
        lo_su22->if_su22_adt_object~delete( EXPORTING iv_key       = ls_key
                                                      iv_cleanup   = abap_true   ).
      CATCH cx_su2n_raise_events INTO lx_msg.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lr_data_head      TYPE REF TO data,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_data_usobx     TYPE REF TO data,
      lr_data_usobt     TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data,
      lv_package        TYPE devclass.

    FIELD-SYMBOLS: <lt_data_head>      TYPE ANY TABLE,
                   <ls_data_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <ls_data_usobx_ext> TYPE any,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_data_usobt_ext> TYPE any,
                   <lt_field>          TYPE any.

    FIELD-SYMBOLS: <ls_head_source>      TYPE any,
                   <ls_usobx_ext_source> TYPE any,
                   <ls_usobt_ext_source> TYPE any.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.
        ASSIGN lr_data_usobx_ext->* TO <ls_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.
        ASSIGN lr_data_usobt_ext->* TO <ls_data_usobt_ext>.

        "HEAD
        io_xml->read( EXPORTING iv_name = 'HEAD'
                      CHANGING  cg_data = <ls_data_head> ).

        "USOBX
        io_xml->read( EXPORTING iv_name = 'USOBX'
                      CHANGING  cg_data = lt_usobx ).

        "USOBT
        io_xml->read( EXPORTING iv_name = 'USOBT'
                      CHANGING  cg_data = lt_usobt ).

        "USOBX_EXT
        io_xml->read( EXPORTING iv_name = 'USOBX_EXT'
                      CHANGING  cg_data = <lt_data_usobx_ext> ).

        "USOBT_EXT
        io_xml->read( EXPORTING iv_name = 'USOBT_EXT'
                      CHANGING  cg_data = <lt_data_usobt_ext> ).

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        IF iv_package IS INITIAL.
          ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_data_head> TO FIELD-SYMBOL(<ls_devclass>).
          ASSERT sy-subrc = 0.
          lv_package = <ls_devclass>.
        ELSE.
          lv_package = iv_package.
        ENDIF.

        " check the existence and name space for Cloud Platform ABAP Environment in customer system
        TRY.
            check_exist_and_name_space( EXPORTING is_head    = <ls_data_head>
                                                  iv_package = lv_package ).
          CATCH cx_su2n_raise_events INTO DATA(lr_err).
            DATA(ltext) = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
            RETURN.
        ENDTRY.

        TRY.
            allown_objects_for_sush( EXPORTING is_head                  = <ls_data_head>
                                               it_usobx_ext             = <lt_data_usobx_ext>
                                               it_usobt_ext             = <lt_data_usobt_ext>
                                               iv_package               = lv_package
                                     IMPORTING et_object_not_to_be_used = DATA(lt_object_not_to_be_used) ).

            LOOP AT lt_object_not_to_be_used REFERENCE INTO DATA(lr_object_not_to_be_used).
              DELETE lt_usobx  WHERE object = lr_object_not_to_be_used->object-name.
              DELETE lt_usobt  WHERE object = lr_object_not_to_be_used->object-name.
              MESSAGE ID lr_object_not_to_be_used->message-msgid
               TYPE lr_object_not_to_be_used->message-msgty
             NUMBER lr_object_not_to_be_used->message-msgno
               WITH lr_object_not_to_be_used->message-msgv1 lr_object_not_to_be_used->message-msgv2 lr_object_not_to_be_used->message-msgv3 lr_object_not_to_be_used->message-msgv4
               INTO DATA(lv_message).
              ii_log->add_warning( iv_msg = lv_message is_item = ms_item ).
              rv_complete_status = if_abapgit_object=>c_complete_status-partly.
            ENDLOOP.

          CATCH cx_su2n_raise_events INTO lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).

        ENDTRY.

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              EXPORTING
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          CATCH cx_su2n_raise_events INTO lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
        ENDTRY.

        IF iv_unittest IS INITIAL.
          corr_insert( iv_package ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: ls_usobhash TYPE usobhash.

    SELECT SINGLE * FROM usobhash INTO ls_usobhash "#EC CI_ALL_FIELDS_NEEDED
        WHERE name = ms_item-obj_name.                "#EC CI_SGLSELECT

    rv_bool = boolc( sy-subrc = 0 ).

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


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = CONV #( ms_item-obj_name )
                                          iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    DATA: ls_key          TYPE usobkey.

    ls_key-type = ms_item-obj_type.
    ls_key-name = ms_item-obj_name.

    CALL FUNCTION 'SU2X_DIALOG_SNGL'
      EXPORTING
        is_key       = ls_key
        id_area      = 'SU22'
        id_actvt     = cl_suso=>gc_show
        id_disp_only = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lt_clr_comps      TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_data_head      TYPE REF TO data,
      lr_data_usobx     TYPE REF TO data,
      lr_data_usobt     TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <ls_data_usobx_ext> TYPE any,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_data_usobt_ext> TYPE any,
                   <lt_field>          TYPE any,
                   <lv_comp>           LIKE LINE OF lt_clr_comps.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.
        ASSIGN lr_data_usobx_ext->* TO <ls_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.
        ASSIGN lr_data_usobt_ext->* TO <ls_data_usobt_ext>.

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~SELECT')
              EXPORTING
                iv_key       = ls_key
              IMPORTING
                es_head      = <ls_data_head>
                et_usobx     = lt_usobx
                et_usobt     = lt_usobt
                et_usobx_ext = <lt_data_usobx_ext>
                et_usobt_ext = <lt_data_usobt_ext>.
          CATCH cx_su2n_raise_events INTO DATA(lr_err).
            DATA(mtext) = lr_err->get_text( ).
            ii_log->add_error( iv_msg = mtext is_item = ms_item ).
        ENDTRY.

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

      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
