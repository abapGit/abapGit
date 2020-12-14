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
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SUSH IMPLEMENTATION.


  METHOD check_exist_and_name_space.
    DATA:
      lo_appl      TYPE REF TO cl_su22_appl,
      ls_key       TYPE usobkey,
      ld_msg       TYPE string,
      lv_text(100) TYPE c,
      lv_text1     TYPE symsgv,
      lv_text2     TYPE symsgv,
      ls_head      TYPE cl_su2x=>ts_head,
      lo_su22      TYPE REF TO cl_su22_adt_object,
      ls_su22_head TYPE if_su22_adt_object=>ts_su2x_head.

    CREATE OBJECT lo_su22.
    ls_su22_head = is_head.
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
    ENDIF.

    IF cl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ). "[A4C_AGIT]
      " Lock Objects
      IF ls_head-obj_name(1) = 'Y' OR
         ls_head-obj_name(1) = 'Z' OR
         ls_head-obj_name(1) = '/' OR
         ls_head-obj_name(1) = 'J'.
      ELSE.
        lv_text = 'Namespace of object &1 (type SUSH) not allowed in SAP Cloud Platform'(003).
        REPLACE '&1' WITH is_head-name INTO lv_text.
        lv_text1 = lv_text(50).
        lv_text2 = lv_text+50(50).
        MESSAGE s471(s#) WITH lv_text1 lv_text2 INTO ld_msg.
        RAISE EXCEPTION TYPE cx_su2n_raise_events EXPORTING textid = cl_su2x=>convert_to_exception( ).
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
        lo_su22->if_su22_adt_object~delete( iv_key       = ls_key
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
      lv_package        TYPE devclass,
      lr_err            TYPE REF TO cx_su2n_raise_events,
      ltext             TYPE string,
      lx_error          TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_data_head>      TYPE ANY TABLE,
                   <ls_data_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <ls_data_usobx_ext> TYPE any,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_data_usobt_ext> TYPE any,
                   <lt_field>          TYPE any.

    FIELD-SYMBOLS: <ls_head_source>      TYPE any,
                   <ls_usobx_ext_source> TYPE any,
                   <ls_usobt_ext_source> TYPE any,
                   <ls_devclass>         TYPE any.

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
          ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_data_head> TO <ls_devclass>.
          ASSERT sy-subrc = 0.
          lv_package = <ls_devclass>.
        ELSE.
          lv_package = iv_package.
        ENDIF.

        " check the existence and name space for Cloud Platform ABAP Environment in customer system
        TRY.
            check_exist_and_name_space( is_head    = <ls_data_head>
                                        iv_package = lv_package ).
          CATCH cx_su2n_raise_events INTO lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext
                               is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
            RETURN.
        ENDTRY.

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              EXPORTING
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          CATCH cx_su2n_raise_events INTO lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg = ltext
                               is_item = ms_item ).
            rv_complete_status = if_abapgit_object=>c_complete_status-nothing.
        ENDTRY.

        IF iv_unittest IS INITIAL.
          corr_insert( iv_package ).
        ENDIF.

      CATCH cx_root INTO lx_error.
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
    DATA lv_lock_object TYPE string.
    lv_lock_object = ms_item-obj_name.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = lv_lock_object
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
      lr_data_usobt_ext TYPE REF TO data,
      lr_err            TYPE REF TO cx_su2n_raise_events,
      ltext             TYPE string,
      lx_error          TYPE REF TO cx_root.

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
          CATCH cx_su2n_raise_events INTO lr_err.
            ltext = lr_err->get_text( ).
            ii_log->add_error( iv_msg  = ltext
                               is_item = ms_item ).
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

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
