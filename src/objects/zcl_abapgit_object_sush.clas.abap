CLASS zcl_abapgit_object_sush DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SUSH IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    DATA ls_key TYPE usobkey.

    ls_key = ms_item-obj_name.

    SELECT SINGLE modifier FROM usob_sm INTO rv_user
      WHERE name = ls_key-name AND type = ls_key-type.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA:
      lo_su22 TYPE REF TO object,
      ls_key  TYPE        usobkey,
      lr_err  TYPE REF TO cx_static_check.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~DELETE')
          EXPORTING
            iv_key     = ls_key
            iv_cleanup = abap_true.
      CATCH cx_static_check INTO lr_err.
        zcx_abapgit_exception=>raise( iv_text = lr_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lo_appl           TYPE REF TO object,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_appl_head      TYPE REF TO data,
      lr_data_head      TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data,
      lr_err            TYPE REF TO cx_static_check,
      lv_text           TYPE string.

    FIELD-SYMBOLS: <ls_data_head>      TYPE any,
                   <ls_appl_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_devclass>       TYPE any.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.

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

        " check if lead application exists
        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~CHECK')
              EXPORTING
                id_mode = '02'
              CHANGING
                cs_head = <ls_data_head>.
          CATCH cx_static_check INTO lr_err.
            lv_text = |Lead application of object { ms_item-obj_name } does not exist|.
            zcx_abapgit_exception=>raise( lv_text ).
        ENDTRY.

        MOVE-CORRESPONDING <ls_data_head> TO ls_key.
        CREATE DATA lr_appl_head TYPE ('CL_SU2X=>TS_HEAD').
        ASSIGN lr_appl_head->* TO <ls_appl_head>.

        CREATE OBJECT lo_appl TYPE ('CL_SU22_APPL').

        CALL METHOD lo_appl->('GET_DATA')
          EXPORTING
            is_key  = ls_key
          IMPORTING
            es_head = <ls_appl_head>.

        ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_appl_head> TO <ls_devclass>.
        IF <ls_devclass> <> iv_package.
          lv_text =
          |Lead application of object { ms_item-obj_name } does not exist in package { <ls_devclass> }|.
          zcx_abapgit_exception=>raise( lv_text ).
        ENDIF.

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              EXPORTING
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          CATCH cx_static_check INTO lr_err.
            zcx_abapgit_exception=>raise( iv_text     = lr_err->get_text( )
                                          ix_previous = lr_err ).
        ENDTRY.

        corr_insert( iv_package ).

      CATCH cx_static_check INTO lr_err.
        zcx_abapgit_exception=>raise( iv_text     = lr_err->get_text( )
                                      ix_previous = lr_err ).
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
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_data_head      TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data,
      lr_err            TYPE REF TO cx_static_check,
      lx_error          TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_data_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <lt_data_usobt_ext> TYPE ANY TABLE.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.

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
          CATCH cx_static_check INTO lr_err.
            zcx_abapgit_exception=>raise( iv_text     = lr_err->get_text( )
                                          ix_previous = lr_err ).
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
