CLASS zcl_abapgit_object_ddlx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mi_persistence TYPE REF TO if_wb_object_persist .
    METHODS get_persistence
      RETURNING
        VALUE(ri_persistence) TYPE REF TO if_wb_object_persist
      RAISING
        zcx_abapgit_exception .
    METHODS clear_fields
      CHANGING
        !cg_data TYPE any .
    METHODS clear_field
      IMPORTING
        !iv_fieldname TYPE csequence
      CHANGING
        !cg_metadata  TYPE any .
ENDCLASS.



CLASS zcl_abapgit_object_ddlx IMPLEMENTATION.


  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

  ENDMETHOD.


  METHOD clear_fields.

    FIELD-SYMBOLS: <lg_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA'
           OF STRUCTURE cg_data
           TO <lg_metadata>.
    ASSERT sy-subrc = 0.

    clear_field( EXPORTING iv_fieldname = 'CHANGED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CHANGED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'PACKAGE_REF-NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CONTAINER_REF-PACKAGE_NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'MASTER_SYSTEM'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGUAGE_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).
    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGU_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

  ENDMETHOD.


  METHOD get_persistence.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        IF mi_persistence IS NOT BOUND.

          CREATE OBJECT mi_persistence
                 TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_persistence = mi_persistence.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      lv_object_key  TYPE seu_objkey,
      li_data_model  TYPE REF TO if_wb_object_data_model,
      li_persistence TYPE REF TO if_wb_object_persist,
      lr_data        TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_data>       TYPE any,
      <lg_changed_by> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        li_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = swbm_version_active
          CHANGING
            p_object_data = li_data_model ).
      CATCH cx_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    li_data_model->get_data( IMPORTING p_data = <lg_data> ).

    ASSIGN COMPONENT 'METADATA-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.
    rv_user = <lg_changed_by>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey,
          lx_error      TYPE REF TO cx_root.


    lv_object_key = ms_item-obj_name.

    TRY.

        get_persistence( )->delete( p_object_key = lv_object_key
                                    p_version    = swbm_version_active ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: li_data_model TYPE REF TO if_wb_object_data_model,
          lr_data       TYPE REF TO data,
          lx_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>    TYPE any,
                   <lg_source>  TYPE data,
                   <lg_version> TYPE data,
                   <lg_package> TYPE data.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        io_xml->read(
          EXPORTING
            iv_name = 'DDLX'
          CHANGING
            cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.

        TRY.
            " If the file doesn't exist that's ok, because previously
            " the source code was stored in the xml. We are downward compatible.
            <lg_source> = zif_abapgit_object~mo_files->read_string( 'asddlxs' ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        ASSIGN COMPONENT 'METADATA-VERSION' OF STRUCTURE <lg_data> TO <lg_version>.
        ASSERT sy-subrc = 0.

        " We have to always save as inactive. Standard activation below activates then
        " and also creates transport request entry if necessary
        <lg_version> = 'inactive'.

        "package needed to be able to determine ABAP language version
        ASSIGN COMPONENT 'METADATA-PACKAGE_REF-NAME' OF STRUCTURE <lg_data> TO <lg_package>.
        IF <lg_package> IS ASSIGNED.
          <lg_package> = iv_package.
        ENDIF.

        li_data_model->set_data( <lg_data> ).

        get_persistence( )->save( li_data_model ).

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    rv_bool = abap_true.

    TRY.
        get_persistence( )->get( p_object_key           = lv_object_key
                                 p_version              = swbm_version_active
                                 p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_object_key  TYPE seu_objkey,
          li_data_model  TYPE REF TO if_wb_object_data_model,
          li_persistence TYPE REF TO if_wb_object_persist,
          lr_data        TYPE REF TO data,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        IF zcl_abapgit_factory=>get_environment( )->compare_with_inactive( ) = abap_true.
          "Retrieve inactive version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_inactive
            CHANGING
              p_object_data = li_data_model ).
          IF li_data_model->get_object_name( ) IS INITIAL.
            "Fallback: retrieve active version
            li_persistence->get(
              EXPORTING
                p_object_key  = lv_object_key
                p_version     = swbm_version_active
              CHANGING
                p_object_data = li_data_model ).
          ENDIF.
        ELSE.
          "Retrieve active version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_active
            CHANGING
              p_object_data = li_data_model ).
        ENDIF.

        li_data_model->get_data( IMPORTING p_data = <lg_data> ).

        clear_fields( CHANGING cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.

        zif_abapgit_object~mo_files->add_string(
          iv_ext    = 'asddlxs'
          iv_string = <lg_field> ).

        CLEAR <lg_field>.

        io_xml->add( iv_name = 'DDLX'
                     ig_data = <lg_data> ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
