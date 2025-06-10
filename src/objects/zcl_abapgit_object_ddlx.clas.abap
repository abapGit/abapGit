CLASS zcl_abapgit_object_ddlx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

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
    DATA mi_persistence TYPE REF TO if_wb_object_persist.
    DATA mi_data_model TYPE REF TO if_wb_object_data_model.
    DATA mv_object_key TYPE seu_objkey.
    METHODS clear_fields
      CHANGING
        !cg_data TYPE any
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS zcl_abapgit_object_ddlx IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    mv_object_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT mi_persistence
          TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

        CREATE OBJECT mi_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_data>       TYPE any,
      <lg_changed_by> TYPE data.

    CREATE DATA lr_data
      TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_object_key
            p_version     = swbm_version_active
          CHANGING
            p_object_data = mi_data_model ).
      CATCH cx_swb_exception.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    mi_data_model->get_data( IMPORTING p_data = <lg_data> ).

    ASSIGN COMPONENT 'METADATA-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.
    rv_user = <lg_changed_by>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_swb_exception.

    TRY.
        mi_persistence->delete( p_object_key = mv_object_key
                                p_version    = swbm_version_active ).

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lr_data  TYPE REF TO data,
          lx_error TYPE REF TO cx_swb_exception.

    FIELD-SYMBOLS: <lg_data>                  TYPE any,
                   <lg_source>                TYPE data,
                   <lg_version>               TYPE data,
                   <lg_package>               TYPE data,
                   <lg_changed_by>            TYPE syuname,
                   <lg_changed_at>            TYPE xsddatetime_z,
                   <lg_abap_language_version> TYPE data.

    CREATE DATA lr_data
      TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.

    io_xml->read(
      EXPORTING
        iv_name = 'DDLX'
      CHANGING
        cg_data = <lg_data> ).

    ASSIGN COMPONENT 'METADATA-ABAP_LANGU_VERSION' OF STRUCTURE <lg_data> TO <lg_abap_language_version>.
    IF sy-subrc = 0.
      set_abap_language_version( CHANGING cv_abap_language_version = <lg_abap_language_version> ).
    ENDIF.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
    ASSERT sy-subrc = 0.

    TRY.
        " If the file doesn't exist that's ok, because previously
        " the source code was stored in the xml. We are downward compatible.
        <lg_source> = mo_files->read_string( 'asddlxs' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

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

    ASSIGN COMPONENT 'METADATA-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    IF <lg_changed_by> IS ASSIGNED.
      <lg_changed_by> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'METADATA-CHANGED_AT' OF STRUCTURE <lg_data> TO <lg_changed_at>.
    IF <lg_changed_at> IS ASSIGNED.
      GET TIME STAMP FIELD <lg_changed_at>.
    ENDIF.

    mi_data_model->set_data( <lg_data> ).

    TRY.
        mi_persistence->save( mi_data_model ).
      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    tadir_insert( iv_package ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = abap_true.

    TRY.
        mi_persistence->get( p_object_key           = mv_object_key
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

    DATA: lr_data  TYPE REF TO data,
          lx_error TYPE REF TO cx_swb_exception.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE data.


    CREATE DATA lr_data
      TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        IF zcl_abapgit_factory=>get_environment( )->compare_with_inactive( ) = abap_true.
          "Retrieve inactive version
          mi_persistence->get(
            EXPORTING
              p_object_key  = mv_object_key
              p_version     = swbm_version_inactive
            CHANGING
              p_object_data = mi_data_model ).
          IF mi_data_model->get_object_name( ) IS INITIAL.
            "Fallback: retrieve active version
            mi_persistence->get(
              EXPORTING
                p_object_key  = mv_object_key
                p_version     = swbm_version_active
              CHANGING
                p_object_data = mi_data_model ).
          ENDIF.
        ELSE.
          "Retrieve active version
          mi_persistence->get(
            EXPORTING
              p_object_key  = mv_object_key
              p_version     = swbm_version_active
            CHANGING
              p_object_data = mi_data_model ).
        ENDIF.

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    mi_data_model->get_data( IMPORTING p_data = <lg_data> ).

    clear_fields( CHANGING cg_data = <lg_data> ).

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.

    mo_files->add_string(
      iv_ext    = 'asddlxs'
      iv_string = <lg_field> ).

    CLEAR <lg_field>.

    io_xml->add( iv_name = 'DDLX'
                 ig_data = <lg_data> ).

  ENDMETHOD.


  METHOD clear_fields.

    DATA:
      BEGIN OF ls_fields_to_clear,
        BEGIN OF metadata,
          changed_at    TYPE d,
          changed_by    TYPE c,
          created_at    TYPE d,
          created_by    TYPE c,
          responsible   TYPE c,
          BEGIN OF package_ref,
            name TYPE c,
          END OF package_ref,
          BEGIN OF container_ref,
            name TYPE c,
          END OF container_ref,
          version       TYPE c,
          master_system TYPE c,
        END OF metadata,
      END OF ls_fields_to_clear.

    FIELD-SYMBOLS:
      <lg_abap_language_version> TYPE any.

    MOVE-CORRESPONDING ls_fields_to_clear TO cg_data.

    ASSIGN COMPONENT 'METADATA-ABAP_LANGUAGE_VERSION' OF STRUCTURE cg_data TO <lg_abap_language_version>.
    IF sy-subrc = 0.
      clear_abap_language_version( CHANGING cv_abap_language_version = <lg_abap_language_version> ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
