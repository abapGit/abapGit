CLASS zcl_abapgit_object_saxx_super DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  ABSTRACT
  CREATE PUBLIC .

* common class for SAPC and SAMC objects
  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL.

  PROTECTED SECTION.

    METHODS get_persistence_class_name ABSTRACT
      RETURNING
        VALUE(rv_persistence_class_name) TYPE seoclsname .
    METHODS get_data_class_name ABSTRACT
      RETURNING
        VALUE(rv_data_class_name) TYPE seoclsname .
    METHODS get_data_structure_name ABSTRACT
      RETURNING
        VALUE(rv_data_structure_name) TYPE string .
    METHODS get_lock_object ABSTRACT
      RETURNING
        VALUE(rv_lock_object) TYPE string.
    METHODS create_channel_objects
      RAISING
        zcx_abapgit_type_not_supported .

  PRIVATE SECTION.

    DATA mi_persistence TYPE REF TO if_wb_object_persist .
    DATA mi_appl_obj_data TYPE REF TO if_wb_object_data_model .
    DATA mv_data_structure_name TYPE string .
    DATA mv_appl_obj_cls_name TYPE seoclsname .
    DATA mv_persistence_cls_name TYPE seoclsname .
    DATA mv_object_key TYPE seu_objkey .

    METHODS get_data
      EXPORTING
        !eg_data TYPE any
      RAISING
        zcx_abapgit_exception .
    METHODS lock
      RAISING
        zcx_abapgit_exception .
    METHODS unlock
      RAISING
        zcx_abapgit_exception .
    METHODS get_names .

ENDCLASS.



CLASS zcl_abapgit_object_saxx_super IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    mv_object_key = ms_item-obj_name.

  ENDMETHOD.

  METHOD create_channel_objects.

    get_names( ).

    TRY.
        CREATE OBJECT mi_appl_obj_data TYPE (mv_appl_obj_cls_name).
        CREATE OBJECT mi_persistence TYPE (mv_persistence_cls_name).

      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = ms_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD get_data.

    DATA: lx_error TYPE REF TO cx_swb_exception.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_object_key
            p_version     = 'A'
          CHANGING
            p_object_data = mi_appl_obj_data ).

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    mi_appl_obj_data->get_data( IMPORTING p_data = eg_data ).

  ENDMETHOD.


  METHOD get_names.

    IF mv_data_structure_name IS INITIAL.
      mv_data_structure_name  = get_data_structure_name( ).
    ENDIF.

    IF mv_appl_obj_cls_name IS INITIAL.
      mv_appl_obj_cls_name    = get_data_class_name( ).
    ENDIF.

    IF mv_persistence_cls_name IS INITIAL.
      mv_persistence_cls_name = get_persistence_class_name( ).
    ENDIF.

  ENDMETHOD.


  METHOD lock.

    DATA: lv_objname TYPE trobj_name,
          lv_objtype TYPE trobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = ms_item-obj_type.

    mi_persistence->lock(
      EXPORTING
        p_objname_tr   = lv_objname
        p_object_key   = mv_object_key
        p_objtype_tr   = lv_objtype
      EXCEPTIONS
        foreign_lock   = 1
        error_occurred = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error occurred while locking { ms_item-obj_type } { lv_objname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD unlock.

    DATA: lv_objname TYPE trobj_name,
          lv_objtype TYPE trobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = ms_item-obj_type.

    mi_persistence->unlock( p_objname_tr = lv_objname
                            p_object_key = mv_object_key
                            p_objtype_tr = lv_objtype ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>       TYPE any,
                   <lg_changed_by> TYPE any.

    CREATE DATA lr_data TYPE (mv_data_structure_name).
    ASSIGN lr_data->* TO <lg_data>.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.

    IF <lg_changed_by> IS NOT INITIAL.
      rv_user = <lg_changed_by>.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    TRY.
        lock( ).
        mi_persistence->delete( mv_object_key ).
        unlock( ).

      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error occurred while deleting { ms_item-obj_type }| ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data> TYPE any.

    CREATE DATA lr_data TYPE (mv_data_structure_name).
    ASSIGN lr_data->* TO <lg_data>.

    io_xml->read(
      EXPORTING
        iv_name = ms_item-obj_type
      CHANGING
        cg_data = <lg_data> ).

    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    TRY.
        lock( ).
        corr_insert( iv_package ).
        mi_appl_obj_data->set_data( <lg_data> ).
        mi_persistence->save( mi_appl_obj_data ).
        unlock( ).

      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error occurred while creating { ms_item-obj_type }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        mi_persistence->get( p_object_key           = mv_object_key
                             p_version              = 'A'
                             p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

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

    DATA: lv_argument    TYPE seqg3-garg,
          lv_lock_object TYPE string.

    lv_lock_object = get_lock_object( ).

    lv_argument = mv_object_key.
    OVERLAY lv_argument WITH '                              '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for(
                     iv_lock_object = lv_lock_object
                     iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>   TYPE any,
                   <lg_header> TYPE any,
                   <lg_field>  TYPE any.

    CREATE DATA lr_data TYPE (mv_data_structure_name).
    ASSIGN lr_data->* TO <lg_data>.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    io_xml->add( iv_name = ms_item-obj_type
                 ig_data = <lg_data> ).

  ENDMETHOD.

ENDCLASS.
