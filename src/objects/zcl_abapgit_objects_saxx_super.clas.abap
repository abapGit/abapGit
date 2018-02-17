CLASS zcl_abapgit_objects_saxx_super DEFINITION PUBLIC ABSTRACT
                                INHERITING FROM zcl_abapgit_objects_super.
* common class for SAPC and SAMC objects

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.

  PROTECTED SECTION.
    METHODS:
      get_persistence_class_name ABSTRACT
        RETURNING
          VALUE(r_persistence_class_name) TYPE seoclsname,

      get_data_class_name ABSTRACT
        RETURNING
          VALUE(r_data_class_name) TYPE seoclsname,

      get_data_structure_name ABSTRACT
        RETURNING
          VALUE(r_data_structure_name) TYPE string.

  PRIVATE SECTION.
    DATA: mo_persistence          TYPE REF TO if_wb_object_persist,
          mo_appl_obj_data        TYPE REF TO if_wb_object_data_model,
          mv_data_structure_name  TYPE string,
          mv_appl_obj_cls_name    TYPE seoclsname,
          mv_persistence_cls_name TYPE seoclsname.

    METHODS:
      create_channel_objects
        RAISING
          zcx_abapgit_exception,

      get_data
        EXPORTING
          p_data TYPE any
        RAISING
          zcx_abapgit_exception,

      lock
        RAISING
          zcx_abapgit_exception,

      unlock
        RAISING
          zcx_abapgit_exception,

      get_names.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_SAXX_SUPER IMPLEMENTATION.


  METHOD create_channel_objects.

    get_names( ).

    TRY.
        IF mo_appl_obj_data IS NOT BOUND.
          CREATE OBJECT mo_appl_obj_data TYPE (mv_appl_obj_cls_name).
        ENDIF.

        IF mo_persistence IS NOT BOUND.
          CREATE OBJECT mo_persistence TYPE (mv_persistence_cls_name).
        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_data.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    TRY.
        mo_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = 'A'
          CHANGING
            p_object_data = mo_appl_obj_data ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    mo_appl_obj_data->get_data(
      IMPORTING
        p_data = p_data ).

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

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.


    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mo_persistence->lock(
      EXPORTING
        p_objname_tr   = lv_objname
        p_object_key   = lv_object_key
        p_objtype_tr   = lv_objtype
      EXCEPTIONS
        foreign_lock   = 1
        error_occurred = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error occured while locking { ms_item-obj_type } | && lv_objname ).
    ENDIF.

  ENDMETHOD.                    "lock


  METHOD unlock.

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.

    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mo_persistence->unlock( p_objname_tr = lv_objname
                            p_object_key = lv_object_key
                            p_objtype_tr = lv_objtype ).

  ENDMETHOD.                    "unlock


  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>       TYPE any,
                   <lg_header>     TYPE any,
                   <lg_changed_by> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |{ ms_item-obj_name } not supported| ).
    ENDTRY.

    get_data(
      IMPORTING
        p_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.

    IF <lg_changed_by> IS NOT INITIAL.
      rv_user = <lg_changed_by>.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        lock( ).

        mo_persistence->delete( lv_object_key ).

        unlock( ).

      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error occured while deleting { ms_item-obj_type }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    io_xml->read(
      EXPORTING
        iv_name = ms_item-obj_type
      CHANGING
        cg_data = <lg_data> ).

    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( ).
    ENDIF.

    TRY.
        lock( ).

        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
            object              = ms_item-obj_name
            object_class        = ms_item-obj_type
            mode                = 'I'
            global_lock         = abap_true
            devclass            = iv_package
            master_language     = mv_language
          EXCEPTIONS
            cancelled           = 1
            permission_failure  = 2
            unknown_objectclass = 3
            OTHERS              = 4.

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
        ENDIF.

        mo_appl_obj_data->set_data( <lg_data> ).

        mo_persistence->save( mo_appl_obj_data ).

        unlock( ).

      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        mo_persistence->get( p_object_key           = lv_object_key
                             p_version              = 'A'
                             p_existence_check_only = abap_true  ).

      CATCH cx_swb_object_does_not_exist cx_swb_exception.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lr_data             TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>   TYPE any,
                   <lg_header> TYPE any,
                   <lg_field>  TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    get_data(
      IMPORTING
        p_data = <lg_data> ).

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
