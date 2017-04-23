*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SAMC
*&---------------------------------------------------------------------*

CLASS lcl_object_samc DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    DATA: mo_persistence       TYPE REF TO if_wb_object_persist,
          mo_amc_appl_obj_data TYPE REF TO if_wb_object_data_model.

    METHODS:
      get_data_object
        RETURNING
          VALUE(ro_amc_appl_obj_data) TYPE REF TO if_wb_object_data_model
        RAISING
          lcx_exception,

      get_persistence
        RETURNING
          VALUE(ro_persistence) TYPE REF TO if_wb_object_persist
        RAISING
          lcx_exception,

      get_data
        EXPORTING
          p_data TYPE any
        RAISING
          lcx_exception,

      lock
        RAISING
          lcx_exception,

      unlock
        RAISING
          lcx_exception.

ENDCLASS.                    "lcl_object_samc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_samc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_samc IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <ls_data>    TYPE any,
                   <ls_header>  TYPE any,
                   <changed_by> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('AMC_APPLICATION_COMPLETE').
        ASSIGN lr_data->* TO <ls_data>.

      CATCH cx_root.
        lcx_exception=>raise( 'SAMC not supported' ).
    ENDTRY.

    get_data(
      IMPORTING
        p_data = <ls_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <ls_data> TO <ls_header>.
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_header> TO <changed_by>.
    ASSERT sy-subrc = 0.

    rv_user = <changed_by>.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata.

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        get_data_object( ).

      CATCH lcx_exception.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lr_data   TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data>   TYPE any,
                   <ls_header> TYPE any,
                   <field>     TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('AMC_APPLICATION_COMPLETE').
        ASSIGN lr_data->* TO <ls_data>.

      CATCH cx_root.
        lcx_exception=>raise( 'SAMC not supported' ).
    ENDTRY.

    get_data(
      IMPORTING
        p_data = <ls_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <ls_data> TO <ls_header>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CHANGED_CLNT' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    ASSIGN COMPONENT 'CREATED_CLNT' OF STRUCTURE <ls_header> TO <field>.
    ASSERT sy-subrc = 0.
    CLEAR <field>.

    io_xml->add( iv_name = 'SAMC'
                 ig_data = <ls_data> ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: li_appl_obj_data TYPE REF TO if_wb_object_data_model,
          lr_data          TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('AMC_APPLICATION_COMPLETE').
        ASSIGN lr_data->* TO <ls_data>.

      CATCH cx_root.
        lcx_exception=>raise( 'SAMC not supported' ).
    ENDTRY.

    io_xml->read(
      EXPORTING
        iv_name = 'SAMC'
      CHANGING
        cg_data = <ls_data> ).

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    li_appl_obj_data = get_data_object( ).

    TRY.
        lock( ).

        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
            object              = ms_item-obj_name
            object_class        = 'SAMC'
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
          lcx_exception=>raise( 'Error occured while creating SAMC' ).
        ENDIF.

        li_appl_obj_data->set_data( <ls_data> ).

        get_persistence( )->save( p_object_data = li_appl_obj_data ).

        unlock( ).

      CATCH cx_swb_exception.
        lcx_exception=>raise( 'Error occured while creating SAMC' ).
    ENDTRY.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: object_key TYPE seu_objkey.

    object_key = ms_item-obj_name.

    TRY.
        lock( ).

        get_persistence( )->delete( p_object_key = object_key ).

        unlock( ).

      CATCH cx_swb_exception.
        lcx_exception=>raise( 'Error occured while deleting SAMC' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.

  ENDMETHOD.

  METHOD get_data_object.

    IF mo_amc_appl_obj_data IS NOT BOUND.

      TRY.
          CREATE OBJECT mo_amc_appl_obj_data TYPE ('CL_AMC_APPLICATION_OBJ_DATA').

        CATCH cx_root.
          lcx_exception=>raise( 'SAMC not supported' ).
      ENDTRY.

    ENDIF.

    ro_amc_appl_obj_data = mo_amc_appl_obj_data.

  ENDMETHOD.


  METHOD get_persistence.

    IF mo_persistence IS NOT BOUND.

      TRY.
          CREATE OBJECT mo_persistence TYPE ('CL_AMC_APPLICATION_OBJ_PERS').

        CATCH cx_root.
          lcx_exception=>raise( 'SAMC not supported' ).
      ENDTRY.

    ENDIF.

    ro_persistence = mo_persistence.

  ENDMETHOD.


  METHOD lock.

    DATA: objname    TYPE trobj_name,
          object_key TYPE seu_objkey,
          objtype    TYPE trobjtype.

    objname    = ms_item-obj_name.
    object_key = ms_item-obj_name.
    objtype    = ms_item-obj_type.

    get_persistence( )->lock(
      EXPORTING
        p_objname_tr   = objname
        p_object_key   = object_key
        p_objtype_tr   = objtype
      EXCEPTIONS
        foreign_lock   = 1
        error_occurred = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      lcx_exception=>raise( `Error occured while locking SAMC ` && objname ).
    ENDIF.

  ENDMETHOD.

  METHOD unlock.

    DATA: objname    TYPE trobj_name,
          object_key TYPE seu_objkey,
          objtype    TYPE trobjtype.

    objname    = ms_item-obj_name.
    object_key = ms_item-obj_name.
    objtype    = ms_item-obj_type.

    get_persistence( )->unlock( p_objname_tr = objname
                                p_object_key = object_key
                                p_objtype_tr = objtype ).

  ENDMETHOD.

  METHOD get_data.

    DATA:  object_key  TYPE seu_objkey.

    object_key = ms_item-obj_name.

    TRY.
        get_persistence( )->get(
          EXPORTING
            p_object_key  = object_key
            p_version     = 'A'
          CHANGING
            p_object_data = mo_amc_appl_obj_data ).

      CATCH cx_root.
        lcx_exception=>raise( 'SAMC error' ).
    ENDTRY.

    mo_amc_appl_obj_data->get_data(
      IMPORTING
        p_data = p_data ).

  ENDMETHOD.


ENDCLASS.                    "lcl_object_samc IMPLEMENTATION
