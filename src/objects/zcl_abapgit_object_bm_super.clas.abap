"! Process modelling object types base class
"! <p>
"! Requires preconfiguration using report SBMCV000 to set up prefix number ranges.
"! </p>
CLASS zcl_abapgit_object_bm_super DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.
  PROTECTED SECTION.
    TYPES:
      ty_specific_fields TYPE STANDARD TABLE OF scrfname WITH DEFAULT KEY.
    METHODS:
      serialize_specific_data ABSTRACT IMPORTING io_xml                  TYPE REF TO zif_abapgit_xml_output
                                                 VALUE(ig_specific_data) TYPE data
                                       RAISING   zcx_abapgit_exception,
      deserialize_specific_data ABSTRACT IMPORTING io_xml             TYPE REF TO zif_abapgit_xml_input
                                         EXPORTING et_specific_fields TYPE ty_specific_fields
                                         CHANGING  cg_specific_data   TYPE data
                                         RAISING   zcx_abapgit_exception,
      get_specific_data_descr ABSTRACT RETURNING VALUE(ro_descriptor) TYPE REF TO cl_abap_structdescr,
      map_remote_to_local_tadir IMPORTING is_item               TYPE zif_abapgit_definitions=>ty_item
                                RETURNING VALUE(rv_object_name) TYPE sobj_name
                                RAISING   zcx_abapgit_exception,
      get_local_tadir_object_name RETURNING VALUE(rv_object_name) TYPE sobj_name
                                  RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_maintain_modes,
        show   TYPE rpygsmode VALUE 'SHOW',
        modify TYPE rpygsmode VALUE 'MODIFY',
        insert TYPE rpygsmode VALUE 'INSERT',
        delete TYPE rpygsmode VALUE 'DELETE',
      END OF c_maintain_modes,
      c_general_data_struct_name TYPE strukname VALUE 'BMOBJECT'.
    DATA:
      mv_local_obj_name TYPE sobj_name.
ENDCLASS.



CLASS zcl_abapgit_object_bm_super IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.
    DATA: ls_general_data TYPE bmobject,
          lv_object_id    TYPE ufobjdef.

    lv_object_id = get_local_tadir_object_name( ).
    IF lv_object_id IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

    CALL FUNCTION 'BM_OBJECT_GET_DATA'
      EXPORTING
        object_type       = ms_item-obj_type
        object_id         = lv_object_id
        language          = mv_language
      CHANGING
        general_data      = ls_general_data
      EXCEPTIONS
        not_found         = 1
        wrong_object_type = 2
        OTHERS            = 3.
    CASE sy-subrc.
      WHEN 0.
        rv_user = ls_general_data-lstuser.
      WHEN 1.
        rv_user = c_user_unknown.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    DATA: lv_object_id  TYPE ufobjdef,
          lv_is_deleted TYPE abap_bool.

    lv_object_id = get_local_tadir_object_name( ).
    IF lv_object_id IS INITIAL.
      zcx_abapgit_exception=>raise( |Could not find { ms_item-obj_type }-{ ms_item-obj_name } to delete| ).
    ENDIF.

    CALL FUNCTION 'BM_OBJECT_MAINTAIN'
      EXPORTING
        i_object_type  = ms_item-obj_type
        i_object_id    = lv_object_id
        i_mode         = c_maintain_modes-delete
      EXCEPTIONS
        error_occurred = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.
    DATA: lv_object_id           TYPE ufobjdef,
          ls_general_data        TYPE bmobject,
          lo_specific_data_descr TYPE REF TO cl_abap_structdescr,
          lr_specific_data       TYPE REF TO data,
          lt_specific_fields     TYPE ty_specific_fields,
          lv_mode                TYPE rpygsmode.
    FIELD-SYMBOLS: <lg_specific_data> TYPE data.

    lv_object_id = get_local_tadir_object_name( ).

    io_xml->read( EXPORTING iv_name = c_general_data_struct_name
                  CHANGING  cg_data = ls_general_data ).

    ls_general_data-type = ms_item-obj_type.

    lo_specific_data_descr = get_specific_data_descr( ).
    ASSERT lo_specific_data_descr IS BOUND.

    CREATE DATA lr_specific_data TYPE HANDLE lo_specific_data_descr.
    ASSERT sy-subrc = 0 AND lr_specific_data IS BOUND.
    ASSIGN lr_specific_data->* TO <lg_specific_data>.
    ASSERT sy-subrc = 0.

    deserialize_specific_data( EXPORTING io_xml             = io_xml
                               IMPORTING et_specific_fields = lt_specific_fields
                               CHANGING  cg_specific_data   = <lg_specific_data> ).

    IF zif_abapgit_object~exists( ) = abap_true.
      lv_mode = c_maintain_modes-modify.
      ASSERT lv_object_id IS NOT INITIAL.
    ELSE.
      lv_mode = c_maintain_modes-insert.
      ASSERT lv_object_id IS INITIAL.
    ENDIF.

    CALL FUNCTION 'BM_OBJECT_MAINTAIN'
      EXPORTING
        i_object_type  = ms_item-obj_type
        i_object_id    = lv_object_id
        i_devclass     = iv_package
        i_language     = mv_language
        i_mode         = lv_mode
      IMPORTING
        e_object_data  = ls_general_data
      TABLES
        t_fieldcat     = lt_specific_fields
      CHANGING
        r_object       = <lg_specific_data>
      EXCEPTIONS
        error_occurred = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.
    DATA: lv_object_id  TYPE ufobjdef,
          lv_is_deleted TYPE abap_bool.

    lv_object_id = get_local_tadir_object_name( ).
    IF lv_object_id IS INITIAL.
      rv_bool = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BM_OBJECT_GET_DATA'
      EXPORTING
        object_type       = ms_item-obj_type
        object_id         = lv_object_id
        language          = mv_language
      IMPORTING
        is_deleted        = lv_is_deleted
      EXCEPTIONS
        not_found         = 1
        wrong_object_type = 2
        OTHERS            = 3.
    CASE sy-subrc.
      WHEN 0.
        rv_bool = boolc( lv_is_deleted = abap_false ).
      WHEN 1.
        rv_bool = abap_false.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'RSUFO'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.

  METHOD zif_abapgit_object~jump.
    DATA: lv_object_id TYPE ufobjdef.

    lv_object_id = get_local_tadir_object_name( ).

    " Hack to set popup size
    sy-scols = 110.
    sy-srows = 20.

    CALL FUNCTION 'BM_OBJECT_MAINTAIN'
      EXPORTING
        i_object_type  = ms_item-obj_type
        i_object_id    = lv_object_id
        i_mode         = c_maintain_modes-show
        i_screen       = abap_true
        i_popup        = abap_true
      EXCEPTIONS
        error_occurred = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.
    DATA: ls_general_data        TYPE bmobject,
          lv_is_deleted          TYPE abap_bool,
          lv_object_id           TYPE ufobjdef,
          lo_specific_data_descr TYPE REF TO cl_abap_structdescr,
          lr_specific_data       TYPE REF TO data.
    FIELD-SYMBOLS: <lg_specific_data> TYPE data.

    lv_object_id = get_local_tadir_object_name( ).
    ASSERT lv_object_id IS NOT INITIAL.

    lo_specific_data_descr = get_specific_data_descr( ).
    ASSERT lo_specific_data_descr IS BOUND.

    CREATE DATA lr_specific_data TYPE HANDLE lo_specific_data_descr.
    ASSERT sy-subrc = 0 AND lr_specific_data IS BOUND.
    ASSIGN lr_specific_data->* TO <lg_specific_data>.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'BM_OBJECT_GET_DATA'
      EXPORTING
        object_type       = ms_item-obj_type
        object_id         = lv_object_id
        language          = mv_language
      IMPORTING
        is_deleted        = lv_is_deleted
      CHANGING
        general_data      = ls_general_data
        specific_data     = <lg_specific_data>
      EXCEPTIONS
        not_found         = 1
        wrong_object_type = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_is_deleted = abap_true.
      " Should not happen because of exists()
      zcx_abapgit_exception=>raise( |{ ms_item-obj_type }-{ ms_item-obj_name } does not exist| ).
    ENDIF.

    CLEAR: ls_general_data-fstuser,
           ls_general_data-fsttime,
           ls_general_data-fstdate,
           ls_general_data-lstuser,
           ls_general_data-lsttime,
           ls_general_data-lstdate,
           ls_general_data-type,
           ls_general_data-id.

    io_xml->add( iv_name = c_general_data_struct_name ig_data = ls_general_data ).

    serialize_specific_data( io_xml = io_xml ig_specific_data = <lg_specific_data> ).
  ENDMETHOD.

  METHOD map_remote_to_local_tadir.
    rv_object_name = is_item-obj_name.
  ENDMETHOD.

  METHOD get_local_tadir_object_name.
    IF mv_local_obj_name IS INITIAL.
      mv_local_obj_name = map_remote_to_local_tadir( ms_item ).
    ENDIF.
    rv_object_name = mv_local_obj_name.
  ENDMETHOD.
ENDCLASS.
