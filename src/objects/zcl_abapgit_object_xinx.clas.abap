CLASS zcl_abapgit_object_xinx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_extension_index,
             dd12v   TYPE dd12v,
             t_dd17v TYPE STANDARD TABLE OF dd17v
                          WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_extension_index.
    DATA:
      mv_name TYPE ddobjname,
      mv_id   TYPE ddobjectid.

ENDCLASS.



CLASS zcl_abapgit_object_xinx IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    cl_wb_object_type=>get_key_components_from_id(
      EXPORTING
        p_key                   = |{ ms_item-obj_name }|
        p_external_id           = swbm_c_type_ddic_db_tabxinx
      IMPORTING
        p_key_component1        = mv_name
        p_key_component2        = mv_id
      EXCEPTIONS
        too_many_key_components = 1
        objecttype_not_existing = 2
        OTHERS                  = 3 ).

    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " RS_DD_INDX_DELETE calls the GUI. Someday we need a better solution

    CALL FUNCTION 'RS_DD_INDX_DELETE'
      EXPORTING
        objname              = mv_name
        indexname            = mv_id
        extension            = abap_true
      EXCEPTIONS
        object_not_found     = 1
        object_not_specified = 2
        permission_failure   = 3
        action_cancelled     = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RS_DD_INDX_DELETE { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_extension_index TYPE ty_extension_index,
          rc                 TYPE sy-subrc.

    io_xml->read(
      EXPORTING
        iv_name = 'XINX'
      CHANGING
        cg_data = ls_extension_index ).

    tadir_insert( iv_package ).

    CALL FUNCTION 'DDIF_INDX_PUT'
      EXPORTING
        name              = mv_name
        id                = mv_id
        dd12v_wa          = ls_extension_index-dd12v
      TABLES
        dd17v_tab         = ls_extension_index-t_dd17v
      EXCEPTIONS
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_PUT { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_INDX_ACTIVATE'
      EXPORTING
        name        = mv_name
        id          = mv_id
      IMPORTING
        rc          = rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_ACTIVATE { sy-subrc }| ).
    ENDIF.

    IF rc <> 0.
      zcx_abapgit_exception=>raise( |Cannot activate extension index { mv_id } of table { mv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_dd12v TYPE dd12v.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
      IMPORTING
        dd12v_wa      = lv_dd12v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_bool = boolc( lv_dd12v IS NOT INITIAL ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RS_TOOL_ACCESS { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_extension_index TYPE ty_extension_index.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
        langu         = sy-langu
      IMPORTING
        dd12v_wa      = ls_extension_index-dd12v
      TABLES
        dd17v_tab     = ls_extension_index-t_dd17v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_GET { sy-subrc }| ).
    ENDIF.

    CLEAR: ls_extension_index-dd12v-as4user,
           ls_extension_index-dd12v-as4date,
           ls_extension_index-dd12v-as4time.

    io_xml->add( iv_name = 'XINX'
                 ig_data = ls_extension_index ).

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.

ENDCLASS.
