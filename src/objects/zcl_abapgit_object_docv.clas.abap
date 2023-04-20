CLASS zcl_abapgit_object_docv DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_data,
        doctitle TYPE dsyst-doktitle,
        head     TYPE thead,
        lines    TYPE tline_tab,
      END OF ty_data.

    CONSTANTS c_typ TYPE dokhl-typ VALUE 'E' ##NO_TEXT.
    CONSTANTS c_version TYPE dokhl-dokversion VALUE '0001' ##NO_TEXT.
    CONSTANTS c_name TYPE string VALUE 'DOC' ##NO_TEXT.

    DATA mv_id TYPE dokhl-id.
    DATA mv_doc_object TYPE dokhl-object.

    METHODS read
      RETURNING
        VALUE(rs_data) TYPE ty_data.
ENDCLASS.



CLASS zcl_abapgit_object_docv IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_prefix    TYPE namespace,
          lv_bare_name TYPE progname.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    IF ms_item-obj_name(2) <> 'DT'. " IN, MO, UO, UP
      mv_id         = ms_item-obj_name(2).
      mv_doc_object = ms_item-obj_name+2.
    ELSE. " DT
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = ms_item-obj_name
        IMPORTING
          namespace              = lv_prefix
          name_without_namespace = lv_bare_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error determining namespace for { ms_item-obj_type } { ms_item-obj_name }| ).
      ENDIF.

      mv_id         = lv_bare_name(2).
      mv_doc_object = |{ lv_prefix }{ lv_bare_name+2 }|.
    ENDIF.

  ENDMETHOD.


  METHOD read.

    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id       = mv_id
        langu    = mv_language
        object   = mv_doc_object
        typ      = c_typ
        version  = c_version
      IMPORTING
        doktitle = rs_data-doctitle
        head     = rs_data-head
      TABLES
        line     = rs_data-lines.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = read( )-head-tdluser.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = mv_id
        langu    = mv_language
        object   = mv_doc_object
        typ      = c_typ
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_data TYPE ty_data.


    io_xml->read( EXPORTING iv_name = c_name
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = c_version
      TABLES
        line    = ls_data-lines.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT SINGLE id FROM dokil INTO mv_id
      WHERE id     = mv_id
        AND object = mv_doc_object.                     "#EC CI_GENBUFF

    rv_bool = boolc( sy-subrc = 0 ).

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
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_data   TYPE ty_data.


    ls_data = read( ).

    CLEAR: ls_data-head-tdfuser,
           ls_data-head-tdfreles,
           ls_data-head-tdfdate,
           ls_data-head-tdftime,
           ls_data-head-tdluser,
           ls_data-head-tdlreles,
           ls_data-head-tdldate,
           ls_data-head-tdltime.

    io_xml->add( iv_name = c_name
                 ig_data = ls_data ).

  ENDMETHOD.
ENDCLASS.
