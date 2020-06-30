CLASS zcl_abapgit_object_dsys DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS: c_typ TYPE dokhl-typ VALUE 'E',
               c_id  TYPE dokhl-id VALUE 'HY'.

    DATA: mv_doc_object  TYPE sobj_name.

    TYPES: BEGIN OF ty_data,
             doctitle TYPE dsyst-doktitle,
             head     TYPE thead,
             lines    TYPE tline_tab,
           END OF ty_data.

    METHODS deserialize_dsys
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS get_master_lang
      RETURNING
        VALUE(rv_language) TYPE spras.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_DSYS IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_prefix    TYPE namespace,
          lv_bare_name TYPE progname.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
      EXPORTING
        name_with_namespace    = ms_item-obj_name
      IMPORTING
        namespace              = lv_prefix
        name_without_namespace = lv_bare_name.

    mv_doc_object = |{ lv_bare_name+0(4) }{ lv_prefix }{ lv_bare_name+4(*) }|.

  ENDMETHOD.


  METHOD deserialize_dsys.

    DATA: ls_data      TYPE ty_data,
          ls_docu_info TYPE dokil,
          lv_version   TYPE dokvers.

    io_xml->read( EXPORTING iv_name = 'DSYS'
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_INIT'
      EXPORTING
        id     = c_id
        langu  = mv_language
        object = mv_doc_object
        typ    = c_typ
      IMPORTING
        xdokil = ls_docu_info.

    lv_version = ls_docu_info-version.

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = lv_version
      TABLES
        line    = ls_data-lines.

  ENDMETHOD.


  METHOD get_master_lang.

    DATA: lv_language TYPE spras.

    SELECT SINGLE langu FROM dokil INTO rv_language
      WHERE id = c_id
      AND object = mv_doc_object
      AND masterlang = abap_true.

    IF sy-subrc <> 0.
      rv_language = mv_language.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = zcl_abapgit_factory=>get_longtexts( )->changed_by(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    zcl_abapgit_factory=>get_longtexts( )->delete(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_metadata TYPE zif_abapgit_definitions=>ty_metadata.

    ls_metadata = io_xml->get_metadata( ).

    CASE ls_metadata-version.

      WHEN 'v1.0.0'.
        deserialize_dsys( io_xml ).

      WHEN 'v2.0.0'.
        zcl_abapgit_factory=>get_longtexts( )->deserialize(
          io_xml             = io_xml
          iv_master_language = mv_language ).

      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'unsupported DSYS version' ).

    ENDCASE.

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_count TYPE i.

    SELECT SINGLE COUNT( * ) FROM dokil INTO lv_count
           WHERE id   = c_id
           AND object = mv_doc_object.                  "#EC CI_GENBUFF

    rv_bool = boolc( lv_count > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA lv_lang TYPE sy-langu.

    lv_lang = get_master_lang( ).

    CALL FUNCTION 'DSYS_EDIT'
      EXPORTING
        dokclass         = mv_doc_object+0(4)
        dokname          = mv_doc_object+4(*)
        doklangu         = lv_lang
      EXCEPTIONS
        class_unknown    = 1
        object_not_found = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DSYS_EDIT' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    io_xml->i18n_params( abap_false ).

    zcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id
      io_xml         = io_xml ).

  ENDMETHOD.
ENDCLASS.
