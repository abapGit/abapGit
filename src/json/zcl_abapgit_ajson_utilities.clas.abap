CLASS zcl_abapgit_ajson_utilities DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS diff
      IMPORTING
        !iv_json_a TYPE string OPTIONAL
        !iv_json_b TYPE string OPTIONAL
        !io_json_a TYPE REF TO zif_abapgit_ajson OPTIONAL
        !io_json_b TYPE REF TO zif_abapgit_ajson OPTIONAL
      EXPORTING
        !eo_insert TYPE REF TO zif_abapgit_ajson
        !eo_delete TYPE REF TO zif_abapgit_ajson
        !eo_change TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .
    METHODS sort
      IMPORTING
        !iv_json         TYPE string OPTIONAL
        !io_json         TYPE REF TO zif_abapgit_ajson OPTIONAL
      RETURNING
        VALUE(rv_sorted) TYPE string
      RAISING
        zcx_abapgit_ajson_error .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_json_a TYPE REF TO zif_abapgit_ajson .
    DATA mo_json_b TYPE REF TO zif_abapgit_ajson .
    DATA mo_insert TYPE REF TO zif_abapgit_ajson_writer .
    DATA mo_delete TYPE REF TO zif_abapgit_ajson_writer .
    DATA mo_change TYPE REF TO zif_abapgit_ajson_writer .

    METHODS diff_a_b
      IMPORTING
        !iv_path TYPE string
      RAISING
        zcx_abapgit_ajson_error .
    METHODS diff_b_a
      IMPORTING
        !iv_path TYPE string
      RAISING
        zcx_abapgit_ajson_error .
    METHODS delete_empty_nodes
      IMPORTING
        !io_json TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .
ENDCLASS.



CLASS zcl_abapgit_ajson_utilities IMPLEMENTATION.


  METHOD delete_empty_nodes.

    DATA ls_json_tree LIKE LINE OF io_json->mt_json_tree.
    DATA lv_subrc TYPE sy-subrc.

    DO.
      LOOP AT io_json->mt_json_tree INTO ls_json_tree
        WHERE type = 'array' AND children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      ENDLOOP.
      lv_subrc = sy-subrc.

      LOOP AT io_json->mt_json_tree INTO ls_json_tree
        WHERE type = 'object' AND children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      ENDLOOP.
      IF lv_subrc = 4 AND sy-subrc = 4.
        EXIT. " nothing else to delete
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD diff.

    IF boolc( iv_json_a IS SUPPLIED ) = boolc( io_json_a IS SUPPLIED ).
      zcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.
    IF boolc( iv_json_b IS SUPPLIED ) = boolc( io_json_b IS SUPPLIED ).
      zcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.

    IF iv_json_a IS SUPPLIED.
      mo_json_a = zcl_abapgit_ajson=>parse( iv_json_a ).
    ELSEIF io_json_a IS BOUND.
      mo_json_a = io_json_a.
    ELSE.
      zcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

    IF iv_json_b IS SUPPLIED.
      mo_json_b = zcl_abapgit_ajson=>parse( iv_json_b ).
    ELSEIF io_json_a IS BOUND.
      mo_json_b = io_json_b.
    ELSE.
      zcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

    mo_insert = zcl_abapgit_ajson=>create_empty( ).
    mo_delete = zcl_abapgit_ajson=>create_empty( ).
    mo_change = zcl_abapgit_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes( eo_insert ).
    delete_empty_nodes( eo_delete ).
    delete_empty_nodes( eo_change ).

  ENDMETHOD.


  METHOD diff_a_b.

    DATA:
      lv_path_a TYPE string,
      lv_path_b TYPE string.

    FIELD-SYMBOLS:
      <node_a> LIKE LINE OF mo_json_a->mt_json_tree,
      <node_b> LIKE LINE OF mo_json_a->mt_json_tree.

    LOOP AT mo_json_a->mt_json_tree ASSIGNING <node_a> WHERE path = iv_path.
      lv_path_a = <node_a>-path && <node_a>-name && '/'.

      READ TABLE mo_json_b->mt_json_tree ASSIGNING <node_b>
        WITH TABLE KEY path = <node_a>-path name = <node_a>-name.
      IF sy-subrc = 0.
        lv_path_b = <node_b>-path && <node_b>-name && '/'.

        IF <node_a>-type = <node_b>-type.
          CASE <node_a>-type.
            WHEN 'array'.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN 'object'.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              IF <node_a>-value <> <node_b>-value.
                " save as changed value
                mo_change->set(
                  iv_path      = lv_path_b
                  iv_val       = <node_b>-value
                  iv_node_type = <node_b>-type ).
              ENDIF.
          ENDCASE.
        ELSE.
          " save changed type as delete + insert
          CASE <node_a>-type.
            WHEN 'array'.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN 'object'.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          ENDCASE.
          CASE <node_b>-type.
            WHEN 'array'.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            WHEN 'object'.
              diff_b_a( lv_path_b ).
            WHEN OTHERS.
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
          ENDCASE.
        ENDIF.
      ELSE.
        " save as delete
        CASE <node_a>-type.
          WHEN 'array'.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          WHEN 'object'.
            diff_a_b( lv_path_a ).
          WHEN OTHERS.
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <node_a>-value
              iv_node_type = <node_a>-type ).
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD diff_b_a.

    DATA lv_path TYPE string.

    FIELD-SYMBOLS:
      <node_a> LIKE LINE OF mo_json_b->mt_json_tree,
      <node_b> LIKE LINE OF mo_json_b->mt_json_tree.

    LOOP AT mo_json_b->mt_json_tree ASSIGNING <node_b> WHERE path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      CASE <node_b>-type.
        WHEN 'array'.
          mo_insert->touch_array( lv_path ).
          diff_b_a( lv_path ).
        WHEN 'object'.
          diff_b_a( lv_path ).
        WHEN OTHERS.
          READ TABLE mo_json_a->mt_json_tree ASSIGNING <node_a>
            WITH TABLE KEY path = <node_b>-path name = <node_b>-name.
          IF sy-subrc <> 0.
            " save as insert
            mo_insert->set(
              iv_path      = lv_path
              iv_val       = <node_b>-value
              iv_node_type = <node_b>-type ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD sort.

    DATA lo_json TYPE REF TO zif_abapgit_ajson.

    IF boolc( iv_json IS SUPPLIED ) = boolc( io_json IS SUPPLIED ).
      zcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.

    IF iv_json IS SUPPLIED.
      lo_json = zcl_abapgit_ajson=>parse( iv_json ).
    ELSEIF io_json IS BOUND.
      lo_json = io_json.
    ELSE.
      zcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  ENDMETHOD.
ENDCLASS.
