CLASS zcl_abapgit_ajson_utilities DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson_utilities.
    METHODS diff
      IMPORTING
        !iv_json_a            TYPE string OPTIONAL
        !iv_json_b            TYPE string OPTIONAL
        !io_json_a            TYPE REF TO zif_abapgit_ajson OPTIONAL
        !io_json_b            TYPE REF TO zif_abapgit_ajson OPTIONAL
        !iv_keep_empty_arrays TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !eo_insert            TYPE REF TO zif_abapgit_ajson
        !eo_delete            TYPE REF TO zif_abapgit_ajson
        !eo_change            TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .
    METHODS merge
      IMPORTING
        !iv_json_a            TYPE string OPTIONAL
        !iv_json_b            TYPE string OPTIONAL
        !io_json_a            TYPE REF TO zif_abapgit_ajson OPTIONAL
        !io_json_b            TYPE REF TO zif_abapgit_ajson OPTIONAL
        !iv_keep_empty_arrays TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_json)        TYPE REF TO zif_abapgit_ajson
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
    METHODS is_equal
      IMPORTING
        !iv_json_a            TYPE string OPTIONAL
        !iv_json_b            TYPE string OPTIONAL
        !ii_json_a            TYPE REF TO zif_abapgit_ajson OPTIONAL
        !ii_json_b            TYPE REF TO zif_abapgit_ajson OPTIONAL
      RETURNING
        VALUE(rv_yes) TYPE abap_bool
      RAISING
        zcx_abapgit_ajson_error .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_json_a TYPE REF TO zif_abapgit_ajson .
    DATA mo_json_b TYPE REF TO zif_abapgit_ajson .
    DATA mo_insert TYPE REF TO zif_abapgit_ajson .
    DATA mo_delete TYPE REF TO zif_abapgit_ajson .
    DATA mo_change TYPE REF TO zif_abapgit_ajson .

    METHODS normalize_input
      IMPORTING
        !iv_json       TYPE string OPTIONAL
        !io_json       TYPE REF TO zif_abapgit_ajson OPTIONAL
      RETURNING
        VALUE(ro_json) TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .
    METHODS diff_a_b
      IMPORTING
        !iv_path TYPE string
      RAISING
        zcx_abapgit_ajson_error .
    METHODS diff_b_a
      IMPORTING
        !iv_path  TYPE string
        !iv_array TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_ajson_error .
    METHODS delete_empty_nodes
      IMPORTING
        !io_json              TYPE REF TO zif_abapgit_ajson
        !iv_keep_empty_arrays TYPE abap_bool
      RAISING
        zcx_abapgit_ajson_error .
ENDCLASS.



CLASS zcl_abapgit_ajson_utilities IMPLEMENTATION.


  METHOD delete_empty_nodes.

    DATA ls_json_tree LIKE LINE OF io_json->mt_json_tree.
    DATA lv_done TYPE abap_bool.

    DO.
      lv_done = abap_true.

      IF iv_keep_empty_arrays = abap_false.
        LOOP AT io_json->mt_json_tree INTO ls_json_tree
          WHERE type = zif_abapgit_ajson_types=>node_type-array AND children = 0.

          io_json->delete( ls_json_tree-path && ls_json_tree-name ).

        ENDLOOP.
        IF sy-subrc = 0.
          lv_done = abap_false.
        ENDIF.
      ENDIF.

      LOOP AT io_json->mt_json_tree INTO ls_json_tree
        WHERE type = zif_abapgit_ajson_types=>node_type-object AND children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      ENDLOOP.
      IF sy-subrc = 0.
        lv_done = abap_false.
      ENDIF.

      IF lv_done = abap_true.
        EXIT. " nothing else to delete
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD diff.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    mo_insert = zcl_abapgit_ajson=>create_empty( ).
    mo_delete = zcl_abapgit_ajson=>create_empty( ).
    mo_change = zcl_abapgit_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes(
      io_json              = eo_insert
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_delete
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_change
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

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
            WHEN zif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN zif_abapgit_ajson_types=>node_type-object.
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
            WHEN zif_abapgit_ajson_types=>node_type-array.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN zif_abapgit_ajson_types=>node_type-object.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          ENDCASE.
          CASE <node_b>-type.
            WHEN zif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            WHEN zif_abapgit_ajson_types=>node_type-object.
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
          WHEN zif_abapgit_ajson_types=>node_type-array.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          WHEN zif_abapgit_ajson_types=>node_type-object.
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

    FIELD-SYMBOLS <node_b> LIKE LINE OF mo_json_b->mt_json_tree.

    LOOP AT mo_json_b->mt_json_tree ASSIGNING <node_b> WHERE path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      CASE <node_b>-type.
        WHEN zif_abapgit_ajson_types=>node_type-array.
          mo_insert->touch_array( lv_path ).
          diff_b_a(
            iv_path  = lv_path
            iv_array = abap_true ).
        WHEN zif_abapgit_ajson_types=>node_type-object.
          diff_b_a( lv_path ).
        WHEN OTHERS.
          IF iv_array = abap_false.
            READ TABLE mo_json_a->mt_json_tree TRANSPORTING NO FIELDS
              WITH TABLE KEY path = <node_b>-path name = <node_b>-name.
            IF sy-subrc <> 0.
              " save as insert
              mo_insert->set(
                iv_path      = lv_path
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
            ENDIF.
          ELSE.
            READ TABLE mo_insert->mt_json_tree TRANSPORTING NO FIELDS
              WITH KEY path = <node_b>-path value = <node_b>-value.
            IF sy-subrc <> 0.
              " save as new array value
              mo_insert->push(
                iv_path = iv_path
                iv_val  = <node_b>-value ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_equal.

    DATA li_ins TYPE REF TO zif_abapgit_ajson.
    DATA li_del TYPE REF TO zif_abapgit_ajson.
    DATA li_mod TYPE REF TO zif_abapgit_ajson.

    diff(
      EXPORTING
        iv_json_a = iv_json_a
        iv_json_b = iv_json_b
        io_json_a = ii_json_a
        io_json_b = ii_json_b
      IMPORTING
        eo_insert = li_ins
        eo_delete = li_del
        eo_change = li_mod ).

    rv_yes = boolc(
      li_ins->is_empty( ) = abap_true AND
      li_del->is_empty( ) = abap_true AND
      li_mod->is_empty( ) = abap_true ).

  ENDMETHOD.


  METHOD merge.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    " Start with first JSON...
    mo_insert = mo_json_a.

    " ...and add all nodes from second JSON
    diff_b_a( '/' ).

    ro_json ?= mo_insert.

    delete_empty_nodes(
      io_json              = ro_json
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD normalize_input.

    IF boolc( iv_json IS INITIAL ) = boolc( io_json IS INITIAL ).
      zcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.

    IF iv_json IS NOT INITIAL.
      ro_json = zcl_abapgit_ajson=>parse( iv_json ).
    ELSEIF io_json IS NOT INITIAL.
      ro_json = io_json.
    ELSE.
      zcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

  ENDMETHOD.


  METHOD sort.

    DATA lo_json TYPE REF TO zif_abapgit_ajson.

    lo_json = normalize_input(
      iv_json = iv_json
      io_json = io_json ).

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  ENDMETHOD.
ENDCLASS.
