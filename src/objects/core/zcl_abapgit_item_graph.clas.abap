CLASS zcl_abapgit_item_graph DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_items TYPE zif_abapgit_definitions=>ty_items_tt .
    METHODS add_edge
      IMPORTING
        !is_from TYPE zif_abapgit_definitions=>ty_item
        !is_to   TYPE zif_abapgit_definitions=>ty_item .
    METHODS has_vertices
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS get_next
      IMPORTING
        !ii_log        TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rs_item) TYPE zif_abapgit_definitions=>ty_item .
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_edge,
             from TYPE zif_abapgit_definitions=>ty_item,
             to   TYPE zif_abapgit_definitions=>ty_item,
           END OF ty_edge.

    DATA mt_vertices TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_item WITH DEFAULT KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH DEFAULT KEY.
    DATA mv_warning TYPE abap_bool.

    METHODS remove_vertex IMPORTING iv_index TYPE i.

ENDCLASS.



CLASS ZCL_ABAPGIT_ITEM_GRAPH IMPLEMENTATION.


  METHOD add_edge.
    DATA ls_edge LIKE LINE OF mt_edges.
    ASSERT is_from IS NOT INITIAL.
    ASSERT is_to IS NOT INITIAL.
    ls_edge-from = is_from.
    ls_edge-to   = is_to.
    APPEND ls_edge TO mt_edges.
  ENDMETHOD.


  METHOD constructor.
    INSERT LINES OF it_items INTO TABLE mt_vertices.
  ENDMETHOD.


  METHOD get_next.
* find a vertex with no inbound edges, if it does not exist pick anything

    DATA ls_vertex LIKE LINE OF mt_vertices.
    DATA lv_index  TYPE i.

    LOOP AT mt_vertices INTO ls_vertex.
      lv_index = sy-tabix.
      READ TABLE mt_edges WITH KEY
        to-obj_type = ls_vertex-obj_type
        to-obj_name = ls_vertex-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        remove_vertex( lv_index ).
        rs_item = ls_vertex.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mv_warning = abap_false.
* only issue the warning once per graph
      ii_log->add_warning( |Cycle detected in item graph| ).
      mv_warning = abap_true.
    ENDIF.

    READ TABLE mt_vertices INTO rs_item INDEX 1.
    ASSERT sy-subrc = 0.
    remove_vertex( 1 ).

  ENDMETHOD.


  METHOD has_vertices.
    rv_bool = boolc( lines( mt_vertices ) > 0 ).
  ENDMETHOD.


  METHOD remove_vertex.
    DATA ls_vertex LIKE LINE OF mt_vertices.

    READ TABLE mt_vertices INDEX iv_index INTO ls_vertex.
    ASSERT sy-subrc = 0.

    DELETE mt_vertices INDEX iv_index.
    DELETE mt_edges WHERE
      from-obj_type = ls_vertex-obj_type AND
      from-obj_name = ls_vertex-obj_name.
  ENDMETHOD.
ENDCLASS.
