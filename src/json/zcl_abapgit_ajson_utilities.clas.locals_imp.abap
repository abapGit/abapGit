**********************************************************************
* ITERATOR
**********************************************************************

CLASS lcl_node_iterator DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_iterator.
    METHODS constructor
      IMPORTING
        ii_json      TYPE REF TO zif_abapgit_ajson
        iv_path      TYPE string
        iv_node_type TYPE zif_abapgit_ajson_types=>ty_node_type
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.
    DATA mi_json TYPE REF TO zif_abapgit_ajson.
    DATA mv_node_type TYPE zif_abapgit_ajson_types=>ty_node_type.
    DATA mv_base_path TYPE string.
    DATA mr_cursor TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    DATA mv_tabix TYPE i.
    DATA mv_has_next TYPE abap_bool.
    METHODS find_first_node.

ENDCLASS.

CLASS lcl_node_iterator IMPLEMENTATION.

  METHOD constructor.

    IF NOT ( iv_node_type = zif_abapgit_ajson_types=>node_type-array OR iv_node_type = zif_abapgit_ajson_types=>node_type-object ).
      zcx_abapgit_ajson_error=>raise( |Iterator can iterate arrays or objects only ("{ iv_node_type }" passed)| ).
    ENDIF.

    mv_base_path = zcl_abapgit_ajson=>normalize_path( iv_path ).
    mv_node_type = iv_node_type.
    mi_json      = ii_json.

    DATA lv_node_type LIKE mv_node_type.
    lv_node_type = ii_json->get_node_type( mv_base_path ).

    IF lv_node_type IS INITIAL.
      zcx_abapgit_ajson_error=>raise( |Path not found: { iv_path }| ).
    ELSEIF mv_node_type = zif_abapgit_ajson_types=>node_type-array AND lv_node_type <> mv_node_type.
      zcx_abapgit_ajson_error=>raise( |Array expected at: { iv_path }| ).
    ELSEIF mv_node_type = zif_abapgit_ajson_types=>node_type-object AND lv_node_type <> mv_node_type.
      zcx_abapgit_ajson_error=>raise( |Object expected at: { iv_path }| ).
    ENDIF.

    find_first_node( ).

  ENDMETHOD.

  METHOD find_first_node.

    CASE mv_node_type.
      WHEN zif_abapgit_ajson_types=>node_type-array.
        " path + array index key
        LOOP AT mi_json->mt_json_tree REFERENCE INTO mr_cursor USING KEY array_index WHERE path = mv_base_path.
          mv_has_next = abap_true.
          mv_tabix    = sy-tabix.
          EXIT. " first found
        ENDLOOP.
      WHEN zif_abapgit_ajson_types=>node_type-object.
        " regular path + name key
        LOOP AT mi_json->mt_json_tree REFERENCE INTO mr_cursor WHERE path = mv_base_path.
          mv_has_next = abap_true.
          mv_tabix    = sy-tabix.
          EXIT. " first found
        ENDLOOP.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_ajson_iterator~has_next.
    rv_yes = mv_has_next.
  ENDMETHOD.

  METHOD zif_abapgit_ajson_iterator~next.

    IF mv_has_next = abap_false.
      RETURN.
    ENDIF.

    ri_item = mi_json->slice( |{ mr_cursor->path }{ mr_cursor->name }| ).
    " TODO: improve performance, see comment in slice, maybe reuse read only reference to node_tree

    mv_tabix = mv_tabix + 1.
    CASE mv_node_type.
      WHEN zif_abapgit_ajson_types=>node_type-array.
        " path + array index key
        READ TABLE mi_json->mt_json_tree
          INDEX mv_tabix USING KEY array_index
          REFERENCE INTO mr_cursor.
      WHEN zif_abapgit_ajson_types=>node_type-object.
        " regular path + name key
        READ TABLE mi_json->mt_json_tree
          INDEX mv_tabix
          REFERENCE INTO mr_cursor.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.
    mv_has_next = boolc( sy-subrc = 0 AND mr_cursor->path = mv_base_path ).

  ENDMETHOD.

ENDCLASS.
