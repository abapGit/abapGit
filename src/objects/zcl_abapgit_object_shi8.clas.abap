CLASS zcl_abapgit_object_shi8 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PRIVATE SECTION.
    DATA: mv_assignment_id  TYPE hier_sfw_id.

ENDCLASS.

CLASS zcl_abapgit_object_shi8 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_assignment_id = ms_item-obj_name.

  ENDMETHOD.                    "constructor

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "ZIF_ABAPGIT_OBJECT~has_changed_since

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "ZIF_ABAPGIT_OBJECT~get_metadata

  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |TODO: Jump SHI8| ).
  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_ID_EXISTS'
      EXPORTING
        assignment_id = mv_assignment_id
      IMPORTING
        exists        = rv_bool.

  ENDMETHOD.                    "ZIF_ABAPGIT_OBJECT~exists

  METHOD zif_abapgit_object~delete.

    DATA: lv_deleted TYPE xfeld,
          ls_message TYPE hier_mess.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_DELETE'
      EXPORTING
        assignment_id = mv_assignment_id
      IMPORTING
        id_deleted    = lv_deleted
        message       = ls_message.

    IF lv_deleted = abap_false.
      zcx_abapgit_exception=>raise( |{ ls_message-msgtxt }| ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~serialize.

    DATA: lt_assignments     TYPE STANDARD TABLE OF hier_sfw_assignment_id,
          ls_assignment      LIKE LINE OF lt_assignments,
          lt_assignment_data TYPE STANDARD TABLE OF ttree_sfw_nodes,
          ls_assignment_data LIKE LINE OF lt_assignment_data.

    ls_assignment-sfw_ass_id = mv_assignment_id.
    INSERT ls_assignment INTO TABLE lt_assignments.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_READ'
      TABLES
        it_assignments     = lt_assignments
        et_assignment_data = lt_assignment_data.

    READ TABLE lt_assignment_data INTO ls_assignment_data
                                  INDEX 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error serializing { ms_item-obj_type } { ms_item-obj_name  }| ).
    ENDIF.

    io_xml->add( iv_name = 'SHI8'
                 ig_data = ls_assignment_data ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: ls_assignment_data TYPE ttree_sfw_nodes,
          ls_node_data       TYPE hier_iface,
          lv_saved           TYPE xfeld,
          ls_message         TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'SHI8'
      CHANGING
        cg_data = ls_assignment_data ).

    ls_node_data-tree_id = ls_assignment_data-tree_id.
    ls_node_data-node_id = ls_assignment_data-node_id.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_SAVE'
      EXPORTING
        assignment_id = ls_assignment_data-sfw_ass_id
        switch_id     = ls_assignment_data-switch_id
        reaction      = ls_assignment_data-reaction
        node_data     = ls_node_data
      IMPORTING
        data_saved    = lv_saved
        message       = ls_message.

    IF lv_saved = abap_false.
      zcx_abapgit_exception=>raise( |{ ls_message-msgtxt }| ).
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_shi8 IMPLEMENTATION
