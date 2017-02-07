*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SHI3
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_shi3 DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_object_shi3 DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

    METHODS constructor
      IMPORTING
        is_item     TYPE ty_item
        iv_language TYPE spras.


  PRIVATE SECTION.

    TYPES: BEGIN OF ty_id_map,
             old TYPE ttree-id,
             new TYPE ttree-id,
           END OF ty_id_map.
    TYPES  tt_id_map TYPE STANDARD TABLE OF ty_id_map.
    TYPES  ts_id_map TYPE SORTED TABLE OF ty_id_map WITH UNIQUE KEY old.

    DATA: mv_tree_id TYPE ttree-id,
          mt_map     TYPE ts_id_map. " SORTED !

    METHODS jump_se43
      RAISING lcx_exception.

    METHODS strip_stamps
      CHANGING cs_head  TYPE ttree
               ct_nodes TYPE hier_iface_t.

    METHODS regenerate_ids
      CHANGING ct_nodes TYPE hier_iface_t
               ct_refs  TYPE hier_ref_t
               ct_texts TYPE hier_texts_t
      RAISING  lcx_exception.

    METHODS replace_id
      IMPORTING iv_id            TYPE clike
      RETURNING VALUE(rv_new_id) TYPE ttree-id
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_shi3 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_shi3 IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_object_shi3 IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD constructor.
    super->constructor( is_item = is_item iv_language = iv_language ).
    mv_tree_id = ms_item-obj_name.
  ENDMETHOD.                    "constructor

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD jump_se43.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLBMEN'.
    <ls_bdcdata>-dynpro   = '0200'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BMENUNAME-ID'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE43'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from ABAP4_CALL_TRANSACTION, SHI3' ).
    ENDIF.

  ENDMETHOD.                    "jump_se43

  METHOD lif_object~jump.
    jump_se43( ).
  ENDMETHOD.                    "jump

  METHOD lif_object~exists.

    DATA: ls_msg    TYPE hier_mess,
          ls_header TYPE ttree,
          ls_tadir  TYPE tadir.

    CALL FUNCTION 'STREE_STRUCTURE_EXIST'
      EXPORTING
        structure_id         = mv_tree_id
        do_not_read_devclass = ''
      IMPORTING
        message              = ls_msg
        structure_header     = ls_header
        structure_tadir      = ls_tadir.

    rv_bool = boolc( ls_header-id IS NOT INITIAL ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~delete.

    CALL FUNCTION 'BMENU_DELETE_TREE'
      EXPORTING
        tree_id            = mv_tree_id
      EXCEPTIONS
        trees_do_not_exist = 1
        no_authority       = 2
        canceled           = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from BMENU_DELETE_TREE, SHI3' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: ls_msg    TYPE hier_mess,
          ls_head   TYPE ttree,
          lt_titles TYPE TABLE OF ttreet,
          lt_nodes  TYPE TABLE OF hier_iface,
          lt_texts  TYPE TABLE OF hier_texts,
          lt_refs   TYPE TABLE OF hier_ref.


    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        message          = ls_msg
        structure_header = ls_head
      TABLES
        description      = lt_titles.

    CALL FUNCTION 'STREE_HIERARCHY_READ'
      EXPORTING
        structure_id       = mv_tree_id
        read_also_texts    = 'X'
        all_languages      = 'X'
      IMPORTING
        message            = ls_msg
      TABLES
        list_of_nodes      = lt_nodes
        list_of_references = lt_refs
        list_of_texts      = lt_texts.

    strip_stamps( CHANGING cs_head  = ls_head
                           ct_nodes = lt_nodes ).

    io_xml->add( iv_name = 'TREE_HEAD'
                 ig_data = ls_head ).
    io_xml->add( iv_name = 'TREE_TITLES'
                 ig_data = lt_titles ).
    io_xml->add( iv_name = 'TREE_NODES'
                 ig_data = lt_nodes ).
    io_xml->add( iv_name = 'TREE_REFS'
                 ig_data = lt_refs ).
    io_xml->add( iv_name = 'TREE_TEXTS'
                 ig_data = lt_texts ).

  ENDMETHOD.                    "serialize

  METHOD strip_stamps.

    FIELD-SYMBOLS <ls_node> LIKE LINE OF ct_nodes.

    CLEAR: cs_head-luser, cs_head-ldate, cs_head-ltime.
    CLEAR: cs_head-fuser, cs_head-fdate, cs_head-ftime.
    CLEAR: cs_head-responsibl.

    LOOP AT ct_nodes ASSIGNING <ls_node>.
      CLEAR: <ls_node>-luser, <ls_node>-ldate, <ls_node>-ltime.
      CLEAR: <ls_node>-fuser, <ls_node>-fdate, <ls_node>-ftime.
    ENDLOOP.

  ENDMETHOD.                    "strip_stamps

  METHOD regenerate_ids.

    DATA: ls_uid TYPE sys_uid,
          lt_map TYPE tt_id_map.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF ct_nodes,
                   <ls_ref>  LIKE LINE OF ct_refs,
                   <ls_text> LIKE LINE OF ct_texts,
                   <ls_map>  LIKE LINE OF mt_map.

    "Build map
    LOOP AT ct_nodes ASSIGNING <ls_node>.
      APPEND INITIAL LINE TO lt_map ASSIGNING <ls_map>.
      IF <ls_node>-parent_id IS INITIAL.
        <ls_map>-old = <ls_node>-node_id.
        <ls_map>-new = <ls_node>-node_id. "Root node
      ELSE.
        CALL FUNCTION 'STREE_GET_UNIQUE_ID'
          IMPORTING
            unique_id = ls_uid.

        <ls_map>-old = <ls_node>-node_id.
        <ls_map>-new = ls_uid-id.
      ENDIF.
      <ls_node>-node_id = <ls_map>-new. "Replace id
    ENDLOOP.

    mt_map = lt_map. "Sort

    LOOP AT ct_nodes ASSIGNING <ls_node>.
      <ls_node>-parent_id  = replace_id( <ls_node>-parent_id ).
      <ls_node>-brother_id = replace_id( <ls_node>-brother_id ).
    ENDLOOP.

    LOOP AT ct_refs ASSIGNING <ls_ref>.
      <ls_ref>-node_id = replace_id( <ls_ref>-node_id ).
    ENDLOOP.

    LOOP AT ct_texts ASSIGNING <ls_text>.
      <ls_text>-node_id = replace_id( <ls_text>-node_id ).
    ENDLOOP.

  ENDMETHOD.                    "regenerate_ids

  METHOD replace_id.

    DATA ls_map LIKE LINE OF mt_map.

    IF iv_id IS INITIAL.
      RETURN. "No substitution for empty values
    ENDIF.

    READ TABLE mt_map WITH TABLE KEY old = iv_id INTO ls_map.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Cannot replace id, SHI3' ).
    ENDIF.

    rv_new_id = ls_map-new.

  ENDMETHOD.                    "replace_id

  METHOD lif_object~deserialize.

    DATA: ls_msg    TYPE hier_mess,
          ls_head   TYPE ttree,
          lt_titles TYPE TABLE OF ttreet,
          lt_nodes  TYPE TABLE OF hier_iface,
          lt_texts  TYPE TABLE OF hier_texts,
          lt_refs   TYPE TABLE OF hier_ref.

    io_xml->read( EXPORTING iv_name = 'TREE_HEAD'
                  CHANGING  cg_data = ls_head ).
    io_xml->read( EXPORTING iv_name = 'TREE_TITLES'
                  CHANGING  cg_data = lt_titles ).
    io_xml->read( EXPORTING iv_name = 'TREE_NODES'
                  CHANGING  cg_data = lt_nodes ).
    io_xml->read( EXPORTING iv_name = 'TREE_REFS'
                  CHANGING  cg_data = lt_refs ).
    io_xml->read( EXPORTING iv_name = 'TREE_TEXTS'
                  CHANGING  cg_data = lt_texts ).

    regenerate_ids( CHANGING ct_nodes = lt_nodes
                             ct_refs  = lt_refs
                             ct_texts = lt_texts ).

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_SAVE'
      EXPORTING
        structure_id             = mv_tree_id
        structure_type           = 'BMENU'
        structure_description    = space
        structure_masterlanguage = mv_language
        structure_responsible    = sy-uname
        development_class        = iv_package
      IMPORTING
        message                  = ls_msg
      TABLES
        list_of_nodes            = lt_nodes
        list_of_references       = lt_refs
        list_of_texts            = lt_texts
        structure_descriptions   = lt_titles
      EXCEPTIONS
        no_nodes_given           = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from STREE_HIERARCHY_SAVE, SHI3' ).
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_shi3 IMPLEMENTATION
