CLASS zcl_abapgit_object_shi3 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.

    METHODS has_authorization
      IMPORTING
        !iv_devclass     TYPE devclass
        !iv_structure_id TYPE hier_guid
        !iv_activity     TYPE activ_auth
      RAISING
        zcx_abapgit_exception .
    METHODS is_used
      IMPORTING
        !iv_structure_id TYPE hier_guid
      RAISING
        zcx_abapgit_exception .
    METHODS delete_tree_structure
      IMPORTING
        !iv_structure_id TYPE hier_guid .
  PRIVATE SECTION.

    DATA mv_tree_id TYPE ttree-id.

    METHODS insert_transport
      IMPORTING
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception.
    METHODS jump_se43
      RAISING
        zcx_abapgit_exception.
    METHODS jump_sbach04
      RAISING
        zcx_abapgit_exception.
    METHODS clear_fields
      CHANGING
        !cs_head  TYPE ttree
        !ct_nodes TYPE hier_iface_t.
ENDCLASS.



CLASS zcl_abapgit_object_shi3 IMPLEMENTATION.


  METHOD clear_fields.

    FIELD-SYMBOLS <ls_node> LIKE LINE OF ct_nodes.

    CLEAR: cs_head-luser, cs_head-ldate, cs_head-ltime.
    CLEAR: cs_head-fuser, cs_head-fdate, cs_head-ftime.
    CLEAR: cs_head-frelease, cs_head-lrelease.
    CLEAR: cs_head-responsibl.

    LOOP AT ct_nodes ASSIGNING <ls_node>.
      CLEAR: <ls_node>-luser, <ls_node>-ldate, <ls_node>-ltime.
      CLEAR: <ls_node>-fuser, <ls_node>-fdate, <ls_node>-ftime.
      CLEAR: <ls_node>-frelease, <ls_node>-lrelease.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    mv_tree_id = ms_item-obj_name.
  ENDMETHOD.


  METHOD delete_tree_structure.
    CALL FUNCTION 'STREE_EXTERNAL_DELETE'
      EXPORTING
        structure_id          = iv_structure_id
        no_confirmation_popup = abap_true.
  ENDMETHOD.


  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
      ID 'DEVCLASS'  FIELD iv_devclass
      ID 'OBJTYPE'   FIELD 'MENU'
      ID 'OBJNAME'   FIELD iv_structure_id
      ID 'P_GROUP'   DUMMY
      ID 'ACTVT'     FIELD iv_activity.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = 'S#'
                                         iv_msgno = '203' ).
    ENDIF.
  ENDMETHOD.


  METHOD insert_transport.

    DATA:
      ls_msg     TYPE hier_mess,
      ls_object  TYPE e071,
      lt_objects TYPE TABLE OF e071,
      lt_keys    TYPE TABLE OF e071k,
      ls_ko200   TYPE ko200,
      lt_ko200   TYPE TABLE OF ko200.

    " This function shows a popup so get objects and keys and insert
    " them into transport below
    CALL FUNCTION 'STREE_INSERT_ALL_IN_TRANSPORT'
      EXPORTING
        structure_id               = mv_tree_id
        iv_return_objects_and_keys = abap_true
      IMPORTING
        message                    = ls_msg
      TABLES
        et_objects                 = lt_objects
        et_keys                    = lt_keys.
    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects INTO ls_object.
      MOVE-CORRESPONDING ls_object TO ls_ko200.
      INSERT ls_ko200 INTO TABLE lt_ko200.
    ENDLOOP.

    CALL FUNCTION 'TR_RECORD_OBJ_CHANGE_TO_REQ'
      EXPORTING
        iv_request = iv_transport
        it_objects = lt_ko200
        it_keys    = lt_keys
      EXCEPTIONS
        cancel     = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_used.

    DATA: lt_used_in_structures TYPE STANDARD TABLE OF ttree WITH DEFAULT KEY.

    CALL FUNCTION 'STREE_GET_STRUCTURE_USAGE'
      EXPORTING
        structure_id       = iv_structure_id
      TABLES
        used_in_structures = lt_used_in_structures.

    IF lt_used_in_structures IS NOT INITIAL.
      zcx_abapgit_exception=>raise( |IMG structure ID { iv_structure_id } is still used| ).
    ENDIF.

  ENDMETHOD.


  METHOD jump_sbach04.
    DATA: ls_message      TYPE hier_mess,
          lv_structure_id TYPE hier_treeg.

    lv_structure_id = ms_item-obj_name.

    CALL FUNCTION 'STREE_EXTERNAL_EDIT'
      EXPORTING
        structure_id   = lv_structure_id
        language       = mv_language
        edit_structure = abap_false
        no_commit_work = abap_false
        activity       = 'D'
      IMPORTING
        message        = ls_message.
    IF ls_message IS NOT INITIAL.
      zcx_abapgit_exception=>raise_t100(
        iv_msgid = ls_message-msgid
        iv_msgno = ls_message-msgno
        iv_msgv1 = ls_message-msgv1
        iv_msgv2 = ls_message-msgv2
        iv_msgv3 = ls_message-msgv3
        iv_msgv4 = ls_message-msgv4 ).
    ENDIF.
  ENDMETHOD.


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

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE43'
      it_bdcdata = lt_bdcdata ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    rv_user = ls_head-luser.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CONSTANTS lc_activity_delete_06 TYPE activ_auth VALUE '06'.

    TRY.
        IF zif_abapgit_object~exists( ) = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( iv_structure_id = mv_tree_id
                       iv_devclass     = ms_item-devclass
                       iv_activity     = lc_activity_delete_06 ).

    is_used( mv_tree_id ).

    delete_tree_structure( mv_tree_id ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_msg    TYPE hier_mess,
          ls_head   TYPE ttree,
          ls_ttree  TYPE ttree,
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

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = io_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = io_xml->i18n_params( )-main_language
      CHANGING
        ct_tab = lt_titles ).
    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = io_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = io_xml->i18n_params( )-main_language
      CHANGING
        ct_tab = lt_texts ).

    IF zif_abapgit_object~exists( ) = abap_true.
      delete_tree_structure( mv_tree_id ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_SAVE'
      EXPORTING
        structure_id             = mv_tree_id
        structure_type           = ls_head-type
        structure_description    = space
        structure_masterlanguage = mv_language
        structure_responsible    = sy-uname
        structure_buffermode     = ls_head-buffermode
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
      zcx_abapgit_exception=>raise_t100( ).
    ELSEIF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Set buffer mode for menus (see function BMENU_CREATE_TREE)
    SELECT SINGLE * FROM ttree INTO ls_ttree
      WHERE type = 'BMENU' AND id = mv_tree_id.
    IF sy-subrc = 0.
      ls_ttree-buffermode = ls_head-buffermode.
      ls_ttree-buffervar  = ls_head-buffervar.
      MODIFY ttree FROM ls_ttree.
    ENDIF.

    IF io_xml->i18n_params( )-translation_languages IS NOT INITIAL AND io_xml->i18n_params( )-use_lxe = abap_true.
      deserialize_lxe_texts( io_xml ).
    ENDIF.

    IF zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.
      " Add necessary SHI6, SHI7, and TABU entries to transport (SAP Note 455542)
      insert_transport( iv_transport ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_msg    TYPE hier_mess,
          ls_header TYPE ttree,
          ls_tadir  TYPE tadir.

    " Ignore buffer and get state from DB
    CALL FUNCTION 'STREE_STRUCTURE_EXIST'
      EXPORTING
        structure_id         = mv_tree_id
        read_from_database   = abap_true
        do_not_read_devclass = abap_false
      IMPORTING
        message              = ls_msg
        structure_header     = ls_header
        structure_tadir      = ls_tadir.

    rv_bool = boolc( ls_header-id IS NOT INITIAL ).

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

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    CASE ls_head-type.
      WHEN 'BMENU'.
        jump_se43( ).
        rv_exit = abap_true.
      WHEN 'GHIER'.
        jump_sbach04( ).
        rv_exit = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_msg           TYPE hier_mess,
          ls_head          TYPE ttree,
          lt_titles        TYPE TABLE OF ttreet,
          lt_nodes         TYPE TABLE OF hier_iface,
          lt_texts         TYPE TABLE OF hier_texts,
          lt_refs          TYPE TABLE OF hier_ref,
          lv_all_languages TYPE abap_bool.


    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        message          = ls_msg
        structure_header = ls_head
      TABLES
        description      = lt_titles.

    IF io_xml->i18n_params( )-main_language_only = abap_true
      OR io_xml->i18n_params( )-translation_languages IS NOT INITIAL AND io_xml->i18n_params( )-use_lxe = abap_true.
      lv_all_languages = abap_false.
      DELETE lt_titles WHERE spras <> mv_language.
    ELSE.
      lv_all_languages = abap_true.
      zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
        EXPORTING
          it_iso_filter = io_xml->i18n_params( )-translation_languages
          iv_lang_field_name = 'SPRAS'
          iv_keep_master_lang = mv_language
        CHANGING
          ct_tab = lt_titles ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_READ'
      EXPORTING
        structure_id       = mv_tree_id
        read_also_texts    = abap_true
        all_languages      = lv_all_languages
        language           = mv_language
      IMPORTING
        message            = ls_msg
      TABLES
        list_of_nodes      = lt_nodes
        list_of_references = lt_refs
        list_of_texts      = lt_texts.

    clear_fields( CHANGING cs_head  = ls_head
                           ct_nodes = lt_nodes ).

    SORT lt_titles BY id.
    DELETE ADJACENT DUPLICATES FROM lt_titles COMPARING spras id.

    SORT lt_texts BY spras.
    DELETE ADJACENT DUPLICATES FROM lt_texts COMPARING spras node_id.

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = io_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = mv_language
      CHANGING
        ct_tab = lt_texts ).

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

    IF io_xml->i18n_params( )-translation_languages IS NOT INITIAL AND io_xml->i18n_params( )-use_lxe = abap_true.
      serialize_lxe_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
