CLASS zcl_abapgit_object_shi5 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_ttree_extt TYPE STANDARD TABLE OF ttree_extt
                               WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF ty_extension,
             header    TYPE ttree_ext,
             texts     TYPE ty_ttree_extt,
             sequences TYPE STANDARD TABLE OF ttrees WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_extension.

    DATA: mv_extension TYPE hier_names.

ENDCLASS.



CLASS zcl_abapgit_object_shi5 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_extension = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      ls_msg              TYPE hier_mess,
      lv_found_users      TYPE hier_yesno,
      ls_check_extensions TYPE treenamesp,
      lt_check_extensions TYPE TABLE OF treenamesp,
      lv_obj_name         TYPE ko200-obj_name.

    " STREE_EXTENSION_DELETE shows a popup so do the same here

    ls_check_extensions-extension = mv_extension.
    INSERT ls_check_extensions INTO TABLE lt_check_extensions.

    CALL FUNCTION 'STREE_CHECK_EXTENSION'
      IMPORTING
        message         = ls_msg
      TABLES
        check_extension = lt_check_extensions.

    READ TABLE lt_check_extensions INTO ls_check_extensions INDEX 1.
    IF ls_check_extensions-original = abap_false.
      zcx_abapgit_exception=>raise( 'Delete enhancement ID in your source system' ).
    ENDIF.

    lv_obj_name = mv_extension.

    CALL FUNCTION 'STREE_TRANSPORT_CHECK'
      EXPORTING
        object   = 'SHI5'
        obj_name = lv_obj_name
      IMPORTING
        message  = ls_msg.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'STREE_EXTENSION_USAGE'
      EXPORTING
        extension         = mv_extension
        no_display        = abap_true
      IMPORTING
        message           = ls_msg
        extension_is_used = lv_found_users.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_found_users = abap_true.
      zcx_abapgit_exception=>raise( 'Enhancement ID is still used' ).
    ENDIF.

    CALL FUNCTION 'STREE_TRANSPORT_INSERT'
      EXPORTING
        object   = 'SHI5'
        obj_name = lv_obj_name
      IMPORTING
        message  = ls_msg.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE FROM ttree_ext WHERE extension = mv_extension.
    DELETE FROM ttree_extt WHERE extension = mv_extension.

    IF ls_check_extensions-transport = abap_false.
      " no transportable Devclass -> delete TADIR
      tadir_delete( ).
    ENDIF.

    " reset some internal tables
    CALL FUNCTION 'STREE_RESET_FUGR_SHI5_TABLES'.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    " We cannot use STREE_EXTENSION_NAME_CREATE
    " the create logic is directly tied to the UI
    "
    " Do it like here LSHI20F01 -> SAVE_DATA

    DATA: ls_extension TYPE ty_extension.

    io_xml->read(
      EXPORTING
        iv_name = 'SHI5'
      CHANGING
        cg_data = ls_extension ).

    INSERT ttree_ext  FROM ls_extension-header.

    DELETE FROM ttrees WHERE extension = ls_extension-header-extension.
    MODIFY ttrees FROM TABLE ls_extension-sequences.

    DELETE FROM ttree_extt WHERE extension = ls_extension-header-extension.
    MODIFY ttree_extt FROM TABLE ls_extension-texts.

    corr_insert( iv_package ).

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_extension_header TYPE ttree_ext.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension_header.

    rv_bool = boolc( ls_extension_header IS NOT INITIAL ).

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
    DATA: lt_extension TYPE STANDARD TABLE OF ttree_ext.
    FIELD-SYMBOLS: <ls_extension> LIKE LINE OF lt_extension.

    INSERT INITIAL LINE INTO TABLE lt_extension ASSIGNING <ls_extension>.
    <ls_extension>-extension = mv_extension.

    CALL FUNCTION 'STREE_EXTENSION_NAME_F4'
      EXPORTING
        originals_only       = abap_true
      TABLES
        show_only_extensions = lt_extension.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_extension TYPE ty_extension.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension-header.

    SELECT * FROM ttree_extt
             INTO TABLE ls_extension-texts
             WHERE extension = mv_extension ORDER BY PRIMARY KEY.

    SELECT * FROM ttrees
            INTO TABLE ls_extension-sequences
            WHERE extension = mv_extension ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'SHI5'
                 ig_data = ls_extension ).

  ENDMETHOD.
ENDCLASS.
