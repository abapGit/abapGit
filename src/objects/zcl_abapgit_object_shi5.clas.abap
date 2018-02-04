CLASS zcl_abapgit_object_shi5 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.


  PRIVATE SECTION.
    TYPES: tty_ttree_extt TYPE STANDARD TABLE OF ttree_extt
                               WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF ty_extension,
             header TYPE ttree_ext,
             texts  TYPE tty_ttree_extt,
           END OF ty_extension.

    DATA: mv_extension TYPE hier_names.

ENDCLASS.

CLASS zcl_abapgit_object_shi5 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_extension = ms_item-obj_name.

  ENDMETHOD.                    "constructor

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |TODO: Jump { ms_item-obj_type }| ).
  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~exists.

    DATA: ls_extension_header TYPE ttree_ext.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension_header.

    rv_bool = boolc( ls_extension_header IS NOT INITIAL ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~delete.

    DATA: ls_message             TYPE hier_mess,
          lv_deletion_successful TYPE hier_yesno.

    CALL FUNCTION 'STREE_EXTENSION_DELETE'
      EXPORTING
        extension           = mv_extension
      IMPORTING
        message             = ls_message
        deletion_successful = lv_deletion_successful.

    IF lv_deletion_successful = abap_false.
      zcx_abapgit_exception=>raise( ls_message-msgtxt ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~serialize.

    DATA: ls_extension TYPE ty_extension.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension-header.

    SELECT * FROM ttree_extt
             INTO TABLE ls_extension-texts
             WHERE extension = mv_extension.

    io_xml->add( iv_name = 'SHI5'
                 ig_data = ls_extension ).

  ENDMETHOD.                    "serialize

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
    MODIFY ttree_extt FROM TABLE ls_extension-texts.

    tadir_insert( iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_shi5 IMPLEMENTATION
