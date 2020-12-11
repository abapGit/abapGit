CLASS zcl_abapgit_object_shi5 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

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
