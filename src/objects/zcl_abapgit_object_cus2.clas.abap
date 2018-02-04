CLASS zcl_abapgit_object_cus2 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PRIVATE SECTION.
    TYPES: tty_attribute_titles        TYPE STANDARD TABLE OF cus_atrt
                                            WITH NON-UNIQUE DEFAULT KEY,
           tty_attribute_countries     TYPE STANDARD TABLE OF cus_atrcou
                                            WITH NON-UNIQUE DEFAULT KEY,
           tty_attribute_components    TYPE STANDARD TABLE OF tfm18
                                            WITH NON-UNIQUE DEFAULT KEY,
           tty_attribute_comp_variants TYPE STANDARD TABLE OF cus_atrvco
                                            WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_customizing_attribute,
             header              TYPE cus_atrh,
             titles              TYPE tty_attribute_titles,
             countries           TYPE tty_attribute_countries,
             components          TYPE tty_attribute_components,
             components_variants TYPE tty_attribute_comp_variants,
           END OF ty_customizing_attribute.

    DATA: mv_img_attribute TYPE cus_atr.

ENDCLASS.

CLASS zcl_abapgit_object_cus2 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_attribute = ms_item-obj_name.

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

    zcx_abapgit_exception=>raise( |TODO: Jump| ).

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_EXIST'
      EXPORTING
        img_attribute         = mv_img_attribute
      EXCEPTIONS
        attributes_exists_not = 1
        OTHERS                = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_DELETE'
      EXPORTING
        img_attribute = mv_img_attribute
      IMPORTING
        message       = ls_message.

    IF ls_message-msgty <> 'S'.
      zcx_abapgit_exception=>raise( |error from delete CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_DELETE| ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~serialize.

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_READ'
      EXPORTING
        img_attribute                 = mv_img_attribute
      IMPORTING
        attribute_header              = ls_customizing_attribute-header
      TABLES
        attribute_title               = ls_customizing_attribute-titles
        attribute_countries           = ls_customizing_attribute-countries
        attribute_components          = ls_customizing_attribute-components
        attribute_components_variants = ls_customizing_attribute-components_variants.

    CLEAR: ls_customizing_attribute-header-fdatetime,
           ls_customizing_attribute-header-fuser,
           ls_customizing_attribute-header-ldatetime,
           ls_customizing_attribute-header-luser.

    io_xml->add( iv_name = 'CUS2'
                 ig_data = ls_customizing_attribute ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute,
          ls_message               TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS2'
      CHANGING
        cg_data = ls_customizing_attribute ).

    CALL FUNCTION 'S_CUS_ATTRIBUTES_SAVE'
      EXPORTING
        img_attribute         = ls_customizing_attribute-header
      IMPORTING
        message               = ls_message
      TABLES
        attributes_title      = ls_customizing_attribute-titles
        attributes_countries  = ls_customizing_attribute-countries
        attributes_components = ls_customizing_attribute-components.

    IF ls_message-msgty <> 'S'.
      zcx_abapgit_exception=>raise( |error from deserialize CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_SAVE| ).
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_cus2 IMPLEMENTATION
