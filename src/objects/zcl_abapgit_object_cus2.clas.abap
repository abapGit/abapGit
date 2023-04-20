CLASS zcl_abapgit_object_cus2 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_attribute_titles        TYPE STANDARD TABLE OF cus_atrt
                                            WITH NON-UNIQUE DEFAULT KEY,
           ty_attribute_countries     TYPE STANDARD TABLE OF cus_atrcou
                                            WITH NON-UNIQUE DEFAULT KEY,
           ty_attribute_components    TYPE STANDARD TABLE OF tfm18
                                            WITH NON-UNIQUE DEFAULT KEY,
           ty_attribute_comp_variants TYPE STANDARD TABLE OF cus_atrvco
                                            WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_customizing_attribute,
             header              TYPE cus_atrh,
             titles              TYPE ty_attribute_titles,
             countries           TYPE ty_attribute_countries,
             components          TYPE ty_attribute_components,
             components_variants TYPE ty_attribute_comp_variants,
           END OF ty_customizing_attribute.

    DATA: mv_img_attribute TYPE cus_atr.

ENDCLASS.



CLASS zcl_abapgit_object_cus2 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_attribute = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.


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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_EXIST'
      EXPORTING
        img_attribute         = mv_img_attribute
      EXCEPTIONS
        attributes_exists_not = 1
        OTHERS                = 2.

    rv_bool = boolc( sy-subrc = 0 ).

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
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


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

    IF io_xml->i18n_params( )-main_language_only = abap_true.
      DELETE ls_customizing_attribute-titles WHERE spras <> mv_language.
    ENDIF.

    io_xml->add( iv_name = 'CUS2'
                 ig_data = ls_customizing_attribute ).

  ENDMETHOD.
ENDCLASS.
