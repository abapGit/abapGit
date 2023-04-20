CLASS zcl_abapgit_object_char DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_char,
        cls_attribute   TYPE cls_attribute,
        cls_attributet  TYPE STANDARD TABLE OF cls_attributet WITH DEFAULT KEY,
        cls_attr_value  TYPE STANDARD TABLE OF cls_attr_value WITH DEFAULT KEY,
        cls_attr_valuet TYPE STANDARD TABLE OF cls_attr_valuet WITH DEFAULT KEY,
      END OF ty_char .

    CONSTANTS c_longtext_id_char TYPE dokil-id VALUE 'CH'.

    METHODS instantiate_char_and_lock
      IMPORTING
        !iv_type_group       TYPE cls_object_type_group
        !iv_activation_state TYPE pak_activation_state
      RETURNING
        VALUE(ro_char)       TYPE REF TO cl_cls_attribute
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_char IMPLEMENTATION.


  METHOD instantiate_char_and_lock.

    DATA: lv_new  TYPE abap_bool,
          lv_name TYPE cls_attribute_name.


    SELECT SINGLE name FROM cls_attribute INTO lv_name WHERE name = ms_item-obj_name.
    lv_new = boolc( sy-subrc <> 0 ).
    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_char
          EXPORTING
            im_name             = lv_name
            im_type_group       = iv_type_group
            im_new              = lv_new
            im_activation_state = iv_activation_state.
      CATCH cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_invalid_state
          cx_pak_wb_object_locked.
        zcx_abapgit_exception=>raise( 'Error while instantiating CL_CLS_ATTRIBUTE' ).
    ENDTRY.

    IF lv_new = abap_false.
      TRY.
          ro_char->if_pak_wb_object~lock_and_refresh( ).
        CATCH cx_pak_invalid_data
            cx_pak_not_authorized
            cx_pak_invalid_state
            cx_pak_wb_object_locked.
          zcx_abapgit_exception=>raise( |Could not aquire lock, CHAR { lv_name }| ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM cls_attribute INTO rv_user
      WHERE name = ms_item-obj_name
      AND activation_state = 'A'.

    IF rv_user IS INITIAL.
      SELECT SINGLE created_by FROM cls_attribute INTO rv_user
        WHERE name = ms_item-obj_name
        AND activation_state = 'A'.
    ENDIF.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_char       TYPE REF TO cl_cls_attribute,
          lv_type_group TYPE cls_attribute-type_group,
          lx_pak_error  TYPE REF TO cx_root,
          lv_text       TYPE string.


    SELECT SINGLE type_group FROM cls_attribute INTO lv_type_group
      WHERE name = ms_item-obj_name
      AND activation_state = 'A'.

    lo_char = instantiate_char_and_lock( iv_type_group       = lv_type_group
                                         iv_activation_state = cl_pak_wb_domains=>co_activation_state-active ).

    TRY.
        lo_char->if_pak_wb_object~delete( ).

        lo_char->if_pak_wb_object~save( ).

        lo_char->if_pak_wb_object_internal~unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_char->if_pak_wb_object_internal~unlock( ).
        lv_text = lx_pak_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
      CLEANUP.
        lo_char->if_pak_wb_object_internal~unlock( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_char        TYPE ty_char,
          ls_description LIKE LINE OF ls_char-cls_attributet,
          lo_char        TYPE REF TO cl_cls_attribute,
          lx_pak_error   TYPE REF TO cx_root,
          lv_text        TYPE string.

    FIELD-SYMBOLS: <ls_value>  LIKE LINE OF ls_char-cls_attr_value,
                   <lg_any>    TYPE any,
                   <ls_valuet> LIKE LINE OF ls_char-cls_attr_valuet.


    io_xml->read( EXPORTING iv_name = 'CHAR'
                  CHANGING cg_data = ls_char ).

    tadir_insert( iv_package ).

    lo_char = instantiate_char_and_lock( iv_type_group       = ls_char-cls_attribute-type_group
                                         iv_activation_state = cl_pak_wb_domains=>co_activation_state-inactive ).

    TRY.
        lo_char->if_cls_attribute~set_kind( ls_char-cls_attribute-kind ).
        lo_char->if_cls_attribute~set_single_valued( ls_char-cls_attribute-is_single_valued ).
        lo_char->if_cls_attribute~set_aspect(
          im_aspect_for   = ls_char-cls_attribute-is_aspect_for
          im_aspect_value = ls_char-cls_attribute-aspect_value ).
        lo_char->if_cls_attribute~set_default_flag( ls_char-cls_attribute-default_flag ).
        lo_char->if_cls_attribute~set_default_value( ls_char-cls_attribute-default_value ).
        lo_char->if_cls_attribute~set_sub_object_treatment( ls_char-cls_attribute-sub_obj_treatm ).
        lo_char->if_cls_attribute~set_automatic_changes_allowed( ls_char-cls_attribute-automatic_change ).
        lo_char->if_cls_attribute~set_manual_changes_allowed( ls_char-cls_attribute-manu_chag_allow ).
        lo_char->if_cls_attribute~set_implicit_changes_allowed( ls_char-cls_attribute-implicit_change ).
        lo_char->if_cls_attribute~set_expl_values_dominate_links( ls_char-cls_attribute-weak_links ).
        lo_char->if_cls_attribute~set_assignment_package_rule( ls_char-cls_attribute-assignment_devc ).

* Method SET_HIDE_ICON does not exist in some releases, not present in 751
        ASSIGN COMPONENT 'HIDE_ICONS' OF STRUCTURE ls_char-cls_attribute TO <lg_any>.
        IF sy-subrc = 0.
          CALL METHOD lo_char->('IF_CLS_ATTRIBUTE~SET_HIDE_ICON')
            EXPORTING
              im_hide_icon = <lg_any>.
        ENDIF.

        lo_char->if_cls_attribute~set_hide_remark( ls_char-cls_attribute-hide_remark ).
        lo_char->if_cls_attribute~set_visible_in_customer_system( ls_char-cls_attribute-visible_for_cust ).
        lo_char->if_cls_attribute~set_value_table( ls_char-cls_attribute-value_table ).
        lo_char->if_cls_attribute~set_vtable_field( ls_char-cls_attribute-vtable_field ).
        lo_char->if_cls_attribute~set_vtable_icon_f( ls_char-cls_attribute-vtable_icon_f ).
        lo_char->if_cls_attribute~set_vtext_langu_f( ls_char-cls_attribute-vtext_langu_f ).
        lo_char->if_cls_attribute~set_vtext_table( ls_char-cls_attribute-vtext_table ).
        lo_char->if_cls_attribute~set_vtext_text_f( ls_char-cls_attribute-vtext_text_f ).
        lo_char->if_cls_attribute~set_vtext_value_f( ls_char-cls_attribute-vtext_value_f ).
        lo_char->if_cls_attribute~set_existing_objects_only( ls_char-cls_attribute-existing_objects ).
        lo_char->if_cls_attribute~set_objs_of_typegr( ls_char-cls_attribute-objs_of_typegr ).
        lo_char->if_cls_attribute~set_obj_values_have_subtypes( ls_char-cls_attribute-objs_w_subtype ).
        lo_char->if_cls_attribute~set_arbtry_val_type( ls_char-cls_attribute-arbtry_val_type ).

        READ TABLE ls_char-cls_attributet INTO ls_description WITH KEY langu = mv_language.
        IF sy-subrc <> 0.
          READ TABLE ls_char-cls_attributet INTO ls_description INDEX 1.
        ENDIF.
        lo_char->if_cls_attribute~set_description( ls_description-text ).

        LOOP AT ls_char-cls_attr_value ASSIGNING <ls_value>.
          <ls_value>-activation_state = 'I'.
        ENDLOOP.
        LOOP AT ls_char-cls_attr_valuet ASSIGNING <ls_valuet>.
          <ls_valuet>-activation_state = 'I'.
        ENDLOOP.

        lo_char->if_cls_attribute~set_values(
          im_values   = ls_char-cls_attr_value
          im_values_t = ls_char-cls_attr_valuet ).

        set_default_package( iv_package ).

        lo_char->if_pak_wb_object~save( ).

        lo_char->if_pak_wb_object~activate( ).

        lo_char->if_pak_wb_object_internal~unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_char->if_pak_wb_object_internal~unlock( ).
        lv_text = lx_pak_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
      CLEANUP.
        lo_char->if_pak_wb_object_internal~unlock( ).
    ENDTRY.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_char ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = cl_cls_attribute=>exists_object_attribute( ms_item-obj_name ).

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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ECLS_ATTRIBUTE'
                                            iv_argument    = |{ ms_item-obj_name }*| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_char TYPE ty_char.

    CONSTANTS: lc_active TYPE c LENGTH 1 VALUE 'A'.


    SELECT SINGLE * FROM cls_attribute INTO ls_char-cls_attribute
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active.
* todo, ASSIGNMENT_DEVC?

    CLEAR: ls_char-cls_attribute-created_by,
           ls_char-cls_attribute-created_on,
           ls_char-cls_attribute-changed_by,
           ls_char-cls_attribute-changed_on.

    SELECT * FROM cls_attributet INTO TABLE ls_char-cls_attributet
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.
    IF io_xml->i18n_params( )-main_language_only = abap_true.
      DELETE ls_char-cls_attributet WHERE langu <> mv_language.
    ENDIF.

    SELECT * FROM cls_attr_value INTO TABLE ls_char-cls_attr_value
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.

    SELECT * FROM cls_attr_valuet INTO TABLE ls_char-cls_attr_valuet
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.
    IF io_xml->i18n_params( )-main_language_only = abap_true.
      DELETE ls_char-cls_attr_valuet WHERE langu <> mv_language.
    ENDIF.

    io_xml->add( iv_name = 'CHAR'
                 ig_data = ls_char ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_char ).

  ENDMETHOD.
ENDCLASS.
