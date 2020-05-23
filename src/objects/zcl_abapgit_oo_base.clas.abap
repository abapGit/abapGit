CLASS zcl_abapgit_oo_base DEFINITION PUBLIC ABSTRACT.
  PUBLIC SECTION.
    INTERFACES: zif_abapgit_oo_object_fnc.

  PROTECTED SECTION.
    CLASS-METHODS:
      convert_attrib_to_vseoattrib
        IMPORTING iv_clsname           TYPE seoclsname
                  it_attributes        TYPE zif_abapgit_definitions=>ty_obj_attribute_tt
        RETURNING VALUE(rt_vseoattrib) TYPE seoo_attributes_r.

  PRIVATE SECTION.
    DATA mv_skip_test_classes TYPE abap_bool.

    METHODS deserialize_abap_source_old
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception.

    METHODS deserialize_abap_source_new
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception
                cx_sy_dyn_call_error.
ENDCLASS.



CLASS zcl_abapgit_oo_base IMPLEMENTATION.


  METHOD convert_attrib_to_vseoattrib.
    FIELD-SYMBOLS: <ls_attribute>  LIKE LINE OF it_attributes,
                   <ls_vseoattrib> LIKE LINE OF rt_vseoattrib.

    LOOP AT it_attributes ASSIGNING <ls_attribute>.
      INSERT INITIAL LINE INTO TABLE rt_vseoattrib ASSIGNING <ls_vseoattrib>.
      MOVE-CORRESPONDING <ls_attribute> TO <ls_vseoattrib>.
      <ls_vseoattrib>-clsname = iv_clsname.
      UNASSIGN <ls_vseoattrib>.
    ENDLOOP.
    UNASSIGN <ls_attribute>.
  ENDMETHOD.


  METHOD deserialize_abap_source_new.
    DATA: lo_factory  TYPE REF TO object,
          lo_source   TYPE REF TO object,
          lo_settings TYPE REF TO object,
          lr_settings TYPE REF TO data.

    FIELD-SYMBOLS <lg_settings> TYPE any.


    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_clskey
        version = seoc_version_inactive.

    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_factory.

    "Enable modification mode to avoid exception CX_OO_ACCESS_PERMISSON when
    "dealing with objects in foreign namespaces (namespace role = C)
    CALL METHOD lo_factory->('CREATE_SETTINGS')
      EXPORTING
        modification_mode_enabled = abap_true
      RECEIVING
        result                    = lo_settings.

    CREATE DATA lr_settings TYPE REF TO ('IF_OO_CLIF_SOURCE_SETTINGS').
    ASSIGN lr_settings->* TO <lg_settings>.

    <lg_settings> ?= lo_settings.

    CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        settings  = <lg_settings>
      RECEIVING
        result    = lo_source.

    TRY.
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'source_new, access permission exception' ).
    ENDTRY.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
      EXPORTING
        source = it_source.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

  ENDMETHOD.


  METHOD deserialize_abap_source_old.
    "for backwards compatability down to 702

    DATA: lo_source TYPE REF TO cl_oo_source.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from CL_OO_SOURCE. Subrc = { sy-subrc }| ).
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( it_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        zcx_abapgit_exception=>raise( 'save failure' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~add_to_activation_list.
    zcl_abapgit_objects_activation=>add_item( is_item ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_documentation.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id            = 'CL'
        langu         = iv_language
        object        = iv_object_name
        no_masterlang = iv_no_masterlang
      TABLES
        line          = it_lines
      EXCEPTIONS
        ret_code      = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DOCU_UPD. Subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.
    TRY.
        deserialize_abap_source_new(
          is_clskey = is_key
          it_source = it_source ).
      CATCH cx_sy_dyn_call_error.
        deserialize_abap_source_old(
          is_clskey = is_key
          it_source = it_source ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc <> 2 ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_class_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_skip_test_classes.
    rv_skip = mv_skip_test_classes.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~insert_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_attributes.
    SELECT cmpname attbusobj attkeyfld
      FROM seocompodf
      INTO CORRESPONDING FIELDS OF TABLE rt_attributes
      WHERE clsname = iv_object_name
        AND ( attbusobj <> space OR attkeyfld <> space )
        AND version = '1'
      ORDER BY PRIMARY KEY.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_descriptions.
    IF iv_language IS INITIAL.
      " load all languages
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
             WHERE clsname   = iv_obejct_name
               AND descript <> ''
             ORDER BY PRIMARY KEY.                        "#EC CI_SUBRC
    ELSE.
      " load master language
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
              WHERE clsname   = iv_obejct_name
                AND langu = iv_language
                AND descript <> ''
              ORDER BY PRIMARY KEY.                       "#EC CI_SUBRC
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_documentation.
    DATA: lv_state  TYPE dokstate,
          lv_object TYPE dokhl-object,
          lt_lines  TYPE tlinetab.

    lv_object = iv_class_name.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = iv_language
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      rt_lines = lt_lines.
    ELSE.
      CLEAR rt_lines.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_superclass.
    SELECT SINGLE refclsname FROM vseoextend INTO rv_superclass
      WHERE clsname = iv_classname.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~serialize_abap.
    DATA lo_oo_serializer TYPE REF TO zcl_abapgit_oo_serializer.
    CREATE OBJECT lo_oo_serializer.
    CASE iv_type.
      WHEN seop_ext_class_locals_def.
        rt_source = lo_oo_serializer->serialize_locals_def( is_class_key ).
      WHEN seop_ext_class_locals_imp.
        rt_source = lo_oo_serializer->serialize_locals_imp( is_class_key ).
      WHEN seop_ext_class_macros.
        rt_source = lo_oo_serializer->serialize_macros( is_class_key ).
      WHEN seop_ext_class_testclasses.
        rt_source = lo_oo_serializer->serialize_testclasses( is_class_key ).
        mv_skip_test_classes = lo_oo_serializer->are_test_classes_skipped( ).
      WHEN OTHERS.
        rt_source = lo_oo_serializer->serialize_abap_clif_source( is_class_key ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~update_descriptions.
    DELETE FROM seocompotx WHERE clsname = is_key-clsname. "#EC CI_SUBRC
    INSERT seocompotx FROM TABLE it_descriptions.         "#EC CI_SUBRC
  ENDMETHOD.
ENDCLASS.
