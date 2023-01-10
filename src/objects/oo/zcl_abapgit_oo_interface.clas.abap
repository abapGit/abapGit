CLASS zcl_abapgit_oo_interface DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_oo_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_oo_object_fnc~create
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~delete
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_includes
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_interface_properties
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~deserialize_source
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~exists
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS update_report
      IMPORTING
        !iv_program       TYPE programm
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_meta
      IMPORTING
        !iv_name   TYPE seoclsname
        !it_source TYPE rswsourcet
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS init_scanner
      IMPORTING
        !it_source        TYPE zif_abapgit_definitions=>ty_string_tt
        !iv_name          TYPE seoclsname
      RETURNING
        VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_interface
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_oo_interface IMPLEMENTATION.


  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.

    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_interface=>create_interface_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        zcx_abapgit_exception=>raise( 'error initializing INTF scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.


  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_interface_section_source,
          lx_error      TYPE REF TO cx_oo_source_save_failure,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
        CREATE OBJECT lo_update TYPE ('CL_OO_INTERFACE_SECTION_SOURCE')
          EXPORTING
            intkey                        = ls_clskey
            state                         = 'A'
            source                        = it_source
          EXCEPTIONS
            interface_not_existing        = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_interface_section_source
          EXPORTING
            intkey                 = ls_clskey
            state                  = 'A'
          EXCEPTIONS
            interface_not_existing = 1
            read_source_error      = 2
            OTHERS                 = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).

    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      zcx_abapgit_exception=>raise( |INTF, error while scanning source. Subrc = { sy-subrc }| ).
    ENDIF.

* this will update the SEO* database tables
    TRY.
        lo_update->revert_scan_result( ).
      CATCH cx_oo_source_save_failure INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD update_report.

    DATA: lt_old TYPE string_table.

    READ REPORT iv_program INTO lt_old.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Fatal error. Include { iv_program } should have been created previously!| ).
    ENDIF.

    IF lt_old <> it_source.
      INSERT REPORT iv_program FROM it_source.
      ASSERT sy-subrc = 0.
      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.

    DATA:
      lt_vseoattrib    TYPE seoo_attributes_r,
      ls_interface_key TYPE seoclskey,
      ls_properties    TYPE vseointerf.

    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    " Get existing interface properties and check if the interface
    " needs to be created/updated (or is the same)
    IF iv_check = abap_true.
      ls_interface_key-clsname = <lv_clsname>.
      ls_properties = zif_abapgit_oo_object_fnc~get_interface_properties( ls_interface_key ).

      IF ls_properties = cg_properties.
        RETURN.
      ENDIF.
    ENDIF.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    " Hardcode STATE (#2612)
    ls_properties = cg_properties.
    ls_properties-state = seoc_state_implemented.

    TRY.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            interface       = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
          CHANGING
            interface       = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_class     = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_interface,
          lt_public  TYPE seop_source_string.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

    lt_public = lo_scanner->get_interface_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_intfsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name   = is_key-clsname
                     it_source = lt_public ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
      EXPORTING
        intkey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_class      = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc = 0 OR sy-subrc = 4 ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
    DATA lv_interface_name TYPE seoclsname.
    lv_interface_name = iv_object_name.
    APPEND cl_oo_classname_service=>get_interfacepool_name( lv_interface_name ) TO rt_includes.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_interface_key
        version      = seoc_version_active
      IMPORTING
        interface    = rs_interface_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR:
      " TODO 2023-08-01: Clear rs_interface_properties-state (#2612)
      rs_interface_properties-uuid,
      rs_interface_properties-author,
      rs_interface_properties-createdon,
      rs_interface_properties-changedby,
      rs_interface_properties-changedon,
      rs_interface_properties-chgdanyby,
      rs_interface_properties-chgdanyon,
      rs_interface_properties-r3release,
      rs_interface_properties-version.
  ENDMETHOD.
ENDCLASS.
