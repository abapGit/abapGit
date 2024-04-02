CLASS zcl_abapgit_object_nspc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    METHODS constructor
      IMPORTING
        is_item         TYPE zif_abapgit_definitions=>ty_item
        iv_language     TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_nspc,
        namespace  TYPE trnspacet-namespace,
        replicense TYPE trnspacet-replicense,
        sscrflag   TYPE trnspacet-sscrflag,
        sapflag    TYPE trnspacet-sapflag,
        gen_only   TYPE trnspacet-gen_only,
      END OF ty_nspc .
    TYPES:
      BEGIN OF ty_nspc_text,
        spras     TYPE trnspacett-spras,
        descriptn TYPE trnspacett-descriptn,
        owner     TYPE trnspacett-owner,
      END OF ty_nspc_text .
    TYPES:
      ty_nspc_texts TYPE STANDARD TABLE OF ty_nspc_text .

    DATA mv_component TYPE cvers-component.

    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml       TYPE REF TO zif_abapgit_xml_input
        !iv_namespace TYPE namespace
      RAISING
        zcx_abapgit_exception .
    METHODS add_to_transport
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_sw_component
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_sw_component
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_nspc IMPLEMENTATION.


  METHOD add_to_transport.

    DATA: li_sap_package TYPE REF TO zif_abapgit_sap_package.

    li_sap_package = zcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      corr_insert( iv_package ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    mv_component = replace( val  = is_item-obj_name
                            sub  = '/'
                            with = ''
                            occ  = 0 ).

  ENDMETHOD.


  METHOD deserialize_sw_component.

    DATA:
      ls_cvers_old TYPE cvers,
      ls_cvers_new TYPE cvers,
      ls_cvers_ref TYPE cvers_ref.

    ii_xml->read( EXPORTING iv_name = 'CVERS'
                  CHANGING  cg_data = ls_cvers_new ).

    ii_xml->read( EXPORTING iv_name = 'CVERS_REF'
                  CHANGING  cg_data = ls_cvers_ref ).

    IF ls_cvers_new IS NOT INITIAL.
      SELECT SINGLE * FROM cvers INTO ls_cvers_old WHERE component = mv_component.
      IF sy-subrc = 0.
        IF ls_cvers_old <> ls_cvers_new.
          zcx_abapgit_exception=>raise( `Update of software component not supported.`
            && ` Use Software Update Manager (SUM)` ).
        ENDIF.
      ELSE.
        INSERT cvers FROM ls_cvers_new.
      ENDIF.
    ENDIF.

    IF ls_cvers_ref IS NOT INITIAL.
      MODIFY cvers_ref FROM ls_cvers_ref.
      IF sy-subrc <> 0.
        INSERT cvers_ref FROM ls_cvers_ref.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_cvers_refs TYPE TABLE OF cvers_ref,
      ls_cvers_ref  TYPE cvers_ref,
      lt_nspc_texts TYPE ty_nspc_texts.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'NSPC_TEXTS'
                  CHANGING  cg_data = lt_nspc_texts ).

    ii_xml->read( EXPORTING iv_name = 'CVERS_REFS'
                  CHANGING  cg_data = lt_cvers_refs ).

    SORT lt_i18n_langs.
    SORT lt_nspc_texts BY spras. " Optimization
    SORT lt_cvers_refs BY langu. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      ls_trnspacett-namespace = iv_namespace.
      READ TABLE lt_nspc_texts ASSIGNING <ls_nspc_text> WITH KEY spras = <lv_lang>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Cannot find language { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_nspc_text> TO ls_trnspacett.

      MODIFY trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        INSERT trnspacett FROM ls_trnspacett.
      ENDIF.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
      ENDIF.

      READ TABLE lt_cvers_refs INTO ls_cvers_ref WITH KEY langu = <lv_lang>.
      IF sy-subrc = 0.
        MODIFY cvers_ref FROM ls_cvers_ref.
        IF sy-subrc <> 0.
          INSERT cvers_ref FROM ls_cvers_ref.
        ENDIF.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error upserting text for software component| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_sw_component.

    DATA:
      ls_cvers     TYPE cvers,
      ls_cvers_ref TYPE cvers_ref.

    SELECT SINGLE * FROM cvers INTO ls_cvers WHERE component = mv_component.
    IF sy-subrc = 0.
      ii_xml->add( iv_name = 'CVERS'
                   ig_data = ls_cvers ).
    ENDIF.

    SELECT SINGLE * FROM cvers_ref INTO ls_cvers_ref WHERE component = mv_component AND langu = mv_language.
    IF sy-subrc = 0.
      ii_xml->add( iv_name = 'CVERS_REF'
                   ig_data = ls_cvers_ref ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_nspc_texts TYPE ty_nspc_texts,
      lt_cvers_refs TYPE TABLE OF cvers_ref,
      lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT spras AS langu FROM trnspacett INTO TABLE lt_i18n_langs
      WHERE namespace = ms_item-obj_name AND spras <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
        WHERE namespace = ms_item-obj_name AND spras = <lv_lang>.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_nspc_texts ASSIGNING <ls_nspc_text>.
        MOVE-CORRESPONDING ls_trnspacett TO <ls_nspc_text>.
      ENDIF.

      SELECT * FROM cvers_ref APPENDING TABLE lt_cvers_refs
        WHERE component = mv_component AND langu = <lv_lang>
        ORDER BY PRIMARY KEY.
    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_nspc_texts BY spras ASCENDING.
    SORT lt_cvers_refs.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'NSPC_TEXTS'
                   ig_data = lt_nspc_texts ).

      ii_xml->add( iv_name = 'CVERS_REFS'
                   ig_data = lt_cvers_refs ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE changeuser FROM trnspacet INTO rv_user
       WHERE namespace = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    RETURN. " not supported
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_nspc       TYPE ty_nspc,
      ls_nspc_text  TYPE ty_nspc_text,
      lv_modifiable TYPE abap_bool,
      ls_trnspacet  TYPE trnspacet,
      ls_trnspacett TYPE trnspacett.

    io_xml->read( EXPORTING iv_name = 'NSPC'
                  CHANGING  cg_data = ls_nspc ).

    io_xml->read( EXPORTING iv_name = 'NSPC_TEXT'
                  CHANGING  cg_data = ls_nspc_text ).

    add_to_transport( iv_package ).

    SELECT SINGLE * FROM trnspacet INTO ls_trnspacet WHERE namespace = ls_nspc-namespace.
    IF sy-subrc = 0.
      " For existing namespace, check if it's modifiable (SE03)
      SELECT SINGLE editflag FROM trnspace INTO lv_modifiable WHERE namespace = ls_nspc-namespace.
      IF sy-subrc = 0 AND lv_modifiable = abap_false.
        zcx_abapgit_exception=>raise( |Namespace is not modifiable| ).
      ENDIF.

      " keep existing role
      ls_trnspacet-replicense = ls_nspc-replicense.
      ls_trnspacet-sscrflag   = ls_nspc-sscrflag.
      ls_trnspacet-sapflag    = ls_nspc-sapflag.
      ls_trnspacet-gen_only   = ls_nspc-gen_only.
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      MODIFY trnspacet FROM ls_trnspacet.
    ELSE.
      MOVE-CORRESPONDING ls_nspc TO ls_trnspacet.
      ls_trnspacet-role       = 'C'. " customer repair license
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      INSERT trnspacet FROM ls_trnspacet.
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error upserting namespace| ).
    ENDIF.

    SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
      WHERE namespace = ls_nspc-namespace AND spras = mv_language.
    IF sy-subrc = 0.
      ls_trnspacett-descriptn = ls_nspc_text-descriptn.
      ls_trnspacett-owner     = ls_nspc_text-owner.
      MODIFY trnspacett FROM ls_trnspacett.
    ELSE.
      MOVE-CORRESPONDING ls_nspc_text TO ls_trnspacett.
      ls_trnspacett-namespace = ls_nspc-namespace.
      INSERT trnspacett FROM ls_trnspacett.
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
    ENDIF.

    deserialize_texts( ii_xml       = io_xml
                       iv_namespace = ls_nspc-namespace ).

    deserialize_sw_component( io_xml ).

    " Fill trnspace and trnspacel tables
    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = ls_nspc-namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error activating namespace| ).
    ENDIF.

    " Make namespace modifiable
    UPDATE trnspace SET editflag = abap_true WHERE namespace = ls_nspc-namespace.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_namespace TYPE trnspace-namespace.

    lv_namespace = ms_item-obj_name.

    CALL FUNCTION 'TR_CHECK_NAMESPACE'
      EXPORTING
        iv_namespace        = lv_namespace
      EXCEPTIONS
        namespace_not_valid = 1
        OTHERS              = 2.

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
    rv_active = zif_abapgit_object~exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Launch general maintenance for namespaces
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'V_TRNSPACE'
        no_warning_for_clientindep   = 'X'
        variant_for_selection        = 'STANDARD'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        OTHERS                       = 14.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      ls_nspc      TYPE ty_nspc,
      ls_nspc_text TYPE ty_nspc_text.

    SELECT SINGLE * FROM trnspacet INTO CORRESPONDING FIELDS OF ls_nspc
      WHERE namespace = ms_item-obj_name.

    SELECT SINGLE * FROM trnspacett INTO CORRESPONDING FIELDS OF ls_nspc_text
      WHERE namespace = ms_item-obj_name AND spras = mv_language.

    io_xml->add( iv_name = 'NSPC'
                 ig_data = ls_nspc ).

    io_xml->add( iv_name = 'NSPC_TEXT'
                 ig_data = ls_nspc_text ).

    serialize_texts( io_xml ).

    serialize_sw_component( io_xml ).

  ENDMETHOD.
ENDCLASS.
