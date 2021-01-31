CLASS zcl_abapgit_object_drul DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      clear_fields
        CHANGING
          cs_dependency_rule TYPE any,

      clear_field
        IMPORTING
          iv_fieldname       TYPE csequence
        CHANGING
          cs_dependency_rule TYPE any,

      fill_metadata_from_db
        CHANGING
          cs_dependency_rule TYPE any
        RAISING
          zcx_abapgit_exception,

      get_transport_req_if_needed
        IMPORTING
          iv_package                  TYPE devclass
        RETURNING
          VALUE(rv_transport_request) TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      get_wb_object_operator
        RETURNING
          VALUE(ri_wb_object_operator) TYPE REF TO object
        RAISING
          zcx_abapgit_exception.

    DATA:
      mr_dependency_rule     TYPE REF TO data,
      mv_dependency_rule_key TYPE seu_objkey,
      mi_persistence         TYPE REF TO if_wb_object_persist,
      mi_wb_object_operator  TYPE REF TO object.

ENDCLASS.



CLASS zcl_abapgit_object_drul IMPLEMENTATION.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_dependency_rule
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.


  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CONTENT-SOURCE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_dependency_rule_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_dependency_rule TYPE ('CL_DRUL_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_DRUL_WB_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        zcx_abapgit_exception=>raise( |DRUL not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator  TYPE REF TO object,
      lr_dependency_rule_old TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_dependency_rule_old> TYPE any,
      <lv_created_at>          TYPE xsddatetime_z,
      <lv_created_by>          TYPE syuname,
      <lv_created_at_old>      TYPE xsddatetime_z,
      <lv_created_by_old>      TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE DATA lr_dependency_rule_old TYPE ('CL_DRUL_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_dependency_rule_old->* TO <ls_dependency_rule_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_dependency_rule_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_dependency_rule
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_dependency_rule
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_dependency_rule_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_dependency_rule_old>
           TO <lv_created_at_old>.
    ASSERT sy-subrc = 0.

    <lv_created_at> = <lv_created_at_old>.
    <lv_created_by> = <lv_created_by_old>.

  ENDMETHOD.


  METHOD get_transport_req_if_needed.

    DATA: li_sap_package TYPE REF TO zif_abapgit_sap_package.

    li_sap_package = zcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      rv_transport_request = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.
    ENDIF.

  ENDMETHOD.


  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'DRUL'.
    ls_object_type-subtype_wb = 'DRL'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_dependency_rule_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_object_data_model->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lx_error              TYPE REF TO cx_root,
      lv_transport_request  TYPE trkorr,
      li_wb_object_operator TYPE REF TO object.

    lv_transport_request = get_transport_req_if_needed( iv_package ).
    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = lv_transport_request.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lv_transport_request  TYPE trkorr.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE data.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'DRUL'
      CHANGING
        cg_data = <ls_dependency_rule> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CREATE OBJECT li_object_data_model TYPE ('CL_DRUL_WB_OBJECT_DATA').

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        <lv_source> = mo_files->read_string( 'asdrul' ).

        tadir_insert( iv_package ).

        lv_transport_request = get_transport_req_if_needed( iv_package ).

        IF zif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_dependency_rule = <ls_dependency_rule> ).
          li_object_data_model->set_data( <ls_dependency_rule> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = lv_transport_request.

        ELSE.

          li_object_data_model->set_data( <ls_dependency_rule> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
              package           = iv_package
              transport_request = lv_transport_request.

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'D' " if_wb_object_data_selection_co=>c_data_content
              transport_request = lv_transport_request.

        ENDIF.

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~ACTIVATE').

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_dependency_rule_key
            p_version              = 'A'
            p_existence_check_only = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE string.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_dependency_rule>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        lv_source = <lv_source>.

        clear_fields( CHANGING cs_dependency_rule = <ls_dependency_rule> ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'DRUL'
        ig_data = <ls_dependency_rule> ).

    mo_files->add_string(
        iv_ext    = 'asdrul'
        iv_string = lv_source ).

  ENDMETHOD.
ENDCLASS.
