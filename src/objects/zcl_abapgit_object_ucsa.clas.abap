CLASS zcl_abapgit_object_ucsa DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PRIVATE SECTION.
    TYPES:
      ty_id TYPE c LENGTH 30.

    METHODS:
      get_persistence
        IMPORTING
          iv_id                 TYPE ty_id
        RETURNING
          VALUE(ro_persistence) TYPE REF TO object,

      clear_dynamic_fields
        CHANGING
          cs_complete_comm_assembly TYPE any,

      clear_field
        IMPORTING
          iv_fieldname TYPE csequence
        CHANGING
          cs_header    TYPE any.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_UCSA IMPLEMENTATION.


  METHOD clear_dynamic_fields.

    FIELD-SYMBOLS: <lg_header> TYPE any.


    ASSIGN COMPONENT 'HEADER' OF STRUCTURE cs_complete_comm_assembly
           TO <lg_header>.
    ASSERT sy-subrc = 0.

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDBY'
      CHANGING  cs_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDON'
      CHANGING  cs_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDAT'
      CHANGING  cs_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDBY'
      CHANGING  cs_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDON'
      CHANGING  cs_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDAT'
      CHANGING  cs_header    = <lg_header> ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_header
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

  ENDMETHOD.


  METHOD get_persistence.

    CALL METHOD ('CL_UCON_SA_DB_PERSIST')=>('IF_UCON_SA_PERSIST~GET_INSTANCE')
      EXPORTING
        id       = iv_id
      RECEIVING
        instance = ro_persistence.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_id          TYPE ty_id,
          lx_root        TYPE REF TO cx_root,
          lv_text        TYPE string,
          lo_persistence TYPE REF TO object.

    TRY.
        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~DELETE')
          EXPORTING
            version = zif_abapgit_definitions=>gc_version-active.

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_id                     TYPE ty_id,
          lx_root                   TYPE REF TO cx_root,
          lv_text                   TYPE string,
          lo_persistence            TYPE REF TO object,
          lr_complete_comm_assembly TYPE REF TO data.

    FIELD-SYMBOLS: <lg_complete_comm_assembly> TYPE any.

    TRY.
        CREATE DATA lr_complete_comm_assembly TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_complete_comm_assembly->* TO <lg_complete_comm_assembly>.
        ASSERT sy-subrc = 0.

        io_xml->read(
          EXPORTING
            iv_name = 'UCSA'
          CHANGING
            cg_data = <lg_complete_comm_assembly> ).

        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~CREATE').

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~SAVE')
          EXPORTING
            sa      = <lg_complete_comm_assembly>
            version = zif_abapgit_definitions=>gc_version-active.

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_id          TYPE ty_id,
          lo_persistence TYPE REF TO object.

    lv_id = ms_item-obj_name.

    TRY.
        lo_persistence = get_persistence( lv_id ).

        " Interface IF_UCON_SA_PERSIST and other objects are not present
        " in lower Netweaver realeses. Therefore we have to call them
        " dynamically to be downward comapatible.

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = zif_abapgit_definitions=>gc_version-active
            language = sy-langu.

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    rv_changed = abap_true.

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
      zcx_abapgit_exception=>raise( 'error from RS_TOOL_ACCESS' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_id                     TYPE ty_id,
          lx_root                   TYPE REF TO cx_root,
          lv_text                   TYPE string,
          lo_persistence            TYPE REF TO object,
          lr_complete_comm_assembly TYPE REF TO data.

    FIELD-SYMBOLS: <lg_complete_comm_assembly> TYPE any.


    lv_id = ms_item-obj_name.

    TRY.
        CREATE DATA lr_complete_comm_assembly TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_complete_comm_assembly->* TO <lg_complete_comm_assembly>.
        ASSERT sy-subrc = 0.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = zif_abapgit_definitions=>gc_version-active
            language = sy-langu
          IMPORTING
            sa       = <lg_complete_comm_assembly>.

        clear_dynamic_fields( CHANGING cs_complete_comm_assembly = <lg_complete_comm_assembly> ).

        io_xml->add( iv_name = 'UCSA'
                     ig_data = <lg_complete_comm_assembly> ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
