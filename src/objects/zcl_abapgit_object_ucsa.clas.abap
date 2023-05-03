CLASS zcl_abapgit_object_ucsa DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_version,
        active   TYPE r3state VALUE 'A',
        inactive TYPE r3state VALUE 'I',
      END OF c_version .

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
          cg_complete_comm_assembly TYPE any,

      clear_field
        IMPORTING
          iv_fieldname TYPE csequence
        CHANGING
          cg_header    TYPE any.

ENDCLASS.



CLASS zcl_abapgit_object_ucsa IMPLEMENTATION.


  METHOD clear_dynamic_fields.

    FIELD-SYMBOLS: <lg_header> TYPE any.


    ASSIGN COMPONENT 'HEADER' OF STRUCTURE cg_complete_comm_assembly
           TO <lg_header>.
    ASSERT sy-subrc = 0.

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDBY'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDON'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDAT'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDBY'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDON'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDAT'
      CHANGING  cg_header    = <lg_header> ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_header
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
            version = c_version-active.

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

    tadir_delete( ).

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
            version = c_version-active.

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
            version  = c_version-active
            language = mv_language.

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

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
    rv_is_locked = abap_false.
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
            version  = c_version-active
            language = mv_language
          IMPORTING
            sa       = <lg_complete_comm_assembly>.

        clear_dynamic_fields( CHANGING cg_complete_comm_assembly = <lg_complete_comm_assembly> ).

        io_xml->add( iv_name = 'UCSA'
                     ig_data = <lg_complete_comm_assembly> ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
