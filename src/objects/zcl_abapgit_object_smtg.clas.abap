CLASS zcl_abapgit_object_smtg DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_template_id TYPE c LENGTH 30,
      mo_structdescr TYPE REF TO cl_abap_structdescr.

    METHODS:
      clear_field
        IMPORTING
          iv_fieldname TYPE string
        CHANGING
          cg_header    TYPE any,

      get_structure
        RETURNING
          VALUE(ro_structdescr) TYPE REF TO cl_abap_structdescr
        RAISING
          zcx_abapgit_exception,

      add_component
        IMPORTING
          iv_fielname       TYPE string
          iv_structure_name TYPE string
        CHANGING
          ct_components     TYPE abap_component_tab
        RAISING
          zcx_abapgit_exception,

      get_template
        EXPORTING
          es_template TYPE any
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_smtg IMPLEMENTATION.


  METHOD add_component.

    DATA:
      ls_component LIKE LINE OF ct_components,
      lo_typedescr TYPE REF TO cl_abap_typedescr.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_structure_name
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |SMTG not supported| ).
    ENDIF.

    ls_component-name =  iv_fielname.
    ls_component-type ?= lo_typedescr.
    INSERT ls_component INTO TABLE ct_components.

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN
      COMPONENT iv_fieldname
      OF STRUCTURE cg_header
      TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_template_id = ms_item-obj_name.
    mo_structdescr = get_structure( ).

  ENDMETHOD.


  METHOD get_structure.

    DATA: lt_components TYPE abap_component_tab.

    add_component(
      EXPORTING
        iv_fielname       = `HEADER`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GS_TMPL_HDR`
      CHANGING
        ct_components     = lt_components ).

    add_component(
      EXPORTING
        iv_fielname       = `HEADER_T`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GT_TMPL_HDR_T`
      CHANGING
        ct_components     = lt_components ).

    add_component(
      EXPORTING
        iv_fielname       = `CONTENT`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GT_TMPL_CONT`
      CHANGING
        ct_components     = lt_components ).

    ro_structdescr = cl_abap_structdescr=>create( lt_components ).

  ENDMETHOD.


  METHOD get_template.

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root,
      lo_template TYPE REF TO object.

    FIELD-SYMBOLS:
      <lg_template> TYPE data,
      <lg_header>   TYPE data,
      <lt_header>   TYPE INDEX TABLE,
      <lt_content>  TYPE INDEX TABLE.


    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER_T'
      OF STRUCTURE <lg_template>
      TO <lt_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'CONTENT'
      OF STRUCTURE <lg_template>
      TO <lt_content>.
    ASSERT sy-subrc = 0.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
          EXPORTING
            iv_id       = mv_template_id
          RECEIVING
            ro_instance = lo_template.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_HDR')
          RECEIVING
            rs_tmpl_hdr = <lg_header>.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_HDR_T_ALL')
          RECEIVING
            rt_tmpl_hdr_t = <lt_header>.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_CONT_ALL')
          RECEIVING
            rt_tmpl_cont = <lt_content>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    es_template = <lg_template>.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      lr_template TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_template>          TYPE data,
      <lv_last_changed_user> TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    get_template( IMPORTING es_template = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER-LST_CH_USER_ACCT'
      OF STRUCTURE <lg_template>
      TO <lv_last_changed_user>.
    ASSERT sy-subrc = 0.

    rv_user = <lv_last_changed_user>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>delete
          EXPORTING
            iv_id = mv_template_id.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root,
      lo_template TYPE REF TO object.

    FIELD-SYMBOLS:
      <lg_template>    TYPE data,
      <lg_header>      TYPE data,
      <lt_header>      TYPE INDEX TABLE,
      <lt_content>     TYPE INDEX TABLE,
      <lg_name>        TYPE data,
      <lg_description> TYPE data,
      <lg_header_text> TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'SMTG'
      CHANGING
        cg_data = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER_T'
      OF STRUCTURE <lg_template>
      TO <lt_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'CONTENT'
      OF STRUCTURE <lg_template>
      TO <lt_content>.
    ASSERT sy-subrc = 0.

    TRY.
        IF zif_abapgit_object~exists( ) = abap_true.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
            EXPORTING
              iv_id       = mv_template_id
            RECEIVING
              ro_instance = lo_template.
        ELSE.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>create
            EXPORTING
              is_tmpl_hdr       = <lg_header>
            RECEIVING
              ro_email_template = lo_template.
        ENDIF.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SET_TMPL_CONT_ALL')
          EXPORTING
            it_tmpl_cont = <lt_content>.

        READ TABLE <lt_header> ASSIGNING <lg_header_text>
                               INDEX 1.
        IF sy-subrc = 0.
          ASSIGN
            COMPONENT 'NAME'
            OF STRUCTURE <lg_header_text>
            TO <lg_name>.
          ASSERT sy-subrc = 0.

          ASSIGN
            COMPONENT 'DESCRIPTION'
            OF STRUCTURE <lg_header_text>
            TO <lg_description>.
          ASSERT sy-subrc = 0.

          CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SET_TEXT')
            EXPORTING
              iv_name        = <lg_name>
              iv_description = <lg_description>.
        ENDIF.

        tadir_insert( iv_package ).
        corr_insert( iv_package ).

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SAVE')
          EXPORTING
            iv_lock   = abap_true
            iv_commit = abap_true
            iv_wait   = abap_true.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
          EXPORTING
            iv_id = mv_template_id.

        rv_bool = abap_true.

      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMTG'
                                            iv_argument    = |{ mv_template_id }| ).
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

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lg_template> TYPE data,
      <lg_header>   TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    get_template( IMPORTING es_template = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    TRY.
        clear_field( EXPORTING iv_fieldname = 'CREA_DATE_TIME'   CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'CREA_USER_ACCT'   CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'LST_CH_DATE_TIME' CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'LST_CH_USER_ACCT' CHANGING cg_header = <lg_header> ).

        io_xml->add(
            iv_name = 'SMTG'
            ig_data = <lg_template> ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
