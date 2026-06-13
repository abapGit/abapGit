CLASS zcl_abapgit_object_smtg DEFINITION PUBLIC INHERITING FROM zcl_abapgit_object_common_aff FINAL.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
    METHODS zif_abapgit_object~deserialize REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_additional_extensions REDEFINITION.

  PRIVATE SECTION.
    DATA mv_template_id TYPE c LENGTH 30.
    DATA mo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA mv_xml_modus_on TYPE abap_bool.

    METHODS get_structure
      RETURNING VALUE(ro_structdescr) TYPE REF TO cl_abap_structdescr
      RAISING   zcx_abapgit_exception.

    METHODS add_component
      IMPORTING iv_fielname       TYPE string
                iv_structure_name TYPE string
      CHANGING  ct_components     TYPE abap_component_tab
      RAISING   zcx_abapgit_exception.

ENDCLASS.


CLASS zcl_abapgit_object_smtg IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    DATA: lo_email_template_api TYPE REF TO object,
          lv_email_template_id  TYPE c LENGTH 30,
          lx_error              TYPE REF TO cx_root,
          ls_template_header    TYPE REF TO data.


    FIELD-SYMBOLS: <lg_template_header>     TYPE any,
                   <lg_template_changed_by> TYPE any.

    lv_email_template_id = ms_item-obj_name.
    TRY.
        IF ms_item-inactive = abap_true.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
            EXPORTING
              iv_id       = lv_email_template_id
              iv_version  = 'I'
            RECEIVING
              ro_instance = lo_email_template_api.
        ELSE.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
            EXPORTING
              iv_id       = lv_email_template_id
              iv_version  = 'A'
            RECEIVING
              ro_instance = lo_email_template_api.
        ENDIF.

        CREATE DATA ls_template_header TYPE ('IF_SMTG_EMAIL_TEMPLATE=>TY_GS_TMPL_HDR').
        ASSIGN ls_template_header->* TO <lg_template_header>.
        CALL METHOD lo_email_template_api->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_HDR')
          RECEIVING
            rs_tmpl_hdr = <lg_template_header>.
        ASSIGN COMPONENT 'LST_CH_USER_ACCT' OF STRUCTURE <lg_template_header> TO <lg_template_changed_by>.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_user = <lg_template_changed_by>.

  ENDMETHOD.

  METHOD get_additional_extensions.
    DATA ls_additional_extension   LIKE LINE OF rv_additional_extensions.
    DATA lo_file_name_pattern TYPE REF TO object.
    DATA lo_aff_file_name_mapper   TYPE REF TO object.
    DATA lx_error TYPE REF TO cx_root.
    DATA lr_pattern_param TYPE REF TO data.
    FIELD-SYMBOLS <lg_pattern_param> TYPE any.

    IF mv_xml_modus_on = abap_true.
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA lr_pattern_param TYPE REF TO ('CL_AFF_FILE_NAME_PATTERN').
        ASSIGN lr_pattern_param->* TO <lg_pattern_param>.

        CALL METHOD ('CL_AFF_FILE_NAME_PATTERN')=>('FOR_OBJ_NAME')
          RECEIVING
            result = lo_file_name_pattern.
        CALL METHOD lo_file_name_pattern->('APPEND_LANGUAGE').
        CALL METHOD lo_file_name_pattern->('APPEND')
          EXPORTING
            fragment = 'html'.

        <lg_pattern_param> ?= lo_file_name_pattern.
        CREATE OBJECT lo_aff_file_name_mapper TYPE ('CL_AFF_FILE_NAME_MAPPER')
          EXPORTING pattern = <lg_pattern_param>.
        ls_additional_extension-file_name_mapper = lo_aff_file_name_mapper.
        ls_additional_extension-extension        = 'html'.
        APPEND ls_additional_extension TO rv_additional_extensions.
        CLEAR ls_additional_extension.


        CALL METHOD ('CL_AFF_FILE_NAME_PATTERN')=>('FOR_OBJ_NAME')
          RECEIVING
            result = lo_file_name_pattern.
        CALL METHOD lo_file_name_pattern->('APPEND_LANGUAGE').
        CALL METHOD lo_file_name_pattern->('APPEND')
          EXPORTING
            fragment = 'txt'.
        <lg_pattern_param> ?= lo_file_name_pattern.
        CREATE OBJECT lo_aff_file_name_mapper TYPE ('CL_AFF_FILE_NAME_MAPPER')
          EXPORTING pattern = <lg_pattern_param>.
        ls_additional_extension-file_name_mapper = lo_aff_file_name_mapper.
        ls_additional_extension-extension        = 'txt'.
        APPEND ls_additional_extension TO rv_additional_extensions.
      CATCH cx_root INTO lx_error    ##NO_HANDLER.
    ENDTRY.
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

    IF mo_files->is_json_metadata( ) = abap_true.
      "If the email template is in AFF format, use the standard deserialization
      super->zif_abapgit_object~deserialize(
      iv_package = iv_package
      iv_transport = iv_transport
      iv_step = iv_step
      ii_log = ii_log
      io_xml     = io_xml ).
      RETURN.
    ELSE.
      mv_xml_modus_on = abap_true.
    ENDIF.


    mo_structdescr = get_structure( ).
    mv_template_id = ms_item-obj_name.

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
      RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = ms_item-obj_type.
    ENDIF.

    ls_component-name = iv_fielname.
    ls_component-type ?= lo_typedescr.
    INSERT ls_component INTO TABLE ct_components.

  ENDMETHOD.

ENDCLASS.
