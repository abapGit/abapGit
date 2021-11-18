*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_chko_persistence DEFINITION
  FINAL.

  PUBLIC SECTION.
    METHODS:
      get_content
        IMPORTING iv_object   TYPE trkey
                  iv_language TYPE spras
                  iv_version  TYPE r3state
        EXPORTING ev_data     TYPE data
        RAISING   cx_static_check,

      save_content
        IMPORTING iv_data     TYPE data
                  iv_object   TYPE trkey
                  iv_language TYPE spras
                  iv_version  TYPE r3state
                  iv_saved_by TYPE as4user
        RAISING   cx_static_check.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_chko_persistence IMPLEMENTATION.

  METHOD get_content.
    DATA  lv_master_language TYPE tadir-masterlang.
    DATA  lr_chko            TYPE REF TO data.
    DATA  lr_header          TYPE REF TO data.
    DATA  lr_header_agit     TYPE REF TO data.
    DATA  lr_content         TYPE REF TO data.
    DATA  lr_content_agit    TYPE REF TO data.
    DATA  lo_chko_db_api     TYPE REF TO object.
    DATA  lr_chko_params     TYPE REF TO data.
    DATA  lr_chko_param      TYPE REF TO data.

    DATA  lr_chko_params_agit TYPE REF TO data.
    DATA  lr_chko_param_agit  TYPE REF TO data.
    DATA  lv_chko_name        TYPE c LENGTH 30.

    FIELD-SYMBOLS <lg_chko_agit>    TYPE any.
    FIELD-SYMBOLS <lg_chko_header>  TYPE any.
    FIELD-SYMBOLS <lg_chko_content> TYPE any.

    FIELD-SYMBOLS <lg_chko_header_agit>  TYPE any.
    FIELD-SYMBOLS <lg_chko_content_agit> TYPE any.

    FIELD-SYMBOLS <lg_struct_categ>      TYPE any.
    FIELD-SYMBOLS <lg_struct_impl_class> TYPE any.
    FIELD-SYMBOLS <lg_descr>             TYPE any.
    FIELD-SYMBOLS <lg_format_vers>       TYPE string.
    FIELD-SYMBOLS <lg_orig_langu>        TYPE sy-langu.

    FIELD-SYMBOLS <lg_category>      TYPE any.
    FIELD-SYMBOLS <lg_category_agit> TYPE any.

    FIELD-SYMBOLS <lg_impl_class>      TYPE any.
    FIELD-SYMBOLS <lg_impl_class_agit> TYPE any.

    FIELD-SYMBOLS <lg_remote>       TYPE any.
    FIELD-SYMBOLS <lg_remote_agit>  TYPE any.

    FIELD-SYMBOLS <lg_tech_id>      TYPE any.
    FIELD-SYMBOLS <lg_tech_id_agit> TYPE any.

    FIELD-SYMBOLS <lg_modifiable_param>  TYPE abap_bool.
    FIELD-SYMBOLS <lg_hidden_param_agit> TYPE abap_bool.

    FIELD-SYMBOLS <lg_name_param>        TYPE any.
    FIELD-SYMBOLS <lg_name_param_agit>   TYPE any.

    FIELD-SYMBOLS <lg_descr_param>       TYPE any.
    FIELD-SYMBOLS <lg_descr_param_agit>  TYPE any.

    FIELD-SYMBOLS <lt_params_chko> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_param_chko>  TYPE any.

    FIELD-SYMBOLS <lt_params_agit> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_param_agit>  TYPE any.

    CLEAR ev_data.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').

    IF lo_chko_db_api IS NOT BOUND.
      RETURN.  " chko object does not exist here
    ENDIF.

    CREATE DATA lr_header TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_header->* TO <lg_chko_header>.
    ASSERT sy-subrc = 0.

    lv_chko_name = iv_object-obj_name.

    SELECT SINGLE masterlang FROM tadir INTO lv_master_language
      WHERE pgmid = 'R3TR' AND object = 'CHKO' AND obj_name = lv_chko_name.

    CALL METHOD lo_chko_db_api->('GET_HEADER')
      EXPORTING
        name     = lv_chko_name
        version  = iv_version
        language = iv_language
      RECEIVING
        header   = <lg_chko_header>.

    CREATE DATA lr_content TYPE ('CL_CHKO_DB_API=>TY_CONTENT').
    ASSIGN lr_content->* TO <lg_chko_content>.
    ASSERT sy-subrc = 0.

    CALL METHOD lo_chko_db_api->('GET_CONTENT')
      EXPORTING
        name     = lv_chko_name
        version  = iv_version
        language = iv_language
      RECEIVING
        content  = <lg_chko_content>.


    ASSIGN COMPONENT 'FORMAT_VERSION' OF STRUCTURE <lg_chko_agit> TO <lg_format_vers>.
    <lg_format_vers> = '1'.

    CREATE DATA lr_header_agit TYPE ('ZIF_ABAPGIT_AFF_TYPES_V1=>TY_HEADER_60').
    ASSIGN lr_header_agit->* TO <lg_chko_header_agit>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_chko_agit> TO <lg_chko_header_agit>.

    ASSIGN COMPONENT 'ORIGINAL_LANGUAGE' OF STRUCTURE <lg_chko_header_agit> TO <lg_orig_langu>.
    <lg_orig_langu> = lv_master_language.

    MOVE-CORRESPONDING <lg_chko_header> TO <lg_chko_header_agit>.

    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <lg_chko_agit>    TO <lg_category_agit>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <lg_chko_content> TO <lg_struct_categ>.
    ASSIGN COMPONENT 'NAME'     OF STRUCTURE <lg_struct_categ> TO <lg_category>.
    <lg_category_agit> = <lg_category>.

    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <lg_chko_agit>    TO <lg_impl_class_agit>.
    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <lg_chko_content> TO <lg_struct_impl_class>.
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_struct_impl_class>       TO <lg_impl_class>.
    <lg_impl_class_agit> = <lg_impl_class>.

    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <lg_chko_agit>    TO <lg_remote_agit>.
    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <lg_chko_content> TO <lg_remote>.
    <lg_remote_agit> = <lg_remote>.

    CREATE DATA lr_chko_params TYPE ('CL_CHKO_DB_API=>TY_PARAMETERS').
    ASSIGN lr_chko_params->* TO <lt_params_chko>.
    CREATE DATA lr_chko_param TYPE ('CL_CHKO_DB_API=>TY_PARAMETER').
    ASSIGN lr_chko_param->* TO <lg_param_chko>.

    CREATE DATA lr_chko_params_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETERS').
    ASSIGN lr_chko_params_agit->* TO <lt_params_agit>.

    CREATE DATA lr_chko_param_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETER').
    ASSIGN lr_chko_param_agit->* TO <lg_param_agit>.

    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <lg_chko_content> TO <lt_params_chko>.
    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <lg_chko_agit>    TO <lt_params_agit>.

    LOOP AT <lt_params_chko> ASSIGNING <lg_param_chko>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <lg_param_agit> TO <lg_tech_id_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <lg_param_chko> TO <lg_tech_id>.
      <lg_tech_id_agit> = <lg_tech_id>.

      ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_param_agit> TO <lg_name_param_agit>.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_param_chko> TO <lg_name_param>.
      <lg_name_param_agit> = <lg_name_param>.

      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <lg_param_agit> TO <lg_descr_param_agit>.
      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <lg_param_chko> TO <lg_descr_param>.
      <lg_descr_param_agit> = <lg_descr_param>.

      ASSIGN COMPONENT 'HIDDEN'     OF STRUCTURE <lg_param_agit> TO <lg_hidden_param_agit>.
      ASSIGN COMPONENT 'MODIFIABLE' OF STRUCTURE <lg_param_chko> TO <lg_modifiable_param>.
      <lg_hidden_param_agit> = <lg_modifiable_param>.

      INSERT <lg_param_agit> INTO TABLE <lt_params_agit>.
    ENDLOOP.

    ev_data = <lg_chko_agit>.
  ENDMETHOD.

  METHOD save_content.

    DATA  lr_chko         TYPE REF TO data.
    DATA  lr_header       TYPE REF TO data.
    DATA  lr_header_agit  TYPE REF TO data.
    DATA  lr_content      TYPE REF TO data.
    DATA  lr_content_agit TYPE REF TO data.
    DATA  lo_chko_db_api  TYPE REF TO object.

    DATA  lr_chko_params      TYPE REF TO data.
    DATA  lr_chko_param       TYPE REF TO data.
    DATA  lr_chko_params_agit TYPE REF TO data.
    DATA  lr_chko_param_agit  TYPE REF TO data.
    DATA  lv_chko_name        TYPE c LENGTH 30.

    FIELD-SYMBOLS <lg_chko_agit>          TYPE any.
    FIELD-SYMBOLS <lg_chko_name>          TYPE any.
    FIELD-SYMBOLS <lg_chko_created_by>    TYPE any.
    FIELD-SYMBOLS <lg_chko_changed_by>    TYPE any.
    FIELD-SYMBOLS <lg_chko_created_at>    TYPE any.
    FIELD-SYMBOLS <lg_chko_changed_at>    TYPE any.
    FIELD-SYMBOLS <lg_chko_version>       TYPE any.
    FIELD-SYMBOLS <lg_chko_header>        TYPE any.
    FIELD-SYMBOLS <lg_chko_content>       TYPE any.

    FIELD-SYMBOLS <lg_chko_header_agit>  TYPE any.
    FIELD-SYMBOLS <lg_chko_content_agit> TYPE any.

    FIELD-SYMBOLS <lg_struct_categ>      TYPE any.
    FIELD-SYMBOLS <lg_struct_impl_class> TYPE any.
    FIELD-SYMBOLS <lg_descr>             TYPE any.

    FIELD-SYMBOLS <lg_format_vers>       TYPE string.
    FIELD-SYMBOLS <lg_orig_langu>        TYPE sy-langu.

    FIELD-SYMBOLS <lg_category>      TYPE any.
    FIELD-SYMBOLS <lg_category_agit> TYPE any.

    FIELD-SYMBOLS <lg_impl_class>      TYPE any.
    FIELD-SYMBOLS <lg_impl_class_agit> TYPE any.

    FIELD-SYMBOLS <lg_remote>       TYPE any.
    FIELD-SYMBOLS <lg_remote_agit>  TYPE any.

    FIELD-SYMBOLS <lg_tech_id>      TYPE any.
    FIELD-SYMBOLS <lg_tech_id_agit> TYPE any.

    FIELD-SYMBOLS <lg_modifiable_param>  TYPE abap_bool.
    FIELD-SYMBOLS <lg_hidden_param_agit> TYPE abap_bool.

    FIELD-SYMBOLS <lg_name_param>      TYPE any.
    FIELD-SYMBOLS <lg_name_param_agit> TYPE any.

    FIELD-SYMBOLS <lg_descr_param>       TYPE any.
    FIELD-SYMBOLS <lg_descr_param_agit>  TYPE any.

    FIELD-SYMBOLS <lt_params_chko> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_param_chko>  TYPE any.

    FIELD-SYMBOLS <lt_params_agit> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_param_agit>  TYPE any.


    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').

    IF lo_chko_db_api IS NOT BOUND.
      RETURN.  " chko object does not exist here
    ENDIF.

    CREATE DATA lr_header TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_header->* TO <lg_chko_header>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_content TYPE ('CL_CHKO_DB_API=>TY_CONTENT').
    ASSIGN lr_content->* TO <lg_chko_content>.
    ASSERT sy-subrc = 0.

    lv_chko_name = iv_object-obj_name.

    <lg_chko_agit> = iv_data.

    CALL METHOD lo_chko_db_api->('GET_HEADER')
      EXPORTING
        name     = lv_chko_name
        version  = iv_version
        language = iv_language
      RECEIVING
        header   = <lg_chko_header>.

    IF <lg_chko_header> IS INITIAL AND iv_version = 'I'.
      CALL METHOD lo_chko_db_api->('GET_HEADER')
        EXPORTING
          name     = lv_chko_name
          version  = 'A'
          language = iv_language
        RECEIVING
          header   = <lg_chko_header>.
    ENDIF.

    IF <lg_chko_header> IS INITIAL.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_chko_header> TO <lg_chko_name>.
      <lg_chko_name> = lv_chko_name.
      ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_chko_header> TO <lg_chko_created_by>.
      <lg_chko_created_by> = iv_saved_by.
      ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <lg_chko_header> TO <lg_chko_created_at>.
      GET TIME STAMP FIELD <lg_chko_created_at>.
    ENDIF.

    CREATE DATA lr_header_agit TYPE ('ZIF_ABAPGIT_AFF_TYPES_V1=>TY_HEADER_60').
    ASSIGN lr_header_agit->* TO <lg_chko_header_agit>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_chko_agit> TO <lg_chko_header_agit>.
    IF <lg_chko_header_agit> IS NOT INITIAL.
      MOVE-CORRESPONDING <lg_chko_header_agit> TO <lg_chko_header>.
    ENDIF.

    ASSIGN COMPONENT 'VERSION' OF STRUCTURE <lg_chko_header> TO <lg_chko_version>.
    <lg_chko_version> = iv_version.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_chko_header> TO <lg_chko_changed_by>.
    <lg_chko_changed_by> = iv_saved_by.
    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <lg_chko_header> TO <lg_chko_changed_at>.
    GET TIME STAMP FIELD <lg_chko_changed_at>.

    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <lg_chko_agit>    TO <lg_category_agit>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <lg_chko_content> TO <lg_struct_categ>.
    ASSIGN COMPONENT 'NAME'     OF STRUCTURE <lg_struct_categ> TO <lg_category>.
    <lg_category> = <lg_category_agit>.

    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <lg_chko_agit>    TO <lg_impl_class_agit>.
    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <lg_chko_content> TO <lg_struct_impl_class>.
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_struct_impl_class>       TO <lg_impl_class>.
    <lg_impl_class> = <lg_impl_class_agit>.

    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <lg_chko_agit>    TO <lg_remote_agit>.
    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <lg_chko_content> TO <lg_remote>.
    <lg_remote> =  <lg_remote_agit>.

    CREATE DATA lr_chko_params TYPE ('CL_CHKO_DB_API=>TY_PARAMETERS').
    ASSIGN lr_chko_params->* TO <lt_params_chko>.
    CREATE DATA lr_chko_param TYPE ('CL_CHKO_DB_API=>TY_PARAMETER').
    ASSIGN lr_chko_param->* TO <lg_param_chko>.

    CREATE DATA lr_chko_params_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETERS').
    ASSIGN lr_chko_params_agit->* TO <lt_params_agit>.

    CREATE DATA lr_chko_param_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETER').
    ASSIGN lr_chko_param_agit->* TO <lg_param_agit>.

    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <lg_chko_content> TO <lt_params_chko>.
    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <lg_chko_agit>    TO <lt_params_agit>.

    LOOP AT <lt_params_agit> ASSIGNING <lg_param_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <lg_param_agit> TO <lg_tech_id_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <lg_param_chko> TO <lg_tech_id>.
      <lg_tech_id> = <lg_tech_id_agit>.

      ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_param_agit> TO <lg_name_param_agit>.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_param_chko> TO <lg_name_param>.
      <lg_name_param> = <lg_name_param_agit>.

      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <lg_param_agit> TO <lg_descr_param_agit>.
      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <lg_param_chko> TO <lg_descr_param>.
      <lg_descr_param> = <lg_descr_param_agit>.

      ASSIGN COMPONENT 'HIDDEN'     OF STRUCTURE <lg_param_agit> TO <lg_hidden_param_agit>.
      ASSIGN COMPONENT 'MODIFIABLE' OF STRUCTURE <lg_param_chko> TO <lg_modifiable_param>.
      <lg_modifiable_param> = <lg_hidden_param_agit>.

      INSERT <lg_param_chko> INTO TABLE <lt_params_chko>.
    ENDLOOP.

    CALL METHOD lo_chko_db_api->('UPDATE')
      EXPORTING
        header  = <lg_chko_header>
        content = <lg_chko_content>.

  ENDMETHOD.

ENDCLASS.
