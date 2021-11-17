*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_chko_persistence DEFINITION
  FINAL.

  PUBLIC SECTION.
    METHODS:
      get_content
        IMPORTING object   TYPE trkey
                  language TYPE spras
                  version  TYPE r3state
        EXPORTING data     TYPE data
        RAISING   cx_static_check,

      save_content
        IMPORTING data     TYPE data
                  object   TYPE trkey
                  language TYPE spras
                  version  TYPE r3state
                  saved_by TYPE as4user
        RAISING   cx_static_check.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_chko_persistence IMPLEMENTATION.

  METHOD get_content.
    DATA master_language TYPE tadir-masterlang.

    DATA  lr_chko         TYPE REF TO data.
    DATA  lr_header       TYPE REF TO data.
    DATA  lr_header_agit  TYPE REF TO data.
    DATA  lr_content      TYPE REF TO data.
    DATA  lr_content_agit TYPE REF TO data.
    DATA  chko_db_api     TYPE REF TO object.

    DATA  lr_chko_params  TYPE REF TO data.
    DATA  lr_chko_param   TYPE REF TO data.
    DATA  lr_chko_params_agit TYPE REF TO data.
    DATA  lr_chko_param_agit  TYPE REF TO data.
    DATA  chko_name TYPE c LENGTH 30.


    FIELD-SYMBOLS <chko_agit>    TYPE any.

    FIELD-SYMBOLS <chko_header>  TYPE any.
    FIELD-SYMBOLS <chko_content> TYPE any.

    FIELD-SYMBOLS <chko_header_agit>  TYPE any.
    FIELD-SYMBOLS <chko_content_agit> TYPE any.

    FIELD-SYMBOLS <fs_struct_categ>      TYPE any.
    FIELD-SYMBOLS <fs_struct_impl_class> TYPE any.
    FIELD-SYMBOLS <fs_descr>             TYPE any.

    FIELD-SYMBOLS <fs_format_vers>       TYPE string.
    FIELD-SYMBOLS <fs_orig_langu>        TYPE sy-langu.

    FIELD-SYMBOLS <fs_category>      TYPE any.
    FIELD-SYMBOLS <fs_category_agit> TYPE any.

    FIELD-SYMBOLS <fs_impl_class>      TYPE any.
    FIELD-SYMBOLS <fs_impl_class_agit> TYPE any.

    FIELD-SYMBOLS <fs_remote>       TYPE flag.
    FIELD-SYMBOLS <fs_remote_agit>  TYPE flag.

    FIELD-SYMBOLS <fs_tech_id>      TYPE any.
    FIELD-SYMBOLS <fs_tech_id_agit> TYPE any.

    FIELD-SYMBOLS <fs_modifiable_param>  TYPE abap_bool.
    FIELD-SYMBOLS <fs_hidden_param_agit> TYPE abap_bool.

    FIELD-SYMBOLS <fs_name_param>        TYPE any.
    FIELD-SYMBOLS <fs_name_param_agit>   TYPE any.

    FIELD-SYMBOLS <fs_descr_param>       TYPE any.
    FIELD-SYMBOLS <fs_descr_param_agit>  TYPE any.

    FIELD-SYMBOLS <fs_params_chko> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_param_chko>  TYPE any.

    FIELD-SYMBOLS <fs_params_agit> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_param_agit>  TYPE any.

    CLEAR data.

    CHECK object-obj_type = 'CHKO'.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT chko_db_api TYPE ('CL_CHKO_DB_API').

    IF chko_db_api IS NOT BOUND.
      RETURN.  " chko object does not exist here
    ENDIF.

    CREATE DATA lr_header TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_header->* TO <chko_header>.
    ASSERT sy-subrc = 0.

    chko_name = object-obj_name.

    SELECT SINGLE masterlang FROM tadir INTO master_language
      WHERE pgmid = 'R3TR' AND object = 'CHKO' AND obj_name = chko_name.

    CALL METHOD chko_db_api->('GET_HEADER')
      EXPORTING
        name     = chko_name
        version  = version
        language = language
      RECEIVING
        header   = <chko_header>.

    CREATE DATA lr_content TYPE ('CL_CHKO_DB_API=>TY_CONTENT').
    ASSIGN lr_content->* TO <chko_content>.
    ASSERT sy-subrc = 0.

    CALL METHOD chko_db_api->('GET_CONTENT')
      EXPORTING
        name     = chko_name
        version  = version
        language = language
      RECEIVING
        content  = <chko_content>.


    ASSIGN COMPONENT 'FORMAT_VERSION' OF STRUCTURE <chko_agit> TO <fs_format_vers>.
    <fs_format_vers> = '1'.

    CREATE DATA lr_header_agit TYPE ('ZIF_ABAPGIT_AFF_TYPES_V1=>TY_HEADER_60').
    ASSIGN lr_header_agit->* TO <chko_header_agit>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <chko_agit> TO <chko_header_agit>.

    ASSIGN COMPONENT 'ORIGINAL_LANGUAGE' OF STRUCTURE <chko_header_agit> TO <fs_orig_langu>.
    <fs_orig_langu> = master_language.

    MOVE-CORRESPONDING <chko_header> TO <chko_header_agit>.

    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <chko_agit>    TO <fs_category_agit>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <chko_content> TO <fs_struct_categ>.
    ASSIGN COMPONENT 'NAME'     OF STRUCTURE <fs_struct_categ> TO <fs_category>.
    <fs_category_agit> = <fs_category>.

    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <chko_agit>    TO <fs_impl_class_agit>.
    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <chko_content> TO <fs_struct_impl_class>.
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_struct_impl_class>       TO <fs_impl_class>.
    <fs_impl_class_agit> = <fs_impl_class>.

    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <chko_agit>    TO <fs_remote_agit>.
    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <chko_content> TO <fs_remote>.
    <fs_remote_agit> = <fs_remote>.

    CREATE DATA lr_chko_params TYPE ('CL_CHKO_DB_API=>TY_PARAMETERS').
    ASSIGN lr_chko_params->* TO <fs_params_chko>.
    CREATE DATA lr_chko_param TYPE ('CL_CHKO_DB_API=>TY_PARAMETER').
    ASSIGN lr_chko_param->* TO <fs_param_chko>.

    CREATE DATA lr_chko_params_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETERS').
    ASSIGN lr_chko_params_agit->* TO <fs_params_agit>.

    CREATE DATA lr_chko_param_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETER').
    ASSIGN lr_chko_param_agit->* TO <fs_param_agit>.

    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <chko_content> TO <fs_params_chko>.
    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <chko_agit>    TO <fs_params_agit>.

    LOOP AT <fs_params_chko> ASSIGNING <fs_param_chko>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <fs_param_agit> TO <fs_tech_id_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <fs_param_chko> TO <fs_tech_id>.
      <fs_tech_id_agit> = <fs_tech_id>.

      ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_param_agit> TO <fs_name_param_agit>.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_param_chko> TO <fs_name_param>.
      <fs_name_param_agit> = <fs_name_param>.

      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_param_agit> TO <fs_descr_param_agit>.
      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_param_chko> TO <fs_descr_param>.
      <fs_descr_param_agit> = <fs_descr_param>.

      ASSIGN COMPONENT 'HIDDEN'     OF STRUCTURE <fs_param_agit> TO <fs_hidden_param_agit>.
      ASSIGN COMPONENT 'MODIFIABLE' OF STRUCTURE <fs_param_chko> TO <fs_modifiable_param>.
      <fs_hidden_param_agit> = <fs_modifiable_param>.

      INSERT <fs_param_agit> INTO TABLE <fs_params_agit>.
    ENDLOOP.

    data = <chko_agit>.
  ENDMETHOD.

  METHOD save_content.

    DATA master_language TYPE tadir-masterlang.

    DATA  lr_chko         TYPE REF TO data.
    DATA  lr_header       TYPE REF TO data.
    DATA  lr_header_agit  TYPE REF TO data.
    DATA  lr_content      TYPE REF TO data.
    DATA  lr_content_agit TYPE REF TO data.
    DATA  chko_db_api     TYPE REF TO object.

    DATA  lr_chko_params  TYPE REF TO data.
    DATA  lr_chko_param   TYPE REF TO data.
    DATA  lr_chko_params_agit TYPE REF TO data.
    DATA  lr_chko_param_agit  TYPE REF TO data.
    DATA  chko_name TYPE c LENGTH 30.

    FIELD-SYMBOLS <chko_agit>    TYPE any.
    FIELD-SYMBOLS <chko_name>    TYPE any.
    FIELD-SYMBOLS <chko_created_by>    TYPE any.
    FIELD-SYMBOLS <chko_changed_by>    TYPE any.
    FIELD-SYMBOLS <chko_created_at>    TYPE any.
    FIELD-SYMBOLS <chko_changed_at>    TYPE any.
    FIELD-SYMBOLS <chko_version>       TYPE any.
    FIELD-SYMBOLS <chko_header>  TYPE any.
    FIELD-SYMBOLS <chko_content> TYPE any.

    FIELD-SYMBOLS <chko_header_agit>  TYPE any.
    FIELD-SYMBOLS <chko_content_agit> TYPE any.

    FIELD-SYMBOLS <fs_struct_categ>      TYPE any.
    FIELD-SYMBOLS <fs_struct_impl_class> TYPE any.
    FIELD-SYMBOLS <fs_descr>             TYPE any.

    FIELD-SYMBOLS <fs_format_vers>       TYPE string.
    FIELD-SYMBOLS <fs_orig_langu>        TYPE sy-langu.

    FIELD-SYMBOLS <fs_category>      TYPE any.
    FIELD-SYMBOLS <fs_category_agit> TYPE any.

    FIELD-SYMBOLS <fs_impl_class>      TYPE any.
    FIELD-SYMBOLS <fs_impl_class_agit> TYPE any.

    FIELD-SYMBOLS <fs_remote>       TYPE flag.
    FIELD-SYMBOLS <fs_remote_agit>  TYPE flag.

    FIELD-SYMBOLS <fs_tech_id>      TYPE any.
    FIELD-SYMBOLS <fs_tech_id_agit> TYPE any.

    FIELD-SYMBOLS <fs_modifiable_param>  TYPE abap_bool.
    FIELD-SYMBOLS <fs_hidden_param_agit> TYPE abap_bool.

    FIELD-SYMBOLS <fs_name_param>      TYPE any.
    FIELD-SYMBOLS <fs_name_param_agit> TYPE any.

    FIELD-SYMBOLS <fs_descr_param>       TYPE any.
    FIELD-SYMBOLS <fs_descr_param_agit>  TYPE any.

    FIELD-SYMBOLS <fs_params_chko> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_param_chko>  TYPE any.

    FIELD-SYMBOLS <fs_params_agit> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_param_agit>  TYPE any.

    CHECK object-obj_type = 'CHKO'.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT chko_db_api TYPE ('CL_CHKO_DB_API').

    IF chko_db_api IS NOT BOUND.
      RETURN.  " chko object does not exist here
    ENDIF.

    CREATE DATA lr_header TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_header->* TO <chko_header>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_content TYPE ('CL_CHKO_DB_API=>TY_CONTENT').
    ASSIGN lr_content->* TO <chko_content>.
    ASSERT sy-subrc = 0.

    chko_name = object-obj_name.

    <chko_agit> = data.

    CALL METHOD chko_db_api->('GET_HEADER')
      EXPORTING
        name     = chko_name
        version  = version
        language = language
      RECEIVING
        header   = <chko_header>.
    IF <chko_header> IS INITIAL AND version = 'I'.
      CALL METHOD chko_db_api->('GET_HEADER')
        EXPORTING
          name     = chko_name
          version  = 'A'
          language = language
        RECEIVING
          header   = <chko_header>.
    ENDIF.

    IF <chko_header> IS INITIAL.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <chko_header> TO <chko_name>.
      <chko_name> = chko_name.
      ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <chko_header> TO <chko_created_by>.
      <chko_created_by> = saved_by.
      ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <chko_header> TO <chko_created_at>.
      GET TIME STAMP FIELD <chko_created_at>.
    ENDIF.

    CREATE DATA lr_header_agit TYPE ('ZIF_ABAPGIT_AFF_TYPES_V1=>TY_HEADER_60').
    ASSIGN lr_header_agit->* TO <chko_header_agit>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <chko_agit> TO <chko_header_agit>.
    IF <chko_header_agit> IS NOT INITIAL.
      MOVE-CORRESPONDING <chko_header_agit> TO <chko_header>.
    ENDIF.

    ASSIGN COMPONENT 'VERSION' OF STRUCTURE <chko_header> TO <chko_version>.
    <chko_version> = version.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <chko_header> TO <chko_changed_by>.
    <chko_changed_by> = saved_by.
    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <chko_header> TO <chko_changed_at>.
    GET TIME STAMP FIELD <chko_changed_at>.

    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <chko_agit>    TO <fs_category_agit>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <chko_content> TO <fs_struct_categ>.
    ASSIGN COMPONENT 'NAME'     OF STRUCTURE <fs_struct_categ> TO <fs_category>.
    <fs_category> = <fs_category_agit>.

    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <chko_agit>    TO <fs_impl_class_agit>.
    ASSIGN COMPONENT 'IMPLEMENTING_CLASS' OF STRUCTURE <chko_content> TO <fs_struct_impl_class>.
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_struct_impl_class>       TO <fs_impl_class>.
    <fs_impl_class> = <fs_impl_class_agit>.

    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <chko_agit>    TO <fs_remote_agit>.
    ASSIGN COMPONENT 'REMOTE_ENABLED' OF STRUCTURE <chko_content> TO <fs_remote>.
    <fs_remote> =  <fs_remote_agit>.

    CREATE DATA lr_chko_params TYPE ('CL_CHKO_DB_API=>TY_PARAMETERS').
    ASSIGN lr_chko_params->* TO <fs_params_chko>.
    CREATE DATA lr_chko_param TYPE ('CL_CHKO_DB_API=>TY_PARAMETER').
    ASSIGN lr_chko_param->* TO <fs_param_chko>.

    CREATE DATA lr_chko_params_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETERS').
    ASSIGN lr_chko_params_agit->* TO <fs_params_agit>.

    CREATE DATA lr_chko_param_agit TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_PARAMETER').
    ASSIGN lr_chko_param_agit->* TO <fs_param_agit>.

    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <chko_content> TO <fs_params_chko>.
    ASSIGN COMPONENT 'PARAMETERS' OF STRUCTURE <chko_agit>    TO <fs_params_agit>.

    LOOP AT <fs_params_agit> ASSIGNING <fs_param_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <fs_param_agit> TO <fs_tech_id_agit>.
      ASSIGN COMPONENT 'TECHNICAL_ID' OF STRUCTURE <fs_param_chko> TO <fs_tech_id>.
      <fs_tech_id> = <fs_tech_id_agit>.

      ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_param_agit> TO <fs_name_param_agit>.
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_param_chko> TO <fs_name_param>.
      <fs_name_param> = <fs_name_param_agit>.

      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_param_agit> TO <fs_descr_param_agit>.
      ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_param_chko> TO <fs_descr_param>.
      <fs_descr_param> = <fs_descr_param_agit>.

      ASSIGN COMPONENT 'HIDDEN'     OF STRUCTURE <fs_param_agit> TO <fs_hidden_param_agit>.
      ASSIGN COMPONENT 'MODIFIABLE' OF STRUCTURE <fs_param_chko> TO <fs_modifiable_param>.
      <fs_modifiable_param> = <fs_hidden_param_agit>.

      INSERT <fs_param_chko> INTO TABLE <fs_params_chko>.
    ENDLOOP.

    CALL METHOD chko_db_api->('UPDATE')
      EXPORTING
        header  = <chko_header>
        content = <chko_content>.

  ENDMETHOD.

ENDCLASS.
