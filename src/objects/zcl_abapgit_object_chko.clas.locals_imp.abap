*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_chko_persistence DEFINITION
  FINAL.

  PUBLIC SECTION.

    TYPES: ty_chko TYPE zif_abagit_aff_chko_v1=>ty_main.

    METHODS:
      get_content
        IMPORTING object   TYPE trkey
                  language TYPE spras
                  version  TYPE r3state
        EXPORTING data     TYPE data
        RAISING   cx_root,

      save_content
        IMPORTING data     TYPE data
                  object   TYPE trkey
                  language TYPE spras
                  version  TYPE r3state
                  saved_by TYPE as4user
        RAISING   cx_root.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_chko_persistence IMPLEMENTATION.

  METHOD get_content.
    DATA master_language TYPE tadir-masterlang.

    CLEAR data.

    CHECK object-obj_type = 'CHKO'.

    DATA(db_api) = NEW cl_chko_db_api( ).
    DATA(chko_name) = object-obj_name.

    SELECT SINGLE masterlang FROM tadir INTO @master_language WHERE pgmid = 'R3TR' AND object = 'CHKO' AND obj_name = @chko_name.

    DATA(chko_header) = db_api->get_header( name     = CONV #( chko_name )
                                            version  = version
                                            language = language ).
    DATA(chko_content) = db_api->get_content( name     = CONV #( chko_name )
                                              version  = version
                                              language = language ).

    DATA(properties) = VALUE ty_chko(
      format_version             = '1'
      header-description         = chko_header-description
      header-original_language   = master_language
      header-abap_language_version = chko_header-abap_language_version
      category           = chko_content-category-name
      implementing_class = chko_content-implementing_class-name
      parameters         = CORRESPONDING #( chko_content-parameters MAPPING hidden = modifiable )
      remote_enabled     = chko_content-remote_enabled ).

    data = properties.
  ENDMETHOD.

  METHOD save_content.
    DATA content  TYPE cl_chko_db_api=>ty_content .

    CHECK object-obj_type = 'CHKO'.

    DATA(properties) = CONV ty_chko( data ).
    DATA(db_api) = NEW cl_chko_db_api( ).
    DATA(chko_name) = object-obj_name.

    DATA(header) = db_api->get_header( name     = CONV #( chko_name )
                                       version  = version
                                       language = language ).

    IF header IS INITIAL AND version = 'I'.
      "try to load the active header
      header = db_api->get_header( name     = CONV #( chko_name )
                                   version  = 'A'
                                   language = language ).
    ENDIF.

    IF header IS INITIAL.
      header-name = chko_name.
      header-created_by = saved_by.
      GET TIME STAMP FIELD header-created_at.
    ENDIF.

    header-version = version.
    header-description = properties-header-description.
    header-abap_language_version = properties-header-abap_language_version.
    header-changed_by = saved_by.
    GET TIME STAMP FIELD header-changed_at.

    content-category-name = properties-category.
    content-implementing_class-name = properties-implementing_class.
    content-parameters = CORRESPONDING #( properties-parameters MAPPING modifiable = hidden ).
    content-remote_enabled = properties-remote_enabled.

    db_api->update( header   = header
                    content  = content ).
  ENDMETHOD.

ENDCLASS.
