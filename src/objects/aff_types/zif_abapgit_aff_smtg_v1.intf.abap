INTERFACE zif_abapgit_aff_smtg_v1
  PUBLIC.

  TYPES:
    BEGIN OF ty_general_information,
      template_description        TYPE c LENGTH 255,
      cds_view                    TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      is_predelivered             TYPE abap_bool,
      email_subject               TYPE c LENGTH 255,
      is_plaintext_auto_generated TYPE abap_bool,
    END OF ty_general_information.
  " Corresponding content ist stored in a separate file and implemented as action

  TYPES:
    BEGIN OF ty_template_content,
      name TYPE string,
    END OF ty_template_content.

  TYPES ty_template_contents TYPE STANDARD TABLE OF ty_template_content WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_main,
      format_version      TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header              TYPE zif_abapgit_aff_types_v1=>ty_header_60,
      general_information TYPE ty_general_information,
      template_content    TYPE ty_template_contents,
    END OF ty_main.

ENDINTERFACE.
