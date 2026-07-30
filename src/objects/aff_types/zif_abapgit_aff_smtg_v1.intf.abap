INTERFACE zif_abapgit_aff_smtg_v1
  PUBLIC.

  TYPES:
    "! <p class="shorttext">General Information</p>
    "! Combines template header and administrative data.
    BEGIN OF ty_general_information,
      "! <p class="shorttext">Template Description</p>
      "! Long description to that email template
      template_description        TYPE c LENGTH 255,
      "! <p class="shorttext">CDS View</p>
      "! The corresponding CDS view name for the template
      "! $required
      cds_view                    TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Predelivered</p>
      "! Indicator if the template is predelivered
      is_predelivered             TYPE abap_bool,
      "! <p class="shorttext">Subject</p>
      "! The subject of the email template
      "! $required
      email_subject               TYPE c LENGTH 255,
      "! <p class="shorttext">Generate Plain Text from HTML</p>
      "! Indicator if the template text body is generated automatically from html body
      is_plaintext_auto_generated TYPE abap_bool,
    END OF ty_general_information.
  " Corresponding content ist stored in a separate file and implemented as action

  TYPES:
    "! <p class="shorttext">Email Template Content</p>
    "! Displays the HTML und plain text template content.
    BEGIN OF ty_template_content,
      "! <p class="shorttext">Name</p>
      "! Name
      name TYPE string,
    END OF ty_template_content.

  "! <p class="shorttext">Table of Template Content Types</p>
  "! Table of template content types
  TYPES ty_template_contents TYPE STANDARD TABLE OF ty_template_content WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Email Template</p>
    "! Email Template
    BEGIN OF ty_main,
      "! <p class="shorttext">Format Version</p>
      "! Format version
      "! $required
      format_version      TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header              TYPE zif_abapgit_aff_types_v1=>ty_header_60,
      "! <p class="shorttext">General Information</p>
      "! General information
      "! $required
      general_information TYPE ty_general_information,
      "! <p class="shorttext">Template Content</p>
      "! Template Content
      template_content    TYPE ty_template_contents,
    END OF ty_main.

ENDINTERFACE.
