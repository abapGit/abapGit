"! <p class="shorttext synchronized" lang="en">General types reusable in AFF</p>
"! Types which can be reused in all AFF object types
INTERFACE zif_abapgit_aff_types_v1 PUBLIC.

  "! <p class="shorttext">ABAP File Format Version</p>
  "! The ABAP file format version
  TYPES ty_format_version TYPE string.

  "! <p class="shorttext">ABAP Language Version</p>
  "! ABAP language version
  "! $values {@link zif_abapgit_aff_types_v1.data:co_abap_language_version}
  "! $default {@link zif_abapgit_aff_types_v1.data:co_abap_language_version.standard}
  TYPES ty_abap_language_version TYPE c LENGTH 1.

  "! <p class="shorttext">ABAP Language Version</p>
  "! ABAP language version
  "! $values {@link zif_abapgit_aff_types_v1.data:co_abap_language_version_cloud}
  "! $default {@link zif_abapgit_aff_types_v1.data:co_abap_language_version_cloud.standard}
  TYPES ty_abap_language_version_cloud TYPE c LENGTH 1.

  "! <p class="shorttext">ABAP Language Version</p>
  "! ABAP language version
  "! $values {@link zif_abapgit_aff_types_v1.data:co_abap_language_version_src}
  "! $default {@link zif_abapgit_aff_types_v1.data:co_abap_language_version_src.standard}
  TYPES ty_abap_language_version_src TYPE c LENGTH 1.

  CONSTANTS:
    "! <p class="shorttext">ABAP Language Version (Source Code Objects)</p>
    "! ABAP language version for source code objects like CLAS, INTF, FUGR or PROG.
    BEGIN OF co_abap_language_version_src,
      "! <p class="shorttext">Standard</p>
      "! Standard
      standard          TYPE ty_abap_language_version_src VALUE 'X',
      "! <p class="shorttext">ABAP for Key Users</p>
      "! ABAP for key user extensibility
      key_user          TYPE ty_abap_language_version_src VALUE '2',
      "! <p class="shorttext">ABAP Cloud Development</p>
      "! ABAP cloud development
      cloud_development TYPE ty_abap_language_version_src VALUE '5',
    END OF co_abap_language_version_src.

  CONSTANTS:
    "! <p class="shorttext">ABAP Language Version (Non-Source Code Objects)</p>
    "! ABAP language version for non-source code objects
    BEGIN OF co_abap_language_version,
      "! <p class="shorttext">Standard</p>
      "! Standard
      standard          TYPE ty_abap_language_version VALUE space,
      "! <p class="shorttext">ABAP for Key Users</p>
      "! ABAP for key user extensibility
      key_user          TYPE ty_abap_language_version VALUE '2',
      "! <p class="shorttext">ABAP Cloud Development</p>
      "! ABAP cloud development
      cloud_development TYPE ty_abap_language_version VALUE '5',
    END OF co_abap_language_version.

  CONSTANTS:
    "! <p class="shorttext">ABAP Language Version</p>
    "! ABAP language version for objects which only exist for standard and cloud development (no key user extensibility)
    BEGIN OF co_abap_language_version_cloud,
      "! <p class="shorttext">Standard</p>
      "! Standard
      standard          TYPE ty_abap_language_version_cloud VALUE space,
      "! <p class="shorttext">ABAP Cloud Development</p>
      "! ABAP cloud development
      cloud_development TYPE ty_abap_language_version_cloud VALUE '5',
    END OF co_abap_language_version_cloud.

  "! <p class="shorttext">Description</p>
  "! Description of the ABAP object
  TYPES ty_description_60 TYPE c LENGTH 60.
  "! <p class="shorttext">Description</p>
  "! Description of the ABAP object
  TYPES ty_description_100 TYPE c LENGTH 100.

  "! <p class="shorttext">Object Name</p>
  "! Object name with max. length 30
  TYPES ty_object_name_30 TYPE c LENGTH 30.

  "! <p class="shorttext">Original Language</p>
  "! Original language of the ABAP object
  TYPES ty_original_language TYPE sy-langu.

  TYPES:
    "! <p class="shorttext">Header for Source Code Objects</p>
    "! The header for an ABAP main object (with source code) with a description of 60 characters
    BEGIN OF ty_header_60_src,
      "! $required
      description           TYPE ty_description_60,
      "! $required
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version_src,
    END OF ty_header_60_src.

  TYPES:
    "! <p class="shorttext">Header for Non-Source Code Objects (no key user)</p>
    "! The header for an ABAP main object (without source code) with a description of 60 characters (no key user)
    BEGIN OF ty_header_60_cloud,
      "! $required
      description           TYPE ty_description_60,
      "! $required
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version_cloud,
    END OF ty_header_60_cloud.

  TYPES:
    "! <p class="shorttext">Header for Non-Source Code Objects</p>
    "! The header for an ABAP main object (without source code) with a description of 60 characters
    BEGIN OF ty_header_60,
      "! $required
      description           TYPE ty_description_60,
      "! $required
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version,
    END OF ty_header_60.

  TYPES:
    "! <p class="shorttext">Header for Non-Source Code Objects</p>
    "! The header for an ABAP main object (without source code) with a description of 100 characters
    BEGIN OF ty_header_100,
      "! $required
      description           TYPE ty_description_100,
      "! $required
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version,
    END OF ty_header_100.

  TYPES:
    "! <p class="shorttext">Header for Subobjects</p>
    "! The header for an ABAP  subobject with a description of 60 characters
    BEGIN OF ty_header_only_description,
      "! $required
      description TYPE ty_description_60,
    END OF ty_header_only_description.

  "! <p class="shorttext">Option</p>
  "! Option
  "! $values {@link zif_abapgit_aff_types_v1.data:co_option}
  TYPES ty_option TYPE c LENGTH 2.

  CONSTANTS:
    "! <p class="shorttext">Option</p>
    "! Option
    BEGIN OF co_option,
      "! <p class="shorttext">Equals</p>
      "! Equals
      equals               TYPE ty_option VALUE 'EQ',
      "! <p class="shorttext">Between</p>
      "! Between
      between              TYPE ty_option VALUE 'BT',
      "! <p class="shorttext">Greater Than</p>
      "! Greater than
      greater_than         TYPE ty_option VALUE 'GT',
      "! <p class="shorttext">Contains Pattern</p>
      "! Contains pattern
      contains_pattern     TYPE ty_option VALUE 'CP',
      "! <p class="shorttext">Not Equal</p>
      "! Not equal
      not_equal            TYPE ty_option VALUE 'NE',
      "! <p class="shorttext">Not Between</p>
      "! Not between
      not_between          TYPE ty_option VALUE 'NB',
      "! <p class="shorttext">Not Contains Pattern</p>
      "! Not contains pattern
      not_contains_pattern TYPE ty_option VALUE 'NP',
      "! <p class="shorttext">Greater Equal</p>
      "! Greater equal
      greater_equal        TYPE ty_option VALUE 'GE',
      "! <p class="shorttext">Less Than</p>
      "! Less than
      less_than            TYPE ty_option VALUE 'LT',
      "! <p class="shorttext">Less Equal</p>
      "! Less equal
      less_equal           TYPE ty_option VALUE 'LE',
    END OF co_option.

  "! <p class="shorttext">Sign</p>
  "! Sign
  "! $values {@link zif_abapgit_aff_types_v1.data:co_sign}
  TYPES ty_sign TYPE c LENGTH 1.

  CONSTANTS:
    "! <p class="shorttext">Sign</p>
    "! Sign
    BEGIN OF co_sign,
      "! <p class="shorttext">Include</p>
      "! Include
      include TYPE ty_sign VALUE 'I',
      "! <p class="shorttext">Exclude</p>
      "! Exclude
      exclude TYPE ty_sign VALUE 'E',
    END OF co_sign.

ENDINTERFACE.
