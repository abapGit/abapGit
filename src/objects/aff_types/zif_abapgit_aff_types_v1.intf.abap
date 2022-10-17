INTERFACE zif_abapgit_aff_types_v1 PUBLIC.

  TYPES ty_format_version TYPE string.

  TYPES ty_abap_language_version TYPE c LENGTH 1.

  TYPES ty_abap_language_version_cloud TYPE c LENGTH 1.

  TYPES ty_abap_language_version_src TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_abap_language_version_src,
      standard          TYPE ty_abap_language_version_src VALUE 'X',
      key_user          TYPE ty_abap_language_version_src VALUE '2',
      cloud_development TYPE ty_abap_language_version_src VALUE '5',
    END OF co_abap_language_version_src.

  CONSTANTS:
    BEGIN OF co_abap_language_version,
      standard          TYPE ty_abap_language_version VALUE space,
      key_user          TYPE ty_abap_language_version VALUE '2',
      cloud_development TYPE ty_abap_language_version VALUE '5',
    END OF co_abap_language_version.

  CONSTANTS:
    BEGIN OF co_abap_language_version_cloud,
      standard          TYPE ty_abap_language_version_cloud VALUE space,
      cloud_development TYPE ty_abap_language_version_cloud VALUE '5',
    END OF co_abap_language_version_cloud.

  TYPES ty_description_60 TYPE c LENGTH 60.
  TYPES ty_description_100 TYPE c LENGTH 100.

  TYPES ty_object_name_30 TYPE c LENGTH 30.

  TYPES ty_original_language TYPE sy-langu.

  TYPES:
    BEGIN OF ty_header_60_src,
      description           TYPE ty_description_60,
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version_src,
    END OF ty_header_60_src.

  TYPES:
    BEGIN OF ty_header_60_cloud,
      description           TYPE ty_description_60,
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version_cloud,
    END OF ty_header_60_cloud.

  TYPES:
    BEGIN OF ty_header_60,
      description           TYPE ty_description_60,
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version,
    END OF ty_header_60.

  TYPES:
    BEGIN OF ty_header_60_no_abap_lv,
      description           TYPE ty_description_60,
      original_language     TYPE ty_original_language,
    END OF ty_header_60_no_abap_lv.

  TYPES:
    BEGIN OF ty_header_100,
      description           TYPE ty_description_100,
      original_language     TYPE ty_original_language,
      abap_language_version TYPE ty_abap_language_version,
    END OF ty_header_100.

  TYPES:
    BEGIN OF ty_header_only_description,
      description TYPE ty_description_60,
    END OF ty_header_only_description.

  TYPES ty_option TYPE c LENGTH 2.

  CONSTANTS:
    BEGIN OF co_option,
      equals               TYPE ty_option VALUE 'EQ',
      between              TYPE ty_option VALUE 'BT',
      greater_than         TYPE ty_option VALUE 'GT',
      contains_pattern     TYPE ty_option VALUE 'CP',
      not_equal            TYPE ty_option VALUE 'NE',
      not_between          TYPE ty_option VALUE 'NB',
      not_contains_pattern TYPE ty_option VALUE 'NP',
      greater_equal        TYPE ty_option VALUE 'GE',
      less_than            TYPE ty_option VALUE 'LT',
      less_equal           TYPE ty_option VALUE 'LE',
    END OF co_option.

  TYPES ty_sign TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_sign,
      include TYPE ty_sign VALUE 'I',
      exclude TYPE ty_sign VALUE 'E',
    END OF co_sign.

ENDINTERFACE.
