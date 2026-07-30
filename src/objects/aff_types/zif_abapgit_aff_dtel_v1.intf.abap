INTERFACE zif_abapgit_aff_dtel_v1 PUBLIC.

  TYPES:
    "! <p class="shorttext">Predefined Type</p>
    "! Predefined ABAP type
    BEGIN OF ty_predefined_type,
      "! <p class="shorttext">Data Type</p>
      "! Data type
      "! $required
      data_type TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,
      "! <p class="shorttext">Length</p>
      "! Length
      "! $required
      length    TYPE zif_abapgit_aff_ddic_types_v1=>ty_length,
      "! <p class="shorttext">Decimals</p>
      "! Decimals
      decimals  TYPE zif_abapgit_aff_ddic_types_v1=>ty_decimals,
    END OF ty_predefined_type.

  TYPES:
    "! <p class="shorttext">Field Labels</p>
    "! Field labels
    BEGIN OF ty_field_labels,
      "! <p class="shorttext">Short</p>
      "! Short field label
      short          TYPE c LENGTH 10,
      "! <p class="shorttext">Length of Short Field Label</p>
      "! The entered length is the upper limit for all translations.
      "! The field label maximum should be somewhat longer than the original text.
      "!
      "! $minimum 0
      "! $maximum 10
      short_length   TYPE i,
      "! <p class="shorttext">Medium</p>
      "! Medium field label
      medium         TYPE c LENGTH 20,
      "! <p class="shorttext">Length of Medium Field Label</p>
      "! The entered length is the upper limit for all translations.
      "! The field label maximum should be somewhat longer than the original text.
      "! Recommended length: 15 characters.
      "!
      "! $minimum 0
      "! $maximum 20
      medium_length  TYPE i,
      "! <p class="shorttext">Long</p>
      "! Long field label
      long           TYPE c LENGTH 40,
      "! <p class="shorttext">Length of Long Field Label</p>
      "! The entered length is the upper limit for all translations.
      "! The field label maximum should be somewhat longer than the original text.
      "! Recommended length: 20 characters.
      "!
      "! $minimum 0
      "! $maximum 40
      long_length    TYPE i,
      "! <p class="shorttext">Heading</p>
      "! Heading field label
      heading        TYPE c LENGTH 55,
      "! <p class="shorttext">Length of Heading</p>
      "! The entered length is the upper limit for all translations.
      "! The field label maximum should be somewhat longer than the original text.
      "! Note that the heading is often displayed when editing list output,
      "! its length should not exceed the data element length.
      "!
      "! $minimum 0
      "! $maximum 55
      heading_length TYPE i,
    END OF ty_field_labels.

  "! <p class="shorttext">Category</p>
  "! Category
  "! $values {@link zif_aff_dtel_v1.data:co_category}
  TYPES ty_category TYPE c LENGTH 30.


  CONSTANTS:
    "! <p class="shorttext">Category</p>
    "! Category
    "! {@link zif_aff_dtel_v1.data:ty_category }
    BEGIN OF co_category,
      "! <p class="shorttext">Domain</p>
      "! Domain
      domain                       TYPE ty_category VALUE 'domain',
      "! <p class="shorttext">Predefined Type</p>
      "! Predefined Type
      predefined_type              TYPE ty_category VALUE 'predefinedAbapType',
      "! <p class="shorttext">Reference to Predefined Type</p>
      "! Reference to predefined type
      reference_to_predefined_type TYPE ty_category VALUE 'refToPredefinedAbapType',
      "! <p class="shorttext">Reference to Dictionary Type</p>
      "! Reference to dictionary type
      reference_dictionary_type    TYPE ty_category VALUE 'refToDictionaryType',
      "! <p class="shorttext">Reference to Class/Interface</p>
      "! Reference to class/interface
      reference_clas_int_type      TYPE ty_category VALUE 'refToClifType',
    END OF co_category.

  TYPES:
    "! <p class="shorttext">Data Type Information</p>
    "! Data type information
    BEGIN OF ty_data_type_information,
      "! <p class="shorttext">Category</p>
      "! Category
      "! $required
      category        TYPE ty_category,
      "! <p class="shorttext">Type Name</p>
      "! Type name
      type_name       TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Predefined Type</p>
      "! Predefined ABAP type
      predefined_type TYPE ty_predefined_type,
    END OF ty_data_type_information.

  TYPES:
    "! <p class="shorttext">Search Help</p>
    "! Search help
    BEGIN OF ty_search_help,
      "! <p class="shorttext">Name</p>
      "! Search help name
      "! $required
      name      TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Parameter</p>
      "! Search help parameters
      "! $required
      parameter TYPE c LENGTH 30,
    END OF ty_search_help.

  "! <p class="shorttext">Basic Direction</p>
  "! Basic direction
  "! $values {@link zif_aff_dtel_v1.co_bidi_basic_direction }
  "! $default {@link zif_aff_dtel_v1.co_bidi_basic_direction.left_to_right }
  TYPES ty_basic_direction TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_bidi_basic_direction,
      "! <p class="shorttext">Left to Right</p>
      "! Left to right
      left_to_right TYPE c LENGTH 1 VALUE ' ',
      "! <p class="shorttext">Right to Left</p>
      "! Right to left
      right_to_left TYPE c LENGTH 1 VALUE 'X',
    END OF co_bidi_basic_direction.

  TYPES:
    "! <p class="shorttext">Bidirectional Options</p>
    "! Bidirectional options
    BEGIN OF ty_bidirectional_options,
      "! <p class="shorttext">Basic Direction</p>
      "! Basic direction
      "! $showAlways
      basic_direction TYPE ty_basic_direction,
      "! <p class="shorttext">No Bidirectional Filtering</p>
      "! Deactivates bidirectional text filtering
      no_filtering    TYPE abap_bool,
    END OF ty_bidirectional_options.

  TYPES:
    "! <p class="shorttext">Additional Properties</p>
    "! Additional properties
    BEGIN OF ty_additional_properties,
      "! <p class="shorttext">Search Help</p>
      "! Search help
      search_help              TYPE ty_search_help,
      "! <p class="shorttext">Bidirectional Options</p>
      "! Bidirectional options
      bidirectional_options    TYPE ty_bidirectional_options,
      "! <p class="shorttext">Set/Get Parameter ID</p>
      "! Set/Get parameter ID
      parameter_id             TYPE c LENGTH 20,
      "! <p class="shorttext">Default Component Name</p>
      "! Default component name
      default_component_name   TYPE c LENGTH 30,
      "! <p class="shorttext">Change Document Relevant</p>
      "! Change document relevant
      change_document_relevant TYPE abap_bool,
      "! <p class="shorttext">No Input History</p>
      "! No input history
      no_input_history         TYPE abap_bool,
    END OF ty_additional_properties.

  TYPES:
    "! <p class="shorttext">Data Element</p>
    "! Data element (DTEL)
    BEGIN OF ty_main,
      "! $required
      format_version        TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header                TYPE zif_abapgit_aff_types_v1=>ty_header_60,
      "! <p class="shorttext">Data Type Information</p>
      "! Data type information
      "! $required
      data_type_information TYPE ty_data_type_information,
      "! <p class="shorttext">Field Labels</p>
      "! Field labels
      field_labels          TYPE ty_field_labels,
      "! <p class="shorttext">Additional Properties</p>
      "! Additional properties
      additional_properties TYPE ty_additional_properties,
    END OF ty_main.

ENDINTERFACE.
