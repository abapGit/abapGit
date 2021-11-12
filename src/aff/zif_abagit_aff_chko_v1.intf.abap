INTERFACE zif_abagit_aff_chko_v1
  PUBLIC.

  TYPES:
    "! <p class="shorttext">Parameter</p>
    "! A parameter of an ATC check object
    BEGIN OF ty_parameter,
      "! <p class="shorttext">Technical ID</p>
      "! The technical Id of a parameter
      technical_id TYPE c LENGTH 4,
      "! <p class="shorttext">Parameter Name</p>
      "! Name of a parameter
      name         TYPE string,
      "! <p class="shorttext">Parameter Description</p>
      "! Description of a parameter
      description  TYPE c LENGTH 255,
      "! <p class="shorttext">Hidden Flag</p>
      "! The parameter is hidden
      hidden       TYPE abap_bool,
    END OF ty_parameter.

  TYPES:
    "! <p class="shorttext">Parameters</p>
    "! List of parameters
    ty_parameters TYPE STANDARD TABLE OF if_aff_chko_v1=>ty_parameter WITH KEY technical_id
                                WITH UNIQUE HASHED KEY xml_name COMPONENTS name.

  TYPES:
    "! <p class="shorttext">ATC Check</p>
    "! ATC check object properties
    BEGIN OF ty_main,
      "! $required
      format_version     TYPE if_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header             TYPE if_aff_types_v1=>ty_header_60,
      "! <p class="shorttext">Category</p>
      "! The parent category of an ATC check object
      "! $required
      category           TYPE if_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Implementing Class</p>
      "! The implementing class of the ATC check object
      "! $required
      implementing_class TYPE if_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Remote-enabled flag</p>
      "! The ATC check object is remote-enabled
      remote_enabled     TYPE abap_bool,
      "! <p class="shorttext">Parameters</p>
      "! Parameters of the ATC check object
      parameters         TYPE ty_parameters,
    END OF ty_main.
ENDINTERFACE.
