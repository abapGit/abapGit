"! <p class="shorttext synchronized" lang="en">Common types for classes and interfaces</p>
"! Types which can be reused for Class and Interface AFF Types
INTERFACE zif_abapgit_aff_oo_types_v1
  PUBLIC.

  TYPES:
    "! <p class="shorttext">Name and Description</p>
    "! Name and description
    BEGIN OF ty_component_description,
      "! <p class="shorttext">Name</p>
      "! Name
      "! $required
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Description</p>
      "! Description
      "! $required
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_component_description,

    "! <p class="shorttext">Component Descriptions</p>
    "! Class component descriptions
    ty_component_descriptions TYPE SORTED TABLE OF ty_component_description WITH UNIQUE KEY name.


  TYPES:
    "! <p class="shorttext">Method Description</p>
    "! Method description
    BEGIN OF ty_method,
      "! <p class="shorttext">Method Name</p>
      "! Name of the method
      "! $required
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Method Description</p>
      "! Description of the method
      "! $required
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
      "! <p class="shorttext">Parameter Descriptions</p>
      "! Parameter descriptions
      parameters  TYPE ty_component_descriptions,
      "! <p class="shorttext">Exception Descriptions</p>
      "! Exception descriptions
      exceptions  TYPE ty_component_descriptions,
    END OF ty_method,
    "! <p class="shorttext">Method Descriptions</p>
    "! Method descriptions
    ty_methods TYPE SORTED TABLE OF ty_method WITH UNIQUE KEY name.

  TYPES:
    "! <p class="shorttext">Event Description</p>
    "! Event description
    BEGIN OF ty_event,
      "! <p class="shorttext">Event Name</p>
      "! Name of the event
      "! $required
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      "! <p class="shorttext">Event Description</p>
      "! Description of the event
      "! $required
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
      "! <p class="shorttext">Parameter Descriptions</p>
      "! Parameter descriptions
      parameters  TYPE ty_component_descriptions,
    END OF ty_event,
    "! <p class="shorttext">Event Descriptions</p>
    "! Event descriptions
    ty_events TYPE SORTED TABLE OF ty_event WITH UNIQUE KEY name.

  TYPES:
    "! <p class="shorttext">Descriptions</p>
    "! Descriptions maintained in SE80
    BEGIN OF ty_descriptions,
      "! <p class="shorttext">Type Descriptions</p>
      "! Type descriptions
      types      TYPE ty_component_descriptions,
      "! <p class="shorttext">Attribute Descriptions</p>
      "! Attribute descriptions
      attributes TYPE ty_component_descriptions,
      "! <p class="shorttext">Event Descriptions</p>
      "! Event descriptions
      events     TYPE ty_events,
      "! <p class="shorttext">Method Descriptions</p>
      "! Method descriptions
      methods    TYPE ty_methods,
    END OF ty_descriptions.

ENDINTERFACE.
