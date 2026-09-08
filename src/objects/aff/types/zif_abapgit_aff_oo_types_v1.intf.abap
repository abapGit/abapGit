INTERFACE zif_abapgit_aff_oo_types_v1
  PUBLIC.

  TYPES:
    BEGIN OF ty_component_description,
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_component_description,

    ty_component_descriptions TYPE SORTED TABLE OF ty_component_description WITH UNIQUE KEY name.


  TYPES:
    BEGIN OF ty_method,
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
      parameters  TYPE ty_component_descriptions,
      exceptions  TYPE ty_component_descriptions,
    END OF ty_method,
    ty_methods TYPE SORTED TABLE OF ty_method WITH UNIQUE KEY name.

  TYPES:
    BEGIN OF ty_event,
      name        TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
      parameters  TYPE ty_component_descriptions,
    END OF ty_event,
    ty_events TYPE SORTED TABLE OF ty_event WITH UNIQUE KEY name.

  TYPES:
    BEGIN OF ty_descriptions,
      types      TYPE ty_component_descriptions,
      attributes TYPE ty_component_descriptions,
      events     TYPE ty_events,
      methods    TYPE ty_methods,
    END OF ty_descriptions.

ENDINTERFACE.
