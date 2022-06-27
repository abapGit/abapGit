INTERFACE zif_abapgit_aff_intf_v1 PUBLIC.

  "! <p class="shorttext">Interface Category</p>
  "! Interface category
  "! $values {@link zif_abapgit_aff_intf_v1.data:co_category}
  TYPES ty_category TYPE n LENGTH 2.

  CONSTANTS:
    "! <p class="shorttext">Interface Category</p>
    "! Interface category
    BEGIN OF co_category,
      "! <p class="shorttext">General</p>
      "! General interface
      general                      TYPE ty_category VALUE '00',
      "! <p class="shorttext">Classic BAdI</p>
      "! Interface definition of a classic BAdI
      classic_badi                 TYPE ty_category VALUE '01',
      "! <p class="shorttext">Business (Static Components)</p>
      "! Business interface for static components
      business_static_components   TYPE ty_category VALUE '51',
      "! <p class="shorttext">Business (Instance-dep. components)</p>
      "! Business interface for instance-dependent components
      business_instance_components TYPE ty_category VALUE '52',
      "! <p class="shorttext">DB Procedure Proxy</p>
      "! Generated interface of a database procedure proxy
      db_procedure_proxy           TYPE ty_category VALUE '65',
      "! <p class="shorttext">Web Dynpro Runtime</p>
      "! Web Dynpro runtime interface
      web_dynpro_runtime           TYPE ty_category VALUE '80',
      "! <p class="shorttext">Enterprise Services</p>
      "! Generated interface of enterprise services
      enterprise_service           TYPE ty_category VALUE '90',
    END OF co_category.

  TYPES:
    "! <p class="shorttext">Interface Properties</p>
    "! Interface properties
    BEGIN OF ty_main,
      "! $required
      format_version TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header         TYPE zif_abapgit_aff_types_v1=>ty_header_60_src,
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      category       TYPE ty_category,
      "! <p class="shorttext">Proxy Interface</p>
      "! Interface is a proxy interface
      proxy          TYPE abap_bool,
      "! <p class="shorttext">Descriptions</p>
      "! Descriptions maintained in SE80
      descriptions   TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
    END OF ty_main.

ENDINTERFACE.
