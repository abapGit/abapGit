"! <p class="shorttext synchronized" lang="en">Compare Customizing Content</p>
INTERFACE zif_abapgit_customizing_comp
  PUBLIC .


  TYPES:
    BEGIN OF ty_bcset_metadata,
      scprattr TYPE scprattr,
      scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
      scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
      scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
      scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
      scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
      subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
    END OF ty_bcset_metadata .

  METHODS compare_customizing_with_table
    IMPORTING
      !is_file_details TYPE zif_abapgit_definitions=>ty_file
      !is_item         TYPE zif_abapgit_definitions=>ty_item
    CHANGING
      !cs_result       TYPE zif_abapgit_definitions=>ty_result
    RAISING
      zcx_abapgit_exception .
  METHODS create_local_file
    CHANGING
      !cs_file TYPE zif_abapgit_definitions=>ty_stage_files
    RAISING
      zcx_abapgit_exception .
  METHODS apply_customizing_content
    IMPORTING
      !is_bcset_metadata TYPE ty_bcset_metadata
      !io_log            TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
