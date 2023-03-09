interface ZIF_ABAPGIT_DATA_DESERIALIZER
  public .


  types:
    BEGIN OF ty_result,
           table   TYPE tadir-obj_name,
           deletes TYPE REF TO data,
           updates TYPE REF TO data,
           inserts TYPE REF TO data,
         END OF ty_result .
  types:
    ty_results TYPE STANDARD TABLE OF ty_result WITH KEY table .

  methods DESERIALIZE
    importing
      !II_CONFIG type ref to ZIF_ABAPGIT_DATA_CONFIG
      !IT_FILES type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_FILES_TT
      !IS_CHECKS type ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZE_CHECKS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods ACTUALIZE
    importing
      !IT_RESULT type TY_RESULTS
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
