CLASS zcl_abapgit_file_deserialize DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_results
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS filter_files_to_deserialize
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
    CLASS-METHODS prioritize_deser
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
    CLASS-METHODS adjust_namespaces
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

ENDCLASS.



CLASS zcl_abapgit_file_deserialize IMPLEMENTATION.


  METHOD adjust_namespaces.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF rt_results.

    rt_results = it_results.

    LOOP AT rt_results ASSIGNING <ls_result>.
      REPLACE ALL OCCURRENCES OF '#' IN <ls_result>-obj_name WITH '/'.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_files_to_deserialize.

    DATA lt_objects LIKE rt_results.
    DATA lr_object  TYPE REF TO zif_abapgit_definitions=>ty_result.
    DATA ls_item    TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_tabix   TYPE sy-tabix.

    rt_results = it_results.

    "preparation for object logging, sort all file entries by objects
    IF ii_log IS BOUND.
      lt_objects = rt_results.
      SORT lt_objects
        BY obj_type
           obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING obj_type obj_name.
      DELETE lt_objects WHERE obj_type IS INITIAL AND obj_name IS INITIAL.
    ENDIF.

    "ignore objects w/o changes
    DELETE rt_results WHERE match = abap_true.     " Full match
    "log objects w/o changes
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts of the objects have not changed
          ls_item-devclass = lr_object->package.
          ls_item-obj_type = lr_object->obj_type.
          ls_item-obj_name = lr_object->obj_name.
          ii_log->add_success(
            iv_msg  = |Object { ls_item-obj_name } (type { ls_item-obj_type }) not changed; no import required|
            is_item = ls_item ).
          "ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore objects w/o object type
    DELETE rt_results WHERE obj_type IS INITIAL.
    "log objects w/o object type
    IF sy-subrc = 0 AND ii_log IS BOUND.
      LOOP AT lt_objects REFERENCE INTO lr_object WHERE obj_type IS INITIAL.
        CHECK lr_object->obj_name IS NOT INITIAL.
        ls_item-devclass = lr_object->package.
        ls_item-obj_type = lr_object->obj_type.
        ls_item-obj_name = lr_object->obj_name.
        ii_log->add_warning(
          iv_msg  = |Object type for { ls_item-obj_name } not defined - will be ignored by abapGit|
          is_item = ls_item ).
      ENDLOOP.
      DELETE lt_objects WHERE obj_type IS INITIAL.
    ENDIF.

    "ignore objects that exists only local
    DELETE rt_results WHERE lstate = zif_abapgit_definitions=>c_state-added AND rstate IS INITIAL.
    "ignore objects that where deleted remotely
    DELETE rt_results WHERE rstate = zif_abapgit_definitions=>c_state-deleted.
    "log objects that exists only local or where deleted remotely
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts exists only local
          "no log message; ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT rt_results
      BY obj_type ASCENDING
         obj_name ASCENDING
         rstate   DESCENDING  " ensures that non-empty rstate is kept
         lstate   DESCENDING. " ensures that non-empty lstate is kept
    DELETE ADJACENT DUPLICATES FROM rt_results COMPARING obj_type obj_name.

  ENDMETHOD.


  METHOD get_results.

    rt_results = adjust_namespaces(
                   prioritize_deser(
                     filter_files_to_deserialize(
                       it_results = zcl_abapgit_file_status=>status( io_repo )
                       ii_log     = ii_log ) ) ).

  ENDMETHOD.


  METHOD prioritize_deser.

* todo, refactor this method

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* WEBI has to be handled before SPRX.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'WEBI'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* SPRX has to be handled before depended objects CLAS/INFT/TABL etc.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'SPRX'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* XSLT has to be handled before CLAS/PROG
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'XSLT'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* DDLS has to be handled before DCLS
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'DDLS'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* IOBJ has to be handled before ODSO
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IOBJ'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* TOBJ has to be handled before SCP1
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'TOBJ'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* OTGR has to be handled before CHAR
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'OTGR'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP'
        AND obj_type <> 'PROG'
        AND obj_type <> 'XSLT'
        AND obj_type <> 'PINF'
        AND obj_type <> 'DEVC'
        AND obj_type <> 'ENHS'
        AND obj_type <> 'ENHO'
        AND obj_type <> 'ENHC'
        AND obj_type <> 'ENSC'
        AND obj_type <> 'DDLS'
        AND obj_type <> 'SPRX'
        AND obj_type <> 'WEBI'
        AND obj_type <> 'IOBJ'
        AND obj_type <> 'TOBJ'
        AND obj_type <> 'OTGR'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* Enhancements might refer to other objects of the repo so create them after
* Order: spots, composite spots, implementations, composite implementations
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'ENHS'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'ENSC'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'ENHO'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'ENHC'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PINF after everything as it can expose objects
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PINF'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* DEVC after PINF, as it can refer for package interface usage
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'DEVC'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
