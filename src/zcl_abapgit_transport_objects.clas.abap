CLASS zcl_abapgit_transport_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_transport_objects TYPE scts_tadir .
    METHODS to_stage
      IMPORTING
        !io_stage           TYPE REF TO zcl_abapgit_stage
        !is_stage_objects   TYPE zif_abapgit_definitions=>ty_stage_files
        !it_object_statuses TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    DATA mt_transport_objects TYPE scts_tadir.

ENDCLASS.



CLASS ZCL_ABAPGIT_TRANSPORT_OBJECTS IMPLEMENTATION.


  METHOD constructor.
    mt_transport_objects = it_transport_objects.
  ENDMETHOD.


  METHOD to_stage.
    DATA: ls_transport_object TYPE tadir,
          ls_local_file       TYPE zif_abapgit_definitions=>ty_file_item,
          ls_object_status    TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT mt_transport_objects INTO ls_transport_object.
      LOOP AT it_object_statuses INTO ls_object_status
          WHERE obj_name = ls_transport_object-obj_name
          AND obj_type = ls_transport_object-object
          AND NOT lstate IS INITIAL.

        CASE ls_object_status-lstate.
          WHEN zif_abapgit_definitions=>gc_state-added OR zif_abapgit_definitions=>gc_state-modified.
            IF ls_transport_object-delflag = abap_true.
              zcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
              } should be added/modified, but has deletion flag in transport| ).
            ENDIF.

            READ TABLE is_stage_objects-local
                  INTO ls_local_file
              WITH KEY item-obj_name = ls_transport_object-obj_name
                       item-obj_type = ls_transport_object-object
                       file-filename = ls_object_status-filename.
            IF sy-subrc <> 0.
              zcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
              } not found in the local repository files| ).
            ENDIF.

            io_stage->add(
              iv_path     = ls_local_file-file-path
              iv_filename = ls_local_file-file-filename
              iv_data     = ls_local_file-file-data ).
          WHEN zif_abapgit_definitions=>gc_state-deleted.
            IF ls_transport_object-delflag = abap_false.
              zcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
              } should be removed, but has NO deletion flag in transport| ).
            ENDIF.
            io_stage->rm(
              iv_path     = ls_object_status-path
              iv_filename = ls_object_status-filename ).
          WHEN OTHERS.
            ASSERT 0 = 1. "Unexpected state
        ENDCASE.
      ENDLOOP.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
        } not found in the local repository files| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
