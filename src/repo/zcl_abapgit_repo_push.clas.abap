CLASS zcl_abapgit_repo_push DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_push .

    METHODS constructor
      IMPORTING
        !ii_repo_online TYPE REF TO zif_abapgit_repo_online .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_repo_online TYPE REF TO zif_abapgit_repo_online .
ENDCLASS.



CLASS zcl_abapgit_repo_push IMPLEMENTATION.


  METHOD constructor.

    mi_repo_online = ii_repo_online.

  ENDMETHOD.


  METHOD zif_abapgit_repo_push~push.

    zcl_abapgit_exit=>get_instance( )->validate_before_push(
      is_comment     = is_comment
      io_stage       = io_stage
      ii_repo_online = mi_repo_online ).

    mi_repo_online->push( is_comment = is_comment
                          io_stage   = io_stage ).

    COMMIT WORK.

    zcl_abapgit_exit=>get_instance( )->validate_after_push( mi_repo_online ).

  ENDMETHOD.
ENDCLASS.
