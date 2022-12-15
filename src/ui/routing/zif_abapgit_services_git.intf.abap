INTERFACE zif_abapgit_services_git PUBLIC.

  TYPES:
    BEGIN OF ty_commit_fields,
      repo_key        TYPE zif_abapgit_persistence=>ty_repo-key,
      committer_name  TYPE string,
      committer_email TYPE string,
      author_name     TYPE string,
      author_email    TYPE string,
      comment         TYPE string,
      body            TYPE string,
    END OF ty_commit_fields.

ENDINTERFACE.
