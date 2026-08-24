INTERFACE zif_abapgit_historical_object PUBLIC.

  TYPES:
    BEGIN OF ty_parts,
      objtype  TYPE vrsd-objtype,
      objname  TYPE vrsd-objname,
      type     TYPE tadir-object,
      name     TYPE tadir-obj_name,
      devclass TYPE tadir-devclass,
    END OF ty_parts .
  TYPES ty_parts_tt TYPE STANDARD TABLE OF ty_parts WITH EMPTY KEY .
  TYPES:
    BEGIN OF ty_vrsd,
      objtype TYPE vrsd-objtype,
      objname TYPE vrsd-objname,
      versno  TYPE vrsd-versno,
      korrnum TYPE vrsd-korrnum,
      author  TYPE vrsd-author,
      datum   TYPE vrsd-datum,
      zeit    TYPE vrsd-zeit,
      source  TYPE string,
    END OF ty_vrsd .
  TYPES ty_vrsd_tt TYPE STANDARD TABLE OF ty_vrsd WITH EMPTY KEY .

* find the parts belonging to this object, read them at the given
* transport, and assemble them into abapGit files
  METHODS build_files
    IMPORTING
      iv_korrnum      TYPE vrsd-korrnum
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_historical_extract=>ty_files_tt .

ENDINTERFACE.
