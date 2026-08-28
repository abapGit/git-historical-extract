INTERFACE zif_abapgit_historical_extract PUBLIC.

  TYPES:
      BEGIN OF ty_file,
        path     TYPE string,
        filename TYPE string,
        source   TYPE string,
        deleted  TYPE abap_bool,
      END OF ty_file .
  TYPES ty_files_tt TYPE STANDARD TABLE OF ty_file WITH EMPTY KEY .

ENDINTERFACE.
