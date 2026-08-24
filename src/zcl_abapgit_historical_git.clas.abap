CLASS zcl_abapgit_historical_git DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS push
      IMPORTING
        it_files TYPE zif_abapgit_historical_extract=>ty_files_tt
        iv_url   TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
ENDCLASS.

CLASS zcl_abapgit_historical_git IMPLEMENTATION.
  METHOD push.
    ASSERT iv_url IS NOT INITIAL.
    ASSERT lines( it_files ) > 0.

    BREAK-POINT.
  ENDMETHOD.

ENDCLASS.