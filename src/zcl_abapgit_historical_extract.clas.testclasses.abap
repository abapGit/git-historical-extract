CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_extract DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_Test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_Cut TYPE REF TO zcl_Abapgit_Historical_Extract.

    METHODS:
      setup,
      build FOR TESTING.
ENDCLASS.


CLASS ltcl_Test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD build.

* todo

    mo_cut->build( ).

  ENDMETHOD.

ENDCLASS.
