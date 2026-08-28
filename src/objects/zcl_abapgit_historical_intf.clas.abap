CLASS zcl_abapgit_historical_intf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_INTF IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'INTF'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA ls_file LIKE LINE OF rt_files.


    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    ls_file-filename = |{ to_lower( ms_tadir-obj_name ) }.intf.abap|.

    READ TABLE lt_vrsd INTO DATA(ls_extended) WITH KEY objtype = 'INTF'.
    IF sy-subrc = 0.
      ls_file-source = zcl_abapgit_historical_source=>read_reps( ls_extended ).
    ENDIF.

    INSERT ls_file INTO TABLE rt_files.

  ENDMETHOD.

  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.intf.abap|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
