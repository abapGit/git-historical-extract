CLASS zcl_abapgit_historical_extract DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_devc_range TYPE RANGE OF tadir-devclass.

    METHODS run
      IMPORTING
        !it_packages TYPE ty_devc_range
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_EXTRACT IMPLEMENTATION.


  METHOD run.

    SELECT devclass, parentcl
      FROM tdevc INTO TABLE @DATA(lt_tdevc)
      WHERE devclass IN @it_packages
      AND parentcl = ''
      ORDER BY PRIMARY KEY.

    DATA(lo_dot) = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot->set_folder_logic( 'FULL' ).

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    LOOP AT lt_tdevc INTO DATA(ls_tdevc).
      APPEND LINES OF zcl_abapgit_factory=>get_tadir( )->read(
        io_dot     = lo_dot
        iv_package = ls_tdevc-devclass ) TO lt_tadir.
    ENDLOOP.
    DELETE lt_tadir WHERE object <> 'PROG'.

    TYPES: BEGIN OF ty_parts,
             objtype  TYPE vrsd-objtype,
             objname  TYPE vrsd-objname,
             type     TYPE tadir-object,
             name     TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_parts.
    DATA lt_parts TYPE STANDARD TABLE OF ty_parts.
    LOOP AT lt_tadir INTO DATA(ls_tadir).
      CASE ls_tadir-object.
        WHEN 'PROG'.
          APPEND VALUE #(
            objtype  = 'REPS'
            objname  = ls_tadir-obj_name
            type     = ls_tadir-object
            name     = ls_tadir-obj_name
            devclass = ls_tadir-devclass ) TO lt_parts.
        WHEN OTHERS.
          ASSERT 1 = 'todo'.
      ENDCASE.
    ENDLOOP.

    IF lines( lt_parts ) = 0.
      RETURN.
    ENDIF.
    SELECT * FROM vrsd INTO TABLE @DATA(lt_vrsd)
      FOR ALL ENTRIES IN @lt_parts
      WHERE objtype = @lt_parts-objtype
      AND objname = @lt_parts-objname
      ORDER BY PRIMARY KEY.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.
    LOOP AT lt_vrsd INTO DATA(ls_vrsd).
* note that this function module returns the full 255 character width source code
      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
          object_name = ls_vrsd-objname
          object_type = ls_vrsd-objtype
          versno      = ls_vrsd-versno
        TABLES
          repos_tab   = lt_repos
          trdir_tab   = lt_trdir
        EXCEPTIONS
          no_version  = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
