REPORT zabapgit_historical_extract.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception.

  DATA(lo_dot) = zcl_abapgit_dot_abapgit=>build_default( ).
  lo_dot->set_folder_logic( 'FULL' ). " todo, replace with constant

  DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir( )->read(
    iv_package = 'ZHISTORY' ). " todo, selection screen


  TYPES: BEGIN OF ty_parts,
           objtype  TYPE vrsd-objtype,
           objname  TYPE vrsd-objname,
           type     TYPE tadir-object,
           name     TYPE tadir-obj_name,
           devclass TYPE tadir-devclass,
         END OF ty_parts.
  DATA lt_parts TYPE STANDARD TABLE OF ty_parts.
  LOOP AT lt_tadir INTO DATA(ls_tadir) WHERE object <> 'DEVC'.
    CASE ls_tadir-object.
      WHEN 'PROG'.
        APPEND VALUE #(
          objtype  = 'REPS'
          objname  = ls_tadir-obj_name
          type     = ls_tadir-object
          name     = ls_tadir-obj_name
          devclass = ls_tadir-devclass ) TO lt_parts.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.
  ENDLOOP.

  IF lines( lt_parts ) = 0.
    RETURN.
  ENDIF.

  SELECT * FROM vrsd INTO TABLE @DATA(lt_vrsd)
    FOR ALL ENTRIES IN @lt_parts
    WHERE objtype = @lt_parts-objtype
    AND objname = @lt_parts-objname.

  LOOP AT lt_vrsd INTO DATA(ls_vrsd).

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.
* note that this function module returns the full 255 character width source code
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = ls_vrsd-objname
        object_type = ls_vrsd-objtype
        versno      = ls_vrsd-versno
      TABLES
        repos_tab   = lt_repos
        trdir_tab   = lt_trdir.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDLOOP.

ENDFORM.
