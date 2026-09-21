# WIP, not working yet, git_historical_extract
Extract historical ABAP objects to git

* Objects will be placed according to current package structure, not historical
* Deleted objects are not taken into account?
* Traditional SAP GUI
* Folder logic = 'FULL'
* Full R3TR objects, doesnt respect LIMU
* Only custom objects
* Language dependent texts are extracted in the original language only, translations are out of scope

## TABL

Tables are written as the ABAP file format pair `<name>.tabl.json` and `<name>.tabl.ddic`,
the DDL source being produced by abapGit's `zcl_abapgit_object_tabl_ddl`. That format only
describes transparent tables, so structures, append structures, pooled and cluster tables
and IDoc segment tables are skipped rather than extracted.

Not extracted:

* technical settings and table indexes, which `SVRS_GET_VERSION_TABD_40` does not return at all;
  writing the settings would additionally need an AFF `TABT` type that the abapGit dependency does not provide,
  so no `<name>.tabl.settings.json` is produced
* long texts and IDoc segment definitions

Known fidelity limit: a currency or quantity field whose reference points at another table
is classified through `DDIF_FIELDINFO_GET` against the active dictionary, so the emitted
type follows today's state of the referenced object rather than its state at that transport.
