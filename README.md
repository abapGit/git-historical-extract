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

The technical settings are versioned as the separate `TABT` sub object, read through
`SVRS_GET_VERSION_TABT_40` and written as `<name>.tabl.settings.json`. The abapGit dependency
has no AFF type for that file, so a copy of `zif_aff_tabt_v1` lives here as
`zif_abapgit_hist_aff_tabt_v1`. The settings file is only written when the transport carries a
settings version next to the table definition, otherwise the file from an earlier transport is kept.
`writableByAmdp` is never written, and the storage type is written as `undefined` on releases
without a row or column store setting.

Not extracted:

* table indexes, which `SVRS_GET_VERSION_TABD_40` does not return at all
* long texts and IDoc segment definitions

Known fidelity limit: a currency or quantity field whose reference points at another table
is classified through `DDIF_FIELDINFO_GET` against the active dictionary, so the emitted
type follows today's state of the referenced object rather than its state at that transport.
