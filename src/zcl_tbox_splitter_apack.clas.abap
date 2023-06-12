CLASS zcl_tbox_splitter_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.
ENDCLASS.


CLASS zcl_tbox_splitter_apack IMPLEMENTATION.
  METHOD constructor.
    if_apack_manifest~descriptor = VALUE #( group_id    = 'ztbox'
                                             artifact_id = 'abap-tbox-splitter'
                                             version     = '0.1'
                                             git_url     = 'https://github.com/zenrosadira/abap-tbox-splitter.git' ).
  ENDMETHOD.
ENDCLASS.
