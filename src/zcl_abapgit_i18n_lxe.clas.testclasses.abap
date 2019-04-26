class ltct_bapgit_i18n_lxe definition
  for testing
  duration short
  risk level harmless.
  private section.
    methods get_lang_iso4 for testing.
    methods merge_portion for testing.
endclass.

class ltct_bapgit_i18n_lxe implementation.
  method get_lang_iso4.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_i18n_lxe=>get_lang_iso4( 'DE' )
      exp = 'deDE' ).
  endmethod.
  method merge_portion.
    "TODO
  endmethod.
endclass.
