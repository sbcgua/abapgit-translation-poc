REPORT ztest_lang.

**********************************************************************
* APP
**********************************************************************

class lcl_app definition final.

  public section.

    methods read
      importing
        iv_s_lang type laiso
        iv_t_lang type string
        iv_obj_type type zif_abapgit_i18n=>ty_obj_type
        iv_obj_name type sobj_name
        iv_sub_type type zif_abapgit_i18n=>ty_obj_type
        iv_sub_name type zif_abapgit_i18n=>ty_sub_name
        iv_textkey_strategy type string
      returning
        value(rt_file) type string_table.

    methods default_demo
      importing
        io_lxe type ref to zcl_abapgit_i18n_lxe
        io_container type ref to zcl_abapgit_i18n_file.

endclass.

class lcl_app implementation.

  method default_demo.

    data lt_texts type zif_abapgit_i18n=>tt_text_object.

    lt_texts = io_lxe->read(
      iv_obj_type     = 'PROG' " Not needed ?
      iv_textkey_conf = '8,8,10i'
      iv_obj_name     = 'ZTEST_LANG'
      iv_sub_type     = 'REPT' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'PROG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG'
      iv_sub_type     = 'DYNP'
      iv_sub_name     = '2001' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'PROG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG'
      iv_sub_type     = 'CUAD' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'DOMA'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG_DOMA' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'DTEL'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG_DTEL' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'TRAN'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' ).
    io_container->put( lt_texts ).

    lt_texts = io_lxe->read(
      iv_obj_type     = 'MSAG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' ).
    io_container->put( lt_texts ).

  endmethod.


  method read.

    data lo_lxe type ref to zcl_abapgit_i18n_lxe.
    data lo_container type ref to zcl_abapgit_i18n_file.
    data lt_langs type zif_abapgit_i18n=>tt_langid.
    data lt_texts type zif_abapgit_i18n=>tt_text_object.
    data lv_str type string.

    split iv_t_lang at ',' into table lt_langs.

    create object lo_container.
    create object lo_lxe
      exporting
        iv_orig_lang = iv_s_lang
        it_alt_langs = lt_langs.

    if iv_obj_type is initial.
      default_demo(
        io_lxe = lo_lxe
        io_container = lo_container ).
    else.
      if iv_obj_type is initial
        or iv_obj_name is initial
*        or iv_sub_type is initial
*        or iv_sub_name is initial
        or iv_textkey_strategy is initial.
        message 'Obj type, name and strategy must be filled' type 'E' display like 'I'.
        return.
      endif.

      lt_texts = lo_lxe->read(
        iv_obj_type     = iv_obj_type
        iv_obj_name     = iv_obj_name
        iv_textkey_conf = iv_textkey_strategy
        iv_sub_type     = iv_sub_type
        iv_sub_name     = iv_sub_name ).
      lo_container->put( lt_texts ).

    endif.

    lv_str = lo_container->render( ).
    split lv_str at cl_abap_char_utilities=>newline into table rt_file.

  endmethod.

endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

  parameter p_otype type zif_abapgit_i18n=>ty_obj_type. " eg. PROG
  parameter p_oname type sobj_name.                     " eg. ZTEST_LANG
  parameter p_stype type zif_abapgit_i18n=>ty_obj_type. " eg. DYNP
  parameter p_sname type zif_abapgit_i18n=>ty_sub_name. " eg. 2001
  parameter p_tstra type string default '32'.

selection-screen end of block b1.

initialization.
  txt_b1   = 'Source program'.          "#EC NOTEXT

form main.

  data lo_i18n type ref to lcl_app.
  data lt_file type string_table.
  field-symbols <s> like line of lt_file.

  create object lo_i18n.

  lt_file = lo_i18n->read(
    iv_s_lang = 'EN'
    iv_t_lang = 'DE,UK'
    iv_obj_type = p_otype
    iv_obj_name = p_oname
    iv_sub_type = p_stype
    iv_sub_name = p_sname
    iv_textkey_strategy = p_tstra ).

  cl_demo_output=>display_data( lt_file ).

*  loop at lt_file assigning <s>.
*    write: / <s>.
*  endloop.

endform.

start-of-selection.
  perform main.
