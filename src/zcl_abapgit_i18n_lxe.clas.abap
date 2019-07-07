class ZCL_ABAPGIT_I18N_LXE definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_lang_map,
          lang type zif_abapgit_i18n=>ty_langid,
          iso4 type lxeisolang,
        end of ty_lang_map .
    types:
      tt_lang_map type standard table of ty_lang_map with default key .

    methods constructor
      importing
        !iv_orig_lang type zif_abapgit_i18n=>ty_langid
        !it_alt_langs type zif_abapgit_i18n=>tt_langid .
    methods read
      importing
        !iv_obj_type type trobjtype
        !iv_obj_name type sobj_name
        !iv_sub_type type trobjtype optional
        !iv_sub_name type zif_abapgit_i18n=>ty_sub_name optional
        !iv_textkey_conf type string
      returning
        value(rt_tobjs) type zif_abapgit_i18n=>tt_text_object .
    methods write
      importing
        !iv_obj_type type trobjtype
        !iv_obj_name type sobj_name
        !iv_sub_type type trobjtype
        !iv_sub_name type zif_abapgit_i18n=>ty_sub_name
        !iv_textkey_conf type string
        !it_tobjs type zif_abapgit_i18n=>tt_text_object .
    class-methods get_lang_iso4
      importing
        !iv_src type ty_lang_map-lang
      returning
        value(rv_iso4) type lxeisolang .

  protected section.
  private section.

    data ms_orig_lang type ty_lang_map.
    data mt_alt_langs type tt_lang_map.

    class-methods get_colob
      importing
        iv_obj_type type trobjtype
        iv_obj_name type sobj_name
        iv_sub_type type trobjtype
        iv_sub_name type zif_abapgit_i18n=>ty_sub_name
      returning
        value(rt_colob) type lxe_tt_colob.

    methods merge_portion
      importing
        iv_sub_type type trobjtype
        iv_sub_name type zif_abapgit_i18n=>ty_sub_name
        iv_dev_type type trobjtype
        iv_iso4     type lxeisolang
        it_portion  type lxe_tt_pcx_s1
        io_key_strategy type ref to zcl_abapgit_i18n_key_strategy
      changing
        ct_tobjs type zif_abapgit_i18n=>tt_text_object.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_LXE IMPLEMENTATION.


  method constructor.
    field-symbols <lang> like line of it_alt_langs.
    field-symbols <alt> like line of mt_alt_langs.

    ms_orig_lang-lang = iv_orig_lang.
    ms_orig_lang-iso4 = get_lang_iso4( iv_orig_lang ).

    if it_alt_langs is initial.
      append ms_orig_lang to mt_alt_langs. " Save only master lang
    else.
      loop at it_alt_langs assigning <lang>.
        append initial line to mt_alt_langs assigning <alt>.
        <alt>-lang = <lang>.
        <alt>-iso4 = get_lang_iso4( <lang> ).
      endloop.
    endif.
  endmethod.


  method get_colob.

    data lv_pgmid         type pgmid.
    data lv_full_obj_name type trobj_name.
    data lv_obj_type      type trobjtype.

    assert iv_obj_name is not initial and iv_obj_type is not initial.
    assert not ( iv_sub_type is initial and iv_sub_name is not initial ).

    lv_full_obj_name = iv_obj_name.
    if iv_sub_type is initial.
      lv_pgmid         = 'R3TR'.
      lv_obj_type      = iv_obj_type.
    else.
      lv_pgmid            = 'LIMU'.
      lv_obj_type         = iv_sub_type.
      lv_full_obj_name+40 = iv_sub_name.
    endif.

    call function 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      exporting
        pgmid    = lv_pgmid
        object   = lv_obj_type
        obj_name = lv_full_obj_name
      tables
        ex_colob = rt_colob.

  endmethod.


  method get_lang_iso4.
    data lv_lang1 type sy-langu.
    lv_lang1 = cl_i18n_languages=>sap2_to_sap1( iv_src ).
    call function 'LXE_T002_CONVERT_2_TO_4'
      exporting
        old_r3_lang = lv_lang1
      importing
        new_lang    = rv_iso4.
  endmethod.


  method merge_portion.

    field-symbols <i> like line of it_portion.
    field-symbols <tobj> like line of ct_tobjs.
    field-symbols <text> like line of <tobj>-texts.
    field-symbols <altlang> like line of mt_alt_langs.
    data lv_temp_id like <tobj>-id.

    if ct_tobjs is initial. " First time - fill original language too

      loop at it_portion assigning <i>.
        append initial line to ct_tobjs assigning <tobj>.

        <tobj>-sub_type = iv_sub_type.
        <tobj>-sub_name = iv_sub_name.
        <tobj>-dev_type = iv_dev_type.
        <tobj>-id       = io_key_strategy->build_id(
          iv_sub_type = iv_sub_type
          iv_sub_name = iv_sub_name
          iv_dev_type = iv_dev_type
          iv_textkey  = <i>-textkey
          iv_max_size = <i>-unitmlt ).

        append initial line to <tobj>-texts assigning <text>.
        <text>-lang     = ms_orig_lang-lang.
        <text>-text     = <i>-s_text.
      endloop.

    endif.

    if iv_iso4 <> ms_orig_lang-iso4. " Skip if only orig lang is serialized

      loop at it_portion assigning <i>.
        lv_temp_id = io_key_strategy->build_id(
          iv_sub_type = iv_sub_type
          iv_sub_name = iv_sub_name
          iv_dev_type = iv_dev_type
          iv_textkey  = <i>-textkey
          iv_max_size = <i>-unitmlt ).

        read table ct_tobjs with key id = lv_temp_id assigning <tobj>. " TODO speedup !
        check sy-subrc = 0. " Unexpected key ? Skip.

        read table mt_alt_langs with key iso4 = iv_iso4 assigning <altlang>. " TODO speedup ?
        assert sy-subrc = 0.

        append initial line to <tobj>-texts assigning <text>.
        <text>-lang     = <altlang>-lang.
        <text>-text     = <i>-t_text.
      endloop.

    endif.

  endmethod.


  method read.

    data lt_colob type lxe_tt_colob.
    data lt_tobjs type zif_abapgit_i18n=>tt_text_object.
    data lt_tmp   type lxe_tt_pcx_s1.
    data lo_key_strategy type ref to zcl_abapgit_i18n_key_strategy.

    field-symbols <colob> like line of lt_colob.
    field-symbols <lang> like line of mt_alt_langs.

    lo_key_strategy = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = boolc( iv_sub_type is not initial )
      iv_use_sub_name = boolc( iv_sub_name is not initial )
      iv_textkey_config = iv_textkey_conf ).

    lt_colob = get_colob(
      iv_obj_type = iv_obj_type
      iv_obj_name = iv_obj_name
      iv_sub_type = iv_sub_type
      iv_sub_name = iv_sub_name ).

    loop at lt_colob assigning <colob>.
      clear lt_tobjs.

      loop at mt_alt_langs assigning <lang>.

        call function 'LXE_OBJ_TEXT_PAIR_READ'
          exporting
            s_lang  = ms_orig_lang-iso4
            t_lang  = <lang>-iso4
            custmnr = <colob>-custmnr
            objtype = <colob>-objtype
            objname = <colob>-objname
          tables
            lt_pcx_s1 = lt_tmp.

        merge_portion(
          exporting
            iv_sub_type = iv_sub_type
            iv_sub_name = iv_sub_name
            iv_dev_type = <colob>-objtype
            iv_iso4     = <lang>-iso4
            it_portion  = lt_tmp
            io_key_strategy = lo_key_strategy
          changing
            ct_tobjs = lt_tobjs ).

      endloop.

      append lines of lt_tobjs to rt_tobjs.
    endloop.

  endmethod.


  method write.

    data lo_key_strategy type ref to zcl_abapgit_i18n_key_strategy.
    data lt_dev_types type standard table of zif_abapgit_i18n=>ty_obj_type with default key.
    data lv_dev_type like line of lt_dev_types.

    field-symbols <ls_tobj> like line of it_tobjs.

    lo_key_strategy = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = boolc( iv_sub_type is not initial )
      iv_use_sub_name = boolc( iv_sub_name is not initial )
      iv_textkey_config = iv_textkey_conf ).

    " Validate and collect
    loop at it_tobjs assigning <ls_tobj>.
      if <ls_tobj>-sub_type <> iv_sub_type or <ls_tobj>-sub_name <> iv_sub_name.
        " TODO error
      endif.
      assert <ls_tobj>-dev_type is not initial.
      assert <ls_tobj>-id is not initial.
      assert <ls_tobj>-texts is not initial.

*      append <ls_objs>-dev_type to lt_dev_types.
    endloop.

    sort lt_dev_types.
    delete adjacent duplicates from lt_dev_types.

    loop at lt_dev_types into lv_dev_type. " = Colob

*        call function 'LXE_OBJ_TEXT_PAIR_WRITE'
*          exporting
*            s_lang  = ms_orig_lang-iso4
*            t_lang  = <lang>-iso4
*            custmnr = <colob>-custmnr
*            objtype = <colob>-objtype
*            objname = <colob>-objname
*          tables
*            lt_pcx_s1 = lt_tmp.


    endloop.


  endmethod.
ENDCLASS.
