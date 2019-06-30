class ZCL_ABAPGIT_I18N_FILE definition
  public
  final
  create public .

  public section.

    methods render
      returning
        value(rv_data) type string.
    methods parse
      importing
        iv_data type string
      raising
        zcx_abapgit_exception.
    methods get
      importing
        iv_sub_type type zif_abapgit_i18n=>ty_text_object-type
        iv_sub_name type zif_abapgit_i18n=>ty_text_object-sub_name
      returning
        value(rt_tab) type zif_abapgit_i18n=>tt_text_object.
    methods put
      importing
        it_tobjs type zif_abapgit_i18n=>tt_text_object.

  protected section.
  private section.
    data mt_text_objects type zif_abapgit_i18n=>tt_text_object.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_FILE IMPLEMENTATION.


  method get.
    " Get all texts related to sub_type+sub_name -> to be saved in one slug
  endmethod.


  method parse.
    " Parse i18n file
  endmethod.


  method put.
    " TODO check duplicates ?
    append lines of it_tobjs to mt_text_objects.
  endmethod.


  method render.

    data lt_lines type string_table.
    data lv_tmp   type string.
    field-symbols <i>    like line of mt_text_objects.
    field-symbols <text> like line of <i>-texts.

    loop at mt_text_objects assigning <i>.
      lv_tmp = |@{ <i>-id }|.
      append lv_tmp to lt_lines.

      loop at <i>-texts assigning <text>.
        lv_tmp = |{ <text>-lang }:{ <text>-text }|.
        append lv_tmp to lt_lines.
      endloop.

      append '' to lt_lines.
    endloop.

    rv_data = concat_lines_of( table = lt_lines sep = cl_abap_char_utilities=>newline ).

  endmethod.
ENDCLASS.
