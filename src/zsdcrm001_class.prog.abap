*&---------------------------------------------------------------------*
*& 包含               ZSDCRM001_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_tree DEFINITION.
  PUBLIC SECTION.
    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key sender.
    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING fieldname node_key.
ENDCLASS.                    "lcl_receiver DEFINITION

CLASS lcl_tree IMPLEMENTATION.
  METHOD handle_node_double_click.
    CLEAR:gs_longtext_im,gs_tree_key.
    READ TABLE gt_tree_key WITH KEY key = node_key INTO gs_tree_key.
    READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY tdid = gs_tree_key-tdid.
    IF gs_longtext_im-longtext IS INITIAL.
      CALL METHOD o_textedit->delete_text.
    ELSE.
      o_textedit->set_textstream( text = gs_longtext_im-longtext ).
    ENDIF.
  ENDMETHOD.                    "handle_node_double_click
  METHOD handle_item_double_click.
    CLEAR:gs_longtext_im,gs_tree_key.
    READ TABLE gt_tree_key WITH KEY key = node_key INTO gs_tree_key.
    READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY tdid = gs_tree_key-tdid.
    IF gs_longtext_im-longtext IS INITIAL.
      CALL METHOD o_textedit->delete_text.
    ELSE.
      o_textedit->set_textstream( text = gs_longtext_im-longtext ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "lcl_receiver IMPLEMENTATION

DATA:o_handle_event TYPE REF TO lcl_tree.

CLASS lcl_simple_tree DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :
      hnd_repo_dblclick
        FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.
ENDCLASS.                    "lcl_application DEFINITION

CLASS lcl_simple_tree IMPLEMENTATION.
  METHOD hnd_repo_dblclick.
    CLEAR:gs_longtext_im,gs_tree_key.
    READ TABLE t_node_repository WITH KEY node_key = node_key INTO s_node_repository.
    READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY tdid = s_node_repository-tdid sapno = s_node_repository-sapno.
    IF gs_longtext_im-longtext IS INITIAL.
      CALL METHOD o_textedit->delete_text.
    ELSE.
      o_textedit->set_textstream( text = gs_longtext_im-longtext ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "lcl_application IMPLEMENTATION

DATA:o_handle_simple_event TYPE REF TO lcl_simple_tree.

CLASS alv_event DEFINITION.
  PUBLIC SECTION.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid"数据改动事件
      IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
    METHODS handle_double_click FOR EVENT double_click  OF cl_gui_alv_grid"双击事件
      IMPORTING e_row e_column es_row_no.
    METHODS handle_on_f4 FOR EVENT onf4 OF cl_gui_alv_grid"F4
      IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid"用户按钮事件
      IMPORTING e_ucomm.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid"热区单击事件
      IMPORTING e_row_id e_column_id es_row_no.
    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid"工具条事件
      IMPORTING e_object e_interactive.
ENDCLASS."alv_event DEFINITION

CLASS alv_event IMPLEMENTATION.
  METHOD handle_data_changed.        "数据改动事件
    PERFORM frm_handle_data_changed USING er_data_changed.
    PERFORM frm_refresh_alv_item .
  ENDMETHOD.
  METHOD handle_double_click.      "双击事件
    PERFORM frm_double_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "HANDLE_USER_COMMAND
  METHOD handle_on_f4.              "F4
    er_event_data->m_event_handled = 'X'.
    ASSIGN er_event_data->m_data->* TO FIELD-SYMBOL(<modi>).
    READ TABLE gt_item INTO DATA(wa) INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.
    CASE e_fieldname.
      WHEN 'MATNR'.
*        PERFORM f4help TABLES return_tab USING  e_fieldname 'GT_ITEM' wa.
    ENDCASE.
    IF return_tab[] IS NOT INITIAL.
*      READ TABLE RETURN_TAB INTO WA_RETURN_TAB INDEX 1.
*      IF SY-SUBRC EQ 0.
*        WA_MODI-ROW_ID = ES_ROW_NO-ROW_ID.
*        WA_MODI-FIELDNAME = E_FIELDNAME.
*        WA_MODI-VALUE = WA_RETURN_TAB-FIELDVAL.
*        APPEND WA_MODI TO <IT_MODI>.
*      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD handle_user_command.       "用户按钮事件
    PERFORM frm_handle_user_command USING e_ucomm.
*    PERFORM frm_refresh_alv_item .
  ENDMETHOD.
  METHOD handle_hotspot_click.       "热区单击事件
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
*    PERFORM frm_refresh_alv_item .
  ENDMETHOD.                    "handle_hotspot_click
  METHOD  handle_toolbar.            "工具条事件
    PERFORM frm_handle_toolbar USING e_object e_interactive."设置工具条图标
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
ENDCLASS. "alv_event IMPLEMENTATION
DATA:lcl_alv_receiver TYPE REF TO alv_event.
*&---------------------------------------------------------------------*
*& Form frm_handle_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM frm_handle_data_changed  USING    p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  LOOP AT p_er_data_changed->mt_mod_cells INTO DATA(mod).
    READ TABLE gt_item ASSIGNING FIELD-SYMBOL(<item>) INDEX mod-row_id.
    CHECK sy-subrc EQ 0.
    CASE mod-fieldname.
      WHEN 'MATNR'.
        SELECT SINGLE
          makt~maktx
          FROM makt
          WHERE spras = @sy-langu
          AND matnr = @mod-value
          INTO @<item>-maktx
          .
        IF sy-subrc NE 0.
          CLEAR <item>-maktx.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM frm_double_click  USING    p_e_row
                                p_e_column
                                p_es_row_no.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_UCOMM
*&---------------------------------------------------------------------*
FORM frm_handle_user_command  USING    p_e_ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_hotspot_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    p_e_row_id
                                    p_e_column_id
                                    p_es_row_no.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_handle_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&---------------------------------------------------------------------*
FORM frm_handle_toolbar  USING    p_e_object
                                  p_e_interactive.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> RETURN_TAB
*&      --> E_FIELDNAME
*&      --> P_
*&      --> WA
*&---------------------------------------------------------------------*
FORM f4help  TABLES   p_return_tab STRUCTURE ddshretval
             USING    p_e_fieldname
                      p_tabname
                      p_wa.
  DATA:retfield    TYPE dfies-fieldname,
       dynprofield TYPE dynfnam.
  FIELD-SYMBOLS:<f4_tab1> TYPE STANDARD TABLE .
  CLEAR:retfield,dynprofield,p_return_tab[].
  CONCATENATE p_tabname '-' p_e_fieldname INTO dynprofield.
  retfield = p_e_fieldname.
  CASE p_e_fieldname.
    WHEN 'MATNR'.

  ENDCASE.
  CHECK <f4_tab1> IS ASSIGNED.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST' "调用函数
    EXPORTING
      retfield        = retfield  "搜索帮助内表要输出的的帮助字段名，注：要大写
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = dynprofield "屏幕字段
      value_org       = 'S'
    TABLES
      value_tab       = <f4_tab1> "存储搜索帮助内容的内表
      return_tab      = p_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
