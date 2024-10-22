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
