*&---------------------------------------------------------------------*
*& 包含               ZSDCRM001_TOP
*&---------------------------------------------------------------------*
DATA: gt_fldct TYPE lvc_t_fcat,
      gs_slayt TYPE lvc_s_layo,
      gs_varnt TYPE disvariant,
      gv_repid TYPE sy-repid.
DATA:BEGIN OF gs_out.
       INCLUDE TYPE ztcrm_so_head.
DATA:  name_ag      TYPE name1,
       name_we      TYPE name1,
       state        TYPE char5,
       zisdxs_des   TYPE as4text,
       zhtjgfs_des  TYPE as4text,
       zhwlx_des    TYPE as4text,
       zcpyt_des    TYPE as4text,
       zcpsx_des    TYPE as4text,
       vkorg_des    TYPE tvkot-vtext,
       vtweg_des    TYPE tvtwt-vtext,
       vkbur_des    TYPE tvkbt-bezei,
       vkgrp_des    TYPE tvgrt-bezei,
       spart_des    TYPE tspat-vtext,
       kdgrp_des    TYPE t151t-ktext,
       province_des TYPE ztsd226-zname,
       city_des     TYPE ztsd226-zname,
       county_des   TYPE ztsd226-zname,
       sel,
     END OF gs_out,
     gt_out LIKE TABLE OF gs_out.
DATA:BEGIN OF gs_item.
       INCLUDE TYPE ztcrm_so_item.
DATA:  maktx TYPE maktx,
       sel,
     END OF gs_item,
     gt_item LIKE TABLE OF gs_item.
DATA:stbl      TYPE lvc_s_stbl,
     l_ref_alv TYPE REF TO cl_gui_alv_grid.
DATA:save_ok LIKE sy-ucomm,
     ok_code LIKE sy-ucomm.
DATA:o_container          TYPE REF TO cl_gui_custom_container,
     o_splitter           TYPE REF TO cl_gui_splitter_container,
     o_container_tdid     TYPE REF TO cl_gui_container,
     o_container_longtext TYPE REF TO cl_gui_container,
     o_tree_longtext      TYPE REF TO cl_gui_alv_tree,
     o_textedit           TYPE REF TO cl_gui_textedit.
DATA:BEGIN OF s_node_tdid.
       INCLUDE TYPE ttxit.
DATA:  key TYPE lvc_nkey,
     END OF s_node_tdid,
     t_node_tdid LIKE TABLE OF s_node_tdid.

TYPES:BEGIN OF ty_tree_key, "ID与树中KEY的关系
        key  TYPE lvc_nkey,
        tdid TYPE ztlongtext-tdid,
        num  TYPE i,
      END OF ty_tree_key.
DATA:gt_tree_key LIKE SORTED TABLE OF s_node_tdid WITH UNIQUE KEY key,
     gs_tree_key LIKE s_node_tdid.
DATA:lt_ttxit TYPE TABLE OF ttxit.

DATA:o_customcontainer TYPE REF TO cl_gui_custom_container,
     item_container    TYPE REF TO cl_gui_container,
     alv_grid_item     TYPE REF TO cl_gui_alv_grid.
DATA:gt_fldct_item TYPE lvc_t_fcat,
     gs_slayt_item TYPE lvc_s_layo.
DATA:o_tree_repository TYPE REF TO cl_gui_simple_tree.
DATA:BEGIN OF s_node_repository.
       INCLUDE TYPE treev_node. "mtreesnode.
DATA : text(100) TYPE c,
       edit(1)   TYPE c,
       queryid   TYPE ztoad-queryid,
       tdid      TYPE ztlongtext-tdid,
       sapno     TYPE ztlongtext-sapno,
     END OF s_node_repository,
     t_node_repository LIKE TABLE OF s_node_repository.

DATA:BEGIN OF wa_domdes,
       domname    TYPE dd07t-domname,
       domvalue_l TYPE dd07t-domvalue_l,
       ddtext     TYPE dd07t-ddtext,
     END OF wa_domdes,
     lt_domdes LIKE TABLE OF wa_domdes.
DATA:it_f4      TYPE  lvc_t_f4,
     wa_f4      TYPE  lvc_s_f4,
     return_tab TYPE ddshretval OCCURS 0.
DATA:ret2 TYPE TABLE OF bapiret2,
     msg  TYPE bapi_msg.

FIELD-SYMBOLS:<fs> TYPE any.
