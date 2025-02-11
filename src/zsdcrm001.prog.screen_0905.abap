PROCESS BEFORE OUTPUT.
* MODULE STATUS_0110.
*
  MODULE modify_screen.
  MODULE set_list_box.
  MODULE showitems.

PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0110.

*PROCESS ON VALUE-REQUEST.
*
*  FIELD gs_out-province MODULE zprovincef4.
*  FIELD gs_out-city     MODULE zcityf4.
*  FIELD gs_out-county   MODULE zcountyf4.
