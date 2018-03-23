000100170502     D DS_Dir          ds                  qualified
000200170502     D  indirizzario                256
000300170508     D FolderSN        s              1a
000400170502
000500170508        DS_Dir.indirizzario = '/VAS/WS';
000600170508        // DS_Dir.indirizzario = '/BrtPdf/DaElaborare';
000700170509     C                   CALL      'TRULDIRC'
000800170502     C                   PARM                    DS_Dir
000900170508     C                   PARM      'S'           FolderSN
001000170502
001100170502        *inlr = *on;
