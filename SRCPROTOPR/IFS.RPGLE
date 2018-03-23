000100090522     D IFS_OpenFile...
000200090522     D                 PR            10I 0 EXTPROC('open')
000300110303     D  path                           *   VALUE OPTIONS(*STRING)
000400110303     D  openFlags                    10I 0 VALUE
000500110303     D  mode                         10U 0 VALUE OPTIONS(*NOPASS)
000600110303     D  ccsid                        10U 0 VALUE OPTIONS(*NOPASS)
000700110113     D  textFileCreationConversionID...
000800110303     D                               10U 0 VALUE OPTIONS(*NOPASS)
000900090522     D IFS_ReadFromDescriptor...
001000090522     D                 PR            10I 0 EXTPROC('read')
001100110303     D  fd                           10I 0 VALUE
001200110303     D  buffer                         *   VALUE
001300110303     D  size                         10U 0 VALUE
001400110112     D IFS_WriteToDescriptor...
001500110112     D                 PR            10I 0 EXTPROC('write')
001600110303     D  fd                           10I 0 VALUE
001700110303     D  buffer                         *   VALUE
001800110303     D  size                         10U 0 VALUE
001900090522     D IFS_CloseFile...
002000090522     D                 PR            10I 0 EXTPROC('close')
002100110303     D  fd                           10I 0 VALUE
002200110303     D IFS_RemoveLinkToFile...
002300110303     D                 PR            10I 0 EXTPROC('unlink')
002400110303     D  path                           *   VALUE OPTIONS(*STRING)
002500110303     D IFS_GetFileInformation...
002600110303     D                 PR            10I 0 EXTPROC('stat')
002700110303     D  path                           *   VALUE OPTIONS(*STRING)
002800110303     D  buff                           *
002900121109     D IFS_ChangeCurrentDirectory...
003000121112     D                 PR            10I 0 EXTPROC('chdir')
003100121109     D  path                           *   VALUE OPTIONS(*STRING)
003200121112     D IFS_RenameFileOrDirectoryUnlink...
003300121112     D                 PR            10I 0 EXTPROC('Qp0lRenameUnlink')
003400121112     D  old                            *   VALUE OPTIONS(*STRING)
003500121112     D  new                            *   VALUE OPTIONS(*STRING)
003600121112     D IFS_RenameFileOrDirectoryKeep...
003700121112     D                 PR            10I 0 EXTPROC('Qp0lRenameKeep')
003800121109     D  old                            *   VALUE OPTIONS(*STRING)
003900121109     D  new                            *   VALUE OPTIONS(*STRING)
