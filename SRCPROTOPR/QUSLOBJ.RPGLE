000100120511     D ListObjects     PR                  EXTPGM('QUSLOBJ')
000200120511     ** Required Parameter Group:
000300120511     D  qualifiedUserSpaceName...                                               1
000400120511     D                               20A   CONST
000500120511     D  formatName...
000600120511     D                                8A   CONST
000700120511     D  objNameAndLib...
000800120511     D                               20A   CONST
000900120511     D  objType...
001000120511     D                               10A   CONST
001100120511     ** Optional Parameter Group 1:
001200120511     D  errorCode...
001300120511     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
001400120511     ** Optional Parameter Group 2:
001500120511     D  authorityControl...
001600120511     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
001700120511     D  selectionControl...
001800120511     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
001900120511     ** Optional Parameter Group 3:
002000120511     D  auxiliaryStoragePool...
002100120511     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
