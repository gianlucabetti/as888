000100110218     D CreateUserSpace...
000200110218     D                 PR                  EXTPGM('QUSCRTUS')
000300101118     ** Required Parameter Group:
000400110218     D qualifiedUserSpaceName...                                                1
000500101118     D                               20A   CONST
000600110218     D extendedAttribute...                                                     2
000700110218     D                               10A   CONST
000800110218     D initialSize...                                                           3
000900101118     D                               10I 0 CONST
001000110218     D initialValue...                                                          4
001100110218     D                                1A   CONST
001200110218     D publicAuthority...                                                       5
001300110218     D                               10A   CONST
001400110218     D textDescription...                                                       6
001500110218     D                               50A   CONST
001600110218     ** Optional Parameter Group 1:
001700110218     D replace...                                                               7
001800110218     D                               10A   CONST OPTIONS(*NOPASS)
001900110218     D errorCode...                                                             8
002000101118     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
002100110218     ** Optional Parameter Group 2:
002200110218     D domain...                                                                9
002300110218     D                               10A   CONST OPTIONS(*NOPASS)
002400110218     ** Optional Parameter Group 3:
002500110218     D transferSizeRequest...                                                   10
002600110218     D                               10I 0 CONST OPTIONS(*NOPASS)
002700110218     D optimumSpaceAlignment...                                                 11
002800110218     D                                1A   CONST OPTIONS(*NOPASS)
