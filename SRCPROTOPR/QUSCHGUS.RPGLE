000100110218     D ChangeUserSpace...
000200110218     D                 PR                  EXTPGM('QUSCHGUS')
000300101118     ** Required Parameter Group:
000400110218     D qualifiedUserSpaceName...                                                1
000500101118     D                               20A   CONST
000600110218     D startingPosition...                                                      2
000700101118     D                               10I 0 CONST
000800110218     D lengthOfData...                                                          3
000900110218     D                               10I 0 CONST
001000110218     D inputData...                                                             4
001100110218     D                            65535A   CONST OPTIONS(*VARSIZE)
001200110218     D forceChangesToAuxiliaryStorage...                                        5
001300110218     D                                1A   CONST
001400110218     ** Optional Parameter Group:
001500110218     D errorCode...                                                             6
001600110218     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
