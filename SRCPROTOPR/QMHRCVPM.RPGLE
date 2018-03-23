000100110623     D ReceiveProgramMessage...
000200110623     D                 PR                  EXTPGM('QMHRCVPM')
000300110623      * Required Parameter Group:
000400110623     D  MessageInformation...                                                   1
000500110623     D                            65535A   OPTIONS(*VARSIZE)
000600110623     D  LengthOfMessageInformation...                                           2
000700110623     D                               10I 0 CONST
000800110623     D  FormatName...                                                           3
000900110623     D                                8A   CONST
001000110623     D  CallStackEntry...                                                       4
001100110623     D                            65535A   CONST OPTIONS(*VARSIZE)
001200110623     D  CallStackCounter...                                                     5
001300110623     D                               10I 0 CONST
001400110623     D  MessageType...                                                          6
001500110623     D                               10A   CONST
001600110623     D  MessageKey...                                                           7
001700110623     D                                4A   CONST
001800110623     D  WaitTime...                                                             8
001900110623     D                               10I 0 CONST
002000110623     D  MessageAction...                                                        9
002100110623     D                               10A   CONST
002200110623     D  errorCode...                                                            10
002300110623     D                            65535A   OPTIONS(*VARSIZE)
002400110623      * Optional Parameter Group 1:
002500110623     D  LengthOfCallStackEntry...                                               11
002600110623     D                               10I 0 CONST OPTIONS(*NOPASS)
002700110623     D  CallStackEntryQualification...                                          12
002800110623     D                               20A   CONST OPTIONS(*NOPASS)
002900110623      * Optional Parameter Group 2:
003000110623     D  CallStackEntryDataType...                                               13
003100110623     D                               10A   CONST OPTIONS(*NOPASS)
003200110623     D  CodedCharacterSetIdentifier...                                          14
003300110623     D                               10I 0 CONST OPTIONS(*NOPASS)
003400110623      * Optional Parameter Group 3:
003500110623     D  AllowDefaultReplyRejection...                                           15
003600110623     D                               10A   CONST OPTIONS(*NOPASS)
