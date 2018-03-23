000100090115     D SendProgramMessage...
000200090115     D                 PR                  EXTPGM('QMHSNDPM')
000300090115     ** Required Parameters Group:
000400090115     D  MessageIdentifier...                                                    1
000500090115     D                                7A   CONST
000600090115     D  QualifiedMessageFileName...                                             2
000700090115     D                               20A   CONST
000800090115     D  MessageDataOrImmediateText...                                           3
000900090115     D                            32767A   CONST
001000090115     D                                     OPTIONS(*VARSIZE)
001100090115     D  LengthOfMessageDataOrImmediateText...                                   4
001200090115     D                               10I 0 CONST
001300090115     D  MessageType...                                                          5
001400090115     D                               10A   CONST
001500090115     D  CallStackEntry...                                                       6
001600090115     D                            32767A   CONST
001700090115     D                                     OPTIONS(*VARSIZE)
001800090115     D  CallStackCounter...                                                     7
001900090115     D                               10I 0 CONST
002000090115     D  MessageKey...                                                           8
002100090115     D                                4A
002200090115     D  ErrorCode...                                                            9
002300090115     D                            32767A   OPTIONS(*VARSIZE)
002400090115     ** Optional Parameter Group 1:
002500090115     D  LengthOfCallStackEntry...                                               10
002600090115     D                               10I 0 CONST
002700090115     D                                      OPTIONS(*NOPASS)
002800090115     D  CallStackEntryQualification...                                          11
002900090115     D                               20A   CONST
003000090115     D                                      OPTIONS(*NOPASS)
003100090115     D  DisplayProgramMessagesScreenWaitTime...                                 12
003200090115     D                               10I 0 CONST
003300090115     D                                      OPTIONS(*NOPASS)
003400090115     ** Optional Parameter Group 2:
003500090115     D  CallStackEntryDataType...                                               13
003600090115     D                               10A   CONST
003700090115     D                                      OPTIONS(*NOPASS)
003800090115     D  CodedCharacterSetIdentifier...                                          14
003900090115     D                               10I 0 CONST
004000090115     D                                      OPTIONS(*NOPASS)
