000100111011     D OpenListOfJobs  PR                  EXTPGM('QGY/QGYOLJOB')
000200111011     ** Required Parameter Group:
000300111011     D  receiverVariable...                                                     1
000400111011     D                            65535A   OPTIONS(*VARSIZE)
000500111011     D  lengthOfReceiverVariable...                                             2
000600111011     D                               10I 0 CONST
000700111011     D  formatName...                                                           3
000800111011     D                                8A   CONST
000900111011     D  receiverVariableDefinitionInformation...                                4
001000111011     D                            65535A   OPTIONS(*VARSIZE)
001100111011     D  lengthOfReceiverVariableDefinitionInformation...                        5
001200111011     D                               10I 0 CONST
001300111011     D  listInformation...                                                      6
001400111011     D                               80A
001500111011     D  numberOfRecordsToReturn...                                              7
001600111011     D                               10I 0 CONST
001700111011     D  sortInformation...                                                      8
001800111011     D                            65535A   OPTIONS(*VARSIZE) CONST
001900111011     D  jobSelectionInformation...                                              9
002000111011     D                            65535A   OPTIONS(*VARSIZE) CONST
002100111011     D  sizeOfJobSelectionInformation...                                        10
002200111011     D                               10I 0 CONST
002300111011     D  numberOfFieldsToReturn...                                               11
002400111011     D                               10I 0 CONST
002500111011     D  keyOfFieldsToBeReturned...                                              12
002600111011     D                               10I 0 OPTIONS(*VARSIZE) CONST DIM(32767)
002700111011     D  errorCode...                                                            13
002800111011     D                            65535A   OPTIONS(*VARSIZE)
002900111011     ** Optional Parameter Group 1:
003000111011     D  jobSelectionFormatName...                                               14
003100111011     D                                8A   OPTIONS(*NOPASS) CONST
003200111011     ** Optional Parameter Group 2:
003300111011     D  resetStatusStatistics...                                                15
003400111011     D                                1A   OPTIONS(*NOPASS) CONST
003500111011     D  generalReturnData...                                                    16
003600111011     D                            65535A   OPTIONS(*NOPASS:*VARSIZE)
003700111011     D  lengthOfGeneralReturnData...                                            17
003800111011     D                               10I 0 OPTIONS(*NOPASS) CONST
