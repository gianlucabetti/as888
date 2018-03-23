000100060515      //********************************************************************************************
000200060515      //
000300060515      // Questo programma restituisce il testo di 1o e 2o livello di un messaggio.
000400060515      //
000500060515      //********************************************************************************************
000600060515     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060515
000800060515      //********************************************************************************************
000900060515      //
001000060515      // Definizione procedure usate.
001100060515      //
001200060515      //********************************************************************************************
001300060515     D retrieveMessage...
001400060515     D                 PR                                                       Reperisce la
001500060515     D                                     EXTPGM('QMHRTVM')                    descrizione di un
001600060515     D                                                                          messaggio in formato
001700060515     D                                                                          RTVM0100
001800060515     D  messageInformation...
001900060515     D                            65535A
002000060515     D                                     OPTIONS(*VARSIZE)
002100060515     D  lengthOfMessageInformation...
002200060515     D                               10I 0
002300060515     D  formatName...
002400060515     D                                8A
002500060515     D                                     CONST
002600060515     D  messageIdentifier...
002700060515     D                                7A
002800060515     D  qualifiedMessageFileName...
002900060515     D                               20A
003000060515     D                                     CONST
003100060515     D  replacementData...
003200060516     D                              512A
003300060515     D                                     OPTIONS(*VARSIZE)
003400060515     D  lengthOfReplacementData...
003500060515     D                               10I 0
003600060515     D  replaceSubstitutionValues...
003700060515     D                               10A
003800060515     D                                     CONST
003900060515     D  returnFormatControlCharacters...
004000060515     D                               10A
004100060515     D                                     CONST
004200060515     D  errorCode...
004300060515     D                                     LIKE(qusec)
004400060515     D                                     OPTIONS(*VARSIZE)
004500060515     D  retrieveOption...
004600060515     D                               10A
004700060515     D                                     OPTIONS(*NOPASS)
004800060515     D  ccsidToConvertTo...
004900060515     D                               10I 0
005000060515     D                                     OPTIONS(*NOPASS)
005100060515     D  ccsidOfRplacementData...
005200060515     D                               10I 0
005300060515     D                                     OPTIONS(*NOPASS)
005400060515
005500060515      //********************************************************************************************
005600060515      //
005700060515      // Definizione variabili.
005800060515      //
005900060515      //********************************************************************************************
006000060515     D/COPY QSYSINC/QRPGLESRC,QMHRTVM
006100060515     D/COPY QSYSINC/QRPGLESRC,QUSEC
006200060515     D msgId...
006300060515     D                 S              7A
006400060515     D msgFile...
006500060515     D                 S             10A
006600060515     D msgFileLib...
006700060515     D                 S             10A
006800060515     D msgDta...
006900060515     D                 S            512A
007000060515     D msg...
007100060516     D                 S            644A                                        132+512
007200060515     D secLvl...
007300060516     D                 S           3512A                                        3000+512
007400060515     D messageInformation...
007500060516     D                 S           4180A                                        24+644+3512
007600060515     D lengthOfMessageInformation...
007700060515     D                 S             10I 0
007800060515     D                                     INZ(%SIZE(messageInformation))
007900060515     D lengthOfReplacementData...
008000060515     D                 S             10I 0
008100060515     D                                     INZ(%SIZE(msgDta))
008200060515     D returnFormatControlCharacters...
008300060515     D                 S             10A
008400060515     D                                     INZ('*NO')
008500060515     D retrieveOption...
008600060515     D                 S             10A
008700060515     D                                     INZ('*MSGID')
008800060515     D ccsidToConvertTo...
008900060515     D                 S             10I 0
009000060515     D ccsidOfRplacementData...
009100060515     D                 S             10I 0
009200060515     D rtnCode...
009300060515     D                 S             10A
009400060515     D esito...
009500060516     D                 S             15P 0
009600060515
009700060515      //********************************************************************************************
009800060515      //
009900060515      // Definizione parametri.
010000060515      //
010100060515      //********************************************************************************************
010200060515     C     *ENTRY        PLIST
010300060515     C                   PARM                    msgId                          Id messaggio
010400060515     C                   PARM                    msgFile                        File messaggi
010500060515     C                   PARM                    msgFileLib                     Libreria file messag
010600060515     C                   PARM                    msgDta                         Valori campi dati me
010700060515     C                   PARM                    msg                            Testo 1o livello
010800060515     C                   PARM                    secLvl                         Testo 2o livello
010900060515     C                   PARM                    rtnCode                        Codice di ritorno
011000060515     C                   PARM                    esito                          Esito
011100060515
011200060515      /FREE
011300060516
011400060515       EXSR chkParm;
011500060515       EXSR rtvMsg;
011600060515       EXSR uscita;
011700060515
011800060515       //********************************************************************************************
011900060515       //
012000060515       // Controllo parametri ricevuti.
012100060515       //
012200060515       //********************************************************************************************
012300060515       BEGSR chkParm;
012400060516
012500060515         RESET msg;
012600060515         RESET secLvl;
012700060515         RESET esito;
012800060515         RESET rtnCode;
012900060516
013000060515         IF msgId = *BLANK OR msgFile = *BLANK;
013100060515           esito = -1;
013200060515           rtnCode = 'ERRORE';
013300060515           EXSR uscita;
013400060515         ENDIF;
013500060516
013600060515         IF msgFileLib = *BLANK;
013700060515           msgFileLib = '*LIBL';
013800060515         ENDIF;
013900060516
014000060515       ENDSR;
014100060515
014200060515       //********************************************************************************************
014300060515       //
014400060515       // Reperisco il messaggio.
014500060515       //
014600060515       //********************************************************************************************
014700060515       BEGSR rtvMsg;
014800060516
014900060515         CLEAR qusec;
015000060515         qusbprv = %SIZE(qusec);
015100060515         retrieveMessage(messageInformation:lengthOfMessageInformation
015200060515         :'RTVM0100':msgId:msgFile+msgFileLib:msgDta:lengthOfReplacementData
015300060515         :'*YES':'*NO':qusec);
015400060516
015500060515         IF qusei <> *BLANK;
015600060515           esito = 1;
015700060515           rtnCode = qusei;
015800060515         ELSE;
015900060515           qmhm010004 = messageInformation;
016000060515           msg = %SUBST(messageInformation:25:qmhlmavl02);
016100060515           secLvl = %SUBST(messageInformation:25+qmhlmavl02:qmhlhavl02);
016200060515         ENDIF;
016300060516
016400060515       ENDSR;
016500060515
016600060515       //********************************************************************************************
016700060515       //
016800060515       // Uscita dal programma.
016900060515       //
017000060515       //********************************************************************************************
017100060515       BEGSR uscita;
017200060516
017300060516         IF rtnCode = *BLANK;
017400060516           rtnCode = 'DONE';
017500060516         ENDIF;
017600060516
017700060515         RETURN;
017800060516
017900060515       ENDSR;
018000060515
018100060515      /END-FREE
