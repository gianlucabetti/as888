000100101203     ***************************************************************************
000200101203     **
000300101221     ** Questo programma, se per la società è prevista la trasmissione con un
000400101221     ** provider diverso da CBK Intesa, punta un membro diverso da CBKLIB34/CBITX.
000500101203     **
000600101203     ***************************************************************************
000700101221     H DFTACTGRP(*NO) BNDDIR('PJXBND':'YKCBI')
000800101203
000900101220     ***************************************************************************
001000101220     **
001100101220     ** Costanti.
001200101220     **
001300101220     ***************************************************************************
001400101220      /COPY GAITRASRC/SRCCONST,YKCBITX
001500101220     D PROJ_TIPO_DISTINTA_PORTAFOGLIO_ATTIVO...
001600101220     D                 C                   '1'
001700101220
001800101203     ***************************************************************************
001900101203     **
002000101203     ** Strutture dati.
002100101203     **
002200101203     ***************************************************************************
002300101220     D andiz00f      E DS                  QUALIFIED BASED(nullPtr)
002400101220     D kpjba         E DS                  QUALIFIED
002500101221     D ndck76ds      E DS                  QUALIFIED
002600101220
002700101220     ***************************************************************************
002800101220     **
002900101220     ** Campi.
003000101220     **
003100101220     ***************************************************************************
003200101221      /COPY QSYSINC/QRPGLESRC,QUSEC
003300101220     D cmd             S            255A   VARYING
003301101222     D descrizioneProvider...
003302101222     D                 S             20A   VARYING
003400101220     D errMsg          S            255A   VARYING
003500101220     D esito           S             10I 0
003600101220     D exceptionMessageID...
003700101220     D                 S              7A   IMPORT('_EXCP_MSGID')
003800101222     D db2CountRows    S             10I 0
003804101222     D db2File         S             10A   VARYING
003900101220     D db2Lib          S             10A   VARYING
004000101220     D db2Mbr          S             10A   VARYING
004100101221     D msgStatus       S            255A
004200101221     D provider        S             15A
004300101203
004400101203     ***************************************************************************
004500101203     **
004600101203     ** Prototipi.
004700101203     **
004800101203     ***************************************************************************
004900101221      /COPY GAITRASRC/SRCPROTOPR,NDCK76R
005000101220      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
005100101221      /COPY GAITRASRC/SRCPROTOPR,QUILNGTX
005200101220      /COPY GAITRASRC/SRCPROTOPR,YKCBITX
005300101203
005400101203     ***************************************************************************
005500101203     **
005600101203     ** Parametri.
005700101203     **
005800101203     ***************************************************************************
005900101203     C     *ENTRY        PLIST
006000101203     C                   PARM                    kpjba
006100101203
006200101203     ***************************************************************************
006300101203     **
006400101203     ** Main.
006500101203     **
006600101203     ***************************************************************************
006700101203
006800101203      /FREE
006900101220
007000101222       *INLR = *ON;
007100101222
007200101221       ndck76ds = kpjba.kpjbu;
007300101220
007400101222       IF CBITX_NewSocieta( ndck76ds.societaK76
007500101222                          : PROJ_TIPO_DISTINTA_PORTAFOGLIO_ATTIVO
007501101224                          ) < CBITX_ESITO_OK;
007600101222         CBITX_Finalize();
007700101222         ndck76ds.errK76 = *ON;
007800101221         kpjba.kpjbu = ndck76ds;
007900101220         RETURN;
008000101220       ENDIF;
010100101222
010200101222       IF esito < CBITX_ESITO_OK;
010300101222
010400101222         SELECT;
010500101222           WHEN esito = CBITX_ESITO_ALLOCAZIONE_NON_RIUSCITA;
010600101222             msgStatus = 'Allocazione di '
010700101222                       + db2Lib + '/' + db2File
010800101222                       + ' non riuscita. Impossibile creare flusso CBI. +
010900101222                         Provare tra qualche minuto e se il problema persiste +
011000101222                         contattare il responsabile della applicazione.'
011100101222                       + ' Premere Invio per continuare.'
011200101222                       ;
011300101222           WHEN esito < CBITX_ESITO_OK;
011400101222             msgStatus = 'Errore ' + %CHAR(esito) + '.'
011500101222                       + ' Premere Invio per continuare.'
011600101222                       ;
011700101222         ENDSL;
011800101222
011900101222         DisplayLongText( msgStatus
012000101222                        : %SIZE(msgStatus)
012100101222                        : *BLANK
012200101222                        : *BLANK
012300101222                        : qusec
012400101222                        );
012500101222
012600101222         ndck76ds.errK76 = *ON;
012700101222
012800101222       ELSE;
012801101224
012802101224         CBITX_GetDb2MbrByProvider( PROJ_TIPO_DISTINTA_PORTAFOGLIO_ATTIVO
012803101224                                  : db2Lib : db2File : db2Mbr
012804101224                                  );
012805101224
012806101224         descrizioneProvider = CBITX_GetDescrizioneProvider();
012807101224         db2CountRows = CBITX_GetCountRigheDb2Mbr();
012808101224
012809101224         msgStatus = 'Il provider CBI per questa società è '
012810101224                   + descrizioneProvider + '.'
012811101224                   + ' I flussi CBI saranno scritti nel file '
012812101224                   + db2Lib + '/' + db2File
012813101224                   + ' che contiene ' + %CHAR(db2CountRows) + ' righe.'
012814101224                   + ' Dopo aver creato i flussi CBI tornare immediatamente a +
012815101224                     menù.'
012816101224                   + ' Premere Invio per continuare.'
012817101224                   ;
012818101224
012819101224         DisplayLongText( msgStatus
012820101224                        : %SIZE(msgStatus)
012821101224                        : *BLANK
012822101224                        : *BLANK
012823101224                        : qusec
012824101224                        );
012900101222
013000101222         Proj_CbiTrasmissioneRiba( kpjba );
013100101222         ndck76ds = kpjba.kpjbu;
013101101224
013102101224         db2CountRows = CBITX_GetCountRigheDb2Mbr();
013201101224
013202101224         msgStatus = 'Il file '
013203101224                   + db2Lib + '/' + db2File
013204101224                   + ' contiene ' + %CHAR(db2CountRows) + ' righe pronte per +
013205101224                     essere trasmesse a ' + descrizioneProvider + '.'
013206101224                   + ' Premere Invio per continuare.'
013207101224                   ;
013208101224
013209101224         DisplayLongText( msgStatus
013210101224                        : %SIZE(msgStatus)
013211101224                        : *BLANK
013212101224                        : *BLANK
013213101224                        : qusec
013214101224                        );
013300101222
013400101222       ENDIF;
013500101222
013600101224       CBITX_EndSocieta();
013700101222       CBITX_Finalize();
013800101222       kpjba.kpjbu = ndck76ds;
013900101222
014000101222       RETURN;
014100101203
014200101203       //***********************************************************************
014300101203       //
014400101203       // Inizializzazioni.
014500101203       //
014600101203       //***********************************************************************
014700101203
014800101203       BEGSR *INZSR;
014900101203
015000101221         ndck76ds = kpjba.kpjbu;
015100101221         CBITX_Init(ndck76ds.societaK76 : kpjba : *ON);
015200101203
015300101203       ENDSR;
015400101203
015500101203      /END-FREE
