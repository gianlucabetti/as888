000100081015     ***************************************************************************
000200081015     **
000300171109     ** cellulari autisti da file CSV
000400081015     **
000500081015     ***************************************************************************
000600081017     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000700081015
000800081015     ***************************************************************************
000900081015     **
001000081015     ** Definizioni costanti.
001100081015     **
001200081015     ***************************************************************************
001500171220     D mailto1         s             50    inz('CEDALERT@BRT.IT')
001501171220     D mailto2         s             50    inz('BARBARA.ARMAROLI@BRT.IT')
001502180213     D mailto3         s             50    inz('MICHELE.MALIPIERO@BRT.IT')
001601171124     D ELABORAZIONE_PROVA...
001602171124     D                 C                   'P'
001700081015     D ESITO_ERROR...
001800081015     D                 C                   -1
001900081015     D ESITO_OK...
002000081015     D                 C                   0
002100081015     D ESITO_WARNING...
002200081015     D                 C                   1
002300081017     D REPLACE...
002400081017     D                 C                   'R'
002700081016     D TRADUZIONE_ESITO_ERROR...
002800081016     D                 C                   '2'
002900081016     D TRADUZIONE_ESITO_OK...
003000081016     D                 C                   '0'
003100081015
003200081015     ***************************************************************************
003300081015     **
003400081015     ** Dichiarazione procedure esterne.
003500081015     **
003600081015     ***************************************************************************
003800081027     D/COPY GAITRASRC/SRCPROTOPR,TRULEMAIL
003900081015     D TraduzioneDati...
004000171109     D                 PR                  EXTPGM('FIDG65R')
004100081015     D  rqsOpCode                    10A
004200081015     D  rpyEsito                     10I 0
004300081015     D  rpyOpCode                    10A
004400081015     D  rpyStatus                    10I 0
004500081016     D  tivlr00f                           LIKEDS(tivlr00f)
004600081016     D ValidazioneDati...
004700171109     D                 PR                  EXTPGM('FIDG66R')
004800081015     D  rqsOpCode                    10A
004900081015     D  rpyEsito                     10I 0
005000081015     D  rpyOpCode                    10A
005100081015     D  rpyStatus                    10I 0
005101151106     D  tivlr00f                           LIKEDS(tivlr00f)
006100081015
006200081015     ***************************************************************************
006300081015     **
006400081015     ** Dichiarazione strutture dati.
006500081015     **
006600081015     ***************************************************************************
006700081027     D kpjba         E DS                  QUALIFIED
006800081027     D                                     BASED(nullPtr)
006900081017     D psds           SDS
007000081017     D  jobName              244    253A
007100081017     D  jobUser              254    263A
007200081017     D  jobNumber            264    269A
007300081017     D tivlr00f      E DS                  QUALIFIED
007400081024     D  vlrPpt_ElaborazioneDiProva...
007500081024     D                                1A   OVERLAY(vlrPpt:1)
007600081024     D  vlrPpt_DelimitatoreCampo...
007700081024     D                                1A   OVERLAY(vlrPpt:2)
007800081024     D  vlrPpt_DelimitatoreStringa...
007900081024     D                                1A   OVERLAY(vlrPpt:3)
008000081024     D  vlrPpt_VirgolaDecimale...
008100081024     D                                1A   OVERLAY(vlrPpt:4)
008200081024
008300081015     ***************************************************************************
008400081015     **
008500081015     ** Dichiarazione variabili.
008600081015     **
008700081015     ***************************************************************************
008800081027     D cEsito          S              1A
008900171109     D oggetto         S             44A   INZ('Cellulari autisti.')
009000081015     D prmEsito        S              1A
009100081015     D prmFir          S             10A
009200081015     D prmLit          S             10A
009300081015     D rpyEsito        S             10I 0
009400081015     D rpyEsito1       S              1A
009500081015     D rpyOpCode       S             10A
009600081015     D rpyStatus       S             10I 0
009700081015     D rqsOpCode       S             10A
010000081015
010100081015     ***************************************************************************
010200081015     **
010300081015     ** Dichiarazione parametri.
010400081015     **
010500081015     ***************************************************************************
010600081015     C     *ENTRY        PLIST
010700081015     C                   PARM                    tivlr00f
010800081015     C                   PARM                    prmEsito
010900081015     C                   PARM                    prmLit
011000081015     C                   PARM                    prmFir
011100081015
011200081015     ***************************************************************************
011300081015     **
011400081015     ** Main.
011500081015     **
011600081015     ***************************************************************************
011700081015      /FREE
011800081015
011900081015       *INLR = *ON;
012000081016
012100081016       CLEAR prmEsito;
015300081017
015301150909       email_setAddressFromUser( jobUser
015302150909                               : REPLACE
015303150909                               );
015304171124       // aggiungo ind. utente alla lista indirizzi mail
015305171220       IF mailto1 <> ' ' ;
015306171220       email_addAddress(mailto1);
015307171124       ENDIF;
015308171220       // aggiungo ind. utente alla lista indirizzi mail
015309171220       IF mailto2 <> ' ' ;
015310171220       email_addAddress(mailto2);
015311171220       ENDIF;
015312180213       // aggiungo ind. utente alla lista indirizzi mail
015313180213       IF mailto3 <> ' ' ;
015314180213       email_addAddress(mailto3);
015315180213       ENDIF;
020800081017       // Traduzione dei dati.
020900081017
021000081017       MONITOR;
021100081016         TraduzioneDati( rqsOpCode
021200081016                       : rpyEsito
021300081016                       : rpyOpCode
021400081016                       : rpyStatus
021500081016                       : tivlr00f
021600081016                       );
021700081016       ON-ERROR;
021800081016         rpyEsito = ESITO_ERROR;
021900081016       ENDMON;
022000081015
022100081015       IF rpyEsito < ESITO_OK;
022200081017         email_send( oggetto
022300081017                   : 'Elaborazione non eseguita.:/N+
022400081017                      Ci sono dei dati non validi nel file CSV.:/N+
022501171124                      Consultare il file WRKQUERY/TL_ERR.'
022600081017                   );
022700081017         prmEsito = TRADUZIONE_ESITO_ERROR;
022800081016         RETURN;
022900081015       ENDIF;
023000081016
023100081016       prmEsito = TRADUZIONE_ESITO_OK;
023200081017
023300081017       // Validazione dei dati tradotti.
023400081016
023500081017       MONITOR;
023600151106         ValidazioneDati( rqsOpCode
023602151106                       : rpyEsito
023603151106                       : rpyOpCode
023604151106                       : rpyStatus
023605151106                       : tivlr00f
023606151106                       );
023700081017       ON-ERROR;
023800081017         rpyEsito = ESITO_ERROR;
023900081017       ENDMON;
024000081015
024100081015       IF rpyEsito < ESITO_OK;
024200081017         email_send( oggetto
024300081017                   : 'Elaborazione non eseguita.:/N+
024400171109                      Dati non validi nella tabella TL*.:/N+
024500081028                      Contattare il responsabile della applicazione.'
024600081017                   );
024700081015         RETURN;
024800081015       ENDIF;
029200081027
029300081017       email_send( oggetto
029400081017                 : 'Elaborazione completata.:/N+
029500081017                    Il dettaglio della elaborazione è disponibile nel file +
029600171109                    FIAPD40F'
029900081024                 );
030000081024
030100081015      /END-FREE
