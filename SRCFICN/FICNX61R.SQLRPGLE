000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX61R *---------------------------------------------------*
000300081110     H*                                                              *
000400111129     H* CONTROLLA i dettagli FPT se NON hanno le relative TGT testate*
000500111129     H*   Tariffe a Dettaglio non a Giornata o a Competenza          *
000600081209     H*                                                              *
000700111123     H* riceve come parametro la Data decorrenza e                   *
000800111123     H*    se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000090401     Ffiapd01L  iF   E           K DISK
001100090401      *
001200090424     FwxFGT00S  o  a E             DISK
001300081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400081211      * -------------------------------------------------------------
001500111122     D KPJBA         E DS
001600111129     D dsFPT         E DS                  ExtName(FIFPT00F)
001700081110     d                                     prefix(STP:3)
001800111129     D FPT_REC         s              9s 0
001900090424     D STP_DATSCAD     s              8s 0
002000081230      *
002100111123     D esito           s              9s 0
002200111123     D esito_ALF       s              9a
002300090109      *
002400111123     D DATA_eur        S               D   DATFMT(*eur)
002500111123     D DATA_iso        S               D   DATFMT(*iso)
002600111123     C*------------------------------------------------------------------------*
002700111123     C*        - OPERAZIONI INIZIALI
002800111123     C*------------------------------------------------------------------------*
002900111123     C     *ENTRY        PLIST
003000111123     C                   PARM                    DATADEC           8
003100111123     C                   PARM                    AGGIORNA          1
003200111123     C                   PARM                    ESITO_ALF
003300111123     C*
003400111123     C     AGGIORNA      comp      ' '                                    44
003500111123     C                   move      datadec       DATA_dec          8 0
003600111123     C     *iso          movel     DATA_DEC      data_iso
003700111123     c     data_iso      subdur    1:*days       data_iso
003800111123     C     *iso          movel     data_iso      data_sca          8 0
003900111123     C                   z-add     data_sca      DATAscad          8 0
004000111123     C*
004100111123     c     Key_APD       KLIST
004200111123     c                   kfld                    apdTIP
004300111123     c                   kfld                    apdPDR
004400111123      *
004500111123     c                   clear                   esito
004600111123     c                   clear                   esito_ALF
004700111123     c                   time                    datetime         14 0
004800111123     c                   move      datetime      data              8 0
004900111123     c                   movel     datetime      ora               6 0
005000111123      *
005100111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
005200111123     C                   clear                   UDATE_8           8 0
005300111123     C                   MOVEL     *DATE         UDATE_8
005400111122     c                   clear                   savSTPpdr         7 0
005500081209     C                   SETON                                        LR
005600090423      **
005700090423     C*=================================================================
005800111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005900090423     C*=================================================================
006000090424      *
006100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006200111129     C+ select RRN(fifPT00f), fifPT00f.*
006300111129     C+     from fifPT00f where fPTsml=' '
006400111129     C+      and fPTtsr in ('C', 'R') and fPTctr <> 999
006500111129     C+      and fPTpdr*1000 + fPTprg
006600111123     C+          NOT in
006700111129     C+ (select tgtpdr*1000 + tgtprg from fitgt00f where tgtsml=' ')
006800111129     C+  ORDER BY fPTPDR
006900111122     C/END-EXEC
007000111122
007100111122     C*=====
007200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007300111122     C*=====
007400081110     c                   if        sqlCODE < 0
007500081110     c                   eval      esito = Sqlcode
007600111123     c                   move      esito         esito_ALF
007700081110     c                   dump(a)
007800081110     c                   return
007900081110     c                   end
008000081110     C/EXEC SQL
008100081110     C+ OPEN A1
008200081110     C/END-EXEC
008300111122     C*=====
008400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008500111122     C*=====
008600081110     c                   if        sqlCODE < 0
008700081110     c                   eval      esito = Sqlcode
008800111123     c                   move      esito         esito_ALF
008900081110     c                   dump(a)
009000081110     c                   return
009100081110     c                   else
009200111122      *
009300081110     c                   except    testa
009400111122      *
009500081110     c                   end
009600090109      * >>>>>>>
009700081209      * resto nel ciclo finchè....
009800081110     C                   DOU       SqlCod < 0
009900081110      * lettura cursore
010000081110     C/EXEC SQL
010100111129     C+ Fetch Next From A1 Into :FPT_REC, :dsFPT
010200081110     C/END-EXEC
010300111122     C*=====
010400111122      *  EoF
010500081110     C                   if        SqlCod = 100
010600081110     c                   Leave
010700111122     C*=====
010800081110      *  x errori
010900081110     C                   ElseIf    SqlCod < 0
011000081110     c                   eval      esito = Sqlcode
011100111123     c                   move      esito         esito_ALF
011200081110     c                   dump(a)
011300081110     c                   leave
011400081110     C                   ENDif
011500090428      *
011600111123      *----------------------------->>>>>
011700111123      *  se sono qui ho letto un record correttamente
011800090428      *----------------------------->>>>>
011900111122      * Controlli su Anagrafica
012000111123     c                   clear                   errore           15
012100111122     c                   exsr      Ctrl_APD
012200090428      *
012300090428      *----------------------------->>>>>
012400111123      *    Segnala
012500111123     c                   if        Errore = *blank
012600111206     c                   Eval      Errore = 'FPT Senza TGT dett'
012700111123     c                   else
012800111123     c                   end
012900111123      *
013000090427     c   Of              except    testa
013100090427     c                   except    detta
013200090428      *----------------------------->>>>>
013300090424      *  scrive il dettaglio su un file
013400090424     c                   exsr      wri_DETTA
013500111123     C                   z-add     *all'1'       esito
013600081110      *
013700081110     C                   ENDDO
013800081110     C/EXEC SQL
013900081110     C+ Close A1
014000081110     C/END-EXEC
014100111123     c                   except    finest
014200111123     C*
014300081110     C                   if        Esito < 0
014400111123     c                   move      esito         esito_ALF
014500081110     c                   return
014600081110     c                   endIF
014700081110     C*
014800111206     c                   move      esito         esito_ALF
014900111206     c                   if        esito = 0
015000111206     c                   clear                   esito_ALF
015100111206     c                   end
015200111206     C*
015300081110     C     FINE          TAG
015400081110     c                   return
015500111122     C*---------------------------------------------------------------*
015600111122      *  Controlli sull'Anagrafica
015700111122      *---------------------------------------------------------------*
015800111122     c     Ctrl_APD      begSR
015900111122      *
016000111122      * rileva dall'anagrafica
016100111122     c                   eval      apdTIP = 'A'
016200111122     c                   eval      apdPDR = stpPDR
016300111122     c     Key_APD       chain     fiapd01l
016400111122     c                   if        %Found(fiapd01l)
016500111122      *
016600111122     c                   if        apdATB <> *blank
016700111123     c                   movel(p)  'ANNULLATO'   errore
016800111122     c                   else
016900111122      *
017000111122      *-  Esluso da autofatturazione
017100111122     c                   if        apdPDD = 'S'
017200111123     c                   movel(p)  'AUTOFAT'     errore
017300111122     c                   end
017400111122      *
017500111122     c                   endIF
017600111122      *
017700111122     c                   else
017800111123     c                   movel(p)  'NO_APD'      errore
017900111122     c                   end
018000111122      *
018100111122     c                   endSR
018200111122     C*---------------------------------------------------------------*
018300111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
018400111122      *---------------------------------------------------------------*
018500111122     c     WRI_DETTA     begSR
018600111122     C*
018700090424     c                   clear                   WSFGT000
018800090424     C*
018900111123     c                   eval      WSFPDR  = stpPDR
019000111129     c                   eval      WSFTSR  =  stpTSR
019100111129     c                   eval      WSFCTR  =  stpCTR
019200090424     c                   eval      WSFPRG  = stpPRG
019300111129     c                   eval      WSFFILE = 'FIFPT00F'
019400111129     c                   eval      WSFREC  = FPT_REC
019500111216     c                   eval      WSFPROGR= 'FICNX61R'
019600111123     c                   eval      WSFtime = datetime
019700090428     c**
019800090428     c                   eval      WSFNOTE = Errore
019900090424     c**
020000090424     c                   write     WSFGT000
020100090424     C*
020200090424     c                   endSR
020300111123     C*---------------------------------------------------------------*
020400111123     OQSYSPRT   E            TESTA            02
020500111216     O                                           10 'FICNX61R'
020600111123     O                                           25 'Autista'
020700111129     O                                           30 'TSR'
020800111129     O                                           38 'Tariffa'
020900111123     O                                           45 'Progr'
021000111123     O                                           55 'Data_Dec'
021100111123     O                                           65 'Data_Sca'
021200111123     O                                           75 'Data_Stp'
021300111123     O                                           85 'Data_Cnv'
021400111129     O                                          100 'N_Rec_FPT'
021500111123     O                                          111 'N O T E'
021600111123     O                       data               132 '  /  /    '
021700111123     O          E            TESTA       1
021800111123     O                                            1 ' '
021900111129     O                                          + 0 'FPT senza TGT '
022000111123     O                                           25 'da fare'
022100111123     O                                          + 2 'ATTENZIONE'
022200111123     O                       ora                130 '  :  :  '
022300111123      *
022400111123     O          E            DETta       1
022500111123     O                       stpPDR        z     25
022600111129     O                       stpTSR              29
022700111129     O                       stpCTR              36
022800111123     O                       stpPRG        z     43
022900111129     o                       FPT_REC       z    100
023000111123     O                       errore             116
023100111123      ****
023200111123     o          E            FINEST      2
023300111123     o                                            1 ' '
023400111216     o                                          + 0 '*** FICNX61R ***'
023500111129     o                                          + 9 '***  FPT  Senza  TGT ***'
023600111123     o                                          + 9 '***   FINE  STAMPA  ***'
023700111123     O                       data               + 9 '  /  /    '
023800111123     O                       ora                + 3 '  :  :  '
023900111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
