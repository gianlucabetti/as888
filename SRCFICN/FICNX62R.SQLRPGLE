000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX62R *---------------------------------------------------*
000300081110     H*                                                              *
000400111129     H* CONTROLLA i dettagli FPT se NON hanno le relative FGT testate*
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
006300111130     C+     from fifPT00f where fPTsml=' '  and fPTctr = 999
006400111129     C+      and fPTpdr*1000 + fPTprg
006500111123     C+          NOT in
006600111130     C+ (select fgtpdr*1000 + fgtprg from fifgt00f where fgtsml=' ')
006700111129     C+  ORDER BY fPTPDR
006800111122     C/END-EXEC
006900111130
007000111122     C*=====
007100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007200111122     C*=====
007300081110     c                   if        sqlCODE < 0
007400081110     c                   eval      esito = Sqlcode
007500111123     c                   move      esito         esito_ALF
007600081110     c                   dump(a)
007700081110     c                   return
007800081110     c                   end
007900081110     C/EXEC SQL
008000081110     C+ OPEN A1
008100081110     C/END-EXEC
008200111122     C*=====
008300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008400111122     C*=====
008500081110     c                   if        sqlCODE < 0
008600081110     c                   eval      esito = Sqlcode
008700111123     c                   move      esito         esito_ALF
008800081110     c                   dump(a)
008900081110     c                   return
009000081110     c                   else
009100111122      *
009200081110     c                   except    testa
009300111122      *
009400081110     c                   end
009500090109      * >>>>>>>
009600081209      * resto nel ciclo finch�....
009700081110     C                   DOU       SqlCod < 0
009800081110      * lettura cursore
009900081110     C/EXEC SQL
010000111129     C+ Fetch Next From A1 Into :FPT_REC, :dsFPT
010100081110     C/END-EXEC
010200111122     C*=====
010300111122      *  EoF
010400081110     C                   if        SqlCod = 100
010500081110     c                   Leave
010600111122     C*=====
010700081110      *  x errori
010800081110     C                   ElseIf    SqlCod < 0
010900081110     c                   eval      esito = Sqlcode
011000111123     c                   move      esito         esito_ALF
011100081110     c                   dump(a)
011200081110     c                   leave
011300081110     C                   ENDif
011400090428      *
011500111123      *----------------------------->>>>>
011600111123      *  se sono qui ho letto un record correttamente
011700090428      *----------------------------->>>>>
011800111122      * Controlli su Anagrafica
011900111123     c                   clear                   errore           15
012000111122     c                   exsr      Ctrl_APD
012100090428      *
012200090428      *----------------------------->>>>>
012300111123      *    Segnala
012400111123     c                   if        Errore = *blank
012500111206     c                   Eval      Errore = 'FPT senza FGT999'
012600111123     c                   else
012700111123     c                   end
012800111123      *
012900090427     c   Of              except    testa
013000090427     c                   except    detta
013100090428      *----------------------------->>>>>
013200090424      *  scrive il dettaglio su un file
013300090424     c                   exsr      wri_DETTA
013400111123     C                   z-add     *all'1'       esito
013500081110      *
013600081110     C                   ENDDO
013700081110     C/EXEC SQL
013800081110     C+ Close A1
013900081110     C/END-EXEC
014000111123     c                   except    finest
014100111123     C*
014200081110     C                   if        Esito < 0
014300111123     c                   move      esito         esito_ALF
014400081110     c                   return
014500081110     c                   endIF
014600081110     C*
014700111206     c                   move      esito         esito_ALF
014800111206     c                   if        esito = 0
014900111206     c                   clear                   esito_ALF
015000111206     c                   end
015100111206     C*
015200081110     C     FINE          TAG
015300081110     c                   return
015400111122     C*---------------------------------------------------------------*
015500111122      *  Controlli sull'Anagrafica
015600111122      *---------------------------------------------------------------*
015700111122     c     Ctrl_APD      begSR
015800111122      *
015900111122      * rileva dall'anagrafica
016000111122     c                   eval      apdTIP = 'A'
016100111122     c                   eval      apdPDR = stpPDR
016200111122     c     Key_APD       chain     fiapd01l
016300111122     c                   if        %Found(fiapd01l)
016400111122      *
016500111122     c                   if        apdATB <> *blank
016600111123     c                   movel(p)  'ANNULLATO'   errore
016700111122     c                   else
016800111122      *
016900111122      *-  Esluso da autofatturazione
017000111122     c                   if        apdPDD = 'S'
017100111123     c                   movel(p)  'AUTOFAT'     errore
017200111122     c                   end
017300111122      *
017400111122     c                   endIF
017500111122      *
017600111122     c                   else
017700111123     c                   movel(p)  'NO_APD'      errore
017800111122     c                   end
017900111122      *
018000111122     c                   endSR
018100111122     C*---------------------------------------------------------------*
018200111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
018300111122      *---------------------------------------------------------------*
018400111122     c     WRI_DETTA     begSR
018500111122     C*
018600090424     c                   clear                   WSFGT000
018700090424     C*
018800111123     c                   eval      WSFPDR  = stpPDR
018900111129     c                   eval      WSFTSR  =  stpTSR
019000111129     c                   eval      WSFCTR  =  stpCTR
019100090424     c                   eval      WSFPRG  = stpPRG
019200111129     c                   eval      WSFFILE = 'FIFPT00F'
019300111129     c                   eval      WSFREC  = FPT_REC
019400111216     c                   eval      WSFPROGR= 'FICNX62R'
019500111123     c                   eval      WSFtime = datetime
019600090428     c**
019700090428     c                   eval      WSFNOTE = Errore
019800090424     c**
019900090424     c                   write     WSFGT000
020000090424     C*
020100090424     c                   endSR
020200111123     C*---------------------------------------------------------------*
020300111123     OQSYSPRT   E            TESTA            02
020400111216     O                                           10 'FICNX62R'
020500111123     O                                           25 'Autista'
020600111129     O                                           30 'TSR'
020700111129     O                                           38 'Tariffa'
020800111123     O                                           45 'Progr'
020900111123     O                                           55 'Data_Dec'
021000111123     O                                           65 'Data_Sca'
021100111123     O                                           75 'Data_Stp'
021200111123     O                                           85 'Data_Cnv'
021300111129     O                                          100 'N_Rec_FPT'
021400111123     O                                          111 'N O T E'
021500111123     O                       data               132 '  /  /    '
021600111123     O          E            TESTA       1
021700111123     O                                            1 ' '
021800111130     O                                          + 0 'FPT999 NO FGT '
021900111123     O                                           25 'da fare'
022000111123     O                                          + 2 'ATTENZIONE'
022100111123     O                       ora                130 '  :  :  '
022200111123      *
022300111123     O          E            DETta       1
022400111123     O                       stpPDR        z     25
022500111129     O                       stpTSR              29
022600111129     O                       stpCTR              36
022700111123     O                       stpPRG        z     43
022800111129     o                       FPT_REC       z    100
022900111123     O                       errore             116
023000111123      ****
023100111123     o          E            FINEST      2
023200111123     o                                            1 ' '
023300111216     o                                          + 0 '*** FICNX62R ***'
023400111130     o                                          + 9 '***  FPT 999 NO  FGT ***'
023500111123     o                                          + 9 '***   FINE  STAMPA  ***'
023600111123     O                       data               + 9 '  /  /    '
023700111123     O                       ora                + 3 '  :  :  '
023800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
