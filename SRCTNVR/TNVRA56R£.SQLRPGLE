000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111122     H* TNVRA50R *---------------------------------------------------*
000300081110     H*                                                              *
000400111129     H* CONTROLLA i dettagli FGT se NON hanno le relative testate TGT*
000500081209     H*                                                              *
000600111123     H* riceve come parametro la Data decorrenza e                   *
000700111123     H*    se deve eseguire l'aggiornamento                          *
000800000000     H*--------------------------------------------------------------*
000900090401     Ffiapd01L  iF   E           K DISK
001000090401      *
001100090424     FwxFGT00S  o  a E             DISK
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300081211      * -------------------------------------------------------------
001400111122     D KPJBA         E DS
001500111129     D dsFGT         E DS                  ExtName(FIFGT00F)
001600081110     d                                     prefix(STP:3)
001700111129     D FGT_REC         s              9s 0
001800090424     D STP_DATSCAD     s              8s 0
001900081230      *
002000111123     D esito           s              9s 0
002100111123     D esito_ALF       s              9a
002200090109      *
002300111123     D DATA_eur        S               D   DATFMT(*eur)
002400111123     D DATA_iso        S               D   DATFMT(*iso)
002500111123     C*------------------------------------------------------------------------*
002600111123     C*        - OPERAZIONI INIZIALI
002700111123     C*------------------------------------------------------------------------*
002800111123     C     *ENTRY        PLIST
002900111123     C                   PARM                    DATADEC           8
003000111123     C                   PARM                    AGGIORNA          1
003100111123     C                   PARM                    ESITO_ALF
003200111123     C*
003300111123     C     AGGIORNA      comp      ' '                                    44
003400111123     C                   move      datadec       DATA_dec          8 0
003500111123     C     *iso          movel     DATA_DEC      data_iso
003600111123     c     data_iso      subdur    1:*days       data_iso
003700111123     C     *iso          movel     data_iso      data_sca          8 0
003800111123     C                   z-add     data_sca      DATAscad          8 0
003900111123     C*
004000111123     c     Key_APD       KLIST
004100111123     c                   kfld                    apdTIP
004200111123     c                   kfld                    apdPDR
004300111123      *
004400111123     c                   clear                   esito
004500111123     c                   clear                   esito_ALF
004600111123     c                   time                    datetime         14 0
004700111123     c                   move      datetime      data              8 0
004800111123     c                   movel     datetime      ora               6 0
004900111123      *
005000111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
005100111123     C                   clear                   UDATE_8           8 0
005200111123     C                   MOVEL     *DATE         UDATE_8
005300111122     c                   clear                   savSTPpdr         7 0
005400081209     C                   SETON                                        LR
005500090423      **
005600090423     C*=================================================================
005700111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005800090423     C*=================================================================
005900090424      *
006000090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006100111129     C+ select RRN(fifgt00f), fifgt00f.*
006200111129     C+     from fifgt00f where fgtsml=' '
006300111129     C+      and fgttsr in ('C', 'R', 'G')
006400111129     C+      and fgtpdr*1000 + fgtprg
006500111123     C+          NOT in
006600111129     C+ (select tgtpdr*1000 + tgtprg from fitgt00f where tgtsml=' ')
006700111129     C+  ORDER BY fgtPDR
006800111122     C/END-EXEC
006900111122
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
009600081209      * resto nel ciclo finchè....
009700081110     C                   DOU       SqlCod < 0
009800081110      * lettura cursore
009900081110     C/EXEC SQL
010000111129     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
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
012500111129     c                   Eval      Errore = 'SENZA TESTATA TGT'
012600111123     c                   else
012700111123     c                   end
012800111123      *
012900090427     c   Of              except    testa
013000090427     c                   except    detta
013100090428      *----------------------------->>>>>
013200090424      *  scrive il dettaglio su un file
013300090424     c                   exsr      wri_DETTA
013400111123     C                   z-add     *all'1'       esito
013500111123     c                   move      esito         esito_ALF
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
014800081110     C     FINE          TAG
014900081110     c                   return
015000111122     C*---------------------------------------------------------------*
015100111122      *  Controlli sull'Anagrafica
015200111122      *---------------------------------------------------------------*
015300111122     c     Ctrl_APD      begSR
015400111122      *
015500111122      * rileva dall'anagrafica
015600111122     c                   eval      apdTIP = 'A'
015700111122     c                   eval      apdPDR = stpPDR
015800111122     c     Key_APD       chain     fiapd01l
015900111122     c                   if        %Found(fiapd01l)
016000111122      *
016100111122     c                   if        apdATB <> *blank
016200111123     c                   movel(p)  'ANNULLATO'   errore
016300111122     c                   else
016400111122      *
016500111122      *-  Esluso da autofatturazione
016600111122     c                   if        apdPDD = 'S'
016700111123     c                   movel(p)  'AUTOFAT'     errore
016800111122     c                   end
016900111122      *
017000111122     c                   endIF
017100111122      *
017200111122     c                   else
017300111123     c                   movel(p)  'NO_APD'      errore
017400111122     c                   end
017500111122      *
017600111122     c                   endSR
017700111122     C*---------------------------------------------------------------*
017800111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
017900111122      *---------------------------------------------------------------*
018000111122     c     WRI_DETTA     begSR
018100111122     C*
018200090424     c                   clear                   WSFGT000
018300090424     C*
018400111123     c                   eval      WSFPDR  = stpPDR
018500111129     c                   eval      WSFTSR  =  stpTSR
018600111129     c                   eval      WSFCTR  =  stpCTR
018700090424     c                   eval      WSFPRG  = stpPRG
018800090424     c                   eval      WSFDDT  = stpDDT
018900090424     c                   eval      WSFDST  = stpDST
019000090424     c                   eval      WSFDTS  = stpDTS
019100090424     c                   eval      WSFDCN  = stpDCN
019200111129     c                   eval      WSFREC  = FGT_REC
019300111129     c                   eval      WSFPROGR= 'TNVRA55R'
019400111123     c                   eval      WSFtime = datetime
019500090428     c**
019600090428     c                   eval      WSFNOTE = Errore
019700090424     c**
019800090424     c                   write     WSFGT000
019900090424     C*
020000090424     c                   endSR
020100111123     C*---------------------------------------------------------------*
020200111123     OQSYSPRT   E            TESTA            02
020300111129     O                                           10 'TNVRA55R'
020400111123     O                                           25 'Autista'
020500111129     O                                           30 'TSR'
020600111129     O                                           38 'Tariffa'
020700111123     O                                           45 'Progr'
020800111123     O                                           55 'Data_Dec'
020900111123     O                                           65 'Data_Sca'
021000111123     O                                           75 'Data_Stp'
021100111123     O                                           85 'Data_Cnv'
021200111129     O                                          100 'N_Rec_FGT'
021300111123     O                                          111 'N O T E'
021400111123     O                       data               132 '  /  /    '
021500111123     O          E            TESTA       1
021600111123     O                                            1 ' '
021700111123     O                                          + 0 'SOLO TESTATE  '
021800111123     O                                           25 'da fare'
021900111123     O                                          + 2 'ATTENZIONE'
022000111123     O                       ora                130 '  :  :  '
022100111123      *
022200111123     O          E            DETta       1
022300111123     O                       stpPDR        z     25
022400111129     O                       stpTSR              29
022500111129     O                       stpCTR              36
022600111123     O                       stpPRG        z     43
022700111123     O                       stpDDT        z     55
022800111123     O                       stpDST        z     65
022900111123     O                       stpDTS        z     75
023000111123     O                       stpDCN        z     85
023100111129     o                       FGT_REC       z    100
023200111123     O                       errore             116
023300111123      ****
023400111123     o          E            FINEST      2
023500111123     o                                            1 ' '
023600111129     o                                          + 0 '*** TNVRA55R ***'
023700111129     o                                          + 9 '***  SENZA   TESTATE ***'
023800111123     o                                          + 9 '***   FINE  STAMPA  ***'
023900111123     O                       data               + 9 '  /  /    '
024000111123     O                       ora                + 3 '  :  :  '
024100111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
