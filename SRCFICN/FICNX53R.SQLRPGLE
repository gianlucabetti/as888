000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX50R *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* CONTROLLA le testate TGT se sono state aperte senza          *
000500111123     H*  poi cpiare le tariffe.                                      *
000600081209     H*                                                              *
000700111123     H* riceve come parametro la Data decorrenza e                   *
000800111123     H*    se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111123     Ffitgt00f  iF   E             DISK    rename(FITGT000:TGTFisico)
001100111122      *
001200090401     Ffiapd01L  iF   E           K DISK
001300090401      *
001400090424     FwxFGT00S  o  a E             DISK
001500081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600081211      * -------------------------------------------------------------
001700111122     D KPJBA         E DS
001800111123     D dsTGT         E DS                  ExtName(FITGT00F)
001900081110     d                                     prefix(STP:3)
002000111123     D TGT_REC         s              9s 0
002100090424     D STP_DATSCAD     s              8s 0
002200081230      *
002300111122     D oldPRG          s                   like(tgtPrg)
002400111122     D newPRG          s                   like(tgtPrg)
002500111123     D esito           s              9s 0
002600111123     D esito_ALF       s              9a
002700090109      *
002800111123     D DATA_eur        S               D   DATFMT(*eur)
002900111123     D DATA_iso        S               D   DATFMT(*iso)
003000111123     C*------------------------------------------------------------------------*
003100111123     C*        - OPERAZIONI INIZIALI
003200111123     C*------------------------------------------------------------------------*
003300111123     C     *ENTRY        PLIST
003400111123     C                   PARM                    DATADEC           8
003500111123     C                   PARM                    AGGIORNA          1
003600111123     C                   PARM                    ESITO_ALF
003700111123     C*
003800111123     C     AGGIORNA      comp      ' '                                    44
003900111123     C                   move      datadec       DATA_dec          8 0
004000111123     C     *iso          movel     DATA_DEC      data_iso
004100111123     c     data_iso      subdur    1:*days       data_iso
004200111123     C     *iso          movel     data_iso      data_sca          8 0
004300111123     C                   z-add     data_sca      DATAscad          8 0
004400111123     C*
004500111123      * duplica la testata TGT
004600111123     C     tgt01key      kLIST
004700111123     C                   kfld                    stpPDR
004800111123     C                   kfld                    stpSML
004900111123     C                   kfld                    stpPRG
005000111123     C*
005100111123     c     Key_APD       KLIST
005200111123     c                   kfld                    apdTIP
005300111123     c                   kfld                    apdPDR
005400111123      *
005500111123     c                   clear                   esito
005600111123     c                   clear                   esito_ALF
005700111123     c                   time                    datetime         14 0
005800111123     c                   move      datetime      data              8 0
005900111123     c                   movel     datetime      ora               6 0
006000111123      *
006100111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006200111123     C                   clear                   UDATE_8           8 0
006300111123     C                   MOVEL     *DATE         UDATE_8
006400111122     c                   clear                   savSTPpdr         7 0
006500081209     C                   SETON                                        LR
006600090423      **
006700090423     C*=================================================================
006800111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
006900090423     C*=================================================================
007000090424      *
007100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007200111123     C+ select RRN(fitgt00f), fitgt00f.*
007300111123     C+     from fitgt00f where tgtsml=' ' and tgtpdr*1000 + tgtprg
007400111123     C+          NOT in
007500111123     C+ (select fgtpdr*1000 + fgtprg from fifgt00f where fgtsml=' ')
007600111123     C+  ORDER BY tgtPDR
007700111122     C/END-EXEC
007800111122
007900111122     C*=====
008000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008100111122     C*=====
008200081110     c                   if        sqlCODE < 0
008300081110     c                   eval      esito = Sqlcode
008400111123     c                   move      esito         esito_ALF
008500081110     c                   dump(a)
008600081110     c                   return
008700081110     c                   end
008800081110     C/EXEC SQL
008900081110     C+ OPEN A1
009000081110     C/END-EXEC
009100111122     C*=====
009200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009300111122     C*=====
009400081110     c                   if        sqlCODE < 0
009500081110     c                   eval      esito = Sqlcode
009600111123     c                   move      esito         esito_ALF
009700081110     c                   dump(a)
009800081110     c                   return
009900081110     c                   else
010000111122      *
010100081110     c                   except    testa
010200111122      *
010300081110     c                   end
010400090109      * >>>>>>>
010500081209      * resto nel ciclo finch�....
010600081110     C                   DOU       SqlCod < 0
010700081110      * lettura cursore
010800081110     C/EXEC SQL
010900111123     C+ Fetch Next From A1 Into :TGT_REC, :dsTGT
011000081110     C/END-EXEC
011100111122     C*=====
011200111122      *  EoF
011300081110     C                   if        SqlCod = 100
011400081110     c                   Leave
011500111122     C*=====
011600081110      *  x errori
011700081110     C                   ElseIf    SqlCod < 0
011800081110     c                   eval      esito = Sqlcode
011900111123     c                   move      esito         esito_ALF
012000081110     c                   dump(a)
012100081110     c                   leave
012200081110     C                   ENDif
012300090428      *
012400111123      *----------------------------->>>>>
012500111123      *  se sono qui ho letto un record correttamente
012600090428      *----------------------------->>>>>
012700111122      * Controlli su Anagrafica
012800111123     c                   clear                   errore           15
012900111122     c                   exsr      Ctrl_APD
013000090428      *
013100090428      *----------------------------->>>>>
013200111123      *    Segnala
013300111123     c                   if        Errore = *blank
013400111206     c                   Eval      Errore = 'TGT SENZA FGT'
013500111123     c                   else
013600111123     c                   end
013700111123      *
013800090427     c   Of              except    testa
013900090427     c                   except    detta
014000090428      *----------------------------->>>>>
014100090424      *  scrive il dettaglio su un file
014200090424     c                   exsr      wri_DETTA
014300111206      *
014400111206      *  blocca il processo solo se c'� almeno una tariffa stampata in sede
014500111206     c                   if         tgtdcn >0
014600111123     C                   z-add     *all'1'       esito
014700111206     c                   end
014800081110      *
014900081110     C                   ENDDO
015000081110     C/EXEC SQL
015100081110     C+ Close A1
015200081110     C/END-EXEC
015300111123     c                   except    finest
015400111123     C*
015500081110     C                   if        Esito < 0
015600111123     c                   move      esito         esito_ALF
015700081110     c                   return
015800081110     c                   endIF
015900081110     C*
016000111206     c                   move      esito         esito_ALF
016100111206     c                   if        esito = 0
016200111206     c                   clear                   esito_ALF
016300111206     c                   end
016400111206     C*
016500081110     C     FINE          TAG
016600081110     c                   return
016700111122     C*---------------------------------------------------------------*
016800111122      *  Controlli sull'Anagrafica
016900111122      *---------------------------------------------------------------*
017000111122     c     Ctrl_APD      begSR
017100111122      *
017200111122      * rileva dall'anagrafica
017300111122     c                   eval      apdTIP = 'A'
017400111122     c                   eval      apdPDR = stpPDR
017500111122     c     Key_APD       chain     fiapd01l
017600111122     c                   if        %Found(fiapd01l)
017700111122      *
017800111122     c                   if        apdATB <> *blank
017900111123     c                   movel(p)  'ANNULLATO'   errore
018000111122     c                   else
018100111122      *
018200111122      *-  Esluso da autofatturazione
018300111122     c                   if        apdPDD = 'S'
018400111123     c                   movel(p)  'AUTOFAT'     errore
018500111122     c                   end
018600111122      *
018700111122     c                   endIF
018800111122      *
018900111122     c                   else
019000111123     c                   movel(p)  'NO_APD'      errore
019100111122     c                   end
019200111122      *
019300111122     c                   endSR
019400111122     C*---------------------------------------------------------------*
019500111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
019600111122      *---------------------------------------------------------------*
019700111122     c     WRI_DETTA     begSR
019800111122     C*
019900090424     c                   clear                   WSFGT000
020000090424     C*
020100111123     c                   eval      WSFPDR  = stpPDR
020200111123     c                   eval      WSFTSR  = *blank
020300111123     c                   eval      WSFCTR  = *zero
020400090424     c                   eval      WSFPRG  = stpPRG
020500090424     c                   eval      WSFDDT  = stpDDT
020600090424     c                   eval      WSFDST  = stpDST
020700090424     c                   eval      WSFDTS  = stpDTS
020800090424     c                   eval      WSFDCN  = stpDCN
020900111123     c                   eval      WSFREC  = TGT_REC
021000111129     c                   eval      WSFFILE = 'FITGT00F'
021100111216     c                   eval      WSFPROGR= 'FICNX53R'
021200111123     c                   eval      WSFtime = datetime
021300090428     c**
021400090428     c                   eval      WSFNOTE = Errore
021500090424     c**
021600090424     c                   write     WSFGT000
021700090424     C*
021800090424     c                   endSR
021900111123     C*---------------------------------------------------------------*
022000111123     OQSYSPRT   E            TESTA            02
022100111216     O                                           10 'FICNX53R'
022200111123     O                                           25 'Autista'
022300111123     O                                           45 'Progr'
022400111123     O                                           55 'Data_Dec'
022500111123     O                                           65 'Data_Sca'
022600111123     O                                           75 'Data_Stp'
022700111123     O                                           85 'Data_Cnv'
022800111123     O                                          100 'N_Rec_TGT'
022900111123     O                                          111 'N O T E'
023000111123     O                       data               132 '  /  /    '
023100111123     O          E            TESTA       1
023200111123     O                                            1 ' '
023300111123     O                                          + 0 'SOLO TESTATE  '
023400111123     O                                           25 'da fare'
023500111123     O                                          + 2 'ATTENZIONE'
023600111123     O                       ora                130 '  :  :  '
023700111123      *
023800111123     O          E            DETta       1
023900111123     O                       stpPDR        z     25
024000111123     O                       stpPRG        z     43
024100111123     O                       stpDDT        z     55
024200111123     O                       stpDST        z     65
024300111123     O                       stpDTS        z     75
024400111123     O                       stpDCN        z     85
024500111123     o                       TGT_REC       z    100
024600111123     O                       errore             116
024700111123      ****
024800111123     o          E            FINEST      2
024900111123     o                                            1 ' '
025000111216     o                                          + 0 '*** FICNX53R ***'
025100111123     o                                          + 9 '***  S O L O TESTATE ***'
025200111123     o                                          + 9 '***   FINE  STAMPA  ***'
025300111123     O                       data               + 9 '  /  /    '
025400111123     O                       ora                + 3 '  :  :  '
025500111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
