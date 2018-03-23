000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111122     H* TNVRA50R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111122     H*  facendole scadere il giorno prima.                          *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111122     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001100111122     Ffitgt01L  uF a E           K DISK
001200111122      *
001300090401     Ffiapd01L  iF   E           K DISK
001400090401      *
001500090424     FwxFGT00S  o  a E             DISK
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800111122     D KPJBA         E DS
001900081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002000081110     d                                     prefix(STP:3)
002100081110     D FGT_REC         s              9s 0
002200090424     D STP_DATSCAD     s              8s 0
002300081230      *
002400111122     D oldPRG          s                   like(tgtPrg)
002500111122     D newPRG          s                   like(tgtPrg)
002600111123     D esito           s              9s 0
002700111123     D esito_ALF       s              9a
002800090109      *
002900111123     D DATA_eur        S               D   DATFMT(*eur)
003000111123     D DATA_iso        S               D   DATFMT(*iso)
003100111123      *
003200111122     D ficn06ds      E DS
003300111123     C*------------------------------------------------------------------------*
003400111123     C*        - OPERAZIONI INIZIALI
003500111123     C*------------------------------------------------------------------------*
003600111123     C     *ENTRY        PLIST
003700111123     C                   PARM                    DATADEC           8
003800111123     C                   PARM                    AGGIORNA          1
003900111123     C                   PARM                    ESITO_ALF
004000111123     C*
004100111123     C     AGGIORNA      comp      ' '                                    44
004200111123     C                   move      datadec       DATA_dec          8 0
004300111123     C     *iso          movel     DATA_DEC      data_iso
004400111123     c     data_iso      subdur    1:*days       data_iso
004500111123     C     *iso          movel     data_iso      data_sca          8 0
004600111123     C                   z-add     data_sca      DATAscad          8 0
004700111123     C*
004800111123      * duplica la testata TGT
004900111123     C     tgt01key      kLIST
005000111123     C                   kfld                    stpPDR
005100111123     C                   kfld                    stpSML
005200111123     C                   kfld                    stpPRG
005300111123     C*
005400111123     c     Key_APD       KLIST
005500111123     c                   kfld                    apdTIP
005600111123     c                   kfld                    apdPDR
005700111123      *
005800111123     c                   clear                   esito
005900111123     c                   clear                   esito_ALF
006000111123     c                   time                    datetime         14 0
006100111123     c                   move      datetime      data              8 0
006200111123     c                   movel     datetime      ora               6 0
006300111123      *
006400111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006500111123     C                   clear                   UDATE_8           8 0
006600111123     C                   MOVEL     *DATE         UDATE_8
006700111122     c                   clear                   savSTPpdr         7 0
006800081209     C                   SETON                                        LR
006900090423      **
007000090423     C*=================================================================
007100111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007200090423     C*=================================================================
007300090424      *
007400090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007500111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
007600111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
007700111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
007800111123     C+                       IN
007900111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
008000111122     C+   order by Fgtpdr
008100111122     C/END-EXEC
008200111122
008300111122     C*=====
008400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008500111122     C*=====
008600081110     c                   if        sqlCODE < 0
008700081110     c                   eval      esito = Sqlcode
008800111123     c                   move      esito         esito_ALF
008900081110     c                   dump(a)
009000081110     c                   return
009100081110     c                   end
009200081110     C/EXEC SQL
009300081110     C+ OPEN A1
009400081110     C/END-EXEC
009500111122     C*=====
009600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009700111122     C*=====
009800081110     c                   if        sqlCODE < 0
009900081110     c                   eval      esito = Sqlcode
010000111123     c                   move      esito         esito_ALF
010100081110     c                   dump(a)
010200081110     c                   return
010300081110     c                   else
010400111122      *
010500081110     c                   except    testa
010600111122      *
010700081110     c                   end
010800090109      * >>>>>>>
010900081209      * resto nel ciclo finchè....
011000081110     C                   DOU       SqlCod < 0
011100081110      * lettura cursore
011200081110     C/EXEC SQL
011300111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011400081110     C/END-EXEC
011500111122     C*=====
011600111122      *  EoF
011700081110     C                   if        SqlCod = 100
011800081110     c                   Leave
011900111122     C*=====
012000081110      *  x errori
012100081110     C                   ElseIf    SqlCod < 0
012200081110     c                   eval      esito = Sqlcode
012300111123     c                   move      esito         esito_ALF
012400081110     c                   dump(a)
012500081110     c                   leave
012600081110     C                   ENDif
012700090428      *
012800111123      *----------------------------->>>>>
012900111123      *  se sono qui ho letto un record correttamente
013000090428      *----------------------------->>>>>
013100111122      * Controlli su Anagrafica
013200111123     c                   clear                   errore           15
013300111122     c                   exsr      Ctrl_APD
013400090428      *
013500090428      *----------------------------->>>>>
013600090427     c   Of              except    testa
013700090427     c                   except    detta
013800090428      *
013900090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014000111123      *----------------------------->>>>>
014100090428     c                   if        Errore  = *blank
014200090428      *----------------------------->>>>>
014300081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014400081110     c                   if        Aggiorna = 'S'
014500111122      *  Deve scrivere la testata a rottura di autista
014600111123      *-+++++++++++++++--
014700111122     c                   if        stpPDR <> savSTPpdr
014800111123      *-+++++++++++++++--
014900111122      *  salva chiave di rottura
015000111122     c                   eval              savSTPpdr = stpPDR
015100111122      *   Aggancia la testata
015200111122     C     tgt01key      chain     fiTGT01L
015300111122     c                   if        %Found(fiTGT01L)
015400111122      *   Mette in SCADENZA
015500111122     c                   z-add     DATA_sca      tgtDST
015600111122     c                   eval        oldPRG = tgtPRG
015700111122     c                   update    fiTGT000
015800111122      *
015900111122      *   Mette in Decorrenza il nuovo Progressivo
016000111122     c                   eval        tgtPRG = tgtPRG + 1
016100111122     c                   eval        newPRG = tgtPRG
016200111123     c                   movel     DATA_dec      tgtDDT
016300111122     c                   z-add     20391231      tgtDST
016400111122      *
016500111122     c                   z-add     *date         tgtDUV
016600111122     c                   moveL     'New2012'     tgtPRU
016700111122      *
016800111122     c                   z-add     0             TGTPCAR
016900111122     c                   z-add     0             TGTCCKM
017000111122     c                   z-add     0             TGTDTS
017100111122      *
017200111122      *  quindi scrive il progressivo seguente
017300111122     c                   write     fitgt000
017400111122     c                   end
017500111123      *-+++++++++++++++--
017600111122     c                   endIF
017700111122      *-+++++++++++++++--
017800111122      *   deve far scadere il record dell'FGT
017900081218     c     fgt_REC       chain     FIFGT00F
018000081218     c                   if        %Found(FIFGT00F)
018100111122      * aggiorna
018200111123     c                   eval      fgtDST = DATA_sca
018300111122     c                   update    fgtFisico
018400111122      *  copia Tariffa
018500111122     c                   exsr      COPIA_TAR
018600090423     c                   end
018700090112      *
018800090423     c                   endIF
018900090428      *----------------------------->>>>>
019000090428     c                   end
019100090428      *----------------------------->>>>>
019200090424      *  scrive il dettaglio su un file
019300090424     c                   exsr      wri_DETTA
019400081110      *
019500081110     C                   ENDDO
019600081110     C/EXEC SQL
019700081110     C+ Close A1
019800081110     C/END-EXEC
019900081110     C                   if        Esito < 0
020000111123     c                   move      esito         esito_ALF
020100081110     c                   return
020200081110     c                   endIF
020300081110     C*
020400081110     C     FINE          TAG
020500081110     c                   except    finest
020600081110     c                   return
020700090424     C*---------------------------------------------------------------*
020800111122      *  Copia la tariffa
020900090424      *---------------------------------------------------------------*
021000111122     c     COPIA_tar     begSR
021100111122      *
021200111122     c                   clear                   ficn06ds
021300111123     c                   movel     fgtPDR        FILOLD
021400111123     c                   move      fgtPDR        PDROLD
021500111123     c                   move      fgtTSR        TSROLD
021600111123     c                   z-add     fgtCTR        CTROLD
021700111122     c                   z-add     oldPRG        PRGOLD
021800111123     c                   movel     fgtPDR        FILNEW
021900111123     c                   move      fgtPDR        PDRNEW
022000111122     c                   z-add     newPRG        PRGNEW
022100111122      *
022200111123     c                   eval      kpjbu = ficn06ds
022300111122     c                   call      'FICN06R'
022400111122     c                   parm                    KPJBA
022500111122      *
022600111122     c                   endSR
022700111122     C*---------------------------------------------------------------*
022800111122      *  Controlli sull'Anagrafica
022900111122      *---------------------------------------------------------------*
023000111122     c     Ctrl_APD      begSR
023100111122      *
023200111122      * rileva dall'anagrafica
023300111122     c                   eval      apdTIP = 'A'
023400111122     c                   eval      apdPDR = stpPDR
023500111122     c     Key_APD       chain     fiapd01l
023600111122     c                   if        %Found(fiapd01l)
023700111122      *
023800111122     c                   if        apdATB <> *blank
023900111123     c                   movel(p)  'ANNULLATO'   errore
024000111122     c                   else
024100111122      *
024200111122      *-  Esluso da autofatturazione
024300111122     c                   if        apdPDD = 'S'
024400111123     c                   movel(p)  'AUTOFAT'     errore
024500111122     c                   end
024600111122      *
024700111122     c                   endIF
024800111122      *
024900111122     c                   else
025000111123     c                   movel(p)  'NO_APD'      errore
025100111122     c                   end
025200111122      *
025300111122     c                   endSR
025400111122     C*---------------------------------------------------------------*
025500111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
025600111122      *---------------------------------------------------------------*
025700111122     c     WRI_DETTA     begSR
025800111122     C*
025900090424     c                   clear                   WSFGT000
026000090424     C*
026100090424     c                   eval      WSFPDR  =  stpPDR
026200090424     c                   eval      WSFTSR  =  stpTSR
026300090424     c                   eval      WSFCTR  =  stpCTR
026400090424     c                   eval      WSFPRG  = stpPRG
026500090424     c                   eval      WSFDDT  = stpDDT
026600090424     c                   eval      WSFDST  = stpDST
026700090424     c                   eval      WSFDTS  = stpDTS
026800090424     c                   eval      WSFDCN  = stpDCN
026900090424     c                   eval      WSFREC  = FGT_REC
027000111122     c                   eval      WSFPROGR= 'TNVRA50R'
027100111123     c                   eval      WSFtime = datetime
027200090428     c**
027300090428     c                   eval      WSFNOTE = Errore
027400090424     c**
027500090424     c                   write     WSFGT000
027600090424     C*
027700090424     c                   endSR
027800081211     C*---------------------------------------------------------------*
027900081110     OQSYSPRT   E            TESTA            02
028000111123     O                                           10 'TNVRA50R'
028100081110     O                                           25 'Autista'
028200081110     O                                           30 'TSR'
028300081110     O                                           38 'Tariffa'
028400081110     O                                           45 'Progr'
028500081110     O                                           55 'Data_Dec'
028600081110     O                                           65 'Data_Sca'
028700081110     O                                           75 'Data_Stp'
028800081110     O                                           85 'Data_Cnv'
028900111123     O                                          100 'N_Rec_FGT'
029000111123     O                                          111 'N O T E'
029100111123     O                       data               132 '  /  /    '
029200111123     O          E            TESTA       1
029300111123     O                       ora                130 '  :  :  '
029400111123      *
029500081110     O          E            DETta       1
029600081110     O                       stpPDR        z     25
029700081110     O                       stpTSR              29
029800081110     O                       stpCTR              36
029900081110     O                       stpPRG        z     43
030000081110     O                       stpDDT        z     55
030100081110     O                       stpDST        z     65
030200081110     O                       stpDTS        z     75
030300081110     O                       stpDCN        z     85
030400111123     o                       FGT_REC       z    100
030500111123     O                       errore             116
030600081110      ****
030700081110     o          E            FINEST      2
030800111123     o                                            1 ' '
030900111123     o                                          + 0 '*** TNVRA50R ***'
031000111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
031100111123     o                                          + 9 '***   FINE  STAMPA  ***'
031200111123     O                       data               + 9 '  /  /    '
031300111123     O                       ora                + 3 '  :  :  '
031400111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
