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
001500111129     FwfFGTXXF  o  a E             DISK
001600090424     FwxFGT00S  o  a E             DISK
001700081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800081211      * -------------------------------------------------------------
001900111122     D KPJBA         E DS
002000081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002100081110     d                                     prefix(STP:3)
002200081110     D FGT_REC         s              9s 0
002300090424     D STP_DATSCAD     s              8s 0
002400081230      *
002500111122     D oldPRG          s                   like(tgtPrg)
002600111122     D newPRG          s                   like(tgtPrg)
002700111123     D esito           s              9s 0
002800111123     D esito_ALF       s              9a
002900090109      *
003000111123     D DATA_eur        S               D   DATFMT(*eur)
003100111123     D DATA_iso        S               D   DATFMT(*iso)
003200111123      *
003300111122     D ficn06ds      E DS
003400111123     C*------------------------------------------------------------------------*
003500111123     C*        - OPERAZIONI INIZIALI
003600111123     C*------------------------------------------------------------------------*
003700111123     C     *ENTRY        PLIST
003800111123     C                   PARM                    DATADEC           8
003900111123     C                   PARM                    AGGIORNA          1
004000111123     C                   PARM                    ESITO_ALF
004100111123     C*
004200111123     C     AGGIORNA      comp      ' '                                    44
004300111123     C                   move      datadec       DATA_dec          8 0
004400111123     C     *iso          movel     DATA_DEC      data_iso
004500111123     c     data_iso      subdur    1:*days       data_iso
004600111123     C     *iso          movel     data_iso      data_sca          8 0
004700111123     C                   z-add     data_sca      DATAscad          8 0
004800111123     C*
004900111123      * duplica la testata TGT
005000111123     C     tgt01key      kLIST
005100111123     C                   kfld                    stpPDR
005200111123     C                   kfld                    stpSML
005300111123     C                   kfld                    stpPRG
005400111123     C*
005500111123     c     Key_APD       KLIST
005600111123     c                   kfld                    apdTIP
005700111123     c                   kfld                    apdPDR
005800111123      *
005900111123     c                   clear                   esito
006000111123     c                   clear                   esito_ALF
006100111123     c                   time                    datetime         14 0
006200111123     c                   move      datetime      data              8 0
006300111123     c                   movel     datetime      ora               6 0
006400111123      *
006500111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006600111123     C                   clear                   UDATE_8           8 0
006700111123     C                   MOVEL     *DATE         UDATE_8
006800111122     c                   clear                   savSTPpdr         7 0
006900081209     C                   SETON                                        LR
007000090423      **
007100090423     C*=================================================================
007200111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007300090423     C*=================================================================
007400090424      *
007500090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007600111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
007700111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
007800111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
007900111123     C+                       IN
008000111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
008100111129     C+           and Fgtpdr  NOT IN
008200111129     C+     (Select fgtpdr from WFFGTxxF)
008300111122     C+   order by Fgtpdr
008400111122     C/END-EXEC
008500111122
008600111122     C*=====
008700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008800111122     C*=====
008900081110     c                   if        sqlCODE < 0
009000081110     c                   eval      esito = Sqlcode
009100111123     c                   move      esito         esito_ALF
009200081110     c                   dump(a)
009300081110     c                   return
009400081110     c                   end
009500081110     C/EXEC SQL
009600081110     C+ OPEN A1
009700081110     C/END-EXEC
009800111122     C*=====
009900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010000111122     C*=====
010100081110     c                   if        sqlCODE < 0
010200081110     c                   eval      esito = Sqlcode
010300111123     c                   move      esito         esito_ALF
010400081110     c                   dump(a)
010500081110     c                   return
010600081110     c                   else
010700111122      *
010800081110     c                   except    testa
010900111122      *
011000081110     c                   end
011100090109      * >>>>>>>
011200081209      * resto nel ciclo finchè....
011300081110     C                   DOU       SqlCod < 0
011400081110      * lettura cursore
011500081110     C/EXEC SQL
011600111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011700081110     C/END-EXEC
011800111122     C*=====
011900111122      *  EoF
012000081110     C                   if        SqlCod = 100
012100081110     c                   Leave
012200111122     C*=====
012300081110      *  x errori
012400081110     C                   ElseIf    SqlCod < 0
012500081110     c                   eval      esito = Sqlcode
012600111123     c                   move      esito         esito_ALF
012700081110     c                   dump(a)
012800081110     c                   leave
012900081110     C                   ENDif
013000090428      *
013100111123      *----------------------------->>>>>
013200111123      *  se sono qui ho letto un record correttamente
013300090428      *----------------------------->>>>>
013400111122      * Controlli su Anagrafica
013500111123     c                   clear                   errore           15
013600111122     c                   exsr      Ctrl_APD
013700090428      *
013800090428      *----------------------------->>>>>
013900090427     c   Of              except    testa
014000090427     c                   except    detta
014100090428      *
014200090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014300111123      *----------------------------->>>>>
014400090428     c                   if        Errore  = *blank
014500090428      *----------------------------->>>>>
014600081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014700081110     c                   if        Aggiorna = 'S'
014800111122      *  Deve scrivere la testata a rottura di autista
014900111123      *-+++++++++++++++--
015000111122     c                   if        stpPDR <> savSTPpdr
015100111123      *-+++++++++++++++--
015200111122      *  salva chiave di rottura
015300111122     c                   eval              savSTPpdr = stpPDR
015400111122      *   Aggancia la testata
015500111122     C     tgt01key      chain     fiTGT01L
015600111122     c                   if        %Found(fiTGT01L)
015700111122      *   Mette in SCADENZA
015800111122     c                   z-add     DATA_sca      tgtDST
015900111122     c                   eval        oldPRG = tgtPRG
016000111122     c                   update    fiTGT000
016100111122      *
016200111122      *   Mette in Decorrenza il nuovo Progressivo
016300111122     c                   eval        tgtPRG = tgtPRG + 1
016400111122     c                   eval        newPRG = tgtPRG
016500111123     c                   movel     DATA_dec      tgtDDT
016600111122     c                   z-add     20391231      tgtDST
016700111122      *
016800111122     c                   z-add     *date         tgtDUV
016900111130     c                   eval        tgtPRU = 'New2012'
017000111122      *
017100111122     c                   z-add     0             TGTPCAR
017200111122     c                   z-add     0             TGTCCKM
017300111130     c                   z-add     *date         TGTDTS
017400111130     c                   z-add     0             TGTDCN
017500111130     c                   z-add     0             TGTDRC
017600111122      *
017700111122      *  quindi scrive il progressivo seguente
017800111122     c                   write     fitgt000
017900111122     c                   end
018000111123      *-+++++++++++++++--
018100111122     c                   endIF
018200111122      *-+++++++++++++++--
018300111122      *   deve far scadere il record dell'FGT
018400081218     c     fgt_REC       chain     FIFGT00F
018500081218     c                   if        %Found(FIFGT00F)
018600111122      * aggiorna
018700111123     c                   eval      fgtDST = DATA_sca
018800111122     c                   update    fgtFisico
018900111122      *  copia Tariffa
019000111122     c                   exsr      COPIA_TAR
019100090423     c                   end
019200090112      *
019300090423     c                   endIF
019400090428      *----------------------------->>>>>
019500090428     c                   end
019600090428      *----------------------------->>>>>
019700090424      *  scrive il dettaglio su un file
019800090424     c                   exsr      wri_DETTA
019900081110      *
020000081110     C                   ENDDO
020100081110     C/EXEC SQL
020200081110     C+ Close A1
020300081110     C/END-EXEC
020400081110     C                   if        Esito < 0
020500111123     c                   move      esito         esito_ALF
020600081110     c                   return
020700081110     c                   endIF
020800081110     C*
020900081110     C     FINE          TAG
021000081110     c                   except    finest
021100081110     c                   return
021200090424     C*---------------------------------------------------------------*
021300111122      *  Copia la tariffa
021400090424      *---------------------------------------------------------------*
021500111122     c     COPIA_tar     begSR
021600111129      *
021700111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
021800111129      *    se si fosse interrotto a metà
021900111129     c                   write     wfFGTxxx
022000111122      *
022100111122     c                   clear                   ficn06ds
022200111123     c                   movel     fgtPDR        FILOLD
022300111123     c                   move      fgtPDR        PDROLD
022400111123     c                   move      fgtTSR        TSROLD
022500111123     c                   z-add     fgtCTR        CTROLD
022600111122     c                   z-add     oldPRG        PRGOLD
022700111123     c                   movel     fgtPDR        FILNEW
022800111123     c                   move      fgtPDR        PDRNEW
022900111122     c                   z-add     newPRG        PRGNEW
023000111122      *
023100111123     c                   eval      kpjbu = ficn06ds
023200111122     c                   call      'FICN06R'
023300111122     c                   parm                    KPJBA
023400111129      *
023500111122     c                   endSR
023600111122     C*---------------------------------------------------------------*
023700111122      *  Controlli sull'Anagrafica
023800111122      *---------------------------------------------------------------*
023900111122     c     Ctrl_APD      begSR
024000111122      *
024100111122      * rileva dall'anagrafica
024200111122     c                   eval      apdTIP = 'A'
024300111122     c                   eval      apdPDR = stpPDR
024400111122     c     Key_APD       chain     fiapd01l
024500111122     c                   if        %Found(fiapd01l)
024600111122      *
024700111122     c                   if        apdATB <> *blank
024800111123     c                   movel(p)  'ANNULLATO'   errore
024900111122     c                   else
025000111122      *
025100111122      *-  Esluso da autofatturazione
025200111122     c                   if        apdPDD = 'S'
025300111123     c                   movel(p)  'AUTOFAT'     errore
025400111122     c                   end
025500111122      *
025600111122     c                   endIF
025700111122      *
025800111122     c                   else
025900111123     c                   movel(p)  'NO_APD'      errore
026000111122     c                   end
026100111122      *
026200111122     c                   endSR
026300111122     C*---------------------------------------------------------------*
026400111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
026500111122      *---------------------------------------------------------------*
026600111122     c     WRI_DETTA     begSR
026700111122     C*
026800090424     c                   clear                   WSFGT000
026900090424     C*
027000090424     c                   eval      WSFPDR  =  stpPDR
027100090424     c                   eval      WSFTSR  =  stpTSR
027200090424     c                   eval      WSFCTR  =  stpCTR
027300090424     c                   eval      WSFPRG  = stpPRG
027400090424     c                   eval      WSFDDT  = stpDDT
027500090424     c                   eval      WSFDST  = stpDST
027600090424     c                   eval      WSFDTS  = stpDTS
027700090424     c                   eval      WSFDCN  = stpDCN
027800111129     c                   eval      WSFFILE = 'FIFGT00F'
027900090424     c                   eval      WSFREC  = FGT_REC
028000111122     c                   eval      WSFPROGR= 'TNVRA50R'
028100111123     c                   eval      WSFtime = datetime
028200090428     c**
028300090428     c                   eval      WSFNOTE = Errore
028400090424     c**
028500090424     c                   write     WSFGT000
028600090424     C*
028700090424     c                   endSR
028800081211     C*---------------------------------------------------------------*
028900081110     OQSYSPRT   E            TESTA            02
029000111123     O                                           10 'TNVRA50R'
029100081110     O                                           25 'Autista'
029200081110     O                                           30 'TSR'
029300081110     O                                           38 'Tariffa'
029400081110     O                                           45 'Progr'
029500081110     O                                           55 'Data_Dec'
029600081110     O                                           65 'Data_Sca'
029700081110     O                                           75 'Data_Stp'
029800081110     O                                           85 'Data_Cnv'
029900111123     O                                          100 'N_Rec_FGT'
030000111123     O                                          111 'N O T E'
030100111123     O                       data               132 '  /  /    '
030200111123     O          E            TESTA       1
030300111123     O                       ora                130 '  :  :  '
030400111123      *
030500081110     O          E            DETta       1
030600081110     O                       stpPDR        z     25
030700081110     O                       stpTSR              29
030800081110     O                       stpCTR              36
030900081110     O                       stpPRG        z     43
031000081110     O                       stpDDT        z     55
031100081110     O                       stpDST        z     65
031200081110     O                       stpDTS        z     75
031300081110     O                       stpDCN        z     85
031400111123     o                       FGT_REC       z    100
031500111123     O                       errore             116
031600081110      ****
031700081110     o          E            FINEST      2
031800111123     o                                            1 ' '
031900111123     o                                          + 0 '*** TNVRA50R ***'
032000111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
032100111123     o                                          + 9 '***   FINE  STAMPA  ***'
032200111123     O                       data               + 9 '  /  /    '
032300111123     O                       ora                + 3 '  :  :  '
032400111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
