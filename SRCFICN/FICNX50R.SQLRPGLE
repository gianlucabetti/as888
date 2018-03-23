000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX50R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111122     H*  facendole scadere il giorno prima.                          *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111122     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001100111206     F                                     commit
001200111122     Ffitgt01L  uF a E           K DISK
001300111206     F                                     commit
001400111122      *
001500090401     Ffiapd01L  iF   E           K DISK
001600090401      *
001700111129     FwfFGTXXF  o  a E             DISK
001800090424     FwxFGT00S  o  a E             DISK
001900081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000081211      * -------------------------------------------------------------
002100111122     D KPJBA         E DS
002200081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002300081110     d                                     prefix(STP:3)
002400081110     D FGT_REC         s              9s 0
002500090424     D STP_DATSCAD     s              8s 0
002600081230      *
002700111122     D oldPRG          s                   like(tgtPrg)
002800111122     D newPRG          s                   like(tgtPrg)
002900111123     D esito           s              9s 0
003000111123     D esito_ALF       s              9a
003100090109      *
003200111123     D DATA_eur        S               D   DATFMT(*eur)
003300111123     D DATA_iso        S               D   DATFMT(*iso)
003400111123      *
003500111205     D DA_Copiare      S              1A
003600111213     D DA_SALTARE      S              1A
003700111122     D ficn06ds      E DS
003800111123     C*------------------------------------------------------------------------*
003900111123     C*        - OPERAZIONI INIZIALI
004000111123     C*------------------------------------------------------------------------*
004100111123     C     *ENTRY        PLIST
004200111123     C                   PARM                    DATADEC           8
004300111123     C                   PARM                    AGGIORNA          1
004400111123     C                   PARM                    ESITO_ALF
004500111123     C*
004600111123     C     AGGIORNA      comp      ' '                                    44
004700111123     C                   move      datadec       DATA_dec          8 0
004800111123     C     *iso          movel     DATA_DEC      data_iso
004900111123     c     data_iso      subdur    1:*days       data_iso
005000111123     C     *iso          movel     data_iso      data_sca          8 0
005100111123     C                   z-add     data_sca      DATAscad          8 0
005200111123     C*
005300111123      * duplica la testata TGT
005400111123     C     tgt01key      kLIST
005500111123     C                   kfld                    stpPDR
005600111123     C                   kfld                    stpSML
005700111123     C                   kfld                    stpPRG
005800111123     C*
005900111123     c     Key_APD       KLIST
006000111123     c                   kfld                    apdTIP
006100111123     c                   kfld                    apdPDR
006200111123      *
006300111123     c                   clear                   esito
006400111123     c                   clear                   esito_ALF
006500111123     c                   time                    datetime         14 0
006600111123     c                   move      datetime      data              8 0
006700111123     c                   movel     datetime      ora               6 0
006800111123      *
006900111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007000111123     C                   clear                   UDATE_8           8 0
007100111123     C                   MOVEL     *DATE         UDATE_8
007200111122     c                   clear                   savSTPpdr         7 0
007300081209     C                   SETON                                        LR
007400090423      **
007500090423     C*=================================================================
007600111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007700090423     C*=================================================================
007800111213      *  Aggiunto il pezzo di leggere solo quelle tariffe che hanno il prezzo
007900111213      *   carburante poichè sono quelle rimaste al vecchio modo e quindi da copiare.
008000111213      *
008100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
008200111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
008300111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
008400111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
008500111123     C+                       IN
008600111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
008700111213     C+               and  fgtPDR * 1000 + fgtPRG
008800111213     C+                          IN
008900111213     C+    (Select tgtpdr * 1000 + tgtprg from fiTGT00F
009000111213     C+       where TGTsml=' '  and  TGTPCAR > 0)
009100111129     C+           and Fgtpdr  NOT IN
009200111129     C+     (Select fgtpdr from WFFGTxxF)
009300111122     C+   order by Fgtpdr
009400111122     C/END-EXEC
009500111122
009600111122     C*=====
009700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009800111122     C*=====
009900081110     c                   if        sqlCODE < 0
010000081110     c                   eval      esito = Sqlcode
010100111123     c                   move      esito         esito_ALF
010200081110     c                   dump(a)
010300081110     c                   return
010400081110     c                   end
010500081110     C/EXEC SQL
010600081110     C+ OPEN A1
010700081110     C/END-EXEC
010800111122     C*=====
010900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
011000111122     C*=====
011100081110     c                   if        sqlCODE < 0
011200081110     c                   eval      esito = Sqlcode
011300111123     c                   move      esito         esito_ALF
011400081110     c                   dump(a)
011500081110     c                   return
011600081110     c                   else
011700111122      *
011800081110     c                   except    testa
011900111122      *
012000081110     c                   end
012100090109      * >>>>>>>
012200081209      * resto nel ciclo finchè....
012300081110     C                   DOU       SqlCod < 0
012400081110      * lettura cursore
012500081110     C/EXEC SQL
012600111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
012700081110     C/END-EXEC
012800111122     C*=====
012900111122      *  EoF
013000081110     C                   if        SqlCod = 100
013100081110     c                   Leave
013200111122     C*=====
013300081110      *  x errori
013400081110     C                   ElseIf    SqlCod < 0
013500081110     c                   eval      esito = Sqlcode
013600111123     c                   move      esito         esito_ALF
013700081110     c                   dump(a)
013800081110     c                   leave
013900081110     C                   ENDif
014000090428      *
014100111123      *----------------------------->>>>>
014200111123      *  se sono qui ho letto un record correttamente
014300090428      *----------------------------->>>>>
014400111122      * Controlli su Anagrafica
014500111123     c                   clear                   errore           15
014600111122     c                   exsr      Ctrl_APD
014700090428      *
014800090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014900111123      *----------------------------->>>>>
015000090428     c                   if        Errore  = *blank
015100090428      *----------------------------->>>>>
015200111213      **
015300081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
015400081110     c                   if        Aggiorna = 'S'
015500111122      *  Deve scrivere la testata a rottura di autista
015600111213      **
015700111213      *-+++++++++++++++--
015800111122     c                   if        stpPDR <> savSTPpdr
015900111205     c                   eval      DA_Copiare = 'S'
016000111213     c                   eval      DA_SALTARE = 'N'
016100111123      *-+++++++++++++++--
016200111122      *  salva chiave di rottura
016300111122     c                   eval              savSTPpdr = stpPDR
016400111205      *-+++++++++++++++--
016500111122      *   Aggancia la testata
016600111122     C     tgt01key      chain     fiTGT01L
016700111122     c                   if        %Found(fiTGT01L)
016800111205      *
016900111213      *  A SEGUITO di diversa DECISIONE le tariffe possono essere miste ossia in parte
017000111213      *   già senza il CARBURANTE ....quindi queste NON devono essere copiate.
017100111213      *
017200111213      *
017300111213      *
017400111205      *  Occorre verificare che la tariffa sia stata stampata in sede.
017500111205      *    allora può procedere con il far scadere e riaprire con un altro progressivo.
017600111205      *                    ========
017700111205     c                   IF        tgtDCN > 0
017800111205      *                    ========
017900111122      *   Mette in SCADENZA
018000111122     c                   z-add     DATA_sca      tgtDST
018100111122     c                   eval        oldPRG = tgtPRG
018200111205      *
018300111122     c                   update    fiTGT000
018400111122      *
018500111122      *   Mette in Decorrenza il nuovo Progressivo
018600111122     c                   eval        tgtPRG = tgtPRG + 1
018700111122     c                   eval        newPRG = tgtPRG
018800111123     c                   movel     DATA_dec      tgtDDT
018900111122     c                   z-add     20391231      tgtDST
019000111122      *
019100111122     c                   z-add     *date         tgtDUV
019200111130     c                   eval        tgtPRU = 'New2012'
019300111122      *
019400111122     c                   z-add     0             TGTPCAR
019500111122     c                   z-add     0             TGTCCKM
019600111130     c                   z-add     *date         TGTDTS
019700111130     c                   z-add     0             TGTDCN
019800111130     c                   z-add     0             TGTDRC
019900111122      *
020000111122      *  quindi scrive il progressivo seguente
020100111205      *
020200111122     c                   write     fitgt000
020300111205      *
020400111205      *                    ========
020500111205     c                   ELSE
020600111205      *                    ========
020700111205      ****
020800111205     c                   eval      DA_Copiare = 'N'
020900111205      ****
021000111205      * Altrimenti se NON è stata stampata in sede
021100111205      *   NON genero un nuovo progressivo ma elimino il costo del carburante
021200111205      **  .....è stata convalidata in FILIALE ????!!!!!
021300111205      ****  solo se la scadenza della Tariffa va oltre il limite di conversione.
021400111205      *                    ++++++++
021500111205     c**************     IF        tgtDTS > 0
021600111205      **************       ++++++++
021700111205      *   Se la scadenza della tariffa è superiore al limite di scadenza della conversione
021800111205      **************       ++++++++
021900111205     c                   IF        tgtDST > datascad
022000111205     c                   z-add     0             TGTPCAR
022100111205     c                   z-add     0             TGTCCKM
022200111205     c                   ENDIF
022300111205      ****
022400111205     c                   update    FiTGT000
022500111205      **************       ++++++++
022600111205     c**************     ENDIF
022700111205      *                    ========
022800111205     c                   ENDIF
022900111205      *                    ========
023000111205      *
023100111122     c                   end
023200111123      *-+++++++++++++++--
023300111122     c                   endIF
023400111122      *-+++++++++++++++--
023500111205     c                   if        DA_Copiare = 'S'
023600111122      *   deve far scadere il record dell'FGT
023700081218     c     fgt_REC       chain     FIFGT00F
023800081218     c                   if        %Found(FIFGT00F)
023900111122      * aggiorna
024000111123     c                   eval      fgtDST = DATA_sca
024100111122     c                   update    fgtFisico
024200111122      *  copia Tariffa
024300111122     c                   exsr      COPIA_TAR
024400090423     c                   end
024500111205     c                   end
024600111205      *
024700090423     c                   endIF
024800090428      *----------------------------->>>>>
024900090428     c                   end
025000090428      *----------------------------->>>>>
025100111206     c                   if        errore  = *blank
025200111206     c                   eval        Errore = 'DUPLICA TAR'
025300111206     c                   end
025400111206      *
025500111206     c   Of              except    testa
025600111206     c                   except    detta
025700111206      *---------------------
025800090424      *  scrive il dettaglio su un file
025900090424     c                   exsr      wri_DETTA
026000081110      *
026100081110     C                   ENDDO
026200081110     C/EXEC SQL
026300081110     C+ Close A1
026400081110     C/END-EXEC
026500081110     C                   if        Esito < 0
026600111123     c                   move      esito         esito_ALF
026700081110     c                   return
026800081110     c                   endIF
026900081110     C*
027000081110     C     FINE          TAG
027100081110     c                   except    finest
027200081110     c                   return
027300090424     C*---------------------------------------------------------------*
027400111122      *  Copia la tariffa
027500090424      *---------------------------------------------------------------*
027600111122     c     COPIA_tar     begSR
027700111129      *
027800111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
027900111129      *    se si fosse interrotto a metà
028000111129     c                   write     wfFGTxxx
028100111122      *
028200111122     c                   clear                   ficn06ds
028300111123     c                   movel     fgtPDR        FILOLD
028400111123     c                   move      fgtPDR        PDROLD
028500111123     c                   move      fgtTSR        TSROLD
028600111123     c                   z-add     fgtCTR        CTROLD
028700111122     c                   z-add     oldPRG        PRGOLD
028800111123     c                   movel     fgtPDR        FILNEW
028900111123     c                   move      fgtPDR        PDRNEW
029000111122     c                   z-add     newPRG        PRGNEW
029100111122      *
029200111123     c                   eval      kpjbu = ficn06ds
029300111122     c                   call      'FICN06R'
029400111122     c                   parm                    KPJBA
029500111129      *
029600111122     c                   endSR
029700111122     C*---------------------------------------------------------------*
029800111122      *  Controlli sull'Anagrafica
029900111122      *---------------------------------------------------------------*
030000111122     c     Ctrl_APD      begSR
030100111122      *
030200111122      * rileva dall'anagrafica
030300111122     c                   eval      apdTIP = 'A'
030400111122     c                   eval      apdPDR = stpPDR
030500111122     c     Key_APD       chain     fiapd01l
030600111122     c                   if        %Found(fiapd01l)
030700111122      *
030800111122     c                   if        apdATB <> *blank
030900111123     c                   movel(p)  'ANNULLATO'   errore
031000111122     c                   else
031100111122      *
031200111122      *-  Esluso da autofatturazione
031300111122     c                   if        apdPDD = 'S'
031400111123     c                   movel(p)  'AUTOFAT'     errore
031500111122     c                   end
031600111122      *
031700111122     c                   endIF
031800111122      *
031900111122     c                   else
032000111123     c                   movel(p)  'NO_APD'      errore
032100111122     c                   end
032200111122      *
032300111122     c                   endSR
032400111122     C*---------------------------------------------------------------*
032500111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
032600111122      *---------------------------------------------------------------*
032700111122     c     WRI_DETTA     begSR
032800111122     C*
032900090424     c                   clear                   WSFGT000
033000090424     C*
033100090424     c                   eval      WSFPDR  =  stpPDR
033200090424     c                   eval      WSFTSR  =  stpTSR
033300090424     c                   eval      WSFCTR  =  stpCTR
033400090424     c                   eval      WSFPRG  = stpPRG
033500090424     c                   eval      WSFDDT  = stpDDT
033600090424     c                   eval      WSFDST  = stpDST
033700090424     c                   eval      WSFDTS  = stpDTS
033800090424     c                   eval      WSFDCN  = stpDCN
033900111129     c                   eval      WSFFILE = 'FIFGT00F'
034000090424     c                   eval      WSFREC  = FGT_REC
034100111216     c                   eval      WSFPROGR= 'FICNX50R'
034200111123     c                   eval      WSFtime = datetime
034300090428     c**
034400090428     c                   eval      WSFNOTE = Errore
034500090424     c**
034600090424     c                   write     WSFGT000
034700090424     C*
034800090424     c                   endSR
034900081211     C*---------------------------------------------------------------*
035000081110     OQSYSPRT   E            TESTA            02
035100111216     O                                           10 'FICNX50R'
035200081110     O                                           25 'Autista'
035300081110     O                                           30 'TSR'
035400081110     O                                           38 'Tariffa'
035500081110     O                                           45 'Progr'
035600081110     O                                           55 'Data_Dec'
035700081110     O                                           65 'Data_Sca'
035800081110     O                                           75 'Data_Stp'
035900081110     O                                           85 'Data_Cnv'
036000111123     O                                          100 'N_Rec_FGT'
036100111123     O                                          111 'N O T E'
036200111123     O                       data               132 '  /  /    '
036300111123     O          E            TESTA       1
036400111123     O                       ora                130 '  :  :  '
036500111123      *
036600081110     O          E            DETta       1
036700081110     O                       stpPDR        z     25
036800081110     O                       stpTSR              29
036900081110     O                       stpCTR              36
037000081110     O                       stpPRG        z     43
037100081110     O                       stpDDT        z     55
037200081110     O                       stpDST        z     65
037300081110     O                       stpDTS        z     75
037400081110     O                       stpDCN        z     85
037500111123     o                       FGT_REC       z    100
037600111123     O                       errore             116
037700081110      ****
037800081110     o          E            FINEST      2
037900111123     o                                            1 ' '
038000111216     o                                          + 0 '*** FICNX50R ***'
038100111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
038200111123     o                                          + 9 '***   FINE  STAMPA  ***'
038300111123     O                       data               + 9 '  /  /    '
038400111123     O                       ora                + 3 '  :  :  '
038500111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
