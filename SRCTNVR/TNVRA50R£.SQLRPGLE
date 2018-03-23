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
003600111122     D ficn06ds      E DS
003700111123     C*------------------------------------------------------------------------*
003800111123     C*        - OPERAZIONI INIZIALI
003900111123     C*------------------------------------------------------------------------*
004000111123     C     *ENTRY        PLIST
004100111123     C                   PARM                    DATADEC           8
004200111123     C                   PARM                    AGGIORNA          1
004300111123     C                   PARM                    ESITO_ALF
004400111123     C*
004500111123     C     AGGIORNA      comp      ' '                                    44
004600111123     C                   move      datadec       DATA_dec          8 0
004700111123     C     *iso          movel     DATA_DEC      data_iso
004800111123     c     data_iso      subdur    1:*days       data_iso
004900111123     C     *iso          movel     data_iso      data_sca          8 0
005000111123     C                   z-add     data_sca      DATAscad          8 0
005100111123     C*
005200111123      * duplica la testata TGT
005300111123     C     tgt01key      kLIST
005400111123     C                   kfld                    stpPDR
005500111123     C                   kfld                    stpSML
005600111123     C                   kfld                    stpPRG
005700111123     C*
005800111123     c     Key_APD       KLIST
005900111123     c                   kfld                    apdTIP
006000111123     c                   kfld                    apdPDR
006100111123      *
006200111123     c                   clear                   esito
006300111123     c                   clear                   esito_ALF
006400111123     c                   time                    datetime         14 0
006500111123     c                   move      datetime      data              8 0
006600111123     c                   movel     datetime      ora               6 0
006700111123      *
006800111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006900111123     C                   clear                   UDATE_8           8 0
007000111123     C                   MOVEL     *DATE         UDATE_8
007100111122     c                   clear                   savSTPpdr         7 0
007200081209     C                   SETON                                        LR
007300090423      **
007400090423     C*=================================================================
007500111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007600090423     C*=================================================================
007700090424      *
007800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007900111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
008000111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
008100111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
008200111123     C+                       IN
008300111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
008400111129     C+           and Fgtpdr  NOT IN
008500111129     C+     (Select fgtpdr from WFFGTxxF)
008600111122     C+   order by Fgtpdr
008700111122     C/END-EXEC
008800111122
008900111122     C*=====
009000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009100111122     C*=====
009200081110     c                   if        sqlCODE < 0
009300081110     c                   eval      esito = Sqlcode
009400111123     c                   move      esito         esito_ALF
009500081110     c                   dump(a)
009600081110     c                   return
009700081110     c                   end
009800081110     C/EXEC SQL
009900081110     C+ OPEN A1
010000081110     C/END-EXEC
010100111122     C*=====
010200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010300111122     C*=====
010400081110     c                   if        sqlCODE < 0
010500081110     c                   eval      esito = Sqlcode
010600111123     c                   move      esito         esito_ALF
010700081110     c                   dump(a)
010800081110     c                   return
010900081110     c                   else
011000111122      *
011100081110     c                   except    testa
011200111122      *
011300081110     c                   end
011400090109      * >>>>>>>
011500081209      * resto nel ciclo finchè....
011600081110     C                   DOU       SqlCod < 0
011700081110      * lettura cursore
011800081110     C/EXEC SQL
011900111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
012000081110     C/END-EXEC
012100111122     C*=====
012200111122      *  EoF
012300081110     C                   if        SqlCod = 100
012400081110     c                   Leave
012500111122     C*=====
012600081110      *  x errori
012700081110     C                   ElseIf    SqlCod < 0
012800081110     c                   eval      esito = Sqlcode
012900111123     c                   move      esito         esito_ALF
013000081110     c                   dump(a)
013100081110     c                   leave
013200081110     C                   ENDif
013300090428      *
013400111123      *----------------------------->>>>>
013500111123      *  se sono qui ho letto un record correttamente
013600090428      *----------------------------->>>>>
013700111122      * Controlli su Anagrafica
013800111123     c                   clear                   errore           15
013900111122     c                   exsr      Ctrl_APD
014000090428      *
014100090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014200111123      *----------------------------->>>>>
014300090428     c                   if        Errore  = *blank
014400090428      *----------------------------->>>>>
014500081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014600081110     c                   if        Aggiorna = 'S'
014700111122      *  Deve scrivere la testata a rottura di autista
014800111123      *-+++++++++++++++--
014900111122     c                   if        stpPDR <> savSTPpdr
015000111205     c                   eval      DA_Copiare = 'S'
015100111123      *-+++++++++++++++--
015200111122      *  salva chiave di rottura
015300111122     c                   eval              savSTPpdr = stpPDR
015400111205      *-+++++++++++++++--
015500111122      *   Aggancia la testata
015600111122     C     tgt01key      chain     fiTGT01L
015700111205      *
015800111122     c                   if        %Found(fiTGT01L)
015900111205      *
016000111205      *  Occorre verificare che la tariffa sia stata stampata in sede.
016100111205      *    allora può procedere con il far scadere e riaprire con un altro progressivo.
016200111205      *                    ========
016300111205     c                   IF        tgtDCN > 0
016400111205      *                    ========
016500111122      *   Mette in SCADENZA
016600111122     c                   z-add     DATA_sca      tgtDST
016700111122     c                   eval        oldPRG = tgtPRG
016800111205      *
016900111122     c                   update    fiTGT000
017000111122      *
017100111122      *   Mette in Decorrenza il nuovo Progressivo
017200111122     c                   eval        tgtPRG = tgtPRG + 1
017300111122     c                   eval        newPRG = tgtPRG
017400111123     c                   movel     DATA_dec      tgtDDT
017500111122     c                   z-add     20391231      tgtDST
017600111122      *
017700111122     c                   z-add     *date         tgtDUV
017800111130     c                   eval        tgtPRU = 'New2012'
017900111122      *
018000111122     c                   z-add     0             TGTPCAR
018100111122     c                   z-add     0             TGTCCKM
018200111130     c                   z-add     *date         TGTDTS
018300111130     c                   z-add     0             TGTDCN
018400111130     c                   z-add     0             TGTDRC
018500111122      *
018600111122      *  quindi scrive il progressivo seguente
018700111205      *
018800111122     c                   write     fitgt000
018900111205      *
019000111205      *                    ========
019100111205     c                   ELSE
019200111205      *                    ========
019300111205      ****
019400111205     c                   eval      DA_Copiare = 'N'
019500111205      ****
019600111205      * Altrimenti se NON è stata stampata in sede
019700111205      *   NON genero un nuovo progressivo ma elimino il costo del carburante
019800111205      **  .....è stata convalidata in FILIALE ????!!!!!
019900111205      ****  solo se la scadenza della Tariffa va oltre il limite di conversione.
020000111205      *                    ++++++++
020100111205     c**************     IF        tgtDTS > 0
020200111205      **************       ++++++++
020300111205      *   Se la scadenza della tariffa è superiore al limite di scadenza della conversione
020400111205      **************       ++++++++
020500111205     c                   IF        tgtDST > datascad
020600111205     c                   z-add     0             TGTPCAR
020700111205     c                   z-add     0             TGTCCKM
020800111205     c                   ENDIF
020900111205      ****
021000111205     c                   update    FiTGT000
021100111205      **************       ++++++++
021200111205     c**************     ENDIF
021300111205      *                    ========
021400111205     c                   ENDIF
021500111205      *                    ========
021600111205      *
021700111122     c                   end
021800111123      *-+++++++++++++++--
021900111122     c                   endIF
022000111122      *-+++++++++++++++--
022100111205     c                   if        DA_Copiare = 'S'
022200111122      *   deve far scadere il record dell'FGT
022300081218     c     fgt_REC       chain     FIFGT00F
022400081218     c                   if        %Found(FIFGT00F)
022500111122      * aggiorna
022600111123     c                   eval      fgtDST = DATA_sca
022700111122     c                   update    fgtFisico
022800111122      *  copia Tariffa
022900111122     c                   exsr      COPIA_TAR
023000090423     c                   end
023100111205     c                   end
023200111205      *
023300090423     c                   endIF
023400090428      *----------------------------->>>>>
023500090428     c                   end
023600090428      *----------------------------->>>>>
023700111206     c                   if        errore  = *blank
023800111206     c                   eval        Errore = 'DUPLICA TAR'
023900111206     c                   end
024000111206      *
024100111206     c   Of              except    testa
024200111206     c                   except    detta
024300111206      *---------------------
024400090424      *  scrive il dettaglio su un file
024500090424     c                   exsr      wri_DETTA
024600081110      *
024700081110     C                   ENDDO
024800081110     C/EXEC SQL
024900081110     C+ Close A1
025000081110     C/END-EXEC
025100081110     C                   if        Esito < 0
025200111123     c                   move      esito         esito_ALF
025300081110     c                   return
025400081110     c                   endIF
025500081110     C*
025600081110     C     FINE          TAG
025700081110     c                   except    finest
025800081110     c                   return
025900090424     C*---------------------------------------------------------------*
026000111122      *  Copia la tariffa
026100090424      *---------------------------------------------------------------*
026200111122     c     COPIA_tar     begSR
026300111129      *
026400111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
026500111129      *    se si fosse interrotto a metà
026600111129     c                   write     wfFGTxxx
026700111122      *
026800111122     c                   clear                   ficn06ds
026900111123     c                   movel     fgtPDR        FILOLD
027000111123     c                   move      fgtPDR        PDROLD
027100111123     c                   move      fgtTSR        TSROLD
027200111123     c                   z-add     fgtCTR        CTROLD
027300111122     c                   z-add     oldPRG        PRGOLD
027400111123     c                   movel     fgtPDR        FILNEW
027500111123     c                   move      fgtPDR        PDRNEW
027600111122     c                   z-add     newPRG        PRGNEW
027700111122      *
027800111123     c                   eval      kpjbu = ficn06ds
027900111122     c                   call      'FICN06R'
028000111122     c                   parm                    KPJBA
028100111129      *
028200111122     c                   endSR
028300111122     C*---------------------------------------------------------------*
028400111122      *  Controlli sull'Anagrafica
028500111122      *---------------------------------------------------------------*
028600111122     c     Ctrl_APD      begSR
028700111122      *
028800111122      * rileva dall'anagrafica
028900111122     c                   eval      apdTIP = 'A'
029000111122     c                   eval      apdPDR = stpPDR
029100111122     c     Key_APD       chain     fiapd01l
029200111122     c                   if        %Found(fiapd01l)
029300111122      *
029400111122     c                   if        apdATB <> *blank
029500111123     c                   movel(p)  'ANNULLATO'   errore
029600111122     c                   else
029700111122      *
029800111122      *-  Esluso da autofatturazione
029900111122     c                   if        apdPDD = 'S'
030000111123     c                   movel(p)  'AUTOFAT'     errore
030100111122     c                   end
030200111122      *
030300111122     c                   endIF
030400111122      *
030500111122     c                   else
030600111123     c                   movel(p)  'NO_APD'      errore
030700111122     c                   end
030800111122      *
030900111122     c                   endSR
031000111122     C*---------------------------------------------------------------*
031100111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
031200111122      *---------------------------------------------------------------*
031300111122     c     WRI_DETTA     begSR
031400111122     C*
031500090424     c                   clear                   WSFGT000
031600090424     C*
031700090424     c                   eval      WSFPDR  =  stpPDR
031800090424     c                   eval      WSFTSR  =  stpTSR
031900090424     c                   eval      WSFCTR  =  stpCTR
032000090424     c                   eval      WSFPRG  = stpPRG
032100090424     c                   eval      WSFDDT  = stpDDT
032200090424     c                   eval      WSFDST  = stpDST
032300090424     c                   eval      WSFDTS  = stpDTS
032400090424     c                   eval      WSFDCN  = stpDCN
032500111129     c                   eval      WSFFILE = 'FIFGT00F'
032600090424     c                   eval      WSFREC  = FGT_REC
032700111122     c                   eval      WSFPROGR= 'TNVRA50R'
032800111123     c                   eval      WSFtime = datetime
032900090428     c**
033000090428     c                   eval      WSFNOTE = Errore
033100090424     c**
033200090424     c                   write     WSFGT000
033300090424     C*
033400090424     c                   endSR
033500081211     C*---------------------------------------------------------------*
033600081110     OQSYSPRT   E            TESTA            02
033700111123     O                                           10 'TNVRA50R'
033800081110     O                                           25 'Autista'
033900081110     O                                           30 'TSR'
034000081110     O                                           38 'Tariffa'
034100081110     O                                           45 'Progr'
034200081110     O                                           55 'Data_Dec'
034300081110     O                                           65 'Data_Sca'
034400081110     O                                           75 'Data_Stp'
034500081110     O                                           85 'Data_Cnv'
034600111123     O                                          100 'N_Rec_FGT'
034700111123     O                                          111 'N O T E'
034800111123     O                       data               132 '  /  /    '
034900111123     O          E            TESTA       1
035000111123     O                       ora                130 '  :  :  '
035100111123      *
035200081110     O          E            DETta       1
035300081110     O                       stpPDR        z     25
035400081110     O                       stpTSR              29
035500081110     O                       stpCTR              36
035600081110     O                       stpPRG        z     43
035700081110     O                       stpDDT        z     55
035800081110     O                       stpDST        z     65
035900081110     O                       stpDTS        z     75
036000081110     O                       stpDCN        z     85
036100111123     o                       FGT_REC       z    100
036200111123     O                       errore             116
036300081110      ****
036400081110     o          E            FINEST      2
036500111123     o                                            1 ' '
036600111123     o                                          + 0 '*** TNVRA50R ***'
036700111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
036800111123     o                                          + 9 '***   FINE  STAMPA  ***'
036900111123     O                       data               + 9 '  /  /    '
037000111123     O                       ora                + 3 '  :  :  '
037100111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
