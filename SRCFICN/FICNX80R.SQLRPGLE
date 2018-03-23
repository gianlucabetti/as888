000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120319     H* FICNX80R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111122     H*  facendole scadere il giorno prima.                          *
000600081209     H*                                                              *
000700120319     H* riceve come parametro la Data decorrenza                     *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111122     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001100111206     F                                     commit
001200111122     Ffitgt01L  uF a E           K DISK
001300111206     F                                     commit
001400111122      *
001500120321     Faitra03L  iF   E           K DISK
001600090401     Ffiapd01L  iF   E           K DISK
001700090401      *
001800111129     FwfFGTXXF  o  a E             DISK
001900090424     FwxFGT00S  o  a E             DISK
002000081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002100081211      * -------------------------------------------------------------
002200111122     D KPJBA         E DS
002300081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002400081110     d                                     prefix(STP:3)
002500081110     D FGT_REC         s              9s 0
002600090424     D STP_DATSCAD     s              8s 0
002700081230      *
002800111122     D oldPRG          s                   like(tgtPrg)
002900111122     D newPRG          s                   like(tgtPrg)
003000111123     D esito           s              9s 0
003100111123     D esito_ALF       s              9a
003200090109      *
003300111123     D DATA_eur        S               D   DATFMT(*eur)
003400111123     D DATA_iso        S               D   DATFMT(*iso)
003500120321      *
003600120321     D Accreditato     S              1A
003700111123      *
003800111205     D DA_Copiare      S              1A
003900111122     D ficn06ds      E DS
004000111123     C*------------------------------------------------------------------------*
004100111123     C*        - OPERAZIONI INIZIALI
004200111123     C*------------------------------------------------------------------------*
004300111123     C     *ENTRY        PLIST
004400111123     C                   PARM                    DATADEC           8
004500111123     C                   PARM                    AGGIORNA          1
004600111123     C                   PARM                    ESITO_ALF
004700111123     C*
004800111123     C     AGGIORNA      comp      ' '                                    44
004900111123     C                   move      datadec       DATA_dec          8 0
005000111123     C     *iso          movel     DATA_DEC      data_iso
005100111123     c     data_iso      subdur    1:*days       data_iso
005200111123     C     *iso          movel     data_iso      data_sca          8 0
005300111123     C                   z-add     data_sca      DATAscad          8 0
005400111123     C*
005500111123      * duplica la testata TGT
005600111123     C     tgt01key      kLIST
005700111123     C                   kfld                    stpPDR
005800111123     C                   kfld                    stpSML
005900111123     C                   kfld                    stpPRG
006000111123     C*
006100111123     c     Key_APD       KLIST
006200111123     c                   kfld                    apdTIP
006300111123     c                   kfld                    apdPDR
006400120321     C*
006500120321     c     Key_TRA       KLIST
006600120321     c                   kfld                    traKAU
006700120321     c                   kfld                    traDFI
006800111123      *
006900111123     c                   clear                   esito
007000111123     c                   clear                   esito_ALF
007100111123     c                   time                    datetime         14 0
007200111123     c                   move      datetime      data              8 0
007300111123     c                   movel     datetime      ora               6 0
007400111123      *
007500111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007600111123     C                   clear                   UDATE_8           8 0
007700111123     C                   MOVEL     *DATE         UDATE_8
007800111122     c                   clear                   savSTPpdr         7 0
007900081209     C                   SETON                                        LR
008000090423      **
008100090423     C*=================================================================
008200111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
008300090423     C*=================================================================
008400120319      *  Tutte le TARIFFE degli AUT di città con scadenza OLTRE la data scadenza
008500120319      *   che sono legate ad un CONTRATTO stampato a MODO vecchio ossia con la
008600120319      *   PRIMA DATA CERTA vuota --> (nel campo FILLER del TRS dalla 13 x 8 bytes)
008700120319      *   inoltre non sono presenti sul file di controllo del processo di duplica.
008800120319      *
008900090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009000120319     C+ Select
009100120319     C+        rrn(fifgt00f), fifgt00f.* FROM fifgt00f
009200120319     C+                         WHERE
009300120319     C+     fgttsr in ('C', 'R', 'G') and fgtsml=' ' and
009400120319     C+                  fgtdst > :DATAscad
009500120319     C+               and  fgtPDR * 1000 + fgtPRG
009600120319     C+                          IN
009700120319     C+    (Select tgtpdr * 1000 + tgtprg from fiTGT00F
009800120319     C+   where TGTsml=' '  and TGTnrc > 0 and tgtATB = ' '
009900120319     C+                     AND tgtNRC
010000120319     C+                          IN
010100120319     C+       (Select trsNRC from Aitrs00f where
010200120319     C+  SUBSTR(trsFLR, 13, 8) = ' ' and trsANN  = ' ' ) )
010300120319     C+                     and FgtPDR
010400120319     C+                        NOT IN
010500120319     C+           (Select fgtpdr from WFFGTxxF)
010600120319     C+  ORDER BY Fgtpdr
010700111122     C/END-EXEC
010800111122
010900111122     C*=====
011000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
011100111122     C*=====
011200081110     c                   if        sqlCODE < 0
011300081110     c                   eval      esito = Sqlcode
011400111123     c                   move      esito         esito_ALF
011500081110     c                   dump(a)
011600081110     c                   return
011700081110     c                   end
011800081110     C/EXEC SQL
011900081110     C+ OPEN A1
012000081110     C/END-EXEC
012100111122     C*=====
012200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
012300111122     C*=====
012400081110     c                   if        sqlCODE < 0
012500081110     c                   eval      esito = Sqlcode
012600111123     c                   move      esito         esito_ALF
012700081110     c                   dump(a)
012800081110     c                   return
012900081110     c                   else
013000111122      *
013100081110     c                   except    testa
013200111122      *
013300081110     c                   end
013400090109      * >>>>>>>
013500081209      * resto nel ciclo finchè....
013600081110     C                   DOU       SqlCod < 0
013700081110      * lettura cursore
013800081110     C/EXEC SQL
013900111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
014000081110     C/END-EXEC
014100111122     C*=====
014200111122      *  EoF
014300081110     C                   if        SqlCod = 100
014400081110     c                   Leave
014500111122     C*=====
014600081110      *  x errori
014700081110     C                   ElseIf    SqlCod < 0
014800081110     c                   eval      esito = Sqlcode
014900111123     c                   move      esito         esito_ALF
015000081110     c                   dump(a)
015100081110     c                   leave
015200081110     C                   ENDif
015300090428      *
015400111123      *----------------------------->>>>>
015500111123      *  se sono qui ho letto un record correttamente
015600090428      *----------------------------->>>>>
015700111122      * Controlli su Anagrafica
015800111123     c                   clear                   errore           15
015900111122     c                   exsr      Ctrl_APD
016000090428      *
016100120319      * x qualsiasi tipo di errore controllato prima non deve fare nulla e
016200120319      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
016300111123      *----------------------------->>>>>
016400120319     c                   if        Errore = *blank  and Aggiorna = 'S'
016500120319      *-------->>>>>
016600120319     c                   exsr      COPIA_TESTATA
016700120319      ***
016800120319     c                   exsr      COPIA_DETTAGLI
016900120319      *-------->>>>>
017000090428     c                   end
017100090428      *----------------------------->>>>>
017200120319      *
017300111206     c                   if        errore  = *blank
017400120321      *
017500120321     c                   eval        Errore = 'Da DUPLICARE'
017600120321     c                   if        Accreditato = 'N'
017700120321     c                   eval        Errore = 'Far SCADERE'
017800111206     c                   end
017900120321      *
018000120321      * solo in aggiornamento
018100120321     c                   if        Aggiorna = 'S'
018200120321     c                   eval        Errore = 'DUPlicata   '
018300120321     c                   if        Accreditato = 'N'
018400120321     c                   eval        Errore = 'Fatta SCADERE'
018500120321     c                   end
018600120321     c                   end
018700120321      *
018800120321     c                   end
018900111206      *
019000111206     c   Of              except    testa
019100111206     c                   except    detta
019200120319      *
019300111206      *---------------------
019400090424      *  scrive il dettaglio su un file
019500090424     c                   exsr      wri_DETTA
019600081110      *
019700081110     C                   ENDDO
019800081110     C/EXEC SQL
019900081110     C+ Close A1
020000081110     C/END-EXEC
020100081110     C                   if        Esito < 0
020200111123     c                   move      esito         esito_ALF
020300081110     c                   return
020400081110     c                   endIF
020500081110     C*
020600081110     C     FINE          TAG
020700081110     c                   except    finest
020800120319      **
020900081110     c                   return
021000090424     C*---------------------------------------------------------------*
021100120319      *  Copia la tariffa  TGT
021200090424      *---------------------------------------------------------------*
021300120319     c     COPIA_TESTATA begSR
021400111129      *
021500120319      *  Deve scrivere la testata a rottura di autista
021600120319      *-+++++++++++++++--
021700120319     c                   if        stpPDR <> savSTPpdr
021800120319      *-+++++++++++++++--
021900120319      *  salva chiave di rottura
022000120319     c                   eval              savSTPpdr = stpPDR
022100120321      *
022200120319     c                   eval      DA_Copiare = 'S'
022300120321     c                   eval      Accreditato = 'N'
022400120319      *
022500120319      *   Aggancia la testata
022600120319     C     tgt01key      chain     fiTGT01L
022700120319     c                   if        NOT %Found(fiTGT01L)
022800120319     c                   eval      DA_Copiare = 'N'
022900120319     c                   eLSe
023000120320      *  Occorre verificare che la tariffa sia stata stampata in sede.
023100120320      * ---------------------
023200120319      * TESTATA OK:
023300120319      * ---------------------
023400120320      ***   Per NON STAMPATA in SEDE NON deve fare nulla ossia NON duplicare
023500120320      *                    ========
023600120320     c                   IF        tgtDCN = 0
023700120320      *                    ========
023800120320      *
023900120320     c                   eval      DA_Copiare = 'N'
024000120320     c                   update    FiTGT000
024100120320      ***
024200120319      *    allora procede con il far scadere e
024300120319      *    riaprire con un altro progressivo.
024400120319      *                    ========
024500120320     c                   elseIF    tgtDCN > 0
024600120319      *                    ========
024700120319      ***
024800120319      *   Mette in SCADENZA la tariffa attuale e aggiorna
024900120319     c                   z-add     DATA_sca      tgtDST
025000120319     c                   eval        oldPRG = tgtPRG
025100120319     c                   update    fiTGT000
025200120319      *
025300120321      ** ++++++++++
025400120321      *   Controlla se Accreditato o DISaccreditato
025500120321      ** ++++++++++
025600120321     c                   eval      traKAU = tgtPDR
025700120321     c                   eval      traDFI = 0
025800120321     c     Key_TRA       chain     aitra03L
025900120321      *
026000120321      ** Solo se Accreditato deve DUPLICARE LA TARIFFA
026100120321      *** altrimenti è Disaccreditato
026200120321      **   e NON deve Duplicare la tariffa
026300120321     c                   if        %found(aitra03L)
026400120321     c                   eval      Accreditato = 'S'
026500120321     c                   end
026600120321      ** ++++++++++
026700120321      *  Solo se ACCREDITATO = 'S' allora procedo con la copia
026800120321      *   altrimenti ho solo FATTO SCADERE la tariffa di un AUT disaccreditato
026900120321      ** ++++++++++
027000120321      **
027100120321      *  Pilotato con DA COPIARE
027200120321     c                   IF        DA_Copiare = 'S'
027300120321     c                               and
027400120321     c                             Accreditato = 'S'
027500120321      ****
027600120319      *   Apre in Decorrenza e scrive il nuovo Progressivo
027700120319     c                   eval        tgtPRG = tgtPRG + 1
027800120319     c                   eval        newPRG = tgtPRG
027900120319     c                   movel     DATA_dec      tgtDDT
028000120319     c                   z-add     20391231      tgtDST
028100120319     c                   z-add     *date         tgtDUV
028200120319     c                   eval        tgtPRU = 'New04_2012'
028300120319      *
028400120321     c                   z-add     0             TGTfil
028500120321     c                   z-add     0             TGTNRC
028600120320     c                   z-add     0             TGTPCAR
028700120319     c                   z-add     0             TGTCCKM
028800120319     c                   z-add     *date         TGTDTS
028900120319     c                   z-add     0             TGTDCN
029000120319     c                   z-add     0             TGTDRC
029100120319      *  quindi scrive il progressivo seguente
029200120319     c                   write     fitgt000
029300120321      ****
029400120321     c                   EndiF
029500120319      ****
029600120319      *                    ========
029700120319     c                   ENDIF
029800120319      *                    ========
029900120319      *
030000120319     c                   end
030100120319      *-+++++++++++++++--
030200120319     c                   endIF
030300120319      *-+++++++++++++++--
030400120321      *    esce con :
030500120321      *      DA_Copiare  S/N
030600120321      *       e
030700120321      *      ACCREDITATO S/N
030800120321      *
030900120319     c                   endSR
031000120319     C*---------------------------------------------------------------*
031100120319      *  Copia la tariffa
031200120319      *---------------------------------------------------------------*
031300120319     c     COPIA_DETTAGLIbegSR
031400120319      *
031500120319      ***
031600120319      ***  Solo se la TESTATA ha dato esito positivo
031700120319      ***
031800120319     c                   if        DA_Copiare = 'S'
031900120319      *
032000120319      *   deve far scadere il record dell'FGT
032100120319     c     fgt_REC       chain     FIFGT00F
032200120319     c                   if        %Found(FIFGT00F)
032300120319      * aggiorna
032400120319     c                   eval      fgtDST = DATA_sca
032500120319     c                   update    fgtFisico
032600120319      *  copia Tariffa
032700120321     c                   if        ACCREDITATO = 'S'
032800120319     c                   exsr      COPIA_TAR
032900120321     c                   end
033000120321      *
033100120319     c                   end
033200120319      *
033300120319     c                   end
033400120319      *
033500120319     c                   endSR
033600120319     C*---------------------------------------------------------------*
033700120319      *  Copia la tariffa
033800120319      *---------------------------------------------------------------*
033900120319     c     COPIA_tar     begSR
034000120319      *
034100111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
034200111129      *    se si fosse interrotto a metà
034300111129     c                   write     wfFGTxxx
034400111122      *
034500111122     c                   clear                   ficn06ds
034600111123     c                   movel     fgtPDR        FILOLD
034700111123     c                   move      fgtPDR        PDROLD
034800111123     c                   move      fgtTSR        TSROLD
034900111123     c                   z-add     fgtCTR        CTROLD
035000111122     c                   z-add     oldPRG        PRGOLD
035100111123     c                   movel     fgtPDR        FILNEW
035200111123     c                   move      fgtPDR        PDRNEW
035300111122     c                   z-add     newPRG        PRGNEW
035400111122      *
035500111123     c                   eval      kpjbu = ficn06ds
035600111122     c                   call      'FICN06R'
035700111122     c                   parm                    KPJBA
035800111129      *
035900111122     c                   endSR
036000111122     C*---------------------------------------------------------------*
036100111122      *  Controlli sull'Anagrafica
036200111122      *---------------------------------------------------------------*
036300111122     c     Ctrl_APD      begSR
036400111122      *
036500111122      * rileva dall'anagrafica
036600111122     c                   eval      apdTIP = 'A'
036700111122     c                   eval      apdPDR = stpPDR
036800111122     c     Key_APD       chain     fiapd01l
036900111122     c                   if        %Found(fiapd01l)
037000111122      *
037100111122     c                   if        apdATB <> *blank
037200111123     c                   movel(p)  'ANNULLATO'   errore
037300111122     c                   else
037400111122      *
037500111122      *-  Esluso da autofatturazione
037600111122     c                   if        apdPDD = 'S'
037700111123     c                   movel(p)  'AUTOFAT'     errore
037800111122     c                   end
037900111122      *
038000111122     c                   endIF
038100111122      *
038200111122     c                   else
038300111123     c                   movel(p)  'NO_APD'      errore
038400111122     c                   end
038500111122      *
038600111122     c                   endSR
038700111122     C*---------------------------------------------------------------*
038800111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
038900111122      *---------------------------------------------------------------*
039000111122     c     WRI_DETTA     begSR
039100111122     C*
039200090424     c                   clear                   WSFGT000
039300090424     C*
039400090424     c                   eval      WSFPDR  =  stpPDR
039500090424     c                   eval      WSFTSR  =  stpTSR
039600090424     c                   eval      WSFCTR  =  stpCTR
039700090424     c                   eval      WSFPRG  = stpPRG
039800090424     c                   eval      WSFDDT  = stpDDT
039900090424     c                   eval      WSFDST  = stpDST
040000090424     c                   eval      WSFDTS  = stpDTS
040100090424     c                   eval      WSFDCN  = stpDCN
040200111129     c                   eval      WSFFILE = 'FIFGT00F'
040300090424     c                   eval      WSFREC  = FGT_REC
040400120319     c                   eval      WSFPROGR= 'FICNX80R'
040500111123     c                   eval      WSFtime = datetime
040600090428     c**
040700090428     c                   eval      WSFNOTE = Errore
040800090424     c**
040900090424     c                   write     WSFGT000
041000090424     C*
041100090424     c                   endSR
041200081211     C*---------------------------------------------------------------*
041300081110     OQSYSPRT   E            TESTA            02
041400120319     O                                           10 'FICNX80R'
041500081110     O                                           25 'Autista'
041600081110     O                                           30 'TSR'
041700081110     O                                           38 'Tariffa'
041800081110     O                                           45 'Progr'
041900081110     O                                           55 'Data_Dec'
042000081110     O                                           65 'Data_Sca'
042100081110     O                                           75 'Data_Stp'
042200081110     O                                           85 'Data_Cnv'
042300111123     O                                          100 'N_Rec_FGT'
042400111123     O                                          111 'N O T E'
042500111123     O                       data               132 '  /  /    '
042600111123     O          E            TESTA       1
042700111123     O                       ora                130 '  :  :  '
042800111123      *
042900081110     O          E            DETta       1
043000081110     O                       stpPDR        z     25
043100081110     O                       stpTSR              29
043200081110     O                       stpCTR              36
043300081110     O                       stpPRG        z     43
043400081110     O                       stpDDT        z     55
043500081110     O                       stpDST        z     65
043600081110     O                       stpDTS        z     75
043700081110     O                       stpDCN        z     85
043800111123     o                       FGT_REC       z    100
043900111123     O                       errore             116
044000081110      ****
044100081110     o          E            FINEST      2
044200111123     o                                            1 ' '
044300120319     o                                          + 0 '*** FICNX80R ***'
044400111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
044500111123     o                                          + 9 '***   FINE  STAMPA  ***'
044600111123     O                       data               + 9 '  /  /    '
044700111123     O                       ora                + 3 '  :  :  '
044800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
