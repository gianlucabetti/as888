000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111201     H* TNVRA65R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111201     H*  facendole scadere il giorno prima.     AFFL/DEFL            *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111201     Ffiadt01L  uF a E           K DISK
001100111206     F                                     commit
001200111201     Ffiadd01L  iF a E           K DISK
001300111206     F                                     commit
001400111122      *
001500090401     Ffiapd01L  iF   E           K DISK
001600090401      *
001700111201     FwfADTXXF  o  a E             DISK
001800111201     FwxADT00S  o  a E             DISK
001900081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000081211      * -------------------------------------------------------------
002100111122     D KPJBA         E DS
002200111201     D dsADT         E DS                  ExtName(FIADT00F)
002300081110     d                                     prefix(STP:3)
002400111201     D ADT_REC         s              9s 0
002500090424     D STP_DATSCAD     s              8s 0
002600081230      *
002700111201     D oldPRG          s                   like(adtPrg)
002800111201     D newPRG          s                   like(adtPrg)
002900111201      *
003000111123     D esito           s              9s 0
003100111123     D esito_ALF       s              9a
003200090109      *
003300111123     D DATA_eur        S               D   DATFMT(*eur)
003400111123     D DATA_iso        S               D   DATFMT(*iso)
003500111123     C*------------------------------------------------------------------------*
003600111123     C*        - OPERAZIONI INIZIALI
003700111123     C*------------------------------------------------------------------------*
003800111123     C     *ENTRY        PLIST
003900111123     C                   PARM                    DATADEC           8
004000111123     C                   PARM                    AGGIORNA          1
004100111123     C                   PARM                    ESITO_ALF
004200111123     C*
004300111123     C     AGGIORNA      comp      ' '                                    44
004400111123     C                   move      datadec       DATA_dec          8 0
004500111123     C     *iso          movel     DATA_DEC      data_iso
004600111123     c     data_iso      subdur    1:*days       data_iso
004700111123     C     *iso          movel     data_iso      data_sca          8 0
004800111123     C                   z-add     data_sca      DATAscad          8 0
004900111123     C*
005000111201      * duplica la testata ADT
005100111201     C     adt01key      kLIST
005200111123     C                   kfld                    stpPDR
005300111123     C                   kfld                    stpPRG
005400111123     C*
005500111201     C     add01key      kLIST
005600111201     C                   kfld                    stpPDR
005700111201     C                   kfld                    oldPRG
005800111201     C*
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
007300111201      *
007400081209     C                   SETON                                        LR
007500090423     C*=================================================================
007600111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007700090423     C*=================================================================
007800111201      *   TESTATE AFFL.DEFL.
007900111213      *  Aggiunto il pezzo di leggere solo quelle tariffe che hanno il prezzo
008000111213      *   carburante poichè sono quelle rimaste al vecchio modo e quindi da copiare.
008100111201      *
008200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
008300111201     C+   SELECT rrn(fiadt00f), fiadt00f.*
008400111201     C+     FROM fiadt00f
008500111213     C+    WHERE adtdst > :DATAscad and
008600111213     C+          ADTPCAR> 0   and adtpdr
008700111123     C+                       IN
008800111201     C+ (Select traBPT from Aitra00f where tradfi=0 and traann=' ')
008900111201     C+             and adtpdr  NOT IN
009000111201     C+        (Select adtpdr from WFADTXXF)
009100111201     C+               order by ADTpdr
009200111122     C/END-EXEC
009300111122
009400111122     C*=====
009500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009600111122     C*=====
009700081110     c                   if        sqlCODE < 0
009800081110     c                   eval      esito = Sqlcode
009900111123     c                   move      esito         esito_ALF
010000081110     c                   dump(a)
010100081110     c                   return
010200081110     c                   end
010300081110     C/EXEC SQL
010400081110     C+ OPEN A1
010500081110     C/END-EXEC
010600111122     C*=====
010700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010800111122     C*=====
010900081110     c                   if        sqlCODE < 0
011000081110     c                   eval      esito = Sqlcode
011100111123     c                   move      esito         esito_ALF
011200081110     c                   dump(a)
011300081110     c                   return
011400081110     c                   else
011500111122      *
011600081110     c                   except    testa
011700111122      *
011800081110     c                   end
011900090109      * >>>>>>>
012000081209      * resto nel ciclo finchè....
012100081110     C                   DOU       SqlCod < 0
012200081110      * lettura cursore
012300081110     C/EXEC SQL
012400111201     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
012500081110     C/END-EXEC
012600111122     C*=====
012700111122      *  EoF
012800081110     C                   if        SqlCod = 100
012900081110     c                   Leave
013000111122     C*=====
013100081110      *  x errori
013200081110     C                   ElseIf    SqlCod < 0
013300081110     c                   eval      esito = Sqlcode
013400111123     c                   move      esito         esito_ALF
013500081110     c                   dump(a)
013600081110     c                   leave
013700081110     C                   ENDif
013800090428      *
013900111123      *----------------------------->>>>>
014000111123      *  se sono qui ho letto un record correttamente
014100090428      *----------------------------->>>>>
014200111122      * Controlli su Anagrafica
014300111123     c                   clear                   errore           15
014400111122     c                   exsr      Ctrl_APD
014500090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014600111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014700111123      *----------------------------->>>>>
014800111201     c                   if        Errore  = *blank  and Aggiorna = 'S'
014900090428      *----------------------------->>>>>
015000111122      *   Aggancia la testata
015100111201     C     ADt01key      chain     fiADT01L
015200111205      *
015300111201     c                   if        %Found(fiADT01L)
015400111201      *
015500111201      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
015600111201      *    se si fosse interrotto a metà
015700111201     c                   write     fiADTxxx
015800111205      *
015900111205      *  Occorre verificare che la tariffa sia stata stampata in sede.
016000111205      *    allora può procedere con il far scadere e riaprire con un altro progressivo.
016100111205      *                    ========
016200111205     c                   IF        ADtDCN > 0
016300111205      *                    ========
016400111201      *
016500111122      *   Mette in SCADENZA
016600111201     c                   z-add     DATA_sca      adtDST
016700111201     c                   eval        oldPRG = adtPRG
016800111205      *
016900111201     c                   update    fiADT000
017000111201      * ---------
017100111122      *   Mette in Decorrenza il nuovo Progressivo
017200111201     c                   eval        adtPRG = adtPRG + 1
017300111201     c                   eval        newPRG = adtPRG
017400111201     c                   movel     DATA_dec      adtDDT
017500111201     c                   z-add     20391231      adtDST
017600111122      *
017700111201     c                   z-add     *date         adtDUV
017800111201     c***********        z-add     *date         ADTDTS
017900111122      *
018000111201     c                   z-add     0             ADTPCAR
018100111201     c                   z-add     0             ADTDRC
018200111201     c                   z-add     0             ADTDCN
018300111122      *
018400111122      *  quindi scrive il progressivo seguente
018500111201     c                   write     fiadt000
018600111122      *  copia Tariffa
018700111122     c                   exsr      COPIA_TAR
018800090112      *
018900111205      *                    ========
019000111205     c                   ELSE
019100111205      *                    ========
019200111205      * Altrimenti se NON è stata stampata in sede
019300111205      *   NON genero un nuovo progressivo ma elimino il costo del carburante
019400111205      **  .....è stata convalidata in FILIALE ????!!!!!
019500111205      ****  solo se la scadenza della Tariffa va oltre il limite di conversione.
019600111205      *                    ++++++++
019700111205     c**************     IF        adtDTS > 0
019800111205      **************       ++++++++
019900111205      *   Se la scadenza della tariffa è superiore al limite di scadenza della conversione
020000111205      **************       ++++++++
020100111205     c                   IF        adtDST > datascad
020200111205     c                   z-add     0             ADTPCAR
020300111205     c                   movel(p)  'NON STAMPATA'errore
020400111205     c                   ENDIF
020500111205      ****
020600111205     c                   update    FiADT000
020700111205      **************       ++++++++
020800111205     c**************     ENDIF
020900111205      *                    ========
021000111205     c                   ENDIF
021100111205      *                    ========
021200111205      *
021300111205      *
021400111201     c                   end
021500090428      *----------------------------->>>>>
021600111201     c                   endIF
021700090428      *----------------------------->>>>>
021800111206     c                   if        errore  = *blank
021900111206     c                   eval        Errore = 'DUPLICA TAR'
022000111206     c                   end
022100111206      *----
022200111206     c   Of              except    testa
022300111206     c                   except    detta
022400111206      *
022500090424      *  scrive il dettaglio su un file
022600090424     c                   exsr      wri_DETTA
022700081110      *
022800081110     C                   ENDDO
022900081110     C/EXEC SQL
023000081110     C+ Close A1
023100081110     C/END-EXEC
023200081110     C                   if        Esito < 0
023300111123     c                   move      esito         esito_ALF
023400081110     c                   return
023500081110     c                   endIF
023600081110     C*
023700081110     C     FINE          TAG
023800081110     c                   except    finest
023900081110     c                   return
024000090424     C*---------------------------------------------------------------*
024100111122      *  Copia la tariffa
024200090424      *---------------------------------------------------------------*
024300111122     c     COPIA_tar     begSR
024400111129      *
024500111201     C     add01key      setll     fiADD01L
024600111201     C     add01key      reade     fiADD01L
024700111122      *
024800111201     c                   dow       not %EoF(fiADD01L)
024900111201      *  copia con nuovo prg.
025000111201     c                   eval        addPRG = newPRG
025100111201     c                   write     fiadd000
025200111201      *
025300111201     C     add01key      reade     fiADD01L
025400111201     c                   end
025500111129      *
025600111122     c                   endSR
025700111122     C*---------------------------------------------------------------*
025800111122      *  Controlli sull'Anagrafica
025900111122      *---------------------------------------------------------------*
026000111122     c     Ctrl_APD      begSR
026100111122      *
026200111122      * rileva dall'anagrafica
026300111201     c                   eval      apdTIP = 'D'
026400111122     c                   eval      apdPDR = stpPDR
026500111122     c     Key_APD       chain     fiapd01l
026600111122     c                   if        %Found(fiapd01l)
026700111122      *
026800111122     c                   if        apdATB <> *blank
026900111123     c                   movel(p)  'ANNULLATO'   errore
027000111122     c                   else
027100111122      *
027200111122      *-  Esluso da autofatturazione
027300111122     c                   if        apdPDD = 'S'
027400111123     c                   movel(p)  'AUTOFAT'     errore
027500111122     c                   end
027600111122      *
027700111122     c                   endIF
027800111122      *
027900111122     c                   else
028000111123     c                   movel(p)  'NO_APD'      errore
028100111122     c                   end
028200111122      *
028300111122     c                   endSR
028400111122     C*---------------------------------------------------------------*
028500111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
028600111122      *---------------------------------------------------------------*
028700111122     c     WRI_DETTA     begSR
028800111122     C*
028900111201     c                   clear                   WSADT000
029000090424     C*
029100090424     c                   eval      WSFPDR  =  stpPDR
029200090424     c                   eval      WSFTSR  =  stpTSR
029300111201     c                   eval      WSFPRG  =  stpPRG
029400111201     c                   eval      WSFDDT  =  stpDDT
029500111201     c                   eval      WSFDST  =  stpDST
029600111201     c                   eval      WSFDCN  =  stpDCN
029700111201     c                   eval      WSFDTS  =  stpDTS
029800111201     c                   eval      WSFDUV  =  stpDUV
029900111201     c                   eval      WSFDRC  =  stpDRC
030000111201     c                   eval      WSFNRC  =  stpNRC
030100111201     c                   eval      WSFCDF  =  stpCDF
030200111201     c                   eval      WSFCSF  =  stpCSF
030300111201     c                   eval      WSFFIL  =  stpFIL
030400111201      *
030500111201     c                   eval      WSFFILE = 'FIADT00F'
030600111201     c                   eval      WSFREC  = ADT_REC
030700111201     c                   eval      WSFPROGR= 'TNVRA65R'
030800111123     c                   eval      WSFtime = datetime
030900090428     c**
031000111206     c**
031100090428     c                   eval      WSFNOTE = Errore
031200090424     c**
031300111201     c                   write     WSADT000
031400090424     C*
031500090424     c                   endSR
031600081211     C*---------------------------------------------------------------*
031700081110     OQSYSPRT   E            TESTA            02
031800111201     O                                           10 'TNVRA65R'
031900081110     O                                           25 'Autista'
032000081110     O                                           30 'TSR'
032100081110     O                                           45 'Progr'
032200081110     O                                           55 'Data_Dec'
032300081110     O                                           65 'Data_Sca'
032400081110     O                                           75 'Data_Stp'
032500081110     O                                           85 'Data_Cnv'
032600111201     O                                          100 'N_Rec_ADT'
032700111123     O                                          111 'N O T E'
032800111123     O                       data               132 '  /  /    '
032900111123     O          E            TESTA       1
033000111123     O                       ora                130 '  :  :  '
033100111123      *
033200081110     O          E            DETta       1
033300081110     O                       stpPDR        z     25
033400081110     O                       stpTSR              29
033500081110     O                       stpPRG        z     43
033600081110     O                       stpDDT        z     55
033700081110     O                       stpDST        z     65
033800081110     O                       stpDTS        z     75
033900081110     O                       stpDCN        z     85
034000111201     o                       ADT_REC       z    100
034100111123     O                       errore             116
034200081110      ****
034300081110     o          E            FINEST      2
034400111123     o                                            1 ' '
034500111201     o                                          + 0 '*** TNVRA65R ***'
034600111201     o                                          + 9 '***  DUPLI - AFF/DEF ***'
034700111123     o                                          + 9 '***   FINE  STAMPA  ***'
034800111123     O                       data               + 9 '  /  /    '
034900111123     O                       ora                + 3 '  :  :  '
035000111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
