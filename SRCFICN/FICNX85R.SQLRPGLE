000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120320     H* FICNX85R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111201     H*  facendole scadere il giorno prima.     AFFL/DEFL            *
000600120320     H*  Quelle che HANNO un CONTRATTO che NON sia 9999999           *
000700120320     H*                                                              *
000800120320     H*                                                              *
000900111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
001000111122     H*  e se deve eseguire l'aggiornamento                          *
001100000000     H*--------------------------------------------------------------*
001200111201     Ffiadt01L  uF a E           K DISK
001300111206     F                                     commit
001400111201     Ffiadd01L  iF a E           K DISK
001500111206     F                                     commit
001600111122      *
001700120321     Faitra05L  iF   E           K DISK
001800090401     Ffiapd01L  iF   E           K DISK
001900090401      *
002000111201     FwfADTXXF  o  a E             DISK
002100111201     FwxADT00S  o  a E             DISK
002200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002300081211      * -------------------------------------------------------------
002400111122     D KPJBA         E DS
002500111201     D dsADT         E DS                  ExtName(FIADT00F)
002600081110     d                                     prefix(STP:3)
002700111201     D ADT_REC         s              9s 0
002800090424     D STP_DATSCAD     s              8s 0
002900081230      *
003000111201     D oldPRG          s                   like(adtPrg)
003100111201     D newPRG          s                   like(adtPrg)
003200111201      *
003300111123     D esito           s              9s 0
003400111123     D esito_ALF       s              9a
003500090109      *
003600111123     D DATA_eur        S               D   DATFMT(*eur)
003700111123     D DATA_iso        S               D   DATFMT(*iso)
003800120321      *
003900120321     D Accreditato     S              1A
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
005500111201      * duplica la testata ADT
005600111201     C     adt01key      kLIST
005700111123     C                   kfld                    stpPDR
005800111123     C                   kfld                    stpPRG
005900111123     C*
006000111201     C     add01key      kLIST
006100111201     C                   kfld                    stpPDR
006200111201     C                   kfld                    oldPRG
006300111201     C*
006400111123     c     Key_APD       KLIST
006500111123     c                   kfld                    apdTIP
006600111123     c                   kfld                    apdPDR
006700120321     C*
006800120321     c     Key_TRA       KLIST
006900120321     c                   kfld                    traBPT
007000120321     c                   kfld                    traDFI
007100111123      *
007200111123     c                   clear                   esito
007300111123     c                   clear                   esito_ALF
007400111123     c                   time                    datetime         14 0
007500111123     c                   move      datetime      data              8 0
007600111123     c                   movel     datetime      ora               6 0
007700111123      *
007800111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007900111123     C                   clear                   UDATE_8           8 0
008000111123     C                   MOVEL     *DATE         UDATE_8
008100111122     c                   clear                   savSTPpdr         7 0
008200111201      *
008300081209     C                   SETON                                        LR
008400090423     C*=================================================================
008500111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
008600090423     C*=================================================================
008700111201      *   TESTATE AFFL.DEFL.
008800111213      *  Aggiunto il pezzo di leggere solo quelle tariffe che hanno il prezzo
008900111213      *   carburante poichè sono quelle rimaste al vecchio modo e quindi da copiare.
009000111201      *
009100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009200120320     C+        SELECT rrn(fiadt00f), fiadt00f.*
009300120320     C+               FROM fiadt00f WHERE
009400120320     C+         adtNRC <> 9999999 and adtNRC > 0
009500120320     C+           and adtdst > :DATAscad and
009600120320     C+                        adtNRC
009700120320     C+                          IN
009800120320     C+       (Select trsNRC from Aitrs00f where
009900120320     C+  SUBSTR(trsFLR, 13, 8) = ' ' and trsANN  = ' ' )
010000120320     C+                      and adtpdr
010100120320     C+                         NOT IN
010200111201     C+        (Select adtpdr from WFADTXXF)
010300111201     C+               order by ADTpdr
010400111122     C/END-EXEC
010500111122
010600111122     C*=====
010700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010800111122     C*=====
010900081110     c                   if        sqlCODE < 0
011000081110     c                   eval      esito = Sqlcode
011100111123     c                   move      esito         esito_ALF
011200081110     c                   dump(a)
011300081110     c                   return
011400081110     c                   end
011500081110     C/EXEC SQL
011600081110     C+ OPEN A1
011700081110     C/END-EXEC
011800111122     C*=====
011900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
012000111122     C*=====
012100081110     c                   if        sqlCODE < 0
012200081110     c                   eval      esito = Sqlcode
012300111123     c                   move      esito         esito_ALF
012400081110     c                   dump(a)
012500081110     c                   return
012600081110     c                   else
012700111122      *
012800081110     c                   except    testa
012900111122      *
013000081110     c                   end
013100090109      * >>>>>>>
013200081209      * resto nel ciclo finchè....
013300081110     C                   DOU       SqlCod < 0
013400081110      * lettura cursore
013500081110     C/EXEC SQL
013600111201     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
013700081110     C/END-EXEC
013800111122     C*=====
013900111122      *  EoF
014000081110     C                   if        SqlCod = 100
014100081110     c                   Leave
014200111122     C*=====
014300081110      *  x errori
014400081110     C                   ElseIf    SqlCod < 0
014500081110     c                   eval      esito = Sqlcode
014600111123     c                   move      esito         esito_ALF
014700081110     c                   dump(a)
014800081110     c                   leave
014900081110     C                   ENDif
015000090428      *
015100111123      *----------------------------->>>>>
015200111123      *  se sono qui ho letto un record correttamente
015300090428      *----------------------------->>>>>
015400111122      * Controlli su Anagrafica
015500111123     c                   clear                   errore           15
015600111122     c                   exsr      Ctrl_APD
015700120320      *
015800090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
015900111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
016000111123      *----------------------------->>>>>
016100111201     c                   if        Errore  = *blank  and Aggiorna = 'S'
016200090428      *----------------------------->>>>>
016300120320      **
016400120320     c                   exsr      COPIA_TESTATA
016500120320      ***
016600090428      *----------------------------->>>>>
016700111201     c                   endIF
016800090428      *----------------------------->>>>>
016900111206     c                   if        errore  = *blank
017000120321      *
017100120321     c                   eval        Errore = 'Da DUPLICARE'
017200120321     c                   if        Accreditato = 'N'
017300120321     c                   eval        Errore = 'Far SCADERE'
017400120321     c                   end
017500120321      *
017600120321      * solo in aggiornamento
017700120321     c                   if        Aggiorna = 'S'
017800120321     c                   eval        Errore = 'DUPlicata   '
017900120321     c                   if        Accreditato = 'N'
018000120321     c                   eval        Errore = 'Fatta SCADERE'
018100120321     c                   end
018200120321     c                   end
018300120321      *
018400111206     c                   end
018500111206      *----
018600111206     c   Of              except    testa
018700111206     c                   except    detta
018800111206      *
018900090424      *  scrive il dettaglio su un file
019000090424     c                   exsr      wri_DETTA
019100081110      *
019200081110     C                   ENDDO
019300081110     C/EXEC SQL
019400081110     C+ Close A1
019500081110     C/END-EXEC
019600081110     C                   if        Esito < 0
019700111123     c                   move      esito         esito_ALF
019800081110     c                   return
019900081110     c                   endIF
020000081110     C*
020100081110     C     FINE          TAG
020200081110     c                   except    finest
020300081110     c                   return
020400120320     C*---------------------------------------------------------------*
020500120320      *  Copia la tariffa  TGT
020600120320      *---------------------------------------------------------------*
020700120320     c     COPIA_TESTATA begSR
020800120320      *
020900120321     c                   eval      Accreditato = 'N'
021000120321      *
021100120320      *   Aggancia la testata
021200120320     C     ADt01key      chain     fiADT01L
021300120320     c                   if        %Found(fiADT01L)
021400120320      *
021500120320      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
021600120320      *    se si fosse interrotto a metà
021700120320     c                   write     fiADTxxx
021800120320      *  Occorre verificare che la tariffa sia stata stampata in sede.
021900120320      *
022000120320      *  Se NON Stampata
022100120320      *                    ========
022200120320     c                   IF        ADtDCN = 0
022300120320      *                    ========
022400120320      *   NON genero un nuovo progressivo
022500120320      ****  solo se la scadenza della Tariffa va oltre il limite di conversione.
022600120320      **************
022700120320     c                   movel(p)  'NON STAMPATA'errore
022800120320     c                   update    FiADT000
022900120320      *
023000120320      *  Se Stampata
023100120320      *   può procedere a far scadere e riaprire con un altro progressivo.
023200120320      *                    ========
023300120320     c                   elseIF    ADtDCN > 0
023400120320      *                    ========
023500120320      *   Mette in SCADENZA
023600120320     c                   z-add     DATA_sca      adtDST
023700120320     c                   eval        oldPRG = adtPRG
023800120320     c                   update    fiADT000
023900120321      **
024000120321      ** ++++++++++
024100120321      *   Controlla se Accreditato o DISaccreditato
024200120321      ** ++++++++++
024300120321     c                   eval      traBPT = adtPDR
024400120321     c                   eval      traDFI = 0
024500120321     c     Key_TRA       chain     aitra05L
024600120321      *
024700120321      ** Solo se Accreditato deve DUPLICARE LA TARIFFA
024800120321      *** altrimenti è Disaccreditato
024900120321      **   e NON deve Duplicare la tariffa
025000120321     c                   if        %found(aitra05L)
025100120321     c                   eval      Accreditato = 'S'
025200120321     c                   end
025300120321      ** ++++++++++
025400120321      *  Solo se ACCREDITATO = 'S' allora procedo con la copia
025500120321      *   altrimenti ho solo FATTO SCADERE la tariffa di un AUT disaccreditato
025600120321      ** ++++++++++
025700120321      **
025800120321     c                   IF        Accreditato = 'S'
025900120320      * ---------
026000120320      *   Mette in Decorrenza il nuovo Progressivo
026100120320     c                   eval        adtPRG = adtPRG + 1
026200120320     c                   eval        newPRG = adtPRG
026300120320     c                   movel     DATA_dec      adtDDT
026400120320     c                   z-add     20391231      adtDST
026500120320      *
026600120320     c                   z-add     *date         adtDUV
026700120320     c                   z-add     *date         ADTDTS
026800120320     c                   z-add     0             ADTNRC
026900120321     c                   z-add     0             ADTfil
027000120320      *
027100120320     c                   z-add     0             ADTPCAR
027200120320     c                   z-add     0             ADTDRC
027300120320     c                   z-add     0             ADTDCN
027400120320      *  quindi scrive il progressivo seguente
027500120320     c                   write     fiadt000
027600120320      *  copia Tariffa
027700120320     c                   exsr      COPIA_DETTAGLI
027800120321      **
027900120321     c                   endIF
028000120320      *
028100120320      *                    ========
028200120320     c                   ENDIF
028300120320      *                    ========
028400120320      *
028500120320     c                   end
028600120320      *
028700120320     c                   endSR
028800090424     C*---------------------------------------------------------------*
028900111122      *  Copia la tariffa
029000090424      *---------------------------------------------------------------*
029100120320     c     COPIA_DETTAGLIbegSR
029200111129      *
029300111201     C     add01key      setll     fiADD01L
029400111201     C     add01key      reade     fiADD01L
029500111122      *
029600111201     c                   dow       not %EoF(fiADD01L)
029700111201      *  copia con nuovo prg.
029800111201     c                   eval        addPRG = newPRG
029900111201     c                   write     fiadd000
030000111201      *
030100111201     C     add01key      reade     fiADD01L
030200111201     c                   end
030300111129      *
030400111122     c                   endSR
030500111122     C*---------------------------------------------------------------*
030600111122      *  Controlli sull'Anagrafica
030700111122      *---------------------------------------------------------------*
030800111122     c     Ctrl_APD      begSR
030900111122      *
031000111122      * rileva dall'anagrafica
031100111201     c                   eval      apdTIP = 'D'
031200111122     c                   eval      apdPDR = stpPDR
031300111122     c     Key_APD       chain     fiapd01l
031400111122     c                   if        %Found(fiapd01l)
031500111122      *
031600111122     c                   if        apdATB <> *blank
031700111123     c                   movel(p)  'ANNULLATO'   errore
031800111122     c                   else
031900111122      *
032000111122      *-  Esluso da autofatturazione
032100111122     c                   if        apdPDD = 'S'
032200111123     c                   movel(p)  'AUTOFAT'     errore
032300111122     c                   end
032400111122      *
032500111122     c                   endIF
032600111122      *
032700111122     c                   else
032800111123     c                   movel(p)  'NO_APD'      errore
032900111122     c                   end
033000111122      *
033100111122     c                   endSR
033200111122     C*---------------------------------------------------------------*
033300111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
033400111122      *---------------------------------------------------------------*
033500111122     c     WRI_DETTA     begSR
033600111122     C*
033700111201     c                   clear                   WSADT000
033800090424     C*
033900090424     c                   eval      WSFPDR  =  stpPDR
034000090424     c                   eval      WSFTSR  =  stpTSR
034100111201     c                   eval      WSFPRG  =  stpPRG
034200111201     c                   eval      WSFDDT  =  stpDDT
034300111201     c                   eval      WSFDST  =  stpDST
034400111201     c                   eval      WSFDCN  =  stpDCN
034500111201     c                   eval      WSFDTS  =  stpDTS
034600111201     c                   eval      WSFDUV  =  stpDUV
034700111201     c                   eval      WSFDRC  =  stpDRC
034800111201     c                   eval      WSFNRC  =  stpNRC
034900111201     c                   eval      WSFCDF  =  stpCDF
035000111201     c                   eval      WSFCSF  =  stpCSF
035100111201     c                   eval      WSFFIL  =  stpFIL
035200111201      *
035300111201     c                   eval      WSFFILE = 'FIADT00F'
035400111201     c                   eval      WSFREC  = ADT_REC
035500120320     c                   eval      WSFPROGR= 'FICNX85R'
035600111123     c                   eval      WSFtime = datetime
035700090428     c**
035800111206     c**
035900090428     c                   eval      WSFNOTE = Errore
036000090424     c**
036100111201     c                   write     WSADT000
036200090424     C*
036300090424     c                   endSR
036400081211     C*---------------------------------------------------------------*
036500081110     OQSYSPRT   E            TESTA            02
036600120320     O                                           10 'FICNX85R'
036700081110     O                                           25 'Autista'
036800081110     O                                           30 'TSR'
036900081110     O                                           45 'Progr'
037000081110     O                                           55 'Data_Dec'
037100081110     O                                           65 'Data_Sca'
037200081110     O                                           75 'Data_Stp'
037300081110     O                                           85 'Data_Cnv'
037400111201     O                                          100 'N_Rec_ADT'
037500111123     O                                          111 'N O T E'
037600111123     O                       data               132 '  /  /    '
037700111123     O          E            TESTA       1
037800111123     O                       ora                130 '  :  :  '
037900111123      *
038000081110     O          E            DETta       1
038100081110     O                       stpPDR        z     25
038200081110     O                       stpTSR              29
038300081110     O                       stpPRG        z     43
038400081110     O                       stpDDT        z     55
038500081110     O                       stpDST        z     65
038600081110     O                       stpDTS        z     75
038700081110     O                       stpDCN        z     85
038800111201     o                       ADT_REC       z    100
038900111123     O                       errore             116
039000081110      ****
039100081110     o          E            FINEST      2
039200111123     o                                            1 ' '
039300120320     o                                          + 0 '*** FICNX85R ***'
039400111201     o                                          + 9 '***  DUPLI - AFF/DEF ***'
039500111123     o                                          + 9 '***   FINE  STAMPA  ***'
039600111123     O                       data               + 9 '  /  /    '
039700111123     O                       ora                + 3 '  :  :  '
039800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
