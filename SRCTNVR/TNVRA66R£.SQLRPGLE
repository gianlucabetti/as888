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
001100111201     Ffiadd01L  iF a E           K DISK
001200111122      *
001300090401     Ffiapd01L  iF   E           K DISK
001400090401      *
001500111201     FwfADTXXF  o  a E             DISK
001600111201     FwxADT00S  o  a E             DISK
001700081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800081211      * -------------------------------------------------------------
001900111122     D KPJBA         E DS
002000111201     D dsADT         E DS                  ExtName(FIADT00F)
002100081110     d                                     prefix(STP:3)
002200111201     D ADT_REC         s              9s 0
002300090424     D STP_DATSCAD     s              8s 0
002400081230      *
002500111201     D oldPRG          s                   like(adtPrg)
002600111201     D newPRG          s                   like(adtPrg)
002700111201      *
002800111123     D esito           s              9s 0
002900111123     D esito_ALF       s              9a
003000090109      *
003100111123     D DATA_eur        S               D   DATFMT(*eur)
003200111123     D DATA_iso        S               D   DATFMT(*iso)
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
004800111201      * duplica la testata ADT
004900111201     C     adt01key      kLIST
005000111123     C                   kfld                    stpPDR
005100111123     C                   kfld                    stpPRG
005200111123     C*
005300111201     C     add01key      kLIST
005400111201     C                   kfld                    stpPDR
005500111201     C                   kfld                    oldPRG
005600111201     C*
005700111123     c     Key_APD       KLIST
005800111123     c                   kfld                    apdTIP
005900111123     c                   kfld                    apdPDR
006000111123      *
006100111123     c                   clear                   esito
006200111123     c                   clear                   esito_ALF
006300111123     c                   time                    datetime         14 0
006400111123     c                   move      datetime      data              8 0
006500111123     c                   movel     datetime      ora               6 0
006600111123      *
006700111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006800111123     C                   clear                   UDATE_8           8 0
006900111123     C                   MOVEL     *DATE         UDATE_8
007000111122     c                   clear                   savSTPpdr         7 0
007100111201      *
007200081209     C                   SETON                                        LR
007300090423     C*=================================================================
007400111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007500090423     C*=================================================================
007600111201      *   TESTATE AFFL.DEFL.
007700111201      *
007800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007900111201     C+   SELECT rrn(fiadt00f), fiadt00f.*
008000111201     C+     FROM fiadt00f
008100111201     C+    WHERE adtdst > :DATAscad and adtpdr
008200111123     C+                       IN
008300111201     C+ (Select traBPT from Aitra00f where tradfi=0 and traann=' ')
008400111201     C+             and adtpdr  NOT IN
008500111201     C+        (Select adtpdr from WFADTXXF)
008600111201     C+               order by ADTpdr
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
011900111201     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
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
014000111201      *----
014100090427     c   Of              except    testa
014200090427     c                   except    detta
014300090428      *
014400090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014500111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014600111123      *----------------------------->>>>>
014700111201     c                   if        Errore  = *blank  and Aggiorna = 'S'
014800090428      *----------------------------->>>>>
014900111122      *   Aggancia la testata
015000111201     C     ADt01key      chain     fiADT01L
015100111201     c                   if        %Found(fiADT01L)
015200111201      *
015300111201      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
015400111201      *    se si fosse interrotto a metà
015500111201     c                   write     fiADTxxx
015600111201      *
015700111122      *   Mette in SCADENZA
015800111201     c                   z-add     DATA_sca      adtDST
015900111201     c                   eval        oldPRG = adtPRG
016000111201     c                   update    fiADT000
016100111201      * ---------
016200111122      *   Mette in Decorrenza il nuovo Progressivo
016300111201     c                   eval        adtPRG = adtPRG + 1
016400111201     c                   eval        newPRG = adtPRG
016500111201     c                   movel     DATA_dec      adtDDT
016600111201     c                   z-add     20391231      adtDST
016700111122      *
016800111201     c                   z-add     *date         adtDUV
016900111201     c***********        z-add     *date         ADTDTS
017000111122      *
017100111201     c                   z-add     0             ADTPCAR
017200111201     c                   z-add     0             ADTDRC
017300111201     c                   z-add     0             ADTDCN
017400111122      *
017500111122      *  quindi scrive il progressivo seguente
017600111201     c                   write     fiadt000
017700111122      *  copia Tariffa
017800111122     c                   exsr      COPIA_TAR
017900090112      *
018000111201     c                   end
018100090428      *----------------------------->>>>>
018200111201     c                   endIF
018300090428      *----------------------------->>>>>
018400090424      *  scrive il dettaglio su un file
018500090424     c                   exsr      wri_DETTA
018600081110      *
018700081110     C                   ENDDO
018800081110     C/EXEC SQL
018900081110     C+ Close A1
019000081110     C/END-EXEC
019100081110     C                   if        Esito < 0
019200111123     c                   move      esito         esito_ALF
019300081110     c                   return
019400081110     c                   endIF
019500081110     C*
019600081110     C     FINE          TAG
019700081110     c                   except    finest
019800081110     c                   return
019900090424     C*---------------------------------------------------------------*
020000111122      *  Copia la tariffa
020100090424      *---------------------------------------------------------------*
020200111122     c     COPIA_tar     begSR
020300111129      *
020400111201     C     add01key      setll     fiADD01L
020500111201     C     add01key      reade     fiADD01L
020600111122      *
020700111201     c                   dow       not %EoF(fiADD01L)
020800111201      *  copia con nuovo prg.
020900111201     c                   eval        addPRG = newPRG
021000111201     c                   write     fiadd000
021100111201      *
021200111201     C     add01key      reade     fiADD01L
021300111201     c                   end
021400111129      *
021500111122     c                   endSR
021600111122     C*---------------------------------------------------------------*
021700111122      *  Controlli sull'Anagrafica
021800111122      *---------------------------------------------------------------*
021900111122     c     Ctrl_APD      begSR
022000111122      *
022100111122      * rileva dall'anagrafica
022200111201     c                   eval      apdTIP = 'D'
022300111122     c                   eval      apdPDR = stpPDR
022400111122     c     Key_APD       chain     fiapd01l
022500111122     c                   if        %Found(fiapd01l)
022600111122      *
022700111122     c                   if        apdATB <> *blank
022800111123     c                   movel(p)  'ANNULLATO'   errore
022900111122     c                   else
023000111122      *
023100111122      *-  Esluso da autofatturazione
023200111122     c                   if        apdPDD = 'S'
023300111123     c                   movel(p)  'AUTOFAT'     errore
023400111122     c                   end
023500111122      *
023600111122     c                   endIF
023700111122      *
023800111122     c                   else
023900111123     c                   movel(p)  'NO_APD'      errore
024000111122     c                   end
024100111122      *
024200111122     c                   endSR
024300111122     C*---------------------------------------------------------------*
024400111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
024500111122      *---------------------------------------------------------------*
024600111122     c     WRI_DETTA     begSR
024700111122     C*
024800111201     c                   clear                   WSADT000
024900090424     C*
025000090424     c                   eval      WSFPDR  =  stpPDR
025100090424     c                   eval      WSFTSR  =  stpTSR
025200111201     c                   eval      WSFPRG  =  stpPRG
025300111201     c                   eval      WSFDDT  =  stpDDT
025400111201     c                   eval      WSFDST  =  stpDST
025500111201     c                   eval      WSFDCN  =  stpDCN
025600111201     c                   eval      WSFDTS  =  stpDTS
025700111201     c                   eval      WSFDUV  =  stpDUV
025800111201     c                   eval      WSFDRC  =  stpDRC
025900111201     c                   eval      WSFNRC  =  stpNRC
026000111201     c                   eval      WSFCDF  =  stpCDF
026100111201     c                   eval      WSFCSF  =  stpCSF
026200111201     c                   eval      WSFFIL  =  stpFIL
026300111201      *
026400111201     c                   eval      WSFFILE = 'FIADT00F'
026500111201     c                   eval      WSFREC  = ADT_REC
026600111201     c                   eval      WSFPROGR= 'TNVRA65R'
026700111123     c                   eval      WSFtime = datetime
026800090428     c**
026900090428     c                   eval      WSFNOTE = Errore
027000090424     c**
027100111201     c                   write     WSADT000
027200090424     C*
027300090424     c                   endSR
027400081211     C*---------------------------------------------------------------*
027500081110     OQSYSPRT   E            TESTA            02
027600111201     O                                           10 'TNVRA65R'
027700081110     O                                           25 'Autista'
027800081110     O                                           30 'TSR'
027900081110     O                                           45 'Progr'
028000081110     O                                           55 'Data_Dec'
028100081110     O                                           65 'Data_Sca'
028200081110     O                                           75 'Data_Stp'
028300081110     O                                           85 'Data_Cnv'
028400111201     O                                          100 'N_Rec_ADT'
028500111123     O                                          111 'N O T E'
028600111123     O                       data               132 '  /  /    '
028700111123     O          E            TESTA       1
028800111123     O                       ora                130 '  :  :  '
028900111123      *
029000081110     O          E            DETta       1
029100081110     O                       stpPDR        z     25
029200081110     O                       stpTSR              29
029300081110     O                       stpPRG        z     43
029400081110     O                       stpDDT        z     55
029500081110     O                       stpDST        z     65
029600081110     O                       stpDTS        z     75
029700081110     O                       stpDCN        z     85
029800111201     o                       ADT_REC       z    100
029900111123     O                       errore             116
030000081110      ****
030100081110     o          E            FINEST      2
030200111123     o                                            1 ' '
030300111201     o                                          + 0 '*** TNVRA65R ***'
030400111201     o                                          + 9 '***  DUPLI - AFF/DEF ***'
030500111123     o                                          + 9 '***   FINE  STAMPA  ***'
030600111123     O                       data               + 9 '  /  /    '
030700111123     O                       ora                + 3 '  :  :  '
030800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
