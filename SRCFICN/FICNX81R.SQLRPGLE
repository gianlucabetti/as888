000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120320     H* FICNX81R *---------------------------------------------------*
000300081110     H*                                                              *
000400120320     H* Per gli AUTISTI NON ancora ACCREDITATI:                      *
000500120320     H* Duplica le tariffe in essere x la data decorrenza facendole  *
000600120320     H*  scadere il giorno prima in presenza di un Contratto aperto  *
000700120320     H*  nel giorno della Nuova Decorrenza.                          *
000800120320     H*                                                              *
000900120320     H* I NON accreditati non hanno sulla tariffa il Nr.Contratto    *
001000120320     H*  ma solo la Società/Fornitore con i quali è possibile trovare*
001100120320     H*  la relativa PIVA....poi con Società/PIVA si controlla sul   *
001200120320     H*  file contratti se presente un contratto aperto in DATA di   *
001300120320     H*  trascodifica. (Attenzione Conversione CONTRATTI deve        *
001400120320     H*  girare prima delle TARIFFE)                                 *
001500120320     H*                                                              *
001600120319     H* riceve come parametro la Data decorrenza                     *
001700111122     H*  e se deve eseguire l'aggiornamento                          *
001800000000     H*--------------------------------------------------------------*
001900111122     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
002000111206     F                                     commit
002100111122     Ffitgt01L  uF a E           K DISK
002200111206     F                                     commit
002300111122      *
002400090401     Ffiapd01L  iF   E           K DISK
002500120320     Faitrs02L  iF   E           K DISK
002600090401      *
002700111129     FwfFGTXXF  o  a E             DISK
002800090424     FwxFGT00S  o  a E             DISK
002900081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000081211      * -------------------------------------------------------------
003100111122     D KPJBA         E DS
003200081110     D dsFGT         E DS                  ExtName(FIFGT00F)
003300081110     d                                     prefix(STP:3)
003400081110     D FGT_REC         s              9s 0
003500090424     D STP_DATSCAD     s              8s 0
003600120319     D*
003700120319     D trmz70s4ds    E DS                  prefix(S4_)
003800081230      *
003900111122     D oldPRG          s                   like(tgtPrg)
004000111122     D newPRG          s                   like(tgtPrg)
004100111123     D esito           s              9s 0
004200111123     D esito_ALF       s              9a
004300090109      *
004400111123     D DATA_eur        S               D   DATFMT(*eur)
004500111123     D DATA_iso        S               D   DATFMT(*iso)
004600111123      *
004700111205     D DA_Copiare      S              1A
004800111122     D ficn06ds      E DS
004900120320      *
005000111123     C*------------------------------------------------------------------------*
005100111123     C*        - OPERAZIONI INIZIALI
005200111123     C*------------------------------------------------------------------------*
005300111123     C     *ENTRY        PLIST
005400111123     C                   PARM                    DATADEC           8
005500111123     C                   PARM                    AGGIORNA          1
005600111123     C                   PARM                    ESITO_ALF
005700111123     C*
005800111123     C     AGGIORNA      comp      ' '                                    44
005900111123     C                   move      datadec       DATA_dec          8 0
006000111123     C     *iso          movel     DATA_DEC      data_iso
006100111123     c     data_iso      subdur    1:*days       data_iso
006200111123     C     *iso          movel     data_iso      data_sca          8 0
006300111123     C                   z-add     data_sca      DATAscad          8 0
006400111123     C*
006500111123      * duplica la testata TGT
006600111123     C     tgt01key      kLIST
006700111123     C                   kfld                    stpPDR
006800111123     C                   kfld                    stpSML
006900111123     C                   kfld                    stpPRG
007000111123     C*
007100111123     c     Key_APD       KLIST
007200111123     c                   kfld                    apdTIP
007300111123     c                   kfld                    apdPDR
007400111123      *
007500120320     C* lettura contratti
007600120320     c                   eval       data_0 = 0
007700120320     c     Key_TRS       KLIST
007800120320     c                   kfld                    s4_Societa
007900120320     c                   kfld                    PIVA
008000120320     c                   kfld                    data_0            8 0
008100120320      *
008200111123     c                   clear                   esito
008300111123     c                   clear                   esito_ALF
008400111123     c                   time                    datetime         14 0
008500111123     c                   move      datetime      data              8 0
008600111123     c                   movel     datetime      ora               6 0
008700111123      *
008800111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
008900111123     C                   clear                   UDATE_8           8 0
009000111123     C                   MOVEL     *DATE         UDATE_8
009100111122     c                   clear                   savSTPpdr         7 0
009200081209     C                   SETON                                        LR
009300090423      **
009400090423     C*=================================================================
009500111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
009600090423     C*=================================================================
009700120319      *  Tutte le TARIFFE degli AUT di città con scadenza OLTRE la data scadenza
009800120319      *   che sono presenti come NON ACCREDITATI e che possono avere collegato
009900120319      *   un contratto stampato a MODO vecchio ossia SENZA la PRIMA DATA CERTA e
010000120319      *   inoltre non sono presenti sul file di controllo del processo di duplica.
010100120319      *
010200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
010300120319     C+ select rrn(fifgt00f), fifgt00f.* FROM fifgt00f WHERE
010400120319     C+     fgttsr in ('C', 'R', 'G') and fgtsml=' ' and
010500120319     C+                  fgtdst > :DATAscad
010600120319     C+               and  fgtPDR * 1000 + fgtPRG
010700120319     C+                          IN
010800120319     C+    (Select tgtpdr * 1000 + tgtprg from fiTGT00F
010900120319     C+  where TGTsml=' '  and TGTnrc = 0 and tgtATB = ' ')
011000120319     C+                    and FgtPDR
011100120319     C+                      NOT IN
011200120319     C+          (Select fgtpdr from WFFGTxxF)
011300120319     C+  ORDER BY Fgtpdr
011400111122     C/END-EXEC
011500111122
011600111122     C*=====
011700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
011800111122     C*=====
011900081110     c                   if        sqlCODE < 0
012000081110     c                   eval      esito = Sqlcode
012100111123     c                   move      esito         esito_ALF
012200081110     c                   dump(a)
012300081110     c                   return
012400081110     c                   end
012500081110     C/EXEC SQL
012600081110     C+ OPEN A1
012700081110     C/END-EXEC
012800111122     C*=====
012900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
013000111122     C*=====
013100081110     c                   if        sqlCODE < 0
013200081110     c                   eval      esito = Sqlcode
013300111123     c                   move      esito         esito_ALF
013400081110     c                   dump(a)
013500081110     c                   return
013600081110     c                   else
013700111122      *
013800081110     c                   except    testa
013900111122      *
014000081110     c                   end
014100090109      * >>>>>>>
014200081209      * resto nel ciclo finchè....
014300081110     C                   DOU       SqlCod < 0
014400081110      * lettura cursore
014500081110     C/EXEC SQL
014600111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
014700081110     C/END-EXEC
014800111122     C*=====
014900111122      *  EoF
015000081110     C                   if        SqlCod = 100
015100081110     c                   Leave
015200111122     C*=====
015300081110      *  x errori
015400081110     C                   ElseIf    SqlCod < 0
015500081110     c                   eval      esito = Sqlcode
015600111123     c                   move      esito         esito_ALF
015700081110     c                   dump(a)
015800081110     c                   leave
015900081110     C                   ENDif
016000090428      *
016100111123      *----------------------------->>>>>
016200111123      *  se sono qui ho letto un record correttamente
016300090428      *----------------------------->>>>>
016400111122      * Controlli su Anagrafica
016500111123     c                   clear                   errore           15
016600111122     c                   exsr      Ctrl_APD
016700120319      *
016800120319      * x qualsiasi tipo di errore controllato prima non deve fare nulla e
016900120319      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
017000111123      *----------------------------->>>>>
017100120320     c                   clear                   eseguito          1
017200120320      *
017300120319     c                   if        Errore = *blank  and Aggiorna = 'S'
017400120319      *-------->>>>>
017500120319     c                   exsr      COPIA_TESTATA
017600120319      ***
017700120319     c                   exsr      COPIA_DETTAGLI
017800120319      *-------->>>>>
017900090428     c                   end
018000090428      *----------------------------->>>>>
018100120319      *
018200111206     c                   if        errore  = *blank
018300120320     c                   if            eseguito = 'S'
018400120320     c                   eval        Errore = 'DUPLICATAok'
018500120320     c                   else
018600111206     c                   eval        Errore = 'DUPLICA TAR'
018700120320     c                   end
018800111206     c                   end
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
021000120319     C*---------------------------------------------------------------*
021100120319      *  Copia la tariffa  TGT
021200120319      *---------------------------------------------------------------*
021300120319     c     COPIA_TESTATA begSR
021400120319      *
021500120319      *  Deve scrivere la testata a rottura di autista
021600120319      *-+++++++++++++++--
021700120319     c                   if        stpPDR <> savSTPpdr
021800120319      *-+++++++++++++++--
021900120319      *  salva chiave di rottura
022000120319     c                   eval              savSTPpdr = stpPDR
022100120319     c                   eval      DA_Copiare = 'S'
022200120319      *
022300120319      *   Aggancia la testata
022400120319     C     tgt01key      chain     fiTGT01L
022500120319      *
022600120319     c                   if        NOT %Found(fiTGT01L)
022700120319     c                   eval      DA_Copiare = 'N'
022800120319     c                   eLSe
022900120319      *
023000120319      *  controlla per partita IVA se è stato aperto un CONTRATTO in data
023100120319      *   decorrenza come da Parametro.
023200120319     c                   exsr      CTRL_PIVA
023300120319      *
023400120319      * se NON ha agganciato un contratto a MODO Vecchio
023500120319      *     NON è da duplicare
023600120319     c                   if        DA_Copiare = 'N'
023700120319     c                   update    fiTGT000
023800120319      *
023900120319      *  SOLO per contratto a MODO vecchio
024000120319     c                   elseIF    DA_Copiare = 'S'
024100120319      * TESTATA OK:
024200120319      * ---------------------
024300120319      ***
024400120319      *   Mette in SCADENZA la tariffa attuale e aggiorna
024500120319     c                   z-add     DATA_sca      tgtDST
024600120319     c                   eval        oldPRG = tgtPRG
024700120319     c                   update    fiTGT000
024800120319      *
024900120319      *   Apre in Decorrenza e scrive il nuovo Progressivo
025000120319     c                   eval        tgtPRG = tgtPRG + 1
025100120319     c                   eval        newPRG = tgtPRG
025200120319     c                   movel     DATA_dec      tgtDDT
025300120319     c                   z-add     20391231      tgtDST
025400120319     c                   z-add     *date         tgtDUV
025500120319     c                   eval        tgtPRU = 'New04_2012'
025600120319      *
025700120321     c                   z-add     0             TGTfil
025800120321     c                   z-add     0             TGTNRC
025900120320     c                   z-add     0             TGTPCAR
026000120319     c                   z-add     0             TGTCCKM
026100120319     c                   z-add     *date         TGTDTS
026200120319     c                   z-add     0             TGTDCN
026300120319     c                   z-add     0             TGTDRC
026400120319      *  quindi scrive il progressivo seguente
026500120319     c                   write     fitgt000
026600120320     c                   eval          eseguito = 'S'
026700120319     c                   end
026800120319      *
026900120319     c                   endIF
027000120319      *-+++++++++++++++--
027100120319     c                   endIF
027200120319      *-+++++++++++++++--
027300120319      *
027400120319      *    esce con il DA_Copiare impostato
027500120319      *
027600120319     c                   endSR
027700120319     C*---------------------------------------------------------------*
027800120319      *  Controlla che con il fornitore SOC/FORNITORE sulla tariffa
027900120319      *    vi sia un Contratto aperto in data Decorrenza
028000120319      *  Se SI deve procedere con la duplica della Tariffa.
028100120319      *---------------------------------------------------------------*
028200120319     c     CTRL_PIVA     begSR
028300120319      *
028400120319      * ricava la Partita IVA
028500120319     c                   exsr      Decod_P_IVA
028600120319      *
028700120320      *  quindi per società/PIVA cerca il contratto e verifica se è
028800120320      *   fatto a modo vecchio o a modo nuovo.
028900120320     c                   eval      Da_COPIARE = 'N'
029000120320      *
029100120320      *  prende il contratto attivo con DATA FINE CONTRATTO = 0
029200120320      *   e se è stato APERTO nella DATA di CONVERSIONE (Nuova DECORRENZA)
029300120320      *  allora significa che deve Duplicare la tariffa.
029400120320     c     Key_TRS       chain     aitrs02l
029500120320     c                   if        %found(aitrs02l)
029600120320     c                                and
029700120320     c                             TRSDEC = DATA_dec
029800120320      *
029900120320     c                   eval      DA_Copiare = 'S'
030000120320      *
030100120320     c                   end
030200120319      *
030300120320      *   esce con DA_Copiare=S/N
030400120319      *
030500120319     c                   endSR
030600120319     C*---------------------------------------------------------------*
030700120319      *  Copia la tariffa
030800120319      *---------------------------------------------------------------*
030900120319     c     COPIA_DETTAGLIbegSR
031000120319      *
031100120319      ***
031200120319      ***  Solo se la TESTATA ha dato esito positivo
031300120319      ***
031400120319     c                   if        DA_Copiare = 'S'
031500120319      *
031600120319      *   deve far scadere il record dell'FGT
031700120319     c     fgt_REC       chain     FIFGT00F
031800120319     c                   if        %Found(FIFGT00F)
031900120319      * aggiorna
032000120319     c                   eval      fgtDST = DATA_sca
032100120319     c                   update    fgtFisico
032200120319      *  copia Tariffa
032300120319     c                   exsr      COPIA_TAR
032400120319     c                   end
032500120319      *
032600120319     c                   end
032700120319      *
032800120319     c                   endSR
032900120319     C*---------------------------------------------------------------*
033000120319      *  Copia la tariffa
033100120319      *---------------------------------------------------------------*
033200120319     c     COPIA_tar     begSR
033300120319      *
033400111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
033500111129      *    se si fosse interrotto a metà
033600111129     c                   write     wfFGTxxx
033700111122      *
033800111122     c                   clear                   ficn06ds
033900111123     c                   movel     fgtPDR        FILOLD
034000111123     c                   move      fgtPDR        PDROLD
034100111123     c                   move      fgtTSR        TSROLD
034200111123     c                   z-add     fgtCTR        CTROLD
034300111122     c                   z-add     oldPRG        PRGOLD
034400111123     c                   movel     fgtPDR        FILNEW
034500111123     c                   move      fgtPDR        PDRNEW
034600111122     c                   z-add     newPRG        PRGNEW
034700111122      *
034800111123     c                   eval      kpjbu = ficn06ds
034900111122     c                   call      'FICN06R'
035000111122     c                   parm                    KPJBA
035100111129      *
035200111122     c                   endSR
035300111122     C*---------------------------------------------------------------*
035400111122      *  Controlli sull'Anagrafica
035500111122      *---------------------------------------------------------------*
035600111122     c     Ctrl_APD      begSR
035700111122      *
035800111122      * rileva dall'anagrafica
035900111122     c                   eval      apdTIP = 'A'
036000111122     c                   eval      apdPDR = stpPDR
036100111122     c     Key_APD       chain     fiapd01l
036200111122     c                   if        %Found(fiapd01l)
036300111122      *
036400111122     c                   if        apdATB <> *blank
036500111123     c                   movel(p)  'ANNULLATO'   errore
036600111122     c                   else
036700111122      *
036800111122      *-  Esluso da autofatturazione
036900111122     c                   if        apdPDD = 'S'
037000111123     c                   movel(p)  'AUTOFAT'     errore
037100111122     c                   end
037200111122      *
037300111122     c                   endIF
037400111122      *
037500111122     c                   else
037600111123     c                   movel(p)  'NO_APD'      errore
037700111122     c                   end
037800111122      *
037900111122     c                   endSR
038000111122     C*---------------------------------------------------------------*
038100111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
038200111122      *---------------------------------------------------------------*
038300111122     c     WRI_DETTA     begSR
038400111122     C*
038500090424     c                   clear                   WSFGT000
038600090424     C*
038700090424     c                   eval      WSFPDR  =  stpPDR
038800090424     c                   eval      WSFTSR  =  stpTSR
038900090424     c                   eval      WSFCTR  =  stpCTR
039000090424     c                   eval      WSFPRG  = stpPRG
039100090424     c                   eval      WSFDDT  = stpDDT
039200090424     c                   eval      WSFDST  = stpDST
039300090424     c                   eval      WSFDTS  = stpDTS
039400090424     c                   eval      WSFDCN  = stpDCN
039500111129     c                   eval      WSFFILE = 'FIFGT00F'
039600090424     c                   eval      WSFREC  = FGT_REC
039700120320     c                   eval      WSFPROGR= 'FICNX81R'
039800111123     c                   eval      WSFtime = datetime
039900090428     c**
040000090428     c                   eval      WSFNOTE = Errore
040100090424     c**
040200090424     c                   write     WSFGT000
040300090424     C*
040400090424     c                   endSR
040500120319     C/EJECT
040600120319     c*-------------------------------------------------------------------------
040700120319     c* tramite Società e Unità decodifica P.IVA su PROJ
040800120319     c*-------------------------------------------------------------------------
040900120319     C     Decod_P_IVA   BegSR
041000120319      **
041100120319      **  Routine x Reperire dati PIVA:
041200120319      **    La routine in base alla sottonatura può funzionare
041300120319      **     x F=Fornitore/C=Cliente
041400120319     C                   clear                   trmz70s4ds                     Input
041500120319     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
041600120319     C                   move      tgtCDF        s4_keyKSC                      Input
041700120319     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
041800120319     C                   movel(p)  tgtSOC        s4_Societa                     Input/Output
041900120319     c                   if        tgtFIL      > 0
042000120319     C                   movel(p)  tgtFIL        s4_Unita                       Input/Output
042100120319     c                   end
042200120319     c                   call      'TRMZ70SR4'
042300120319     c                   parm                    trmz70s4ds
042400120319      *
042500120319      *  se torna con l'errore riprova e aveva passato l'unità
042600120319      *   riprova passandogli l'unità vuota
042700120319     c                   if        s4_errore <> '0'
042800120319     C                   clear                   trmz70s4ds                     Input
042900120319     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
043000120319     C                   move      tgtCDF        s4_keyKSC                      Input
043100120319     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
043200120319     C                   movel(p)  tgtSOC        s4_Societa                     Input/Output
043300120319     c                   call      'TRMZ70SR4'
043400120319     c                   parm                    trmz70s4ds
043500120319     c                   end
043600120319      *
043700120320     c                   clear                   desfrn           35
043800120320     c                   clear                   piva             16
043900120319      *  Descrizione Ragione Sociale FORNITORE
044000120319     c                   if        s4_errore = '0'
044100120319     c                   eval        DESFRN = s4_RAGSOCKSC
044200120319     c                   eval        PIVA   = s4_PARTITAIVA
044300120319     c                   end
044400120319      *
044500120319     C                   ENDSR
044600120319     C/EJECT
044700081211     C*---------------------------------------------------------------*
044800081110     OQSYSPRT   E            TESTA            02
044900120320     O                                           10 'FICNX81R'
045000081110     O                                           25 'Autista'
045100081110     O                                           30 'TSR'
045200081110     O                                           38 'Tariffa'
045300081110     O                                           45 'Progr'
045400081110     O                                           55 'Data_Dec'
045500081110     O                                           65 'Data_Sca'
045600081110     O                                           75 'Data_Stp'
045700081110     O                                           85 'Data_Cnv'
045800111123     O                                          100 'N_Rec_FGT'
045900111123     O                                          111 'N O T E'
046000111123     O                       data               132 '  /  /    '
046100111123     O          E            TESTA       1
046200111123     O                       ora                130 '  :  :  '
046300111123      *
046400081110     O          E            DETta       1
046500081110     O                       stpPDR        z     25
046600081110     O                       stpTSR              29
046700081110     O                       stpCTR              36
046800081110     O                       stpPRG        z     43
046900081110     O                       stpDDT        z     55
047000081110     O                       stpDST        z     65
047100081110     O                       stpDTS        z     75
047200081110     O                       stpDCN        z     85
047300111123     o                       FGT_REC       z    100
047400111123     O                       errore             116
047500081110      ****
047600081110     o          E            FINEST      2
047700111123     o                                            1 ' '
047800120320     o                                          + 0 '*** FICNX81R ***'
047900111123     o                                          + 9 '***  DUPLI - TARIFFA ***'
048000111123     o                                          + 9 '***   FINE  STAMPA  ***'
048100111123     O                       data               + 9 '  /  /    '
048200111123     O                       ora                + 3 '  :  :  '
048300111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
