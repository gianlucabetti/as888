000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120320     H* FICNX86R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111201     H*  facendole scadere il giorno prima.     AFFL/DEFL            *
000600120320     H*  Quelle che NON HANNO ancora un CONTRATTO ossia NRC=0        *
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
001700090401     Ffiapd01L  iF   E           K DISK
001800120320     Faitrs02L  iF   E           K DISK
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
003200120320     D*
003300120320     D trmz70s4ds    E DS                  prefix(S4_)
003400120320     D DA_Copiare      S              1A
003500111201      *
003600111123     D esito           s              9s 0
003700111123     D esito_ALF       s              9a
003800090109      *
003900111123     D DATA_eur        S               D   DATFMT(*eur)
004000111123     D DATA_iso        S               D   DATFMT(*iso)
004100111123     C*------------------------------------------------------------------------*
004200111123     C*        - OPERAZIONI INIZIALI
004300111123     C*------------------------------------------------------------------------*
004400111123     C     *ENTRY        PLIST
004500111123     C                   PARM                    DATADEC           8
004600111123     C                   PARM                    AGGIORNA          1
004700111123     C                   PARM                    ESITO_ALF
004800111123     C*
004900111123     C     AGGIORNA      comp      ' '                                    44
005000111123     C                   move      datadec       DATA_dec          8 0
005100111123     C     *iso          movel     DATA_DEC      data_iso
005200111123     c     data_iso      subdur    1:*days       data_iso
005300111123     C     *iso          movel     data_iso      data_sca          8 0
005400111123     C                   z-add     data_sca      DATAscad          8 0
005500111123     C*
005600111201      * duplica la testata ADT
005700111201     C     adt01key      kLIST
005800111123     C                   kfld                    stpPDR
005900111123     C                   kfld                    stpPRG
006000111123     C*
006100111201     C     add01key      kLIST
006200111201     C                   kfld                    stpPDR
006300111201     C                   kfld                    oldPRG
006400111201     C*
006500111123     c     Key_APD       KLIST
006600111123     c                   kfld                    apdTIP
006700111123     c                   kfld                    apdPDR
006800120320      *
006900120320     C* lettura contratti
007000120320     c                   eval       data_0 = 0
007100120320     c     Key_TRS       KLIST
007200120320     c                   kfld                    s4_Societa
007300120320     c                   kfld                    PIVA
007400120320     c                   kfld                    data_0            8 0
007500120320      *
007600111123      *
007700111123     c                   clear                   esito
007800111123     c                   clear                   esito_ALF
007900111123     c                   time                    datetime         14 0
008000111123     c                   move      datetime      data              8 0
008100111123     c                   movel     datetime      ora               6 0
008200111123      *
008300111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
008400111123     C                   clear                   UDATE_8           8 0
008500111123     C                   MOVEL     *DATE         UDATE_8
008600111122     c                   clear                   savSTPpdr         7 0
008700111201      *
008800081209     C                   SETON                                        LR
008900090423     C*=================================================================
009000111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
009100090423     C*=================================================================
009200111201      *   TESTATE AFFL.DEFL.
009300111213      *  Aggiunto il pezzo di leggere solo quelle tariffe che hanno il prezzo
009400111213      *   carburante poichè sono quelle rimaste al vecchio modo e quindi da copiare.
009500111201      *
009600090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009700120320     C+        SELECT rrn(fiadt00f), fiadt00f.*
009800120320     C+          FROM fiadt00f WHERE adtNRC = 0
009900120320     C+           and adtdst > :DATAscad and
010000120320     C+                    adtpdr
010100120320     C+                    NOT IN
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
016300120320     c                   clear                   eseguito          1
016400120320      **
016500120320     c                   exsr      COPIA_TESTATA
016600120320      ***
016700090428      *----------------------------->>>>>
016800111201     c                   endIF
016900090428      *----------------------------->>>>>
017000111206     c                   if        errore  = *blank
017100120320     c                   if            eseguito = 'S'
017200120320     c                   eval        Errore = 'DUPLICATAok'
017300120320     c                   else
017400111206     c                   eval        Errore = 'DUPLICA TAR'
017500120320     c                   end
017600111206     c                   end
017700111206      *----
017800111206     c   Of              except    testa
017900111206     c                   except    detta
018000111206      *
018100090424      *  scrive il dettaglio su un file
018200090424     c                   exsr      wri_DETTA
018300081110      *
018400081110     C                   ENDDO
018500081110     C/EXEC SQL
018600081110     C+ Close A1
018700081110     C/END-EXEC
018800081110     C                   if        Esito < 0
018900111123     c                   move      esito         esito_ALF
019000081110     c                   return
019100081110     c                   endIF
019200081110     C*
019300081110     C     FINE          TAG
019400081110     c                   except    finest
019500081110     c                   return
019600120320     C*---------------------------------------------------------------*
019700120320      *  Copia la tariffa  TGT
019800120320      *---------------------------------------------------------------*
019900120320     c     COPIA_TESTATA begSR
020000120320      *
020100120320      *   Aggancia la testata
020200120320     C     ADt01key      chain     fiADT01L
020300120320     c                   if        %Found(fiADT01L)
020400120320      *
020500120320      *  controlla per partita IVA se è stato aperto un CONTRATTO in data
020600120320      *   decorrenza come da Parametro.
020700120320     c                   exsr      CTRL_PIVA
020800120320      * ++++++++++++
020900120320      * se NON ha agganciato un contratto a MODO Vecchio
021000120320      *     NON è da duplicare
021100120320     c                   if        DA_Copiare = 'N'
021200120320     c                   update    fiADT000
021300120320      *
021400120320      *  SOLO per contratto a MODO vecchio
021500120320     c                   elseIF    DA_Copiare = 'S'
021600120320      * ++++++++++++
021700120320      *
021800120320      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
021900120320      *    se si fosse interrotto a metà
022000120320     c                   write     fiADTxxx
022100120320      *
022200120320      *  Occorre verificare che la tariffa sia stata stampata in sede.
022300120320      *    allora può procedere con il far scadere e riaprire con un altro progressivo.
022400120320      *                    ========
022500120320     c                   IF        ADtDCN > 0
022600120320      *                    ========
022700120320      *   Mette in SCADENZA
022800120320     c                   z-add     DATA_sca      adtDST
022900120320     c                   eval        oldPRG = adtPRG
023000120320     c                   update    fiADT000
023100120320      * ---------
023200120320      *   Mette in Decorrenza il nuovo Progressivo
023300120320     c                   eval        adtPRG = adtPRG + 1
023400120320     c                   eval        newPRG = adtPRG
023500120320     c                   movel     DATA_dec      adtDDT
023600120320     c                   z-add     20391231      adtDST
023700120320      *
023800120320     c                   z-add     *date         adtDUV
023900120320     c                   z-add     *date         ADTDTS
024000120320     c                   z-add     0             ADTNRC
024100120321     c                   z-add     0             ADTfil
024200120320      *
024300120320     c                   z-add     0             ADTPCAR
024400120320     c                   z-add     0             ADTDRC
024500120320     c                   z-add     0             ADTDCN
024600120320      *  quindi scrive il progressivo seguente
024700120320     c                   write     fiadt000
024800120320     c                   eval          eseguito = 'S'
024900120320      *  copia Tariffa
025000120320     c                   exsr      COPIA_DETTAGLI
025100120320      *
025200120320      *                    ========
025300120320     c                   ELSE
025400120320      *                    ========
025500120320      * Altrimenti se NON è stata stampata in sede
025600120320      *   NON genero un nuovo progressivo ma elimino il costo del carburante
025700120320      **  .....è stata convalidata in FILIALE ????!!!!!
025800120320      ****  solo se la scadenza della Tariffa va oltre il limite di conversione.
025900120320      *                    ++++++++
026000120320      *   Se la scadenza della tariffa è superiore al limite di scadenza della conversione
026100120320      **************       ++++++++
026200120320     c                   IF        adtDST > datascad
026300120320     c                   z-add     0             ADTPCAR
026400120320     c                   movel(p)  'NON STAMPATA'errore
026500120320     c                   ENDIF
026600120320      ****
026700120320     c                   update    FiADT000
026800120320      *                    ========
026900120320     c                   ENDIF
027000120320      *                    ========
027100120320      *
027200120320      * ++++++++++++
027300120320     c                   end
027400120320      * ++++++++++++
027500120320     c                   end
027600120320      *
027700120320     c                   endSR
027800120320     C*---------------------------------------------------------------*
027900120320      *  Controlla che con il fornitore SOC/FORNITORE sulla tariffa
028000120320      *    vi sia un Contratto aperto in data Decorrenza
028100120320      *  Se SI deve procedere con la duplica della Tariffa.
028200120320      *---------------------------------------------------------------*
028300120320     c     CTRL_PIVA     begSR
028400120320      *
028500120320      * ricava la Partita IVA
028600120320     c                   exsr      Decod_P_IVA
028700120320      *
028800120320      *  quindi per società/PIVA cerca il contratto e verifica se è
028900120320      *   fatto a modo vecchio o a modo nuovo.
029000120320     c                   eval      Da_COPIARE = 'N'
029100120320      *
029200120320      *  prende il contratto attivo con DATA FINE CONTRATTO = 0
029300120320      *   e se è stato APERTO nella DATA di CONVERSIONE (Nuova DECORRENZA)
029400120320      *  allora significa che deve Duplicare la tariffa.
029500120320     c     Key_TRS       chain     aitrs02l
029600120320     c                   if        %found(aitrs02l)
029700120320     c                                and
029800120320     c                             TRSDEC = DATA_dec
029900120320      *
030000120320     c                   eval      DA_Copiare = 'S'
030100120320      *
030200120320     c                   end
030300120320      *
030400120320      *   esce con DA_Copiare=S/N
030500120320      *
030600120320     c                   endSR
030700090424     C*---------------------------------------------------------------*
030800111122      *  Copia la tariffa
030900090424      *---------------------------------------------------------------*
031000120320     c     COPIA_DETTAGLIbegSR
031100111129      *
031200111201     C     add01key      setll     fiADD01L
031300111201     C     add01key      reade     fiADD01L
031400111122      *
031500111201     c                   dow       not %EoF(fiADD01L)
031600111201      *  copia con nuovo prg.
031700111201     c                   eval        addPRG = newPRG
031800111201     c                   write     fiadd000
031900111201      *
032000111201     C     add01key      reade     fiADD01L
032100111201     c                   end
032200111129      *
032300111122     c                   endSR
032400111122     C*---------------------------------------------------------------*
032500111122      *  Controlli sull'Anagrafica
032600111122      *---------------------------------------------------------------*
032700111122     c     Ctrl_APD      begSR
032800111122      *
032900111122      * rileva dall'anagrafica
033000111201     c                   eval      apdTIP = 'D'
033100111122     c                   eval      apdPDR = stpPDR
033200111122     c     Key_APD       chain     fiapd01l
033300111122     c                   if        %Found(fiapd01l)
033400111122      *
033500111122     c                   if        apdATB <> *blank
033600111123     c                   movel(p)  'ANNULLATO'   errore
033700111122     c                   else
033800111122      *
033900111122      *-  Esluso da autofatturazione
034000111122     c                   if        apdPDD = 'S'
034100111123     c                   movel(p)  'AUTOFAT'     errore
034200111122     c                   end
034300111122      *
034400111122     c                   endIF
034500111122      *
034600111122     c                   else
034700111123     c                   movel(p)  'NO_APD'      errore
034800111122     c                   end
034900111122      *
035000111122     c                   endSR
035100111122     C*---------------------------------------------------------------*
035200111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
035300111122      *---------------------------------------------------------------*
035400111122     c     WRI_DETTA     begSR
035500111122     C*
035600111201     c                   clear                   WSADT000
035700090424     C*
035800090424     c                   eval      WSFPDR  =  stpPDR
035900090424     c                   eval      WSFTSR  =  stpTSR
036000111201     c                   eval      WSFPRG  =  stpPRG
036100111201     c                   eval      WSFDDT  =  stpDDT
036200111201     c                   eval      WSFDST  =  stpDST
036300111201     c                   eval      WSFDCN  =  stpDCN
036400111201     c                   eval      WSFDTS  =  stpDTS
036500111201     c                   eval      WSFDUV  =  stpDUV
036600111201     c                   eval      WSFDRC  =  stpDRC
036700111201     c                   eval      WSFNRC  =  stpNRC
036800111201     c                   eval      WSFCDF  =  stpCDF
036900111201     c                   eval      WSFCSF  =  stpCSF
037000111201     c                   eval      WSFFIL  =  stpFIL
037100111201      *
037200111201     c                   eval      WSFFILE = 'FIADT00F'
037300111201     c                   eval      WSFREC  = ADT_REC
037400120320     c                   eval      WSFPROGR= 'FICNX86R'
037500111123     c                   eval      WSFtime = datetime
037600090428     c**
037700111206     c**
037800090428     c                   eval      WSFNOTE = Errore
037900090424     c**
038000111201     c                   write     WSADT000
038100090424     C*
038200090424     c                   endSR
038300120320     C/EJECT
038400120320     c*-------------------------------------------------------------------------
038500120320     c* tramite Società e Unità decodifica P.IVA su PROJ
038600120320     c*-------------------------------------------------------------------------
038700120320     C     Decod_P_IVA   BegSR
038800120320      **
038900120320      **  Routine x Reperire dati PIVA:
039000120320      **    La routine in base alla sottonatura può funzionare
039100120320      **     x F=Fornitore/C=Cliente
039200120320     C                   clear                   trmz70s4ds                     Input
039300120320     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
039400120320     C                   move      adtCDF        s4_keyKSC                      Input
039500120320     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
039600120320     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
039700120320     c                   if        adtFIL      > 0
039800120320     C                   movel(p)  adtFIL        s4_Unita                       Input/Output
039900120320     c                   end
040000120320     c                   call      'TRMZ70SR4'
040100120320     c                   parm                    trmz70s4ds
040200120320      *
040300120320      *  se torna con l'errore riprova e aveva passato l'unità
040400120320      *   riprova passandogli l'unità vuota
040500120320     c                   if        s4_errore <> '0'
040600120320     C                   clear                   trmz70s4ds                     Input
040700120320     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
040800120320     C                   move      adtCDF        s4_keyKSC                      Input
040900120320     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
041000120320     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
041100120320     c                   call      'TRMZ70SR4'
041200120320     c                   parm                    trmz70s4ds
041300120320     c                   end
041400120320      *
041500120320     c                   clear                   desfrn           35
041600120320     c                   clear                   piva             16
041700120320      *  Descrizione Ragione Sociale FORNITORE
041800120320     c                   if        s4_errore = '0'
041900120320     c                   eval        DESFRN = s4_RAGSOCKSC
042000120320     c                   eval        PIVA   = s4_PARTITAIVA
042100120320     c                   end
042200120320      *
042300120320     C                   ENDSR
042400120320     C/EJECT
042500120320     C*---------------------------------------------------------------*
042600081110     OQSYSPRT   E            TESTA            02
042700120320     O                                           10 'FICNX86R'
042800081110     O                                           25 'Autista'
042900081110     O                                           30 'TSR'
043000081110     O                                           45 'Progr'
043100081110     O                                           55 'Data_Dec'
043200081110     O                                           65 'Data_Sca'
043300081110     O                                           75 'Data_Stp'
043400081110     O                                           85 'Data_Cnv'
043500111201     O                                          100 'N_Rec_ADT'
043600111123     O                                          111 'N O T E'
043700111123     O                       data               132 '  /  /    '
043800111123     O          E            TESTA       1
043900111123     O                       ora                130 '  :  :  '
044000111123      *
044100081110     O          E            DETta       1
044200081110     O                       stpPDR        z     25
044300081110     O                       stpTSR              29
044400081110     O                       stpPRG        z     43
044500081110     O                       stpDDT        z     55
044600081110     O                       stpDST        z     65
044700081110     O                       stpDTS        z     75
044800081110     O                       stpDCN        z     85
044900111201     o                       ADT_REC       z    100
045000111123     O                       errore             116
045100081110      ****
045200081110     o          E            FINEST      2
045300111123     o                                            1 ' '
045400120320     o                                          + 0 '*** FICNX86R ***'
045500111201     o                                          + 9 '***  DUPLI - AFF/DEF ***'
045600111123     o                                          + 9 '***   FINE  STAMPA  ***'
045700111123     O                       data               + 9 '  /  /    '
045800111123     O                       ora                + 3 '  :  :  '
045900111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
