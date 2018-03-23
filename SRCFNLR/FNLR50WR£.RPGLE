000100170203     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('FNLR50R')
000200121011     H*--------------------------------------------------------------*
000300170203      * utility per verificare intanto se l'autista è stato disaccreditato
000400170203      * il LIMITE dell'ACCREDITAMENTO
000500121108      *--------------------------------------------------------------*
000600170203      *  01           Controllo Accreditamento
000700170223      *  02           Tipo Autista SENZA MEZZO (riserva o fattorino)
000800130314      *  07           Usato per la Forzatura
000900130314      *  12           Uscita
001000940224      *  99           INDIC. GENERALE DI ERRORE
001100170203     F*----------------------------------------------------*
001200170301     Ffiapd01L  IF   E           K DISK
001300170206     Ffitgt01L  IF   E           K DISK
001400170221     Ftntbe01L  IF   E           K DISK
001500170206     Faitra03L  IF   E           K DISK
001600170224     FFNLR50WD  CF   E             WORKSTN INFDS(DSFMT)  usropn
001700940128     D*----------------------------------------------------*
001800940211     D* Passaggio Parametri
001900940211     D KPJBA         E DS
002000170224     D*-------------
002100170203     D fnlr50wDS     E DS
002200170224     d va_in_Batch     s              1A   INZ(' ')
002300170224     D*-------------
002400170224     d  BloccaForza    s              1A   INZ(' ')
002500170224     D*-------------
002600170224     d Accreditato     s              1A   INZ(' ')
002700170224     d Senza_Mezzo     s              1A   INZ(' ')
002800170224     D*-------------
002900130315     d AZUTEds       e ds                  extname(AZUTE00F)
003000130315     d DDatiUte      e ds
003100130315     d* Parametri x Controllo profilo utenti
003200130315     d TIBS34ds      e ds
003300940211     D*-------------
003400170221     d dTPA          e ds
003500170221     D*-------------
003600940211     D DSFMT           DS           512
003700940506     D  $TASTO               369    369
003800940211     D  NRG                  370    370
003900940211     D  NCL                  371    371
004000940211     D  SFLNRR               378    379B 0
004100940207     D*-------------
004200940207     D* Nome PGM a video
004300940207     D                 DS
004400940207     D  PROGR                  1     10
004500940207     D  ASTER1                 1      1    INZ('*')
004600940207     D  SIGLA                  2      9
004700940207     D  ASTER2                10     10    INZ('*')
004800940127     D*-------------
004900940127     D* Reperimento nome PGM
005000940127     D STATUS         SDS           333
005100940127     D  DSPGM            *PROC
005200940607     D* descrizione tabella
005300030113     D*-------------
005400121011     D WLBDAT          DS
005500121011     D  G02DAT                 1      8  0
005600121011     D  G02INV                 9     16  0
005700121011     D  G02ERR                17     17
005800121011     D  G02TGI                18     22  0
005900130308     D*-------------
006000121011     d dataiso         s               d   datfmt(*iso)
006100121011     d dataeur         s               d   datfmt(*eur)
006200121011     d udatiso         s               d   Datfmt(*iso)
006300170224     D*-------------
006400940211     D* COSTANTI
006500940211     D*-------------
006600940506     D* Tasti di funzione
006700940506     D F01             C                   CONST(X'31')
006800940506     D F02             C                   CONST(X'32')
006900940506     D F03             C                   CONST(X'33')
007000940506     D F04             C                   CONST(X'34')
007100940506     D F05             C                   CONST(X'35')
007200940506     D F06             C                   CONST(X'36')
007300940506     D F07             C                   CONST(X'37')
007400940506     D F08             C                   CONST(X'38')
007500940506     D F09             C                   CONST(X'39')
007600940506     D F10             C                   CONST(X'3A')
007700940506     D F11             C                   CONST(X'3B')
007800940506     D F12             C                   CONST(X'3C')
007900940506     D F13             C                   CONST(X'B1')
008000940506     D F14             C                   CONST(X'B2')
008100940506     D F15             C                   CONST(X'B3')
008200940506     D F16             C                   CONST(X'B4')
008300940506     D F17             C                   CONST(X'B5')
008400940506     D F18             C                   CONST(X'B6')
008500940506     D F19             C                   CONST(X'B7')
008600940506     D F20             C                   CONST(X'B8')
008700940506     D F21             C                   CONST(X'B9')
008800940506     D F22             C                   CONST(X'BA')
008900940506     D F23             C                   CONST(X'BB')
009000940506     D F24             C                   CONST(X'BC')
009100940506     D ENTER           C                   CONST(X'F1')
009200130314     D RITORNO         C                   CONST('F12=Ritorno')
009300130314     D FORZATURA       C                   CONST('F7=Forzare Avanzamento')
009400170224     D da_Bloccare     C                   CONST('S')
009500170224     D da_Forzare      C                   CONST('F')
009600130314     -*-------------------------------------------------------------------------*
009700130314     -*                MAIN LINE PROGRAM
009800130314     -*-------------------------------------------------------------------------*
009900130314     -* Reperimento parametri
010000121115     C     *ENTRY        PLIST
010100121115     C                   PARM                    KPJBA
010200170203     C                   movel     kpjbu         fnlr50wDS
010300130308     C                   SETON                                            LR
010400170224      *
010500170221     C     Ktbe          KLIST
010600170221     C                   KFLD                    tbecod
010700170221     C                   KFLD                    tbeke1
010800170301      *
010900170301     C     Kapd          KLIST
011000170301     C                   KFLD                    apdtip
011100170301     C                   KFLD                    apdpdr
011200170301     C                   movel     'A'           apdtip
011300170203      *
011400170206     c     KTRA03_1      Klist
011500170203     C                   kfld                    §50AUTISTA
011600170206      *
011700170206     c     KTRA03_2      Klist
011800170206     C                   kfld                    §50AUTISTA
011900170224     C                   kfld                    DATA_FINERAPP     8 0
012000170203      *
012100170203      * inizializzazione variabili
012200130315     C                   EXSR      DatiJOB
012300130315     C                   EXSR      INZVAR
012400170203     C                   EXSR      CONTROLLA
012500170224     C                   EXSR      ERRORI
012600170206      *
012700170224      *  se non deve mostrare  la finestra
012800170224      *     oppure   non ha nulla da mostrare
012900170224      *           deve uscire senza fare nulla
013000170224     c                   IF          va_in_Batch ='S' or  not *in99
013100170206     C                   MOVE      *ON           $FINE
013200170224     C                   Else
013300170224      * altrimenti
013400170224      *   deve aprire la window
013500170224     c                   open      FNLR50WD
013600170224     C                   ENDif
013700170224      *
013800170224      * rimane sulla Window
013900940223     C     $FINE         DOWEQ     *OFF
014000130314     C     $GEST         CASEQ     'W1'          GESW1
014100940117     C                   END
014200940117     C                   END
014300170203      * fine programma
014400170224     c                   close     FNLR50WD                             99
014500170203     c                   eval      kpjbu = fNLR50Wds
014600130315      *---------------------------------------------------------------*
014700130315      *?  Reperimento Dati del job (Utente/Operativi)                ?
014800130315      *---------------------------------------------------------------*
014900130315     c     DatiJOB       BEGSR
015000130315      *
015100130315     c     *dtaara       define    §azute        azuteds
015200130315     c     *dtaara       define    §datiute      ddatiute
015300130315      *
015400130315     c                   in(E)     *dtaara
015500130315     c                   IF        %ERROR or RSUT = *blanks
015600130315     c                   clear                   Tibs34Ds
015700130315     c                   call      'TIBS34R'
015800130315     c                   parm                    Tibs34Ds
015900130315     c                   in        *dtaara
016000130315     c                   ENDIF
016100130315      *
016200130315     c                   ENDSR
016300121011      ************************************************************
016400121011      * OPERAZIONI INIZIALI
016500121011      ************************************************************
016600121011     C     INZVAR        BEGSR
016700170224      *
016800170224     c                   clear                   BloccaForza
016900170224      *
017000170224      * Subito trasforma il flag di richiesta su come deve comportarsi DAL CHIAMANTE
017100170224      *      se deve essere esguito in BATCH --> quindi ritorna solo l'esito
017200170224     c                   IF          §50WINDOW ='N'
017300170224     c                   eval      va_in_Batch ='S'
017400170224      *
017500170224     c                   ELSEIF      §50WINDOW = da_BLOCCARE
017600170224     c                   eval        BloccaForza ='B'
017700170224      *
017800170224     c                   ELSEIF      §50WINDOW = da_FORZARE
017900170224     c                   eval        BloccaForza ='F'
018000170224     c                   END
018100170224      *
018200170224      *  Pulisce indicatori di errori e campi
018300170224     C                   SETOFF                                       990102
018400170224     c                   eval          accreditato = ' '
018500170224     c                   eval          senza_mezzo = ' '
018600130314      ***
018700170224      *** per il controllo sull'accreditamento, la data di fine rapporto deve essere 0
018800170224     C                   z-add     0             DATA_FINERAPP
018900170224      ***
019000130314      * Variabili per gestione videate
019100130314     C                   MOVE      'W1'          $GEST             2
019200130314     C                   MOVE      *OFF          $FINE             1
019300130314     C                   MOVE      *OFF          $ESCI             1
019400130314     C                   MOVE      *OFF          $RCDOK            1
019500170203      * GIRO DATA ODIERNA
019600121011     C                   TIME                    W0140            14 0
019700121011     C                   MOVE      W0140         UDATE8            8 0
019800121011     C                   Z-ADD     UDATE8        G02DAT
019900121011     C                   MOVE      *ZEROS        G02INV
020000121011     C                   MOVE      *BLANKS       G02ERR
020100121011     C                   CALL      'XSRDA8'
020200121011     C                   PARM                    WLBDAT
020300121011     C                   Z-ADD     G02INV        DATEU8            8 0
020400121011     C                   MOVE      g02inv        UDATiso
020500130314      **     pulizia
020600130314      ****   CAMPI di RITORNO dal PROGRAMMA
020700170203     c                   clear                   §50ESITO
020800170206     c                   clear                   §50FUNZ
020900170224      *
021000130314     C                   endsr
021100170203      ************************************************************
021200170224      *    Fa i ragionamenti sull'AITRA e/o la TARIFFA
021300170203      ************************************************************
021400170203     C     CONTROLLA     Begsr
021500170203      *
021600170224      * con data disaccr.= 0
021700170224     c     Ktra03_2      setll     aitra03l
021800170224      **-------------
021900170224     c                   IF          not %Equal
022000170224      * Se Non trovato
022100170224      *  controlla che vi sia almeno un record con il solo cod.autista
022200170224      *   oppure
022300170224      *  potrebbe essere totalmente nuovo, ancora da inserire
022400170224     c                   exsr      solo_con_aut
022500170224      **----
022600170224     c                   ELSE
022700170224      **----
022800170224      * Se invece è EQUAL  la SETLL  -  legge il record
022900170224     c     Ktra03_2      reade     aitra03l
023000170224      * ed è quindi accreditato
023100170224     c                   if        not %EoF(aitra03l)
023200170224     c                   eval          accreditato = 'S'
023300170224      * QUI,
023400170224      * SOLO SUGLI ACCREDITATI, facciamo il CONTROLLO del tipo AUTISTA
023500170224      *  ..per poter dire se BLOCCARE o FORZARE l'inserimento della DISTINTA
023600170224      *     o allegato
023700170224     c                   if            traRIS <> *blank
023800170224     c                   exsr      CHK_Autista
023900170224     c                   end
024000170224      *
024100170224     c                   end
024200170224      **----
024300170224     c                   ENDIF
024400170224      **-------------
024500170224     C                   ENDSR
024600170224      ************************************************************
024700170224      *   Se deve emettere degli ERRORI
024800170224      ************************************************************
024900170224     C     ERRORI        Begsr
025000170224      *
025100170224      *  Se disaccreditato deve segnalare
025200170224     c                   if           accreditato <>'S'
025300170223     C                   SETON                                          9901
025400170223     c                   endIF
025500170223      *
025600170224      *  Se NON può fare una DISTINTA  deve segnalare
025700170224     c                   if           senza_Mezzo = 'S'
025800170224     c                   SETON                                          9902
025900170224     c                   endIF
026000170224      *
026100170224     -*  se era da BLOCCARE  o da Forzare  comunque segnala
026200170224     c   99              if        BloccaForza <> *blank
026300170224     c                   eval        §50ESITO = '1'
026400170224     c                   end
026500170224      *
026600130314     C                   ENDSR
026700170224      ************************************************************
026800170224      *    Controlla solo con il codice autista a RITROSO
026900170224      ************************************************************
027000170224     C     Solo_con_AUT  BEGSR
027100170224      *
027200170223      * va all'indietro con il solo codice AUTISTA cercando
027300170223      *  l'ultimo disaccreditamento
027400170223     c     KTRA03_1      setgt     aitra03l
027500170223     c     KTRA03_1      readpe    aitra03l
027600170223      *
027700170223     c                   if          %Eof(aitra03l)
027800170223      *
027900170223      *   se non era inserito in assoluto, significa che deve ancora lavorare
028000170223      * (quindi lo considero come OK in quanto NON esiste in assoluto
028100170223      *   ...deve essere inserito ancora)
028200170223     c                   eval         accreditato = 'S'
028300170223      *
028400170223     c                   else
028500170223      * invece,
028600170223      *   se c'era un precedente disaccreditamento
028700170223      * allora,
028800170223      *   bisogna  controllare la tariffa
028900170223      *  - e basta ce ne sia una attiva sulla data della distinta
029000170223     c     KTRA03_1      setll     fitgt01l
029100170223     c     KTRA03_1      reade     fitgt01l
029200170223     c                   dow       not %Eof(fitgt01l)
029300170223     c                   if          tgtsml  = *blank and
029400170223     c                               tgtnrc <> tranrc and
029500170223     c                               tgtddt <= §50DATADIS and
029600170223     c                               tgtdst >= §50DATADIS
029700170223      *  esce e NON deve
029800170223      *   far SCATTARE la segnalazione di ERRORE (dando x accreditato)
029900170223     c                   eval         accreditato = 'S'
030000170223     c                   leave
030100170223     c                   end
030200170223      *
030300170223     c     KTRA03_1      reade     fitgt01l
030400170223     c                   enddo
030500170224      *
030600170223     c                   end
030700170223      *
030800170223     C                   ENDSR
030900170223     -************************************************************
031000170223     -* Controlla se il Tipo Autista è particolare
031100170223     -************************************************************
031200170224     C     CHK_Autista   BEGSR
031300170223      *
031400170223      ** e NON è un AUTISTA normale, occorre controllare
031500170223      *      se NON deve fare le distinte oppure se richiede FORZATURA
031600170224      ** decodifica tipologia autista
031700170223     C                   movel     'TPA'         tbecod
031800170223     C                   movel(p)  traRIS        tbeke1
031900170223     C     Ktbe          chain     tntbe01l
032000170223     c                   if        %Found(tntbe01l)
032100170224     c                   eval        senza_mezzo = 'S'
032200170223     c                   movel     tbeUNI        DTPA
032300170224      *
032400170224      *  in base al flag in tabella     il BLOCCO PREVALE
032500170224     c                   if          §tpaDIST = da_bloccare
032600170224     c                   eval         BloccaForza = 'B'
032700170224      *
032800170224      *   se NON era da Bloccare da prima.... ed è da Forzare x la tabella
032900170224     c                   elseif      §tpaDIST = da_forzare
033000170224     c                               and BloccaForza <>'B'
033100170224     c                   eval         BloccaForza = 'F'
033200170224     c                   end
033300170224      *
033400170223     c                   end
033500170223      *
033600170223     C                   ENDSR
033700170223     -************************************************************
033800170223     -* GESTIONE LISTA
033900170223     -************************************************************
034000170223     C     GESW1         BEGSR
034100170223     -*
034200130314     -* inizializzazione videata
034300130314     C                   EXSR      INZW1
034400130314     -*
034500130314     -*              *------------------*
034600130314     C                   EXFMT     W1
034700130314     -*              *------------------*
034800130314     -*
0349009401271    C                   SELECT
035000130314     -* F12=Immissione
035100121011     C     $TASTO        WHENEQ    F12
035200130314     C                   EXSR      F12W1
035300130314     -*
035400130314     -* F07=Forzare e proseguire
035500130314     C     $TASTO        WHENEQ    F07
035600130314     C                   EXSR      F07W1
035700130314     -*
0358009401271O   C                   OTHER
035900130314     -*
0360009401271-   C                   ENDSL
036100130314     -*
036200940127     C                   ENDSR
036300940224     C/EJECT
036400130314     -************************************************************
036500130314     -* INIZIALIZZAZIONE LISTA
036600130314     -************************************************************
036700130314     C     INZW1         BEGSR
036800130314     -*
036900130314     -*   imposta messaggio di errore
037000121109     c                   clear                   p1ERR0
037100121109     c                   clear                   p1ERR1
037200121011     c                   clear                   p1ERR2
037300130314     c                   clear                   w1D001
037400130314     c                   clear                   w1D002
037500130320     c                   clear                   w1VARIA
037600170301      *
037700170301     c                   clear                   decodAUT         25
037800170301     c                   eval      apdpdr = §50AUTISTA
037900170301     C     Kapd          chain     fiapd01l
038000170301     c                   if        %found(fiapd01l)
038100170301     c                   eval        decodAUT = apdrsc
038200170301     c                   end
038300170301      *
038400170206      *  DISACCREDITATO
038500130314     C                   IF        *in01
038600130321     c                   eval      w1D001 = '  Autista ' +
038700170301     c                             %editc(§50AUTISTA:'Z')  + ' ' + decodAUT
038800170301     c                   eval      w1D002 = ' ** NON ACCREDITATO **'
038900170203     c                   eval       w1VARIA = §50MSGvid
039000130315     c                   end
039100170223      *
039200170224      *  NON HA MEZZO E NON DEVE FARE DISTINTE
039300170223     C                   IF        *in02
039400170223     c                   eval      w1D001 = '  Autista ' +
039500170301     c                             %editc(§50AUTISTA:'Z') + ' ' + decodAUT
039600170224     c                   eval      w1D002 = ' ** SENZA MEZZO **'
039700170224     c                   eval      w1D002 = %TrimR(w1D002) +
039800170301     c                             '     - ' + §tpaDES
039900170223     c                   eval       w1VARIA = §50MSGvid
040000170223     c                   end
040100170223     -*
040200130314     -*  tornare con F12
040300130314     c                   move      'F12=Ritorno' p1ERR2
040400170224     c                   setoff                                       07
040500130314     -*
040600170224      *  Forza solo se non vi è almeno un imposizione di BLOCCARE
040700170224     C                   IF        BloccaForza = 'F'
040800130314     c                   seton                                        07
040900130321     c                   movel     Forzatura     p1ERR2
041000121109     c                   end
041100130314     -*
041200940127     C                   ENDSR
041300130314     -************************************************************
041400130314     -* GESTIONE F12 ritorno
041500130314     -************************************************************
041600130314     C     F12W1         BEGSR
041700130314     -*
041800121011     C                   MOVE      *ON           $FINE
041900170206     c                   eval      §50FUNZ  = 'F12'
042000130314     -*
042100940309     C                   ENDSR
042200121108     C/EJECT
042300130314     -************************************************************
042400130314     -* GESTIONE F07 FORZA AVANZAMENTO
042500130314     -************************************************************
042600130314     C     F07W1         BEGSR
042700130314     -*
042800170206     -* se era   forzabile
042900170206     C                   MOVE      *ON           $FINE
043000170206     c                   eval      §50FUNZ  = 'F07'
043100130314     -*
043200121108     C                   ENDSR
043300940127     C/EJECT
043400130314     -************************************************************
