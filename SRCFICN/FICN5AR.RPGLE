000100130308     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('FICN5AR')
000200121011     H*--------------------------------------------------------------*
000300130308      * utility per verificare i termini di scadenza in giorni da tabella 5A
000400130308      * DS5AAUT per l'accreditamento dell'autista e per la ricezione della
000500130308      * copia firmata della tariffa.
000600130314      * il LIMITE dell'ACCREDITAMENTO prevale come info di errore
000700130314      * sulla RICEZIONE TARIFFA FIRMATA.
000800121108      *--------------------------------------------------------------*
000900130314      *  01           Controllo Accreditamento -> oltre i gg. possibili
001000130314      *  02           Controllo Tariffe        -> oltre i gg. possibili
001100130314      *  07           Usato per la Forzatura
001200130314      *  12           Uscita
001300940224      *  99           INDIC. GENERALE DI ERRORE
001400940128     F*----------------------------------------------------*
001500121011     FTABEL00F  IF   E           K DISK
001600130308     FFICN5AD   CF   E             WORKSTN
001700940201     F                                     INFDS(DSFMT)
001800940128     D*----------------------------------------------------*
001900940211     D* Passaggio Parametri
002000940211     D KPJBA         E DS
002100130308     D ficn5Ads      E DS
002200121011     D ds5aAUT       E DS
002300130315     d AZUTEds       e ds                  extname(AZUTE00F)
002400130315     d DDatiUte      e ds
002500130315     d* Parametri x Controllo profilo utenti
002600130315     d TIBS34ds      e ds
002700940211     D*-------------
002800940211     D DSFMT           DS           512
002900940506     D  $TASTO               369    369
003000940211     D  NRG                  370    370
003100940211     D  NCL                  371    371
003200940211     D  SFLNRR               378    379B 0
003300940207     D*-------------
003400940207     D* Nome PGM a video
003500940207     D                 DS
003600940207     D  PROGR                  1     10
003700940207     D  ASTER1                 1      1    INZ('*')
003800940207     D  SIGLA                  2      9
003900940207     D  ASTER2                10     10    INZ('*')
004000940127     D*-------------
004100940127     D* Reperimento nome PGM
004200940127     D STATUS         SDS           333
004300940127     D  DSPGM            *PROC
004400940607     D* descrizione tabella
004500030113     D*-------------
004600121011     D WLBDAT          DS
004700121011     D  G02DAT                 1      8  0
004800121011     D  G02INV                 9     16  0
004900121011     D  G02ERR                17     17
005000121011     D  G02TGI                18     22  0
005100130308     D*-------------
005200121011     d dataiso         s               d   datfmt(*iso)
005300121011     d dataeur         s               d   datfmt(*eur)
005400121011     d udatiso         s               d   Datfmt(*iso)
005500130308     d dt_ISO_accred   s               d   Datfmt(*iso)
005600130308     d dt_ISO_rictar   s               d   Datfmt(*iso)
005700130311     d dt_ISO_data1    s               d   Datfmt(*iso)
005800130311     d dt_ISO_data2    s               d   Datfmt(*iso)
005900130314     D*-------------
006000130308     d DATA_accred     s              8s 0
006100130308     d DATA_rictar     s              8s 0
006200130314     D*-------------
006300130314     d gg_accred       s              5s 0
006400130314     d gg_rictar       s              5s 0
006500130314     d gg_DIFFE        s              5s 0
006600940207     D*-------------
006700940211     D* COSTANTI
006800940211     D*-------------
006900940506     D* Tasti di funzione
007000940506     D F01             C                   CONST(X'31')
007100940506     D F02             C                   CONST(X'32')
007200940506     D F03             C                   CONST(X'33')
007300940506     D F04             C                   CONST(X'34')
007400940506     D F05             C                   CONST(X'35')
007500940506     D F06             C                   CONST(X'36')
007600940506     D F07             C                   CONST(X'37')
007700940506     D F08             C                   CONST(X'38')
007800940506     D F09             C                   CONST(X'39')
007900940506     D F10             C                   CONST(X'3A')
008000940506     D F11             C                   CONST(X'3B')
008100940506     D F12             C                   CONST(X'3C')
008200940506     D F13             C                   CONST(X'B1')
008300940506     D F14             C                   CONST(X'B2')
008400940506     D F15             C                   CONST(X'B3')
008500940506     D F16             C                   CONST(X'B4')
008600940506     D F17             C                   CONST(X'B5')
008700940506     D F18             C                   CONST(X'B6')
008800940506     D F19             C                   CONST(X'B7')
008900940506     D F20             C                   CONST(X'B8')
009000940506     D F21             C                   CONST(X'B9')
009100940506     D F22             C                   CONST(X'BA')
009200940506     D F23             C                   CONST(X'BB')
009300940506     D F24             C                   CONST(X'BC')
009400940506     D ENTER           C                   CONST(X'F1')
009500130314     D RITORNO         C                   CONST('F12=Ritorno')
009600130314     D FORZATURA       C                   CONST('F7=Forzare Avanzamento')
009700130321     D effesette       C                   CONST('  F7 - per Forzare ava-
009800130315     D                                     nzamento             ')
009900130315     D manca_Ricez1    C                   CONST('  Allegato Tariffario n-
010000130315     D                                     on ricevuto in sede.')
010100130314     -*-------------------------------------------------------------------------*
010200130314     -*                MAIN LINE PROGRAM
010300130314     -*-------------------------------------------------------------------------*
010400130314     -* Reperimento parametri
010500121115     C     *ENTRY        PLIST
010600121115     C                   PARM                    KPJBA
010700130308     C                   movel     kpjbu         ficn5ADs
010800130308     C                   SETON                                            LR
010900130314     -*
011000130314     -* inizializzazione variabili
011100130315     C                   EXSR      DatiJOB
011200130315     C                   EXSR      INZVAR
011300130314     C                   EXSR      CONFRONTA
011400130314     -*
011500940223     C     $FINE         DOWEQ     *OFF
011600130314     C     $GEST         CASEQ     'W1'          GESW1
011700940117     C                   END
011800940117     C                   END
011900130314     -* fine programma
012000130308     c                   eval      kpjbu = ficn5Ads
012100130315      *---------------------------------------------------------------*
012200130315      *?  Reperimento Dati del job (Utente/Operativi)                ?
012300130315      *---------------------------------------------------------------*
012400130315     c     DatiJOB       BEGSR
012500130315      *
012600130315     c     *dtaara       define    §azute        azuteds
012700130315     c     *dtaara       define    §datiute      ddatiute
012800130315      *
012900130315     c                   in(E)     *dtaara
013000130315     c                   IF        %ERROR or RSUT = *blanks
013100130315     c                   clear                   Tibs34Ds
013200130315     c                   call      'TIBS34R'
013300130315     c                   parm                    Tibs34Ds
013400130315     c                   in        *dtaara
013500130315     c                   ENDIF
013600130315      *
013700130315     c                   ENDSR
013800121011      ************************************************************
013900121011      * OPERAZIONI INIZIALI
014000121011      ************************************************************
014100121011     C     INZVAR        BEGSR
014200130314      ***
014300130314      * Variabili per gestione videate
014400130314     C                   MOVE      'W1'          $GEST             2
014500130314     C                   MOVE      *OFF          $FINE             1
014600130314     C                   MOVE      *OFF          $ESCI             1
014700130314     C                   MOVE      *OFF          $RCDOK            1
014800130314     -* GIRO DATA ODIERNA
014900121011     C                   TIME                    W0140            14 0
015000121011     C                   MOVE      W0140         UDATE8            8 0
015100121011     C                   Z-ADD     UDATE8        G02DAT
015200121011     C                   MOVE      *ZEROS        G02INV
015300121011     C                   MOVE      *BLANKS       G02ERR
015400121011     C                   CALL      'XSRDA8'
015500121011     C                   PARM                    WLBDAT
015600121011     C                   Z-ADD     G02INV        DATEU8            8 0
015700121011     C                   MOVE      g02inv        UDATiso
015800130311     **
015900130311     **   se la data1 NON vien passata allora si deve prendere l'UDATE
016000130311     c                   if        §5ADATA1 = 0
016100130311     c                   move      UDATiso       dt_ISO_data1
016200130311     c                   else
016300130311     c     *iso          move      §5ADATA1      dt_ISO_data1
016400130311     c                   end
016500130314     -*
016600130311     c     *iso          move      §5ADATA2      dt_ISO_data2
016700130314     -*
016800130314     C                   clear                   ds5Aaut
016900130314     C                   clear                   gg_rictar
017000130314     C                   clear                   gg_accred
017100130314     -***
017200130314     -*** Tabella AUT (5A)
017300130314     -***   DS§5AAUT                    PARAMETRI VARI X AUTROTRASP.
017400130314     -***   §5AGGTARA      1     3 S  0 GG MIN DI VALIDITÀ ALLEGATI TARIFFARI AUTOTRAS
017500130314     -***   §5AGGVALDA     4     3 S  0 GG MAX X ACCREDITARE AUT.
017600130314     -***   §5AGGVALDR     7     3 S  0 GG MAX X RICEVERE COPIA FIRMATA
017700130314     -***   §5ADTAINI     10     8 S  0 INIZIO PERIODO X AUMENTO GG
017800130314     -***   §5ADTAFIN     18     8 S  0 FINE PERIODO X AUMENTO GG
017900130314     -***   §5AGGINPIU    26     3 S  0 GG DI AUMENTO NEL PERIODO
018000130314     -***   §5AGGCPI      29     3 S  0 GG DI AUMENTO X CAMBIO P.IVA
018100130314     -***   §5AGGRIS      32     3 S  0 GG DI AUMENTO X RISTAMPA DOC
018200130314     -***   .......
018300130314     -***   .....
018400130314     -*
018500130314     -* se non viene già passata la DS allora aggancia la tabella per
018600130314     -*  valorizzarne i campi.
018700130314     c                   if            §5AUNIDS = *blank
018800130315     C                   z-add     1             tblkut
018900130315     C                   movel     '5A'          TBLCOD
019000130315     C                   MOVEL(P)  'AUT'         TBLKEY
019100130315     C     KTAB          CHAIN     TABEL00F
019200130314     c                   if        %Found(tabel00f)
019300130314     C                   eval      §5AuniDS = tbluni
019400130314     c                   else
019500130314      * se non trova ERRORE e va a fine
019600130314     c                   eval      §5AESITO = '1'
019700130314     c                   eval      kpjbu = ficn5Ads
019800130314     c                   RETURN
019900130314     c                   end
020000130314     c                   endIF
020100130314      **
020200130314     C                   movel     §5AUNIDS      ds5Aaut
020300130314     C                   move      §5AGGVALDR    gg_rictar
020400130314     C                   move      §5AGGVALDA    gg_accred
020500130314      ****
020600130314      **   TRAZIONI RIDOTTE
020700130314      ** se siamo in periodo di trazioni ridotte occorre
020800130314      **  aggiunge i giorni previsti in tabella
020900130314     c                   if        DATEU8  >= §5ADTAINI and
021000130314     c                             DATEU8  <= §5ADTAFIN
021100130314     c                   eval      gg_rictar = gg_rictar + §5AGGinPIU
021200130314     c                   eval      gg_accred = gg_accred + §5AGGinPIU
021300130314     c                   end
021400130314      *
021500130314      **   RISTAMPA
021600130314      **  aggiunge i giorni previsti in tabella
021700130314     c                   if        §5ARISTAMP ='S'
021800130314     c                   eval      gg_rictar = gg_rictar + §5AGGris
021900130314     c                   eval      gg_accred = gg_accred + §5AGGris
022000130314     c                   end
022100130314      **     pulizia
022200130314      ****   CAMPI di RITORNO dal PROGRAMMA
022300130314     c                   clear                   §5AGGDIFF
022400130315     c                   clear                   §5AtipoGG
022500130315     c                   clear                   §5AESITO
022600130314     -*
022700130314     -* calcola i giorni di differenza fra le 2 date passate
022800130314     -*   e comunque la differenza la imposta nel parametro di ritorno
022900130314     -*    CAMPO DI RITORNO
023000130314     -*                                         ==============
023100130314     c     dt_ISO_data1  subdur    dt_ISO_data2  gg_DIFFE:*D
023200130314     -*                                         ==============
023300130314      *
023400121011      * Klist
023500121011     C     KTAB          KLIST
023600130315     C                   KFLD                    TBLKUT
023700130315     C                   KFLD                    TBLCOD
023800130315     C                   KFLD                    TBLKEY
023900130314     -*
024000130314     C                   endsr
024100130314     -************************************************************
024200130314     -*   Esegue il test con i campi della tabella e i parametri ricevuti
024300130314     -************************************************************
024400130314     C     CONFRONTA     Begsr
024500130314     -*
024600130314     -*  Fra i 2 Controlli:
024700130314     -*  Prevale l'info dei giorni per cui NON è stato accreditato
024800130314     -*
024900130314     -*   Esegue le comparazioni con i GIORNI TABELLATI
025000130314     -*     x VEDERE SE FUORI TEMPO MASSIMO
025100130314     C                   SETOFF                                       990102
025200130314     c                   clear                   gg_in_piu         3 0
025300130314     -*
025400130314     -*  controlla l'accreditamento
025500130314     c                   if        gg_accred  < gg_DIFFE
025600130315     c                   if        §5ATIPOCHK = '§5AGGVALDA' or
025700130314     c                             §5ATIPOCHK = *blank
025800130314     c     gg_DIFFE      sub       gg_accred     gg_in_piu
025900130315     c                   eval      §5ATIPOgg  = '§5AGGVALDA'
026000130314     C                   SETON                                        99  01
026100130314     c                   end
026200130314     c                   end
026300130314     -*
026400130314     -*  controlla la Ricezione Copia Firmata
026500130314     c                   if        gg_rictar  < gg_DIFFE and
026600130314     c                              gg_in_piu = 0
026700130315     c                   if        §5ATIPOCHK = '§5AGGVALDR' or
026800130314     c                             §5ATIPOCHK = *blank
026900130314     c     gg_DIFFE      sub       gg_rictar     gg_in_piu
027000130315     c                   eval      §5ATIPOgg  = '§5AGGVALDR'
027100130314     C                   SETON                                        99  02
027200130314     c                   end
027300130314     c                   end
027400130314     -*
027500130315     -*  imposta i Giorni di Differenza oltre i quali ha superato
027600130314     -*    i giorni delle tabelle
027700130314     c                   z-add     gg_in_piu     §5AGGDIFF
027800130314     -*
027900130314     -*  se non deve mostrare  la finestra oppure non ha nulla da mostrare
028000130314     -* deve uscire senza fare nulla
028100130314     c                   IF          §5AWINDOW ='N'    or
028200130314     c                               gg_in_piu = 0
028300130314     C                   MOVE      *ON           $FINE
028400130314     C                   ENDif
028500130314     -*
028600130314     C                   ENDSR
028700130314     -************************************************************
028800130314     -* GESTIONE LISTA
028900130314     -************************************************************
029000130314     C     GESW1         BEGSR
029100130314     -*
029200130314     -* inizializzazione videata
029300130314     C                   EXSR      INZW1
029400130314     -*
029500130314     -*              *------------------*
029600130314     C                   EXFMT     W1
029700130314     -*              *------------------*
029800130314     -*
0299009401271    C                   SELECT
030000130314     -* F12=Immissione
030100121011     C     $TASTO        WHENEQ    F12
030200130314     C                   EXSR      F12W1
030300130314     -*
030400130314     -* F07=Forzare e proseguire
030500130314     C     $TASTO        WHENEQ    F07
030600130314     C                   EXSR      F07W1
030700130314     -*
0308009401271O   C                   OTHER
030900130314     -*
031000130314     -*
0311009401271-   C                   ENDSL
031200130314     -*
031300130314     -*
031400940127     C                   ENDSR
031500940224     C/EJECT
031600130314     -************************************************************
031700130314     -* INIZIALIZZAZIONE LISTA
031800130314     -************************************************************
031900130314     C     INZW1         BEGSR
032000130314     -*
032100130314     -*   imposta messaggio di errore
032200121109     c                   clear                   p1ERR0
032300121109     c                   clear                   p1ERR1
032400121011     c                   clear                   p1ERR2
032500130314     c                   clear                   w1D001
032600130314     c                   clear                   w1D002
032700130320     c                   clear                   w1VARIA
032800130321     -*
032900130314     -*
033000130314     -*  Se superati i giorni di Limite ACCREDITAMENTO
033100130314     C                   IF        *in01
033200130321     c                   eval      w1D001 = '  Autista ' +
033300130321     c                              %editc(§5aAUTISTA:'Z') + ' Progr.Tar.:' +
033400130321     c                              %editc(§5aPRGTAR:'Z') + ' NON ACCREDITATO  '
033500130321     c                   eval       w1VARIA = §5aMSGvid
033600130315     c                   if           §5AWINDOW ='F'
033700130321     c                   eval      w1D002 = effesette
033800130314     c                   end
033900130315     c                   end
034000130314     -*
034100130314     -*  Se superati i giorni di Limite RICEZIONE TARIFFA
034200130315     C                   IF        *in02 and not *in01
034300130321     c                   eval      w1D001 =  ' Allegato del ' +
034400130321     c                              %editc(§5aAUTISTA:'Z') + ' Prg.' +
034500130321     c                              %editc(§5aPRGTAR:'Z') +
034600130321     c                              ' NON ricevuto in Sede '
034700130321     c                   eval       w1VARIA = §5aMSGvid
034800130315     c                   if           §5AWINDOW ='F'
034900130321     c                   eval      w1D002 = effesette
035000130314     c                   end
035100130315     c                   end
035200130314     -*
035300130314     c                   setoff                                       07
035400130314     -*  tornare con F12
035500130314     c                   move      'F12=Ritorno' p1ERR2
035600130314     -*
035700130314     -*  Deve attivare il comando di Forzatura F7 = Forzabile
035800130314     C                   IF        §5AWINDOW =  'F'
035900130314     c                   seton                                        07
036000130321     c                   movel     Forzatura     p1ERR2
036100121109     c                   end
036200130314     -*
036300940127     C                   ENDSR
036400130314     -************************************************************
036500130314     -* GESTIONE F12 ritorno
036600130314     -************************************************************
036700130314     C     F12W1         BEGSR
036800130314     -*
036900121011     C                   MOVE      *ON           $FINE
037000130314     c                   eval      §5AESITO = ' '
037100130314     -*
037200130314     -*  se era BLOCCATO
037300130321     C                   IF        §5AWINDOW =  'S'  or
037400130321     C                             §5AWINDOW =  'F'
037500130314     c                   eval      §5AESITO = '1'
037600121011     c                   end
037700130314     -*
037800940309     C                   ENDSR
037900121108     C/EJECT
038000130314     -************************************************************
038100130314     -* GESTIONE F07 FORZA AVANZAMENTO
038200130314     -************************************************************
038300130314     C     F07W1         BEGSR
038400130314     -*
038500130314     -* se era   forzabile
038600121108     C                   MOVE      *ON           $FINE
038700130314     c                   eval      §5AESITO = ' '
038800130314     -*
038900121108     C                   ENDSR
039000940127     C/EJECT
039100130314     -************************************************************
