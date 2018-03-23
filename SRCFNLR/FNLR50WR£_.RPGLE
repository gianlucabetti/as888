000100170203     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('FNLR50R')
000200121011     H*--------------------------------------------------------------*
000300170203      * utility per verificare intanto se l'autista è stato disaccreditato
000400170203      * il LIMITE dell'ACCREDITAMENTO
000500121108      *--------------------------------------------------------------*
000600170203      *  01           Controllo Accreditamento
000700130314      *  07           Usato per la Forzatura
000800130314      *  12           Uscita
000900940224      *  99           INDIC. GENERALE DI ERRORE
001000170203     F*----------------------------------------------------*
001100170206     Ffitgt01L  IF   E           K DISK
001200170206     Faitra03L  IF   E           K DISK
001300170203     FFNLR50WD  CF   E             WORKSTN
001400940201     F                                     INFDS(DSFMT)
001500940128     D*----------------------------------------------------*
001600940211     D* Passaggio Parametri
001700940211     D KPJBA         E DS
001800170203     D fnlr50wDS     E DS
001900130315     d AZUTEds       e ds                  extname(AZUTE00F)
002000130315     d DDatiUte      e ds
002100130315     d* Parametri x Controllo profilo utenti
002200130315     d TIBS34ds      e ds
002300940211     D*-------------
002400940211     D DSFMT           DS           512
002500940506     D  $TASTO               369    369
002600940211     D  NRG                  370    370
002700940211     D  NCL                  371    371
002800940211     D  SFLNRR               378    379B 0
002900940207     D*-------------
003000940207     D* Nome PGM a video
003100940207     D                 DS
003200940207     D  PROGR                  1     10
003300940207     D  ASTER1                 1      1    INZ('*')
003400940207     D  SIGLA                  2      9
003500940207     D  ASTER2                10     10    INZ('*')
003600940127     D*-------------
003700940127     D* Reperimento nome PGM
003800940127     D STATUS         SDS           333
003900940127     D  DSPGM            *PROC
004000940607     D* descrizione tabella
004100030113     D*-------------
004200121011     D WLBDAT          DS
004300121011     D  G02DAT                 1      8  0
004400121011     D  G02INV                 9     16  0
004500121011     D  G02ERR                17     17
004600121011     D  G02TGI                18     22  0
004700130308     D*-------------
004800121011     d dataiso         s               d   datfmt(*iso)
004900121011     d dataeur         s               d   datfmt(*eur)
005000121011     d udatiso         s               d   Datfmt(*iso)
005100130314     D*-------------
005200170203     d accreditato     s              1A   INZ(' ')
005300940207     D*-------------
005400940211     D* COSTANTI
005500940211     D*-------------
005600940506     D* Tasti di funzione
005700940506     D F01             C                   CONST(X'31')
005800940506     D F02             C                   CONST(X'32')
005900940506     D F03             C                   CONST(X'33')
006000940506     D F04             C                   CONST(X'34')
006100940506     D F05             C                   CONST(X'35')
006200940506     D F06             C                   CONST(X'36')
006300940506     D F07             C                   CONST(X'37')
006400940506     D F08             C                   CONST(X'38')
006500940506     D F09             C                   CONST(X'39')
006600940506     D F10             C                   CONST(X'3A')
006700940506     D F11             C                   CONST(X'3B')
006800940506     D F12             C                   CONST(X'3C')
006900940506     D F13             C                   CONST(X'B1')
007000940506     D F14             C                   CONST(X'B2')
007100940506     D F15             C                   CONST(X'B3')
007200940506     D F16             C                   CONST(X'B4')
007300940506     D F17             C                   CONST(X'B5')
007400940506     D F18             C                   CONST(X'B6')
007500940506     D F19             C                   CONST(X'B7')
007600940506     D F20             C                   CONST(X'B8')
007700940506     D F21             C                   CONST(X'B9')
007800940506     D F22             C                   CONST(X'BA')
007900940506     D F23             C                   CONST(X'BB')
008000940506     D F24             C                   CONST(X'BC')
008100940506     D ENTER           C                   CONST(X'F1')
008200130314     D RITORNO         C                   CONST('F12=Ritorno')
008300130314     D FORZATURA       C                   CONST('F7=Forzare Avanzamento')
008400130321     D effesette       C                   CONST('  F7 - per Forzare ava-
008500130315     D                                     nzamento             ')
008600130314     -*-------------------------------------------------------------------------*
008700130314     -*                MAIN LINE PROGRAM
008800130314     -*-------------------------------------------------------------------------*
008900130314     -* Reperimento parametri
009000121115     C     *ENTRY        PLIST
009100121115     C                   PARM                    KPJBA
009200170203     C                   movel     kpjbu         fnlr50wDS
009300130308     C                   SETON                                            LR
009400170203      *
009500170206     c     KTRA03_1      Klist
009600170203     C                   kfld                    §50AUTISTA
009700170206      *
009800170206     c     KTRA03_2      Klist
009900170206     C                   kfld                    §50AUTISTA
010000170203     C                   kfld                    DATA_a0           8 0
010100170203     C                   z-add     0             DATA_a0
010200170203      *
010300170203      * inizializzazione variabili
010400130315     C                   EXSR      DatiJOB
010500130315     C                   EXSR      INZVAR
010600170203     C                   EXSR      CONTROLLA
010700170206      *
010800170206      *  se non deve mostrare  la finestra oppure non ha nulla da mostrare
010900170206      * deve uscire senza fare nulla
011000170206     c                   IF          §50WINDOW ='N' or  not *in99
011100170206     C                   MOVE      *ON           $FINE
011200170206     C                   ENDif
011300170203      *
011400940223     C     $FINE         DOWEQ     *OFF
011500130314     C     $GEST         CASEQ     'W1'          GESW1
011600940117     C                   END
011700940117     C                   END
011800170203      * fine programma
011900170203     c                   eval      kpjbu = fNLR50Wds
012000130315      *---------------------------------------------------------------*
012100130315      *?  Reperimento Dati del job (Utente/Operativi)                ?
012200130315      *---------------------------------------------------------------*
012300130315     c     DatiJOB       BEGSR
012400130315      *
012500130315     c     *dtaara       define    §azute        azuteds
012600130315     c     *dtaara       define    §datiute      ddatiute
012700130315      *
012800130315     c                   in(E)     *dtaara
012900130315     c                   IF        %ERROR or RSUT = *blanks
013000130315     c                   clear                   Tibs34Ds
013100130315     c                   call      'TIBS34R'
013200130315     c                   parm                    Tibs34Ds
013300130315     c                   in        *dtaara
013400130315     c                   ENDIF
013500130315      *
013600130315     c                   ENDSR
013700121011      ************************************************************
013800121011      * OPERAZIONI INIZIALI
013900121011      ************************************************************
014000121011     C     INZVAR        BEGSR
014100130314      ***
014200130314      * Variabili per gestione videate
014300130314     C                   MOVE      'W1'          $GEST             2
014400130314     C                   MOVE      *OFF          $FINE             1
014500130314     C                   MOVE      *OFF          $ESCI             1
014600130314     C                   MOVE      *OFF          $RCDOK            1
014700170203      * GIRO DATA ODIERNA
014800121011     C                   TIME                    W0140            14 0
014900121011     C                   MOVE      W0140         UDATE8            8 0
015000121011     C                   Z-ADD     UDATE8        G02DAT
015100121011     C                   MOVE      *ZEROS        G02INV
015200121011     C                   MOVE      *BLANKS       G02ERR
015300121011     C                   CALL      'XSRDA8'
015400121011     C                   PARM                    WLBDAT
015500121011     C                   Z-ADD     G02INV        DATEU8            8 0
015600121011     C                   MOVE      g02inv        UDATiso
015700130314      **     pulizia
015800130314      ****   CAMPI di RITORNO dal PROGRAMMA
015900170203     c                   clear                   §50ESITO
016000170206     c                   clear                   §50FUNZ
016100130314      *
016200130314     C                   endsr
016300170203      ************************************************************
016400170203      *   Esegue il test con i campi della tabella e i parametri ricevuti
016500170203      ************************************************************
016600170203     C     CONTROLLA     Begsr
016700170203      *
016800170206     C                   SETOFF                                       99  01
016900170206     c                   eval      accreditato = 'S'
017000170203      *
017100170206      * con data disaccr.= 0
017200170206     c     Ktra03_2      setll     aitra03l
017300170206     c                   if          not %Equal
017400170206      * Se Non trovato
017500170206      *  controlla che vi sia almeno un record per l'autista solo con il cod.aut.
017600170208     c     KTRA03_1      setgt     aitra03l
017700170208     c     KTRA03_1      readpe    aitra03l
017800170206      *
017900170206      *   se non era inserito in assoluto, significa che deve ancora lavorare
018000170206      * (quindi lo considero come OK in quanto NON esiste in assoluto
018100170206      *   ...deve essere inserito ancora)
018200170206      *
018300170206      *   solo se trovato
018400170208     c                   if        not %Eof(aitra03l)
018500170206      * allora
018600170206      *     controlla la tariffa - basta ce ne sia una attiva sulla data della distinta
018700170206     c     KTRA03_1      setll     fitgt01l
018800170206     c     KTRA03_1      reade     fitgt01l
018900170206     c                   dow       not %Eof(fitgt01l)
019000170208     c                   if        tgtsml  = *blank and
019100170208     c                             tgtnrc <> tranrc and
019200170208     c                             tgtddt <= §50DATADIS and
019300170206     c                             tgtdst >= §50DATADIS
019400170206      *  esce SENZA far SCATTARE la segnalazione di ERRORE
019500170206     c                   leavesr
019600170208     c                   end
019700170206     c     KTRA03_1      reade     fitgt01l
019800170206     c                   enddo
019900170206      *
020000170206      *  Se passa da qui significa che deve bloccare con errore
020100170206     C                   SETON                                        99  01
020200170206     c                   eval      accreditato = 'N'
020300170206     -*
020400170206     -*  se era da BLOCCARE
020500170206     C                   IF        §50WINDOW =  'S'  or
020600170206     C                             §50WINDOW =  'F'
020700170206     c                   eval      §50ESITO = '1'
020800170206     c                   end
020900170206      *
021000170203     c                   end
021100170206     c                   end
021200170203      *
021300130314     C                   ENDSR
021400130314     -************************************************************
021500130314     -* GESTIONE LISTA
021600130314     -************************************************************
021700130314     C     GESW1         BEGSR
021800130314     -*
021900130314     -* inizializzazione videata
022000130314     C                   EXSR      INZW1
022100130314     -*
022200130314     -*              *------------------*
022300130314     C                   EXFMT     W1
022400130314     -*              *------------------*
022500130314     -*
0226009401271    C                   SELECT
022700130314     -* F12=Immissione
022800121011     C     $TASTO        WHENEQ    F12
022900130314     C                   EXSR      F12W1
023000130314     -*
023100130314     -* F07=Forzare e proseguire
023200130314     C     $TASTO        WHENEQ    F07
023300130314     C                   EXSR      F07W1
023400130314     -*
0235009401271O   C                   OTHER
023600130314     -*
0237009401271-   C                   ENDSL
023800130314     -*
023900940127     C                   ENDSR
024000940224     C/EJECT
024100130314     -************************************************************
024200130314     -* INIZIALIZZAZIONE LISTA
024300130314     -************************************************************
024400130314     C     INZW1         BEGSR
024500130314     -*
024600130314     -*   imposta messaggio di errore
024700121109     c                   clear                   p1ERR0
024800121109     c                   clear                   p1ERR1
024900121011     c                   clear                   p1ERR2
025000130314     c                   clear                   w1D001
025100130314     c                   clear                   w1D002
025200130320     c                   clear                   w1VARIA
025300170203      *
025400170206      *  DISACCREDITATO
025500130314     C                   IF        *in01
025600130321     c                   eval      w1D001 = '  Autista ' +
025700170203     c                             %editc(§50AUTISTA:'Z') + ' NON ACCREDITATO '
025800170203     c                   eval       w1VARIA = §50MSGvid
025900170203     c                   if           §50WINDOW ='F'
026000130321     c                   eval      w1D002 = effesette
026100130314     c                   end
026200130315     c                   end
026300130314     -*
026400130314     c                   setoff                                       07
026500130314     -*  tornare con F12
026600130314     c                   move      'F12=Ritorno' p1ERR2
026700130314     -*
026800130314     -*  Deve attivare il comando di Forzatura F7 = Forzabile
026900170203     C                   IF        §50WINDOW =  'F'
027000130314     c                   seton                                        07
027100130321     c                   movel     Forzatura     p1ERR2
027200121109     c                   end
027300130314     -*
027400940127     C                   ENDSR
027500130314     -************************************************************
027600130314     -* GESTIONE F12 ritorno
027700130314     -************************************************************
027800130314     C     F12W1         BEGSR
027900130314     -*
028000121011     C                   MOVE      *ON           $FINE
028100170206     c                   eval      §50FUNZ  = 'F12'
028200130314     -*
028300940309     C                   ENDSR
028400121108     C/EJECT
028500130314     -************************************************************
028600130314     -* GESTIONE F07 FORZA AVANZAMENTO
028700130314     -************************************************************
028800130314     C     F07W1         BEGSR
028900130314     -*
029000170206     -* se era   forzabile
029100170206     C                   MOVE      *ON           $FINE
029200170206     c                   eval      §50FUNZ  = 'F07'
029300130314     -*
029400121108     C                   ENDSR
029500940127     C/EJECT
029600130314     -************************************************************
