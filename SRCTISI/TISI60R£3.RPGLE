000100000000     H*------------------------------------------------------------------------*
000200980220     H* STAMPA DELIVERY TIME - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800970623     FTISI60D   CF   E             WORKSTN INFDS(DEVDS1)
000900970623     F                                     SFILE(SI60DVM:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200061120     fAZLIN03L  if   e           k disk
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI, COMANDI E FUNZIONI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002200000000     D*-------------------
002300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002400000000     D*-------------------
002500000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002600000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002700000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002800000000     D*-------------------
002900000000     D* TABELLA DESCRIZIONI A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003200000000     D*-------------------
003300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003400000000     D*-------------------
003500000000     D KEYS            S              1    DIM(40)
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900000000     D*---------------------
004000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004100000000     D*---------------------
004200000000     D DSFUN         E DS
004300000000     D  FUNRIG                 1     40
004400000000     D*---------------------
004500000000     D* DS PER GESTIONE TASTI FUNZIONALI
004600000000     D*---------------------
004700000000     D DSKEYS        E DS
004800000000     D  KEYRIG                 1     40
004900000000     D*---------------------
005000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100000000     D*---------------------
005200000000     D                SDS
005300000000     D  USER                 254    263
005400000000     D*---------------------
005500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600000000     D*---------------------
005700000000     D DEVDS1          DS
005800000000     D  KEY                  369    369
005900000000     D  CURSOR               370    371
006000980210     D*---------------------
006100980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200980210     D*---------------------
006300980210     D WLBDA8          DS                  INZ
006400980210     D  G08DAT                 1      8  0
006500980210     D  G08INV                 9     16  0
006600980210     D  G08ERR                17     17
006700980210     D  G08TGI                18     22  0
006800000000     D*---------------------
006900000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007000000000     D*---------------------
007100000000     D DSMSG         E DS
007200000000     D*---------------------
007300020919     D* DS REPERIMENTO DATI UTENTE
007400000000     D*---------------------
007500020919     D TIBS34DS      E DS                                                       *Profili utente
007600020919     D DDATIUTE      E DS                                                       *Dati utente
007700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007800020924     D TRUL06DS      E DS                                                       *caricamento £x
007900020924     D  LIA                    1     90  0 DIM(30)                               -filiali
008000020924     D TRUL26DS      E DS                                                       *caricamento P.O.
008100020924     D  SKPO                  11    760  0 dim(250)
008200020924     D  SKPOT                761   1510  0 dim(250)
008300020924     D*-------------------
008400020924     D* FILIALI IN GESTIONE
008500020924     D*-------------------
008600020924     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
008700000000     D*---------------------
008800000000     D* DS PARAMETRI DI LANCIO
008900000000     D*------------------
009000000000     D PARAM           DS                  INZ
009100000000     D  PARLNP                 1      3
009200000000     D  PARTEM                 4      4
009300000000     D  PARTSP                 5      5
009400000000     D  PARISO                 6      6
009500980210     D  PARDIS                 7      7
009600980210     D  PARSSA                 8      8
009700980210     D  PARND1                 9     83
009800980210     D  PARND2                84    158
009900980210     D  PARND3               159    233
010000061117     d  PARlin                        1
010100980210     D*---------------------
010200980210     D* ARCHITETTURA
010300980210     D*---------------------
010400980210     D KPJBA         E DS
010500061120      *---------------------
010600061120      * TABELLE
010700061120      *---------------------
010800061120     d ds1H          e ds
010900061120      *---------------------
011000061120      * VARIABILI
011100061120      *---------------------
011200061120     d P37iso2         s                   like(LINiso2)   inz
011300061120     d P37desi         s                   like(LINdesita) inz
011400000000     C*------------------------------------------------------------------------*
011500000000     C* MAIN LINE
011600000000     C*------------------------------------------------------------------------*
011700000000     C*
011800000000     C* CICLO FINO A CHE:
011900000000DO  1C     $FINE         DOUEQ     '1'
012000000000     C*
012100000000     C* CONTROLLA GESTIONE VIDEO
012200980210     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012300000000IF  2C     $GES          IFEQ      'V1'
012400000000     C                   EXSR      GESV1                                        *PARAMETRI
012500000000E   2C                   ENDIF
012600000000E   1C                   ENDDO
012700980210     C*
012800000000     C                   SETON                                        LR
012900000000     C*------------------------------------------------------------------------*
013000000000     C* GESV1 - GESTIONE VIDEO UNO
013100000000     C*------------------------------------------------------------------------*
013200000000     C     GESV1         BEGSR
013300000000     C*
013400000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013500000000     C                   EXSR      CARV1                                        *CARICA VIDEO
013600000000E   1C                   ENDIF
013700000000     C*
013800000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
013900000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014000000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014100000000E   1C                   ENDIF
014200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014300000000     C*
014400970623     C                   EXFMT     SI60DV1                                      *PARAMETRI
014500000000     C*
014600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
014700000000     C*
014800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014900000000SL  2C                   SELECT
015000000000     C     KEY           WHENEQ    ENTER
015100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015200000000     C     KEY           WHENEQ    F6
015300000000     C                   EXSR      F06GES                                       *CONFERMA
015400000000     C     KEY           WHENEQ    F22
015500000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
015600000000     C                   OTHER
015700000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015800000000E   2C                   ENDSL
015900000000E   1C                   ENDIF
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* CARV1 - CARICA VIDEO UNO
016400000000     C*------------------------------------------------------------------------*
016500000000     C     CARV1         BEGSR
016600000000     C*
016700000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016800000000     C*
016900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017100000000     C*
017200970623     C                   WRITE     SI60DVT                                      *TESTATA
017300970623     C                   WRITE     SI60DVF                                      *FUNZIONI
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017800000000     C*------------------------------------------------------------------------*
017900000000     C     ENTV1         BEGSR
018000000000     C*
018100000000     C* CONTROLLA I CAMPI IMMESSI
018200000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018700000000     C*------------------------------------------------------------------------*
018800000000     C     F06GES        BEGSR
018900000000     C*
019000000000     C* CONTROLLA I CAMPI IMMESSI
019100000000     C                   EXSR      DECODI
019200000000     C*
019300000000     C* LANCIA IL PROGRAMMA DI ESECUZIONE
019400000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019500000000     C                   MOVEL     V1CLNP        PARLNP                         *LINEA DI PARTENZA
019600000000     C                   MOVEL     V1CTEM        PARTEM                         *TEMPI DELIVERY
019700000000     C                   MOVEL     V1CTSP        PARTSP                         *TIPO SERVIZIO
019800980210     C                   MOVEL     V1CISO        PARISO                         *CONSIDERA ISOLE
019900980210     C                   MOVEL     V1CDIS        PARDIS                         *CONSIDERA L.DISAGG.
020000980210     C                   MOVEL     V1CSSA        PARSSA                         *SINTETICA/DETTAGLIO
020100000000     C                   MOVEL     V1CND1        PARND1                         *NOTA DELIVERY N°1
020200000000     C                   MOVEL     V1CND2        PARND2                         *NOTA DELIVERY N°2
020300000000     C                   MOVEL     V1CND3        PARND3                         *NOTA DELIVERY N°3
020400061120     c                   move      LINtabel      PARlin                         *NOTA DELIVERY N°3
020500000000     C*
020600970708     C                   MOVEL     'SI62'        KCOAZ
020700000000     C                   MOVEL     PARAM         KPJBU
020800061117     c                   if        KNMUS = *all'1'
020900061117     c                   call      'TISI62R'
021000061117     c                   parm                    KPJBA
021100061117     c                   else
021200061117     C                   CALL      'BCH10'
021300000000     C                   PARM                    KPJBA
021400061117     c                   endif
021500000000     C*
021600000000     C                   MOVEL     MSG(15)       $MSG
021700000000     C                   EXSR      CARMSG
021800000000E   1C                   ENDIF
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
022300000000     C*------------------------------------------------------------------------*
022400000000     C     F22GES        BEGSR
022500000000     C*
022600000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022700000000     C                   Z-ADD     80            LENGH            15 5
022800000000     C                   CALL      'QCAEXEC'
022900000000     C                   PARM                    CMD(1)
023000000000     C                   PARM                    LENGH
023100000000     C*
023200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023300000000     C*
023400000000     C                   ENDSR
023500000000     C*------------------------------------------------------------------------*
023600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023700000000     C*------------------------------------------------------------------------*
023800000000     C     DECODI        BEGSR
023900000000     C*
024000000000     C* REIMPOSTA INDICATORI DI ERRORE
024100000000     C                   SETOFF                                       606162
024200980210     C                   SETOFF                                       636465
024300061120     c                   setoff                                       66
024400000000     C*--------------------
024500000000     C* LINEA DI PARTENZA
024600000000     C*--------------------
024700000000IF  1C     $ERR          IFEQ      '0'
024800000000     C                   MOVEL     V1CLNP        WFIL
024900980210     C                   MOVEL     *BLANKS       WDFIL
025000980210     C                   EXSR      CHKFIL
025100000000     C                   MOVEL     WFIL          V1CLNP                         *CODICE
025200000000     C                   MOVEL     WDFIL         V1DLNP                         *DESCRIZIONE
025300000000IF  2C     $ERR          IFEQ      '1'
025400000000     C                   SETON                                        60
025500000000     C                   MOVEL     MSG(7)        $MSG
025600000000     C                   EXSR      CARMSG
025700000000E   2C                   ENDIF
025800000000E   1C                   ENDIF
025900000000     C*--------------------
026000000000     C* TEMPO DELIVERY
026100000000     C*--------------------
026200000000IF  1C     $ERR          IFEQ      '0'
026300000000IF  2C     V1CTEM        IFEQ      'I'
026400000000     C     V1CTEM        OREQ      'C'
026500000000X   2C                   ELSE
026600000000     C                   SETON                                        61
026700000000     C                   MOVEL     '1'           $ERR
026800980210     C                   MOVEL     MSG(11)       $MSG
026900000000     C                   EXSR      CARMSG
027000000000E   2C                   ENDIF
027100000000E   1C                   ENDIF
027200000000     C*--------------------
027300000000     C* TIPO SERVIZIO
027400000000     C*--------------------
027500000000IF  1C     $ERR          IFEQ      '0'
027600000000     C                   MOVEL     V1CTSP        WTSP
027700980210     C                   MOVEL     *BLANKS       WDTSP
027800000000     C                   EXSR      CHKTSP
027900000000     C                   MOVEL     WTSP          V1CTSP                         *CODICE
028000000000     C                   MOVEL     WDTSP         V1DTSP                         *DESCRIZIONE
028100000000IF  2C     $ERR          IFEQ      '1'
028200000000     C                   SETON                                        62
028300980210     C                   MOVEL     MSG(8)        $MSG
028400000000     C                   EXSR      CARMSG
028500000000E   2C                   ENDIF
028600000000E   1C                   ENDIF
028700061120      *--------------------
028800061120      * LINGUA
028900061120      *--------------------
029000061120if  1c                   if        $Err   = *off
029100061120     c                   clear                   V1Dlin
029200061120      * Ricerca
029300061120if  2c                   if        %scan('?':V1Clin) > *zeros
029400061120     c                   clear                   P37iso2
029500061120     c                   clear                   P37desi
029600061120     c                   call      'TRUL37R'
029700061120     c                   parm                    P37iso2
029800061120     c                   parm                    P37desi
029900061120     c                   eval      V1Clin = P37iso2
030000061120     c                   eval      V1Dlin = P37desi
030100061120     c                   leavesr
030200061120e   2c                   endif
030300061120      * Controllo:
030400061120      * - lingua obbligatoria
030500061120if  2c                   if        V1Clin = *blanks
030600061120     c                   eval      $Err   = *on
030700061120     c                   eval      *in66  = *on
030800061120     c                   movel     Msg(10)       $Msg
030900061120     c                   exsr      CarMSG
031000061120     c                   leavesr
031100061120e   2c                   endif
031200061120      * - lingua errata
031300061120     c     V1Clin        chain     AZLIN000
031400061120if  2c                   if        not %found(AZLIN03L)
031500061120     c                   eval      $Err   = *on
031600061120     c                   eval      *in66  = *on
031700061120     c                   movel     Msg(10)       $Msg
031800061120     c                   exsr      CarMSG
031900061120     c                   leavesr
032000061120e   2c                   endif
032100061120     c                   eval      V1Dlin = LINdesita
032200061120      * - se non è attiva per i testi errore
032300061120if  2c                   if        LINfntxt = '0'
032400061120     c                   eval      $Err   = *on
032500061120     c                   eval      *in66  = *on
032600061120     c                   movel     Msg(10)       $Msg
032700061120     c                   exsr      CarMSG
032800061120     c                   leavesr
032900061120e   2c                   endif
033000061120      * - se ho scelto di stampare in lingua il testo deve essere
033100061120      *   gestito in lingua
033200061120if  2c                   if            V1Clin <> 'it'
033300061120     c                             and §1Hlin = *blanks
033400061120     c                   eval      $Err   = *on
033500061120     c                   eval      *in66  = *on
033600061120     c                   movel     Msg(10)       $Msg
033700061120     c                   exsr      CarMSG
033800061120     c                   leavesr
033900061120e   2c                   endif
034000061120e   1c                   ENDIF
034100000000     C*--------------------
034200980216     C* CAP/LOCALITA' ISOLA
034300000000     C*--------------------
034400000000IF  1C     $ERR          IFEQ      '0'
034500000000IF  2C     V1CISO        IFEQ      'S'
034600000000     C     V1CISO        OREQ      'N'
034700000000X   2C                   ELSE
034800000000     C                   SETON                                        63
034900000000     C                   MOVEL     '1'           $ERR
035000000000     C                   MOVEL     MSG(11)       $MSG
035100000000     C                   EXSR      CARMSG
035200000000E   2C                   ENDIF
035300000000E   1C                   ENDIF
035400980210     C*--------------------
035500980216     C* CAP/LOCALITA' DISAGIATE
035600980210     C*--------------------
035700980210IF  1C     $ERR          IFEQ      '0'
035800980210IF  2C     V1CDIS        IFEQ      'S'
035900980210     C     V1CDIS        OREQ      'N'
036000980210X   2C                   ELSE
036100980210     C                   SETON                                        64
036200980210     C                   MOVEL     '1'           $ERR
036300980210     C                   MOVEL     MSG(11)       $MSG
036400980210     C                   EXSR      CARMSG
036500980210E   2C                   ENDIF
036600980210E   1C                   ENDIF
036700980210     C*--------------------
036800980210     C* STAMPA SINTETICA / ANALITICA
036900980210     C*--------------------
037000980210IF  1C     $ERR          IFEQ      '0'
037100980210IF  2C     V1CSSA        IFEQ      'S'
037200980210     C     V1CSSA        OREQ      'A'
037300980210X   2C                   ELSE
037400980210     C                   SETON                                        65
037500980210     C                   MOVEL     '1'           $ERR
037600980210     C                   MOVEL     MSG(11)       $MSG
037700980210     C                   EXSR      CARMSG
037800980210E   2C                   ENDIF
037900980210E   1C                   ENDIF
038000050912     C*--------------------
038100050912     C* CAP/LOCALITA' ISOLA & DISAGIATE
038200050912     C*--------------------
038300050912if  1C                   if             $Err   = *off
038400050912     C                             and  V1Cssa = 'S'
038500050912     C                             and (V1Ciso = 'S'
038600050912     C                              or  V1Cdis = 'S')
038700050912sel 2c                   select
038800050912w   2C                   when      V1Ciso = 'S'
038900050912     C                   eval      *in63 = *on
039000050912w   2C                   when      V1Cdis = 'S'
039100050912     C                   eval      *in64 = *on
039200050912e   2c                   endsl
039300050912     C                   eval      $Err  = *on
039400050912     C                   movel     Msg(9)        $Msg
039500050912     C                   exsr      CarMSG
039600050912E   1C                   endif
039700000000     C*
039800000000     C                   ENDSR
039900000000     C*------------------------------------------------------------------------*
040000980210     C* CHKFIL - CONTROLLA LA FILIALE
040100000000     C*------------------------------------------------------------------------*
040200980210     C     CHKFIL        BEGSR
040300000000     C*
040400000000     C     *LIKE         DEFINE    V1CLNP        WFIL
040500000000     C     *LIKE         DEFINE    V1DLNP        WDFIL
040600000000     C*
040700980210     C* RICHIESTO ELENCO (?)
040800000000     C                   SETOFF                                           28
040900000000     C     '?'           SCAN      WFIL                                   28
041000000000IF  1C     *IN28         IFEQ      *ON
041100000000     C                   MOVEL     *BLANKS       §COD1
041200980210     C                   MOVEL     *BLANKS       §DES1
041300020806     C                   MOVEL     *BLANKS       §PRA
041400971203     C                   CALL      'TNSD24R'
041500000000     C                   PARM                    §COD1             3
041600000000     C                   PARM                    §TIP              1
041700000000     C                   PARM                    §DES1            25
041800971203     C                   PARM                    §PRA              3
041900000000     C                   MOVEL     §COD1         WFIL                           *CODICE
042000000000     C                   MOVEL     §DES1         WDFIL                          *DESCRIZIONE
042100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
042200000000E   1C                   ENDIF
042300000000     C*
042400980210     C* CONTROLLA SOLO CARATTERI NUMERICI
042500000000     C                   SETOFF                                       28
042600000000     C                   TESTN                   WFIL                 28
042700000000     C                   MOVE      WFIL          WA1               1
042800000000IF  1C     *IN28         IFEQ      *OFF                                         *CARATTERI ALFABETIC
042900000000     C     WA1           ORLT      '0'                                          *ULTIMO CARATTERE AL
043000980210     C                   MOVEL     '1'           $ERR                           *ERRORE
043100000000X   1C                   ELSE
043200980210     C*
043300980210     C* CONTROLLA ESISTEZA RECORD
043400000000     C                   MOVEL     WFIL          KORFIL
043500000000     C     KORFIL        CHAIN     AZORG01L                           99
043600980210IF  2C     *IN99         IFEQ      *OFF                                         *ESISTENTE
043700980210     C     ORGFVA        ANDEQ     *BLANKS                                      *NO ANNULLATA
043800980210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
043900000000     C                   MOVEL     ORGDES        WDFIL
044000980210X   2C                   ELSE
044100000000     C                   MOVEL     '1'           $ERR
044200980210E   2C                   ENDIF
044300000000E   1C                   ENDIF
044400020924     C*
044500020924     C* CONTROLLA SE FILIALE IN GESTIONE
044600020924     C                   MOVEL     WFIL          WNUMFIL           3 0
044700020924     C     WNUMFIL       LOOKUP    L1                                     20
044800020924     C                   IF        *IN20 = *ON
044900020924     C                   ELSE
045000020924     C                   MOVEL     '1'           $ERR
045100020924     C                   ENDIF
045200000000     C*
045300000000     C                   ENDSR
045400000000     C*------------------------------------------------------------------------*
045500000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
045600000000     C*------------------------------------------------------------------------*
045700000000     C     CHKTSP        BEGSR
045800000000     C*
045900000000     C     *LIKE         DEFINE    V1CTSP        WTSP
046000000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
046100000000     C*
046200980210     C* RICHIESTO ELENCO (?)
046300000000IF  1C     WTSP          IFEQ      '?'
046400020919     C                   MOVEL     1             §KUT
046500000000     C                   MOVEL     '5E'          §COD
046600000000     C                   MOVEL     *BLANKS       §KEY
046700000000     C                   MOVEL     *BLANKS       §DES
046800000000     C                   CALL      'X§TABER'
046900000000     C                   PARM                    §KUT              1 0
047000000000     C                   PARM                    §COD              2
047100000000     C                   PARM                    §KEY              8
047200000000     C                   PARM                    §DES             30
047300000000     C                   MOVEL     §KEY          WTSP
047400000000     C                   MOVEL     §DES          WDTSP
047500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
047600000000E   1C                   ENDIF
047700000000     C*
047800980210     C* CONTROLLA ESISTEZA RECORD
047900020919     C                   MOVEL     1             KTBKUT
048000000000     C                   MOVEL     '5E'          KTBCOD
048100000000     C                   MOVEL(P)  WTSP          KTBKEY
048200000000     C     KEYTAB        CHAIN     TABEL00F                           99
048300000000IF  1C     *IN99         IFEQ      *OFF                                         *ESISTENTE
048400000000     C     TBLFLG        ANDEQ     *BLANKS                                      *NON ANNULLATO
048500000000     C                   MOVEL     TBLUNI        WDTSP
048600000000X   1C                   ELSE
048700000000     C                   MOVEL     '1'           $ERR
048800000000E   1C                   ENDIF
048900000000     C*
049000000000     C                   ENDSR
049100000000     C*------------------------------------------------------------------------*
049200000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
049300000000     C*------------------------------------------------------------------------*
049400000000     C     IMPVID        BEGSR
049500000000     C*
049600020919     C                   MOVEL     DUTPOU        V1CLNP                         *LINEA DI PARTENZA
049700000000     C                   MOVEL     'C'           V1CTEM                         *TEMPI DELIVERY
049800960702     C                   MOVEL     'E'           V1CTSP                         *TIPO SERVIZIO
049900980210     C                   MOVEL     'N'           V1CISO                         *CONSIDERA ISOLE
050000980210     C                   MOVEL     'N'           V1CDIS                         *CONSIDERA L.DISAG.
050100980210     C                   MOVEL     'S'           V1CSSA                         *SINTETICA/ANALITICA
050200061120     c                   eval      V1Clin = 'it'
050300061120      * decodifico la lingua da azlin
050400061120     c     V1Clin        chain     AZLIN000
050500061120     c                   if        %found(AZLIN03L)
050600061120     c                   eval      V1Dlin = LINdesita
050700061120     c                   endif
050800000000     C*
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* CARTAB - CARICA LE TABELLE
051200000000     C*------------------------------------------------------------------------*
051300000000     C     CARTAB        BEGSR
051400020924     C*
051500020924     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
051600020924     C                   CLEAR                   L1
051700020924     C                   CLEAR                   TRUL06DS
051800020924     C                   CLEAR                   LIA
051900020924     C                   CLEAR                   TRUL26DS
052000020924     C                   CLEAR                   SKPO
052100020924     C                   CLEAR                   SKPOT
052200020924     C*
052300020924     C* CARICA LE FILIALI IN GESTIONE
052400020924     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
052500020924     C                   EVAL      DEPKPJBU = KPJBU
052600020924     C                   MOVEL     'L'           D06TLA
052700020924     C                   MOVEL     '£1'          D06COD
052800021007     C                   MOVEL     duttfp        D06KEY
052900020924     C                   MOVEL     TRUL06DS      KPJBU
053000020924     C                   CALL      'TRUL06R'
053100020924     C                   PARM                    KPJBA
053200020924     C                   MOVEL     KPJBU         TRUL06DS
053300020924     C                   MOVEA     LIA           L1                              -P.O. terminal
053400020924     C                   EVAL      KPJBU = DEPKPJBU
053500020924     C                   ENDIF
053600020924     C*
053700020924     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
053800020924     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
053900020924     C                   ENDIF
054000020924     C*
054100020924     C                   IF        DUTLPO = 'S'                                 *SEDE
054200020924     C                   EVAL      I26TLA = 'L'
054300020924     C                   EVAL      I26TPT = 'P'
054400020924     C                   CALL      'TRUL26R'
054500020924     C                   PARM                    TRUL26DS
054600020924     C                   MOVEA     SKPO          L1                              -tutti i P.O.
054700020924     C                   ENDIF
054800061120      *
054900061120      * reperisco tabella 1H
055000061120     c                   clear                   ds1H
055100061120     c                   eval      KTBkut = 1
055200061120     c                   eval      KTBcod = '1H'
055300061120     c                   eval      KTBKEY = '37      '
055400061120     c     KEYTAB        chain     TABEL
055500061120if  1c                   if             %found(TABEL00F)                        *ESISTENTE
055600061120     c                             and  TBLflg = *blanks                        *NON ANNULLATO
055700061120     c                   movel     TBLuni        ds1H
055800061120e   1c                   endif
055900000000     C*
056000000000     C                   ENDSR
056100020919     C*--------------------------------------------------------------------------------------------*
056200020919     C* REPERISCE I DATI UTENTE
056300020919     C*--------------------------------------------------------------------------------------------*
056400020919     C     REPDATIUTE    BEGSR
056500020919     C*
056600020919     C* INIZIALIZZA VARIABILI DI WRK
056700020919     C                   CLEAR                   TIBS34DS
056800020919     C                   CLEAR                   AZUTEDS
056900020919     C                   CLEAR                   DDATIUTE
057000020919     C*
057100020919     C     *DTAARA       DEFINE    §azute        azuteds
057200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
057300020919     C                   IN(E)     *DTAARA
057400020919if  1C                   IF        %Error
057500020919     c                   EVAL      I34Tla = 'L'
057600020919     C                   CALL      'TIBS34R'
057700020919     C                   PARM                    Tibs34Ds
057800020919     C                   IN        *DTAARA
057900020919e   1C                   ENDIF
058000020919     C*
058100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
058200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
058300020919     C*
058400020919     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600000000     C* *INZSR - ROUTINE INIZIALE
058700000000     C*------------------------------------------------------------------------*
058800000000     C     *INZSR        BEGSR
058900980210     C*---
059000000000     C* RICEVIMENTO PARAMETRI
059100980210     C*---
059200000000     C     *ENTRY        PLIST
059300000000     C                   PARM                    KPJBA
059400980210     C*---
059500000000     C* VARIABILI RIFERITE AL DATA BASE
059600980210     C*---
059700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
059800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
059900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
060000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
060100020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
060200980210     C*---
060300000000     C* CHIAVI DI LETTURA
060400980210     C*---
060500980210     C     KEYTAB        KLIST                                                  *TABEL00F
060600980210     C                   KFLD                    KTBKUT                          -UTENTE
060700980210     C                   KFLD                    KTBCOD                          -TABELLA
060800980210     C                   KFLD                    KTBKEY                          -CHIAVE
060900980210     C     KE2TAB        KLIST                                                  *TABEL00F
061000980210     C                   KFLD                    KTBKUT                          -UTENTE
061100980210     C                   KFLD                    KTBCOD                          -TABELLA
061200980210     C*---
061300000000     C* VARIABILI DI CONTROLLO
061400980210     C*---
061500000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
061600000000     C*
061700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
061800980210     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
061900000000     C                   EXSR      INZSTD
062000980210     C*
062100980210     C* CALCOLA LA DATA CORRENTE
062200980210     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
062300980210     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
062400980210     C                   Z-ADD     WN8           G08DAT
062500980210     C                   Z-ADD     *ZEROS        G08INV
062600980210     C                   MOVEL     '0'           G08ERR
062700980210     C                   CALL      'XSRDA8'
062800980210     C                   PARM                    WLBDA8
062900980210     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
063000000000     C*
063100980210     C* CARICA TABELLE OCCORRENTI
063200000000     C                   EXSR      CARTAB
063300000000     C*
063400000000     C* IMPOSTAZIONI VIDEO
063500000000     C                   EXSR      IMPVID
063600000000     C*
063700000000     C* EMISSIONE VIDEO
063800000000     C                   EXSR      CARV1
063900000000     C*
064000000000     C                   ENDSR
064100000000     C*------------------------------------------------------------------------*
064200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
064500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
064600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
064700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064900000000     C*------------------------------------------------------------------------*
065000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
065100000000     C*------------------------------------------------------------------------*
065200000000     C     INZSTD        BEGSR
065300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
065400000000     C                   CALL      'XKEYS'
065500000000     C                   PARM                    DSKEYS
065600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
065700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
065800000000     C     XMSGPA        PLIST
065900000000     C                   PARM                    DSMSMS
066000000000     C                   PARM                    DSMSPG
066100000000     C                   PARM                    DSMSKE
066200000000     C                   PARM                    DSMSER
066300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
066400970623     C                   MOVEL     'TISI60R'     DSMSPG                         *NOME PGM CHIAMANTE
066500970623     C                   MOVEL     'TISI60R'     VMCQUE                         *CODA MESSAGGI DESTI
066600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
066700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
066800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
066900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
067000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
067100000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
067200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
067300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
067400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
067500000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
067600000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
067700000000     C* LANCIO ROUTINE PER VERIFICARE SE
067800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
067900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
068000020919     C                   EXSR      REPDATIUTE
068100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
068200000000     C                   BITOFF    '01234567'    POSMI1            1
068300000000     C                   BITOFF    '01234567'    POSMI2            1
068400000000     C                   BITOFF    '01234567'    POSMA1            1
068500000000     C                   BITOFF    '01234567'    POSMA2            1
068600000000     C                   BITON     '34'          POSMI1            1            *18
068700000000     C                   BITON     '7'           POSMI2            1            *01
068800000000     C                   BITON     '34'          POSMA1            1            *18
068900000000     C                   BITON     '0'           POSMA2            1            *80
069000000000     C                   MOVE      POSMI2        POSMIN            2
069100000000     C                   MOVEL     POSMI1        POSMIN
069200000000     C                   MOVE      POSMA2        POSMAX            2
069300000000     C                   MOVEL     POSMA1        POSMAX
069400000000     C                   ENDSR
069500000000     C*------------------------------------------------------------------------*
069600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
069700000000     C*------------------------------------------------------------------------*
069800000000     C     FUNSTD        BEGSR
069900000000     C                   SELECT
070000000000     C     KEY           WHENEQ    F3
070100000000     C                   EXSR      F03STD
070200000000     C     KEY           WHENEQ    F12
070300000000     C                   EXSR      F12STD
070400000000     C     KEY           WHENEQ    F24
070500000000     C                   EXSR      F24STD
070600000000     C                   OTHER
070700000000     C                   MOVEL     MSG(2)        $MSG
070800000000     C                   EXSR      CARMSG
070900000000     C                   ENDSL
071000000000     C                   ENDSR
071100000000     C*------------------------------------------------------------------------*
071200000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
071300000000     C*------------------------------------------------------------------------*
071400000000     C     F03STD        BEGSR
071500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
071600000000     C                   MOVEL     '1'           $F03
071700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
071800000000     C                   MOVEL     '1'           $FINE
071900000000     C                   ENDSR
072000000000     C*------------------------------------------------------------------------*
072100000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
072200000000     C*------------------------------------------------------------------------*
072300000000     C     F12STD        BEGSR
072400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
072500000000     C                   MOVEL     '1'           $F12
072600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
072700000000     C                   MOVEL     '1'           $FINE
072800000000     C                   ENDSR
072900000000     C*------------------------------------------------------------------------*
073000000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
073100000000     C*------------------------------------------------------------------------*
073200000000     C     F24STD        BEGSR
073300000000     C*
073400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
073500000000     C                   EXSR      OUTFUN
073600000000     C*
073700000000     C                   ENDSR
073800000000     C*------------------------------------------------------------------------*
073900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
074000000000     C*          CODA MESSAGGI DEL PGM
074100000000     C*------------------------------------------------------------------------*
074200000000     C     CARMSG        BEGSR
074300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
074400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
074500000000     C     MSGULT        IFEQ      0
074600000000     C                   MOVEL     '1'           *IN40
074700970623     C                   WRITE     SI60DVK
074800000000     C                   MOVEL     '0'           *IN40
074900000000     C                   ENDIF
075000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
075100000000     C                   MOVEL     $MSG          DSMSMS
075200000000     C                   CALL      'XMSG'        XMSGPA
075300000000     C     DSMSER        IFEQ      0
075400000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
075500000000     C                   ENDIF
075600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
075700000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
075800970623     C                   WRITE     SI60DVM                                      *SFL MESS
075900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
076000000000     C                   MOVEL     *BLANKS       $MSG
076100000000     C                   ENDSR
076200000000     C*------------------------------------------------------------------------*
076300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
076400000000     C*------------------------------------------------------------------------*
076500000000     C     OUTMSG        BEGSR
076600000000     C* VISUALIZZAZIONE MESSAGGIO
076700000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
076800970623     C                   WRITE     SI60DVK                                      *CONTR MESS
076900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
077000000000     C                   Z-ADD     *ZEROS        MSGULT
077100000000     C                   ENDSR
077200000000     C*------------------------------------------------------------------------*
077300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
077400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
077500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
077600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
077700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
077800000000     C*------------------------------------------------------------------------*
077900000000     C     CARFUN        BEGSR
078000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
078100000000     C     1             DO        31            I                 5 0          --- 1 -->
078200000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
078300000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
078400000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
078500020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
078600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
078700000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
078800000000     C                   ELSE                                                   *INTERROG.
078900000000     C                   MOVEL     FUNASI        FAT(I)
079000000000     C                   ENDIF
079100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
079200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
079300000000     C                   MOVEL     FUNAFG        FAT(I)
079400000000     C                   ELSE                                                   *INTERROG.
079500000000     C                   MOVEL     FUNAFI        FAT(I)
079600000000     C                   ENDIF
079700000000     C                   ENDIF                                                  <-- 2 ---
079800000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
079900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
080000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
080100000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
080200000000     C                   ELSE
080300000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
080400000000     C                   ENDIF
080500000000     C                   ENDIF
080600000000     C                   ENDDO                                                  <-- 1 ---
080700000000     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
081000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
081100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
081200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
081300000000     C*------------------------------------------------------------------------*
081400000000     C     OUTFUN        BEGSR
081500000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
081600000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
081700000000     C                   ENDDO
081800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
081900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
082000000000     C     1             DO        31            I
082100000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
082200000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
082300000000     C                   ADD       1             FUNNAT
082400000000     C                   ENDIF
082500000000     C                   ENDDO
082600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
082700000000     C     FUNNAT        IFGT      8
082800000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
082900000000     C                   ENDIF
083000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
083100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
083200000000     C                   Z-ADD     8             FUNVIS            5 0
083300000000     C     FAT(23)       IFEQ      'S'
083400000000     C                   SUB       1             FUNVIS
083500000000     C                   ENDIF
083600000000     C     FAT(24)       IFEQ      'S'
083700000000     C                   SUB       1             FUNVIS
083800000000     C                   ENDIF
083900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
084000000000     C                   Z-ADD     *ZEROS        W                 5 0
084100000000     C                   ADD       1             FUNULT
084200000000     C     FUNULT        IFGT      31
084300000000     C                   Z-ADD     1             FUNULT
084400000000     C                   ENDIF
084500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
084600000000     C     FUNULT        DO        31            I                              --- 1 -->
084700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
084800000000     C     W             IFLT      FUNVIS                                       --- 2 -->
084900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
085000000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
085100000000     C     FDE(I)        ANDNE     *BLANKS
085200000000     C     I             IFEQ      23                                           *TOLGO F23
085300000000     C     I             OREQ      24                                           *E F24
085400000000     C                   ELSE
085500000000     C                   ADD       1             W
085600000000     C                   MOVEL     FDE(I)        FUV(W)
085700000000     C                   ENDIF
085800000000     C                   ENDIF                                                  <-- 3 ---
085900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
086000000000     C                   Z-ADD     I             FUNULT
086100000000     C                   ENDIF                                                  <-- 2 ---
086200000000     C                   ENDDO                                                  <-- 1 ---
086300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
086400000000     C                   MOVEL     FUV(1)        VFD001
086500000000     C                   MOVEL     FUV(2)        VFD002
086600000000     C                   MOVEL     FUV(3)        VFD003
086700000000     C                   MOVEL     FUV(4)        VFD004
086800000000     C                   MOVEL     FUV(5)        VFD005
086900000000     C                   MOVEL     FUV(6)        VFD006
087000000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
087100000000     C     FAT(24)       ANDEQ     'N'
087200000000     C                   MOVEL     FUV(7)        VFD007
087300000000     C                   MOVEL     FUV(8)        VFD008
087400000000     C                   ELSE                                                   <-- 1 -->
087500000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
087600000000     C     FAT(24)       ANDEQ     'S'
087700000000     C                   MOVEL     FDE(23)       VFD007
087800000000     C                   MOVEL     FDE(24)       VFD008
087900000000     C                   ELSE                                                   <-- 2 -->
088000000000     C                   MOVEL     FUV(7)        VFD007
088100000000     C     FAT(23)       IFEQ      'S'
088200000000     C                   MOVEL     FDE(23)       VFD008
088300000000     C                   ELSE
088400000000     C                   MOVEL     FDE(24)       VFD008
088500000000     C                   ENDIF
088600000000     C                   ENDIF                                                  <-- 2 ---
088700000000     C                   ENDIF                                                  <-- 1 ---
088800000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
088900000000     C                   ENDSR
089000000000     C*------------------------------------------------------------------------*
089100000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
089200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
089300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
089400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
089500000000     C*------------------------------------------------------------------------*
089600000000     C     CONFUN        BEGSR
089700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
089800000000     C     1             DO        31            I                              --- 1 -->
089900000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
090000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
090100000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
090200000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
090300000000     C                   EXSR      CARMSG
090400000000     C                   MOVEL     '1'           $ERR
090500000000     C                   ENDIF                                                  <-- 3 ---
090600000000     C                   ENDIF                                                  <-- 2 ---
090700000000     C                   ENDDO                                                  <-- 1 ---
090800000000     C                   ENDSR
090900000000     O*------------------------------------------------------------------------*
091000000000** MSG - (MESSAGGI)
091100970623TISI60R- Tasto funzionale non valido.                                          1
091200970623TISI60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
091300970623TISI60R- Opzione non valida.                                                   3
091400970623TISI60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
091500970623TISI60R- Inizio elenco.                                                        5
091600970623TISI60R- Fine elenco.                                                          6
091700020924TISI60R- Punto operativo errato o non in gestione.                             7
091800980210TISI60R- Tipo servizio errato                                                  8
091900050912TISI60R- Selezione ammessa SOLO per stampa analitica                           9
092000061120TISI60R- Lingua errata
092100980210TISI60R- Immettere uno dei valori possibili                                   11
092200061120TISI60R- 12
092300970623TISI60R- 13
092400970623TISI60R- 14
092500970623TISI60R- Stampa lanciata ...                                                  15
092600000000** CMD  (COMANDI)
092700000000WRKSPLF
092800000000
092900000000
093000000000
093100000000
093200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
093300000000F1    -                   -    NN     NN
093400000000F2    -                   -    NN     NN
093500000000F3    -F3=Fine            -    SS     SS
093600061120F4    -                   -    NN     NN
093700000000F5    -                   -    NN     NN
093800000000F6    -F6=Conferma        -    SS     SS
093900000000F7    -                   -    NN     NN
094000000000F8    -                   -    NN     NN
094100000000F9    -                   -    NN     NN
094200000000F10   -                   -    NN     NN
094300000000F11   -                   -    NN     NN
094400000000F12   -F12=Ritorno        -    SS     SS
094500000000F13   -                   -    NN     NN
094600000000F14   -                   -    NN     NN
094700000000F15   -                   -    NN     NN
094800000000F16   -                   -    NN     NN
094900000000F17   -                   -    NN     NN
095000000000F18   -                   -    NN     NN
095100000000F19   -                   -    NN     NN
095200000000F20   -                   -    NN     NN
095300000000F21   -                   -    NN     NN
095400000000F22   -F22=Gestione stampe-    EE     EE
095500000000F23   -F23=Altre opzioni  -    NN     NN
095600000000F24   -F24=Altri tasti    -    NN     NN
095700000000ENTER -                   -    SS     SS
095800000000ROLLUP-                   -    NN     NN
095900000000ROLLDN-                   -    NN     NN
096000000000HELP  -                   -    NN     NN
096100000000PRINT -                   -    NN     NN
096200000000BSPACE-                   -    NN     NN
096300000000CLEAR -                   -    NN     NN
