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
010000090608     d  PARlin               234    234
010100090608     d  PARold               235    235
010200980210     D*---------------------
010300980210     D* ARCHITETTURA
010400980210     D*---------------------
010500980210     D KPJBA         E DS
010600061120      *---------------------
010700061120      * TABELLE
010800061120      *---------------------
010900061120     d ds1H          e ds
011000061120      *---------------------
011100061120      * VARIABILI
011200061120      *---------------------
011300061120     d P37iso2         s                   like(LINiso2)   inz
011400061120     d P37desi         s                   like(LINdesita) inz
011500000000     C*------------------------------------------------------------------------*
011600000000     C* MAIN LINE
011700000000     C*------------------------------------------------------------------------*
011800000000     C*
011900000000     C* CICLO FINO A CHE:
012000000000DO  1C     $FINE         DOUEQ     '1'
012100000000     C*
012200000000     C* CONTROLLA GESTIONE VIDEO
012300980210     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012400000000IF  2C     $GES          IFEQ      'V1'
012500000000     C                   EXSR      GESV1                                        *PARAMETRI
012600000000E   2C                   ENDIF
012700000000E   1C                   ENDDO
012800980210     C*
012900000000     C                   SETON                                        LR
013000000000     C*------------------------------------------------------------------------*
013100000000     C* GESV1 - GESTIONE VIDEO UNO
013200000000     C*------------------------------------------------------------------------*
013300000000     C     GESV1         BEGSR
013400000000     C*
013500000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013600000000     C                   EXSR      CARV1                                        *CARICA VIDEO
013700000000E   1C                   ENDIF
013800000000     C*
013900000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
014000000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014100000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014200000000E   1C                   ENDIF
014300000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014400000000     C*
014500970623     C                   EXFMT     SI60DV1                                      *PARAMETRI
014600000000     C*
014700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
014800000000     C*
014900000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015000000000SL  2C                   SELECT
015100000000     C     KEY           WHENEQ    ENTER
015200000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015300000000     C     KEY           WHENEQ    F6
015400000000     C                   EXSR      F06GES                                       *CONFERMA
015500000000     C     KEY           WHENEQ    F22
015600000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
015700000000     C                   OTHER
015800000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015900000000E   2C                   ENDSL
016000000000E   1C                   ENDIF
016100000000     C*
016200000000     C                   ENDSR
016300000000     C*------------------------------------------------------------------------*
016400000000     C* CARV1 - CARICA VIDEO UNO
016500000000     C*------------------------------------------------------------------------*
016600000000     C     CARV1         BEGSR
016700000000     C*
016800000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016900000000     C*
017000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017200000000     C*
017300970623     C                   WRITE     SI60DVT                                      *TESTATA
017400970623     C                   WRITE     SI60DVF                                      *FUNZIONI
017500000000     C*
017600000000     C                   ENDSR
017700000000     C*------------------------------------------------------------------------*
017800000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017900000000     C*------------------------------------------------------------------------*
018000000000     C     ENTV1         BEGSR
018100000000     C*
018200000000     C* CONTROLLA I CAMPI IMMESSI
018300000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*------------------------------------------------------------------------*
018700000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018800000000     C*------------------------------------------------------------------------*
018900000000     C     F06GES        BEGSR
019000000000     C*
019100000000     C* CONTROLLA I CAMPI IMMESSI
019200000000     C                   EXSR      DECODI
019300000000     C*
019400000000     C* LANCIA IL PROGRAMMA DI ESECUZIONE
019500000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019600000000     C                   MOVEL     V1CLNP        PARLNP                         *LINEA DI PARTENZA
019700000000     C                   MOVEL     V1CTEM        PARTEM                         *TEMPI DELIVERY
019800000000     C                   MOVEL     V1CTSP        PARTSP                         *TIPO SERVIZIO
019900090608     C                   MOVEL     V1Cold        PARold                         *TIPO SERVIZIO
020000980210     C                   MOVEL     V1CISO        PARISO                         *CONSIDERA ISOLE
020100980210     C                   MOVEL     V1CDIS        PARDIS                         *CONSIDERA L.DISAGG.
020200980210     C                   MOVEL     V1CSSA        PARSSA                         *SINTETICA/DETTAGLIO
020300000000     C                   MOVEL     V1CND1        PARND1                         *NOTA DELIVERY N°1
020400000000     C                   MOVEL     V1CND2        PARND2                         *NOTA DELIVERY N°2
020500000000     C                   MOVEL     V1CND3        PARND3                         *NOTA DELIVERY N°3
020600061120     c                   move      LINtabel      PARlin                         *NOTA DELIVERY N°3
020700000000     C*
020800970708     C                   MOVEL     'SI62'        KCOAZ
020900000000     C                   MOVEL     PARAM         KPJBU
021000090604     c**                 if        KNMUS = *all'1'
021100090619     c**                 call      'TISI62R'
021200090619     c**                 parm                    KPJBA
021300090604     c**                 else
021400090619     C                   CALL      'BCH10'
021500090619     C                   PARM                    KPJBA
021600090604     c**                 endif
021700000000     C*
021800000000     C                   MOVEL     MSG(15)       $MSG
021900000000     C                   EXSR      CARMSG
022000000000E   1C                   ENDIF
022100000000     C*
022200000000     C                   ENDSR
022300000000     C*------------------------------------------------------------------------*
022400000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
022500000000     C*------------------------------------------------------------------------*
022600000000     C     F22GES        BEGSR
022700000000     C*
022800000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022900000000     C                   Z-ADD     80            LENGH            15 5
023000000000     C                   CALL      'QCAEXEC'
023100000000     C                   PARM                    CMD(1)
023200000000     C                   PARM                    LENGH
023300000000     C*
023400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023900000000     C*------------------------------------------------------------------------*
024000000000     C     DECODI        BEGSR
024100000000     C*
024200000000     C* REIMPOSTA INDICATORI DI ERRORE
024300000000     C                   SETOFF                                       606162
024400980210     C                   SETOFF                                       636465
024500061120     c                   setoff                                       66
024600000000     C*--------------------
024700000000     C* LINEA DI PARTENZA
024800000000     C*--------------------
024900000000IF  1C     $ERR          IFEQ      '0'
025000000000     C                   MOVEL     V1CLNP        WFIL
025100980210     C                   MOVEL     *BLANKS       WDFIL
025200980210     C                   EXSR      CHKFIL
025300000000     C                   MOVEL     WFIL          V1CLNP                         *CODICE
025400000000     C                   MOVEL     WDFIL         V1DLNP                         *DESCRIZIONE
025500000000IF  2C     $ERR          IFEQ      '1'
025600000000     C                   SETON                                        60
025700000000     C                   MOVEL     MSG(7)        $MSG
025800000000     C                   EXSR      CARMSG
025900000000E   2C                   ENDIF
026000000000E   1C                   ENDIF
026100000000     C*--------------------
026200000000     C* TEMPO DELIVERY
026300000000     C*--------------------
026400000000IF  1C     $ERR          IFEQ      '0'
026500000000IF  2C     V1CTEM        IFEQ      'I'
026600000000     C     V1CTEM        OREQ      'C'
026700000000X   2C                   ELSE
026800000000     C                   SETON                                        61
026900000000     C                   MOVEL     '1'           $ERR
027000980210     C                   MOVEL     MSG(11)       $MSG
027100000000     C                   EXSR      CARMSG
027200000000E   2C                   ENDIF
027300000000E   1C                   ENDIF
027400000000     C*--------------------
027500000000     C* TIPO SERVIZIO
027600000000     C*--------------------
027700000000IF  1C     $ERR          IFEQ      '0'
027800000000     C                   MOVEL     V1CTSP        WTSP
027900980210     C                   MOVEL     *BLANKS       WDTSP
028000000000     C                   EXSR      CHKTSP
028100000000     C                   MOVEL     WTSP          V1CTSP                         *CODICE
028200000000     C                   MOVEL     WDTSP         V1DTSP                         *DESCRIZIONE
028300000000IF  2C     $ERR          IFEQ      '1'
028400000000     C                   SETON                                        62
028500980210     C                   MOVEL     MSG(8)        $MSG
028600000000     C                   EXSR      CARMSG
028700000000E   2C                   ENDIF
028800000000E   1C                   ENDIF
028900090608     c* non posso chidere stampa sintetica per tipo "E" a modo nuovo
029000090608IF  1C     $ERR          IFEQ      '0'
029100090608     c                   if        v1ctsp='E'  and v1cold=' 'and
029200090608     c                             v1cssa='S'
029300090608     C                   SETON                                        62
029400090608     C                   MOVEL     MSG(12)       $MSG
029500090608     C                   EXSR      CARMSG
029600090608E   2C                   ENDIF
029700090608E   1C                   ENDIF
029800090608     c
029900061120      *--------------------
030000061120      * LINGUA
030100061120      *--------------------
030200061120if  1c                   if        $Err   = *off
030300061120     c                   clear                   V1Dlin
030400061120      * Ricerca
030500061120if  2c                   if        %scan('?':V1Clin) > *zeros
030600061120     c                   clear                   P37iso2
030700061120     c                   clear                   P37desi
030800061120     c                   call      'TRUL37R'
030900061120     c                   parm                    P37iso2
031000061120     c                   parm                    P37desi
031100061120     c                   eval      V1Clin = P37iso2
031200061120     c                   eval      V1Dlin = P37desi
031300061120     c                   leavesr
031400061120e   2c                   endif
031500061120      * Controllo:
031600061120      * - lingua obbligatoria
031700061120if  2c                   if        V1Clin = *blanks
031800061120     c                   eval      $Err   = *on
031900061120     c                   eval      *in66  = *on
032000061120     c                   movel     Msg(10)       $Msg
032100061120     c                   exsr      CarMSG
032200061120     c                   leavesr
032300061120e   2c                   endif
032400061120      * - lingua errata
032500061120     c     V1Clin        chain     AZLIN000
032600061120if  2c                   if        not %found(AZLIN03L)
032700061120     c                   eval      $Err   = *on
032800061120     c                   eval      *in66  = *on
032900061120     c                   movel     Msg(10)       $Msg
033000061120     c                   exsr      CarMSG
033100061120     c                   leavesr
033200061120e   2c                   endif
033300061120     c                   eval      V1Dlin = LINdesita
033400061120      * - se non è attiva per i testi errore
033500061120if  2c                   if        LINfntxt = '0'
033600061120     c                   eval      $Err   = *on
033700061120     c                   eval      *in66  = *on
033800061120     c                   movel     Msg(10)       $Msg
033900061120     c                   exsr      CarMSG
034000061120     c                   leavesr
034100061120e   2c                   endif
034200061120      * - se ho scelto di stampare in lingua il testo deve essere
034300061120      *   gestito in lingua
034400061120if  2c                   if            V1Clin <> 'it'
034500061120     c                             and §1Hlin = *blanks
034600061120     c                   eval      $Err   = *on
034700061120     c                   eval      *in66  = *on
034800061120     c                   movel     Msg(10)       $Msg
034900061120     c                   exsr      CarMSG
035000061120     c                   leavesr
035100061120e   2c                   endif
035200061120e   1c                   ENDIF
035300000000     C*--------------------
035400980216     C* CAP/LOCALITA' ISOLA
035500000000     C*--------------------
035600000000IF  1C     $ERR          IFEQ      '0'
035700000000IF  2C     V1CISO        IFEQ      'S'
035800000000     C     V1CISO        OREQ      'N'
035900000000X   2C                   ELSE
036000000000     C                   SETON                                        63
036100000000     C                   MOVEL     '1'           $ERR
036200000000     C                   MOVEL     MSG(11)       $MSG
036300000000     C                   EXSR      CARMSG
036400000000E   2C                   ENDIF
036500000000E   1C                   ENDIF
036600980210     C*--------------------
036700980216     C* CAP/LOCALITA' DISAGIATE
036800980210     C*--------------------
036900980210IF  1C     $ERR          IFEQ      '0'
037000980210IF  2C     V1CDIS        IFEQ      'S'
037100980210     C     V1CDIS        OREQ      'N'
037200980210X   2C                   ELSE
037300980210     C                   SETON                                        64
037400980210     C                   MOVEL     '1'           $ERR
037500980210     C                   MOVEL     MSG(11)       $MSG
037600980210     C                   EXSR      CARMSG
037700980210E   2C                   ENDIF
037800980210E   1C                   ENDIF
037900980210     C*--------------------
038000980210     C* STAMPA SINTETICA / ANALITICA
038100980210     C*--------------------
038200980210IF  1C     $ERR          IFEQ      '0'
038300980210IF  2C     V1CSSA        IFEQ      'S'
038400980210     C     V1CSSA        OREQ      'A'
038500980210X   2C                   ELSE
038600980210     C                   SETON                                        65
038700980210     C                   MOVEL     '1'           $ERR
038800980210     C                   MOVEL     MSG(11)       $MSG
038900980210     C                   EXSR      CARMSG
039000980210E   2C                   ENDIF
039100980210E   1C                   ENDIF
039200050912     C*--------------------
039300050912     C* CAP/LOCALITA' ISOLA & DISAGIATE
039400050912     C*--------------------
039500050912if  1C                   if             $Err   = *off
039600050912     C                             and  V1Cssa = 'S'
039700050912     C                             and (V1Ciso = 'S'
039800050912     C                              or  V1Cdis = 'S')
039900050912sel 2c                   select
040000050912w   2C                   when      V1Ciso = 'S'
040100050912     C                   eval      *in63 = *on
040200050912w   2C                   when      V1Cdis = 'S'
040300050912     C                   eval      *in64 = *on
040400050912e   2c                   endsl
040500050912     C                   eval      $Err  = *on
040600050912     C                   movel     Msg(9)        $Msg
040700050912     C                   exsr      CarMSG
040800050912E   1C                   endif
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*------------------------------------------------------------------------*
041200980210     C* CHKFIL - CONTROLLA LA FILIALE
041300000000     C*------------------------------------------------------------------------*
041400980210     C     CHKFIL        BEGSR
041500000000     C*
041600000000     C     *LIKE         DEFINE    V1CLNP        WFIL
041700000000     C     *LIKE         DEFINE    V1DLNP        WDFIL
041800000000     C*
041900980210     C* RICHIESTO ELENCO (?)
042000000000     C                   SETOFF                                           28
042100000000     C     '?'           SCAN      WFIL                                   28
042200000000IF  1C     *IN28         IFEQ      *ON
042300000000     C                   MOVEL     *BLANKS       §COD1
042400980210     C                   MOVEL     *BLANKS       §DES1
042500020806     C                   MOVEL     *BLANKS       §PRA
042600971203     C                   CALL      'TNSD24R'
042700000000     C                   PARM                    §COD1             3
042800000000     C                   PARM                    §TIP              1
042900000000     C                   PARM                    §DES1            25
043000971203     C                   PARM                    §PRA              3
043100000000     C                   MOVEL     §COD1         WFIL                           *CODICE
043200000000     C                   MOVEL     §DES1         WDFIL                          *DESCRIZIONE
043300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
043400000000E   1C                   ENDIF
043500000000     C*
043600980210     C* CONTROLLA SOLO CARATTERI NUMERICI
043700000000     C                   SETOFF                                       28
043800000000     C                   TESTN                   WFIL                 28
043900000000     C                   MOVE      WFIL          WA1               1
044000000000IF  1C     *IN28         IFEQ      *OFF                                         *CARATTERI ALFABETIC
044100000000     C     WA1           ORLT      '0'                                          *ULTIMO CARATTERE AL
044200980210     C                   MOVEL     '1'           $ERR                           *ERRORE
044300000000X   1C                   ELSE
044400980210     C*
044500980210     C* CONTROLLA ESISTEZA RECORD
044600000000     C                   MOVEL     WFIL          KORFIL
044700000000     C     KORFIL        CHAIN     AZORG01L                           99
044800980210IF  2C     *IN99         IFEQ      *OFF                                         *ESISTENTE
044900980210     C     ORGFVA        ANDEQ     *BLANKS                                      *NO ANNULLATA
045000980210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
045100000000     C                   MOVEL     ORGDES        WDFIL
045200980210X   2C                   ELSE
045300000000     C                   MOVEL     '1'           $ERR
045400980210E   2C                   ENDIF
045500000000E   1C                   ENDIF
045600020924     C*
045700020924     C* CONTROLLA SE FILIALE IN GESTIONE
045800020924     C                   MOVEL     WFIL          WNUMFIL           3 0
045900020924     C     WNUMFIL       LOOKUP    L1                                     20
046000020924     C                   IF        *IN20 = *ON
046100020924     C                   ELSE
046200020924     C                   MOVEL     '1'           $ERR
046300020924     C                   ENDIF
046400000000     C*
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
046800000000     C*------------------------------------------------------------------------*
046900000000     C     CHKTSP        BEGSR
047000000000     C*
047100000000     C     *LIKE         DEFINE    V1CTSP        WTSP
047200000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
047300000000     C*
047400980210     C* RICHIESTO ELENCO (?)
047500000000IF  1C     WTSP          IFEQ      '?'
047600020919     C                   MOVEL     1             §KUT
047700000000     C                   MOVEL     '5E'          §COD
047800000000     C                   MOVEL     *BLANKS       §KEY
047900000000     C                   MOVEL     *BLANKS       §DES
048000000000     C                   CALL      'X§TABER'
048100000000     C                   PARM                    §KUT              1 0
048200000000     C                   PARM                    §COD              2
048300000000     C                   PARM                    §KEY              8
048400000000     C                   PARM                    §DES             30
048500000000     C                   MOVEL     §KEY          WTSP
048600000000     C                   MOVEL     §DES          WDTSP
048700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
048800000000E   1C                   ENDIF
048900000000     C*
049000980210     C* CONTROLLA ESISTEZA RECORD
049100020919     C                   MOVEL     1             KTBKUT
049200000000     C                   MOVEL     '5E'          KTBCOD
049300000000     C                   MOVEL(P)  WTSP          KTBKEY
049400000000     C     KEYTAB        CHAIN     TABEL00F                           99
049500000000IF  1C     *IN99         IFEQ      *OFF                                         *ESISTENTE
049600000000     C     TBLFLG        ANDEQ     *BLANKS                                      *NON ANNULLATO
049700000000     C                   MOVEL     TBLUNI        WDTSP
049800000000X   1C                   ELSE
049900000000     C                   MOVEL     '1'           $ERR
050000000000E   1C                   ENDIF
050100000000     C*
050200000000     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
050500000000     C*------------------------------------------------------------------------*
050600000000     C     IMPVID        BEGSR
050700000000     C*
050800020919     C                   MOVEL     DUTPOU        V1CLNP                         *LINEA DI PARTENZA
050900000000     C                   MOVEL     'C'           V1CTEM                         *TEMPI DELIVERY
051000960702     C                   MOVEL     'E'           V1CTSP                         *TIPO SERVIZIO
051100980210     C                   MOVEL     'N'           V1CISO                         *CONSIDERA ISOLE
051200980210     C                   MOVEL     'N'           V1CDIS                         *CONSIDERA L.DISAG.
051300980210     C                   MOVEL     'S'           V1CSSA                         *SINTETICA/ANALITICA
051400090608     C                   MOVEL     ' '           V1Cold                         *SINTETICA/ANALITICA
051500061120     c                   eval      V1Clin = 'it'
051600061120      * decodifico la lingua da azlin
051700061120     c     V1Clin        chain     AZLIN000
051800061120     c                   if        %found(AZLIN03L)
051900061120     c                   eval      V1Dlin = LINdesita
052000061120     c                   endif
052100000000     C*
052200000000     C                   ENDSR
052300000000     C*------------------------------------------------------------------------*
052400000000     C* CARTAB - CARICA LE TABELLE
052500000000     C*------------------------------------------------------------------------*
052600000000     C     CARTAB        BEGSR
052700020924     C*
052800020924     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
052900020924     C                   CLEAR                   L1
053000020924     C                   CLEAR                   TRUL06DS
053100020924     C                   CLEAR                   LIA
053200020924     C                   CLEAR                   TRUL26DS
053300020924     C                   CLEAR                   SKPO
053400020924     C                   CLEAR                   SKPOT
053500020924     C*
053600020924     C* CARICA LE FILIALI IN GESTIONE
053700020924     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
053800020924     C                   EVAL      DEPKPJBU = KPJBU
053900020924     C                   MOVEL     'L'           D06TLA
054000020924     C                   MOVEL     '£1'          D06COD
054100021007     C                   MOVEL     duttfp        D06KEY
054200020924     C                   MOVEL     TRUL06DS      KPJBU
054300020924     C                   CALL      'TRUL06R'
054400020924     C                   PARM                    KPJBA
054500020924     C                   MOVEL     KPJBU         TRUL06DS
054600020924     C                   MOVEA     LIA           L1                              -P.O. terminal
054700020924     C                   EVAL      KPJBU = DEPKPJBU
054800020924     C                   ENDIF
054900020924     C*
055000020924     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
055100020924     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
055200020924     C                   ENDIF
055300020924     C*
055400020924     C                   IF        DUTLPO = 'S'                                 *SEDE
055500020924     C                   EVAL      I26TLA = 'L'
055600020924     C                   EVAL      I26TPT = 'P'
055700020924     C                   CALL      'TRUL26R'
055800020924     C                   PARM                    TRUL26DS
055900020924     C                   MOVEA     SKPO          L1                              -tutti i P.O.
056000020924     C                   ENDIF
056100061120      *
056200061120      * reperisco tabella 1H
056300061120     c                   clear                   ds1H
056400061120     c                   eval      KTBkut = 1
056500061120     c                   eval      KTBcod = '1H'
056600061120     c                   eval      KTBKEY = '37      '
056700061120     c     KEYTAB        chain     TABEL
056800061120if  1c                   if             %found(TABEL00F)                        *ESISTENTE
056900061120     c                             and  TBLflg = *blanks                        *NON ANNULLATO
057000061120     c                   movel     TBLuni        ds1H
057100061120e   1c                   endif
057200000000     C*
057300000000     C                   ENDSR
057400020919     C*--------------------------------------------------------------------------------------------*
057500020919     C* REPERISCE I DATI UTENTE
057600020919     C*--------------------------------------------------------------------------------------------*
057700020919     C     REPDATIUTE    BEGSR
057800020919     C*
057900020919     C* INIZIALIZZA VARIABILI DI WRK
058000020919     C                   CLEAR                   TIBS34DS
058100020919     C                   CLEAR                   AZUTEDS
058200020919     C                   CLEAR                   DDATIUTE
058300020919     C*
058400020919     C     *DTAARA       DEFINE    §azute        azuteds
058500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
058600020919     C                   IN(E)     *DTAARA
058700020919if  1C                   IF        %Error
058800020919     c                   EVAL      I34Tla = 'L'
058900020919     C                   CALL      'TIBS34R'
059000020919     C                   PARM                    Tibs34Ds
059100020919     C                   IN        *DTAARA
059200020919e   1C                   ENDIF
059300020919     C*
059400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
059600020919     C*
059700020919     C                   ENDSR
059800000000     C*------------------------------------------------------------------------*
059900000000     C* *INZSR - ROUTINE INIZIALE
060000000000     C*------------------------------------------------------------------------*
060100000000     C     *INZSR        BEGSR
060200980210     C*---
060300000000     C* RICEVIMENTO PARAMETRI
060400980210     C*---
060500000000     C     *ENTRY        PLIST
060600000000     C                   PARM                    KPJBA
060700980210     C*---
060800000000     C* VARIABILI RIFERITE AL DATA BASE
060900980210     C*---
061000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
061100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
061200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
061300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
061400020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
061500980210     C*---
061600000000     C* CHIAVI DI LETTURA
061700980210     C*---
061800980210     C     KEYTAB        KLIST                                                  *TABEL00F
061900980210     C                   KFLD                    KTBKUT                          -UTENTE
062000980210     C                   KFLD                    KTBCOD                          -TABELLA
062100980210     C                   KFLD                    KTBKEY                          -CHIAVE
062200980210     C     KE2TAB        KLIST                                                  *TABEL00F
062300980210     C                   KFLD                    KTBKUT                          -UTENTE
062400980210     C                   KFLD                    KTBCOD                          -TABELLA
062500980210     C*---
062600000000     C* VARIABILI DI CONTROLLO
062700980210     C*---
062800000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
062900090608     c* Utente EDP
063000090608     c                   if        %subst(knmus:1:3)='EDP'
063100090608     c                   seton                                        15
063200090608     c                   endif
063300000000     C*
063400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
063500980210     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
063600000000     C                   EXSR      INZSTD
063700980210     C*
063800980210     C* CALCOLA LA DATA CORRENTE
063900980210     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064000980210     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064100980210     C                   Z-ADD     WN8           G08DAT
064200980210     C                   Z-ADD     *ZEROS        G08INV
064300980210     C                   MOVEL     '0'           G08ERR
064400980210     C                   CALL      'XSRDA8'
064500980210     C                   PARM                    WLBDA8
064600980210     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064700000000     C*
064800980210     C* CARICA TABELLE OCCORRENTI
064900000000     C                   EXSR      CARTAB
065000000000     C*
065100000000     C* IMPOSTAZIONI VIDEO
065200000000     C                   EXSR      IMPVID
065300000000     C*
065400000000     C* EMISSIONE VIDEO
065500000000     C                   EXSR      CARV1
065600000000     C*
065700000000     C                   ENDSR
065800000000     C*------------------------------------------------------------------------*
065900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
066200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
066300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
066400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066600000000     C*------------------------------------------------------------------------*
066700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
066800000000     C*------------------------------------------------------------------------*
066900000000     C     INZSTD        BEGSR
067000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
067100000000     C                   CALL      'XKEYS'
067200000000     C                   PARM                    DSKEYS
067300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
067400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
067500000000     C     XMSGPA        PLIST
067600000000     C                   PARM                    DSMSMS
067700000000     C                   PARM                    DSMSPG
067800000000     C                   PARM                    DSMSKE
067900000000     C                   PARM                    DSMSER
068000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
068100970623     C                   MOVEL     'TISI60R'     DSMSPG                         *NOME PGM CHIAMANTE
068200970623     C                   MOVEL     'TISI60R'     VMCQUE                         *CODA MESSAGGI DESTI
068300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
068400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
068500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
068600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
068700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
068800000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
068900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
069000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
069100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
069200000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
069300000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
069400000000     C* LANCIO ROUTINE PER VERIFICARE SE
069500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
069600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
069700020919     C                   EXSR      REPDATIUTE
069800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
069900000000     C                   BITOFF    '01234567'    POSMI1            1
070000000000     C                   BITOFF    '01234567'    POSMI2            1
070100000000     C                   BITOFF    '01234567'    POSMA1            1
070200000000     C                   BITOFF    '01234567'    POSMA2            1
070300000000     C                   BITON     '34'          POSMI1            1            *18
070400000000     C                   BITON     '7'           POSMI2            1            *01
070500000000     C                   BITON     '34'          POSMA1            1            *18
070600000000     C                   BITON     '0'           POSMA2            1            *80
070700000000     C                   MOVE      POSMI2        POSMIN            2
070800000000     C                   MOVEL     POSMI1        POSMIN
070900000000     C                   MOVE      POSMA2        POSMAX            2
071000000000     C                   MOVEL     POSMA1        POSMAX
071100000000     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
071400000000     C*------------------------------------------------------------------------*
071500000000     C     FUNSTD        BEGSR
071600000000     C                   SELECT
071700000000     C     KEY           WHENEQ    F3
071800000000     C                   EXSR      F03STD
071900000000     C     KEY           WHENEQ    F12
072000000000     C                   EXSR      F12STD
072100000000     C     KEY           WHENEQ    F24
072200000000     C                   EXSR      F24STD
072300000000     C                   OTHER
072400000000     C                   MOVEL     MSG(2)        $MSG
072500000000     C                   EXSR      CARMSG
072600000000     C                   ENDSL
072700000000     C                   ENDSR
072800000000     C*------------------------------------------------------------------------*
072900000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
073000000000     C*------------------------------------------------------------------------*
073100000000     C     F03STD        BEGSR
073200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
073300000000     C                   MOVEL     '1'           $F03
073400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
073500000000     C                   MOVEL     '1'           $FINE
073600000000     C                   ENDSR
073700000000     C*------------------------------------------------------------------------*
073800000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
073900000000     C*------------------------------------------------------------------------*
074000000000     C     F12STD        BEGSR
074100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
074200000000     C                   MOVEL     '1'           $F12
074300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
074400000000     C                   MOVEL     '1'           $FINE
074500000000     C                   ENDSR
074600000000     C*------------------------------------------------------------------------*
074700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
074800000000     C*------------------------------------------------------------------------*
074900000000     C     F24STD        BEGSR
075000000000     C*
075100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
075200000000     C                   EXSR      OUTFUN
075300000000     C*
075400000000     C                   ENDSR
075500000000     C*------------------------------------------------------------------------*
075600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
075700000000     C*          CODA MESSAGGI DEL PGM
075800000000     C*------------------------------------------------------------------------*
075900000000     C     CARMSG        BEGSR
076000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
076100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
076200000000     C     MSGULT        IFEQ      0
076300000000     C                   MOVEL     '1'           *IN40
076400970623     C                   WRITE     SI60DVK
076500000000     C                   MOVEL     '0'           *IN40
076600000000     C                   ENDIF
076700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
076800000000     C                   MOVEL     $MSG          DSMSMS
076900000000     C                   CALL      'XMSG'        XMSGPA
077000000000     C     DSMSER        IFEQ      0
077100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
077200000000     C                   ENDIF
077300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
077400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
077500970623     C                   WRITE     SI60DVM                                      *SFL MESS
077600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
077700000000     C                   MOVEL     *BLANKS       $MSG
077800000000     C                   ENDSR
077900000000     C*------------------------------------------------------------------------*
078000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
078100000000     C*------------------------------------------------------------------------*
078200000000     C     OUTMSG        BEGSR
078300000000     C* VISUALIZZAZIONE MESSAGGIO
078400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
078500970623     C                   WRITE     SI60DVK                                      *CONTR MESS
078600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
078700000000     C                   Z-ADD     *ZEROS        MSGULT
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
079100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
079200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
079300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
079400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
079500000000     C*------------------------------------------------------------------------*
079600000000     C     CARFUN        BEGSR
079700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
079800000000     C     1             DO        31            I                 5 0          --- 1 -->
079900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
080000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
080100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
080200020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
080300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
080400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
080500000000     C                   ELSE                                                   *INTERROG.
080600000000     C                   MOVEL     FUNASI        FAT(I)
080700000000     C                   ENDIF
080800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
080900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
081000000000     C                   MOVEL     FUNAFG        FAT(I)
081100000000     C                   ELSE                                                   *INTERROG.
081200000000     C                   MOVEL     FUNAFI        FAT(I)
081300000000     C                   ENDIF
081400000000     C                   ENDIF                                                  <-- 2 ---
081500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
081600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
081700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
081800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
081900000000     C                   ELSE
082000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
082100000000     C                   ENDIF
082200000000     C                   ENDIF
082300000000     C                   ENDDO                                                  <-- 1 ---
082400000000     C                   ENDSR
082500000000     C*------------------------------------------------------------------------*
082600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
082700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
082800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
082900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
083000000000     C*------------------------------------------------------------------------*
083100000000     C     OUTFUN        BEGSR
083200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
083300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
083400000000     C                   ENDDO
083500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
083600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
083700000000     C     1             DO        31            I
083800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
083900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
084000000000     C                   ADD       1             FUNNAT
084100000000     C                   ENDIF
084200000000     C                   ENDDO
084300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
084400000000     C     FUNNAT        IFGT      8
084500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
084600000000     C                   ENDIF
084700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
084800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
084900000000     C                   Z-ADD     8             FUNVIS            5 0
085000000000     C     FAT(23)       IFEQ      'S'
085100000000     C                   SUB       1             FUNVIS
085200000000     C                   ENDIF
085300000000     C     FAT(24)       IFEQ      'S'
085400000000     C                   SUB       1             FUNVIS
085500000000     C                   ENDIF
085600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
085700000000     C                   Z-ADD     *ZEROS        W                 5 0
085800000000     C                   ADD       1             FUNULT
085900000000     C     FUNULT        IFGT      31
086000000000     C                   Z-ADD     1             FUNULT
086100000000     C                   ENDIF
086200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
086300000000     C     FUNULT        DO        31            I                              --- 1 -->
086400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
086500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
086600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
086700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
086800000000     C     FDE(I)        ANDNE     *BLANKS
086900000000     C     I             IFEQ      23                                           *TOLGO F23
087000000000     C     I             OREQ      24                                           *E F24
087100000000     C                   ELSE
087200000000     C                   ADD       1             W
087300000000     C                   MOVEL     FDE(I)        FUV(W)
087400000000     C                   ENDIF
087500000000     C                   ENDIF                                                  <-- 3 ---
087600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
087700000000     C                   Z-ADD     I             FUNULT
087800000000     C                   ENDIF                                                  <-- 2 ---
087900000000     C                   ENDDO                                                  <-- 1 ---
088000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
088100000000     C                   MOVEL     FUV(1)        VFD001
088200000000     C                   MOVEL     FUV(2)        VFD002
088300000000     C                   MOVEL     FUV(3)        VFD003
088400000000     C                   MOVEL     FUV(4)        VFD004
088500000000     C                   MOVEL     FUV(5)        VFD005
088600000000     C                   MOVEL     FUV(6)        VFD006
088700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
088800000000     C     FAT(24)       ANDEQ     'N'
088900000000     C                   MOVEL     FUV(7)        VFD007
089000000000     C                   MOVEL     FUV(8)        VFD008
089100000000     C                   ELSE                                                   <-- 1 -->
089200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
089300000000     C     FAT(24)       ANDEQ     'S'
089400000000     C                   MOVEL     FDE(23)       VFD007
089500000000     C                   MOVEL     FDE(24)       VFD008
089600000000     C                   ELSE                                                   <-- 2 -->
089700000000     C                   MOVEL     FUV(7)        VFD007
089800000000     C     FAT(23)       IFEQ      'S'
089900000000     C                   MOVEL     FDE(23)       VFD008
090000000000     C                   ELSE
090100000000     C                   MOVEL     FDE(24)       VFD008
090200000000     C                   ENDIF
090300000000     C                   ENDIF                                                  <-- 2 ---
090400000000     C                   ENDIF                                                  <-- 1 ---
090500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
090600000000     C                   ENDSR
090700000000     C*------------------------------------------------------------------------*
090800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
090900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
091000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
091100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
091200000000     C*------------------------------------------------------------------------*
091300000000     C     CONFUN        BEGSR
091400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
091500000000     C     1             DO        31            I                              --- 1 -->
091600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
091700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
091800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
091900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
092000000000     C                   EXSR      CARMSG
092100000000     C                   MOVEL     '1'           $ERR
092200000000     C                   ENDIF                                                  <-- 3 ---
092300000000     C                   ENDIF                                                  <-- 2 ---
092400000000     C                   ENDDO                                                  <-- 1 ---
092500000000     C                   ENDSR
092600000000     O*------------------------------------------------------------------------*
092700000000** MSG - (MESSAGGI)
092800970623TISI60R- Tasto funzionale non valido.                                          1
092900970623TISI60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
093000970623TISI60R- Opzione non valida.                                                   3
093100970623TISI60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
093200970623TISI60R- Inizio elenco.                                                        5
093300970623TISI60R- Fine elenco.                                                          6
093400020924TISI60R- Punto operativo errato o non in gestione.                             7
093500980210TISI60R- Tipo servizio errato                                                  8
093600050912TISI60R- Selezione ammessa SOLO per stampa analitica                           9
093700061120TISI60R- Lingua errata
093800980210TISI60R- Immettere uno dei valori possibili                                   11
093900090608TISI60R- Stampa"Sintetica" non selezionabile per nuova stampa servizio "E"    12
094000970623TISI60R- 13
094100970623TISI60R- 14
094200970623TISI60R- Stampa lanciata ...                                                  15
094300000000** CMD  (COMANDI)
094400000000WRKSPLF
094500000000
094600000000
094700000000
094800000000
094900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
095000000000F1    -                   -    NN     NN
095100000000F2    -                   -    NN     NN
095200000000F3    -F3=Fine            -    SS     SS
095300061120F4    -                   -    NN     NN
095400000000F5    -                   -    NN     NN
095500000000F6    -F6=Conferma        -    SS     SS
095600000000F7    -                   -    NN     NN
095700000000F8    -                   -    NN     NN
095800000000F9    -                   -    NN     NN
095900000000F10   -                   -    NN     NN
096000000000F11   -                   -    NN     NN
096100000000F12   -F12=Ritorno        -    SS     SS
096200000000F13   -                   -    NN     NN
096300000000F14   -                   -    NN     NN
096400000000F15   -                   -    NN     NN
096500000000F16   -                   -    NN     NN
096600000000F17   -                   -    NN     NN
096700000000F18   -                   -    NN     NN
096800000000F19   -                   -    NN     NN
096900000000F20   -                   -    NN     NN
097000000000F21   -                   -    NN     NN
097100000000F22   -F22=Gestione stampe-    EE     EE
097200000000F23   -F23=Altre opzioni  -    NN     NN
097300000000F24   -F24=Altri tasti    -    NN     NN
097400000000ENTER -                   -    SS     SS
097500000000ROLLUP-                   -    NN     NN
097600000000ROLLDN-                   -    NN     NN
097700000000HELP  -                   -    NN     NN
097800000000PRINT -                   -    NN     NN
097900000000BSPACE-                   -    NN     NN
098000000000CLEAR -                   -    NN     NN
