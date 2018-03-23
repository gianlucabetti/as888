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
011000090922     d
011100090922     d dvpodeco      e ds
011200090922     d Tibs02Ds      e ds
011300061120      *---------------------
011400061120      * VARIABILI
011500061120      *---------------------
011600061120     d P37iso2         s                   like(LINiso2)   inz
011700061120     d P37desi         s                   like(LINdesita) inz
011800000000     C*------------------------------------------------------------------------*
011900000000     C* MAIN LINE
012000000000     C*------------------------------------------------------------------------*
012100000000     C*
012200000000     C* CICLO FINO A CHE:
012300000000DO  1C     $FINE         DOUEQ     '1'
012400000000     C*
012500000000     C* CONTROLLA GESTIONE VIDEO
012600980210     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012700000000IF  2C     $GES          IFEQ      'V1'
012800000000     C                   EXSR      GESV1                                        *PARAMETRI
012900000000E   2C                   ENDIF
013000000000E   1C                   ENDDO
013100980210     C*
013200000000     C                   SETON                                        LR
013300000000     C*------------------------------------------------------------------------*
013400000000     C* GESV1 - GESTIONE VIDEO UNO
013500000000     C*------------------------------------------------------------------------*
013600000000     C     GESV1         BEGSR
013700000000     C*
013800000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013900000000     C                   EXSR      CARV1                                        *CARICA VIDEO
014000000000E   1C                   ENDIF
014100000000     C*
014200000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
014300000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014400000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014500000000E   1C                   ENDIF
014600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014700000000     C*
014800970623     C                   EXFMT     SI60DV1                                      *PARAMETRI
014900000000     C*
015000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
015100000000     C*
015200000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015300000000SL  2C                   SELECT
015400000000     C     KEY           WHENEQ    ENTER
015500000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015600000000     C     KEY           WHENEQ    F6
015700000000     C                   EXSR      F06GES                                       *CONFERMA
015800000000     C     KEY           WHENEQ    F22
015900000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
016000000000     C                   OTHER
016100000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
016200000000E   2C                   ENDSL
016300000000E   1C                   ENDIF
016400000000     C*
016500000000     C                   ENDSR
016600000000     C*------------------------------------------------------------------------*
016700000000     C* CARV1 - CARICA VIDEO UNO
016800000000     C*------------------------------------------------------------------------*
016900000000     C     CARV1         BEGSR
017000000000     C*
017100000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
017200000000     C*
017300000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017400000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017500000000     C*
017600970623     C                   WRITE     SI60DVT                                      *TESTATA
017700970623     C                   WRITE     SI60DVF                                      *FUNZIONI
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
018200000000     C*------------------------------------------------------------------------*
018300000000     C     ENTV1         BEGSR
018400000000     C*
018500000000     C* CONTROLLA I CAMPI IMMESSI
018600000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018700000000     C*
018800000000     C                   ENDSR
018900000000     C*------------------------------------------------------------------------*
019000000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
019100000000     C*------------------------------------------------------------------------*
019200000000     C     F06GES        BEGSR
019300000000     C*
019400000000     C* CONTROLLA I CAMPI IMMESSI
019500000000     C                   EXSR      DECODI
019600000000     C*
019700000000     C* LANCIA IL PROGRAMMA DI ESECUZIONE
019800000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019900000000     C                   MOVEL     V1CLNP        PARLNP                         *LINEA DI PARTENZA
020000000000     C                   MOVEL     V1CTEM        PARTEM                         *TEMPI DELIVERY
020100000000     C                   MOVEL     V1CTSP        PARTSP                         *TIPO SERVIZIO
020200090608     C                   MOVEL     V1Cold        PARold                         *TIPO SERVIZIO
020300980210     C                   MOVEL     V1CISO        PARISO                         *CONSIDERA ISOLE
020400980210     C                   MOVEL     V1CDIS        PARDIS                         *CONSIDERA L.DISAGG.
020500980210     C                   MOVEL     V1CSSA        PARSSA                         *SINTETICA/DETTAGLIO
020600000000     C                   MOVEL     V1CND1        PARND1                         *NOTA DELIVERY N°1
020700000000     C                   MOVEL     V1CND2        PARND2                         *NOTA DELIVERY N°2
020800000000     C                   MOVEL     V1CND3        PARND3                         *NOTA DELIVERY N°3
020900061120     c                   move      LINtabel      PARlin                         *NOTA DELIVERY N°3
021000000000     C*
021100970708     C                   MOVEL     'SI62'        KCOAZ
021200000000     C                   MOVEL     PARAM         KPJBU
021300091221     c
021400091221     c* Se si tratta di EDP dalla gaitra201 prima della data di attivazione per "H"
021500091221     c*  elaboro in interattivo da EDPES --> aggiungo in lista
021600100111     c**                 if        (knsif='GAITRA201 ' or knsif='GAITRAPSP') and
021700100111     c**                           *in15 and (v1ctsp='H' or v1ctsp='E')
021800091221     C* eseguo ADDLIBLE
021900100111     C**                 Z-ADD     80            LENGH            15 5
022000100111     C**                 CALL      'QCAEXEC'
022100100111     C**                 PARM                    CMD(2)
022200100111     C**                 PARM                    LENGH
022300091221
022400100111     c**                 call      'TISI62R'
022500100111     c**                 parm                    KPJBA
022600091221     C* eseguo RMVLIBLE
022700100111     C**                 Z-ADD     80            LENGH            15 5
022800100111     C**                 CALL      'QCAEXEC'
022900100111     C**                 PARM                    CMD(3)
023000100111     C**                 PARM                    LENGH
023100091221
023200100111     c**                 else
023300090619     C                   CALL      'BCH10'
023400090619     C                   PARM                    KPJBA
023500100111     c**                 endif
023600000000     C*
023700000000     C                   MOVEL     MSG(15)       $MSG
023800000000     C                   EXSR      CARMSG
023900000000E   1C                   ENDIF
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
024400000000     C*------------------------------------------------------------------------*
024500000000     C     F22GES        BEGSR
024600000000     C*
024700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
024800000000     C                   Z-ADD     80            LENGH            15 5
024900000000     C                   CALL      'QCAEXEC'
025000000000     C                   PARM                    CMD(1)
025100000000     C                   PARM                    LENGH
025200000000     C*
025300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025800000000     C*------------------------------------------------------------------------*
025900000000     C     DECODI        BEGSR
026000000000     C*
026100000000     C* REIMPOSTA INDICATORI DI ERRORE
026200000000     C                   SETOFF                                       606162
026300980210     C                   SETOFF                                       636465
026400061120     c                   setoff                                       66
026500000000     C*--------------------
026600000000     C* LINEA DI PARTENZA
026700000000     C*--------------------
026800000000IF  1C     $ERR          IFEQ      '0'
026900000000     C                   MOVEL     V1CLNP        WFIL
027000980210     C                   MOVEL     *BLANKS       WDFIL
027100980210     C                   EXSR      CHKFIL
027200000000     C                   MOVEL     WFIL          V1CLNP                         *CODICE
027300000000     C                   MOVEL     WDFIL         V1DLNP                         *DESCRIZIONE
027400000000IF  2C     $ERR          IFEQ      '1'
027500000000     C                   SETON                                        60
027600000000     C                   MOVEL     MSG(7)        $MSG
027700000000     C                   EXSR      CARMSG
027800000000E   2C                   ENDIF
027900000000E   1C                   ENDIF
028000000000     C*--------------------
028100000000     C* TEMPO DELIVERY
028200000000     C*--------------------
028300000000IF  1C     $ERR          IFEQ      '0'
028400000000IF  2C     V1CTEM        IFEQ      'I'
028500000000     C     V1CTEM        OREQ      'C'
028600000000X   2C                   ELSE
028700000000     C                   SETON                                        61
028800000000     C                   MOVEL     '1'           $ERR
028900980210     C                   MOVEL     MSG(11)       $MSG
029000000000     C                   EXSR      CARMSG
029100000000E   2C                   ENDIF
029200000000E   1C                   ENDIF
029300000000     C*--------------------
029400000000     C* TIPO SERVIZIO
029500000000     C*--------------------
029600000000IF  1C     $ERR          IFEQ      '0'
029700000000     C                   MOVEL     V1CTSP        WTSP
029800980210     C                   MOVEL     *BLANKS       WDTSP
029900000000     C                   EXSR      CHKTSP
030000000000     C                   MOVEL     WTSP          V1CTSP                         *CODICE
030100000000     C                   MOVEL     WDTSP         V1DTSP                         *DESCRIZIONE
030200000000IF  2C     $ERR          IFEQ      '1'
030300000000     C                   SETON                                        62
030400980210     C                   MOVEL     MSG(8)        $MSG
030500000000     C                   EXSR      CARMSG
030600000000E   2C                   ENDIF
030700000000E   1C                   ENDIF
030800090922     c*
030900090922     c* controllo se utilizzabile, non per EDP
031000090922IF  1C  n15$ERR          IFEQ      '0'
031100090922     c                   if        (v1ctsp='H' and datcor<§vpotsph)
031200090922     c                             or v1ctsp='P'
031300090922     C                   SETON                                        62
031400090922     C                   MOVEL     MSG(14)       $MSG
031500090922     C                   EXSR      CARMSG
031600090922     c                   endif
031700090922     c                   endif
031800090922     c
031900090608     c* non posso chidere stampa sintetica per tipo "E" a modo nuovo
032000090608IF  1C     $ERR          IFEQ      '0'
032100090608     c                   if        v1ctsp='E'  and v1cold=' 'and
032200090608     c                             v1cssa='S'
032300090608     C                   SETON                                        62
032400091204     c                   eval      $Err   = *on
032500090608     C                   MOVEL     MSG(12)       $MSG
032600090608     C                   EXSR      CARMSG
032700090608E   2C                   ENDIF
032800090608E   1C                   ENDIF
032900090922     c* sservizio H --> solo stampa modo nuovo
033000090922IF  1C     $ERR          IFEQ      '0'
033100090922     c                   if        v1ctsp='H'  and v1cssa='S'
033200090922     C                   SETON                                        62
033300090922     C                   MOVEL     MSG(13)       $MSG
033400090922     C                   EXSR      CARMSG
033500090922E   2C                   ENDIF
033600090922E   1C                   ENDIF
033700090608     c
033800061120      *--------------------
033900061120      * LINGUA
034000061120      *--------------------
034100061120if  1c                   if        $Err   = *off
034200061120     c                   clear                   V1Dlin
034300061120      * Ricerca
034400061120if  2c                   if        %scan('?':V1Clin) > *zeros
034500061120     c                   clear                   P37iso2
034600061120     c                   clear                   P37desi
034700061120     c                   call      'TRUL37R'
034800061120     c                   parm                    P37iso2
034900061120     c                   parm                    P37desi
035000061120     c                   eval      V1Clin = P37iso2
035100061120     c                   eval      V1Dlin = P37desi
035200091002     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
035300061120     c                   leavesr
035400061120e   2c                   endif
035500061120      * Controllo:
035600061120      * - lingua obbligatoria
035700061120if  2c                   if        V1Clin = *blanks
035800061120     c                   eval      $Err   = *on
035900061120     c                   eval      *in66  = *on
036000061120     c                   movel     Msg(10)       $Msg
036100061120     c                   exsr      CarMSG
036200061120     c                   leavesr
036300061120e   2c                   endif
036400061120      * - lingua errata
036500061120     c     V1Clin        chain     AZLIN000
036600061120if  2c                   if        not %found(AZLIN03L)
036700061120     c                   eval      $Err   = *on
036800061120     c                   eval      *in66  = *on
036900061120     c                   movel     Msg(10)       $Msg
037000061120     c                   exsr      CarMSG
037100061120     c                   leavesr
037200061120e   2c                   endif
037300061120     c                   eval      V1Dlin = LINdesita
037400061120      * - se non è attiva per i testi errore
037500061120if  2c                   if        LINfntxt = '0'
037600061120     c                   eval      $Err   = *on
037700061120     c                   eval      *in66  = *on
037800061120     c                   movel     Msg(10)       $Msg
037900061120     c                   exsr      CarMSG
038000061120     c                   leavesr
038100061120e   2c                   endif
038200061120      * - se ho scelto di stampare in lingua il testo deve essere
038300061120      *   gestito in lingua
038400061120if  2c                   if            V1Clin <> 'it'
038500061120     c                             and §1Hlin = *blanks
038600061120     c                   eval      $Err   = *on
038700061120     c                   eval      *in66  = *on
038800061120     c                   movel     Msg(10)       $Msg
038900061120     c                   exsr      CarMSG
039000061120     c                   leavesr
039100061120e   2c                   endif
039200061120e   1c                   ENDIF
039300000000     C*--------------------
039400980216     C* CAP/LOCALITA' ISOLA
039500000000     C*--------------------
039600000000IF  1C     $ERR          IFEQ      '0'
039700000000IF  2C     V1CISO        IFEQ      'S'
039800000000     C     V1CISO        OREQ      'N'
039900000000X   2C                   ELSE
040000000000     C                   SETON                                        63
040100000000     C                   MOVEL     '1'           $ERR
040200000000     C                   MOVEL     MSG(11)       $MSG
040300000000     C                   EXSR      CARMSG
040400000000E   2C                   ENDIF
040500000000E   1C                   ENDIF
040600980210     C*--------------------
040700980216     C* CAP/LOCALITA' DISAGIATE
040800980210     C*--------------------
040900980210IF  1C     $ERR          IFEQ      '0'
041000980210IF  2C     V1CDIS        IFEQ      'S'
041100980210     C     V1CDIS        OREQ      'N'
041200980210X   2C                   ELSE
041300980210     C                   SETON                                        64
041400980210     C                   MOVEL     '1'           $ERR
041500980210     C                   MOVEL     MSG(11)       $MSG
041600980210     C                   EXSR      CARMSG
041700980210E   2C                   ENDIF
041800980210E   1C                   ENDIF
041900980210     C*--------------------
042000980210     C* STAMPA SINTETICA / ANALITICA
042100980210     C*--------------------
042200980210IF  1C     $ERR          IFEQ      '0'
042300980210IF  2C     V1CSSA        IFEQ      'S'
042400980210     C     V1CSSA        OREQ      'A'
042500980210X   2C                   ELSE
042600980210     C                   SETON                                        65
042700980210     C                   MOVEL     '1'           $ERR
042800980210     C                   MOVEL     MSG(11)       $MSG
042900980210     C                   EXSR      CARMSG
043000980210E   2C                   ENDIF
043100980210E   1C                   ENDIF
043200050912     C*--------------------
043300050912     C* CAP/LOCALITA' ISOLA & DISAGIATE
043400050912     C*--------------------
043500050912if  1C                   if             $Err   = *off
043600050912     C                             and  V1Cssa = 'S'
043700050912     C                             and (V1Ciso = 'S'
043800050912     C                              or  V1Cdis = 'S')
043900050912sel 2c                   select
044000050912w   2C                   when      V1Ciso = 'S'
044100050912     C                   eval      *in63 = *on
044200050912w   2C                   when      V1Cdis = 'S'
044300050912     C                   eval      *in64 = *on
044400050912e   2c                   endsl
044500050912     C                   eval      $Err  = *on
044600050912     C                   movel     Msg(9)        $Msg
044700050912     C                   exsr      CarMSG
044800050912E   1C                   endif
044900000000     C*
045000000000     C                   ENDSR
045100000000     C*------------------------------------------------------------------------*
045200980210     C* CHKFIL - CONTROLLA LA FILIALE
045300000000     C*------------------------------------------------------------------------*
045400980210     C     CHKFIL        BEGSR
045500000000     C*
045600000000     C     *LIKE         DEFINE    V1CLNP        WFIL
045700000000     C     *LIKE         DEFINE    V1DLNP        WDFIL
045800000000     C*
045900980210     C* RICHIESTO ELENCO (?)
046000000000     C                   SETOFF                                           28
046100000000     C     '?'           SCAN      WFIL                                   28
046200000000IF  1C     *IN28         IFEQ      *ON
046300000000     C                   MOVEL     *BLANKS       §COD1
046400980210     C                   MOVEL     *BLANKS       §DES1
046500020806     C                   MOVEL     *BLANKS       §PRA
046600971203     C                   CALL      'TNSD24R'
046700000000     C                   PARM                    §COD1             3
046800000000     C                   PARM                    §TIP              1
046900000000     C                   PARM                    §DES1            25
047000971203     C                   PARM                    §PRA              3
047100000000     C                   MOVEL     §COD1         WFIL                           *CODICE
047200000000     C                   MOVEL     §DES1         WDFIL                          *DESCRIZIONE
047300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
047400000000E   1C                   ENDIF
047500000000     C*
047600980210     C* CONTROLLA SOLO CARATTERI NUMERICI
047700000000     C                   SETOFF                                       28
047800000000     C                   TESTN                   WFIL                 28
047900000000     C                   MOVE      WFIL          WA1               1
048000000000IF  1C     *IN28         IFEQ      *OFF                                         *CARATTERI ALFABETIC
048100000000     C     WA1           ORLT      '0'                                          *ULTIMO CARATTERE AL
048200980210     C                   MOVEL     '1'           $ERR                           *ERRORE
048300000000X   1C                   ELSE
048400980210     C*
048500980210     C* CONTROLLA ESISTEZA RECORD
048600000000     C                   MOVEL     WFIL          KORFIL
048700000000     C     KORFIL        CHAIN     AZORG01L                           99
048800980210IF  2C     *IN99         IFEQ      *OFF                                         *ESISTENTE
048900980210     C     ORGFVA        ANDEQ     *BLANKS                                      *NO ANNULLATA
049000980210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
049100000000     C                   MOVEL     ORGDES        WDFIL
049200980210X   2C                   ELSE
049300000000     C                   MOVEL     '1'           $ERR
049400980210E   2C                   ENDIF
049500000000E   1C                   ENDIF
049600020924     C*
049700020924     C* CONTROLLA SE FILIALE IN GESTIONE
049800020924     C                   MOVEL     WFIL          WNUMFIL           3 0
049900020924     C     WNUMFIL       LOOKUP    L1                                     20
050000020924     C                   IF        *IN20 = *ON
050100020924     C                   ELSE
050200020924     C                   MOVEL     '1'           $ERR
050300020924     C                   ENDIF
050400000000     C*
050500000000     C                   ENDSR
050600000000     C*------------------------------------------------------------------------*
050700000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
050800000000     C*------------------------------------------------------------------------*
050900000000     C     CHKTSP        BEGSR
051000000000     C*
051100000000     C     *LIKE         DEFINE    V1CTSP        WTSP
051200000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
051300000000     C*
051400980210     C* RICHIESTO ELENCO (?)
051500000000IF  1C     WTSP          IFEQ      '?'
051600020919     C                   MOVEL     1             §KUT
051700000000     C                   MOVEL     '5E'          §COD
051800000000     C                   MOVEL     *BLANKS       §KEY
051900000000     C                   MOVEL     *BLANKS       §DES
052000000000     C                   CALL      'X§TABER'
052100000000     C                   PARM                    §KUT              1 0
052200000000     C                   PARM                    §COD              2
052300000000     C                   PARM                    §KEY              8
052400000000     C                   PARM                    §DES             30
052500000000     C                   MOVEL     §KEY          WTSP
052600000000     C                   MOVEL     §DES          WDTSP
052700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
052800000000E   1C                   ENDIF
052900000000     C*
053000090922     C* CONTROLLA ESISTENZA RECORD
053100020919     C                   MOVEL     1             KTBKUT
053200000000     C                   MOVEL     '5E'          KTBCOD
053300000000     C                   MOVEL(P)  WTSP          KTBKEY
053400000000     C     KEYTAB        CHAIN     TABEL00F                           99
053500000000IF  1C     *IN99         IFEQ      *OFF                                         *ESISTENTE
053600000000     C     TBLFLG        ANDEQ     *BLANKS                                      *NON ANNULLATO
053700000000     C                   MOVEL     TBLUNI        WDTSP
053800000000X   1C                   ELSE
053900000000     C                   MOVEL     '1'           $ERR
054000000000E   1C                   ENDIF
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
054500000000     C*------------------------------------------------------------------------*
054600000000     C     IMPVID        BEGSR
054700000000     C*
054800020919     C                   MOVEL     DUTPOU        V1CLNP                         *LINEA DI PARTENZA
054900000000     C                   MOVEL     'C'           V1CTEM                         *TEMPI DELIVERY
055000960702     C                   MOVEL     'E'           V1CTSP                         *TIPO SERVIZIO
055100980210     C                   MOVEL     'N'           V1CISO                         *CONSIDERA ISOLE
055200980210     C                   MOVEL     'N'           V1CDIS                         *CONSIDERA L.DISAG.
055300980210     C                   MOVEL     'S'           V1CSSA                         *SINTETICA/ANALITICA
055400090608     C                   MOVEL     ' '           V1Cold                         *SINTETICA/ANALITICA
055500061120     c                   eval      V1Clin = 'it'
055600061120      * decodifico la lingua da azlin
055700061120     c     V1Clin        chain     AZLIN000
055800061120     c                   if        %found(AZLIN03L)
055900061120     c                   eval      V1Dlin = LINdesita
056000061120     c                   endif
056100000000     C*
056200000000     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400000000     C* CARTAB - CARICA LE TABELLE
056500000000     C*------------------------------------------------------------------------*
056600000000     C     CARTAB        BEGSR
056700020924     C*
056800020924     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
056900020924     C                   CLEAR                   L1
057000020924     C                   CLEAR                   TRUL06DS
057100020924     C                   CLEAR                   LIA
057200020924     C                   CLEAR                   TRUL26DS
057300020924     C                   CLEAR                   SKPO
057400020924     C                   CLEAR                   SKPOT
057500020924     C*
057600020924     C* CARICA LE FILIALI IN GESTIONE
057700020924     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
057800020924     C                   EVAL      DEPKPJBU = KPJBU
057900020924     C                   MOVEL     'L'           D06TLA
058000020924     C                   MOVEL     '£1'          D06COD
058100021007     C                   MOVEL     duttfp        D06KEY
058200020924     C                   MOVEL     TRUL06DS      KPJBU
058300020924     C                   CALL      'TRUL06R'
058400020924     C                   PARM                    KPJBA
058500020924     C                   MOVEL     KPJBU         TRUL06DS
058600020924     C                   MOVEA     LIA           L1                              -P.O. terminal
058700020924     C                   EVAL      KPJBU = DEPKPJBU
058800020924     C                   ENDIF
058900020924     C*
059000020924     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
059100020924     C                   MOVEL     DUTPOU        L1(1)                           -P.O. utente
059200020924     C                   ENDIF
059300020924     C*
059400020924     C                   IF        DUTLPO = 'S'                                 *SEDE
059500020924     C                   EVAL      I26TLA = 'L'
059600020924     C                   EVAL      I26TPT = 'P'
059700020924     C                   CALL      'TRUL26R'
059800020924     C                   PARM                    TRUL26DS
059900020924     C                   MOVEA     SKPO          L1                              -tutti i P.O.
060000020924     C                   ENDIF
060100061120      *
060200061120      * reperisco tabella 1H
060300061120     c                   clear                   ds1H
060400061120     c                   eval      KTBkut = 1
060500061120     c                   eval      KTBcod = '1H'
060600061120     c                   eval      KTBKEY = '37      '
060700061120     c     KEYTAB        chain     TABEL
060800061120if  1c                   if             %found(TABEL00F)                        *ESISTENTE
060900061120     c                             and  TBLflg = *blanks                        *NON ANNULLATO
061000061120     c                   movel     TBLuni        ds1H
061100061120e   1c                   endif
061200000000     C*
061300000000     C                   ENDSR
061400020919     C*--------------------------------------------------------------------------------------------*
061500020919     C* REPERISCE I DATI UTENTE
061600020919     C*--------------------------------------------------------------------------------------------*
061700020919     C     REPDATIUTE    BEGSR
061800020919     C*
061900020919     C* INIZIALIZZA VARIABILI DI WRK
062000020919     C                   CLEAR                   TIBS34DS
062100020919     C                   CLEAR                   AZUTEDS
062200020919     C                   CLEAR                   DDATIUTE
062300020919     C*
062400020919     C     *DTAARA       DEFINE    §azute        azuteds
062500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
062600020919     C                   IN(E)     *DTAARA
062700020919if  1C                   IF        %Error
062800020919     c                   EVAL      I34Tla = 'L'
062900020919     C                   CALL      'TIBS34R'
063000020919     C                   PARM                    Tibs34Ds
063100020919     C                   IN        *DTAARA
063200020919e   1C                   ENDIF
063300020919     C*
063400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
063500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
063600020919     C*
063700020919     C                   ENDSR
063800000000     C*------------------------------------------------------------------------*
063900000000     C* *INZSR - ROUTINE INIZIALE
064000000000     C*------------------------------------------------------------------------*
064100000000     C     *INZSR        BEGSR
064200980210     C*---
064300000000     C* RICEVIMENTO PARAMETRI
064400980210     C*---
064500000000     C     *ENTRY        PLIST
064600000000     C                   PARM                    KPJBA
064700980210     C*---
064800000000     C* VARIABILI RIFERITE AL DATA BASE
064900980210     C*---
065000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
065100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
065200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
065300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
065400020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
065500980210     C*---
065600000000     C* CHIAVI DI LETTURA
065700980210     C*---
065800980210     C     KEYTAB        KLIST                                                  *TABEL00F
065900980210     C                   KFLD                    KTBKUT                          -UTENTE
066000980210     C                   KFLD                    KTBCOD                          -TABELLA
066100980210     C                   KFLD                    KTBKEY                          -CHIAVE
066200980210     C     KE2TAB        KLIST                                                  *TABEL00F
066300980210     C                   KFLD                    KTBKUT                          -UTENTE
066400980210     C                   KFLD                    KTBCOD                          -TABELLA
066500980210     C*---
066600000000     C* VARIABILI DI CONTROLLO
066700980210     C*---
066800000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
066900090608     c* Utente EDP
067000090608     c                   if        %subst(knmus:1:3)='EDP'
067100090608     c                   seton                                        15
067200090608     c                   endif
067300090922     c*
067400090922     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
067500090922     c* per stamparlo
067600090922     c                   clear                   tibs02ds
067700090922     c                   clear                   dvpodeco
067800090922     c                   eval      t02mod = 'C'
067900090922     c                   eval      t02sif = knsif
068000090922     c                   eval      t02cod = 'VPO'
068100090922     c                   eval      t02ke1 = 'DECO'
068200090922     c                   call      'TIBS02R'
068300090922     c                   parm                    kpjba
068400090922     c                   parm                    tibs02ds
068500090922     c                   if        t02err = *blanks
068600090922     c                   eval      dvpodeco = t02uni
068700090922     c                   else
068800090922     c                   eval      §vpotsph=20091001
068900090922     c                   endif
069000090922
069100000000     C*
069200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
069300980210     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
069400000000     C                   EXSR      INZSTD
069500980210     C*
069600980210     C* CALCOLA LA DATA CORRENTE
069700980210     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
069800980210     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
069900980210     C                   Z-ADD     WN8           G08DAT
070000980210     C                   Z-ADD     *ZEROS        G08INV
070100980210     C                   MOVEL     '0'           G08ERR
070200980210     C                   CALL      'XSRDA8'
070300980210     C                   PARM                    WLBDA8
070400980210     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
070500091221     c* Sse attivo tipo servizio "H" --> faccio vedere la richeista della lingua
070600000000     C*
070700980210     C* CARICA TABELLE OCCORRENTI
070800000000     C                   EXSR      CARTAB
070900000000     C*
071000000000     C* IMPOSTAZIONI VIDEO
071100000000     C                   EXSR      IMPVID
071200000000     C*
071300000000     C* EMISSIONE VIDEO
071400000000     C                   EXSR      CARV1
071500000000     C*
071600000000     C                   ENDSR
071700000000     C*------------------------------------------------------------------------*
071800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072000000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
072100000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
072200000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
072300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072500000000     C*------------------------------------------------------------------------*
072600000000     C* INZSTD - ROUTINE INIZIALE STANDARD
072700000000     C*------------------------------------------------------------------------*
072800000000     C     INZSTD        BEGSR
072900000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
073000000000     C                   CALL      'XKEYS'
073100000000     C                   PARM                    DSKEYS
073200000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
073300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
073400000000     C     XMSGPA        PLIST
073500000000     C                   PARM                    DSMSMS
073600000000     C                   PARM                    DSMSPG
073700000000     C                   PARM                    DSMSKE
073800000000     C                   PARM                    DSMSER
073900000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
074000970623     C                   MOVEL     'TISI60R'     DSMSPG                         *NOME PGM CHIAMANTE
074100970623     C                   MOVEL     'TISI60R'     VMCQUE                         *CODA MESSAGGI DESTI
074200000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
074300000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
074400000000     C* INIZIALIZZO FLAG CONTROLLO PGM
074500000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
074600000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
074700000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
074800000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
074900000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
075000000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
075100000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
075200000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
075300000000     C* LANCIO ROUTINE PER VERIFICARE SE
075400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
075500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
075600020919     C                   EXSR      REPDATIUTE
075700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
075800000000     C                   BITOFF    '01234567'    POSMI1            1
075900000000     C                   BITOFF    '01234567'    POSMI2            1
076000000000     C                   BITOFF    '01234567'    POSMA1            1
076100000000     C                   BITOFF    '01234567'    POSMA2            1
076200000000     C                   BITON     '34'          POSMI1            1            *18
076300000000     C                   BITON     '7'           POSMI2            1            *01
076400000000     C                   BITON     '34'          POSMA1            1            *18
076500000000     C                   BITON     '0'           POSMA2            1            *80
076600000000     C                   MOVE      POSMI2        POSMIN            2
076700000000     C                   MOVEL     POSMI1        POSMIN
076800000000     C                   MOVE      POSMA2        POSMAX            2
076900000000     C                   MOVEL     POSMA1        POSMAX
077000000000     C                   ENDSR
077100000000     C*------------------------------------------------------------------------*
077200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
077300000000     C*------------------------------------------------------------------------*
077400000000     C     FUNSTD        BEGSR
077500000000     C                   SELECT
077600000000     C     KEY           WHENEQ    F3
077700000000     C                   EXSR      F03STD
077800000000     C     KEY           WHENEQ    F12
077900000000     C                   EXSR      F12STD
078000000000     C     KEY           WHENEQ    F24
078100000000     C                   EXSR      F24STD
078200000000     C                   OTHER
078300000000     C                   MOVEL     MSG(2)        $MSG
078400000000     C                   EXSR      CARMSG
078500000000     C                   ENDSL
078600000000     C                   ENDSR
078700000000     C*------------------------------------------------------------------------*
078800000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
078900000000     C*------------------------------------------------------------------------*
079000000000     C     F03STD        BEGSR
079100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
079200000000     C                   MOVEL     '1'           $F03
079300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
079400000000     C                   MOVEL     '1'           $FINE
079500000000     C                   ENDSR
079600000000     C*------------------------------------------------------------------------*
079700000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
079800000000     C*------------------------------------------------------------------------*
079900000000     C     F12STD        BEGSR
080000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
080100000000     C                   MOVEL     '1'           $F12
080200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
080300000000     C                   MOVEL     '1'           $FINE
080400000000     C                   ENDSR
080500000000     C*------------------------------------------------------------------------*
080600000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
080700000000     C*------------------------------------------------------------------------*
080800000000     C     F24STD        BEGSR
080900000000     C*
081000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
081100000000     C                   EXSR      OUTFUN
081200000000     C*
081300000000     C                   ENDSR
081400000000     C*------------------------------------------------------------------------*
081500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
081600000000     C*          CODA MESSAGGI DEL PGM
081700000000     C*------------------------------------------------------------------------*
081800000000     C     CARMSG        BEGSR
081900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
082000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
082100000000     C     MSGULT        IFEQ      0
082200000000     C                   MOVEL     '1'           *IN40
082300970623     C                   WRITE     SI60DVK
082400000000     C                   MOVEL     '0'           *IN40
082500000000     C                   ENDIF
082600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
082700000000     C                   MOVEL     $MSG          DSMSMS
082800000000     C                   CALL      'XMSG'        XMSGPA
082900000000     C     DSMSER        IFEQ      0
083000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
083100000000     C                   ENDIF
083200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
083300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
083400970623     C                   WRITE     SI60DVM                                      *SFL MESS
083500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
083600000000     C                   MOVEL     *BLANKS       $MSG
083700000000     C                   ENDSR
083800000000     C*------------------------------------------------------------------------*
083900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
084000000000     C*------------------------------------------------------------------------*
084100000000     C     OUTMSG        BEGSR
084200000000     C* VISUALIZZAZIONE MESSAGGIO
084300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
084400970623     C                   WRITE     SI60DVK                                      *CONTR MESS
084500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
084600000000     C                   Z-ADD     *ZEROS        MSGULT
084700000000     C                   ENDSR
084800000000     C*------------------------------------------------------------------------*
084900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
085000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
085100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
085200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
085300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
085400000000     C*------------------------------------------------------------------------*
085500000000     C     CARFUN        BEGSR
085600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
085700000000     C     1             DO        31            I                 5 0          --- 1 -->
085800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
085900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
086000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
086100020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
086200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
086300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
086400000000     C                   ELSE                                                   *INTERROG.
086500000000     C                   MOVEL     FUNASI        FAT(I)
086600000000     C                   ENDIF
086700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
086800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
086900000000     C                   MOVEL     FUNAFG        FAT(I)
087000000000     C                   ELSE                                                   *INTERROG.
087100000000     C                   MOVEL     FUNAFI        FAT(I)
087200000000     C                   ENDIF
087300000000     C                   ENDIF                                                  <-- 2 ---
087400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
087500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
087600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
087700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
087800000000     C                   ELSE
087900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
088000000000     C                   ENDIF
088100000000     C                   ENDIF
088200000000     C                   ENDDO                                                  <-- 1 ---
088300000000     C                   ENDSR
088400000000     C*------------------------------------------------------------------------*
088500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
088600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
088700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
088800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
088900000000     C*------------------------------------------------------------------------*
089000000000     C     OUTFUN        BEGSR
089100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
089200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
089300000000     C                   ENDDO
089400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
089500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
089600000000     C     1             DO        31            I
089700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
089800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
089900000000     C                   ADD       1             FUNNAT
090000000000     C                   ENDIF
090100000000     C                   ENDDO
090200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
090300000000     C     FUNNAT        IFGT      8
090400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
090500000000     C                   ENDIF
090600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
090700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
090800000000     C                   Z-ADD     8             FUNVIS            5 0
090900000000     C     FAT(23)       IFEQ      'S'
091000000000     C                   SUB       1             FUNVIS
091100000000     C                   ENDIF
091200000000     C     FAT(24)       IFEQ      'S'
091300000000     C                   SUB       1             FUNVIS
091400000000     C                   ENDIF
091500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
091600000000     C                   Z-ADD     *ZEROS        W                 5 0
091700000000     C                   ADD       1             FUNULT
091800000000     C     FUNULT        IFGT      31
091900000000     C                   Z-ADD     1             FUNULT
092000000000     C                   ENDIF
092100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
092200000000     C     FUNULT        DO        31            I                              --- 1 -->
092300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
092400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
092500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
092600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
092700000000     C     FDE(I)        ANDNE     *BLANKS
092800000000     C     I             IFEQ      23                                           *TOLGO F23
092900000000     C     I             OREQ      24                                           *E F24
093000000000     C                   ELSE
093100000000     C                   ADD       1             W
093200000000     C                   MOVEL     FDE(I)        FUV(W)
093300000000     C                   ENDIF
093400000000     C                   ENDIF                                                  <-- 3 ---
093500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
093600000000     C                   Z-ADD     I             FUNULT
093700000000     C                   ENDIF                                                  <-- 2 ---
093800000000     C                   ENDDO                                                  <-- 1 ---
093900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
094000000000     C                   MOVEL     FUV(1)        VFD001
094100000000     C                   MOVEL     FUV(2)        VFD002
094200000000     C                   MOVEL     FUV(3)        VFD003
094300000000     C                   MOVEL     FUV(4)        VFD004
094400000000     C                   MOVEL     FUV(5)        VFD005
094500000000     C                   MOVEL     FUV(6)        VFD006
094600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
094700000000     C     FAT(24)       ANDEQ     'N'
094800000000     C                   MOVEL     FUV(7)        VFD007
094900000000     C                   MOVEL     FUV(8)        VFD008
095000000000     C                   ELSE                                                   <-- 1 -->
095100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
095200000000     C     FAT(24)       ANDEQ     'S'
095300000000     C                   MOVEL     FDE(23)       VFD007
095400000000     C                   MOVEL     FDE(24)       VFD008
095500000000     C                   ELSE                                                   <-- 2 -->
095600000000     C                   MOVEL     FUV(7)        VFD007
095700000000     C     FAT(23)       IFEQ      'S'
095800000000     C                   MOVEL     FDE(23)       VFD008
095900000000     C                   ELSE
096000000000     C                   MOVEL     FDE(24)       VFD008
096100000000     C                   ENDIF
096200000000     C                   ENDIF                                                  <-- 2 ---
096300000000     C                   ENDIF                                                  <-- 1 ---
096400000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
096500000000     C                   ENDSR
096600000000     C*------------------------------------------------------------------------*
096700000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
096800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
096900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
097000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
097100000000     C*------------------------------------------------------------------------*
097200000000     C     CONFUN        BEGSR
097300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
097400000000     C     1             DO        31            I                              --- 1 -->
097500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
097600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
097700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
097800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
097900000000     C                   EXSR      CARMSG
098000000000     C                   MOVEL     '1'           $ERR
098100000000     C                   ENDIF                                                  <-- 3 ---
098200000000     C                   ENDIF                                                  <-- 2 ---
098300000000     C                   ENDDO                                                  <-- 1 ---
098400000000     C                   ENDSR
098500000000     O*------------------------------------------------------------------------*
098600000000** MSG - (MESSAGGI)
098700970623TISI60R- Tasto funzionale non valido.                                          1
098800970623TISI60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
098900970623TISI60R- Opzione non valida.                                                   3
099000970623TISI60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
099100970623TISI60R- Inizio elenco.                                                        5
099200970623TISI60R- Fine elenco.                                                          6
099300020924TISI60R- Punto operativo errato o non in gestione.                             7
099400980210TISI60R- Tipo servizio errato                                                  8
099500050912TISI60R- Selezione ammessa SOLO per stampa analitica                           9
099600061120TISI60R- Lingua errata
099700980210TISI60R- Immettere uno dei valori possibili                                   11
099800090608TISI60R- Stampa"Sintetica" non selezionabile per nuova stampa servizio "E"    12
099900090922TISI60R- Stampa"Sintetica" non selezionabile per nuova stampa servizio "H"    13
100000090922TISI60R- Tipo servizio non utilizzabile                                       14
100100970623TISI60R- Stampa lanciata ...                                                  15
100200000000** CMD  (COMANDI)
100300000000WRKSPLF
100400091221ADDLIBLE EDPESTISI *FIRST
100500091221RMVLIBLE EDPESTISI
100600000000
100700000000
100800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
100900000000F1    -                   -    NN     NN
101000000000F2    -                   -    NN     NN
101100000000F3    -F3=Fine            -    SS     SS
101200061120F4    -                   -    NN     NN
101300000000F5    -                   -    NN     NN
101400000000F6    -F6=Conferma        -    SS     SS
101500000000F7    -                   -    NN     NN
101600000000F8    -                   -    NN     NN
101700000000F9    -                   -    NN     NN
101800000000F10   -                   -    NN     NN
101900000000F11   -                   -    NN     NN
102000000000F12   -F12=Ritorno        -    SS     SS
102100000000F13   -                   -    NN     NN
102200000000F14   -                   -    NN     NN
102300000000F15   -                   -    NN     NN
102400000000F16   -                   -    NN     NN
102500000000F17   -                   -    NN     NN
102600000000F18   -                   -    NN     NN
102700000000F19   -                   -    NN     NN
102800000000F20   -                   -    NN     NN
102900000000F21   -                   -    NN     NN
103000000000F22   -F22=Gestione stampe-    EE     EE
103100000000F23   -F23=Altre opzioni  -    NN     NN
103200000000F24   -F24=Altri tasti    -    NN     NN
103300000000ENTER -                   -    SS     SS
103400000000ROLLUP-                   -    NN     NN
103500000000ROLLDN-                   -    NN     NN
103600000000HELP  -                   -    NN     NN
103700000000PRINT -                   -    NN     NN
103800000000BSPACE-                   -    NN     NN
103900000000CLEAR -                   -    NN     NN
