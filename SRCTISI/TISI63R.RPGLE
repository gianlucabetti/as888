000100051117      *------------------------------------------------------------------------*
000200051117      * STAMPA PICKUP   TIME - PARAMETRI
000300051117      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500051117      *------------------------------------------------------------------------*
000600051117      * DATA BASE
000700051117      *------------------------------------------------------------------------*
000800010329     FTISI63D   CF   E             WORKSTN INFDS(DEVDS1)
000900010329     F                                     SFILE(SI63DVM:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100051117      *------------------------------------------------------------------------*
001200051117      * SCHIERE
001300051117      *------------------------------------------------------------------------*
001400051117      *-------------------
001500051117      * MESSAGGI, COMANDI E FUNZIONI
001600051117      *-------------------
001700010509     D REG             S             75    DIM(3) CTDATA PERRCD(1)              *REGOLE
001800000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
001900000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100051117      *-------------------
002200051117      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300051117      *-------------------
002400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700051117      *-------------------
002800051117      * TABELLA DESCRIZIONI A VIDEO
002900051117      *-------------------
003000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100051117      *-------------------
003200051117      * TABELLA TASTI FUNZIONALI PREMUTI
003300051117      *-------------------
003400000000     D KEYS            S              1    DIM(40)
003500051117      *------------------------------------------------------------------------*
003600051117      * INPUT
003700051117      *------------------------------------------------------------------------*
003800040618     d wora            ds
003900040618     d  wore                   1      2
004000040618     d  wmin                   3      4
004100051117      *---------------------
004200051117      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300051117      *---------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600051117      *---------------------
004700051117      * DS PER GESTIONE TASTI FUNZIONALI
004800051117      *---------------------
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100051117      *---------------------
005200051117      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300051117      *---------------------
005400000000     D                SDS
005500000000     D  USER                 254    263
005600051117      *---------------------
005700051117      * RIDEFINIZIONE INFDS DEL FILE VIDEO
005800051117      *---------------------
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D  CURSOR               370    371
006200051117      *---------------------
006300051117      * DS "XSRDA8" - CONTROLLA DATA (8)
006400051117      *---------------------
006500980210     D WLBDA8          DS                  INZ
006600980210     D  G08DAT                 1      8  0
006700980210     D  G08INV                 9     16  0
006800980210     D  G08ERR                17     17
006900980210     D  G08TGI                18     22  0
007000051117      *---------------------
007100051117      * DS "XMSG" - GESTIONE MESSAGGI AL PGM
007200051117      *---------------------
007300000000     D DSMSG         E DS
007400051117      *---------------------
007500051117      * DS REPERIMENTO DATI UTENTE
007600051117      *---------------------
007700020919     D TIBS34DS      E DS                                                       *Profili utente
007800020919     D DDATIUTE      E DS                                                       *Dati utente
007900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008000051117      *---------------------
008100051117      * DS PARAMETRI DI LANCIO
008200051117      *------------------
008300000000     D PARAM           DS                  INZ
008400000000     D  PARLNP                 1      3
008500000000     D  PARTEM                 4      4
008600000000     D  PARTSP                 5      5
008700000000     D  PARISO                 6      6
008800980210     D  PARDIS                 7      7
008900980210     D  PARSSA                 8      8
009000980210     D  PARND1                 9     83
009100980210     D  PARND2                84    158
009200980210     D  PARND3               159    233
009300010509     D  PARORE               234    235  0
009400010509     D  PARRD1               236    310
009500010509     D  PARRD2               311    385
009600010509     D  PARRD3               386    460
009700011016     D  PARPRI               461    461
009800051117      *---------------------
009900051117      * ARCHITETTURA
010000051117      *---------------------
010100980210     D KPJBA         E DS
010200051117      *-------------------
010300051117      * DS REPERIMENTO TABELLA
010400051117      *-------------------
010500040618     D Tibs02ds      e ds
010600051117      *-------------------
010700051117      * DS TABELLA DFT
010800051117      *-------------------
010900040618     D dDft          e ds
011000051117      *------------------------------------------------------------------------*
011100051117      * MAIN LINE
011200051117      *------------------------------------------------------------------------*
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000DO  1C     $FINE         DOUEQ     '1'
011600000000     C*
011700000000     C* CONTROLLA GESTIONE VIDEO
011800980210     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011900000000IF  2C     $GES          IFEQ      'V1'
012000000000     C                   EXSR      GESV1                                        *PARAMETRI
012100000000E   2C                   ENDIF
012200000000E   1C                   ENDDO
012300980210     C*
012400000000     C                   SETON                                        LR
012500051117      *------------------------------------------------------------------------*
012600051117      * GESV1 - GESTIONE VIDEO UNO
012700051117      *------------------------------------------------------------------------*
012800000000     C     GESV1         BEGSR
012900000000     C*
013000000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013100000000     C                   EXSR      CARV1                                        *CARICA VIDEO
013200000000E   1C                   ENDIF
013300000000     C*
013400000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
013500000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013600000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013700000000E   1C                   ENDIF
013800000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013900000000     C*
014000010329     C                   EXFMT     SI63DV1                                      *PARAMETRI
014100000000     C*
014200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
014300000000     C*
014400000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014500000000SL  2C                   SELECT
014600000000     C     KEY           WHENEQ    ENTER
014700051117     ***C                   EXSR      ENTV1                                     *GEST.ENTER VIDEO V1
014800000000     C     KEY           WHENEQ    F6
014900000000     C                   EXSR      F06GES                                       *CONFERMA
015000000000     C     KEY           WHENEQ    F22
015100000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
015200000000     C                   OTHER
015300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015400000000E   2C                   ENDSL
015500000000E   1C                   ENDIF
015600000000     C*
015700000000     C                   ENDSR
015800051117      *------------------------------------------------------------------------*
015900051117      * CARV1 - CARICA VIDEO UNO
016000051117      *------------------------------------------------------------------------*
016100000000     C     CARV1         BEGSR
016200000000     C*
016300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016400000000     C*
016500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016700000000     C*
016800010329     C                   WRITE     SI63DVT                                      *TESTATA
016900010329     C                   WRITE     SI63DVF                                      *FUNZIONI
017000000000     C*
017100000000     C                   ENDSR
017200051117     ***C*------------------------------------------------------------------------*
017300051117     ***C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017400051117     ***C*------------------------------------------------------------------------*
017500051117     ***C     ENTV1         BEGSR
017600051117     ***C*
017700051117     ***C* CONTROLLA I CAMPI IMMESSI
017800051117     ***C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017900051117     ***C*
018000051117     ***C                   ENDSR
018100051117      *------------------------------------------------------------------------*
018200051117      * F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018300051117      *------------------------------------------------------------------------*
018400000000     C     F06GES        BEGSR
018500051117     ***C*
018600051117     ***C* CONTROLLA I CAMPI IMMESSI
018700051117     ***C                   EXSR      DECODI
018800000000     C*
018900000000     C* LANCIA IL PROGRAMMA DI ESECUZIONE
019000000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019100051117     ***C                   MOVEL     *BLANKS       PARLNP                      *LINEA DI PARTENZA
019200051117     ***C                   MOVEL     V1CTEM        PARTEM                      *TEMPI PICKUP
019300051117     ***C                   MOVEL     V1CTSP        PARTSP                      *TIPO SERVIZIO
019400051117     ***C                   MOVEL     V1CISO        PARISO                      *CONSIDERA ISOLE
019500051117     ***C                   MOVEL     V1CDIS        PARDIS                      *CONSIDERA L.DISAGG.
019600051117     ***C                   MOVEL     V1CSSA        PARSSA                      *SINTETICA/DETTAGLIO
019700051117     ***C                   MOVEL     V1CND1        PARND1                      *NOTA PICKUP   N°1
019800051117     ***C                   MOVEL     V1CND2        PARND2                      *NOTA PICKUP   N°2
019900051117     ***C                   MOVEL     V1CND3        PARND3                      *NOTA PICKUP   N°3
020000051117     ***C                   Z-ADD     V1CORE        PARORE                      *ORE STANDARD PICKUP
020100051117     ***C                   MOVEL     V1CRD1        PARRD1                      *REGOLA PICKUP   N°1
020200051117     ***C                   MOVEL     V1CRD2        PARRD2                      *REGOLA PICKUP   N°2
020300051117     ***C                   MOVEL     V1CRD3        PARRD3                      *REGOLA PICKUP   N°3
020400051117     ***C                   MOVEL     V1CPRI        PARPRI                      *STAMAP SOLO LOC PRI
020500051117     c                   clear                   Param
020600051117     c                   movel     'C'           PARtem                         *TEMPI PICKUP
020700051117     c                   movel     'C'           PARtsp                         *TIPO SERVIZIO
020800051117     c                   movel     'N'           PARiso                         *CONSIDERA ISOLE
020900051117     c                   movel     'N'           PARdis                         *CONSIDERA L.DISAG.
021000051117     c                   movel     'A'           PARssa                         *SINTETICA/ANALITICA
021100051117     c                   z-add     12            PARore                         *NUM. ORE STANDARD
021200051117     c                   movel     V1Cpri        PARpri                         *STAMPA SOLO LOC PRI
021300051117     c                   movel     V1Cnd1        PARnd1                         *NOTA PICKUP   N°1
021400051117     c                   movel     V1Cnd2        PARnd2                         *NOTA PICKUP   N°2
021500051117     c                   movel     V1Cnd3        PARnd3                         *NOTA PICKUP   N°3
021600051117     c                   movel     V1Crd1        PARrd1                         *REGOLA PICKUP   N°1
021700051117     c                   movel     V1Crd2        PARrd2                         *REGOLA PICKUP   N°2
021800051117     c                   movel     V1Crd3        PARrd3                         *REGOLA PICKUP   N°3
021900000000     C*
022000020925     C* Essendo la lista parametri più grande della KPJBU  occorre usare la data area del pgm
022100020925     C* la *LDA per poter passare tutti i parametri con il codice operativo OUT.
022200010509     C                   OUT       PARAM
022300010509     C                   MOVEL     'SI65'        KCOAZ
022400051117     ***C                   CALL      'BCH10'
022500051118     c***                movel(p)  'TISI65R'     Wpgm             10
022600051117     c                   movel(P)  'BCH10'       Wpgm             10
022700051117     c                   call      Wpgm
022800010705     C                   PARM                    KPJBA
022900000000     C*
023000000000     C                   MOVEL     MSG(15)       $MSG
023100000000     C                   EXSR      CARMSG
023200000000E   1C                   ENDIF
023300000000     C*
023400000000     C                   ENDSR
023500051117      *------------------------------------------------------------------------*
023600051117      * F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
023700051117      *------------------------------------------------------------------------*
023800000000     C     F22GES        BEGSR
023900000000     C*
024000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
024100000000     C                   Z-ADD     80            LENGH            15 5
024200000000     C                   CALL      'QCAEXEC'
024300000000     C                   PARM                    CMD(1)
024400000000     C                   PARM                    LENGH
024500000000     C*
024600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
024700000000     C*
024800000000     C                   ENDSR
024900051117     ***C*------------------------------------------------------------------------*
025000051117     ***C* DECODI - CONTROLLA CAMPI E DECODIFICA
025100051117     ***C*------------------------------------------------------------------------*
025200051117     ***C     DECODI        BEGSR
025300051117     ***C*
025400051117     ***C* REIMPOSTA INDICATORI DI ERRORE
025500051117     ***C                   SETOFF                                         6162
025600051117     ***C                   SETOFF                                       636465
025700051117     ***C*--------------------
025800051117     ***C* TEMPO PICKUP
025900051117     ***C*--------------------
026000051117   IF**1C     $ERR          IFEQ      '0'
026100051117   IF**2C     V1CTEM        IFEQ      'I'
026200051117     ***C     V1CTEM        OREQ      'C'
026300051117   X **2C                   ELSE
026400051117     ***C                   SETON                                        61
026500051117     ***C                   MOVEL     '1'           $ERR
026600051117     ***C                   MOVEL     MSG(11)       $MSG
026700051117     ***C                   EXSR      CARMSG
026800051117   E **2C                   ENDIF
026900051117   E **1C                   ENDIF
027000051117     ***C*--------------------
027100051117     ***C* TIPO SERVIZIO
027200051117     ***C*--------------------
027300051117   IF**1C     $ERR          IFEQ      '0'
027400051117     ***C                   MOVEL     V1CTSP        WTSP
027500051117     ***C                   MOVEL     *BLANKS       WDTSP
027600051117     ***C                   EXSR      CHKTSP
027700051117     ***C                   MOVEL     WTSP          V1CTSP                         *CODICE
027800051117     ***C                   MOVEL     WDTSP         V1DTSP                         *DESCRIZIONE
027900051117   IF**2C     $ERR          IFEQ      '1'
028000051117     ***C                   SETON                                        62
028100051117     ***C                   MOVEL     MSG(8)        $MSG
028200051117     ***C                   EXSR      CARMSG
028300051117   E **2C                   ENDIF
028400051117   E **1C                   ENDIF
028500051117     ***C*--------------------
028600051117     ***C* CAP/LOCALITA' ISOLA
028700051117     ***C*--------------------
028800051117   IF**1C     $ERR          IFEQ      '0'
028900051117   IF**2C     V1CISO        IFEQ      'S'
029000051117     ***C     V1CISO        OREQ      'N'
029100051117   X **2C                   ELSE
029200051117     ***C                   SETON                                        63
029300051117     ***C                   MOVEL     '1'           $ERR
029400051117     ***C                   MOVEL     MSG(11)       $MSG
029500051117     ***C                   EXSR      CARMSG
029600051117   E **2C                   ENDIF
029700051117   E **1C                   ENDIF
029800051117     ***C*--------------------
029900051117     ***C* CAP/LOCALITA' DISAGIATE
030000051117     ***C*--------------------
030100051117   IF**1C     $ERR          IFEQ      '0'
030200051117   IF**2C     V1CDIS        IFEQ      'S'
030300051117     ***C     V1CDIS        OREQ      'N'
030400051117   X **2C                   ELSE
030500051117     ***C                   SETON                                        64
030600051117     ***C                   MOVEL     '1'           $ERR
030700051117     ***C                   MOVEL     MSG(11)       $MSG
030800051117     ***C                   EXSR      CARMSG
030900051117   E **2C                   ENDIF
031000051117   E **1C                   ENDIF
031100051117     ***C*--------------------
031200051117     ***C* STAMPA SINTETICA / ANALITICA
031300051117     ***C*--------------------
031400051117   IF**1C     $ERR          IFEQ      '0'
031500051117   IF**2C     V1CSSA        IFEQ      'S'
031600051117     ***C     V1CSSA        OREQ      'A'
031700051117   X **2C                   ELSE
031800051117     ***C                   SETON                                        65
031900051117     ***C                   MOVEL     '1'           $ERR
032000051117     ***C                   MOVEL     MSG(11)       $MSG
032100051117     ***C                   EXSR      CARMSG
032200051117   E **2C                   ENDIF
032300051117   E **1C                   ENDIF
032400051117     ***C*
032500051117     ***C                   ENDSR
032600051117     ***C*------------------------------------------------------------------------*
032700051117     ***C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
032800051117     ***C*------------------------------------------------------------------------*
032900051117     ***C     CHKTSP        BEGSR
033000051117     ***C*
033100051117     ***C     *LIKE         DEFINE    V1CTSP        WTSP
033200051117     ***C     *LIKE         DEFINE    V1DTSP        WDTSP
033300051117     ***C*
033400051117     ***C* RICHIESTO ELENCO (?)
033500051117   IF**1C     WTSP          IFEQ      '?'
033600051117     ***C                   MOVEL     1             §KUT
033700051117     ***C                   MOVEL     '5E'          §COD
033800051117     ***C                   MOVEL     *BLANKS       §KEY
033900051117     ***C                   MOVEL     *BLANKS       §DES
034000051117     ***C                   CALL      'X§TABER'
034100051117     ***C                   PARM                    §KUT              1 0
034200051117     ***C                   PARM                    §COD              2
034300051117     ***C                   PARM                    §KEY              8
034400051117     ***C                   PARM                    §DES             30
034500051117     ***C                   MOVEL     §KEY          WTSP
034600051117     ***C                   MOVEL     §DES          WDTSP
034700051117     ***C                   MOVEL     '1'           $TESVD                      *RIEMETTERE TESTATA
034800051117   E **1C                   ENDIF
034900051117     ***C*
035000051117     ***C* CONTROLLA ESISTEZA RECORD
035100051117     ***C                   MOVEL     1             KTBKUT
035200051117     ***C                   MOVEL     '5E'          KTBCOD
035300051117     ***C                   MOVEL(P)  WTSP          KTBKEY
035400051117     ***C     KEYTAB        CHAIN     TABEL00F                           99
035500051117   IF**1C     *IN99         IFEQ      *OFF                                         *ESISTENTE
035600051117     ***C     TBLFLG        ANDEQ     *BLANKS                                      *NON ANNULLATO
035700051117     ***C                   MOVEL     TBLUNI        WDTSP
035800051117   X **1C                   ELSE
035900051117     ***C                   MOVEL     '1'           $ERR
036000051117   E **1C                   ENDIF
036100051117     ***C*
036200051117     ***C                   ENDSR
036300051117      *------------------------------------------------------------------------*
036400051117      * IMPVID - IMPOSTAZIONI INIZIALI VIDEO
036500051117      *------------------------------------------------------------------------*
036600000000     C     IMPVID        BEGSR
036700000000     C*
036800051117     ***C                   MOVEL     'C'           V1CTEM                      *TEMPI PICKUP
036900051117     ***C                   MOVEL     'C'           V1CTSP                      *TIPO SERVIZIO
037000051117     ***C                   MOVEL     'N'           V1CISO                      *CONSIDERA ISOLE
037100051117     ***C                   MOVEL     'N'           V1CDIS                      *CONSIDERA L.DISAG.
037200051117     ***C                   MOVEL     'A'           V1CSSA                      *SINTETICA/ANALITICA
037300051117     ***C                   Z-ADD     12            V1CORE                      *NUM. ORE STANDARD
037400010509     C                   MOVEL     REG(1)        V1CRD1                         *1° RIGA REGOLE
037500010509     C                   MOVEL     REG(2)        V1CRD2                         *2° RIGA REGOLE
037600010509     C                   MOVEL     REG(3)        V1CRD3                         *3° RIGA REGOLE
037700011016     C                   MOVEL     'S'           V1CPRI                         *STAMPA SOLO LOC. PR
037800040618     c                   Eval      %subst(v1crd1:47:2) = wore
037900040618     c                   Eval      %subst(v1crd1:50:2) = wmin
038000000000     C*
038100000000     C                   ENDSR
038200051117      *------------------------------------------------------------------------*
038300051117      * CARTAB - CARICA LE TABELLE
038400051117      *------------------------------------------------------------------------*
038500000000     C     CARTAB        BEGSR
038600040618
038700040618      * Tabella DFT
038800040618     c                   Clear                   dDft
038900040618     c                   Clear                   Tibs02ds
039000040618     c                   Eval      T02Mod = 'C'
039100040618     c                   Eval      T02Sif = knsif
039200040618     c                   Eval      T02Cod = 'DFT'
039300040618     c                   Eval      T02Ke1 = '999'
039400040618     c                   Call      'TIBS02R'
039500040618     c                   Parm                    kpjba
039600040618     c                   Parm                    Tibs02ds
039700040618     c                   Eval      dDft = T02Uni
039800040618     c                   Movel     d§dftorr      wora
039900000000     C*
040000000000     C                   ENDSR
040100051117      *--------------------------------------------------------------------------------------------*
040200051117      * REPERISCE I DATI UTENTE
040300051117      *--------------------------------------------------------------------------------------------*
040400020919     C     REPDATIUTE    BEGSR
040500020919     C*
040600020919     C* INIZIALIZZA VARIABILI DI WRK
040700020919     C                   CLEAR                   TIBS34DS
040800020919     C                   CLEAR                   AZUTEDS
040900020919     C                   CLEAR                   DDATIUTE
041000020919     C*
041100020919     C     *DTAARA       DEFINE    §azute        azuteds
041200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
041300020919     C                   IN(E)     *DTAARA
041400020919if  1C                   IF        %Error
041500020919     c                   EVAL      I34Tla = 'L'
041600020919     C                   CALL      'TIBS34R'
041700020919     C                   PARM                    Tibs34Ds
041800020919     C                   IN        *DTAARA
041900020919e   1C                   ENDIF
042000020919     C*
042100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
042200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
042300020919     C*
042400020919     C                   ENDSR
042500051117      *------------------------------------------------------------------------*
042600051117      * *INZSR - ROUTINE INIZIALE
042700051117      *------------------------------------------------------------------------*
042800000000     C     *INZSR        BEGSR
042900980210     C*---
043000000000     C* RICEVIMENTO PARAMETRI
043100980210     C*---
043200000000     C     *ENTRY        PLIST
043300000000     C                   PARM                    KPJBA
043400010509     C*---
043500010509     C* DEFINISCO LA *LDA
043600010509     C*---
043700010509     C     *DTAARA       DEFINE    *LDA          PARAM
043800980210     C*---
043900000000     C* VARIABILI RIFERITE AL DATA BASE
044000980210     C*---
044100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
044200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
044300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
044400980210     C*---
044500000000     C* CHIAVI DI LETTURA
044600980210     C*---
044700980210     C     KEYTAB        KLIST                                                  *TABEL00F
044800980210     C                   KFLD                    KTBKUT                          -UTENTE
044900980210     C                   KFLD                    KTBCOD                          -TABELLA
045000980210     C                   KFLD                    KTBKEY                          -CHIAVE
045100980210     C     KE2TAB        KLIST                                                  *TABEL00F
045200980210     C                   KFLD                    KTBKUT                          -UTENTE
045300980210     C                   KFLD                    KTBCOD                          -TABELLA
045400051117     ***C*---
045500051117     ***C* VARIABILI DI CONTROLLO
045600051117     ***C*---
045700051117     ***C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
045800051117     ***C*
045900051117     ***C* ACCENDO INDICATORI CONDIZIONANTI I PARAMETRI A VIDEO
046000051117     ***C                   SETON                                        60
046100000000     C*
046200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
046300980210     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
046400000000     C                   EXSR      INZSTD
046500980210     C*
046600980210     C* CALCOLA LA DATA CORRENTE
046700980210     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
046800980210     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
046900980210     C                   Z-ADD     WN8           G08DAT
047000980210     C                   Z-ADD     *ZEROS        G08INV
047100980210     C                   MOVEL     '0'           G08ERR
047200980210     C                   CALL      'XSRDA8'
047300980210     C                   PARM                    WLBDA8
047400980210     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
047500000000     C*
047600980210     C* CARICA TABELLE OCCORRENTI
047700000000     C                   EXSR      CARTAB
047800000000     C*
047900000000     C* IMPOSTAZIONI VIDEO
048000000000     C                   EXSR      IMPVID
048100000000     C*
048200000000     C* EMISSIONE VIDEO
048300000000     C                   EXSR      CARV1
048400000000     C*
048500000000     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
049000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
049100000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
049200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049400051117      *------------------------------------------------------------------------*
049500051117      * INZSTD - ROUTINE INIZIALE STANDARD
049600051117      *------------------------------------------------------------------------*
049700000000     C     INZSTD        BEGSR
049800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
049900000000     C                   CALL      'XKEYS'
050000000000     C                   PARM                    DSKEYS
050100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
050200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
050300000000     C     XMSGPA        PLIST
050400000000     C                   PARM                    DSMSMS
050500000000     C                   PARM                    DSMSPG
050600000000     C                   PARM                    DSMSKE
050700000000     C                   PARM                    DSMSER
050800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
050900010329     C                   MOVEL     'TISI63R'     DSMSPG                         *NOME PGM CHIAMANTE
051000010329     C                   MOVEL     'TISI63R'     VMCQUE                         *CODA MESSAGGI DESTI
051100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
051200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
051300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
051400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
051500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
051600000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
051700000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
051800000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
051900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
052000000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
052100000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
052200000000     C* LANCIO ROUTINE PER VERIFICARE SE
052300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
052400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
052500020919     C                   EXSR      REPDATIUTE
052600000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
052700000000     C                   BITOFF    '01234567'    POSMI1            1
052800000000     C                   BITOFF    '01234567'    POSMI2            1
052900000000     C                   BITOFF    '01234567'    POSMA1            1
053000000000     C                   BITOFF    '01234567'    POSMA2            1
053100000000     C                   BITON     '34'          POSMI1            1            *18
053200000000     C                   BITON     '7'           POSMI2            1            *01
053300000000     C                   BITON     '34'          POSMA1            1            *18
053400000000     C                   BITON     '0'           POSMA2            1            *80
053500000000     C                   MOVE      POSMI2        POSMIN            2
053600000000     C                   MOVEL     POSMI1        POSMIN
053700000000     C                   MOVE      POSMA2        POSMAX            2
053800000000     C                   MOVEL     POSMA1        POSMAX
053900000000     C                   ENDSR
054000051117      *------------------------------------------------------------------------*
054100051117      * FUNSTD - LANCIA SUBROUTINE STANDARD
054200051117      *------------------------------------------------------------------------*
054300000000     C     FUNSTD        BEGSR
054400000000     C                   SELECT
054500000000     C     KEY           WHENEQ    F3
054600000000     C                   EXSR      F03STD
054700000000     C     KEY           WHENEQ    F12
054800000000     C                   EXSR      F12STD
054900000000     C     KEY           WHENEQ    F24
055000000000     C                   EXSR      F24STD
055100000000     C                   OTHER
055200000000     C                   MOVEL     MSG(2)        $MSG
055300000000     C                   EXSR      CARMSG
055400000000     C                   ENDSL
055500000000     C                   ENDSR
055600051117      *------------------------------------------------------------------------*
055700051117      * F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
055800051117      *------------------------------------------------------------------------*
055900000000     C     F03STD        BEGSR
056000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
056100000000     C                   MOVEL     '1'           $F03
056200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
056300000000     C                   MOVEL     '1'           $FINE
056400000000     C                   ENDSR
056500051117      *------------------------------------------------------------------------*
056600051117      * F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
056700051117      *------------------------------------------------------------------------*
056800000000     C     F12STD        BEGSR
056900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
057000000000     C                   MOVEL     '1'           $F12
057100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
057200000000     C                   MOVEL     '1'           $FINE
057300000000     C                   ENDSR
057400051117      *------------------------------------------------------------------------*
057500051117      * F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
057600051117      *------------------------------------------------------------------------*
057700000000     C     F24STD        BEGSR
057800000000     C*
057900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
058000000000     C                   EXSR      OUTFUN
058100000000     C*
058200000000     C                   ENDSR
058300051117      *------------------------------------------------------------------------*
058400051117      * CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
058500051117      *          CODA MESSAGGI DEL PGM
058600051117      *------------------------------------------------------------------------*
058700000000     C     CARMSG        BEGSR
058800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
058900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
059000000000     C     MSGULT        IFEQ      0
059100000000     C                   MOVEL     '1'           *IN40
059200010329     C                   WRITE     SI63DVK
059300000000     C                   MOVEL     '0'           *IN40
059400000000     C                   ENDIF
059500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
059600000000     C                   MOVEL     $MSG          DSMSMS
059700000000     C                   CALL      'XMSG'        XMSGPA
059800000000     C     DSMSER        IFEQ      0
059900000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
060000000000     C                   ENDIF
060100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
060200000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
060300010329     C                   WRITE     SI63DVM                                      *SFL MESS
060400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
060500000000     C                   MOVEL     *BLANKS       $MSG
060600000000     C                   ENDSR
060700051117      *------------------------------------------------------------------------*
060800051117      * OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
060900051117      *------------------------------------------------------------------------*
061000000000     C     OUTMSG        BEGSR
061100000000     C* VISUALIZZAZIONE MESSAGGIO
061200000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
061300010329     C                   WRITE     SI63DVK                                      *CONTR MESS
061400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
061500000000     C                   Z-ADD     *ZEROS        MSGULT
061600000000     C                   ENDSR
061700051117      *------------------------------------------------------------------------*
061800051117      * CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
061900051117      *          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
062000051117      *          OUTPUT: - FFU    (SCHIERA FUNZIONI)
062100051117      *                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
062200051117      *                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
062300051117      *------------------------------------------------------------------------*
062400000000     C     CARFUN        BEGSR
062500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
062600000000     C     1             DO        31            I                 5 0          --- 1 -->
062700000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
062800000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
062900000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
063000020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
063100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
063200000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
063300000000     C                   ELSE                                                   *INTERROG.
063400000000     C                   MOVEL     FUNASI        FAT(I)
063500000000     C                   ENDIF
063600000000     C                   ELSE                                                   <-- 2 --> *FILIALE
063700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
063800000000     C                   MOVEL     FUNAFG        FAT(I)
063900000000     C                   ELSE                                                   *INTERROG.
064000000000     C                   MOVEL     FUNAFI        FAT(I)
064100000000     C                   ENDIF
064200000000     C                   ENDIF                                                  <-- 2 ---
064300000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
064400000000     C                   MOVEL     USER          WEDP              3            *PROFILO
064500000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
064600000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
064700000000     C                   ELSE
064800000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
064900000000     C                   ENDIF
065000000000     C                   ENDIF
065100000000     C                   ENDDO                                                  <-- 1 ---
065200000000     C                   ENDSR
065300051117      *------------------------------------------------------------------------*
065400051117      * OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
065500051117      *          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
065600051117      *          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
065700051117      *                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
065800051117      *------------------------------------------------------------------------*
065900000000     C     OUTFUN        BEGSR
066000000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
066100000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
066200000000     C                   ENDDO
066300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
066400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
066500000000     C     1             DO        31            I
066600000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
066700000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
066800000000     C                   ADD       1             FUNNAT
066900000000     C                   ENDIF
067000000000     C                   ENDDO
067100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
067200000000     C     FUNNAT        IFGT      8
067300000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
067400000000     C                   ENDIF
067500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
067600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
067700000000     C                   Z-ADD     8             FUNVIS            5 0
067800000000     C     FAT(23)       IFEQ      'S'
067900000000     C                   SUB       1             FUNVIS
068000000000     C                   ENDIF
068100000000     C     FAT(24)       IFEQ      'S'
068200000000     C                   SUB       1             FUNVIS
068300000000     C                   ENDIF
068400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
068500000000     C                   Z-ADD     *ZEROS        W                 5 0
068600000000     C                   ADD       1             FUNULT
068700000000     C     FUNULT        IFGT      31
068800000000     C                   Z-ADD     1             FUNULT
068900000000     C                   ENDIF
069000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
069100000000     C     FUNULT        DO        31            I                              --- 1 -->
069200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
069300000000     C     W             IFLT      FUNVIS                                       --- 2 -->
069400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
069500000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
069600000000     C     FDE(I)        ANDNE     *BLANKS
069700000000     C     I             IFEQ      23                                           *TOLGO F23
069800000000     C     I             OREQ      24                                           *E F24
069900000000     C                   ELSE
070000000000     C                   ADD       1             W
070100000000     C                   MOVEL     FDE(I)        FUV(W)
070200000000     C                   ENDIF
070300000000     C                   ENDIF                                                  <-- 3 ---
070400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
070500000000     C                   Z-ADD     I             FUNULT
070600000000     C                   ENDIF                                                  <-- 2 ---
070700000000     C                   ENDDO                                                  <-- 1 ---
070800000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
070900000000     C                   MOVEL     FUV(1)        VFD001
071000000000     C                   MOVEL     FUV(2)        VFD002
071100000000     C                   MOVEL     FUV(3)        VFD003
071200000000     C                   MOVEL     FUV(4)        VFD004
071300000000     C                   MOVEL     FUV(5)        VFD005
071400000000     C                   MOVEL     FUV(6)        VFD006
071500000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
071600000000     C     FAT(24)       ANDEQ     'N'
071700000000     C                   MOVEL     FUV(7)        VFD007
071800000000     C                   MOVEL     FUV(8)        VFD008
071900000000     C                   ELSE                                                   <-- 1 -->
072000000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
072100000000     C     FAT(24)       ANDEQ     'S'
072200000000     C                   MOVEL     FDE(23)       VFD007
072300000000     C                   MOVEL     FDE(24)       VFD008
072400000000     C                   ELSE                                                   <-- 2 -->
072500000000     C                   MOVEL     FUV(7)        VFD007
072600000000     C     FAT(23)       IFEQ      'S'
072700000000     C                   MOVEL     FDE(23)       VFD008
072800000000     C                   ELSE
072900000000     C                   MOVEL     FDE(24)       VFD008
073000000000     C                   ENDIF
073100000000     C                   ENDIF                                                  <-- 2 ---
073200000000     C                   ENDIF                                                  <-- 1 ---
073300000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
073400000000     C                   ENDSR
073500051117      *------------------------------------------------------------------------*
073600051117      * CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
073700051117      *           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
073800051117      *           OUTPUT: - FUNOK  (OPZIONE OK S/N)
073900051117      *                   - ERRORE SE FUNZIONE NON ATTIVA
074000051117      *------------------------------------------------------------------------*
074100000000     C     CONFUN        BEGSR
074200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
074300000000     C     1             DO        31            I                              --- 1 -->
074400000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
074500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
074600000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
074700000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
074800000000     C                   EXSR      CARMSG
074900000000     C                   MOVEL     '1'           $ERR
075000000000     C                   ENDIF                                                  <-- 3 ---
075100000000     C                   ENDIF                                                  <-- 2 ---
075200000000     C                   ENDDO                                                  <-- 1 ---
075300000000     C                   ENDSR
075400000000     O*------------------------------------------------------------------------*
075500000000** REG - (REGOLE)
075600040618Le richieste di ritiro pervenute entro le ore xx:xx vengono evase in
075700040618giornata.
075800040615
075900000000** MSG - (MESSAGGI)
076000010329TISI63R- Tasto funzionale non valido.                                          1
076100010329TISI63R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
076200051117TISI63R- 3                                                                     3
076300051117TISI63R- 4                                                                     4
076400051117TISI63R- 5            .                                                        5
076500051117TISI63R- 6                                                                     6
076600051117TISI63R- 7                                                                     7
076700051117>?LIBERO?< TISI63R- Tipo servizio errato                                                  8
076800010329TISI63R- 9
076900010329TISI63R- 10
077000051117>?LIBERO?< TISI63R- Immettere uno dei valori possibili                                   11
077100010329TISI63R- 12
077200010329TISI63R- 13
077300010329TISI63R- 14
077400010329TISI63R- Stampa lanciata ...                                                  15
077500000000** CMD  (COMANDI)
077600000000WRKSPLF
077700000000
077800000000
077900000000
078000000000
078100000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
078200000000F1    -                   -    NN     NN
078300000000F2    -                   -    NN     NN
078400000000F3    -F3=Fine            -    SS     SS
078500000000F4    -                   -    NN     NN
078600000000F5    -                   -    NN     NN
078700000000F6    -F6=Conferma        -    SS     SS
078800000000F7    -                   -    NN     NN
078900000000F8    -                   -    NN     NN
079000000000F9    -                   -    NN     NN
079100000000F10   -                   -    NN     NN
079200000000F11   -                   -    NN     NN
079300000000F12   -F12=Ritorno        -    SS     SS
079400000000F13   -                   -    NN     NN
079500000000F14   -                   -    NN     NN
079600000000F15   -                   -    NN     NN
079700000000F16   -                   -    NN     NN
079800000000F17   -                   -    NN     NN
079900000000F18   -                   -    NN     NN
080000000000F19   -                   -    NN     NN
080100000000F20   -                   -    NN     NN
080200000000F21   -                   -    NN     NN
080300000000F22   -F22=Gestione stampe-    EE     EE
080400000000F23   -F23=Altre opzioni  -    NN     NN
080500000000F24   -F24=Altri tasti    -    NN     NN
080600000000ENTER -                   -    SS     SS
080700000000ROLLUP-                   -    NN     NN
080800000000ROLLDN-                   -    NN     NN
080900000000HELP  -                   -    NN     NN
081000000000PRINT -                   -    NN     NN
081100000000BSPACE-                   -    NN     NN
081200000000CLEAR -                   -    NN     NN
