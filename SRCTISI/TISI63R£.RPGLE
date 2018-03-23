000100000000     H*------------------------------------------------------------------------*
000200010329     H* STAMPA PICKUP   TIME - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800010329     FTISI63D   CF   E             WORKSTN INFDS(DEVDS1)
000900010329     F                                     SFILE(SI63DVM:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400000000     D*-------------------
001500000000     D* MESSAGGI, COMANDI E FUNZIONI
001600000000     D*-------------------
001700010509     D REG             S             75    DIM(3) CTDATA PERRCD(1)              *REGOLE
001800000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
001900000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100000000     D*-------------------
002200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300000000     D*-------------------
002400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700000000     D*-------------------
002800000000     D* TABELLA DESCRIZIONI A VIDEO
002900000000     D*-------------------
003000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100000000     D*-------------------
003200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003300000000     D*-------------------
003400000000     D KEYS            S              1    DIM(40)
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800040618     d wora            ds
003900040618     d  wore                   1      2
004000040618     d  wmin                   3      4
004100000000     D*---------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*---------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*---------------------
004700000000     D* DS PER GESTIONE TASTI FUNZIONALI
004800000000     D*---------------------
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100000000     D*---------------------
005200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300000000     D*---------------------
005400000000     D                SDS
005500000000     D  USER                 254    263
005600000000     D*---------------------
005700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D*---------------------
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D  CURSOR               370    371
006200980210     D*---------------------
006300980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400980210     D*---------------------
006500980210     D WLBDA8          DS                  INZ
006600980210     D  G08DAT                 1      8  0
006700980210     D  G08INV                 9     16  0
006800980210     D  G08ERR                17     17
006900980210     D  G08TGI                18     22  0
007000000000     D*---------------------
007100000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007200000000     D*---------------------
007300000000     D DSMSG         E DS
007400000000     D*---------------------
007500020919     D* DS REPERIMENTO DATI UTENTE
007600000000     D*---------------------
007700020919     D TIBS34DS      E DS                                                       *Profili utente
007800020919     D DDATIUTE      E DS                                                       *Dati utente
007900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008000000000     D*---------------------
008100000000     D* DS PARAMETRI DI LANCIO
008200000000     D*------------------
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
009800980210     D*---------------------
009900980210     D* ARCHITETTURA
010000980210     D*---------------------
010100980210     D KPJBA         E DS
010200040618     D*-------------------
010300040618     D* DS REPERIMENTO TABELLA
010400040618     D*-------------------
010500040618     D Tibs02ds      e ds
010600040618     D*-------------------
010700040618     D* DS TABELLA DFT
010800040618     D*-------------------
010900040618     D dDft          e ds
011000000000     C*------------------------------------------------------------------------*
011100000000     C* MAIN LINE
011200000000     C*------------------------------------------------------------------------*
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
012500000000     C*------------------------------------------------------------------------*
012600000000     C* GESV1 - GESTIONE VIDEO UNO
012700000000     C*------------------------------------------------------------------------*
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
014700000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
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
015800000000     C*------------------------------------------------------------------------*
015900000000     C* CARV1 - CARICA VIDEO UNO
016000000000     C*------------------------------------------------------------------------*
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
017200000000     C*------------------------------------------------------------------------*
017300000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017400000000     C*------------------------------------------------------------------------*
017500000000     C     ENTV1         BEGSR
017600000000     C*
017700000000     C* CONTROLLA I CAMPI IMMESSI
017800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     F06GES        BEGSR
018500000000     C*
018600000000     C* CONTROLLA I CAMPI IMMESSI
018700000000     C                   EXSR      DECODI
018800000000     C*
018900000000     C* LANCIA IL PROGRAMMA DI ESECUZIONE
019000000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019100010402     C                   MOVEL     *BLANKS       PARLNP                         *LINEA DI PARTENZA
019200010329     C                   MOVEL     V1CTEM        PARTEM                         *TEMPI PICKUP
019300000000     C                   MOVEL     V1CTSP        PARTSP                         *TIPO SERVIZIO
019400980210     C                   MOVEL     V1CISO        PARISO                         *CONSIDERA ISOLE
019500980210     C                   MOVEL     V1CDIS        PARDIS                         *CONSIDERA L.DISAGG.
019600980210     C                   MOVEL     V1CSSA        PARSSA                         *SINTETICA/DETTAGLIO
019700010329     C                   MOVEL     V1CND1        PARND1                         *NOTA PICKUP   N°1
019800010329     C                   MOVEL     V1CND2        PARND2                         *NOTA PICKUP   N°2
019900010329     C                   MOVEL     V1CND3        PARND3                         *NOTA PICKUP   N°3
020000010509     C                   Z-ADD     V1CORE        PARORE                         *ORE STANDARD PICKUP
020100010509     C                   MOVEL     V1CRD1        PARRD1                         *REGOLA PICKUP   N°1
020200010509     C                   MOVEL     V1CRD2        PARRD2                         *REGOLA PICKUP   N°2
020300010509     C                   MOVEL     V1CRD3        PARRD3                         *REGOLA PICKUP   N°3
020400011016     C                   MOVEL     V1CPRI        PARPRI                         *STAMAP SOLO LOC PRI
020500000000     C*
020600020925     C* Essendo la lista parametri più grande della KPJBU  occorre usare la data area del pgm
020700020925     C* la *LDA per poter passare tutti i parametri con il codice operativo OUT.
020800010509     C                   OUT       PARAM
020900010509     C                   MOVEL     'SI65'        KCOAZ
021000011016     C                   CALL      'BCH10'
021100010705     C                   PARM                    KPJBA
021200000000     C*
021300000000     C                   MOVEL     MSG(15)       $MSG
021400000000     C                   EXSR      CARMSG
021500000000E   1C                   ENDIF
021600000000     C*
021700000000     C                   ENDSR
021800000000     C*------------------------------------------------------------------------*
021900000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
022000000000     C*------------------------------------------------------------------------*
022100000000     C     F22GES        BEGSR
022200000000     C*
022300000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022400000000     C                   Z-ADD     80            LENGH            15 5
022500000000     C                   CALL      'QCAEXEC'
022600000000     C                   PARM                    CMD(1)
022700000000     C                   PARM                    LENGH
022800000000     C*
022900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023000000000     C*
023100000000     C                   ENDSR
023200000000     C*------------------------------------------------------------------------*
023300000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023400000000     C*------------------------------------------------------------------------*
023500000000     C     DECODI        BEGSR
023600000000     C*
023700000000     C* REIMPOSTA INDICATORI DI ERRORE
023800010731     C                   SETOFF                                         6162
023900980210     C                   SETOFF                                       636465
024000000000     C*--------------------
024100010329     C* TEMPO PICKUP
024200000000     C*--------------------
024300000000IF  1C     $ERR          IFEQ      '0'
024400000000IF  2C     V1CTEM        IFEQ      'I'
024500000000     C     V1CTEM        OREQ      'C'
024600000000X   2C                   ELSE
024700000000     C                   SETON                                        61
024800000000     C                   MOVEL     '1'           $ERR
024900980210     C                   MOVEL     MSG(11)       $MSG
025000000000     C                   EXSR      CARMSG
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C*--------------------
025400000000     C* TIPO SERVIZIO
025500000000     C*--------------------
025600000000IF  1C     $ERR          IFEQ      '0'
025700000000     C                   MOVEL     V1CTSP        WTSP
025800980210     C                   MOVEL     *BLANKS       WDTSP
025900000000     C                   EXSR      CHKTSP
026000000000     C                   MOVEL     WTSP          V1CTSP                         *CODICE
026100000000     C                   MOVEL     WDTSP         V1DTSP                         *DESCRIZIONE
026200000000IF  2C     $ERR          IFEQ      '1'
026300000000     C                   SETON                                        62
026400980210     C                   MOVEL     MSG(8)        $MSG
026500000000     C                   EXSR      CARMSG
026600000000E   2C                   ENDIF
026700000000E   1C                   ENDIF
026800000000     C*--------------------
026900980216     C* CAP/LOCALITA' ISOLA
027000000000     C*--------------------
027100000000IF  1C     $ERR          IFEQ      '0'
027200000000IF  2C     V1CISO        IFEQ      'S'
027300000000     C     V1CISO        OREQ      'N'
027400000000X   2C                   ELSE
027500000000     C                   SETON                                        63
027600000000     C                   MOVEL     '1'           $ERR
027700000000     C                   MOVEL     MSG(11)       $MSG
027800000000     C                   EXSR      CARMSG
027900000000E   2C                   ENDIF
028000000000E   1C                   ENDIF
028100980210     C*--------------------
028200980216     C* CAP/LOCALITA' DISAGIATE
028300980210     C*--------------------
028400980210IF  1C     $ERR          IFEQ      '0'
028500980210IF  2C     V1CDIS        IFEQ      'S'
028600980210     C     V1CDIS        OREQ      'N'
028700980210X   2C                   ELSE
028800980210     C                   SETON                                        64
028900980210     C                   MOVEL     '1'           $ERR
029000980210     C                   MOVEL     MSG(11)       $MSG
029100980210     C                   EXSR      CARMSG
029200980210E   2C                   ENDIF
029300980210E   1C                   ENDIF
029400980210     C*--------------------
029500980210     C* STAMPA SINTETICA / ANALITICA
029600980210     C*--------------------
029700980210IF  1C     $ERR          IFEQ      '0'
029800980210IF  2C     V1CSSA        IFEQ      'S'
029900980210     C     V1CSSA        OREQ      'A'
030000980210X   2C                   ELSE
030100980210     C                   SETON                                        65
030200980210     C                   MOVEL     '1'           $ERR
030300980210     C                   MOVEL     MSG(11)       $MSG
030400980210     C                   EXSR      CARMSG
030500980210E   2C                   ENDIF
030600980210E   1C                   ENDIF
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
031100000000     C*------------------------------------------------------------------------*
031200000000     C     CHKTSP        BEGSR
031300000000     C*
031400000000     C     *LIKE         DEFINE    V1CTSP        WTSP
031500000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
031600000000     C*
031700980210     C* RICHIESTO ELENCO (?)
031800000000IF  1C     WTSP          IFEQ      '?'
031900020919     C                   MOVEL     1             §KUT
032000000000     C                   MOVEL     '5E'          §COD
032100000000     C                   MOVEL     *BLANKS       §KEY
032200000000     C                   MOVEL     *BLANKS       §DES
032300000000     C                   CALL      'X§TABER'
032400000000     C                   PARM                    §KUT              1 0
032500000000     C                   PARM                    §COD              2
032600000000     C                   PARM                    §KEY              8
032700000000     C                   PARM                    §DES             30
032800000000     C                   MOVEL     §KEY          WTSP
032900000000     C                   MOVEL     §DES          WDTSP
033000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
033100000000E   1C                   ENDIF
033200000000     C*
033300980210     C* CONTROLLA ESISTEZA RECORD
033400020919     C                   MOVEL     1             KTBKUT
033500000000     C                   MOVEL     '5E'          KTBCOD
033600000000     C                   MOVEL(P)  WTSP          KTBKEY
033700000000     C     KEYTAB        CHAIN     TABEL00F                           99
033800000000IF  1C     *IN99         IFEQ      *OFF                                         *ESISTENTE
033900000000     C     TBLFLG        ANDEQ     *BLANKS                                      *NON ANNULLATO
034000000000     C                   MOVEL     TBLUNI        WDTSP
034100000000X   1C                   ELSE
034200000000     C                   MOVEL     '1'           $ERR
034300000000E   1C                   ENDIF
034400000000     C*
034500000000     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
034800000000     C*------------------------------------------------------------------------*
034900000000     C     IMPVID        BEGSR
035000000000     C*
035100010329     C                   MOVEL     'C'           V1CTEM                         *TEMPI PICKUP
035200010509     C                   MOVEL     'C'           V1CTSP                         *TIPO SERVIZIO
035300980210     C                   MOVEL     'N'           V1CISO                         *CONSIDERA ISOLE
035400980210     C                   MOVEL     'N'           V1CDIS                         *CONSIDERA L.DISAG.
035500010509     C                   MOVEL     'A'           V1CSSA                         *SINTETICA/ANALITICA
035600010509     C                   Z-ADD     12            V1CORE                         *NUM. ORE STANDARD
035700010509     C                   MOVEL     REG(1)        V1CRD1                         *1° RIGA REGOLE
035800010509     C                   MOVEL     REG(2)        V1CRD2                         *2° RIGA REGOLE
035900010509     C                   MOVEL     REG(3)        V1CRD3                         *3° RIGA REGOLE
036000011016     C                   MOVEL     'S'           V1CPRI                         *STAMPA SOLO LOC. PR
036100040618     c                   Eval      %subst(v1crd1:47:2) = wore
036200040618     c                   Eval      %subst(v1crd1:50:2) = wmin
036300000000     C*
036400000000     C                   ENDSR
036500000000     C*------------------------------------------------------------------------*
036600000000     C* CARTAB - CARICA LE TABELLE
036700000000     C*------------------------------------------------------------------------*
036800000000     C     CARTAB        BEGSR
036900040618
037000040618      * Tabella DFT
037100040618     c                   Clear                   dDft
037200040618     c                   Clear                   Tibs02ds
037300040618     c                   Eval      T02Mod = 'C'
037400040618     c                   Eval      T02Sif = knsif
037500040618     c                   Eval      T02Cod = 'DFT'
037600040618     c                   Eval      T02Ke1 = '999'
037700040618     c                   Call      'TIBS02R'
037800040618     c                   Parm                    kpjba
037900040618     c                   Parm                    Tibs02ds
038000040618     c                   Eval      dDft = T02Uni
038100040618     c                   Movel     d§dftorr      wora
038200000000     C*
038300000000     C                   ENDSR
038400020919     C*--------------------------------------------------------------------------------------------*
038500020919     C* REPERISCE I DATI UTENTE
038600020919     C*--------------------------------------------------------------------------------------------*
038700020919     C     REPDATIUTE    BEGSR
038800020919     C*
038900020919     C* INIZIALIZZA VARIABILI DI WRK
039000020919     C                   CLEAR                   TIBS34DS
039100020919     C                   CLEAR                   AZUTEDS
039200020919     C                   CLEAR                   DDATIUTE
039300020919     C*
039400020919     C     *DTAARA       DEFINE    §azute        azuteds
039500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
039600020919     C                   IN(E)     *DTAARA
039700020919if  1C                   IF        %Error
039800020919     c                   EVAL      I34Tla = 'L'
039900020919     C                   CALL      'TIBS34R'
040000020919     C                   PARM                    Tibs34Ds
040100020919     C                   IN        *DTAARA
040200020919e   1C                   ENDIF
040300020919     C*
040400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
040500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
040600020919     C*
040700020919     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* *INZSR - ROUTINE INIZIALE
041000000000     C*------------------------------------------------------------------------*
041100000000     C     *INZSR        BEGSR
041200980210     C*---
041300000000     C* RICEVIMENTO PARAMETRI
041400980210     C*---
041500000000     C     *ENTRY        PLIST
041600000000     C                   PARM                    KPJBA
041700010509     C*---
041800010509     C* DEFINISCO LA *LDA
041900010509     C*---
042000010509     C     *DTAARA       DEFINE    *LDA          PARAM
042100980210     C*---
042200000000     C* VARIABILI RIFERITE AL DATA BASE
042300980210     C*---
042400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
042500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
042600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
042700980210     C*---
042800000000     C* CHIAVI DI LETTURA
042900980210     C*---
043000980210     C     KEYTAB        KLIST                                                  *TABEL00F
043100980210     C                   KFLD                    KTBKUT                          -UTENTE
043200980210     C                   KFLD                    KTBCOD                          -TABELLA
043300980210     C                   KFLD                    KTBKEY                          -CHIAVE
043400980210     C     KE2TAB        KLIST                                                  *TABEL00F
043500980210     C                   KFLD                    KTBKUT                          -UTENTE
043600980210     C                   KFLD                    KTBCOD                          -TABELLA
043700980210     C*---
043800000000     C* VARIABILI DI CONTROLLO
043900980210     C*---
044000000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
044100010731     C*
044200010731     C* ACCENDO INDICATORI CONDIZIONANTI I PARAMETRI A VIDEO
044300010731     C                   SETON                                        60
044400000000     C*
044500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
044600980210     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
044700000000     C                   EXSR      INZSTD
044800980210     C*
044900980210     C* CALCOLA LA DATA CORRENTE
045000980210     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045100980210     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045200980210     C                   Z-ADD     WN8           G08DAT
045300980210     C                   Z-ADD     *ZEROS        G08INV
045400980210     C                   MOVEL     '0'           G08ERR
045500980210     C                   CALL      'XSRDA8'
045600980210     C                   PARM                    WLBDA8
045700980210     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
045800000000     C*
045900980210     C* CARICA TABELLE OCCORRENTI
046000000000     C                   EXSR      CARTAB
046100000000     C*
046200000000     C* IMPOSTAZIONI VIDEO
046300000000     C                   EXSR      IMPVID
046400000000     C*
046500000000     C* EMISSIONE VIDEO
046600000000     C                   EXSR      CARV1
046700000000     C*
046800000000     C                   ENDSR
046900000000     C*------------------------------------------------------------------------*
047000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047200000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
047300000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
047400000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
047500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047700000000     C*------------------------------------------------------------------------*
047800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
047900000000     C*------------------------------------------------------------------------*
048000000000     C     INZSTD        BEGSR
048100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
048200000000     C                   CALL      'XKEYS'
048300000000     C                   PARM                    DSKEYS
048400000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
048500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
048600000000     C     XMSGPA        PLIST
048700000000     C                   PARM                    DSMSMS
048800000000     C                   PARM                    DSMSPG
048900000000     C                   PARM                    DSMSKE
049000000000     C                   PARM                    DSMSER
049100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
049200010329     C                   MOVEL     'TISI63R'     DSMSPG                         *NOME PGM CHIAMANTE
049300010329     C                   MOVEL     'TISI63R'     VMCQUE                         *CODA MESSAGGI DESTI
049400000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
049500000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
049600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
049700000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
049800000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
049900000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
050000000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
050100000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
050200000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
050300000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
050400000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
050500000000     C* LANCIO ROUTINE PER VERIFICARE SE
050600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
050700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
050800020919     C                   EXSR      REPDATIUTE
050900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
051000000000     C                   BITOFF    '01234567'    POSMI1            1
051100000000     C                   BITOFF    '01234567'    POSMI2            1
051200000000     C                   BITOFF    '01234567'    POSMA1            1
051300000000     C                   BITOFF    '01234567'    POSMA2            1
051400000000     C                   BITON     '34'          POSMI1            1            *18
051500000000     C                   BITON     '7'           POSMI2            1            *01
051600000000     C                   BITON     '34'          POSMA1            1            *18
051700000000     C                   BITON     '0'           POSMA2            1            *80
051800000000     C                   MOVE      POSMI2        POSMIN            2
051900000000     C                   MOVEL     POSMI1        POSMIN
052000000000     C                   MOVE      POSMA2        POSMAX            2
052100000000     C                   MOVEL     POSMA1        POSMAX
052200000000     C                   ENDSR
052300000000     C*------------------------------------------------------------------------*
052400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
052500000000     C*------------------------------------------------------------------------*
052600000000     C     FUNSTD        BEGSR
052700000000     C                   SELECT
052800000000     C     KEY           WHENEQ    F3
052900000000     C                   EXSR      F03STD
053000000000     C     KEY           WHENEQ    F12
053100000000     C                   EXSR      F12STD
053200000000     C     KEY           WHENEQ    F24
053300000000     C                   EXSR      F24STD
053400000000     C                   OTHER
053500000000     C                   MOVEL     MSG(2)        $MSG
053600000000     C                   EXSR      CARMSG
053700000000     C                   ENDSL
053800000000     C                   ENDSR
053900000000     C*------------------------------------------------------------------------*
054000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
054100000000     C*------------------------------------------------------------------------*
054200000000     C     F03STD        BEGSR
054300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
054400000000     C                   MOVEL     '1'           $F03
054500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
054600000000     C                   MOVEL     '1'           $FINE
054700000000     C                   ENDSR
054800000000     C*------------------------------------------------------------------------*
054900000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
055000000000     C*------------------------------------------------------------------------*
055100000000     C     F12STD        BEGSR
055200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
055300000000     C                   MOVEL     '1'           $F12
055400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
055500000000     C                   MOVEL     '1'           $FINE
055600000000     C                   ENDSR
055700000000     C*------------------------------------------------------------------------*
055800000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
055900000000     C*------------------------------------------------------------------------*
056000000000     C     F24STD        BEGSR
056100000000     C*
056200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
056300000000     C                   EXSR      OUTFUN
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
056800000000     C*          CODA MESSAGGI DEL PGM
056900000000     C*------------------------------------------------------------------------*
057000000000     C     CARMSG        BEGSR
057100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
057200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
057300000000     C     MSGULT        IFEQ      0
057400000000     C                   MOVEL     '1'           *IN40
057500010329     C                   WRITE     SI63DVK
057600000000     C                   MOVEL     '0'           *IN40
057700000000     C                   ENDIF
057800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
057900000000     C                   MOVEL     $MSG          DSMSMS
058000000000     C                   CALL      'XMSG'        XMSGPA
058100000000     C     DSMSER        IFEQ      0
058200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
058300000000     C                   ENDIF
058400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
058500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
058600010329     C                   WRITE     SI63DVM                                      *SFL MESS
058700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
058800000000     C                   MOVEL     *BLANKS       $MSG
058900000000     C                   ENDSR
059000000000     C*------------------------------------------------------------------------*
059100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
059200000000     C*------------------------------------------------------------------------*
059300000000     C     OUTMSG        BEGSR
059400000000     C* VISUALIZZAZIONE MESSAGGIO
059500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
059600010329     C                   WRITE     SI63DVK                                      *CONTR MESS
059700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
059800000000     C                   Z-ADD     *ZEROS        MSGULT
059900000000     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
060200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
060300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
060400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
060500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
060600000000     C*------------------------------------------------------------------------*
060700000000     C     CARFUN        BEGSR
060800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
060900000000     C     1             DO        31            I                 5 0          --- 1 -->
061000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
061100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
061200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
061300020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
061400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
061500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
061600000000     C                   ELSE                                                   *INTERROG.
061700000000     C                   MOVEL     FUNASI        FAT(I)
061800000000     C                   ENDIF
061900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
062000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
062100000000     C                   MOVEL     FUNAFG        FAT(I)
062200000000     C                   ELSE                                                   *INTERROG.
062300000000     C                   MOVEL     FUNAFI        FAT(I)
062400000000     C                   ENDIF
062500000000     C                   ENDIF                                                  <-- 2 ---
062600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
062700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
062800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
062900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
063000000000     C                   ELSE
063100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
063200000000     C                   ENDIF
063300000000     C                   ENDIF
063400000000     C                   ENDDO                                                  <-- 1 ---
063500000000     C                   ENDSR
063600000000     C*------------------------------------------------------------------------*
063700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
063800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
063900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
064000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
064100000000     C*------------------------------------------------------------------------*
064200000000     C     OUTFUN        BEGSR
064300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
064400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
064500000000     C                   ENDDO
064600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
064700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
064800000000     C     1             DO        31            I
064900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
065000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
065100000000     C                   ADD       1             FUNNAT
065200000000     C                   ENDIF
065300000000     C                   ENDDO
065400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
065500000000     C     FUNNAT        IFGT      8
065600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
065700000000     C                   ENDIF
065800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
065900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
066000000000     C                   Z-ADD     8             FUNVIS            5 0
066100000000     C     FAT(23)       IFEQ      'S'
066200000000     C                   SUB       1             FUNVIS
066300000000     C                   ENDIF
066400000000     C     FAT(24)       IFEQ      'S'
066500000000     C                   SUB       1             FUNVIS
066600000000     C                   ENDIF
066700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
066800000000     C                   Z-ADD     *ZEROS        W                 5 0
066900000000     C                   ADD       1             FUNULT
067000000000     C     FUNULT        IFGT      31
067100000000     C                   Z-ADD     1             FUNULT
067200000000     C                   ENDIF
067300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
067400000000     C     FUNULT        DO        31            I                              --- 1 -->
067500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
067600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
067700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
067800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
067900000000     C     FDE(I)        ANDNE     *BLANKS
068000000000     C     I             IFEQ      23                                           *TOLGO F23
068100000000     C     I             OREQ      24                                           *E F24
068200000000     C                   ELSE
068300000000     C                   ADD       1             W
068400000000     C                   MOVEL     FDE(I)        FUV(W)
068500000000     C                   ENDIF
068600000000     C                   ENDIF                                                  <-- 3 ---
068700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
068800000000     C                   Z-ADD     I             FUNULT
068900000000     C                   ENDIF                                                  <-- 2 ---
069000000000     C                   ENDDO                                                  <-- 1 ---
069100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
069200000000     C                   MOVEL     FUV(1)        VFD001
069300000000     C                   MOVEL     FUV(2)        VFD002
069400000000     C                   MOVEL     FUV(3)        VFD003
069500000000     C                   MOVEL     FUV(4)        VFD004
069600000000     C                   MOVEL     FUV(5)        VFD005
069700000000     C                   MOVEL     FUV(6)        VFD006
069800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
069900000000     C     FAT(24)       ANDEQ     'N'
070000000000     C                   MOVEL     FUV(7)        VFD007
070100000000     C                   MOVEL     FUV(8)        VFD008
070200000000     C                   ELSE                                                   <-- 1 -->
070300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
070400000000     C     FAT(24)       ANDEQ     'S'
070500000000     C                   MOVEL     FDE(23)       VFD007
070600000000     C                   MOVEL     FDE(24)       VFD008
070700000000     C                   ELSE                                                   <-- 2 -->
070800000000     C                   MOVEL     FUV(7)        VFD007
070900000000     C     FAT(23)       IFEQ      'S'
071000000000     C                   MOVEL     FDE(23)       VFD008
071100000000     C                   ELSE
071200000000     C                   MOVEL     FDE(24)       VFD008
071300000000     C                   ENDIF
071400000000     C                   ENDIF                                                  <-- 2 ---
071500000000     C                   ENDIF                                                  <-- 1 ---
071600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
071700000000     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
072000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
072100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
072200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
072300000000     C*------------------------------------------------------------------------*
072400000000     C     CONFUN        BEGSR
072500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
072600000000     C     1             DO        31            I                              --- 1 -->
072700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
072800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
072900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
073000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
073100000000     C                   EXSR      CARMSG
073200000000     C                   MOVEL     '1'           $ERR
073300000000     C                   ENDIF                                                  <-- 3 ---
073400000000     C                   ENDIF                                                  <-- 2 ---
073500000000     C                   ENDDO                                                  <-- 1 ---
073600000000     C                   ENDSR
073700000000     O*------------------------------------------------------------------------*
073800000000** REG - (REGOLE)
073900040618Le richieste di ritiro pervenute entro le ore xx:xx vengono evase in
074000040618giornata.
074100040615
074200000000** MSG - (MESSAGGI)
074300010329TISI63R- Tasto funzionale non valido.                                          1
074400010329TISI63R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
074500010329TISI63R- Opzione non valida.                                                   3
074600010329TISI63R- Opzione abilitata ma non gestita. Avvertire CED.                      4
074700010329TISI63R- Inizio elenco.                                                        5
074800010329TISI63R- Fine elenco.                                                          6
074900010329TISI63R- Punto operativo errato                                                7
075000010329TISI63R- Tipo servizio errato                                                  8
075100010329TISI63R- 9
075200010329TISI63R- 10
075300010329TISI63R- Immettere uno dei valori possibili                                   11
075400010329TISI63R- 12
075500010329TISI63R- 13
075600010329TISI63R- 14
075700010329TISI63R- Stampa lanciata ...                                                  15
075800000000** CMD  (COMANDI)
075900000000WRKSPLF
076000000000
076100000000
076200000000
076300000000
076400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
076500000000F1    -                   -    NN     NN
076600000000F2    -                   -    NN     NN
076700000000F3    -F3=Fine            -    SS     SS
076800000000F4    -                   -    NN     NN
076900000000F5    -                   -    NN     NN
077000000000F6    -F6=Conferma        -    SS     SS
077100000000F7    -                   -    NN     NN
077200000000F8    -                   -    NN     NN
077300000000F9    -                   -    NN     NN
077400000000F10   -                   -    NN     NN
077500000000F11   -                   -    NN     NN
077600000000F12   -F12=Ritorno        -    SS     SS
077700000000F13   -                   -    NN     NN
077800000000F14   -                   -    NN     NN
077900000000F15   -                   -    NN     NN
078000000000F16   -                   -    NN     NN
078100000000F17   -                   -    NN     NN
078200000000F18   -                   -    NN     NN
078300000000F19   -                   -    NN     NN
078400000000F20   -                   -    NN     NN
078500000000F21   -                   -    NN     NN
078600000000F22   -F22=Gestione stampe-    EE     EE
078700000000F23   -F23=Altre opzioni  -    NN     NN
078800000000F24   -F24=Altri tasti    -    NN     NN
078900000000ENTER -                   -    SS     SS
079000000000ROLLUP-                   -    NN     NN
079100000000ROLLDN-                   -    NN     NN
079200000000HELP  -                   -    NN     NN
079300000000PRINT -                   -    NN     NN
079400000000BSPACE-                   -    NN     NN
079500000000CLEAR -                   -    NN     NN
