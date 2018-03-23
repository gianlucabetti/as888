000100990802      *------------------------------------------------------------------------*
000200990802      * INTERROGAZIONE VARIAZIONI BOLLE DI SEDE - SUBFILE DI SCELTA
000300990802      *------------------------------------------------------------------------*
000400990802
000500990802     H DECEDIT('0,') DATEDIT(*DMY.)
000600990802      *------------------------------------------------------------------------*
000700990802     FTNSB53D   CF   E             WORKSTN INFDS(DEVDS1)
000800990802     F                                     SFILE(SB53DVS:SFLULT)
000900990802     F                                     SFILE(SB53DVM:MSGULT)
001000990802     FTITASV1L  IF   E           K DISK
001100990802     FTABEL00F  IF   E           K DISK
001200990802
001300990802      *------------------------------------------------------------------------*
001400990802      * SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
001500990802     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001600990802     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
001700990802     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
001800990802     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
001900990802      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000990802     D FFU             S              6    DIM(31)                              *FUNZIONE
002100990802     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002200990802     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002300990802      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
002400990802     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
002500990802     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
002600990802     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
002700990802     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
002800990802      * TABELLA DESCRIZIONI A VIDEO
002900990802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000990802     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
003100990802      * TABELLA TASTI FUNZIONALI PREMUTI
003200990802     D KEYS            S              1    DIM(40)
003300990802      *------------------------------------------------------------------------*
003400990802      * INPUT
003500990802      *------------------------------------------------------------------------*
003600990802      *---------------------
003700990802      * RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
003800990802      *---------------------
003900990802     D DSOPZ         E DS
004000990802     D  OPZRIG                 1     40
004100990802      *---------------------
004200990802      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300990802      *---------------------
004400990802     D DSFUN         E DS
004500990802     D  FUNRIG                 1     40
004600990802      *---------------------
004700990802      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800990802      *---------------------
004900990802     D DSMSG         E DS
005000020724      *---------------------
005100020724     D TIBS02DS      E DS
005200020724     D TIBS34DS      e ds                  inz
005300020724     D DDATIUTE      e ds                  inz
005400020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
005500020724     D DUTE01        e ds                  inz
005600020724     D DLAT          e ds                  inz
005700990802      *---------------------
005800990802      * DS PER GESTIONE TASTI FUNZIONALI
005900990802      *---------------------
006000990802     D DSKEYS        E DS
006100990802     D  KEYRIG                 1     40
006200990802      *---------------------
006300990802      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006400990802      *---------------------
006500990802     D                SDS
006600990802     D  VTCPGM                 1     10
006700990802     D  USER                 254    263
006800990802      *---------------------
006900990802      * RIDEFINIZIONE INFDS DEL FILE VIDEO
007000990802      *---------------------
007100990802     D DEVDS1          DS
007200990802      * CHIAVE FUNZIONALE BATTUTA
007300990802     D  KEY                  369    369
007400990802      * POSIZIONE CURSORE (IN HEX)
007500990802     D  CURSOR               370    371
007600990802      * RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007700990802     D  FRTSFL               378    379B 0
007800990802      *---------------------
007900990802     D TNSB01DS      E DS
008000990802     D KPJBA         E DS
008100990802      *------------------
008200990802     D DATA_eur        S               D   DATFMT(*eur)
008300990802
008400990802     D KTASaas         S                   LIKE(TASaas)
008500990802     D KTASlnp         S                   LIKE(TASlnp)
008600990802     D KTASnrs         S                   LIKE(TASnrs)
008700990802     D KTASnsp         S                   LIKE(TASnsp)
008800990802     D DEPaas          S                   LIKE(VCCaas)
008900990802     D Wop0            S                   LIKE(D01op0)
009000990802     D Wop1            S                   LIKE(D01op1)
009100990802     D Waas            S                   LIKE(D01aas)
009200990802     D Wlnp            S                   LIKE(D01lnp)
009300990802     D Wnrs            S                   LIKE(D01nrs)
009400990802     D Wnsp            S                   LIKE(D01nsp)
009500990802     D Wdsp            S                   LIKE(D01dsp)
009600990802     D Wdtv            S                   LIKE(D01dtv)
009700990802     D Worv            S                   LIKE(D01orv)
009800990802     D Wpru            S                   LIKE(D01pru)
009900990802      *------------------------------------------------------------------------*
010000990802      * MAIN LINE
010100990802      *------------------------------------------------------------------------*
010200990802      *
010300990802      * CICLO FINO A CHE:
010400020724     C     $FINE         DOWNE     '1'
010500990802      *
010600990802     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010700990802      *
010800990802      * CONTROLLA GESTIONE VIDEO
010900990802     C     $GES          IFEQ      'VS'
011000990802     C                   EXSR      GESVS                                        *SUBFILE
011100990802     C                   ENDIF
011200990802      *
011300990802     C                   ENDDO
011400990802      *
011500990802      * SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
011600990802     C     $F03          IFEQ      '1'
011700990802     C                   MOVEL     '1'           D01F03
011800990802     C                   ENDIF
011900990802      * SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
012000990802     C     $F12          IFEQ      '1'
012100990802     C                   MOVEL     '1'           D01F12
012200990802     C                   ENDIF
012300990802      * FINE PGM
012400990802     C                   MOVEL     TNSB01DS      KPJBU
012500990802     C                   SETON                                        LR
012600990802      *------------------------------------------------------------------------*
012700990802      * CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012800990802      *------------------------------------------------------------------------*
012900990802     C     CARVS         BEGSR
013000990802      *
013100990802     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
013200990802      *
013300990802     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
013400990802     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
013500990802     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
013600990802     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013700990802     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013800990802      *
013900990802     C                   WRITE     SB53DVT                                      *TESTATA
014000990802     C                   WRITE     SB53DVO                                      *OPZIONI
014100990802     C                   WRITE     SB53DVF                                      *FUNZIONI
014200990802      *
014300990802     C                   EXSR      POSSFL                                       *CARICA SFL
014400990802      *
014500990802     C                   ENDSR
014600990802      *------------------------------------------------------------------------*
014700990802      * GESVS  - GESTIONE SUBFILE
014800990802      *------------------------------------------------------------------------*
014900990802     C     GESVS         BEGSR
015000990802      *
015100990802     C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015200990802     C                   WRITE     SB53DVT                                      *TESTATA
015300990802     C                   WRITE     SB53DVO                                      *OPZIONI
015400990802     C                   WRITE     SB53DVF                                      *FUNZIONI
015500990802     C                   ENDIF
015600990802      *
015700990802     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
015800990802      *
015900990802     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
016000990802      *
016100990802     C     $ERR          IFEQ      '0'
016200990802     C                   SELECT
016300990802     C     KEY           WHENEQ    F12
016400990802     C                   EXSR      F12GES
016500990802     C                   OTHER
016600990802     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
016700990802     C                   ENDSL
016800990802     C                   ENDIF
016900990802      *
017000990802     C                   ENDSR
017100990802      *------------------------------------------------------------------------*
017200990802      * F12GES -GEST.TASTO FUNZ. F12 ===> PRECEDENTE
017300990802      *------------------------------------------------------------------------*
017400990802     C     F12GES        BEGSR
017500990802      *
017600990802     C                   MOVEL     *BLANKS       D01OP1
017700990802     C                   MOVEL     '1'           $FINE
017800990802      *
017900990802     C                   ENDSR
018000990802      *------------------------------------------------------------------------*
018100990802      *  GESOPZ - GESTIONE OPZIONI
018200990802      *------------------------------------------------------------------------*
018300990802     C     GESOPZ        BEGSR
018400990802      *
018500990802     C                   if        VSCOPZ  = ' 5'
018600990802     C                   EXSR      O05GES                                       *VISUALIZZAZIONE
018700990802     C                   else
018800990802     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
018900990802     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
019000990802     C                   endif
019100990802      *
019200990802     C                   ENDSR
019300990802      *------------------------------------------------------------------------*
019400990802      *  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
019500990802      *------------------------------------------------------------------------*
019600990802     C     O05GES        BEGSR
019700990802      *
019800990802      * LANCIO LA VISUALIZZAZIONE
019900990802     C                   Z-ADD     VCCAAS        WAAS                           *ANNO
020000990802     C                   Z-ADD     VCCLNP        WLNP                           *LNP
020100990802     C                   Z-ADD     VCCNRS        WNRS                           *SERIE
020200990802     C                   Z-ADD     VCCNSP        WNSP                           *SPEDIZIONE
020300990802     C                   Z-ADD     VCCDSP        WDSP                           *DATA SPEDIZIONE GMA
020400990802     C     *eur          movel     VSCDTV        DATA_EUR                       *DATA VARIAZIONE
020500990802     C     *iso          movel     DATA_EUR      WDTV                           *AAAA/MM/GG
020600990802     C                   Z-ADD     VSCORV        WORV                           *ORA
020700990802     C                   MOVEL     VSCPRU        WPRU                           *UTENTE
020800990802      *
020900990802     C                   MOVEL     D01OP0        WOP0                           *LIVELLO PROCEDURA
021000990802     C                   MOVEL     'O05'         WOP1                           *LIVELLO PROGRAMMA
021100990802     C                   MOVEL     'TNSB54R'     WPGM              8            *ROUTINE VISUALIZZA
021200990802     C                   EXSR      LANPGM
021300990802      *
021400990802     C                   ENDSR
021500990802      *------------------------------------------------------------------------*
021600990802      * NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
021700990802      *------------------------------------------------------------------------*
021800990802     C     NEWPOS        BEGSR
021900990802      *
022000990802      * SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
022100990802      *        1 = RICHIESTO POSIZIONAMENTO
022200990802     C                   Z-ADD     0             SWPOSI            7 0
022300990802      *
022400990802      * SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
022500990802      *            NON OCCORRE RICARICARE IL SUBFILE
022600990802      *        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
022700990802     C                   Z-ADD     0             SWMODI            1 0
022800990802      *
022900990802      * CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
023000990802      *----------------
023100990802      * N.B.:  PER COMODITA' HO LASCIATO L'IMPOSTAZIONE CLASSICA DEL
023200990802      *        PROGRAMMA, CIOE' CON I CAMPI POSIZIONAMENTO DI I/O NEL
023300990802      *        SFLCONTROL (QUINDI CON I CAMPI "DEPOSITO").
023400990802      *        ORA TALI CAMPI NON VI SONO A VIDEO MA METTENDOLI,
023500990802      *        IL PGM E' GIA' PREDISPOSTO.
023600990802      *----------------
023700990802     C     VCCAAS        IFNE      DEPAAS                                       *ANNO
023800990802      *
023900990802      * E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
024000990802     C                   EXSR      CHKPAR
024100990802      *
024200990802      * SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
024300990802     C     $ERR          IFEQ      '0'
024400990802     C                   Z-ADD     1             SWPOSI                         *RICHIESTO POSIZ.TO
024500990802     C                   Z-ADD     VCCAAS        DEPAAS                         *SALVATO NEW POSIZ.T
024600990802     C                   ENDIF
024700990802     C                   ENDIF
024800990802      *
024900990802     C                   ENDSR
025000990802      *------------------------------------------------------------------------*
025100990802      *  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
025200990802      *------------------------------------------------------------------------*
025300990802     C     CHKPAR        BEGSR
025400990802      *
025500990802      * REIMPOSTO INDICATORI DI ERRORE
025600990802     C                   SETOFF                                         81
025700990802      *
025800990802      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
025900990802      * SUL CAMPO DEL CONTROL
026000990802     C     $ERR          IFEQ      '1'
026100990802     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
026200990802     C                   ELSE
026300990802     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
026400990802     C                   ENDIF
026500990802      *
026600990802     C                   ENDSR
026700990802      *------------------------------------------------------------------------*
026800990802      * SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
026900990802      *------------------------------------------------------------------------*
027000990802     C     SETFIL        BEGSR
027100990802      *
027200990802     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
027300990802      *
027400990802      * POSIZIONAMENTO ARCHIVIO
027500990802     C                   Z-ADD     VCCAAS        Ktasaas
027600990802     C                   Z-ADD     VCCLNP        Ktaslnp
027700990802     C                   Z-ADD     VCCNRS        Ktasnrs
027800990802     C                   Z-ADD     VCCNSP        Ktasnsp
027900990802     C     KEYTA0        SETLL     TITASV1L                           99        *FINE FILE
028000990802      *
028100990802     C  N99              EXSR      LETFIL
028200990802      *
028300990802      * IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
028400990802     C     *IN99         IFEQ      '1'
028500990802     C                   Z-ADD     1             SFLEOF
028600990802     c                   ENDIF
028700990802      *
028800990802     C                   ENDSR
028900990802      *------------------------------------------------------------------------*
029000990802      * LETFIL - LEGGE RECORD SUCESSIVO
029100990802      *------------------------------------------------------------------------*
029200990802     C     LETFIL        BEGSR
029300990802      *
029400990802      * LEGGO FINO A:
029500990802      *     - FINE FILE
029600990802      *     - TROVATO RECORD VALIDO
029700990802     C                   MOVEL     '0'           WRECOK            1            *RECORD VALIDO
029800990802     C     *IN99         DOWEQ     *OFF
029900990802     C     WRECOK        ANDEQ     '0'
030000990802      *
030100990802      * LETTURA
030200990802     C     KEYTA0        READE     TITASV1L                               99
030300990802      *
030400990802      * FINE FILE  O TABELLA DIVERSA
030500990802     C     *IN99         IFEQ      '1'
030600990802     C                   Z-ADD     1             SFLEOF
030700990802     C                   ELSE
030800990802     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
030900990802     C                   ENDIF
031000990802      *
031100990802     C                   ENDDO
031200990802      *
031300990802     C                   ENDSR
031400990802      *------------------------------------------------------------------------*
031500990802      * CHKREC - CONTROLLA VALIDITA' RECORD
031600990802      *------------------------------------------------------------------------*
031700990802     C     CHKREC        BEGSR
031800990802      *
031900990802     C                   MOVEL     '1'           WRECOK                         RECORD OK IN GENERAL
032000990802      *
032100990802     C                   ENDSR
032200990802      *------------------------------------------------------------------------*
032300990802      * IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
032400990802      *------------------------------------------------------------------------*
032500990802     C     IRESFL        BEGSR
032600990802      *
032700990802     C     *iso          movel     TASDTV        data_eur                       *DATA SPEDIZIONE
032800990802     C     *eur          movel     data_eur      VSCDTV                         * GG/MM/AAAA
032900990802     C                   Z-ADD     TASORV        VSCORV
033000990802     C                   MOVEL     TASPRU        VSCPRU
033100990802     C                   MOVEL     TASTBL        VSCTBL
033200990802     C                   MOVEL     TASTSP        VSCTSP
033300990802     C                   Z-ADD     TASKSC        VSCKSC
033400990802     C                   Z-ADD     TASCTR        VSCCTR
033500990802     C                   MOVEL     TASFPF        VSCFPF
033600990802     C                   Z-ADD     TASPKF        VSCPKF
033700990802     C                   MOVEL     TASFVF        VSCFVF
033800990802     C                   Z-ADD     TASVLF        VSCVLF
033900990802      * CONSEGNE PARTICOLARI
034000990802     C                   MOVEL     '1P'          KCOD
034100990802     C                   CLEAR                   VSCFTC
034200990802     C                   CLEAR                   VSCTC2
034300990802      *
034400990802     C     TASFTC        IFNE      *BLANKS
034500990802     C                   MOVEL(P)  TASFTC        KKEY
034600990802     C     KTAB          CHAIN     TABEL                              30
034700990802     C     *IN30         IFEQ      *OFF
034800990802     C                   MOVEL     TBLUNI        W007A             7
034900990802     C                   MOVEL     ' -'          VSCFTC
035000990802     C                   MOVEL     TASFTC        VSCFTC
035100990802     C                   MOVE      W007A         VSCFTC
035200990802     C                   ENDIF
035300990802     C                   ENDIF
035400990802      *
035500990802     C     TASTC2        IFNE      *BLANKS
035600990802     C                   MOVEL(P)  TASTC2        KKEY
035700990802     C     KTAB          CHAIN     TABEL                              30
035800990802     C     *IN30         IFEQ      *OFF
035900990802     C                   MOVEL     TBLUNI        W008A             8
036000990802     C                   MOVEL     ' -'          VSCTC2
036100990802     C                   MOVEL     TASTC2        VSCTC2
036200990802     C                   MOVE      W008A         VSCTC2
036300990802     C                   ENDIF
036400990802     C                   ENDIF
036500990802      *
036600990802     C                   ENDSR
036700990802      *------------------------------------------------------------------------*
036800990802      * LANPGM - LANCIA UN PROGRAMMA
036900990802      *------------------------------------------------------------------------*
037000990802     C     LANPGM        BEGSR
037100990802      *--------------------
037200990802      * PARAMETRI INFORMATIVI
037300990802      *--------------------
037400990802     C                   MOVEL     WOP0          D01OP0                         *LIVELLO PROCEDURA
037500990802     C                   MOVEL     WOP1          D01OP1                         *LIVELLO PROGRAMMA
037600990802     C                   MOVEL     '0'           D01F03                         *USCITA PGM
037700990802     C                   MOVEL     '0'           D01F12                         *USCITA PGM
037800990802     C                   MOVEL     '0'           D01ERR                         *ERRORE
037900990802     C                   MOVEL     *BLANKS       D01MSG                         *MESSAGGIO ERRORE
038000990802      *--------------------
038100990802      * PARAMETRI
038200990802      *--------------------
038300990802     C                   Z-ADD     WAAS          D01AAS                         *ANNO
038400990802     C                   Z-ADD     WLNP          D01LNP                         *LNP
038500990802     C                   Z-ADD     WNRS          D01NRS                         *SERIE
038600990802     C                   Z-ADD     WNSP          D01NSP                         *SPEDIZIONE
038700990802     C                   Z-ADD     WDSP          D01DSP                         *DATA SPEDIZIONE GMA
038800990802     C                   Z-ADD     WDTV          D01DTV                         *DATA VARIAZIONE AMG
038900990802     C                   Z-ADD     WORV          D01ORV                         *ORA
039000990802     C                   MOVEL     WPRU          D01PRU                         *UTENTE
039100990802      * LANCIO
039200990802     C                   MOVEL     TNSB01DS      KPJBU
039300990802     C                   CALL      WPGM                                 99
039400990802     C                   PARM                    KPJBA
039500990802     C                   MOVEL     KPJBU         TNSB01DS
039600990802      *
039700990802      * SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
039800990802     C     D01F03        IFEQ      '1'
039900990802     C                   MOVEL     '1'           $F03
040000990802     C                   MOVEL     '1'           $ERR
040100990802     C                   MOVEL     '1'           $FINE
040200990802      *
040300990802      * ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
040400990802     C                   ELSE
040500990802     C                   MOVEL     D01MSG        $MSG
040600990802     C                   EXSR      CARMSG
040700990802     C                   ENDIF
040800990802      *
040900990802      * PREMUTO F12 DAL CHIAMATO
041000990802     C     D01F12        IFEQ      '1'
041100990802     C                   MOVEL     '1'           $ERR
041200990802     C                   ENDIF
041300990802      *
041400990802     C                   CLEAR                   D01OP1
041500990802     C                   MOVEL     '0'           D01F12
041600990802      *
041700990802     C                   ENDSR
041800990802      *------------------------------------------------------------------------*
041900990802      *  *INZSR - ROUTINE INIZIALE
042000990802      *------------------------------------------------------------------------*
042100990802     C     *INZSR        BEGSR
042200990802      *
042300990802      * RICEVIMENTO PARAMETRI
042400990802     C     *ENTRY        PLIST
042500990802     C                   PARM                    KPJBA
042600990802     C                   MOVEL     KPJBU         TNSB01DS
042700990802      *
042800990802      * CHIAVE LETTURA ECEVD01L - PARZIALE
042900990802     C     KEYTA0        KLIST                                                  TITAS30C
043000990802     C                   KFLD                    Ktasaas                        *ANNO
043100990802     C                   KFLD                    Ktaslnp                        *LNP
043200990802     C                   KFLD                    Ktasnrs                        *SERIE
043300990802     C                   KFLD                    Ktasnsp                        *SPEDIZIONE
043400990802      * CHIAVE LETTURA ECEVD01L - PARZIALE
043500990802     C     KTAB          KLIST                                                  TABEL00F
043600990802     C                   KFLD                    CODUT             1 0
043700990802     C                   KFLD                    KCOD              2
043800990802     C                   KFLD                    KKEY              8
043900990802      *
044000990802     C                   Z-ADD     1             CODUT
044100990802      *
044200990802      * V02 --> GESTIONE;  V05 --> INTERROGAZIONE
044300990802     C                   MOVE      D01OP0        INTGES            2
044400990802     C     INTGES        IFEQ      '02'
044500990802     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
044600990802     C                   ELSE
044700990802     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
044800990802     C                   ENDIF
044900990802      *
045000990802      * ESEGUE OPERAZIONI INIZIALI STANDARD
045100990802     C                   EXSR      INZSTD
045200020724     C     $Fine         cabeq     *on           EndINZ
045300990802      *
045400990802      * ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
045500990802     C                   EXSR      INZSFL
045600990802      *
045700990802      * IMPOSTO CAMPI TESTATA VIDEO
045800020724     C                   MOVEL     RSUT          VTDRSU                         *RAGIONE SOC. UTENTE
045900990802      *
046000990802      * IMPOSTO ANCHE I DEPOSITI PER CONFRONTI SUCCESSIVI
046100990802     C                   Z-ADD     D01AAS        VCCAAS                         *ANNO
046200990802     C                   Z-ADD     VCCAAS        DEPAAS                         *DEPOSITO
046300990802     C                   Z-ADD     D01LNP        VCCLNP                         *LNP
046400990802     C                   Z-ADD     D01NRS        VCCNRS                         *SERIE
046500990802     C                   Z-ADD     D01NSP        VCCNSP                         *SPEDIZIONE
046600990802     C                   Z-ADD     D01DSP        VCCDSP                         *DATA SPEDIZIONE
046700990802      *
046800990802      * EMETTO VIDEATA SUBFILE
046900990802     C                   EXSR      CARVS                                        CARICA E EMETTE SFIL
047000990802      *
047100020724     C     EndINZ        tag
047200990802     C                   ENDSR
047300990802      *------------------------------------------------------------------------*
047400990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047500990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047600990802      *    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
047700990802      *    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
047800990802      *    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
047900990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048000990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048100990802      *------------------------------------------------------------------------*
048200990802      * INZSTD - ROUTINE INIZIALE STANDARD
048300990802      *------------------------------------------------------------------------*
048400990802     C     INZSTD        BEGSR
048500990802      * CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
048600990802     C                   CALL      'XKEYS'
048700990802     C                   PARM                    DSKEYS
048800990802     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
048900990802      * PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
049000990802     C     XMSGPA        PLIST
049100990802     C                   PARM                    DSMSMS
049200990802     C                   PARM                    DSMSPG
049300990802     C                   PARM                    DSMSKE
049400990802     C                   PARM                    DSMSER
049500990802      * IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
049600990802     C                   MOVEL     'TNSB53R'     DSMSPG                         *NOME PGM CHIAMANTE
049700990802     C                   MOVEL     'TNSB53R'     VMCQUE                         *CODA MESSAGGI DESTI
049800990802     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
049900990802     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
050000990802      * INIZIALIZZO FLAG CONTROLLO PGM
050100990802     C                   MOVEL     '0'           $ERR              1            *ERRORE
050200990802     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
050300990802     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
050400990802     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
050500990802     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
050600990802     C                   MOVEL     '0'           $FINE             1            *FINE PGM
050700990802     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
050800990802      * POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
050900990802     C                   BITOFF    '01234567'    POSMI1            1
051000990802     C                   BITOFF    '01234567'    POSMI2            1
051100990802     C                   BITOFF    '01234567'    POSMA1            1
051200990802     C                   BITOFF    '01234567'    POSMA2            1
051300990802     C                   BITON     '34'          POSMI1            1            *18
051400990802     C                   BITON     '7'           POSMI2            1            *01
051500990802     C                   BITON     '34'          POSMA1            1            *18
051600990802     C                   BITON     '0'           POSMA2            1            *80
051700990802     C                   MOVE      POSMI2        POSMIN            2
051800990802     C                   MOVEL     POSMI1        POSMIN
051900990802     C                   MOVE      POSMA2        POSMAX            2
052000990802     C                   MOVEL     POSMA1        POSMAX
052100020724      *
052200020724      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
052300020724     c     *dtaara       define    §azute        azuteds
052400020724     c     *dtaara       define    §datiute      ddatiute
052500020724      *
052600020724     c                   in(E)     *dtaara
052700020724if  1c                   IF        %Error  or  RSUT = *blanks
052800020724     c                   call      'TIBS34R'
052900020724     c                   parm                    Tibs34Ds
053000020724     c                   in        *dtaara
053100020724e   1c                   ENDIF
053200020724      *-- Verifica errori e autorità profilo
053300020724sel 1C                   SELECT
053400020724      *-- controllo se ho errori nei dati utente
053500020724      *--   nel qual caso NON risulta un profilo abilitato
053600020724w   1C                   WHEN      DUTERR = 'E'
053700020724     c                   eval      D01msg = DUTMSG
053800020724     C                   eval      $Fine  = *on
053900020724      *
054000020724      *-- CONTROLLO AUTORITA'
054100020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
054200020724      *-- se il chiamante non richiede autorità specifica verificare
054300020724      *--   quella del profilo
054400020724      *-- se il chiamante richiede autorità specifica verificarla,
054500020724      *--  se è blank verificare quella del profilo
054600020724      *
054700020724      * se UTEAUT = *BLANK non siamo in sede
054800020724w   1C                   WHEN      UTEAUT = *blank
054900020724      *
055000020724x   1c                   OTHER
055100020724      *--
055200020724     c                   movel     UTEFAF        DUTE01
055300020724      *-- Autorizzazione alla manutenzione delle bolle di sede
055400020724     c                   clear                   tibs02ds
055500020724     c                   movel     'C'           t02mod
055600020724     c                   movel     knsif         t02sif
055700020724     c                   movel     'LAT'         t02cod
055800020724if  2c                   IF        §UTEibs <> *blanks
055900020724      *--   Con AUTORITÀ SPECIFICA
056000020724     c                   movel(p)  §UTEibs       t02ke1
056100020724x   2c                   ELSE
056200020724      *--   Senza AUTORITÀ SPECIFICA
056300020724     c                   movel(p)  UTEAUT        t02ke1
056400020724e   2c                   ENDIF
056500020724      *
056600020724     c                   call      'TIBS02R'
056700020724     c                   parm                    kpjba
056800020724     c                   parm                    tibs02ds
056900020724if  2c                   if        t02err = *blanks
057000020724     c                   movel     t02uni        DLAT
057100020724e   2c                   endif
057200020724if  2c                   if        t02err <> *blanks   or
057300020724     c                             §LATabi = 'S'
057400020724     c                   eval      D01msg  = MSG(7)
057500020724     c                   eval      $Fine   = *on
057600020724e   2c                   endif
057700020724      *
057800020724e   1C                   ENDSL
057900020724      *
058000990802     C                   ENDSR
058100990802      *------------------------------------------------------------------------*
058200990802      * FUNSTD - LANCIA SUBROUTINE STANDARD
058300990802      *------------------------------------------------------------------------*
058400990802     C     FUNSTD        BEGSR
058500990802     C                   SELECT
058600990802     C     KEY           WHENEQ    F3
058700990802     C                   EXSR      F03STD
058800990802     C     KEY           WHENEQ    F12
058900990802     C                   EXSR      F12STD
059000990802     C     KEY           WHENEQ    F24
059100990802     C                   EXSR      F24STD
059200990802     C                   OTHER
059300990802     C                   MOVEL     MSG(2)        $MSG
059400990802     C                   EXSR      CARMSG
059500990802     C                   ENDSL
059600990802     C                   ENDSR
059700990802      *------------------------------------------------------------------------*
059800990802      * F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
059900990802      *------------------------------------------------------------------------*
060000990802     C     F03STD        BEGSR
060100990802      * IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
060200990802     C                   MOVEL     '1'           $F03
060300990802      * IMPOSTO FLAG DI FINE PROGRAMMA
060400990802     C                   MOVEL     '1'           $FINE
060500990802     C                   ENDSR
060600990802      *------------------------------------------------------------------------*
060700990802      * F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
060800990802      *------------------------------------------------------------------------*
060900990802     C     F12STD        BEGSR
061000990802      * IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
061100990802     C                   MOVEL     '1'           $F12
061200990802      * IMPOSTO FLAG DI FIN PGM
061300990802     C                   MOVEL     '1'           $FINE
061400990802     C                   ENDSR
061500990802      *------------------------------------------------------------------------*
061600990802      * F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
061700990802      *------------------------------------------------------------------------*
061800990802     C     F24STD        BEGSR
061900990802      * GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
062000990802     C                   EXSR      OUTFUN
062100990802     C                   ENDSR
062200990802      *------------------------------------------------------------------------*
062300990802      * INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
062400990802      *------------------------------------------------------------------------*
062500990802     C     INZSFL        BEGSR
062600990802      * DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
062700990802     C     *LIKE         DEFINE    VCCNBR        SFLPAG
062800990802     C     *LIKE         DEFINE    VCCNBR        SFLEOF
062900990802     C     *LIKE         DEFINE    VCCNBR        SFLERR
063000990802     C     *LIKE         DEFINE    VCCNBR        SFLULT
063100990802     C     *LIKE         DEFINE    VCCNBR        WFLULT
063200990802     C     *LIKE         DEFINE    VCCNBR        SFLRIG
063300990802      * INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
063400990802      * (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
063500990802     C                   Z-ADD     14            SFLPAG
063600990802     C                   ENDSR
063700990802      *------------------------------------------------------------------------*
063800990802      * FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
063900990802      *------------------------------------------------------------------------*
064000990802     C     FUNSFL        BEGSR
064100990802     C                   SELECT
064200990802     C     KEY           WHENEQ    ENTER
064300990802     C                   EXSR      ENTSFL
064400990802     C     KEY           WHENEQ    F3
064500990802     C                   EXSR      F03STD
064600990802     C     KEY           WHENEQ    F5
064700990802     C                   EXSR      F05STD
064800990802     C     KEY           WHENEQ    F12
064900990802     C                   EXSR      F12STD
065000990802     C     KEY           WHENEQ    F17
065100990802     C                   EXSR      F17STD
065200990802     C     KEY           WHENEQ    F18
065300990802     C                   EXSR      F18STD
065400990802     C     KEY           WHENEQ    F23
065500990802     C                   EXSR      F23STD
065600990802     C     KEY           WHENEQ    F24
065700990802     C                   EXSR      F24STD
065800990802     C     KEY           WHENEQ    ROLLUP
065900990802     C                   EXSR      RUPSTD
066000990802     C     KEY           WHENEQ    ROLLDN
066100990802     C                   EXSR      RDNSTD
066200990802     C                   OTHER
066300990802     C                   MOVEL     MSG(2)        $MSG
066400990802     C                   EXSR      CARMSG
066500990802     C                   ENDSL
066600990802     C                   ENDSR
066700990802      *------------------------------------------------------------------------*
066800990802      * F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
066900990802      *------------------------------------------------------------------------*
067000990802     C     F05STD        BEGSR
067100990802      * LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
067200990802     C                   EXSR      POSSFL
067300990802     C                   ENDSR
067400990802      *------------------------------------------------------------------------*
067500990802      * F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
067600990802      *------------------------------------------------------------------------*
067700990802     C     F17STD        BEGSR
067800990802      * IMPOSTA SFLRCDNBR PER PRIMA PAGINA
067900990802     C                   Z-ADD     1             VCCNBR
068000990802     C                   ENDSR
068100990802      *------------------------------------------------------------------------*
068200990802      * F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
068300990802      *------------------------------------------------------------------------*
068400990802     C     F18STD        BEGSR
068500990802      * IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
068600990802     C                   Z-ADD     SFLULT        VCCNBR
068700990802     C                   ENDSR
068800990802      *------------------------------------------------------------------------*
068900990802      * F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
069000990802      *------------------------------------------------------------------------*
069100990802     C     F23STD        BEGSR
069200990802      * GESTIONE VISUALIZZAZIONE OPZIONI
069300990802     C                   EXSR      OUTOPZ
069400990802     C                   ENDSR
069500990802      *------------------------------------------------------------------------*
069600990802      * RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
069700990802      *------------------------------------------------------------------------*
069800990802     C     RUPSTD        BEGSR
069900990802      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
070000990802      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
070100990802     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
070200990802     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
070300990802     C                   ELSE
070400990802     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
070500990802     C                   ENDIF
070600990802     C                   ENDSR
070700990802      *------------------------------------------------------------------------*
070800990802      * RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
070900990802      *------------------------------------------------------------------------*
071000990802     C     RDNSTD        BEGSR
071100990802      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
071200990802      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
071300990802     C     CURSOR        IFGE      POSMIN
071400990802     C     CURSOR        ANDLE     POSMAX
071500990802     C                   ELSE
071600990802      * INVIO MESSAGGIO DI INIZIO ELENCO
071700990802     C                   MOVEL     '1'           $ERR
071800990802     C                   MOVEL     MSG(5)        $MSG
071900990802     C                   EXSR      CARMSG
072000990802     C                   ENDIF
072100990802     C                   ENDSR
072200990802      *------------------------------------------------------------------------*
072300990802      * ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
072400990802      *------------------------------------------------------------------------*
072500990802     C     ENTSFL        BEGSR
072600990802      * CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
072700990802     C                   EXSR      NEWPOS
072800990802      * SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
072900990802     C     $ERR          IFEQ      '0'                                          --- 1 -->
073000990802      * SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
073100990802     C     SWPOSI        IFNE      0                                            --- 2 -->
073200990802     C                   EXSR      POSSFL
073300990802     C                   ELSE                                                   <-- 2 -->
073400990802      * GESTIONE SFL CON TASTO FUNZIONALE ENTER
073500990802     C     SFLULT        IFNE      0                                            --- 3 -->
073600990802     C                   EXSR      OPZSFL
073700990802      * SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
073800990802     C     SWMODI        IFNE      0                                            --- 4 -->
073900990802     C                   EXSR      POSSFL
074000990802     C                   ENDIF                                                  <-- 4 ---
074100990802     C                   ENDIF                                                  <-- 3 ---
074200990802     C                   ENDIF                                                  <-- 2 ---
074300990802     C                   ENDIF                                                  <-- 1 ---
074400990802     C                   ENDSR
074500990802      *------------------------------------------------------------------------*
074600990802      * POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
074700990802      *------------------------------------------------------------------------*
074800990802     C     POSSFL        BEGSR
074900990802      * CLEAR SFL (ELIMINA CONTENUTO SFL)
075000990802     C                   EXSR      CLRSFL
075100990802      * CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
075200990802     C                   EXSR      SETFIL
075300990802      * SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
075400990802     C     SFLEOF        IFEQ      0
075500990802     C                   EXSR      CARSFL
075600990802     C                   ENDIF
075700990802     C                   ENDSR
075800990802      *------------------------------------------------------------------------*
075900990802      * OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
076000990802      *------------------------------------------------------------------------*
076100990802     C     OPZSFL        BEGSR
076200990802      * AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
076300990802     C                   MOVEL     'S'           OPZOK             1
076400990802      * AZZERO CONTATORE DI RIGHE SFL VARIATE
076500990802     C                   Z-ADD     0             SFLVAR            5 0
076600990802      * INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
076700990802     C                   Z-ADD     0             SFLERR
076800990802      * SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
076900990802     C                   Z-ADD     SFLULT        WFLULT
077000990802      * SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
077100990802     C                   READC     SB53DVS                                49
077200990802     C     *IN49         DOWEQ     '0'                                          --- 1 -->
077300990802     C     $ERR          ANDEQ     '0'
077400990802      * ADD CONTATORE DI RIGHE SFL VARIATE
077500990802     C                   ADD       1             SFLVAR
077600990802      * SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
077700990802     C     OPZOK         IFEQ      'S'                                          --- 2 -->
077800990802      * CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
077900990802     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
078000990802     C                   EXSR      CONOPZ
078100990802     C     OPZOK         IFEQ      'S'
078200990802     C     $ERR          ANDEQ     '0'
078300990802     C                   EXSR      GESOPZ
078400990802     C                   ENDIF
078500990802     C                   ENDIF                                                  <-- 3 ---
078600990802      * SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
078700990802      *            2. PULIZIA SCELTA IN SFL
078800990802     C     OPZOK         IFEQ      'S'                                          --- 3 -->
078900990802     C                   MOVEL     '0'           *IN41
079000990802     C                   MOVEL     *BLANKS       VSCOPZ
079100990802     C                   ELSE                                                   <-- 3 -->
079200990802      * SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
079300990802      *            2. IMPOSTO RECORD CON PRIMO ERRORE
079400990802     C                   MOVEL     '1'           *IN41
079500990802     C                   Z-ADD     SFLULT        SFLERR
079600990802     C                   ENDIF                                                  <-- 3 ---
079700990802     C                   ELSE                                                   <-- 2 -->
079800990802      * SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
079900990802      *   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
080000990802     C                   MOVEL     '1'           *IN41
080100990802     C                   ENDIF                                                  <-- 2 ---
080200990802      * IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
080300990802      *   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
080400990802      *   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
080500990802     C     OPZOK         IFNE      'S'
080600990802     C                   Z-ADD     SFLERR        VCCNBR
080700990802     C                   ELSE
080800990802     C                   Z-ADD     SFLULT        VCCNBR
080900990802     C                   ENDIF
081000990802      * AGGIORNO RECORD SFL
081100990802     C                   UPDATE    SB53DVS
081200990802     C     $ERR          IFEQ      '0'
081300990802     C                   READC     SB53DVS                                49
081400990802     C                   ENDIF
081500990802     C                   ENDDO                                                  <-- 1 ---
081600990802      * REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
081700990802     C                   Z-ADD     WFLULT        SFLULT
081800990802      * SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
081900990802     C     SFLVAR        IFEQ      0
082000990802     C                   MOVEL     '1'           $FINE
082100990802     C                   ENDIF
082200990802     C                   ENDSR
082300990802      *------------------------------------------------------------------------*
082400990802      * CLRSFL - ELIMINAZIONE CONTENUTO SFL
082500990802      *------------------------------------------------------------------------*
082600990802     C     CLRSFL        BEGSR
082700990802      * INIZIALIZZO SWITCH DI FINE FILE "FISICO"
082800990802      * (0=NON FINE FILE  1=FINE FILE)
082900990802     C                   Z-ADD     0             SFLEOF
083000990802      * INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
083100990802     C                   Z-ADD     0             SFLULT
083200990802      * ACCENSIONE INDICATORE PER CLEAR SFL
083300990802     C                   MOVEL     '1'           *IN40
083400990802     C                   WRITE     SB53DVC
083500990802     C                   MOVEL     '0'           *IN40
083600990802     C                   ENDSR
083700990802      *------------------------------------------------------------------------*
083800990802      * CARSFL - CARICAMENTO SFL
083900990802      *------------------------------------------------------------------------*
084000990802     C     CARSFL        BEGSR
084100990802      * SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
084200990802     C     SFLULT        IFEQ      0
084300990802     C                   EXSR      SETFIL
084400990802     C                   ENDIF
084500990802      * SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
084600990802     C                   Z-ADD     SFLULT        WFLULT
084700990802      * CICLO:
084800990802      *    -CARICO NUMERO DI RIGHE=SFLPAG
084900990802      *    -SFLEOF=0 (NON FINE FILE "FISICO")
085000990802     C                   Z-ADD     1             SFLRIG
085100990802     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
085200990802     C     SFLEOF        ANDEQ     0
085300990802      * IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
085400990802     C                   MOVEL     *BLANKS       VSCOPZ
085500990802      * IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
085600990802     C                   EXSR      IRESFL
085700990802      * AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
085800990802     C                   ADD       1             SFLULT
085900990802      * SCRITTURA RECORD SFL
086000990802     C                   WRITE     SB53DVS
086100990802     C                   ADD       1             SFLRIG
086200990802      * LETTURA FILE DA CARICARE
086300990802     C                   EXSR      LETFIL
086400990802     C                   ENDDO                                                  <-- 1 ---
086500990802      * IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
086600990802     C     WFLULT        IFNE      SFLULT
086700990802     C                   Z-ADD     WFLULT        VCCNBR
086800990802     C                   ADD       1             VCCNBR
086900990802     C                   ENDIF
087000990802      * SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
087100990802     C     SFLEOF        IFEQ      1                                            --- 1 -->
087200990802     C     SFLULT        ANDGT     *ZEROS
087300990802     C                   MOVEL     '1'           $ERR
087400990802     C                   MOVEL     MSG(6)        $MSG
087500990802     C                   EXSR      CARMSG
087600990802      * AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
087700990802      ******     SFLULT    CHAINSB53DVS              99
087800990802      ******     *IN99     IFEQ '0'
087900990802      ******               SETON                     45
088000990802      ******               UPDATSB53DVS
088100990802      ******               SETOF                     45
088200990802      ******               ENDIF
088300990802     C                   ENDIF                                                  <-- 1 ---
088400990802     C                   ENDSR
088500990802      *------------------------------------------------------------------------*
088600990802      * OUTSFL - EMISSIONE SUBFILE A VIDEO
088700990802      *------------------------------------------------------------------------*
088800990802     C     OUTSFL        BEGSR
088900990802      * SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
089000990802     C     MSGULT        IFNE      *ZEROS
089100990802     C                   EXSR      OUTMSG
089200990802     C                   ENDIF
089300990802      * GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
089400990802      * SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
089500990802     C     SFLEOF        IFEQ      0
089600990802     C                   MOVEL     '0'           *IN42
089700990802     C                   ELSE
089800990802      * SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
089900990802     C                   MOVEL     '1'           *IN42
090000990802     C                   ENDIF
090100990802      * EMISSIONE SUBFILE
090200990802     C     SFLULT        IFLE      0
090300990802      *  --> CASO DI SFL VUOTO
090400990802     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
090500990802     C                   WRITE     SB53DVV                                      *'NON VI SONO DATI'
090600990802     C                   WRITE     SB53DVC                                      *SFL CONTROL
090700990802     C                   READ      SB53DVC                                99    *SFL CONTROL
090800990802     C                   ELSE
090900990802      *  --> CASO DI SFL CON RECORDS
091000990802     C                   WRITE     SB53DVC                                      *SFL CONTROL
091100990802     C                   READ      SB53DVC                                99    *SFL CONTROL
091200990802     C                   ENDIF
091300990802      * MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
091400990802      * (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
091500990802     C                   Z-ADD     FRTSFL        VCCNBR
091600990802     C                   ENDSR
091700990802      *------------------------------------------------------------------------*
091800990802      * CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
091900990802      *          CODA MESSAGGI DEL PGM
092000990802      *------------------------------------------------------------------------*
092100990802     C     CARMSG        BEGSR
092200990802      * SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
092300990802      * PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
092400990802     C     MSGULT        IFEQ      0
092500990802     C                   MOVEL     '1'           *IN40
092600990802     C                   WRITE     SB53DVK
092700990802     C                   MOVEL     '0'           *IN40
092800990802     C                   ENDIF
092900990802      * INVIA MESSAGGIO ALLA CODA DEL PGM
093000990802     C                   MOVEL     $MSG          DSMSMS
093100990802     C                   CALL      'XMSG'        XMSGPA
093200990802     C     DSMSER        IFEQ      0
093300990802     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
093400990802     C                   ENDIF
093500990802      * CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
093600990802     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
093700990802     C                   WRITE     SB53DVM                                      *SFL MESS
093800990802      * PULISCO CAMPO MESSAGGIO APPENA CARICATO
093900990802     C                   MOVEL     *BLANKS       $MSG
094000990802     C                   ENDSR
094100990802      *------------------------------------------------------------------------*
094200990802      * OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
094300990802      *------------------------------------------------------------------------*
094400990802     C     OUTMSG        BEGSR
094500990802      * VISUALIZZAZIONE MESSAGGIO
094600990802     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
094700990802     C                   WRITE     SB53DVK                                      *CONTR MESS
094800990802      * AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
094900990802     C                   Z-ADD     *ZEROS        MSGULT
095000990802     C                   ENDSR
095100990802      *------------------------------------------------------------------------*
095200990802      * CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
095300990802      *          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
095400990802      *          OUTPUT: - FFU    (SCHIERA FUNZIONI)
095500990802      *                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
095600990802      *                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
095700990802      *------------------------------------------------------------------------*
095800990802     C     CARFUN        BEGSR
095900990802     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
096000990802     C     1             DO        31            I                 5 0          --- 1 -->
096100990802     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
096200990802     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
096300990802     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
096400020724     C     DUTlpo        IFEQ      'S'                                          --- 2 --> *SEDE
096500990802     C     $TGES         IFEQ      'G'                                          *GESTIONE
096600990802     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
096700990802     C                   ELSE                                                   *INTERROG.
096800990802     C                   MOVEL     FUNASI        FAT(I)
096900990802     C                   ENDIF
097000990802     C                   ELSE                                                   <-- 2 --> *FILIALE
097100990802     C     $TGES         IFEQ      'G'                                          *GESTIONE
097200990802     C                   MOVEL     FUNAFG        FAT(I)
097300990802     C                   ELSE                                                   *INTERROG.
097400990802     C                   MOVEL     FUNAFI        FAT(I)
097500990802     C                   ENDIF
097600990802     C                   ENDIF                                                  <-- 2 ---
097700990802     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
097800990802     C                   MOVEL     USER          WEDP              3            *PROFILO
097900990802     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
098000990802     C                   MOVEL     'S'           FAT(I)                         *ABILITO
098100990802     C                   ELSE
098200990802     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
098300990802     C                   ENDIF
098400990802     C                   ENDIF
098500990802     C                   ENDDO                                                  <-- 1 ---
098600990802     C                   ENDSR
098700990802      *------------------------------------------------------------------------*
098800990802      * OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
098900990802      *          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
099000990802      *          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
099100990802      *                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
099200990802      *------------------------------------------------------------------------*
099300990802     C     OUTFUN        BEGSR
099400990802     C     1             DO        8             I                              *PULIZIA FUNZIONI A
099500990802     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
099600990802     C                   ENDDO
099700990802      * CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
099800990802     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
099900990802     C     1             DO        31            I
100000990802     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
100100990802     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
100200990802     C                   ADD       1             FUNNAT
100300990802     C                   ENDIF
100400990802     C                   ENDDO
100500990802      * SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
100600990802     C     FUNNAT        IFGT      8
100700990802     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
100800990802     C                   ENDIF
100900990802      * IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
101000990802      * (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
101100990802     C                   Z-ADD     8             FUNVIS            5 0
101200990802     C     FAT(23)       IFEQ      'S'
101300990802     C                   SUB       1             FUNVIS
101400990802     C                   ENDIF
101500990802     C     FAT(24)       IFEQ      'S'
101600990802     C                   SUB       1             FUNVIS
101700990802     C                   ENDIF
101800990802      * AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
101900990802     C                   Z-ADD     *ZEROS        W                 5 0
102000990802     C                   ADD       1             FUNULT
102100990802     C     FUNULT        IFGT      31
102200990802     C                   Z-ADD     1             FUNULT
102300990802     C                   ENDIF
102400990802      * RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
102500990802     C     FUNULT        DO        31            I                              --- 1 -->
102600990802      * SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
102700990802     C     W             IFLT      FUNVIS                                       --- 2 -->
102800990802      * SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
102900990802     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
103000990802     C     FDE(I)        ANDNE     *BLANKS
103100990802     C     I             IFEQ      23                                           *TOLGO F23
103200990802     C     I             OREQ      24                                           *E F24
103300990802     C                   ELSE
103400990802     C                   ADD       1             W
103500990802     C                   MOVEL     FDE(I)        FUV(W)
103600990802     C                   ENDIF
103700990802     C                   ENDIF                                                  <-- 3 ---
103800990802      * MEMORIZZO L'ULTIMO INDICE TRATTATO
103900990802     C                   Z-ADD     I             FUNULT
104000990802     C                   ENDIF                                                  <-- 2 ---
104100990802     C                   ENDDO                                                  <-- 1 ---
104200990802      * TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
104300990802     C                   MOVEL     FUV(1)        VFD001
104400990802     C                   MOVEL     FUV(2)        VFD002
104500990802     C                   MOVEL     FUV(3)        VFD003
104600990802     C                   MOVEL     FUV(4)        VFD004
104700990802     C                   MOVEL     FUV(5)        VFD005
104800990802     C                   MOVEL     FUV(6)        VFD006
104900990802     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
105000990802     C     FAT(24)       ANDEQ     'N'
105100990802     C                   MOVEL     FUV(7)        VFD007
105200990802     C                   MOVEL     FUV(8)        VFD008
105300990802     C                   ELSE                                                   <-- 1 -->
105400990802     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
105500990802     C     FAT(24)       ANDEQ     'S'
105600990802     C                   MOVEL     FDE(23)       VFD007
105700990802     C                   MOVEL     FDE(24)       VFD008
105800990802     C                   ELSE                                                   <-- 2 -->
105900990802     C                   MOVEL     FUV(7)        VFD007
106000990802     C     FAT(23)       IFEQ      'S'
106100990802     C                   MOVEL     FDE(23)       VFD008
106200990802     C                   ELSE
106300990802     C                   MOVEL     FDE(24)       VFD008
106400990802     C                   ENDIF
106500990802     C                   ENDIF                                                  <-- 2 ---
106600990802     C                   ENDIF                                                  <-- 1 ---
106700990802     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
106800990802     C                   ENDSR
106900990802      *------------------------------------------------------------------------*
107000990802      *  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
107100990802      *           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
107200990802      *           OUTPUT: - FUNOK  (OPZIONE OK S/N)
107300990802      *                   - ERRORE SE FUNZIONE NON ATTIVA
107400990802      *------------------------------------------------------------------------*
107500990802     C     CONFUN        BEGSR
107600990802      * RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
107700990802     C     1             DO        31            I                              --- 1 -->
107800990802     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
107900990802      * CONTROLLO SE IL TASTO E' ATTIVO
108000990802     C     FAT(I)        IFNE      'S'                                          --- 3 -->
108100990802     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
108200990802     C                   EXSR      CARMSG
108300990802     C                   MOVEL     '1'           $ERR
108400990802     C                   ENDIF                                                  <-- 3 ---
108500990802     C                   ENDIF                                                  <-- 2 ---
108600990802     C                   ENDDO                                                  <-- 1 ---
108700990802     C                   ENDSR
108800990802      *------------------------------------------------------------------------*
108900990802      * CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
109000990802      *          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
109100990802      *          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
109200990802      *                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
109300990802      *                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
109400990802      *                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
109500990802      *------------------------------------------------------------------------*
109600990802     C     CAROPZ        BEGSR
109700990802     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
109800990802     C     1             DO        24            I                              --- 1 -->
109900990802     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
110000990802     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
110100990802     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
110200990802     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
110300020724     C     DUTlpo        IFEQ      'S'                                          --- 2 --> *SEDE
110400990802     C     $TGES         IFEQ      'G'                                          *GESTIONE
110500990802     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
110600990802     C                   ELSE                                                   *INTERROG.
110700990802     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
110800990802     C                   ENDIF
110900990802     C                   ELSE                                                   <-- 2 --> *FILIALE
111000990802     C     $TGES         IFEQ      'G'                                          *GESTIONE
111100990802     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
111200990802     C                   ELSE                                                   *INTERROG.
111300990802     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
111400990802     C                   ENDIF
111500990802     C                   ENDIF                                                  <-- 2 ---
111600990802     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
111700990802     C                   MOVEL     USER          WEDP              3            *PROFILO
111800990802     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
111900990802     C                   MOVEL     'S'           OAT(I)                         *ABILITO
112000990802     C                   ELSE
112100990802     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
112200990802     C                   ENDIF
112300990802     C                   ENDIF
112400990802     C                   ENDDO                                                  <-- 1 ---
112500990802     C                   ENDSR
112600990802      *------------------------------------------------------------------------*
112700990802      * OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
112800990802      *          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
112900990802      *          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
113000990802      *                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
113100990802      *                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
113200990802      *------------------------------------------------------------------------*
113300990802     C     OUTOPZ        BEGSR
113400990802     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
113500990802     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
113600990802     C                   ENDDO
113700990802     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
113800990802      * AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
113900990802     C                   ADD       1             OPZULT
114000990802     C     OPZULT        IFGT      24
114100990802     C                   Z-ADD     1             OPZULT
114200990802     C                   END
114300990802      * CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
114400990802     C     1             DO        24            I
114500990802     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
114600990802     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
114700990802     C                   ADD       1             OPZNAT
114800990802     C                   ENDIF
114900990802     C                   ENDDO
115000990802      * RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
115100990802     C                   Z-ADD     0             W
115200990802     C     OPZULT        DO        24            I                              --- 1 -->
115300990802      * SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
115400990802     C     W             IFLT      8                                            --- 2 -->
115500990802      * SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
115600990802     C     OAT(I)        IFEQ      'S'
115700990802     C     ODE(I)        ANDNE     *BLANKS
115800990802     C                   ADD       1             W
115900990802     C                   MOVEL     ODE(I)        OPV(W)
116000990802     C                   ENDIF
116100990802      * MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
116200990802     C                   Z-ADD     I             OPZULT
116300990802     C                   ENDIF                                                  <-- 2 ---
116400990802     C                   ENDDO                                                  <-- 1 ---
116500990802      * TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
116600990802     C                   MOVEL     OPV(1)        VOD001
116700990802     C                   MOVEL     OPV(2)        VOD002
116800990802     C                   MOVEL     OPV(3)        VOD003
116900990802     C                   MOVEL     OPV(4)        VOD004
117000990802     C                   MOVEL     OPV(5)        VOD005
117100990802     C                   MOVEL     OPV(6)        VOD006
117200990802     C                   MOVEL     OPV(7)        VOD007
117300990802     C                   MOVEL     OPV(8)        VOD008
117400990802     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
117500990802      * SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
117600990802     C     OPZNAT        IFGT      8
117700990802     C                   MOVEL     'S'           FAT(23)
117800990802     C                   ENDIF
117900990802     C                   ENDSR
118000990802      *------------------------------------------------------------------------*
118100990802      *  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
118200990802      *           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
118300990802      *           OUTPUT: - OPZOK  (OPZIONE OK S/N)
118400990802      *                   - ERRORE SE OPZIONE NON ATTIVA
118500990802      *------------------------------------------------------------------------*
118600990802     C     CONOPZ        BEGSR
118700990802      * RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
118800990802     C                   MOVEL     *BLANKS       OPZOK             1
118900990802     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
119000990802     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
119100990802     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
119200990802     C     OAT(I)        IFEQ      'S'
119300990802     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
119400990802     C                   MOVEL     'S'           OPZOK
119500990802     C                   ELSE
119600990802     C                   MOVEL     'N'           OPZOK             1
119700990802     C                   ENDIF
119800990802     C                   ENDIF                                                  <-- 2 ---
119900990802     C                   ENDDO                                                  <-- 1 ---
120000990802      * SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
120100990802     C     OPZOK         IFNE      'S'
120200990802     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
120300990802     C                   EXSR      CARMSG
120400990802     C                   MOVEL     '1'           $ERR
120500990802     C                   MOVEL     'N'           OPZOK             1
120600990802     C                   ENDIF
120700990802     C                   ENDSR
120800990802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
120900990802TNSB53R- Tasto funzionale non valido.                                          1
121000990802TNSB53R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
121100990802TNSB53R- Opzione non valida.                                                   3
121200990802TNSB53R- Opzione abilitata ma non gestita. Avvertire CED.                      4
121300990802TNSB53R- Inizio elenco.                                                        5
121400990802TNSB53R- Fine elenco.                                                          6
121500020724TNSB53R- Utente non autorizzato.                                               7
121600990802TNSB53R- 8
121700990802TNSB53R- 9
121800990802TNSB53R- 10
121900990802TNSB53R- 11
122000990802TNSB53R- 12
122100990802TNSB53R- 13
122200990802TNSB53R- 14
122300990802TNSB53R- 15
122400990802TNSB53R- 16
122500990802TNSB53R- 17
122600990802TNSB53R- 18
122700990802TNSB53R- 19
122800990802TNSB53R- 20
122900990802** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
123000990802- 1-1 -                   -    NN     NN
123100990802- 2-2 -2=Modifica         -    NN     NN
123200990802- 3-3 -                   -    NN     NN
123300990802- 4-4 -4=Cancellazione    -    NN     NN
123400990802- 5-5 -5=Visualizzazione  -    SS     SS
123500990802- 6-6 -                   -    NN     NN
123600990802- 7-7 -7=Ripristino       -    NN     NN
123700990802- 8-8 -                   -    NN     NN
123800990802- 9-9 -                   -    NN     NN
123900990802-10-  -                   -    NN     NN
124000990802-11-  -                   -    NN     NN
124100990802-12-  -                   -    NN     NN
124200990802-13-  -                   -    NN     NN
124300990802-14-  -                   -    NN     NN
124400990802-15-  -                   -    NN     NN
124500990802-16-  -                   -    NN     NN
124600990802-17-  -                   -    NN     NN
124700990802-18-  -                   -    NN     NN
124800990802-19-  -                   -    NN     NN
124900990802-20-  -                   -    NN     NN
125000990802-21-  -                   -    NN     NN
125100990802-22-  -                   -    NN     NN
125200990802-23-  -                   -    NN     NN
125300990802-24-  -                   -    NN     NN
125400990802** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
125500990802F1    -                   -    NN     NN
125600990802F2    -                   -    NN     NN
125700990802F3    -F3=Fine            -    SS     SS
125800990802F4    -                   -    NN     NN
125900990802F5    -F5=Rivisualizza    -    SS     SS
126000990802F6    -F6=Inserimento     -    NN     NN
126100990802F7    -                   -    NN     NN
126200990802F8    -                   -    NN     NN
126300990802F9    -F9=Altri dati      -    SS     SS
126400990802F10   -                   -    NN     NN
126500990802F11   -                   -    NN     NN
126600990802F12   -F12=Ritorno        -    SS     SS
126700990802F13   -                   -    NN     NN
126800990802F14   -                   -    NN     NN
126900990802F15   -                   -    NN     NN
127000990802F16   -                   -    NN     NN
127100990802F17   -F17=Inizio dati    -    SS     SS
127200990802F18   -F18=Fine dati vis. -    SS     SS
127300990802F19   -                   -    NN     NN
127400990802F20   -                   -    NN     NN
127500990802F21   -                   -    NN     NN
127600990802F22   -                   -    NN     NN
127700990802F23   -F23=Altre opzioni  -    NN     NN
127800990802F24   -F24=Altri tasti    -    NN     NN
127900990802ENTER -                   -    SS     SS
128000990802ROLLUP-                   -    SS     SS
128100990802ROLLDN-                   -    SS     SS
128200990802HELP  -                   -    NN     NN
128300990802PRINT -                   -    NN     NN
128400990802BSPACE-                   -    NN     NN
128500990802CLEAR -                   -    NN     NN
