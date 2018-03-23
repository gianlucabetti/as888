000100000000     H*------------------------------------------------------------------------*
000200960822     H* GESTIONE BUDGET DI FILIALE - SCELTA                                    *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800020115     FTISE95D   CF   E             WORKSTN INFDS(DEVDS1)
000900020115     F                                     SFILE(SE95DVS:SFLULT)
001000020115     F                                     SFILE(SE95DVM:MSGULT)
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300020115     FTEBDG01L  IF   E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*---------------------
001800000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
001900000000     D*---------------------
002000000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
002100000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.OPZ
002200000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
002300000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.FUN
002400000000     D*---------------------
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D*---------------------
002700000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002800000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
002900000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000000000     D*---------------------
003100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003200000000     D*---------------------
003300000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003400000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003500000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
003600000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
003700000000     D*---------------------
003800000000     D* TABELLA DESCRIZIONI A VIDEO
003900000000     D*---------------------
004000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004100000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004200000000     D*---------------------
004300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004400000000     D*---------------------
004500000000     D KEYS            S              1    DIM(40)
004600000000     D*---------------------
004700000000     D* TABELLA DECODIFICHE DEI MESI
004800000000     D*---------------------
004900000000     D MES             S              9    DIM(12) CTDATA PERRCD(1)             *DECODIFICHE MENSILITA'
005000000000     D*------------------------------------------------------------------------*
005100000000     D* INPUT
005200000000     D*------------------------------------------------------------------------*
005300000000     D*---------------------
005400000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005500000000     D*---------------------
005600000000     D DSOPZ         E DS
005700000000     D  OPZRIG                 1     40
005800000000     D*---------------------
005900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006000000000     D*---------------------
006100000000     D DSFUN         E DS
006200000000     D  FUNRIG                 1     40
006300000000     D*---------------------
006400000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006500000000     D*---------------------
006600000000     D DSMSG         E DS
006700000000     D*---------------------
006800000000     D* DS PER GESTIONE TASTI FUNZIONALI
006900000000     D*---------------------
007000000000     D DSKEYS        E DS
007100000000     D  KEYRIG                 1     40
007200000000     D*---------------------
007300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007400000000     D*---------------------
007500000000     D                SDS
007600000000     D  USER                 254    263                                         UTENTE
007700000000     D*---------------------
007800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007900000000     D*---------------------
008000000000     D DEVDS1          DS
008100000000     D  KEY                  369    369
008200000000     D  CURSOR               370    371
008300000000     D  FRTSFL               378    379B 0
008400000000     D*---------------------
008500000000     D* ARCHITETTURA
008600000000     D*---------------------
008700000000     D KPJBA         E DS
008800000000     D*---------------------
008900020916     D* DS REPERIMENTO DATI UTENTE
009000020916     D*-------------------
009100020916     D TIBS34DS      E DS                                                       *Profili utente
009200020916     D DDATIUTE      E DS                                                       *Dati utente
009300020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009400000000     D*---------------------
009500000000     D* DS PARAMETRI LANCIO
009600000000     D*---------------------
009700000000     D DS95            DS
009800000000     D  D95OP0                 1      3
009900000000     D  D95OP1                 4      6
010000000000     D  D95F03                 7      7
010100000000     D  D95ERR                 8      8
010200000000     D  D95MSG                 9     88
010300000000     D  D95FIL                89     91  0
010400960108     D  D95ANN                92     95  0
010500960108     D  D95MES                96     97  0
010600960108     D  D95IOE                98     98
010700960108     D  D95FPE                99    100
010800000000     D*------------------
010900000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
011000000000     D*------------------
011100000000     D DESGES          C                   CONST('** GESTIONE **')
011200000000     D DESINT          C                   CONST('INTERROGAZIONE')
011300000000     C*------------------------------------------------------------------------*
011400000000     C* MAIN LINE
011500000000     C*------------------------------------------------------------------------*
011600000000     C*
011700000000     C* CICLO FINO A CHE:
011800000000DO  1C     $FINE         DOUEQ     '1'
011900000000     C*
012000000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012100000000     C*
012200000000     C* CONTROLLA GESTIONE VIDEO
012300000000IF  2C     $GES          IFEQ      'VS'
012400000000     C                   EXSR      GESVS                                        *SUBFILE
012500000000E   2C                   ENDIF
012600000000     C*
012700000000E   1C                   ENDDO
012800000000     C* FINE PGM
012900000000     C                   SETON                                        LR
013000000000     C*------------------------------------------------------------------------*
013100000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
013200000000     C*------------------------------------------------------------------------*
013300000000     C     CARVS         BEGSR
013400000000     C*
013500000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
013600000000     C*
013700000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
013800000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
013900000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO 2
014000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014200000000     C*
014300020115     C                   WRITE     SE95DVT                                      *TESTATA
014400020115     C                   WRITE     SE95DVO                                      *OPZIONI
014500020115     C                   WRITE     SE95DVF                                      *FUNZIONI
014600000000     C*
014700000000     C                   EXSR      POSSFL                                       *CARICA SFL
014800000000     C*
014900000000     C                   ENDSR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* GESVS  - GESTIONE SUBFILE
015200000000     C*------------------------------------------------------------------------*
015300000000     C     GESVS         BEGSR
015400000000     C*
015500000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015600020115     C                   WRITE     SE95DVT                                      *TESTATA
015700020115     C                   WRITE     SE95DVO                                      *OPZIONI
015800020115     C                   WRITE     SE95DVF                                      *FUNZIONI
015900000000E   1C                   ENDIF
016000000000     C*
016100000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
016200000000     C*
016300000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
016400000000     C*
016500000000IF  1C     $ERR          IFEQ      '0'
016600000000SL  2C                   SELECT
016700000000     C     KEY           WHENEQ    F6
016800000000     C                   EXSR      F06GES
016900000000     C                   OTHER
017000000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFIL
017100000000E   2C                   ENDSL
017200000000E   1C                   ENDIF
017300000000     C*
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
017700000000     C*------------------------------------------------------------------------*
017800000000     C     F06GES        BEGSR
017900000000     C*
018000000000     C* LANCIO L'INSERIMENTO
018100000000     C                   Z-ADD     *ZEROS        WFIL                           *FILIALE
018200000000     C                   Z-ADD     *ZEROS        WANN                           *ANNO
018300000000     C                   Z-ADD     *ZEROS        WMES                           *MESE
018400960115     C                   MOVEL     *BLANKS       WIOE                           *ITALIA/IMPORT/EXPORT
018500000000     C                   MOVEL     *BLANKS       WFPE                           *FASCIA PESO
018600000000     C*
018700000000     C                   MOVEL     D95OP0        WOP0                           *LIVELLO PROCEDURA
018800000000     C                   MOVEL     'F06'         WOP1                           *LIVELLO PROGRAMMA
018900020115     C                   MOVEL     'TISE96R'     WPGM              8            *ROUTINE MANUTENZIONE
019000000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
019100000000     C*
019200000000     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*------------------------------------------------------------------------*
019600000000     C*  GESOPZ - GESTIONE OPZIONI
019700000000     C*------------------------------------------------------------------------*
019800000000     C     GESOPZ        BEGSR
019900000000     C*
020000000000SL  1C                   SELECT
020100000000     C     VSCOPZ        WHENEQ    ' 2'
020200000000     C                   EXSR      O02GES                                       *MODIFICA
020300000000     C     VSCOPZ        WHENEQ    ' 4'
020400000000     C                   EXSR      O04GES                                       *ANNULLAMENTO
020500000000     C     VSCOPZ        WHENEQ    ' 5'
020600000000     C                   EXSR      O05GES                                       *VISUALIZZAZIONE
020700000000     C     VSCOPZ        WHENEQ    ' 7'
020800000000     C                   EXSR      O07GES                                       *RIPRISTINO
020900000000     C                   OTHER
021000000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GEST
021100000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
021200000000E   1C                   ENDSL
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
021700000000     C*------------------------------------------------------------------------*
021800000000     C     O02GES        BEGSR
021900000000     C*
022000000000IF  1C     VSCATB        IFEQ      'A'
022100000000     C                   MOVEL     '1'           $ERR
022200000000     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
022300000000     C                   EXSR      CARMSG
022400000000E   1C                   ENDIF
022500000000     C*
022600000000IF  1C     $ERR          IFEQ      '0'
022700000000     C*
022800000000     C* LANCIO LA MODIFICA
022900000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE
023000000000     C                   Z-ADD     VSCANN        WANN                           *ANNO
023100000000     C                   Z-ADD     VSCMES        WMES                           *MESE
023200960115     C                   MOVEL     VSCIOE        WIOE                           *ITALIA/IMPORT/EXPORT
023300000000     C                   MOVEL     VSCFPE        WFPE                           *FASCIA DI PESO
023400000000     C*
023500000000     C                   MOVEL     D95OP0        WOP0                           *LIVELLO PROCEDURA
023600000000     C                   MOVEL     'O02'         WOP1                           *LIVELLO PROGRAMMA
023700020115     C                   MOVEL     'TISE96R'     WPGM              8            *ROUTINE MODIFICA
023800000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
023900000000     C*
024000000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
024100000000     C*
024200000000E   1C                   ENDIF
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
024700000000     C*------------------------------------------------------------------------*
024800000000     C     O04GES        BEGSR
024900000000     C*
025000000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
025100000000IF  1C     VSCATB        IFEQ      'A'
025200000000     C                   MOVEL     '1'           $ERR
025300000000     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
025400000000     C                   EXSR      CARMSG
025500000000E   1C                   ENDIF
025600000000     C*
025700000000IF  1C     $ERR          IFEQ      '0'
025800000000     C*
025900000000     C* LANCIO LA CANCELLAZIONE
026000000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE
026100000000     C                   Z-ADD     VSCANN        WANN                           *ANNO
026200000000     C                   Z-ADD     VSCMES        WMES                           *MESE
026300960115     C                   MOVEL     VSCIOE        WIOE                           *ITALIA/IMPORT/EXPORT
026400000000     C                   MOVEL     VSCFPE        WFPE                           *FASCIA DI PESO
026500000000     C*
026600000000     C                   MOVEL     D95OP0        WOP0                           *LIVELLO PROCEDURA
026700000000     C                   MOVEL     'O04'         WOP1                           *LIVELLO PROGRAMMA
026800020115     C                   MOVEL     'TISE96R'     WPGM              8            *ROUTINE CANCELLAZ.NE
026900000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
027000000000     C*
027100000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
027200000000     C*
027300000000E   1C                   ENDIF
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*------------------------------------------------------------------------*
027700000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
027800000000     C*------------------------------------------------------------------------*
027900000000     C     O05GES        BEGSR
028000000000     C*
028100000000     C* LANCIO LA VISUALIZZAZIONE
028200000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE
028300000000     C                   Z-ADD     VSCANN        WANN                           *ANNO
028400000000     C                   Z-ADD     VSCMES        WMES                           *MESE
028500960115     C                   MOVEL     VSCIOE        WIOE                           *ITALIA/IMPORT/EXPORT
028600000000     C                   MOVEL     VSCFPE        WFPE                           *FASCIA DI PESO
028700000000     C*
028800000000     C                   MOVEL     D95OP0        WOP0                           *LIVELLO PROCEDURA
028900000000     C                   MOVEL     'O05'         WOP1                           *LIVELLO PROGRAMMA
029000020115     C                   MOVEL     'TISE96R'     WPGM              8            *ROUTINE VISUALIZZA
029100000000     C                   EXSR      LANPGM
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
029600000000     C*------------------------------------------------------------------------*
029700000000     C     O07GES        BEGSR
029800000000     C*
029900000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
030000000000IF  1C     VSCATB        IFNE      'A'
030100000000     C                   MOVEL     '1'           $ERR
030200000000     C                   MOVEL     MSG(13)       $MSG                           *NON ANNULLATO
030300000000     C                   EXSR      CARMSG
030400000000E   1C                   ENDIF
030500000000     C*
030600000000IF  1C     $ERR          IFEQ      '0'
030700000000     C*
030800000000     C* LANCIO IL RIPRISTINO
030900000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE
031000000000     C                   Z-ADD     VSCANN        WANN                           *ANNO
031100000000     C                   Z-ADD     VSCMES        WMES                           *MESE
031200960115     C                   MOVEL     VSCIOE        WIOE                           *ITALIA/IMPORT/EXPORT
031300000000     C                   MOVEL     VSCFPE        WFPE                           *FASCIA DI PESO
031400000000     C*
031500000000     C                   MOVEL     D95OP0        WOP0                           *LIVELLO PROCEDURA
031600000000     C                   MOVEL     'O07'         WOP1                           *LIVELLO PROGRAMMA
031700020115     C                   MOVEL     'TISE96R'     WPGM              8            *ROUTINE RIPRISTINO
031800000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
031900000000     C*
032000000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
032100000000     C*
032200000000E   1C                   ENDIF
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
032700000000     C*------------------------------------------------------------------------*
032800000000     C     NEWPOS        BEGSR
032900000000     C*
033000000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
033100000000     C*        1 = RICHIESTO POSIZIONAMENTO
033200000000     C                   Z-ADD     0             SWPOSI            7 0
033300000000     C*
033400000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
033500000000     C*            NON OCCORRE RICARICARE IL SUBFILE
033600000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
033700000000     C                   Z-ADD     0             SWMODI            1 0
033800000000     C*
033900000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
034000000000IF  1C     VCCFIL        IFNE      DEPFIL
034100000000     C     VCCANN        ORNE      DEPANN
034200000000     C     VCCMES        ORNE      DEPMES
034300960108     C     VCCIOE        ORNE      DEPIOE
034400000000     C*
034500000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
034600000000     C                   EXSR      CHKPAR
034700000000     C*
034800000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
034900000000IF  2C     $ERR          IFEQ      '0'
035000000000     C                   Z-ADD     1             SWPOSI                         *RICHIESTO POSIZ.TO
035100000000     C                   Z-ADD     VCCFIL        DEPFIL                         *SALVATO NEW POSIZ.TO
035200000000     C                   Z-ADD     VCCANN        DEPANN                         *SALVATO NEW POSIZ.TO
035300000000     C                   Z-ADD     VCCMES        DEPMES                         *SALVATO NEW POSIZ.TO
035400960108     C                   MOVEL     VCCIOE        DEPIOE                         *SALVATO NEW POSIZ.TO
035500000000E   2C                   ENDIF
035600000000E   1C                   ENDIF
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
036100000000     C*------------------------------------------------------------------------*
036200000000     C     CHKPAR        BEGSR
036300000000     C*
036400000000     C* REIMPOSTO INDICATORI DI ERRORE
036500960108     C                   SETOFF                                       808182
036600960108     C                   SETOFF                                       8384
036700000000     C*--------------------
036800000000     C*
036900000000     C*--------------------
037000000000     C*
037100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
037200000000     C* SUL CAMPO DEL CONTROL
037300000000IF  1C     $ERR          IFEQ      '1'
037400000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
037500000000X   1C                   ELSE
037600000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
037700000000E   1C                   ENDIF
037800000000     C*
037900000000     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
038200000000     C*------------------------------------------------------------------------*
038300000000     C     SETFIL        BEGSR
038400000000     C*
038500000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
038600000000     C*
038700000000     C* POSIZIONAMENTO ARCHIVIO
038800000000     C                   Z-ADD     VCCFIL        KDGFIL
038900000000     C                   Z-ADD     VCCANN        KDGANN
039000000000     C                   Z-ADD     VCCMES        KDGMES
039100960108     C                   MOVEL     VCCIOE        KDGIOE
039200020115     C     KEYBDG        SETLL     TEBDG01L                           99        *FINE FILE
039300000000     C*
039400000000     C  N99              EXSR      LETFIL
039500000000     C*
039600000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
039700000000IF  1C     *IN99         IFEQ      '1'
039800000000     C                   Z-ADD     1             SFLEOF
039900000000E   1C                   ENDIF
040000000000     C*
040100000000     C                   ENDSR
040200000000     C*------------------------------------------------------------------------*
040300000000     C* LETFIL - LEGGE RECORD SUCESSIVO
040400000000     C*------------------------------------------------------------------------*
040500000000     C     LETFIL        BEGSR
040600000000     C*
040700000000     C* LEGGO FINO A:
040800000000     C*     - FINE FILE
040900000000     C*     - TROVATO RECORD VALIDO
041000000000     C                   MOVEL     '0'           WRECOK            1            *RECORD VALIDO
041100000000DO  1C     *IN99         DOWEQ     *OFF
041200000000     C     WRECOK        ANDEQ     '0'
041300000000     C* LETTURA
041400020115     C                   READ      TEBDG01L                               99
041500000000     C*
041600000000     C* FINE FILE  O TABELLA DIVERSA
041700000000IF  2C     *IN99         IFEQ      '1'
041800000000     C                   Z-ADD     1             SFLEOF
041900000000X   2C                   ELSE
042000000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
042100000000E   2C                   ENDIF
042200000000     C*
042300000000E   1C                   ENDDO
042400000000     C*
042500000000     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
042800000000     C*------------------------------------------------------------------------*
042900000000     C     CHKREC        BEGSR
043000000000     C*
043100000000     C                   MOVEL     '1'           WRECOK                         RECORD OK IN GENERALE
043200000000     C*
043300000000     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
043600000000     C*------------------------------------------------------------------------*
043700000000     C     IRESFL        BEGSR
043800000000     C*
043900990217IF  1C     TBGATB        IFNE      *BLANKS
044000000000     C                   MOVEL     'A'           VSCATB                         *ANNULLAMENTO
044100000000X   1C                   ELSE
044200000000     C                   MOVEL     ' '           VSCATB
044300000000E   1C                   ENDIF
044400000000     C*
044500000000     C                   Z-ADD     TBGFIL        VSCFIL                         *FILIALE
044600000000     C     TBGFIL        CHAIN     AZORG01L                           99
044700000000     C  N99              MOVEL     ORGDES        VSDFIL                         *DECOD.FILIALE
044800000000     C   99              MOVEL     *BLANKS       VSDFIL                         *DECOD.FILIALE
044900000000     C                   Z-ADD     TBGANN        VSCANN                         *ANNO
045000000000     C                   Z-ADD     TBGMES        VSCMES                         *MESE
045100000000     C                   Z-ADD     TBGMES        $A                2 0
045200000000     C                   MOVEL     MES($A)       VSDMES                         *DECODIFICA MESE
045300960115     C                   MOVEL     TBGIOE        VSCIOE                         *ITALIA/IMPORT/EXPORT
045400960108     C                   MOVEL     *BLANKS       VSDIOE
045500960115IF  1C     TBGIOE        IFEQ      'I'
045600960115     C                   MOVEL     'ITA'         VSDIOE                         *DECODIFICA NAZIONALE
045700960108E   1C                   ENDIF
045800960115IF  1C     TBGIOE        IFEQ      'M'
045900960108     C                   MOVEL     'IMP'         VSDIOE                         *DECODIFICA IMPORT
046000960108E   1C                   ENDIF
046100960115IF  1C     TBGIOE        IFEQ      'X'
046200960108     C                   MOVEL     'EXP'         VSDIOE                         *DECODIFICA EXPORT
046300960108E   1C                   ENDIF
046400000000     C                   MOVEL     TBGFPE        VSCFPE                         *FASCIA PESO
046500000000     C*
046600020916     C                   MOVEL     1             KTBKUT                         *CODICE UTENTE
046700000000     C                   MOVEL     '2L'          KTBCOD                         *CODICE TABELLA
046800990217IF  1C     TBGANN        IFLE      1998                                         *FASCE FINO AL 98
046900000000     C                   MOVEL     'SDR'         $COMOD            5            *CHIAVE COMPOSTA
047000990217X   1C                   ELSE                                                   *FASCE DOPO IL 99
047100020328     C                   MOVEL     'S9R'         $COMOD
047200990217E   1C                   ENDIF
047300000000     C                   MOVE      TBGFPE        $COMOD
047400000000     C                   MOVE      *BLANKS       KTBKEY
047500000000     C                   MOVEL     $COMOD        KTBKEY                         *CODICE TENTATIVO
047600000000     C     KEYTAB        CHAIN     TABEL00F                           99
047700000000     C     *IN99         IFEQ      '1'
047800000000     C     TBLFLG        ORNE      *BLANKS
047900000000     C                   MOVEL     *BLANKS       VSDFPE                         *DESCRIZIONE
048000000000     C                   ELSE
048100000000     C                   MOVEL     TBLUNI        VSDFPE                         *DESCRIZIONE
048200000000     C                   ENDIF
048300000000     C*
048400000000     C                   ENDSR
048500000000     C*------------------------------------------------------------------------*
048600000000     C* LANPGM - LANCIA UN PROGRAMMA
048700000000     C*------------------------------------------------------------------------*
048800000000     C     LANPGM        BEGSR
048900000000     C*--------------------
049000000000     C* PARAMETRI INFORMATIVI
049100000000     C*--------------------
049200000000     C                   MOVEL     WOP0          D95OP0                         *LIVELLO PROCEDURA
049300000000     C                   MOVEL     WOP1          D95OP1                         *LIVELLO PROGRAMMA
049400000000     C                   MOVEL     '0'           D95F03                         *USCITA PGM
049500000000     C                   MOVEL     '0'           D95ERR                         *ERRORE
049600000000     C                   MOVEL     *BLANKS       D95MSG                         *MESSAGGIO ERRORE
049700000000     C*--------------------
049800000000     C* PARAMETRI
049900000000     C*--------------------
050000000000     C                   Z-ADD     WFIL          D95FIL                         *FILIALE
050100000000     C                   Z-ADD     WANN          D95ANN                         *ANNO
050200000000     C                   Z-ADD     WMES          D95MES                         *MESE
050300960115     C                   MOVEL     WIOE          D95IOE                         *ITALIA/IMPORT/EXPORT
050400000000     C                   MOVEL     WFPE          D95FPE                         *FASCIA DI PESO
050500000000     C* LANCIO
050600000000     C                   MOVEL     DS95          KPJBU
050700000000     C                   CALL      WPGM                                 99
050800000000     C                   PARM                    KPJBA
050900000000     C                   MOVEL     KPJBU         DS95
051000000000     C*
051100000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
051200000000IF  1C     D95F03        IFEQ      '1'
051300000000     C                   MOVEL     '1'           $F03
051400000000     C                   MOVEL     '1'           $ERR
051500000000     C                   MOVEL     '1'           $FINE
051600000000     C*
051700000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
051800000000X   1C                   ELSE
051900000000     C                   MOVEL     D95MSG        $MSG
052000000000     C                   EXSR      CARMSG
052100000000E   1C                   ENDIF
052200000000     C*
052300000000     C                   ENDSR
052400020916     C*--------------------------------------------------------------------------------------------*
052500020916     C* REPERISCE I DATI UTENTE
052600020916     C*--------------------------------------------------------------------------------------------*
052700020916     C     REPDATIUTE    BEGSR
052800020916     C*
052900020916     C* INIZIALIZZA VARIABILI DI WRK
053000020916     C                   CLEAR                   TIBS34DS
053100020916     C                   CLEAR                   AZUTEDS
053200020916     C                   CLEAR                   DDATIUTE
053300020916     C*
053400020916     C     *DTAARA       DEFINE    §azute        azuteds
053500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
053600020916     C                   IN(E)     *DTAARA
053700020916if  1C                   IF        %Error
053800020916     c                   EVAL      I34Tla = 'L'
053900020916     C                   CALL      'TIBS34R'
054000020916     C                   PARM                    Tibs34Ds
054100020916     C                   IN        *DTAARA
054200020916e   1C                   ENDIF
054300020916     C*
054400020916     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C*  *INZSR - ROUTINE INIZIALE
054700000000     C*------------------------------------------------------------------------*
054800000000     C     *INZSR        BEGSR
054900000000     C*
055000000000     C* RICEVIMENTO PARAMETRI
055100000000     C     *ENTRY        PLIST
055200000000     C                   PARM                    KPJBA
055300000000     C                   MOVEL     KPJBU         DS95
055400000000     C*
055500000000     C* DEFINIZIONE KLIST
055600020115     C     *LIKE         DEFINE    TBGFIL        KDGFIL                         *TEBDG00F
055700000000     C     *LIKE         DEFINE    TBGANN        KDGANN
055800000000     C     *LIKE         DEFINE    TBGMES        KDGMES
055900960108     C     *LIKE         DEFINE    TBGIOE        KDGIOE
056000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
056100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
056200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
056300000000     C*
056400000000     C* DEFINIZIONE DEPOSITI
056500000000     C     *LIKE         DEFINE    VCCFIL        DEPFIL
056600000000     C     *LIKE         DEFINE    VCCANN        DEPANN
056700000000     C     *LIKE         DEFINE    VCCMES        DEPMES
056800960108     C     *LIKE         DEFINE    VCCIOE        DEPIOE
056900000000     C*
057000000000     C* DEFINIZIONE CAMPI DI COMODO LANCIO PROGRAMMA
057100000000     C     *LIKE         DEFINE    D95OP0        WOP0
057200000000     C     *LIKE         DEFINE    D95OP1        WOP1
057300000000     C     *LIKE         DEFINE    D95F03        WF03
057400000000     C     *LIKE         DEFINE    D95ERR        WERR
057500000000     C     *LIKE         DEFINE    D95MSG        WMSG
057600000000     C     *LIKE         DEFINE    D95FIL        WFIL
057700000000     C     *LIKE         DEFINE    D95ANN        WANN
057800000000     C     *LIKE         DEFINE    D95MES        WMES
057900960108     C     *LIKE         DEFINE    D95IOE        WIOE
058000000000     C     *LIKE         DEFINE    D95FPE        WFPE
058100000000     C*
058200020115     C* CHIAVE LETTURA TEBDG01L - PARZIALE
058300000000     C     KEYBDG        KLIST
058400000000     C                   KFLD                    KDGFIL                         *FILIALE
058500000000     C                   KFLD                    KDGANN                         *ANNO
058600000000     C                   KFLD                    KDGMES                         *MESE
058700960115     C                   KFLD                    KDGIOE                         *ITA/IMP/EXP
058800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
058900000000     C     KEYTAB        KLIST
059000000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
059100000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
059200000000     C                   KFLD                    KTBKEY                         *CHIAVE
059300000000     C*
059400000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
059500000000     C                   MOVE      D95OP0        INTGES            2
059600000000IF  1C     INTGES        IFEQ      '02'
059700000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
059800000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
059900000000X   1C                   ELSE
060000000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
060100000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
060200000000E   1C                   ENDIF
060300000000     C*
060400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
060500000000     C                   EXSR      INZSTD
060600000000     C*
060700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
060800000000     C                   EXSR      INZSFL
060900000000     C*
061000000000     C* IMPOSTO CAMPI TESTATA VIDEO
061100020916     C                   MOVEL     RSUT          VTDRSU                         *RAGIONE SOC. UTENTE
061200000000     C*
061300000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
061400000000     C                   Z-ADD     *ZEROS        VCCFIL
061500000000     C                   Z-ADD     *ZEROS        VCCANN
061600000000     C                   Z-ADD     *ZEROS        VCCMES
061700960108     C                   MOVEL     *BLANKS       VCCIOE
061800000000     C*
061900000000     C* EMETTO VIDEATA SUBFILE
062000000000     C                   EXSR      CARVS                                        CARICA E EMETTE SFILE
062100000000     C*
062200000000     C                   ENDSR
062300000000     C*------------------------------------------------------------------------*
062400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
062700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
062800000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
062900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063100000000     C*------------------------------------------------------------------------*
063200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
063300000000     C*------------------------------------------------------------------------*
063400000000     C     INZSTD        BEGSR
063500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
063600000000     C                   CALL      'XKEYS'
063700000000     C                   PARM                    DSKEYS
063800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
063900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
064000000000     C     XMSGPA        PLIST
064100000000     C                   PARM                    DSMSMS
064200000000     C                   PARM                    DSMSPG
064300000000     C                   PARM                    DSMSKE
064400000000     C                   PARM                    DSMSER
064500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
064600020115     C                   MOVEL     'TISE95R'     DSMSPG                         *NOME PGM CHIAMANTE
064700020115     C                   MOVEL     'TISE95R'     VMCQUE                         *CODA MESSAGGI DESTIN
064800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
064900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
065000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
065100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
065200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
065300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
065400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
065500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
065600000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
065700000000     C* LANCIO ROUTINE PER VERIFICARE SE
065800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
065900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
066000020916     C                   EXSR      REPDATIUTE
066100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
066200000000     C                   BITOFF    '01234567'    POSMI1            1
066300000000     C                   BITOFF    '01234567'    POSMI2            1
066400000000     C                   BITOFF    '01234567'    POSMA1            1
066500000000     C                   BITOFF    '01234567'    POSMA2            1
066600000000     C                   BITON     '34'          POSMI1            1            *18
066700000000     C                   BITON     '7'           POSMI2            1            *01
066800000000     C                   BITON     '34'          POSMA1            1            *18
066900000000     C                   BITON     '0'           POSMA2            1            *80
067000000000     C                   MOVE      POSMI2        POSMIN            2
067100000000     C                   MOVEL     POSMI1        POSMIN
067200000000     C                   MOVE      POSMA2        POSMAX            2
067300000000     C                   MOVEL     POSMA1        POSMAX
067400000000     C                   ENDSR
067500000000     C*------------------------------------------------------------------------*
067600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
067700000000     C*------------------------------------------------------------------------*
067800000000     C     FUNSTD        BEGSR
067900000000     C                   SELECT
068000000000     C     KEY           WHENEQ    F3
068100000000     C                   EXSR      F03STD
068200000000     C     KEY           WHENEQ    F12
068300000000     C                   EXSR      F12STD
068400000000     C     KEY           WHENEQ    F24
068500000000     C                   EXSR      F24STD
068600000000     C                   OTHER
068700000000     C                   MOVEL     MSG(2)        $MSG
068800000000     C                   EXSR      CARMSG
068900000000     C                   ENDSL
069000000000     C                   ENDSR
069100000000     C*------------------------------------------------------------------------*
069200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
069300000000     C*------------------------------------------------------------------------*
069400000000     C     F03STD        BEGSR
069500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
069600000000     C                   MOVEL     '1'           $F03
069700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
069800000000     C                   MOVEL     '1'           $FINE
069900000000     C                   ENDSR
070000000000     C*------------------------------------------------------------------------*
070100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
070200000000     C*------------------------------------------------------------------------*
070300000000     C     F12STD        BEGSR
070400000000     C* IMPOSTO FLAG DI FIN PGM
070500000000     C                   MOVEL     '1'           $FINE
070600000000     C                   ENDSR
070700000000     C*------------------------------------------------------------------------*
070800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
070900000000     C*------------------------------------------------------------------------*
071000000000     C     F24STD        BEGSR
071100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
071200000000     C                   EXSR      OUTFUN
071300000000     C                   ENDSR
071400000000     C*------------------------------------------------------------------------*
071500000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
071600000000     C*------------------------------------------------------------------------*
071700000000     C     INZSFL        BEGSR
071800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
071900000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
072000000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
072100000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
072200000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
072300000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
072400000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
072500000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
072600000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
072700000000     C                   Z-ADD     12            SFLPAG
072800000000     C                   ENDSR
072900000000     C*------------------------------------------------------------------------*
073000000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
073100000000     C*------------------------------------------------------------------------*
073200000000     C     FUNSFL        BEGSR
073300000000     C                   SELECT
073400000000     C     KEY           WHENEQ    ENTER
073500000000     C                   EXSR      ENTSFL
073600000000     C     KEY           WHENEQ    F3
073700000000     C                   EXSR      F03STD
073800000000     C     KEY           WHENEQ    F5
073900000000     C                   EXSR      F05STD
074000000000     C     KEY           WHENEQ    F12
074100000000     C                   EXSR      F12STD
074200000000     C     KEY           WHENEQ    F17
074300000000     C                   EXSR      F17STD
074400000000     C     KEY           WHENEQ    F18
074500000000     C                   EXSR      F18STD
074600000000     C     KEY           WHENEQ    F23
074700000000     C                   EXSR      F23STD
074800000000     C     KEY           WHENEQ    F24
074900000000     C                   EXSR      F24STD
075000000000     C     KEY           WHENEQ    ROLLUP
075100000000     C                   EXSR      RUPSTD
075200000000     C     KEY           WHENEQ    ROLLDN
075300000000     C                   EXSR      RDNSTD
075400000000     C                   OTHER
075500000000     C                   MOVEL     MSG(2)        $MSG
075600000000     C                   EXSR      CARMSG
075700000000     C                   ENDSL
075800000000     C                   ENDSR
075900000000     C*------------------------------------------------------------------------*
076000000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
076100000000     C*------------------------------------------------------------------------*
076200000000     C     F05STD        BEGSR
076300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
076400000000     C                   EXSR      POSSFL
076500000000     C                   ENDSR
076600000000     C*------------------------------------------------------------------------*
076700000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
076800000000     C*------------------------------------------------------------------------*
076900000000     C     F17STD        BEGSR
077000000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
077100000000     C                   Z-ADD     1             VCCNBR
077200000000     C                   ENDSR
077300000000     C*------------------------------------------------------------------------*
077400000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
077500000000     C*------------------------------------------------------------------------*
077600000000     C     F18STD        BEGSR
077700000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
077800000000     C                   Z-ADD     SFLULT        VCCNBR
077900000000     C                   ENDSR
078000000000     C*------------------------------------------------------------------------*
078100000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
078200000000     C*------------------------------------------------------------------------*
078300000000     C     F23STD        BEGSR
078400000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
078500000000     C                   EXSR      OUTOPZ
078600000000     C                   ENDSR
078700000000     C*------------------------------------------------------------------------*
078800000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
078900000000     C*------------------------------------------------------------------------*
079000000000     C     RUPSTD        BEGSR
079100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
079200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
079300000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 24
079400000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
079500000000     C                   ELSE
079600000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
079700000000     C                   ENDIF
079800000000     C                   ENDSR
079900000000     C*------------------------------------------------------------------------*
080000000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
080100000000     C*------------------------------------------------------------------------*
080200000000     C     RDNSTD        BEGSR
080300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
080400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
080500000000     C     CURSOR        IFGE      POSMIN
080600000000     C     CURSOR        ANDLE     POSMAX
080700000000     C                   ELSE
080800000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
080900000000     C                   MOVEL     '1'           $ERR
081000000000     C                   MOVEL     MSG(5)        $MSG
081100000000     C                   EXSR      CARMSG
081200000000     C                   ENDIF
081300000000     C                   ENDSR
081400000000     C*------------------------------------------------------------------------*
081500000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
081600000000     C*------------------------------------------------------------------------*
081700000000     C     ENTSFL        BEGSR
081800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
081900000000     C                   EXSR      NEWPOS
082000000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
082100000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
082200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
082300000000     C     SWPOSI        IFNE      0                                            --- 2 -->
082400000000     C                   EXSR      POSSFL
082500000000     C                   ELSE                                                   <-- 2 -->
082600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
082700000000     C     SFLULT        IFNE      0                                            --- 3 -->
082800000000     C                   EXSR      OPZSFL
082900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
083000000000     C     SWMODI        IFNE      0                                            --- 4 -->
083100000000     C                   EXSR      POSSFL
083200000000     C                   ENDIF                                                  <-- 4 ---
083300000000     C                   ENDIF                                                  <-- 3 ---
083400000000     C                   ENDIF                                                  <-- 2 ---
083500000000     C                   ENDIF                                                  <-- 1 ---
083600000000     C                   ENDSR
083700000000     C*------------------------------------------------------------------------*
083800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
083900000000     C*------------------------------------------------------------------------*
084000000000     C     POSSFL        BEGSR
084100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
084200000000     C                   EXSR      CLRSFL
084300000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
084400000000     C                   EXSR      SETFIL
084500000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
084600000000     C     SFLEOF        IFEQ      0
084700000000     C                   EXSR      CARSFL
084800000000     C                   ENDIF
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
085200000000     C*------------------------------------------------------------------------*
085300000000     C     OPZSFL        BEGSR
085400000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
085500000000     C                   MOVEL     'S'           OPZOK             1
085600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
085700000000     C                   Z-ADD     0             SFLVAR            5 0
085800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
085900000000     C                   Z-ADD     0             SFLERR
086000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
086100000000     C                   Z-ADD     SFLULT        WFLULT
086200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
086300020115     C                   READC     SE95DVS                                49
086400000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
086500000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
086600000000     C                   ADD       1             SFLVAR
086700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
086800000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
086900000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
087000000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
087100000000     C                   EXSR      CONOPZ
087200000000     C     OPZOK         IFEQ      'S'
087300000000     C                   EXSR      GESOPZ
087400000000     C                   ENDIF
087500000000     C                   ENDIF                                                  <-- 3 ---
087600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
087700000000     C*            2. PULIZIA SCELTA IN SFL
087800000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
087900000000     C                   MOVEL     '0'           *IN41
088000000000     C                   MOVEL     *BLANKS       VSCOPZ
088100000000     C                   ELSE                                                   <-- 3 -->
088200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
088300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
088400000000     C                   MOVEL     '1'           *IN41
088500000000     C                   Z-ADD     SFLULT        SFLERR
088600000000     C                   ENDIF                                                  <-- 3 ---
088700000000     C                   ELSE                                                   <-- 2 -->
088800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
088900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
089000000000     C                   MOVEL     '1'           *IN41
089100000000     C                   ENDIF                                                  <-- 2 ---
089200000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
089300000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
089400000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
089500000000     C     OPZOK         IFNE      'S'
089600000000     C                   Z-ADD     SFLERR        VCCNBR
089700000000     C                   ELSE
089800000000     C                   Z-ADD     SFLULT        VCCNBR
089900000000     C                   ENDIF
090000000000     C* AGGIORNO RECORD SFL
090100020115     C                   UPDATE    SE95DVS
090200020115     C                   READC     SE95DVS                                49
090300000000     C                   ENDDO                                                  <-- 1 ---
090400000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
090500000000     C                   Z-ADD     WFLULT        SFLULT
090600000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
090700000000     C     SFLVAR        IFEQ      0
090800000000     C                   MOVEL     '1'           $FINE
090900000000     C                   ENDIF
091000000000     C                   ENDSR
091100000000     C*------------------------------------------------------------------------*
091200000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
091300000000     C*------------------------------------------------------------------------*
091400000000     C     CLRSFL        BEGSR
091500000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
091600000000     C* (0=NON FINE FILE  1=FINE FILE)
091700000000     C                   Z-ADD     0             SFLEOF
091800000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
091900000000     C                   Z-ADD     0             SFLULT
092000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
092100000000     C                   MOVEL     '1'           *IN40
092200020115     C                   WRITE     SE95DVC
092300000000     C                   MOVEL     '0'           *IN40
092400000000     C                   ENDSR
092500000000     C*------------------------------------------------------------------------*
092600000000     C* CARSFL - CARICAMENTO SFL
092700000000     C*------------------------------------------------------------------------*
092800000000     C     CARSFL        BEGSR
092900000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
093000000000     C     SFLULT        IFEQ      0
093100000000     C                   EXSR      SETFIL
093200000000     C                   ENDIF
093300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
093400000000     C                   Z-ADD     SFLULT        WFLULT
093500000000     C* CICLO:
093600000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
093700000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
093800000000     C                   Z-ADD     1             SFLRIG
093900000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
094000000000     C     SFLEOF        ANDEQ     0
094100000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
094200000000     C                   MOVEL     *BLANKS       VSCOPZ
094300000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
094400000000     C                   EXSR      IRESFL
094500000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
094600000000     C                   ADD       1             SFLULT
094700000000     C* SCRITTURA RECORD SFL
094800020115     C                   WRITE     SE95DVS
094900000000     C                   ADD       1             SFLRIG
095000000000     C* LETTURA FILE DA CARICARE
095100000000     C                   EXSR      LETFIL
095200000000     C                   ENDDO                                                  <-- 1 ---
095300000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
095400000000     C     WFLULT        IFNE      SFLULT
095500000000     C                   Z-ADD     WFLULT        VCCNBR
095600000000     C                   ADD       1             VCCNBR
095700000000     C                   ENDIF
095800000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
095900000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
096000000000     C     SFLULT        ANDGT     *ZEROS
096100000000     C                   MOVEL     '1'           $ERR
096200000000     C                   MOVEL     MSG(6)        $MSG
096300000000     C                   EXSR      CARMSG
096400000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
096500020115     C     SFLULT        CHAIN     SE95DVS                            99
096600000000     C     *IN99         IFEQ      '0'
096700000000     C                   SETON                                        45
096800020115     C                   UPDATE    SE95DVS
096900000000     C                   SETOFF                                       45
097000000000     C                   ENDIF
097100000000     C                   ENDIF                                                  <-- 1 ---
097200000000     C                   ENDSR
097300000000     C*------------------------------------------------------------------------*
097400000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
097500000000     C*------------------------------------------------------------------------*
097600000000     C     OUTSFL        BEGSR
097700000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
097800000000     C     MSGULT        IFNE      *ZEROS
097900000000     C                   EXSR      OUTMSG
098000000000     C                   ENDIF
098100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
098200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
098300000000     C     SFLEOF        IFEQ      0
098400000000     C                   MOVEL     '0'           *IN42
098500000000     C                   ELSE
098600000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
098700000000     C                   MOVEL     '1'           *IN42
098800000000     C                   ENDIF
098900000000     C* EMISSIONE SUBFILE
099000000000     C     SFLULT        IFLE      0
099100000000     C*  --> CASO DI SFL VUOTO
099200000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
099300020115     C                   WRITE     SE95DVV                                      *'NON VI SONO DATI'
099400020115     C                   WRITE     SE95DVC                                      *SFL CONTROL
099500020115     C                   READ      SE95DVC                                99    *SFL CONTROL
099600000000     C                   ELSE
099700000000     C*  --> CASO DI SFL CON RECORDS
099800020115     C                   WRITE     SE95DVC                                      *SFL CONTROL
099900020115     C                   READ      SE95DVC                                99    *SFL CONTROL
100000000000     C                   ENDIF
100100000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
100200000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
100300000000     C                   Z-ADD     FRTSFL        VCCNBR
100400000000     C                   ENDSR
100500000000     C*------------------------------------------------------------------------*
100600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
100700000000     C*          CODA MESSAGGI DEL PGM
100800000000     C*------------------------------------------------------------------------*
100900000000     C     CARMSG        BEGSR
101000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
101100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
101200000000     C     MSGULT        IFEQ      0
101300000000     C                   MOVEL     '1'           *IN40
101400020115     C                   WRITE     SE95DVK
101500000000     C                   MOVEL     '0'           *IN40
101600000000     C                   ENDIF
101700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
101800000000     C                   MOVEL     $MSG          DSMSMS
101900000000     C                   CALL      'XMSG'        XMSGPA
102000000000     C     DSMSER        IFEQ      0
102100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
102200000000     C                   ENDIF
102300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
102400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
102500020115     C                   WRITE     SE95DVM                                      *SFL MESS
102600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
102700000000     C                   MOVEL     *BLANKS       $MSG
102800000000     C                   ENDSR
102900000000     C*------------------------------------------------------------------------*
103000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
103100000000     C*------------------------------------------------------------------------*
103200000000     C     OUTMSG        BEGSR
103300000000     C* VISUALIZZAZIONE MESSAGGIO
103400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
103500020115     C                   WRITE     SE95DVK                                      *CONTR MESS
103600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
103700000000     C                   Z-ADD     *ZEROS        MSGULT
103800000000     C                   ENDSR
103900000000     C*------------------------------------------------------------------------*
104000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
104100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
104200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
104300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
104400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
104500000000     C*------------------------------------------------------------------------*
104600000000     C     CARFUN        BEGSR
104700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
104800000000     C     1             DO        31            I                 5 0          --- 1 -->
104900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
105000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
105100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
105200020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
105300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
105400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
105500000000     C                   ELSE                                                   *INTERROG.
105600000000     C                   MOVEL     FUNASI        FAT(I)
105700000000     C                   ENDIF
105800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
105900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
106000000000     C                   MOVEL     FUNAFG        FAT(I)
106100000000     C                   ELSE                                                   *INTERROG.
106200000000     C                   MOVEL     FUNAFI        FAT(I)
106300000000     C                   ENDIF
106400000000     C                   ENDIF                                                  <-- 2 ---
106500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
106600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
106700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
106800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
106900000000     C                   ELSE
107000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
107100000000     C                   ENDIF
107200000000     C                   ENDIF
107300000000     C                   ENDDO                                                  <-- 1 ---
107400000000     C                   ENDSR
107500000000     C*------------------------------------------------------------------------*
107600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
107700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
107800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
107900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
108000000000     C*------------------------------------------------------------------------*
108100000000     C     OUTFUN        BEGSR
108200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
108300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
108400000000     C                   ENDDO
108500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
108600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
108700000000     C     1             DO        31            I
108800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
108900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
109000000000     C                   ADD       1             FUNNAT
109100000000     C                   ENDIF
109200000000     C                   ENDDO
109300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
109400000000     C     FUNNAT        IFGT      8
109500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
109600000000     C                   ENDIF
109700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
109800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
109900000000     C                   Z-ADD     8             FUNVIS            5 0
110000000000     C     FAT(23)       IFEQ      'S'
110100000000     C                   SUB       1             FUNVIS
110200000000     C                   ENDIF
110300000000     C     FAT(24)       IFEQ      'S'
110400000000     C                   SUB       1             FUNVIS
110500000000     C                   ENDIF
110600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
110700000000     C                   Z-ADD     *ZEROS        W                 5 0
110800000000     C                   ADD       1             FUNULT
110900000000     C     FUNULT        IFGT      31
111000000000     C                   Z-ADD     1             FUNULT
111100000000     C                   ENDIF
111200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
111300000000     C     FUNULT        DO        31            I                              --- 1 -->
111400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
111500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
111600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
111700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
111800000000     C     FDE(I)        ANDNE     *BLANKS
111900000000     C     I             IFEQ      23                                           *TOLGO F23
112000000000     C     I             OREQ      24                                           *E F24
112100000000     C                   ELSE
112200000000     C                   ADD       1             W
112300000000     C                   MOVEL     FDE(I)        FUV(W)
112400000000     C                   ENDIF
112500000000     C                   ENDIF                                                  <-- 3 ---
112600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
112700000000     C                   Z-ADD     I             FUNULT
112800000000     C                   ENDIF                                                  <-- 2 ---
112900000000     C                   ENDDO                                                  <-- 1 ---
113000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
113100000000     C                   MOVEL     FUV(1)        VFD001
113200000000     C                   MOVEL     FUV(2)        VFD002
113300000000     C                   MOVEL     FUV(3)        VFD003
113400000000     C                   MOVEL     FUV(4)        VFD004
113500000000     C                   MOVEL     FUV(5)        VFD005
113600000000     C                   MOVEL     FUV(6)        VFD006
113700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
113800000000     C     FAT(24)       ANDEQ     'N'
113900000000     C                   MOVEL     FUV(7)        VFD007
114000000000     C                   MOVEL     FUV(8)        VFD008
114100000000     C                   ELSE                                                   <-- 1 -->
114200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
114300000000     C     FAT(24)       ANDEQ     'S'
114400000000     C                   MOVEL     FDE(23)       VFD007
114500000000     C                   MOVEL     FDE(24)       VFD008
114600000000     C                   ELSE                                                   <-- 2 -->
114700000000     C                   MOVEL     FUV(7)        VFD007
114800000000     C     FAT(23)       IFEQ      'S'
114900000000     C                   MOVEL     FDE(23)       VFD008
115000000000     C                   ELSE
115100000000     C                   MOVEL     FDE(24)       VFD008
115200000000     C                   ENDIF
115300000000     C                   ENDIF                                                  <-- 2 ---
115400000000     C                   ENDIF                                                  <-- 1 ---
115500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
115600000000     C                   ENDSR
115700000000     C*------------------------------------------------------------------------*
115800000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
115900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
116000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
116100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
116200000000     C*------------------------------------------------------------------------*
116300000000     C     CONFUN        BEGSR
116400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
116500000000     C     1             DO        31            I                              --- 1 -->
116600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
116700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
116800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
116900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
117000000000     C                   EXSR      CARMSG
117100000000     C                   MOVEL     '1'           $ERR
117200000000     C                   ENDIF                                                  <-- 3 ---
117300000000     C                   ENDIF                                                  <-- 2 ---
117400000000     C                   ENDDO                                                  <-- 1 ---
117500000000     C                   ENDSR
117600000000     C*------------------------------------------------------------------------*
117700000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
117800000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
117900000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
118000000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
118100000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
118200000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
118300000000     C*------------------------------------------------------------------------*
118400000000     C     CAROPZ        BEGSR
118500000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISUA
118600000000     C     1             DO        24            I                              --- 1 -->
118700000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
118800000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTRA
118900000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIST
119000000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
119100020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
119200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
119300000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
119400000000     C                   ELSE                                                   *INTERROG.
119500000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
119600000000     C                   ENDIF
119700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
119800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
119900000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
120000000000     C                   ELSE                                                   *INTERROG.
120100000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
120200000000     C                   ENDIF
120300000000     C                   ENDIF                                                  <-- 2 ---
120400000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
120500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
120600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
120700000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
120800000000     C                   ELSE
120900000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
121000000000     C                   ENDIF
121100000000     C                   ENDIF
121200000000     C                   ENDDO                                                  <-- 1 ---
121300000000     C                   ENDSR
121400000000     C*------------------------------------------------------------------------*
121500000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
121600000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
121700000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
121800000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
121900000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
122000000000     C*------------------------------------------------------------------------*
122100000000     C     OUTOPZ        BEGSR
122200000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
122300000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
122400000000     C                   ENDDO
122500000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
122600000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
122700000000     C                   ADD       1             OPZULT
122800000000     C     OPZULT        IFGT      24
122900000000     C                   Z-ADD     1             OPZULT
123000000000     C                   END
123100000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
123200000000     C     1             DO        24            I
123300000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
123400000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
123500000000     C                   ADD       1             OPZNAT
123600000000     C                   ENDIF
123700000000     C                   ENDDO
123800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
123900000000     C                   Z-ADD     0             W
124000000000     C     OPZULT        DO        24            I                              --- 1 -->
124100000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
124200000000     C     W             IFLT      8                                            --- 2 -->
124300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124400000000     C     OAT(I)        IFEQ      'S'
124500000000     C     ODE(I)        ANDNE     *BLANKS
124600000000     C                   ADD       1             W
124700000000     C                   MOVEL     ODE(I)        OPV(W)
124800000000     C                   ENDIF
124900000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
125000000000     C                   Z-ADD     I             OPZULT
125100000000     C                   ENDIF                                                  <-- 2 ---
125200000000     C                   ENDDO                                                  <-- 1 ---
125300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
125400000000     C                   MOVEL     OPV(1)        VOD001
125500000000     C                   MOVEL     OPV(2)        VOD002
125600000000     C                   MOVEL     OPV(3)        VOD003
125700000000     C                   MOVEL     OPV(4)        VOD004
125800000000     C                   MOVEL     OPV(5)        VOD005
125900000000     C                   MOVEL     OPV(6)        VOD006
126000000000     C                   MOVEL     OPV(7)        VOD007
126100000000     C                   MOVEL     OPV(8)        VOD008
126200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
126300000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
126400000000     C     OPZNAT        IFGT      8
126500000000     C                   MOVEL     'S'           FAT(23)
126600000000     C                   ENDIF
126700000000     C                   ENDSR
126800000000     C*------------------------------------------------------------------------*
126900000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
127000000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
127100000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
127200000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
127300000000     C*------------------------------------------------------------------------*
127400000000     C     CONOPZ        BEGSR
127500000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
127600000000     C                   MOVEL     *BLANKS       OPZOK             1
127700000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OPZ
127800000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
127900000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
128000000000     C     OAT(I)        IFEQ      'S'
128100000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
128200000000     C                   MOVEL     'S'           OPZOK
128300000000     C                   ELSE
128400000000     C                   MOVEL     'N'           OPZOK             1
128500000000     C                   ENDIF
128600000000     C                   ENDIF                                                  <-- 2 ---
128700000000     C                   ENDDO                                                  <-- 1 ---
128800000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
128900000000     C     OPZOK         IFNE      'S'
129000000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
129100000000     C                   EXSR      CARMSG
129200000000     C                   MOVEL     '1'           $ERR
129300000000     C                   MOVEL     'N'           OPZOK             1
129400000000     C                   ENDIF
129500000000     C                   ENDSR
129600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
129700020115TISE95R- Tasto funzionale non valido.                                          1
129800020115TISE95R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
129900020115TISE95R- Opzione non valida.                                                   3
130000020115TISE95R- Opzione abilitata ma non gestita. Avvertire CED.                      4
130100020115TISE95R- Inizio elenco.                                                        5
130200020115TISE95R- Fine elenco.                                                          6
130300020115TISE95R- 7
130400020115TISE95R- 8
130500020115TISE95R- 9
130600020115TISE95R- 10
130700020115TISE95R- 11
130800020115TISE95R- Elemento annullato, consentito solo il ripristino                    12
130900020115TISE95R- Elemento non annullato, impossibile ripristinare                     13
131000020115TISE95R- 14
131100020115TISE95R- 15
131200000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
131300000000- 1-1 -                   -    NN     NN
131400000000- 2-2 -2=Modifica         -    SN     NN
131500000000- 3-3 -                   -    NN     NN
131600000000- 4-4 -4=Cancellazione    -    SN     NN
131700000000- 5-5 -5=Visualizzazione  -    SS     SS
131800000000- 6-6 -                   -    NN     NN
131900000000- 7-7 -7=Ripristino       -    SN     NN
132000000000- 8-8 -                   -    NN     NN
132100000000- 9-9 -                   -    NN     NN
132200000000-10-10-                   -    NN     NN
132300000000-11-  -                   -    NN     NN
132400000000-12-  -                   -    NN     NN
132500000000-13-  -                   -    NN     NN
132600000000-14-  -                   -    NN     NN
132700000000-15-  -                   -    NN     NN
132800000000-16-  -                   -    NN     NN
132900000000-17-  -                   -    NN     NN
133000000000-18-  -                   -    NN     NN
133100000000-19-  -                   -    NN     NN
133200000000-20-  -                   -    NN     NN
133300000000-21-  -                   -    NN     NN
133400000000-22-  -                   -    NN     NN
133500000000-23-  -                   -    NN     NN
133600000000-24-  -                   -    NN     NN
133700000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
133800000000F1    -                   -    NN     NN
133900000000F2    -                   -    NN     NN
134000000000F3    -F3=Fine            -    SS     SS
134100000000F4    -                   -    NN     NN
134200000000F5    -F5=Rivisualizza    -    SS     SS
134300000000F6    -F6=Inserimento     -    SN     NN
134400000000F7    -                   -    NN     NN
134500000000F8    -                   -    NN     NN
134600000000F9    -                   -    NN     NN
134700000000F10   -                   -    NN     NN
134800000000F11   -                   -    NN     NN
134900000000F12   -F12=Ritorno        -    SS     SS
135000000000F13   -                   -    NN     NN
135100000000F14   -                   -    NN     NN
135200000000F15   -                   -    NN     NN
135300000000F16   -                   -    NN     NN
135400000000F17   -F17=Inizio dati    -    SS     SS
135500000000F18   -F18=Fine dati      -    SS     SS
135600000000F19   -                   -    NN     NN
135700000000F20   -                   -    NN     NN
135800000000F21   -                   -    NN     NN
135900000000F22   -                   -    NN     NN
136000000000F23   -F23=Altre opzioni  -    NN     NN
136100000000F24   -F24=Altri tasti    -    NN     NN
136200000000ENTER -                   -    SS     SS
136300000000ROLLUP-                   -    SS     SS
136400000000ROLLDN-                   -    SS     SS
136500000000HELP  -                   -    NN     NN
136600000000PRINT -                   -    NN     NN
136700000000BSPACE-                   -    NN     NN
136800000000CLEAR -                   -    NN     NN
136900000000**
137000000000GENNAIO
137100000000FEBBRAIO
137200000000MARZO
137300000000APRILE
137400000000MAGGIO
137500000000GIUGNO
137600000000LUGLIO
137700000000AGOSTO
137800000000SETTEMBRE
137900000000OTTOBRE
138000000000NOVEMBRE
138100000000DICEMBRE
