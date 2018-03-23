000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - ELENCO FERMATE - SUBFILE DI SCELTA
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL16D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL16DVS:SFLULT)
001000000000     F                                     SFILE(TL16DVM:MSGULT)
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTNTLR01L  UF A E           K DISK
001300000000     F                                     COMMIT
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
001900000000     D*-------------------
002000000000     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             *MESSAGGI
002100000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002200000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
002300000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
002400000000     D*-------------------
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D*-------------------
002700000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002800000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000000000     D*-------------------
003100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003200000000     D*-------------------
003300000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003400000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003500000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
003600000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
003700000000     D*-------------------
003800000000     D* TABELLA DESCRIZIONI A VIDEO
003900000000     D*-------------------
004000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004100000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004200000000     D*-------------------
004300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004400000000     D*-------------------
004500000000     D KEYS            S              1    DIM(40)
004600000000     D*-------------------
004700000000     D* FILIALI
004800000000     D*-------------------
004900971125     D ORG             S              3  0 DIM(500)                             *CODICE
005000971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
005100000000     D*------------------------------------------------------------------------*
005200000000     D* INPUT
005300000000     D*------------------------------------------------------------------------*
005400000000     D*---------------------
005500000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005600000000     D*---------------------
005700000000     D DSOPZ         E DS
005800000000     D  OPZRIG                 1     40
005900000000     D*---------------------
006000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006100000000     D*---------------------
006200000000     D DSFUN         E DS
006300000000     D  FUNRIG                 1     40
006400000000     D*---------------------
006500000000     D* DS PER GESTIONE TASTI FUNZIONALI
006600000000     D*---------------------
006700000000     D DSKEYS        E DS
006800000000     D  KEYRIG                 1     40
006900000000     D*---------------------
007000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007100000000     D*---------------------
007200000000     D                SDS
007300000000     D  USER                 254    263
007400000000     D*---------------------
007500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007600000000     D*---------------------
007700000000     D DEVDS1          DS
007800000000     D  KEY                  369    369
007900000000     D  CURSOR               370    371
008000000000     D  FRTSFL               378    379B 0
008100000000     D*---------------------
008200000000     D* ARCHITETTURA
008300000000     D*---------------------
008400000000     D KPJBA         E DS
008500000000     D*---------------------
008600000000     D* DS ESTERNA DI PROCEDURA
008700000000     D*---------------------
008800000000     D DS00          E DS                  EXTNAME(TNTL00DS)
008900000000     D*---------------------
009000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
009100000000     D*---------------------
009200000000     D DSMSG         E DS
009300020715
009400020715     d DdatiUte      e ds
009500020715     d AzuteDs       e ds                  ExtName(Azute00f)
009600020715     d Tibs34Ds      e ds                  Inz
009700020715
009800000000     D*------------------
009900000000     D* COSTANTI
010000000000     D*------------------
010100000000     D DESAND          C                   CONST('ANDATA    ')
010200000000     D DESRIT          C                   CONST('RITORNO   ')
010300000000     C*------------------------------------------------------------------------*
010400000000     C* MAIN LINE
010500000000     C*------------------------------------------------------------------------*
010600000000     C*
010700000000     C* CICLO FINO A CHE:
010800000000DO  1C     $FINE         DOUEQ     '1'
010900000000     C*
011000000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011100000000     C*
011200000000     C* CONTROLLA GESTIONE VIDEO
011300000000IF  2C     $GES          IFEQ      'VS'
011400000000     C                   EXSR      GESVS                                        *SUBFILE
011500000000E   2C                   ENDIF
011600000000     C*
011700000000E   1C                   ENDDO
011800000000     C*
011900000000IF  1C     $F03          IFEQ      '1'
012000000000     C                   MOVEL     '1'           D00F03
012100000000E   1C                   ENDIF
012200000000IF  1C     $F12          IFEQ      '1'
012300000000     C                   MOVEL     '1'           D00F12
012400000000E   1C                   ENDIF
012500000000     C* FINE PGM
012600000000     C                   MOVEL     DS00          KPJBU
012700000000     C                   SETON                                        LR
012800000000     C*------------------------------------------------------------------------*
012900000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
013000000000     C*------------------------------------------------------------------------*
013100000000     C     CARVS         BEGSR
013200000000     C*
013300000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
013400000000     C*
013500000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
013600000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
013700000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
013800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014000000000     C*
014100000000     C                   WRITE     TL16DVT                                      *TESTATA
014200000000     C                   WRITE     TL16DVO                                      *OPZIONI
014300000000     C                   WRITE     TL16DVF                                      *FUNZIONI
014400000000     C*
014500000000     C                   EXSR      POSSFL                                       *CARICA SFL
014600000000     C*
014700000000     C                   ENDSR
014800000000     C*------------------------------------------------------------------------*
014900000000     C* GESVS  - GESTIONE SUBFILE
015000000000     C*------------------------------------------------------------------------*
015100000000     C     GESVS         BEGSR
015200000000     C*
015300000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015400000000     C                   WRITE     TL16DVT                                      *TESTATA
015500000000     C                   WRITE     TL16DVO                                      *OPZIONI
015600000000     C                   WRITE     TL16DVF                                      *FUNZIONI
015700000000E   1C                   ENDIF
015800000000     C*
015900000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
016000000000     C*
016100000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
016200000000     C*
016300000000IF  1C     $ERR          IFEQ      '0'
016400000000SL  2C                   SELECT
016500000000     C                   OTHER
016600000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
016700000000E   2C                   ENDSL
016800000000E   1C                   ENDIF
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* GESOPZ - GESTIONE OPZIONI
017300000000     C*------------------------------------------------------------------------*
017400000000     C     GESOPZ        BEGSR
017500000000     C*
017600000000SL  1C                   SELECT
017700000000     C     VSCOPZ        WHENEQ    ' 1'
017800000000     C                   EXSR      O01GES                                       *SCELTA
017900000000     C                   OTHER
018000000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
018100000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
018200000000E   1C                   ENDSL
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* O01GES - GESTIONE OPZIONE 1=SCELTA
018700000000     C*------------------------------------------------------------------------*
018800000000     C     O01GES        BEGSR
018900000000     C*
019000000000     C* RITORNA IL CODICE SELEZIONATO
019100000000IF  1C     $ERR          IFEQ      '0'
019200000000     C                   Z-ADD     VSCFIL        D00FIL
019300000000     C                   Z-ADD     VSCSEQ        D00SEQ
019400000000     C                   MOVEL     '1'           $FINE
019500000000E   1C                   ENDIF
019600000000     C*
019700000000     C                   ENDSR
019800000000     C*------------------------------------------------------------------------*
019900000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
020000000000     C*------------------------------------------------------------------------*
020100000000     C     NEWPOS        BEGSR
020200000000     C*
020300000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
020400000000     C*        1 = RICHIESTO POSIZIONAMENTO
020500000000     C                   Z-ADD     0             SWPOSI            7 0
020600000000     C*
020700000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
020800000000     C*            NON OCCORRE RICARICARE IL SUBFILE
020900000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
021000000000     C                   Z-ADD     0             SWMODI            1 0
021100000000     C*
021200000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
021300000000     C                   EXSR      CHKPAR
021400000000     C*
021500000000     C                   ENDSR
021600000000     C*------------------------------------------------------------------------*
021700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
021800000000     C*------------------------------------------------------------------------*
021900000000     C     CHKPAR        BEGSR
022000000000     C*
022100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
022200000000     C* CURSORE SUL CAMPO DEL CONTROL
022300000000IF  1C     $ERR          IFEQ      '1'
022400000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
022500000000X   1C                   ELSE
022600000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
022700000000E   1C                   ENDIF
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
023200000000     C*------------------------------------------------------------------------*
023300000000     C     SETFIL        BEGSR
023400000000     C*
023500000000     C                   MOVEL     '1'           *IN43                          *EMETTE SFILE
023600000000     C*
023700000000     C                   Z-ADD     D00TRN        KLRTRN
023800000000     C                   Z-ADD     D00DDE        KLRDDE
023900000000     C     KEYTLR        SETLL     TNTLR01L                           99        *FINE FILE
024000000000     C*
024100000000     C  N99              EXSR      LETFIL
024200000000     C*
024300000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
024400000000IF  1C     *IN99         IFEQ      *ON
024500000000     C                   Z-ADD     1             SFLEOF
024600000000E   1C                   ENDIF
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*------------------------------------------------------------------------*
025000000000     C* LETFIL - LETTURA RECORD SUCESSIVO
025100000000     C*------------------------------------------------------------------------*
025200000000     C     LETFIL        BEGSR
025300000000     C*
025400000000     C* LEGGO FINO A:
025500000000     C*     - FINE FILE
025600000000     C*     - TROVATO RECORD VALIDO
025700000000     C                   MOVEL     'N'           WRECOK            1            *RECORD NON VALIDO
025800000000DO  1C     *IN99         DOWEQ     *OFF
025900000000     C     WRECOK        ANDEQ     'N'
026000000000     C     KEYTLR        READE(N)  TNTLR01L                               99
026100000000     C*
026200000000IF  2C     *IN99         IFEQ      *ON
026300000000     C                   Z-ADD     1             SFLEOF
026400000000X   2C                   ELSE
026500000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
026600000000E   2C                   ENDIF
026700000000     C*
026800000000E   1C                   ENDDO
026900000000     C*
027000000000     C                   ENDSR
027100000000     C*------------------------------------------------------------------------*
027200000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
027300000000     C*------------------------------------------------------------------------*
027400000000     C     CHKREC        BEGSR
027500000000     C*
027600000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
027700000000     C*
027800000000     C* RECORD ANNULLATO, ESCLUDE
027900000000IF  1C     TLRATB        IFNE      *BLANKS
028000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028100000000E   1C                   ENDIF
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*------------------------------------------------------------------------*
028500000000     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
028600000000     C*------------------------------------------------------------------------*
028700000000     C     IRESFL        BEGSR
028800000000     C*
028900000000     C                   Z-ADD     TLRNFM        VSCSEQ                         *NUMERO FERMATA
029000000000     C                   Z-ADD     TLRFIL        VSCFIL                         *FILIALE
029100000000     C                   MOVEL     TLRSCA        VSCSCA                         *SCARICO
029200000000     C                   MOVEL     TLRCAR        VSCCAR                         *CARICO
029300000000     C                   MOVEL     TLRGFV        VSCGFV                         *GIORNO APERTURA FV
029400000000     C                   Z-ADD     TLRKMT        VSCKMT                         *KILOMETRI
029500000000     C                   Z-ADD     TLRTEM        VSCTEM                         *TEMPO
029600000000     C* DECODIFICHE
029700000000IF  1C     TLRTFM        IFEQ      'A'
029800000000     C                   MOVEL     DESAND        VSDTFM                         *TIPO FERMATA
029900000000X   1C                   ELSE
030000000000     C                   MOVEL     DESRIT        VSDTFM                         *TIPO FERMATA
030100000000E   1C                   ENDIF
030200000000     C*
030300000000     C                   SETOFF                                           28
030400000000     C                   Z-ADD     1             J
030500000000     C     VSCFIL        LOOKUP    ORG(J)                                 28
030600000000IF  1C     *IN28         IFEQ      *ON
030700000000     C                   MOVEL     DORG(J)       VSDFIL                         *FILIALE
030800000000E   1C                   ENDIF
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C* CARTAB - CARICA LE TABELLE
031300000000     C*------------------------------------------------------------------------*
031400000000     C     CARTAB        BEGSR
031500000000     C*--------------------
031600000000     C* FILIALI
031700000000     C*--------------------
031800000000     C                   Z-ADD     *ZEROS        J                 4 0
031900000000     C     *LOVAL        SETLL     AZORG01L
032000000000     C                   READ      AZORG01L                               99
032100000000DO  1C     *IN99         DOWEQ     *OFF
032200000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
032300000000     C                   ADD       1             J
032400000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
032500000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
032600000000E   2C                   ENDIF
032700000000     C                   READ      AZORG01L                               99
032800000000E   1C                   ENDDO
032900000000     C*
033000000000     C                   ENDSR
033100000000     C*------------------------------------------------------------------------*
033200000000     C* *INZSR - ROUTINE INIZIALE
033300000000     C*------------------------------------------------------------------------*
033400000000     C     *INZSR        BEGSR
033500000000     C*--------------------
033600000000     C* RICEVIMENTO PARAMETRI
033700000000     C*--------------------
033800000000     C     *ENTRY        PLIST
033900000000     C                   PARM                    KPJBA
034000000000     C                   MOVEL     KPJBU         DS00
034100000000     C*--------------------
034200000000     C* VARIABILI RIFERITE AL DATA BASE
034300000000     C*--------------------
034400000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
034500000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
034600000000     C*--------------------
034700000000     C* CHIAVI DI LETTURA
034800000000     C*--------------------
034900000000     C* LETTURA TNTLR01L - PARZIALE
035000000000     C     KEYTLR        KLIST
035100000000     C                   KFLD                    KLRTRN                         *TRAINO
035200000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
035300000000     C*--------------------
035400000000     C* VARIABILI DI CONTROLLO
035500000000     C*--------------------
035600000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
035700000000     C***
035800000000     C* IMPOSTAZIONI VIDEO
035900000000     C***
036000000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
036100000000     C                   MOVE      D00OP0        INTGES            2
036200000000IF  1C     INTGES        IFEQ      '02'
036300000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
036400000000X   1C                   ELSE
036500000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
036900000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
037000000000IF  1C     *IN99         IFEQ      *ON
037100000000     C     *IN99         ANDEQ     *OFF
037200000000     C* FERMATE
037300000000     C                   UPDATE    TNTLR000
037400000000     C                   WRITE     TNTLR000
037500000000E   1C                   ENDIF
037600000000     C*
037700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
037800000000     C                   EXSR      INZSTD
037900000000     C*
038000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
038100000000     C                   EXSR      INZSFL
038200000000     C*
038300000000     C* CARICA LE TABELLE
038400000000     C                   EXSR      CARTAB
038500000000     C*
038600000000     C* EMETTE VIDEATA SUBFILE
038700000000     C                   EXSR      CHKPAR
038800000000     C                   EXSR      CARVS
038900000000     C*
039000000000     C                   ENDSR
039100000000     C*------------------------------------------------------------------------*
039200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
039500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
039600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
039700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039900000000     C*------------------------------------------------------------------------*
040000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
040100000000     C*------------------------------------------------------------------------*
040200000000     C     INZSTD        BEGSR
040300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
040400000000     C                   CALL      'XKEYS'
040500000000     C                   PARM                    DSKEYS
040600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
040700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
040800000000     C     XMSGPA        PLIST
040900000000     C                   PARM                    DSMSMS
041000000000     C                   PARM                    DSMSPG
041100000000     C                   PARM                    DSMSKE
041200000000     C                   PARM                    DSMSER
041300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
041400000000     C                   MOVEL     'TNTL16R'     DSMSPG                         *NOME PGM CHIAMANTE
041500000000     C                   MOVEL     'TNTL16R'     VMCQUE                         *CODA MESSAGGI DESTI
041600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
041700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
041800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
041900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
042000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
042100000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F03
042200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
042300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
042400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
042500000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
042600000000     C* LANCIO ROUTINE PER VERIFICARE SE
042700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
042800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
042900020715
043000020715     c     *dtaara       define    §azute        azuteds
043100020715     c     *dtaara       define    §datiute      ddatiute
043200020715
043300020715     c                   in(E)     *dtaara
043400020715     c                   if        %Error or rsut = *blanks
043500020715     c                   clear                   tibs34ds
043600020715     c                   call      'TIBS34R'
043700020715     c                   parm                    Tibs34Ds
043800020715     c                   in        *dtaara
043900020715     c                   endif
044000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
044100000000     C                   BITOFF    '01234567'    POSMI1            1
044200000000     C                   BITOFF    '01234567'    POSMI2            1
044300000000     C                   BITOFF    '01234567'    POSMA1            1
044400000000     C                   BITOFF    '01234567'    POSMA2            1
044500000000     C                   BITON     '34'          POSMI1            1            *18
044600000000     C                   BITON     '7'           POSMI2            1            *01
044700000000     C                   BITON     '34'          POSMA1            1            *18
044800000000     C                   BITON     '0'           POSMA2            1            *80
044900000000     C                   MOVE      POSMI2        POSMIN            2
045000000000     C                   MOVEL     POSMI1        POSMIN
045100000000     C                   MOVE      POSMA2        POSMAX            2
045200000000     C                   MOVEL     POSMA1        POSMAX
045300000000     C                   ENDSR
045400000000     C*------------------------------------------------------------------------*
045500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
045600000000     C*------------------------------------------------------------------------*
045700000000     C     FUNSTD        BEGSR
045800000000     C                   SELECT
045900000000     C     KEY           WHENEQ    F3
046000000000     C                   EXSR      F03STD
046100000000     C     KEY           WHENEQ    F12
046200000000     C                   EXSR      F12STD
046300000000     C     KEY           WHENEQ    F24
046400000000     C                   EXSR      F24STD
046500000000     C                   OTHER
046600000000     C                   MOVEL     MSG(2)        $MSG
046700000000     C                   EXSR      CARMSG
046800000000     C                   ENDSL
046900000000     C                   ENDSR
047000000000     C*------------------------------------------------------------------------*
047100000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
047200000000     C*------------------------------------------------------------------------*
047300000000     C     F03STD        BEGSR
047400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
047500000000     C                   MOVEL     '1'           $F03
047600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
047700000000     C                   MOVEL     '1'           $FINE
047800000000     C* PULISCE IL MESSAGGIO
047900000000     C                   MOVEL     *BLANKS       D00MSG
048000000000     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
048300000000     C*------------------------------------------------------------------------*
048400000000     C     F12STD        BEGSR
048500000000     C*
048600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
048700000000     C                   MOVEL     '1'           $F12
048800000000     C* IMPOSTO FLAG DI FIN PGM
048900000000     C                   MOVEL     '1'           $FINE
049000000000     C* PULISCE IL MESSAGGIO
049100000000     C                   MOVEL     *BLANKS       D00MSG
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
049500000000     C*------------------------------------------------------------------------*
049600000000     C     F24STD        BEGSR
049700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
049800000000     C                   EXSR      OUTFUN
049900000000     C                   ENDSR
050000000000     C*------------------------------------------------------------------------*
050100000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
050200000000     C*------------------------------------------------------------------------*
050300000000     C     INZSFL        BEGSR
050400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
050500000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
050600000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
050700000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
050800000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
050900000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
051000000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
051100000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
051200000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
051300000000     C                   Z-ADD     12            SFLPAG
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
051700000000     C*------------------------------------------------------------------------*
051800000000     C     FUNSFL        BEGSR
051900000000     C                   SELECT
052000000000     C     KEY           WHENEQ    ENTER
052100000000     C                   EXSR      ENTSFL
052200000000     C     KEY           WHENEQ    F3
052300000000     C                   EXSR      F03STD
052400000000     C     KEY           WHENEQ    F5
052500000000     C                   EXSR      F05STD
052600000000     C     KEY           WHENEQ    F12
052700000000     C                   EXSR      F12STD
052800000000     C     KEY           WHENEQ    F17
052900000000     C                   EXSR      F17STD
053000000000     C     KEY           WHENEQ    F18
053100000000     C                   EXSR      F18STD
053200000000     C     KEY           WHENEQ    F23
053300000000     C                   EXSR      F23STD
053400000000     C     KEY           WHENEQ    F24
053500000000     C                   EXSR      F24STD
053600000000     C     KEY           WHENEQ    ROLLUP
053700000000     C                   EXSR      RUPSTD
053800000000     C     KEY           WHENEQ    ROLLDN
053900000000     C                   EXSR      RDNSTD
054000000000     C                   OTHER
054100000000     C                   MOVEL     MSG(2)        $MSG
054200000000     C                   EXSR      CARMSG
054300000000     C                   ENDSL
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
054700000000     C*------------------------------------------------------------------------*
054800000000     C     F05STD        BEGSR
054900000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
055000000000     C                   EXSR      POSSFL
055100000000     C                   ENDSR
055200000000     C*------------------------------------------------------------------------*
055300000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
055400000000     C*------------------------------------------------------------------------*
055500000000     C     F17STD        BEGSR
055600000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
055700000000     C                   Z-ADD     1             VCCNBR
055800000000     C                   ENDSR
055900000000     C*------------------------------------------------------------------------*
056000000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
056100000000     C*------------------------------------------------------------------------*
056200000000     C     F18STD        BEGSR
056300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
056400000000     C                   Z-ADD     SFLULT        VCCNBR
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
056800000000     C*------------------------------------------------------------------------*
056900000000     C     F23STD        BEGSR
057000000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
057100000000     C                   EXSR      OUTOPZ
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
057500000000     C*------------------------------------------------------------------------*
057600000000     C     RUPSTD        BEGSR
057700000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
057800000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
057900000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
058000000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
058100000000     C                   ELSE
058200000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
058300000000     C                   ENDIF
058400000000     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
058700000000     C*------------------------------------------------------------------------*
058800000000     C     RDNSTD        BEGSR
058900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
059000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
059100000000     C     CURSOR        IFGE      POSMIN
059200000000     C     CURSOR        ANDLE     POSMAX
059300000000     C                   ELSE
059400000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
059500000000     C                   MOVEL     '1'           $ERR
059600000000     C                   MOVEL     MSG(5)        $MSG
059700000000     C                   EXSR      CARMSG
059800000000     C                   ENDIF
059900000000     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
060200000000     C*------------------------------------------------------------------------*
060300000000     C     ENTSFL        BEGSR
060400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
060500000000     C                   EXSR      NEWPOS
060600000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
060700000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
060800000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
060900000000     C     SWPOSI        IFNE      0                                            --- 2 -->
061000000000     C                   EXSR      POSSFL
061100000000     C                   ELSE                                                   <-- 2 -->
061200000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
061300000000     C     SFLULT        IFNE      0                                            --- 3 -->
061400000000     C                   EXSR      OPZSFL
061500000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
061600000000     C     SWMODI        IFNE      0                                            --- 4 -->
061700000000     C                   EXSR      POSSFL
061800000000     C                   ENDIF                                                  <-- 4 ---
061900000000     C                   ENDIF                                                  <-- 3 ---
062000000000     C                   ENDIF                                                  <-- 2 ---
062100000000     C                   ENDIF                                                  <-- 1 ---
062200000000     C                   ENDSR
062300000000     C*------------------------------------------------------------------------*
062400000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
062500000000     C*------------------------------------------------------------------------*
062600000000     C     POSSFL        BEGSR
062700000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
062800000000     C                   EXSR      CLRSFL
062900000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
063000000000     C                   EXSR      SETFIL
063100000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
063200000000     C     SFLEOF        IFEQ      0
063300000000     C                   EXSR      CARSFL
063400000000     C                   ENDIF
063500000000     C                   ENDSR
063600000000     C*------------------------------------------------------------------------*
063700000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
063800000000     C*------------------------------------------------------------------------*
063900000000     C     OPZSFL        BEGSR
064000000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
064100000000     C                   MOVEL     'S'           OPZOK             1
064200000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
064300000000     C                   Z-ADD     0             SFLVAR            5 0
064400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
064500000000     C                   Z-ADD     0             SFLERR
064600000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
064700000000     C                   Z-ADD     SFLULT        WFLULT
064800000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
064900000000     C                   READC     TL16DVS                                49
065000000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
065100000000     C     $ERR          ANDEQ     '0'
065200000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
065300000000     C                   ADD       1             SFLVAR
065400000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
065500000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
065600000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
065700000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
065800000000     C                   EXSR      CONOPZ
065900000000     C     OPZOK         IFEQ      'S'
066000000000     C     $ERR          ANDEQ     '0'
066100000000     C                   EXSR      GESOPZ
066200000000     C                   ENDIF
066300000000     C                   ENDIF                                                  <-- 3 ---
066400000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
066500000000     C*            2. PULIZIA SCELTA IN SFL
066600000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
066700000000     C                   MOVEL     '0'           *IN41
066800000000     C                   MOVEL     *BLANKS       VSCOPZ
066900000000     C                   ELSE                                                   <-- 3 -->
067000000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
067100000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
067200000000     C                   MOVEL     '1'           *IN41
067300000000     C                   Z-ADD     SFLULT        SFLERR
067400000000     C                   ENDIF                                                  <-- 3 ---
067500000000     C                   ELSE                                                   <-- 2 -->
067600000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
067700000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
067800000000     C                   MOVEL     '1'           *IN41
067900000000     C                   ENDIF                                                  <-- 2 ---
068000000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
068100000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
068200000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
068300000000     C     OPZOK         IFNE      'S'
068400000000     C                   Z-ADD     SFLERR        VCCNBR
068500000000     C                   ELSE
068600000000     C                   Z-ADD     SFLULT        VCCNBR
068700000000     C                   ENDIF
068800000000     C* AGGIORNO RECORD SFL
068900000000     C                   UPDATE    TL16DVS
069000000000     C     $ERR          IFEQ      '0'
069100000000     C                   READC     TL16DVS                                49
069200000000     C                   ENDIF
069300000000     C                   ENDDO                                                  <-- 1 ---
069400000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
069500000000     C                   Z-ADD     WFLULT        SFLULT
069600000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
069700000000     C     SFLVAR        IFEQ      0
069800000000     C                   MOVEL     '1'           $FINE
069900000000     C                   ENDIF
070000000000     C                   ENDSR
070100000000     C*------------------------------------------------------------------------*
070200000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
070300000000     C*------------------------------------------------------------------------*
070400000000     C     CLRSFL        BEGSR
070500000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
070600000000     C* (0=NON FINE FILE  1=FINE FILE)
070700000000     C                   Z-ADD     0             SFLEOF
070800000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
070900000000     C                   Z-ADD     0             SFLULT
071000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
071100000000     C                   MOVEL     '1'           *IN40
071200000000     C                   WRITE     TL16DVC
071300000000     C                   MOVEL     '0'           *IN40
071400000000     C                   ENDSR
071500000000     C*------------------------------------------------------------------------*
071600000000     C* CARSFL - CARICAMENTO SFL
071700000000     C*------------------------------------------------------------------------*
071800000000     C     CARSFL        BEGSR
071900000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
072000000000     C     SFLULT        IFEQ      0
072100000000     C                   EXSR      SETFIL
072200000000     C                   ENDIF
072300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
072400000000     C                   Z-ADD     SFLULT        WFLULT
072500000000     C* CICLO:
072600000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
072700000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
072800000000     C                   Z-ADD     1             SFLRIG
072900000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
073000000000     C     SFLEOF        ANDEQ     0
073100000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
073200000000     C                   MOVEL     *BLANKS       VSCOPZ
073300000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
073400000000     C                   EXSR      IRESFL
073500000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
073600000000     C                   ADD       1             SFLULT
073700000000     C* SCRITTURA RECORD SFL
073800000000     C                   WRITE     TL16DVS
073900000000     C                   ADD       1             SFLRIG
074000000000     C* LETTURA FILE DA CARICARE
074100000000     C                   EXSR      LETFIL
074200000000     C                   ENDDO                                                  <-- 1 ---
074300000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
074400000000     C     WFLULT        IFNE      SFLULT
074500000000     C                   Z-ADD     WFLULT        VCCNBR
074600000000     C                   ADD       1             VCCNBR
074700000000     C                   ENDIF
074800000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
074900000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
075000000000     C     SFLULT        ANDGT     *ZEROS
075100000000     C                   MOVEL     '1'           $ERR
075200000000     C                   MOVEL     MSG(6)        $MSG
075300000000     C                   EXSR      CARMSG
075400000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
075500000000     C     SFLULT        CHAIN     TL16DVS                            99
075600000000     C     *IN99         IFEQ      '0'
075700000000     C                   SETON                                        45
075800000000     C                   UPDATE    TL16DVS
075900000000     C                   SETOFF                                       45
076000000000     C                   ENDIF
076100000000     C                   ENDIF                                                  <-- 1 ---
076200000000     C                   ENDSR
076300000000     C*------------------------------------------------------------------------*
076400000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
076500000000     C*------------------------------------------------------------------------*
076600000000     C     OUTSFL        BEGSR
076700000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
076800000000     C     MSGULT        IFNE      *ZEROS
076900000000     C                   EXSR      OUTMSG
077000000000     C                   ENDIF
077100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
077200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
077300000000     C     SFLEOF        IFEQ      0
077400000000     C                   MOVEL     '0'           *IN42
077500000000     C                   ELSE
077600000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
077700000000     C                   MOVEL     '1'           *IN42
077800000000     C                   ENDIF
077900000000     C* EMISSIONE SUBFILE
078000000000     C     SFLULT        IFLE      0
078100000000     C*  --> CASO DI SFL VUOTO
078200000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
078300000000     C                   WRITE     TL16DVV                                      *'NON VI SONO DATI'
078400000000     C                   WRITE     TL16DVC                                      *SFL CONTROL
078500000000     C                   READ      TL16DVC                                99    *SFL CONTROL
078600000000     C                   ELSE
078700000000     C*  --> CASO DI SFL CON RECORDS
078800000000     C                   WRITE     TL16DVC                                      *SFL CONTROL
078900000000     C                   READ      TL16DVC                                99    *SFL CONTROL
079000000000     C                   ENDIF
079100000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
079200000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
079300000000     C                   Z-ADD     FRTSFL        VCCNBR
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
079700000000     C*          CODA MESSAGGI DEL PGM
079800000000     C*------------------------------------------------------------------------*
079900000000     C     CARMSG        BEGSR
080000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
080100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
080200000000     C     MSGULT        IFEQ      0
080300000000     C                   MOVEL     '1'           *IN40
080400000000     C                   WRITE     TL16DVK
080500000000     C                   MOVEL     '0'           *IN40
080600000000     C                   ENDIF
080700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
080800000000     C                   MOVEL     $MSG          DSMSMS
080900000000     C                   CALL      'XMSG'        XMSGPA
081000000000     C     DSMSER        IFEQ      0
081100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
081200000000     C                   ENDIF
081300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
081400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
081500000000     C                   WRITE     TL16DVM                                      *SFL MESS
081600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
081700000000     C                   MOVEL     *BLANKS       $MSG
081800000000     C                   ENDSR
081900000000     C*------------------------------------------------------------------------*
082000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
082100000000     C*------------------------------------------------------------------------*
082200000000     C     OUTMSG        BEGSR
082300000000     C* VISUALIZZAZIONE MESSAGGIO
082400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
082500000000     C                   WRITE     TL16DVK                                      *CONTR MESS
082600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
082700000000     C                   Z-ADD     *ZEROS        MSGULT
082800000000     C                   ENDSR
082900000000     C*------------------------------------------------------------------------*
083000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
083100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
083200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
083300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
083400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
083500000000     C*------------------------------------------------------------------------*
083600000000     C     CARFUN        BEGSR
083700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
083800000000     C     1             DO        31            I                 5 0          --- 1 -->
083900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
084000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
084100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
084200020715     c                   If        DutLpo = 'S'
084300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
084400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
084500000000     C                   ELSE                                                   *INTERROG.
084600000000     C                   MOVEL     FUNASI        FAT(I)
084700000000     C                   ENDIF
084800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
084900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
085000000000     C                   MOVEL     FUNAFG        FAT(I)
085100000000     C                   ELSE                                                   *INTERROG.
085200000000     C                   MOVEL     FUNAFI        FAT(I)
085300000000     C                   ENDIF
085400000000     C                   ENDIF                                                  <-- 2 ---
085500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
085600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
085700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
085800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
085900000000     C                   ELSE
086000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
086100000000     C                   ENDIF
086200000000     C                   ENDIF
086300000000     C                   ENDDO                                                  <-- 1 ---
086400000000     C                   ENDSR
086500000000     C*------------------------------------------------------------------------*
086600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
086700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
086800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
086900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
087000000000     C*------------------------------------------------------------------------*
087100000000     C     OUTFUN        BEGSR
087200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
087300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
087400000000     C                   ENDDO
087500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
087600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
087700000000     C     1             DO        31            I
087800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
087900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
088000000000     C                   ADD       1             FUNNAT
088100000000     C                   ENDIF
088200000000     C                   ENDDO
088300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
088400000000     C     FUNNAT        IFGT      8
088500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
088600000000     C                   ENDIF
088700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
088800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
088900000000     C                   Z-ADD     8             FUNVIS            5 0
089000000000     C     FAT(23)       IFEQ      'S'
089100000000     C                   SUB       1             FUNVIS
089200000000     C                   ENDIF
089300000000     C     FAT(24)       IFEQ      'S'
089400000000     C                   SUB       1             FUNVIS
089500000000     C                   ENDIF
089600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
089700000000     C                   Z-ADD     *ZEROS        W                 5 0
089800000000     C                   ADD       1             FUNULT
089900000000     C     FUNULT        IFGT      31
090000000000     C                   Z-ADD     1             FUNULT
090100000000     C                   ENDIF
090200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
090300000000     C     FUNULT        DO        31            I                              --- 1 -->
090400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
090500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
090600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
090700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
090800000000     C     FDE(I)        ANDNE     *BLANKS
090900000000     C     I             IFEQ      23                                           *TOLGO F23
091000000000     C     I             OREQ      24                                           *E F24
091100000000     C                   ELSE
091200000000     C                   ADD       1             W
091300000000     C                   MOVEL     FDE(I)        FUV(W)
091400000000     C                   ENDIF
091500000000     C                   ENDIF                                                  <-- 3 ---
091600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
091700000000     C                   Z-ADD     I             FUNULT
091800000000     C                   ENDIF                                                  <-- 2 ---
091900000000     C                   ENDDO                                                  <-- 1 ---
092000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
092100000000     C                   MOVEL     FUV(1)        VFD001
092200000000     C                   MOVEL     FUV(2)        VFD002
092300000000     C                   MOVEL     FUV(3)        VFD003
092400000000     C                   MOVEL     FUV(4)        VFD004
092500000000     C                   MOVEL     FUV(5)        VFD005
092600000000     C                   MOVEL     FUV(6)        VFD006
092700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
092800000000     C     FAT(24)       ANDEQ     'N'
092900000000     C                   MOVEL     FUV(7)        VFD007
093000000000     C                   MOVEL     FUV(8)        VFD008
093100000000     C                   ELSE                                                   <-- 1 -->
093200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
093300000000     C     FAT(24)       ANDEQ     'S'
093400000000     C                   MOVEL     FDE(23)       VFD007
093500000000     C                   MOVEL     FDE(24)       VFD008
093600000000     C                   ELSE                                                   <-- 2 -->
093700000000     C                   MOVEL     FUV(7)        VFD007
093800000000     C     FAT(23)       IFEQ      'S'
093900000000     C                   MOVEL     FDE(23)       VFD008
094000000000     C                   ELSE
094100000000     C                   MOVEL     FDE(24)       VFD008
094200000000     C                   ENDIF
094300000000     C                   ENDIF                                                  <-- 2 ---
094400000000     C                   ENDIF                                                  <-- 1 ---
094500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
094600000000     C                   ENDSR
094700000000     C*------------------------------------------------------------------------*
094800000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
094900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
095000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
095100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
095200000000     C*------------------------------------------------------------------------*
095300000000     C     CONFUN        BEGSR
095400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
095500000000     C     1             DO        31            I                              --- 1 -->
095600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
095700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
095800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
095900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
096000000000     C                   EXSR      CARMSG
096100000000     C                   MOVEL     '1'           $ERR
096200000000     C                   ENDIF                                                  <-- 3 ---
096300000000     C                   ENDIF                                                  <-- 2 ---
096400000000     C                   ENDDO                                                  <-- 1 ---
096500000000     C                   ENDSR
096600000000     C*------------------------------------------------------------------------*
096700000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
096800000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
096900000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
097000000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
097100000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
097200000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
097300000000     C*------------------------------------------------------------------------*
097400000000     C     CAROPZ        BEGSR
097500000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
097600000000     C     1             DO        24            I                              --- 1 -->
097700000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
097800000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
097900000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
098000000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
098100020715     c                   If        DutLpo = 'S'
098200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
098300000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
098400000000     C                   ELSE                                                   *INTERROG.
098500000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
098600000000     C                   ENDIF
098700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
098800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
098900000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
099000000000     C                   ELSE                                                   *INTERROG.
099100000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
099200000000     C                   ENDIF
099300000000     C                   ENDIF                                                  <-- 2 ---
099400000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
099500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
099600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
099700000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
099800000000     C                   ELSE
099900000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
100000000000     C                   ENDIF
100100000000     C                   ENDIF
100200000000     C                   ENDDO                                                  <-- 1 ---
100300000000     C                   ENDSR
100400000000     C*------------------------------------------------------------------------*
100500000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
100600000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
100700000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
100800000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
100900000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
101000000000     C*------------------------------------------------------------------------*
101100000000     C     OUTOPZ        BEGSR
101200000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
101300000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
101400000000     C                   ENDDO
101500000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
101600000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
101700000000     C                   ADD       1             OPZULT
101800000000     C     OPZULT        IFGT      24
101900000000     C                   Z-ADD     1             OPZULT
102000000000     C                   END
102100000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
102200000000     C     1             DO        24            I
102300000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
102400000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
102500000000     C                   ADD       1             OPZNAT
102600000000     C                   ENDIF
102700000000     C                   ENDDO
102800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
102900000000     C                   Z-ADD     0             W
103000000000     C     OPZULT        DO        24            I                              --- 1 -->
103100000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
103200000000     C     W             IFLT      8                                            --- 2 -->
103300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
103400000000     C     OAT(I)        IFEQ      'S'
103500000000     C     ODE(I)        ANDNE     *BLANKS
103600000000     C                   ADD       1             W
103700000000     C                   MOVEL     ODE(I)        OPV(W)
103800000000     C                   ENDIF
103900000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
104000000000     C                   Z-ADD     I             OPZULT
104100000000     C                   ENDIF                                                  <-- 2 ---
104200000000     C                   ENDDO                                                  <-- 1 ---
104300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
104400000000     C                   MOVEL     OPV(1)        VOD001
104500000000     C                   MOVEL     OPV(2)        VOD002
104600000000     C                   MOVEL     OPV(3)        VOD003
104700000000     C                   MOVEL     OPV(4)        VOD004
104800000000     C                   MOVEL     OPV(5)        VOD005
104900000000     C                   MOVEL     OPV(6)        VOD006
105000000000     C                   MOVEL     OPV(7)        VOD007
105100000000     C                   MOVEL     OPV(8)        VOD008
105200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
105300000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
105400000000     C     OPZNAT        IFGT      8
105500000000     C                   MOVEL     'S'           FAT(23)
105600000000     C                   ENDIF
105700000000     C                   ENDSR
105800000000     C*------------------------------------------------------------------------*
105900000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
106000000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
106100000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
106200000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
106300000000     C*------------------------------------------------------------------------*
106400000000     C     CONOPZ        BEGSR
106500000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
106600000000     C                   MOVEL     *BLANKS       OPZOK             1
106700000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
106800000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
106900000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
107000000000     C     OAT(I)        IFEQ      'S'
107100000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
107200000000     C                   MOVEL     'S'           OPZOK
107300000000     C                   ELSE
107400000000     C                   MOVEL     'N'           OPZOK             1
107500000000     C                   ENDIF
107600000000     C                   ENDIF                                                  <-- 2 ---
107700000000     C                   ENDDO                                                  <-- 1 ---
107800000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
107900000000     C     OPZOK         IFNE      'S'
108000000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
108100000000     C                   EXSR      CARMSG
108200000000     C                   MOVEL     '1'           $ERR
108300000000     C                   MOVEL     'N'           OPZOK             1
108400000000     C                   ENDIF
108500000000     C                   ENDSR
108600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
108700000000TNTL16R- Tasto funzionale non valido.                                          1
108800000000TNTL16R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
108900000000TNTL16R- Opzione non valida.                                                   3
109000000000TNTL16R- Opzione abilitata ma non gestita. Avvertire CED.                      4
109100000000TNTL16R- Inizio elenco.                                                        5
109200000000TNTL16R- Fine elenco.                                                          6
109300000000TNTL16R- 7
109400000000TNTL16R- 8
109500000000TNTL16R- 9
109600000000TNTL16R- 10
109700000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
109800000000- 1-1 -1=Scelta           -    SN     SN
109900000000- 2-2 -                   -    NN     NN
110000000000- 3-3 -                   -    NN     NN
110100000000- 4-4 -                   -    NN     NN
110200000000- 5-5 -                   -    NN     NN
110300000000- 6-6 -                   -    NN     NN
110400000000- 7-7 -                   -    NN     NN
110500000000- 8-8 -                   -    NN     NN
110600000000- 9-9 -                   -    NN     NN
110700000000-10-  -                   -    NN     NN
110800000000-11-  -                   -    NN     NN
110900000000-12-  -                   -    NN     NN
111000000000-13-  -                   -    NN     NN
111100000000-14-  -                   -    NN     NN
111200000000-15-  -                   -    NN     NN
111300000000-16-  -                   -    NN     NN
111400000000-17-  -                   -    NN     NN
111500000000-18-  -                   -    NN     NN
111600000000-19-  -                   -    NN     NN
111700000000-20-  -                   -    NN     NN
111800000000-21-  -                   -    NN     NN
111900000000-22-  -                   -    NN     NN
112000000000-23-  -                   -    NN     NN
112100000000-24-  -                   -    NN     NN
112200000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
112300000000F1    -                   -    NN     NN
112400000000F2    -                   -    NN     NN
112500000000F3    -                   -    NN     NN
112600000000F4    -                   -    NN     NN
112700000000F5    -                   -    NN     NN
112800000000F6    -                   -    NN     NN
112900000000F7    -                   -    NN     NN
113000000000F8    -                   -    NN     NN
113100000000F9    -                   -    NN     NN
113200000000F10   -                   -    NN     NN
113300000000F11   -                   -    NN     NN
113400000000F12   -F12=Ritorno        -    SS     SS
113500000000F13   -                   -    NN     NN
113600000000F14   -                   -    NN     NN
113700000000F15   -                   -    NN     NN
113800000000F16   -                   -    NN     NN
113900000000F17   -                   -    NN     NN
114000000000F18   -                   -    NN     NN
114100000000F19   -                   -    NN     NN
114200000000F20   -                   -    NN     NN
114300000000F21   -                   -    NN     NN
114400000000F22   -                   -    NN     NN
114500000000F23   -F23=Altre opzioni  -    NN     NN
114600000000F24   -F24=Altri tasti    -    NN     NN
114700000000ENTER -                   -    SS     SS
114800000000ROLLUP-                   -    SS     SS
114900000000ROLLDN-                   -    SS     SS
115000000000HELP  -                   -    NN     NN
115100000000PRINT -                   -    NN     NN
115200000000BSPACE-                   -    NN     NN
115300000000CLEAR -                   -    NN     NN
