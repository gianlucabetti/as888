000100930923     H*===============================================================*
000200931115     H*             - AGGIORNAMENTO SALDI BOLLE-        - FILTRO -    *
000300930923     H*===============================================================*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500991223     FTNSB22D   CF   E             WORKSTN INFDS(DEVDS1)
000600991223     F                                     SFILE(SB22DVM:MSGULT)
000700930922     D*================================================================
000800931111     D MSG             S             78    DIM(22) CTDATA PERRCD(1)             *MESSAGGI
000900930806     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001000930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001100930803     D FFU             S              6    DIM(31)                              *FUNZIONE
001200930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
001300930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
001400930802     D* TABELLA DESCRIZIONI A VIDEO
001500930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
001600930802     D* TABELLA TASTI FUNZIONALI PREMUTI
001700930802     D KEYS            S              1    DIM(40)
001800930922     D*================================================================
001900930723     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002000930723     D DSFUN         E DS
002100930803     D  FUNRIG                 1     40
002200930922     D*================================================================
002300930127     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002400930127     D DSMSG         E DS
002500930922     D*================================================================
002600020920     D* DS REPERIMENTO DATI UTENTE
002700020920     D TIBS34DS      E DS                                                       *Profili utente
002800020920     D DDATIUTE      E DS                                                       *Dati utente
002900020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003000950309     D*================================================================
003100991223     D* DS PER LANCIO PGM 'XSRDA8'
003200991223     D WLBDA8          DS                  INZ
003300950309     D  G08DAT                 1      8  0
003400950309     D  G08INV                 9     16  0
003500950309     D  G08ERR                17     17
003600950309     D  G08TGI                18     22  0
003700991223     D*================================================================
003800991223     D* DS S/COMPOSIZIONE DATA
003900991223     D                 DS
004000991223     D  DSGIO                  1      2  0
004100991223     D  DSMES                  3      4  0
004200991223     D  DSANN                  5      8  0
004300991223     D  DSDAT                  1      8  0
004400930922     D*================================================================
004500000000     D* DS PER GESTIONE TASTI FUNZIONALI
004600930802     D DSKEYS        E DS
004700930803     D  KEYRIG                 1     40
004800930922     D*================================================================
004900930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005000930806     D                SDS
005100930806     D  USER                 254    263
005200930922     D*================================================================
005300930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400000000     D DEVDS1          DS
005500000000     D  KEY                  369    369
005600930721     D  CURSOR               370    371
005700930923     D*================================================================
005800931008     D PARAM           DS                  INZ
005900991223     D  PARDAT                 1      8  0
006000991223     D*================================================================
006100991223     D* ARCHITETTURA
006200991223     D KPJBA         E DS
006300000000     C*================================================================
006400940826     C* MAIN - PROGRAMMA FILTRO
006500930922     C*================================================================
006600931008     C*
006700000000     C* CICLO FINO A CHE:
006800930922DO  1C     $FINE         DOUEQ     '1'
006900000000     C*
007000931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
007100000000     C*
007200930721     C* CONTROLLA GESTIONE VIDEO
007300930922IF  2C     $GES          IFEQ      'V1'
007400930806     C                   EXSR      GESV1                                        *PARAMETRI
007500930922E   2C                   ENDIF
007600000000     C*
007700930922E   1C                   ENDDO
007800000000     C*
007900931018     C* FINE MAIN DEL PGM
008000000000     C                   SETON                                        LR
008100930922     C*================================================================
008200930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
008300930922     C*================================================================
008400930922     C     GESV1         BEGSR
008500930922     C*
008600930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
008700930922     C                   EXSR      CARV1                                        *CARICA VIDEO
008800930922E   1C                   ENDIF
008900930922     C*
009000931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
009100931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
009200931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
009300931018E   1C                   ENDIF
009400931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
009500930922     C*
009600991223     C                   EXFMT     SB22DV1                                      *PARAMETRI
009700930922     C*
009800930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
009900930922     C*
010000930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
010100931008SL  2C                   SELECT
010200930922     C     KEY           WHENEQ    ENTER
010300930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
010400930922     C     KEY           WHENEQ    F6
010500930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
010600930922     C                   OTHER
010700930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
010800930922E   2C                   ENDSL
010900930922E   1C                   ENDIF
011000930922     C*
011100930922     C                   ENDSR
011200930713     C*================================================================
011300930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
011400930713     C*================================================================
011500930806     C     CARV1         BEGSR
011600930713     C*
011700930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
011800930713     C*
011900930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012000930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012100930713     C*
012200991223     C                   WRITE     SB22DVT                                      *TESTATA
012300991223     C                   WRITE     SB22DVF                                      *FUNZIONI
012400930713     C*
012500930713     C                   ENDSR
012600930804     C*================================================================
012700930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
012800930804     C*================================================================
012900930806     C     ENTV1         BEGSR
013000930806     C*
013100930806     C* CONTROLLA I CAMPI IMMESSI
013200930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
013300930804     C*
013400930804     C                   ENDSR
013500000000     C*================================================================
013600930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
013700000000     C*================================================================
013800930126     C     F06GES        BEGSR
013900930923     C*
014000930924     C* CONTROLLA I CAMPI IMMESSI
014100930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
014200930923     C*
014300991223     C* LANCIO PROGRAMMA DI STAMPA                         *NO ERRORI
014400991223IF  1C     $ERR          IFEQ      '0'
014500991223     C                   Z-ADD     DATV1C        PARDAT
014600991223     C                   MOVEL     'SB23'        KCOAZ
014700930923     C                   MOVEL     PARAM         KPJBU
014800930923     C                   CALL      'BCH10'
014900930923     C                   PARM                    KPJBA
015000930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
015100930923E   1C                   ENDIF
015200930127     C*
015300000000     C                   ENDSR
015400930205     C*================================================================
015500930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
015600930205     C*================================================================
015700930906     C     DECODI        BEGSR
015800930205     C*
015900931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
016000950308     C                   SETOFF                                       6061
016100930712     C*
016200931115     C* CONTROLLO DATA INIZIO
016300991223     C                   Z-ADD     *ZEROS        DATV1C            8 0
016400991223     C                   Z-ADD     V1CDAT        G08DAT
016500991223     C                   Z-ADD     *ZEROS        G08INV
016600991223     C                   MOVEL     '0'           G08ERR
016700950308     C                   CALL      'XSRDA8'
016800991223     C                   PARM                    WLBDA8
016900991223     C                   Z-ADD     G08DAT        V1CDAT
017000991223     C                   Z-ADD     G08INV        DATV1C
017100991223IF  1C     G08ERR        IFEQ      '1'
017200931115     C                   MOVEL     '1'           $ERR
017300931115     C                   MOVEL     MSG(7)        $MSG
017400931115     C                   EXSR      CARMSG
017500931115     C                   SETON                                        60
017600991223E   1C                   ENDIF
017700930923     C*
017800931008     C                   ENDSR
017900020920     C*--------------------------------------------------------------------------------------------*
018000020920     C* REPERISCE I DATI UTENTE
018100020920     C*--------------------------------------------------------------------------------------------*
018200020920     C     REPDATIUTE    BEGSR
018300020920     C*
018400020920     C* INIZIALIZZA VARIABILI DI WRK
018500020920     C                   CLEAR                   TIBS34DS
018600020920     C                   CLEAR                   AZUTEDS
018700020920     C                   CLEAR                   DDATIUTE
018800020920     C*
018900020920     C     *DTAARA       DEFINE    §azute        azuteds
019000020920     C     *DTAARA       DEFINE    §datiute      ddatiute
019100020920     C                   IN(E)     *DTAARA
019200020920if  1C                   IF        %Error
019300020920     c                   EVAL      I34Tla = 'L'
019400020920     C                   CALL      'TIBS34R'
019500020920     C                   PARM                    Tibs34Ds
019600020920     C                   IN        *DTAARA
019700020920e   1C                   ENDIF
019800020920     C*
019900020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
020000020920     C                   MOVEL(P)  RSUT          DSFIRS           20
020100020920     C*
020200020920     C                   ENDSR
020300930923     C*================================================================
020400931008     C* *INZSR  - ROUTINE INIZIALE
020500000000     C*================================================================
020600930713     C     *INZSR        BEGSR
020700000000     C*
020800000000     C* RICEVIMENTO PARAMETRI
020900000000     C     *ENTRY        PLIST
021000930922     C                   PARM                    KPJBA
021100930714     C*
021200930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
021300930921     C                   EXSR      INZSTD
021400991223     C*
021500991223     C* REPERISCE LA DATA CORRENTE
021600991223     C                   TIME                    N14              14 0          *ORA(6) + DATA(8)
021700991223     C                   MOVE      N14           N8                8 0          *DATA G/M/AA
021800991223     C                   Z-ADD     N8            DSDAT
021900991223     C     DSMES         SUB       1             DSMES
022000991223IF  1C     DSMES         IFEQ      0
022100991223     C                   Z-ADD     12            DSMES
022200991223     C     DSANN         SUB       1             DSANN
022300991223E   1C                   ENDIF
022400991223IF  1C     DSMES         IFEQ      01
022500991223     C     DSMES         OREQ      03
022600991223     C     DSMES         OREQ      05
022700991223     C     DSMES         OREQ      07
022800991223     C     DSMES         OREQ      08
022900991223     C     DSMES         OREQ      10
023000991223     C     DSMES         OREQ      12
023100991223     C                   Z-ADD     31            DSGIO
023200991223E   1C                   ENDIF
023300991223IF  1C     DSMES         IFEQ      04
023400991223     C     DSMES         OREQ      06
023500991223     C     DSMES         OREQ      09
023600991223     C     DSMES         OREQ      11
023700991223     C                   Z-ADD     30            DSGIO
023800991223E   1C                   ENDIF
023900991223IF  1C     DSMES         IFEQ      02
024000991223     C     DSANN         DIV       4             RESTO             4 0
024100991223     C                   MVR                     RESTO
024200991223IF  2C     RESTO         IFEQ      *ZEROS
024300991223     C                   Z-ADD     29            DSGIO
024400991223X   2C                   ELSE
024500991223     C                   Z-ADD     28            DSGIO
024600991223E   2C                   ENDIF
024700991223E   1C                   ENDIF
024800991223     C*
024900991223     C* IMPOSTAZIONI INIZIALI CAMPI VIDEO:
025000991223     C                   Z-ADD     DSDAT         V1CDAT
025100991223     C*
025200991223     C* CARICA VIDEO
025300930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
025400930923     C*
025500000000     C                   ENDSR
025600930924     C*================================================================
025700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025800930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
025900930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
026000930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
026100930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
026200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026300930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026400930924     C*================================================================
026500930805     C*
026600930714     C*================================================================
026700930923     C* INZSTD - ROUTINE INIZIALE STANDARD
026800930714     C*================================================================
026900930803     C     INZSTD        BEGSR
027000930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
027100930802     C                   CALL      'XKEYS'
027200930802     C                   PARM                    DSKEYS
027300930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
027400930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
027500930714     C     XMSGPA        PLIST
027600930714     C                   PARM                    DSMSMS
027700930714     C                   PARM                    DSMSPG
027800930714     C                   PARM                    DSMSKE
027900930714     C                   PARM                    DSMSER
028000930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
028100991223     C                   MOVEL     'TNSB22R'     DSMSPG                         *NOME PGM CHIAMANTE
028200991223     C                   MOVEL     'TNSB22R'     VMCQUE                         *CODA MESSAGGI DESTI
028300930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
028400930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
028500930805     C* INIZIALIZZO FLAG CONTROLLO PGM
028600930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
028700930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
028800930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
028900930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
029000930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
029100930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
029200930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
029300930714     C* LANCIO ROUTINE PER VERIFICARE SE
029400930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
029500930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
029600020920     C                   EXSR      REPDATIUTE
029700930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
029800930721     C                   BITOFF    '01234567'    POSMI1            1
029900930721     C                   BITOFF    '01234567'    POSMI2            1
030000930721     C                   BITOFF    '01234567'    POSMA1            1
030100930721     C                   BITOFF    '01234567'    POSMA2            1
030200930721     C                   BITON     '34'          POSMI1            1            *18
030300930721     C                   BITON     '7'           POSMI2            1            *01
030400930721     C                   BITON     '34'          POSMA1            1            *18
030500930721     C                   BITON     '0'           POSMA2            1            *80
030600930721     C                   MOVE      POSMI2        POSMIN            2
030700930721     C                   MOVEL     POSMI1        POSMIN
030800930721     C                   MOVE      POSMA2        POSMAX            2
030900930721     C                   MOVEL     POSMA1        POSMAX
031000930714     C                   ENDSR
031100930804     C*================================================================
031200930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
031300930804     C*================================================================
031400930804     C     FUNSTD        BEGSR
031500930804     C                   SELECT
031600930804     C     KEY           WHENEQ    F3
031700930804     C                   EXSR      F03STD
031800931115     C     KEY           WHENEQ    F12
031900931115     C                   EXSR      F12STD
032000930804     C     KEY           WHENEQ    F24
032100930804     C                   EXSR      F24STD
032200930804     C                   OTHER
032300930804     C                   MOVEL     MSG(2)        $MSG
032400930804     C                   EXSR      CARMSG
032500930804     C                   ENDSL
032600930804     C                   ENDSR
032700930804     C*================================================================
032800930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
032900930804     C*================================================================
033000930804     C     F03STD        BEGSR
033100930923     C*
033200930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
033300930927     C                   MOVEL     '1'           $FINE
033400930927     C*
033500930927     C                   ENDSR
033600931115     C*================================================================
033700931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
033800931115     C*================================================================
033900931115     C     F12STD        BEGSR
034000931115     C* IMPOSTO FLAG DI FIN PGM
034100931115     C                   MOVEL     '1'           $FINE
034200931115     C                   ENDSR
034300930804     C*================================================================
034400930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
034500930804     C*================================================================
034600930804     C     F24STD        BEGSR
034700930923     C*
034800930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
034900930804     C                   EXSR      OUTFUN
035000930923     C*
035100930804     C                   ENDSR
035200930721     C*================================================================
035300930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
035400930721     C*          CODA MESSAGGI DEL PGM
035500930721     C*================================================================
035600930721     C     CARMSG        BEGSR
035700930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
035800930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
035900930721     C     MSGULT        IFEQ      0
036000930721     C                   MOVEL     '1'           *IN40
036100991223     C                   WRITE     SB22DVK
036200930721     C                   MOVEL     '0'           *IN40
036300930805     C                   ENDIF
036400930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
036500930721     C                   MOVEL     $MSG          DSMSMS
036600930721     C                   CALL      'XMSG'        XMSGPA
036700930721     C     DSMSER        IFEQ      0
036800930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
036900930805     C                   ENDIF
037000930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
037100930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
037200991223     C                   WRITE     SB22DVM                                      *SFL MESS
037300930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
037400930721     C                   MOVEL     *BLANKS       $MSG
037500930721     C                   ENDSR
037600930721     C*================================================================
037700930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
037800930721     C*================================================================
037900930721     C     OUTMSG        BEGSR
038000930721     C* VISUALIZZAZIONE MESSAGGIO
038100930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
038200991223     C                   WRITE     SB22DVK                                      *CONTR MESS
038300930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
038400930721     C                   Z-ADD     *ZEROS        MSGULT
038500930721     C                   ENDSR
038600930803     C*================================================================
038700930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
038800930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
038900930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
039000930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
039100930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
039200930803     C*================================================================
039300930803     C     CARFUN        BEGSR
039400930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
039500930923     C     1             DO        31            I                 5 0          --- 1 -->
039600930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
039700930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
039800930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
039900020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
040000930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
040100930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
040200930803     C                   ELSE                                                   *INTERROG.
040300930803     C                   MOVEL     FUNASI        FAT(I)
040400930803     C                   ENDIF
040500930803     C                   ELSE                                                   <-- 2 --> *FILIALE
040600930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
040700930803     C                   MOVEL     FUNAFG        FAT(I)
040800930803     C                   ELSE                                                   *INTERROG.
040900930922     C                   MOVEL     FUNAFI        FAT(I)
041000930803     C                   ENDIF
041100930803     C                   ENDIF                                                  <-- 2 ---
041200930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
041300930806     C                   MOVEL     USER          WEDP              3            *PROFILO
041400930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
041500930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
041600930806     C                   ELSE
041700930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
041800930806     C                   ENDIF
041900930806     C                   ENDIF
042000930803     C                   ENDDO                                                  <-- 1 ---
042100930803     C                   ENDSR
042200930803     C*================================================================
042300930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
042400930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
042500930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
042600930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
042700930803     C*================================================================
042800930803     C     OUTFUN        BEGSR
042900930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
043000930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
043100930803     C                   ENDDO
043200930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
043300930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
043400930803     C     1             DO        31            I
043500930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
043600930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
043700930803     C                   ADD       1             FUNNAT
043800930803     C                   ENDIF
043900930803     C                   ENDDO
044000930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
044100930803     C     FUNNAT        IFGT      8
044200930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
044300930803     C                   ENDIF
044400930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
044500930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
044600930803     C                   Z-ADD     8             FUNVIS            5 0
044700930803     C     FAT(23)       IFEQ      'S'
044800930803     C                   SUB       1             FUNVIS
044900930803     C                   ENDIF
045000930803     C     FAT(24)       IFEQ      'S'
045100930803     C                   SUB       1             FUNVIS
045200930803     C                   ENDIF
045300930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
045400930923     C                   Z-ADD     *ZEROS        W                 5 0
045500930803     C                   ADD       1             FUNULT
045600930803     C     FUNULT        IFGT      31
045700930803     C                   Z-ADD     1             FUNULT
045800930805     C                   ENDIF
045900930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
046000930803     C     FUNULT        DO        31            I                              --- 1 -->
046100930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
046200930803     C     W             IFLT      FUNVIS                                       --- 2 -->
046300930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
046400930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
046500930803     C     FDE(I)        ANDNE     *BLANKS
046600930805     C     I             IFEQ      23                                           *TOLGO F23
046700930805     C     I             OREQ      24                                           *E F24
046800930803     C                   ELSE
046900930803     C                   ADD       1             W
047000930803     C                   MOVEL     FDE(I)        FUV(W)
047100930803     C                   ENDIF
047200930803     C                   ENDIF                                                  <-- 3 ---
047300930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
047400930803     C                   Z-ADD     I             FUNULT
047500930803     C                   ENDIF                                                  <-- 2 ---
047600930803     C                   ENDDO                                                  <-- 1 ---
047700930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
047800930806     C                   MOVEL     FUV(1)        VFD001
047900930806     C                   MOVEL     FUV(2)        VFD002
048000930806     C                   MOVEL     FUV(3)        VFD003
048100930806     C                   MOVEL     FUV(4)        VFD004
048200930806     C                   MOVEL     FUV(5)        VFD005
048300930806     C                   MOVEL     FUV(6)        VFD006
048400930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
048500930803     C     FAT(24)       ANDEQ     'N'
048600930806     C                   MOVEL     FUV(7)        VFD007
048700930806     C                   MOVEL     FUV(8)        VFD008
048800930803     C                   ELSE                                                   <-- 1 -->
048900930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
049000930803     C     FAT(24)       ANDEQ     'S'
049100930806     C                   MOVEL     FDE(23)       VFD007
049200930806     C                   MOVEL     FDE(24)       VFD008
049300930803     C                   ELSE                                                   <-- 2 -->
049400930806     C                   MOVEL     FUV(7)        VFD007
049500930803     C     FAT(23)       IFEQ      'S'
049600930806     C                   MOVEL     FDE(23)       VFD008
049700930803     C                   ELSE
049800930806     C                   MOVEL     FDE(24)       VFD008
049900930805     C                   ENDIF
050000930805     C                   ENDIF                                                  <-- 2 ---
050100930805     C                   ENDIF                                                  <-- 1 ---
050200930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
050300930803     C                   ENDSR
050400930803     C*================================================================
050500930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
050600930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
050700930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
050800930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
050900930803     C*================================================================
051000930803     C     CONFUN        BEGSR
051100930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
051200930803     C     1             DO        31            I                              --- 1 -->
051300930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
051400930805     C* CONTROLLO SE IL TASTO E' ATTIVO
051500930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
051600930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
051700930803     C                   EXSR      CARMSG
051800930803     C                   MOVEL     '1'           $ERR
051900930803     C                   ENDIF                                                  <-- 3 ---
052000930803     C                   ENDIF                                                  <-- 2 ---
052100930803     C                   ENDDO                                                  <-- 1 ---
052200930803     C                   ENDSR
052300930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
052400991223TNSB22R- Tasto funzionale non valido.                                          1
052500991223TNSB22R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
052600991223TNSB22R- Opzione non valida.                                                   3
052700991223TNSB22R- Opzione abilitata ma non gestita. Avvertire CED.                      4
052800991223TNSB22R- Inizio elenco.                                                        5
052900991223TNSB22R- Fine elenco.                                                          6
053000991223TNSB22R- Data inizio errata.                                                   7
053100991223TNSB22R- Data fine errata.                                                     8
053200991223TNSB22R- Data inizio superiore alla data fine.                                 9
053300991223TNSB22R- Data inizio maggiore della data piu alta consentita.                 10
053400991223TNSB22R- Data fine maggiore della data piu alta consentita.                   11
053500991223TNSB22R- Codice filiale errato                                                12
053600991223TNSB22R- Codice area errato                                                   13
053700991223TNSB22R- Codice divisione errato                                              14
053800991223TNSB22R- Tab. '$3' non trovata. Impossibile continuare.                       15
053900991223TNSB22R- 16                                                                   16
054000991223TNSB22R- 17                                                                   17
054100991223TNSB22R- 18                                                                   18
054200991223TNSB22R- Immettere solo una selezione per filiale, area o divisione           19
054300991223TNSB22R- 20                                                                   20
054400991223TNSB22R- 21                                                                   21
054500991223TNSB22R- 22                                                                   22
054600930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
054700930806F1    -                   -    NN     NN
054800930723F2    -                   -    NN     NN
054900930921F3    -F3=Fine            -    SS     SS
055000930723F4    -                   -    NN     NN
055100930723F5    -                   -    NN     NN
055200931026F6    -F6=Lancio          -    SS     SS
055300930723F7    -                   -    NN     NN
055400930723F8    -                   -    NN     NN
055500930723F9    -                   -    NN     NN
055600930723F10   -                   -    NN     NN
055700930723F11   -                   -    NN     NN
055800931115F12   -F12=Ritorno        -    SS     SS
055900930723F13   -                   -    NN     NN
056000930723F14   -                   -    NN     NN
056100930723F15   -                   -    NN     NN
056200930723F16   -                   -    NN     NN
056300930723F17   -                   -    NN     NN
056400930723F18   -                   -    NN     NN
056500930723F19   -                   -    NN     NN
056600930723F20   -                   -    NN     NN
056700930804F21   -                   -    NN     NN
056800930927F22   -                   -    NN     NN
056900930723F23   -                   -    NN     NN
057000930803F24   -F24=Altri tasti    -    NN     NN
057100930921ENTER -                   -    SS     SS
057200930802ROLLUP-                   -    NN     NN
057300930802ROLLDN-                   -    NN     NN
057400930802HELP  -                   -    NN     NN
057500930802PRINT -                   -    NN     NN
057600930802BSPACE-                   -    NN     NN
057700930802CLEAR -                   -    NN     NN
