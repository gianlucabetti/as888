000100930923     H*===============================================================*
000200950717     H* - SALVATAGGIO LIBRERIA TASAAMM SU CASSETTA                    *
000300930923     H*===============================================================*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950717     FTNSB39D   CF   E             WORKSTN INFDS(DEVDS1)
000600950717     F                                     SFILE(SB39DVM:MSGULT)
000700950308     FTABEL00F  IF   E           K DISK
000800930922     D*================================================================
000900931111     D MSG             S             78    DIM(22) CTDATA PERRCD(1)             *MESSAGGI
001000930806     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001100930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001200930803     D FFU             S              6    DIM(31)                              *FUNZIONE
001300930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
001400930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
001500930802     D* TABELLA DESCRIZIONI A VIDEO
001600930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
001700930802     D* TABELLA TASTI FUNZIONALI PREMUTI
001800930802     D KEYS            S              1    DIM(40)
001900930922     D*================================================================
002000930723     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002100930723     D DSFUN         E DS
002200930803     D  FUNRIG                 1     40
002300930922     D*================================================================
002400930127     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002500930127     D DSMSG         E DS
002600930922     D*================================================================
002700020920     D* DS REPERIMENTO DATI UTENTE
002800020920     D TIBS34DS      E DS                                                       *Profili utente
002900020920     D DDATIUTE      E DS                                                       *Dati utente
003000020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003100950308     D*================================================================
003200950308     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
003300950308     D DS$3          E DS
003400930922     D*================================================================
003500950309     D* DS PER LANCIO PGM 'XSRDA8' (DATA>GIORNI) + (INVERSIONE DATA)
003600950308     D G08DS           DS                  INZ
003700950308     D  G08DAT                 1      8  0
003800950308     D  G08INV                 9     16  0
003900950308     D  G08ERR                17     17
004000950308     D  G08TGI                18     22  0
004100950309     D*================================================================
004200950309     D* DS PER LANCIO PGM 'XSRGI8' (GIORNI>DATA)
004300950309     D GI8DS           DS                  INZ
004400950309     D  GI8DAT                 1      8  0
004500950309     D  GI8INV                 9     16  0
004600950309     D  GI8TGI                17     21  0
004700930922     D*================================================================
004800000000     D* DS PER GESTIONE TASTI FUNZIONALI
004900930802     D DSKEYS        E DS
005000930803     D  KEYRIG                 1     40
005100930922     D*================================================================
005200930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300930806     D                SDS
005400930806     D* NOME UTENTE
005500930806     D  USER                 254    263
005600930922     D*================================================================
005700930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D DEVDS1          DS
005900930720     D* CHIAVE FUNZIONALE BATTUTA
006000000000     D  KEY                  369    369
006100930721     D* POSIZIONE CURSORE (IN HEX)
006200930721     D  CURSOR               370    371
006300950308     D***************************************************************
006400950308     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
006500950308     D DS§DAT          DS
006600990803     D  DSANN                  1      4  0
006700950308     D  DSMES                  5      6  0
006800950308     D  DSGIO                  7      8  0
006900950308     D  DSDAT8                 1      8  0
007000930922     D*================================================================
007100000000     D* ARCHITETTURA
007200930922     D KPJBA         E DS
007201040220     C*================================================================
007202040220     D aa              s              2
007203040220     D mm              s              2
007700000000     C*================================================================
007800940826     C* MAIN - PROGRAMMA FILTRO
007900930922     C*================================================================
008000931008     C*
008100000000     C* CICLO FINO A CHE:
008200930922DO  1C     $FINE         DOUEQ     '1'
008300000000     C*
008400931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008500000000     C*
008600930721     C* CONTROLLA GESTIONE VIDEO
008700930922IF  2C     $GES          IFEQ      'V1'
008800930806     C                   EXSR      GESV1                                        *PARAMETRI
008900930922E   2C                   ENDIF
009000000000     C*
009100930922E   1C                   ENDDO
009200000000     C*
009300931018     C* FINE MAIN DEL PGM
009400000000     C                   SETON                                        LR
009500930922     C*================================================================
009600930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
009700930922     C*================================================================
009800930922     C     GESV1         BEGSR
009900930922     C*
010000930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010100930922     C                   EXSR      CARV1                                        *CARICA VIDEO
010200930922E   1C                   ENDIF
010300930922     C*
010400931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010500931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010600931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010700931018E   1C                   ENDIF
010800931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010900930922     C*
011000950717     C                   EXFMT     SB39DV1                                      *PARAMETRI
011100930922     C*
011200930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011300930922     C*
011400930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011500931008SL  2C                   SELECT
011600930922     C     KEY           WHENEQ    ENTER
011700930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011800930922     C     KEY           WHENEQ    F6
011900930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012000930922     C                   OTHER
012100930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012200930922E   2C                   ENDSL
012300930922E   1C                   ENDIF
012400930922     C*
012500930922     C                   ENDSR
012600930713     C*================================================================
012700930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
012800930713     C*================================================================
012900930806     C     CARV1         BEGSR
013000930713     C*
013100930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013200930713     C*
013300930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013400930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013500930713     C*
013600950717     C                   WRITE     SB39DVT                                      *TESTATA
013700950717     C                   WRITE     SB39DVF                                      *FUNZIONI
013800930713     C*
013900930713     C                   ENDSR
014000930804     C*================================================================
014100930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
014200930804     C*================================================================
014300930806     C     ENTV1         BEGSR
014400930806     C*
014500930806     C* CONTROLLA I CAMPI IMMESSI
014600930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
014700930804     C*
014800930804     C                   ENDSR
014900000000     C*================================================================
015000930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015100000000     C*================================================================
015200930126     C     F06GES        BEGSR
015300930923     C*
015400930924     C* CONTROLLA I CAMPI IMMESSI
015500930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015600930923     C*
015700930923     C* NON CI SONO ERRORI
015800930923IF  1C     $ERR          IFEQ      '0'
015900930923     C*
016000950717     C* LANCIO PROGRAMMA DI SALVATAGGIO
016201040223     c                   move      dsann         aa
016202040223     c                   move      dsmes         mm
016203040220     c                   eval      kpjbu = 'SAVTAS' + aa + mm
016300930923     C*
016400950717     C                   MOVEL     'SB39'        KCOAZ
016600930923     C                   CALL      'BCH10'
016700930923     C                   PARM                    KPJBA
016800930923     C*
016900930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
017000930923E   1C                   ENDIF
017100930127     C*
017200000000     C                   ENDSR
017300930205     C*================================================================
017400930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017500930205     C*================================================================
017600930906     C     DECODI        BEGSR
017700930205     C*
017800931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
017900950717     C                   SETOFF                                       6261
018000930712     C*
018100950717     C* CONTROLLO ANNO
018200950717     C     V1CANN        IFEQ      *ZEROS
018300950308     C                   MOVEL     '1'           $ERR
018400950308     C                   MOVEL     MSG(10)       $MSG
018500950308     C                   EXSR      CARMSG
018600950717     C                   SETON                                        61
018700950308     C                   ENDIF
018800930922     C*
018900950717     C* CONTROLLO MESE
019000950717     C     V1CMES        IFGT      12
019100950717     C     V1CMES        ORLT      1
019200950308     C                   MOVEL     '1'           $ERR
019300950308     C                   MOVEL     MSG(11)       $MSG
019400950308     C                   EXSR      CARMSG
019500950717     C                   SETON                                        62
019600950308     C                   ENDIF
019700930923     C*
019800950717     C* CONTROLLO SE DATA MAGGIORE ULTIMO TASP > SALVAT.
019900950717     C     $ERR          IFEQ      '0'
020000950717     C                   Z-ADD     V1CANN        DSANN
020100950717     C                   Z-ADD     V1CMES        DSMES
020200950717     C                   Z-ADD     *ZEROS        DSGIO
020300950717     C     DSDAT8        IFGT      §$3DP0
020400950717     C                   MOVEL     '1'           $ERR
020500950717     C                   MOVEL     MSG(12)       $MSG
020600950717     C                   EXSR      CARMSG
020700950717     C                   SETON                                        6261
020800950717     C                   ENDIF
020900950717     C                   ENDIF
021000950717     C*
021100931008     C                   ENDSR
021200020920     C*--------------------------------------------------------------------------------------------*
021300020920     C* REPERISCE I DATI UTENTE
021400020920     C*--------------------------------------------------------------------------------------------*
021500020920     C     REPDATIUTE    BEGSR
021600020920     C*
021700020920     C* INIZIALIZZA VARIABILI DI WRK
021800020920     C                   CLEAR                   TIBS34DS
021900020920     C                   CLEAR                   AZUTEDS
022000020920     C                   CLEAR                   DDATIUTE
022100020920     C*
022200020920     C     *DTAARA       DEFINE    §azute        azuteds
022300020920     C     *DTAARA       DEFINE    §datiute      ddatiute
022400020920     C                   IN(E)     *DTAARA
022500020920if  1C                   IF        %Error
022600020920     c                   EVAL      I34Tla = 'L'
022700020920     C                   CALL      'TIBS34R'
022800020920     C                   PARM                    Tibs34Ds
022900020920     C                   IN        *DTAARA
023000020920e   1C                   ENDIF
023100020920     C*
023200020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
023300020920     C                   MOVEL(P)  RSUT          DSFIRS           20
023400020920     C*
023500020920     C                   ENDSR
023600930923     C*================================================================
023700931008     C* *INZSR  - ROUTINE INIZIALE
023800000000     C*================================================================
023900930713     C     *INZSR        BEGSR
024000000000     C*
024100000000     C* RICEVIMENTO PARAMETRI
024200000000     C     *ENTRY        PLIST
024300930922     C                   PARM                    KPJBA
024400950308     C* DEFINISCO CAMPI
024500950308     C     *LIKE         DEFINE    TBLKUT        KTBUTE
024600950308     C     *LIKE         DEFINE    TBLCOD        KTBCOD
024700950308     C     *LIKE         DEFINE    TBLKEY        KTBKEY
024800950308     C*
024900950308     C* CHIAVE LETTURA TABEL00F - COMPLETA
025000950308     C     KEYTAB        KLIST
025100950308     C                   KFLD                    KTBUTE
025200950308     C                   KFLD                    KTBCOD
025300950308     C                   KFLD                    KTBKEY
025400930714     C*
025500930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
025600930921     C                   EXSR      INZSTD
025700931008     C*
025800931008     C* IMPOSTO CAMPI TESTATA VIDEO
025900931008     C                   MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
026000950308     C*
026100950717     C* DALLA TAB. $3 RECUPERO L'ULTIMA DATA TASP > SALVAT.
026200950308     C                   Z-ADD     1             KTBUTE
026300950308     C                   MOVEL     '$3'          KTBCOD
026400950308     C                   MOVEL     '1'           KTBKEY
026500950308     C     KEYTAB        CHAIN(N)  TABEL                              99
026600950308     C     *IN99         IFEQ      '0'
026700950308     C                   MOVEL     TBLUNI        DS$3
026800950717     C                   Z-ADD     §$3DP0        DSDAT8                         *DATA ULT. 10 > P
026900950309     C                   ELSE
027000950309     C                   MOVEL     '1'           $ERR
027100950309     C                   MOVEL     MSG(15)       $MSG                           *TAB NON TROVATA
027200950309     C                   EXSR      CARMSG
027300950717     C                   SETON                                        6261
027400950309     C                   ENDIF
027500950717     C                   Z-ADD     DSANN         V1CANN                         *ANNO PROPOSTO
027600950717     C                   Z-ADD     DSMES         V1CMES                         *MESE PROPOSTO
027700950309     C* EMETTO FORMATO VIDEO
027800930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
027900930923     C*
028000000000     C                   ENDSR
028100930924     C*================================================================
028200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
028300930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
028400930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
028500930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
028600930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
028700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
028800930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
028900930924     C*================================================================
029000930805     C*
029100930714     C*================================================================
029200930923     C* INZSTD - ROUTINE INIZIALE STANDARD
029300930714     C*================================================================
029400930803     C     INZSTD        BEGSR
029500930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
029600930802     C                   CALL      'XKEYS'
029700930802     C                   PARM                    DSKEYS
029800930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
029900930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
030000930714     C     XMSGPA        PLIST
030100930714     C                   PARM                    DSMSMS
030200930714     C                   PARM                    DSMSPG
030300930714     C                   PARM                    DSMSKE
030400930714     C                   PARM                    DSMSER
030500930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
030600950717     C                   MOVEL     'TNSB39R'     DSMSPG                         *NOME PGM CHIAMANTE
030700950717     C                   MOVEL     'TNSB39R'     VMCQUE                         *CODA MESSAGGI DESTI
030800930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
030900930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
031000930805     C* INIZIALIZZO FLAG CONTROLLO PGM
031100930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
031200930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
031300930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
031400930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
031500930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
031600930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
031700930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
031800930714     C* LANCIO ROUTINE PER VERIFICARE SE
031900930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
032000930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
032100020920     C                   EXSR      REPDATIUTE
032200930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
032300930721     C                   BITOFF    '01234567'    POSMI1            1
032400930721     C                   BITOFF    '01234567'    POSMI2            1
032500930721     C                   BITOFF    '01234567'    POSMA1            1
032600930721     C                   BITOFF    '01234567'    POSMA2            1
032700930721     C                   BITON     '34'          POSMI1            1            *18
032800930721     C                   BITON     '7'           POSMI2            1            *01
032900930721     C                   BITON     '34'          POSMA1            1            *18
033000930721     C                   BITON     '0'           POSMA2            1            *80
033100930721     C                   MOVE      POSMI2        POSMIN            2
033200930721     C                   MOVEL     POSMI1        POSMIN
033300930721     C                   MOVE      POSMA2        POSMAX            2
033400930721     C                   MOVEL     POSMA1        POSMAX
033500930714     C                   ENDSR
033600930804     C*================================================================
033700930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
033800930804     C*================================================================
033900930804     C     FUNSTD        BEGSR
034000930804     C                   SELECT
034100930804     C     KEY           WHENEQ    F3
034200930804     C                   EXSR      F03STD
034300931115     C     KEY           WHENEQ    F12
034400931115     C                   EXSR      F12STD
034500930804     C     KEY           WHENEQ    F24
034600930804     C                   EXSR      F24STD
034700930804     C                   OTHER
034800930804     C                   MOVEL     MSG(2)        $MSG
034900930804     C                   EXSR      CARMSG
035000930804     C                   ENDSL
035100930804     C                   ENDSR
035200930804     C*================================================================
035300930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
035400930804     C*================================================================
035500930804     C     F03STD        BEGSR
035600930923     C*
035700930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
035800930927     C                   MOVEL     '1'           $FINE
035900930927     C*
036000930927     C                   ENDSR
036100931115     C*================================================================
036200931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
036300931115     C*================================================================
036400931115     C     F12STD        BEGSR
036500931115     C* IMPOSTO FLAG DI FIN PGM
036600931115     C                   MOVEL     '1'           $FINE
036700931115     C                   ENDSR
036800930804     C*================================================================
036900930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
037000930804     C*================================================================
037100930804     C     F24STD        BEGSR
037200930923     C*
037300930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
037400930804     C                   EXSR      OUTFUN
037500930923     C*
037600930804     C                   ENDSR
037700930721     C*================================================================
037800930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
037900930721     C*          CODA MESSAGGI DEL PGM
038000930721     C*================================================================
038100930721     C     CARMSG        BEGSR
038200930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
038300930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
038400930721     C     MSGULT        IFEQ      0
038500930721     C                   MOVEL     '1'           *IN40
038600950717     C                   WRITE     SB39DVK
038700930721     C                   MOVEL     '0'           *IN40
038800930805     C                   ENDIF
038900930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
039000930721     C                   MOVEL     $MSG          DSMSMS
039100930721     C                   CALL      'XMSG'        XMSGPA
039200930721     C     DSMSER        IFEQ      0
039300930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
039400930805     C                   ENDIF
039500930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
039600930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
039700950717     C                   WRITE     SB39DVM                                      *SFL MESS
039800930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
039900930721     C                   MOVEL     *BLANKS       $MSG
040000930721     C                   ENDSR
040100930721     C*================================================================
040200930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
040300930721     C*================================================================
040400930721     C     OUTMSG        BEGSR
040500930721     C* VISUALIZZAZIONE MESSAGGIO
040600930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
040700950717     C                   WRITE     SB39DVK                                      *CONTR MESS
040800930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
040900930721     C                   Z-ADD     *ZEROS        MSGULT
041000930721     C                   ENDSR
041100930803     C*================================================================
041200930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
041300930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
041400930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
041500930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
041600930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
041700930803     C*================================================================
041800930803     C     CARFUN        BEGSR
041900930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
042000930923     C     1             DO        31            I                 5 0          --- 1 -->
042100930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
042200930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
042300930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
042400020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
042500930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
042600930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
042700930803     C                   ELSE                                                   *INTERROG.
042800930803     C                   MOVEL     FUNASI        FAT(I)
042900930803     C                   ENDIF
043000930803     C                   ELSE                                                   <-- 2 --> *FILIALE
043100930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
043200930803     C                   MOVEL     FUNAFG        FAT(I)
043300930803     C                   ELSE                                                   *INTERROG.
043400930922     C                   MOVEL     FUNAFI        FAT(I)
043500930803     C                   ENDIF
043600930803     C                   ENDIF                                                  <-- 2 ---
043700930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
043800930806     C                   MOVEL     USER          WEDP              3            *PROFILO
043900930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
044000930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
044100930806     C                   ELSE
044200930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
044300930806     C                   ENDIF
044400930806     C                   ENDIF
044500930803     C                   ENDDO                                                  <-- 1 ---
044600930803     C                   ENDSR
044700930803     C*================================================================
044800930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
044900930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
045000930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
045100930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
045200930803     C*================================================================
045300930803     C     OUTFUN        BEGSR
045400930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
045500930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
045600930803     C                   ENDDO
045700930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
045800930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
045900930803     C     1             DO        31            I
046000930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
046100930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
046200930803     C                   ADD       1             FUNNAT
046300930803     C                   ENDIF
046400930803     C                   ENDDO
046500930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
046600930803     C     FUNNAT        IFGT      8
046700930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
046800930803     C                   ENDIF
046900930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
047000930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
047100930803     C                   Z-ADD     8             FUNVIS            5 0
047200930803     C     FAT(23)       IFEQ      'S'
047300930803     C                   SUB       1             FUNVIS
047400930803     C                   ENDIF
047500930803     C     FAT(24)       IFEQ      'S'
047600930803     C                   SUB       1             FUNVIS
047700930803     C                   ENDIF
047800930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
047900930923     C                   Z-ADD     *ZEROS        W                 5 0
048000930803     C                   ADD       1             FUNULT
048100930803     C     FUNULT        IFGT      31
048200930803     C                   Z-ADD     1             FUNULT
048300930805     C                   ENDIF
048400930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
048500930803     C     FUNULT        DO        31            I                              --- 1 -->
048600930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
048700930803     C     W             IFLT      FUNVIS                                       --- 2 -->
048800930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
048900930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
049000930803     C     FDE(I)        ANDNE     *BLANKS
049100930805     C     I             IFEQ      23                                           *TOLGO F23
049200930805     C     I             OREQ      24                                           *E F24
049300930803     C                   ELSE
049400930803     C                   ADD       1             W
049500930803     C                   MOVEL     FDE(I)        FUV(W)
049600930803     C                   ENDIF
049700930803     C                   ENDIF                                                  <-- 3 ---
049800930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
049900930803     C                   Z-ADD     I             FUNULT
050000930803     C                   ENDIF                                                  <-- 2 ---
050100930803     C                   ENDDO                                                  <-- 1 ---
050200930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
050300930806     C                   MOVEL     FUV(1)        VFD001
050400930806     C                   MOVEL     FUV(2)        VFD002
050500930806     C                   MOVEL     FUV(3)        VFD003
050600930806     C                   MOVEL     FUV(4)        VFD004
050700930806     C                   MOVEL     FUV(5)        VFD005
050800930806     C                   MOVEL     FUV(6)        VFD006
050900930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
051000930803     C     FAT(24)       ANDEQ     'N'
051100930806     C                   MOVEL     FUV(7)        VFD007
051200930806     C                   MOVEL     FUV(8)        VFD008
051300930803     C                   ELSE                                                   <-- 1 -->
051400930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
051500930803     C     FAT(24)       ANDEQ     'S'
051600930806     C                   MOVEL     FDE(23)       VFD007
051700930806     C                   MOVEL     FDE(24)       VFD008
051800930803     C                   ELSE                                                   <-- 2 -->
051900930806     C                   MOVEL     FUV(7)        VFD007
052000930803     C     FAT(23)       IFEQ      'S'
052100930806     C                   MOVEL     FDE(23)       VFD008
052200930803     C                   ELSE
052300930806     C                   MOVEL     FDE(24)       VFD008
052400930805     C                   ENDIF
052500930805     C                   ENDIF                                                  <-- 2 ---
052600930805     C                   ENDIF                                                  <-- 1 ---
052700930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
052800930803     C                   ENDSR
052900930803     C*================================================================
053000930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
053100930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
053200930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
053300930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
053400930803     C*================================================================
053500930803     C     CONFUN        BEGSR
053600930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
053700930803     C     1             DO        31            I                              --- 1 -->
053800930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
053900930805     C* CONTROLLO SE IL TASTO E' ATTIVO
054000930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
054100930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
054200930803     C                   EXSR      CARMSG
054300930803     C                   MOVEL     '1'           $ERR
054400930803     C                   ENDIF                                                  <-- 3 ---
054500930803     C                   ENDIF                                                  <-- 2 ---
054600930803     C                   ENDDO                                                  <-- 1 ---
054700930803     C                   ENDSR
054800930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
054900950717TNSB39R- Tasto funzionale non valido.                                          1
055000950717TNSB39R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
055100950717TNSB39R- Opzione non valida.                                                   3
055200950717TNSB39R- Opzione abilitata ma non gestita. Avvertire CED.                      4
055300950717TNSB39R- Inizio elenco.                                                        5
055400950717TNSB39R- Fine elenco.                                                          6
055500950717TNSB39R- Data inizio errata.                                                   7
055600950717TNSB39R- Data fine errata.                                                     8
055700950717TNSB39R- 9                                                                     9
055800950717TNSB39R- Anno non accettabile !                                               10
055900950717TNSB39R- Mese non accettabile !                                               11
056000950717TNSB39R- Anno/Mese superiori all'ultimo TASP > SALVAT. !!                     12
056100950717TNSB39R- 13                                                                   13
056200950717TNSB39R- 14                                                                   14
056300950717TNSB39R- Tab. '$3' non trovata. Impossibile continuare.                       15
056400950717TNSB39R- 16                                                                   16
056500950717TNSB39R- 17                                                                   17
056600950717TNSB39R- 18                                                                   18
056700950717TNSB39R- 19                                                                   19
056800950717TNSB39R- 20                                                                   20
056900950717TNSB39R- 21                                                                   21
057000950717TNSB39R- 22                                                                   22
057100930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
057200930806F1    -                   -    NN     NN
057300930723F2    -                   -    NN     NN
057400930921F3    -F3=Fine            -    SS     SS
057500930723F4    -                   -    NN     NN
057600930723F5    -                   -    NN     NN
057700931026F6    -F6=Lancio          -    SS     SS
057800930723F7    -                   -    NN     NN
057900930723F8    -                   -    NN     NN
058000930723F9    -                   -    NN     NN
058100930723F10   -                   -    NN     NN
058200930723F11   -                   -    NN     NN
058300931115F12   -F12=Ritorno        -    SS     SS
058400930723F13   -                   -    NN     NN
058500930723F14   -                   -    NN     NN
058600930723F15   -                   -    NN     NN
058700930723F16   -                   -    NN     NN
058800930723F17   -                   -    NN     NN
058900930723F18   -                   -    NN     NN
059000930723F19   -                   -    NN     NN
059100930723F20   -                   -    NN     NN
059200930804F21   -                   -    NN     NN
059300930927F22   -                   -    NN     NN
059400930723F23   -                   -    NN     NN
059500930803F24   -F24=Altri tasti    -    NN     NN
059600930921ENTER -                   -    SS     SS
059700930802ROLLUP-                   -    NN     NN
059800930802ROLLDN-                   -    NN     NN
059900930802HELP  -                   -    NN     NN
060000930802PRINT -                   -    NN     NN
060100930802BSPACE-                   -    NN     NN
060200930802CLEAR -                   -    NN     NN
