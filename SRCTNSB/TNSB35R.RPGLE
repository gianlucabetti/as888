000100930923     H*===============================================================*
000200931115     H*             - AGGIORNAMENTO SALDI BOLLE-        - FILTRO -    *
000300930923     H*===============================================================*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950308     FTNSB35D   CF   E             WORKSTN INFDS(DEVDS1)
000600950308     F                                     SFILE(SB35DVM:MSGULT)
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
003400950309     D*================================================================
003500950309     D* DS PER LANCIO PGM 'XSRDA8' (DATA>GIORNI) + (INVERSIONE DATA)
003600950309     D G08DS           DS                  INZ
003700950309     D  G08DAT                 1      8  0
003800950309     D  G08INV                 9     16  0
003900950309     D  G08ERR                17     17
004000950309     D  G08TGI                18     22  0
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
006600950308     D  DSSEC                  1      2  0
006700950308     D  DSANN                  3      4  0
006800950308     D  DSMES                  5      6  0
006900950308     D  DSGIO                  7      8  0
007000950308     D  DSAAS                  1      4  0
007100950308     D  DSMGS                  5      8  0
007200950308     D  DSDAT6                 3      8  0
007300950308     D  DSDAT8                 1      8  0
007400930922     D*================================================================
007500000000     D* ARCHITETTURA
007600930922     D KPJBA         E DS
007700930923     D*================================================================
007800931008     D PARAM           DS                  INZ
007900950308     D  PARDTI                 1      8  0                                      DT.INI
008000950308     D  PARDTF                 9     16  0                                      DT.FIN
008100000000     C*================================================================
008200940826     C* MAIN - PROGRAMMA FILTRO
008300930922     C*================================================================
008400931008     C*
008500000000     C* CICLO FINO A CHE:
008600930922DO  1C     $FINE         DOUEQ     '1'
008700000000     C*
008800931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008900000000     C*
009000930721     C* CONTROLLA GESTIONE VIDEO
009100930922IF  2C     $GES          IFEQ      'V1'
009200930806     C                   EXSR      GESV1                                        *PARAMETRI
009300930922E   2C                   ENDIF
009400000000     C*
009500930922E   1C                   ENDDO
009600000000     C*
009700931018     C* FINE MAIN DEL PGM
009800000000     C                   SETON                                        LR
009900930922     C*================================================================
010000930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
010100930922     C*================================================================
010200930922     C     GESV1         BEGSR
010300930922     C*
010400930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010500930922     C                   EXSR      CARV1                                        *CARICA VIDEO
010600930922E   1C                   ENDIF
010700930922     C*
010800931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010900931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011000931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011100931018E   1C                   ENDIF
011200931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011300930922     C*
011400950308     C                   EXFMT     SB35DV1                                      *PARAMETRI
011500930922     C*
011600930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011700930922     C*
011800930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011900931008SL  2C                   SELECT
012000930922     C     KEY           WHENEQ    ENTER
012100930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012200930922     C     KEY           WHENEQ    F6
012300930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012400930922     C                   OTHER
012500930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012600930922E   2C                   ENDSL
012700930922E   1C                   ENDIF
012800930922     C*
012900930922     C                   ENDSR
013000930713     C*================================================================
013100930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
013200930713     C*================================================================
013300930806     C     CARV1         BEGSR
013400930713     C*
013500930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013600930713     C*
013700930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013800930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013900930713     C*
014000950308     C                   WRITE     SB35DVT                                      *TESTATA
014100950308     C                   WRITE     SB35DVF                                      *FUNZIONI
014200930713     C*
014300930713     C                   ENDSR
014400930804     C*================================================================
014500930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
014600930804     C*================================================================
014700930806     C     ENTV1         BEGSR
014800930806     C*
014900930806     C* CONTROLLA I CAMPI IMMESSI
015000930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015100930804     C*
015200930804     C                   ENDSR
015300000000     C*================================================================
015400930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015500000000     C*================================================================
015600930126     C     F06GES        BEGSR
015700930923     C*
015800930924     C* CONTROLLA I CAMPI IMMESSI
015900930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016000930923     C*
016100930923     C* NON CI SONO ERRORI
016200930923IF  1C     $ERR          IFEQ      '0'
016300930923     C*
016400930923     C* LANCIO PROGRAMMA DI STAMPA
016500931115     C                   Z-ADD     W1CDTI        PARDTI
016600931115     C                   Z-ADD     W1CDTF        PARDTF
016700930923     C*
016800950308     C                   MOVEL     'SB36'        KCOAZ
016900930923     C                   MOVEL     PARAM         KPJBU
017000930923     C                   CALL      'BCH10'
017100930923     C                   PARM                    KPJBA
017200930923     C*
017300930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
017400930923E   1C                   ENDIF
017500930127     C*
017600000000     C                   ENDSR
017700930205     C*================================================================
017800930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017900930205     C*================================================================
018000930906     C     DECODI        BEGSR
018100930205     C*
018200931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
018300950308     C                   SETOFF                                       6061
018400930712     C*
018500931115     C* CONTROLLO DATA INIZIO
018600950308     C                   Z-ADD     V1CDTI        G08DAT
018700950308     C                   CALL      'XSRDA8'
018800950308     C                   PARM                    G08DS
018900951017     C                   Z-ADD     G08DAT        V1CDTI
019000950308     C     G08ERR        IFEQ      '1'
019100931115     C                   MOVEL     '1'           $ERR
019200931115     C                   MOVEL     MSG(7)        $MSG
019300931115     C                   EXSR      CARMSG
019400931115     C                   SETON                                        60
019500931115     C                   ELSE
019600950309     C     G08INV        IFGT      DATMAX
019700950308     C                   MOVEL     '1'           $ERR
019800950308     C                   MOVEL     MSG(10)       $MSG
019900950308     C                   EXSR      CARMSG
020000950308     C                   SETON                                        60
020100950308     C                   ELSE
020200950308     C                   Z-ADD     G08INV        W1CDTI            8 0
020300931115     C                   ENDIF
020400950308     C                   ENDIF
020500930922     C*
020600931115     C* CONTROLLO DATA FINE
020700950308     C                   Z-ADD     V1CDTF        G08DAT
020800950308     C                   CALL      'XSRDA8'
020900950308     C                   PARM                    G08DS
021000951017     C                   Z-ADD     G08DAT        V1CDTF
021100950308     C     G08ERR        IFEQ      '1'
021200930922     C                   MOVEL     '1'           $ERR
021300930922     C                   MOVEL     MSG(8)        $MSG
021400930922     C                   EXSR      CARMSG
021500931115     C                   SETON                                        61
021600931115     C                   ELSE
021700950309     C     G08INV        IFGT      DATMAX
021800950308     C                   MOVEL     '1'           $ERR
021900950308     C                   MOVEL     MSG(11)       $MSG
022000950308     C                   EXSR      CARMSG
022100950308     C                   SETON                                        61
022200950308     C                   ELSE
022300950308     C                   Z-ADD     G08INV        W1CDTF            8 0
022400950308     C                   ENDIF
022500931115     C                   ENDIF
022600931008     C*
022700931008     C* ANNO/MESE INIZIALE NON SUPERIORE A QUELLO FINALE
022800931115     C     $ERR          IFEQ      '0'
022900931115     C     W1CDTI        IFGT      W1CDTF
023000931008     C                   MOVEL     '1'           $ERR
023100940905     C                   MOVEL     MSG(9)        $MSG
023200931008     C                   EXSR      CARMSG
023300931115     C                   SETON                                        6061
023400931115     C                   ENDIF
023500931115     C                   ENDIF
023600930923     C*
023700931008     C                   ENDSR
023800020920     C*--------------------------------------------------------------------------------------------*
023900020920     C* REPERISCE I DATI UTENTE
024000020920     C*--------------------------------------------------------------------------------------------*
024100020920     C     REPDATIUTE    BEGSR
024200020920     C*
024300020920     C* INIZIALIZZA VARIABILI DI WRK
024400020920     C                   CLEAR                   TIBS34DS
024500020920     C                   CLEAR                   AZUTEDS
024600020920     C                   CLEAR                   DDATIUTE
024700020920     C*
024800020920     C     *DTAARA       DEFINE    §azute        azuteds
024900020920     C     *DTAARA       DEFINE    §datiute      ddatiute
025000020920     C                   IN(E)     *DTAARA
025100020920if  1C                   IF        %Error
025200020920     c                   EVAL      I34Tla = 'L'
025300020920     C                   CALL      'TIBS34R'
025400020920     C                   PARM                    Tibs34Ds
025500020920     C                   IN        *DTAARA
025600020920e   1C                   ENDIF
025700020920     C*
025800020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
025900020920     C                   MOVEL(P)  RSUT          DSFIRS           20
026000020920     C*
026100020920     C                   ENDSR
026200930923     C*================================================================
026300931008     C* *INZSR  - ROUTINE INIZIALE
026400000000     C*================================================================
026500930713     C     *INZSR        BEGSR
026600000000     C*
026700000000     C* RICEVIMENTO PARAMETRI
026800000000     C     *ENTRY        PLIST
026900930922     C                   PARM                    KPJBA
027000950308     C* DEFINISCO CAMPI
027100950308     C     *LIKE         DEFINE    TBLKUT        KTBUTE
027200950308     C     *LIKE         DEFINE    TBLCOD        KTBCOD
027300950308     C     *LIKE         DEFINE    TBLKEY        KTBKEY
027400950308     C*
027500950308     C* CHIAVE LETTURA TABEL00F - COMPLETA
027600950308     C     KEYTAB        KLIST
027700950308     C                   KFLD                    KTBUTE
027800950308     C                   KFLD                    KTBCOD
027900950308     C                   KFLD                    KTBKEY
028000930714     C*
028100930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
028200930921     C                   EXSR      INZSTD
028300931008     C*
028400931008     C* IMPOSTO CAMPI TESTATA VIDEO
028500931008     C                   MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
028600950308     C*
028700950308     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE
028800950308     C                   Z-ADD     1             KTBUTE
028900950308     C                   MOVEL     '$3'          KTBCOD
029000950308     C                   MOVEL     '1'           KTBKEY
029100950308     C     KEYTAB        CHAIN(N)  TABEL                              99
029200950308     C     *IN99         IFEQ      '0'
029300950308     C                   MOVEL     TBLUNI        DS$3
029400950308     C                   Z-ADD     §$3D00        DSDAT8                         *DATA ULT. 0 > 10
029500980105IF  2C     DSGIO         IFGE      27
029600980105     C                   ADD       1             DSMES
029700980105IF  3C     DSMES         IFEQ      13
029800980105     C                   Z-ADD     1             DSMES
029900980105     C                   ADD       1             DSAAS
030000980105E   3C                   ENDIF
030100980105E   2C                   ENDIF
030200950308     C     DSMES         IFGT      §$3M10
030300950308     C     DSMES         SUB       §$3M10        DSMES
030400950308     C                   ELSE
030500950308     C                   SUB       1             DSAAS
030600950308     C                   ADD       12            DSMES
030700950308     C                   SUB       §$3M10        DSMES
030800950308     C                   ENDIF
030900950309     C                   Z-ADD     DSDAT8        DATMAX            8 0
031000950309     C                   ELSE
031100950309     C                   MOVEL     '1'           $ERR
031200950309     C                   MOVEL     MSG(15)       $MSG                           *TAB NON TROVATA
031300950309     C                   EXSR      CARMSG
031400950309     C                   SETON                                        6061
031500950309     C                   ENDIF
031600930922     C*
031700930924     C* EMETTO FORMATO VIDEO
031800950309     C     DSMES         IFGT      1
031900950309     C                   SUB       1             DSMES
032000950309     C                   ELSE
032100950309     C                   SUB       1             DSAAS
032200950309     C                   Z-ADD     12            DSMES
032300950309     C                   ENDIF
032400950309     C                   Z-ADD     31            DSGIO
032500950309     C     DSMES         IFEQ      2
032600950309     C                   Z-ADD     28            DSGIO
032700950309     C                   ENDIF
032800950309     C     DSMES         IFEQ      4
032900950309     C     DSMES         OREQ      6
033000950309     C     DSMES         OREQ      9
033100950309     C     DSMES         OREQ      11
033200950309     C                   Z-ADD     30            DSGIO
033300950309     C                   ENDIF
033400950309     C                   Z-ADD     DSDAT8        G08INV
033500950309     C                   MOVEL     '3'           G08ERR
033600950309     C                   CALL      'XSRDA8'
033700950309     C                   PARM                    G08DS
033800950309     C                   Z-ADD     G08DAT        V1CDTF                         *DATA FINE PROPOSTA
033900951019     C*=============
034000951017     C* COME DATA INIZIALE IMPOSTA LA DATA FINALE ULTIMO LANCIO 10>P + 1 GIORNO
034100951019     C******               Z-ADD§$3D10    G08INV
034200951019     C******               MOVEL'3'       G08ERR
034300951019     C******               CALL 'XSRDA8'
034400951019     C******               PARM           G08DS
034500951019     C******     G08TGI    ADD  1         GI8TGI
034600951019     C******               CALL 'XSRGI8'
034700951019     C******               PARM           GI8DS
034800951019     C******               Z-ADDGI8DAT    V1CDTI           *DATA INIZIO PROPOSTA
034900951019     C*=============
035000951019     C* COME DATA INIZIALE IMPOSTA 1° GEN. '90
035100951019     C                   Z-ADD     01011990      V1CDTI                         *DATA INIZIO PROPOST
035200930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
035300930923     C*
035400000000     C                   ENDSR
035500930924     C*================================================================
035600930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035800930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
035900930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
036000930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
036100930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036300930924     C*================================================================
036400930805     C*
036500930714     C*================================================================
036600930923     C* INZSTD - ROUTINE INIZIALE STANDARD
036700930714     C*================================================================
036800930803     C     INZSTD        BEGSR
036900930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
037000930802     C                   CALL      'XKEYS'
037100930802     C                   PARM                    DSKEYS
037200930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
037300930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
037400930714     C     XMSGPA        PLIST
037500930714     C                   PARM                    DSMSMS
037600930714     C                   PARM                    DSMSPG
037700930714     C                   PARM                    DSMSKE
037800930714     C                   PARM                    DSMSER
037900930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
038000950308     C                   MOVEL     'TNSB35R'     DSMSPG                         *NOME PGM CHIAMANTE
038100950308     C                   MOVEL     'TNSB35R'     VMCQUE                         *CODA MESSAGGI DESTI
038200930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
038300930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
038400930805     C* INIZIALIZZO FLAG CONTROLLO PGM
038500930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
038600930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
038700930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
038800930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
038900930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
039000930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
039100930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
039200930714     C* LANCIO ROUTINE PER VERIFICARE SE
039300930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
039400930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
039500020920     C                   EXSR      REPDATIUTE
039600930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
039700930721     C                   BITOFF    '01234567'    POSMI1            1
039800930721     C                   BITOFF    '01234567'    POSMI2            1
039900930721     C                   BITOFF    '01234567'    POSMA1            1
040000930721     C                   BITOFF    '01234567'    POSMA2            1
040100930721     C                   BITON     '34'          POSMI1            1            *18
040200930721     C                   BITON     '7'           POSMI2            1            *01
040300930721     C                   BITON     '34'          POSMA1            1            *18
040400930721     C                   BITON     '0'           POSMA2            1            *80
040500930721     C                   MOVE      POSMI2        POSMIN            2
040600930721     C                   MOVEL     POSMI1        POSMIN
040700930721     C                   MOVE      POSMA2        POSMAX            2
040800930721     C                   MOVEL     POSMA1        POSMAX
040900930714     C                   ENDSR
041000930804     C*================================================================
041100930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
041200930804     C*================================================================
041300930804     C     FUNSTD        BEGSR
041400930804     C                   SELECT
041500930804     C     KEY           WHENEQ    F3
041600930804     C                   EXSR      F03STD
041700931115     C     KEY           WHENEQ    F12
041800931115     C                   EXSR      F12STD
041900930804     C     KEY           WHENEQ    F24
042000930804     C                   EXSR      F24STD
042100930804     C                   OTHER
042200930804     C                   MOVEL     MSG(2)        $MSG
042300930804     C                   EXSR      CARMSG
042400930804     C                   ENDSL
042500930804     C                   ENDSR
042600930804     C*================================================================
042700930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
042800930804     C*================================================================
042900930804     C     F03STD        BEGSR
043000930923     C*
043100930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
043200930927     C                   MOVEL     '1'           $FINE
043300930927     C*
043400930927     C                   ENDSR
043500931115     C*================================================================
043600931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
043700931115     C*================================================================
043800931115     C     F12STD        BEGSR
043900931115     C* IMPOSTO FLAG DI FIN PGM
044000931115     C                   MOVEL     '1'           $FINE
044100931115     C                   ENDSR
044200930804     C*================================================================
044300930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
044400930804     C*================================================================
044500930804     C     F24STD        BEGSR
044600930923     C*
044700930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
044800930804     C                   EXSR      OUTFUN
044900930923     C*
045000930804     C                   ENDSR
045100930721     C*================================================================
045200930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
045300930721     C*          CODA MESSAGGI DEL PGM
045400930721     C*================================================================
045500930721     C     CARMSG        BEGSR
045600930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
045700930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
045800930721     C     MSGULT        IFEQ      0
045900930721     C                   MOVEL     '1'           *IN40
046000950308     C                   WRITE     SB35DVK
046100930721     C                   MOVEL     '0'           *IN40
046200930805     C                   ENDIF
046300930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
046400930721     C                   MOVEL     $MSG          DSMSMS
046500930721     C                   CALL      'XMSG'        XMSGPA
046600930721     C     DSMSER        IFEQ      0
046700930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
046800930805     C                   ENDIF
046900930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
047000930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
047100950308     C                   WRITE     SB35DVM                                      *SFL MESS
047200930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
047300930721     C                   MOVEL     *BLANKS       $MSG
047400930721     C                   ENDSR
047500930721     C*================================================================
047600930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
047700930721     C*================================================================
047800930721     C     OUTMSG        BEGSR
047900930721     C* VISUALIZZAZIONE MESSAGGIO
048000930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
048100950308     C                   WRITE     SB35DVK                                      *CONTR MESS
048200930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
048300930721     C                   Z-ADD     *ZEROS        MSGULT
048400930721     C                   ENDSR
048500930803     C*================================================================
048600930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
048700930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
048800930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
048900930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
049000930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
049100930803     C*================================================================
049200930803     C     CARFUN        BEGSR
049300930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
049400930923     C     1             DO        31            I                 5 0          --- 1 -->
049500930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
049600930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
049700930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
049800020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
049900930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
050000930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
050100930803     C                   ELSE                                                   *INTERROG.
050200930803     C                   MOVEL     FUNASI        FAT(I)
050300930803     C                   ENDIF
050400930803     C                   ELSE                                                   <-- 2 --> *FILIALE
050500930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
050600930803     C                   MOVEL     FUNAFG        FAT(I)
050700930803     C                   ELSE                                                   *INTERROG.
050800930922     C                   MOVEL     FUNAFI        FAT(I)
050900930803     C                   ENDIF
051000930803     C                   ENDIF                                                  <-- 2 ---
051100930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
051200930806     C                   MOVEL     USER          WEDP              3            *PROFILO
051300930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
051400930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
051500930806     C                   ELSE
051600930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
051700930806     C                   ENDIF
051800930806     C                   ENDIF
051900930803     C                   ENDDO                                                  <-- 1 ---
052000930803     C                   ENDSR
052100930803     C*================================================================
052200930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
052300930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
052400930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
052500930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
052600930803     C*================================================================
052700930803     C     OUTFUN        BEGSR
052800930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
052900930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
053000930803     C                   ENDDO
053100930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
053200930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
053300930803     C     1             DO        31            I
053400930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
053500930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
053600930803     C                   ADD       1             FUNNAT
053700930803     C                   ENDIF
053800930803     C                   ENDDO
053900930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
054000930803     C     FUNNAT        IFGT      8
054100930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
054200930803     C                   ENDIF
054300930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
054400930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
054500930803     C                   Z-ADD     8             FUNVIS            5 0
054600930803     C     FAT(23)       IFEQ      'S'
054700930803     C                   SUB       1             FUNVIS
054800930803     C                   ENDIF
054900930803     C     FAT(24)       IFEQ      'S'
055000930803     C                   SUB       1             FUNVIS
055100930803     C                   ENDIF
055200930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
055300930923     C                   Z-ADD     *ZEROS        W                 5 0
055400930803     C                   ADD       1             FUNULT
055500930803     C     FUNULT        IFGT      31
055600930803     C                   Z-ADD     1             FUNULT
055700930805     C                   ENDIF
055800930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
055900930803     C     FUNULT        DO        31            I                              --- 1 -->
056000930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
056100930803     C     W             IFLT      FUNVIS                                       --- 2 -->
056200930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
056300930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
056400930803     C     FDE(I)        ANDNE     *BLANKS
056500930805     C     I             IFEQ      23                                           *TOLGO F23
056600930805     C     I             OREQ      24                                           *E F24
056700930803     C                   ELSE
056800930803     C                   ADD       1             W
056900930803     C                   MOVEL     FDE(I)        FUV(W)
057000930803     C                   ENDIF
057100930803     C                   ENDIF                                                  <-- 3 ---
057200930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
057300930803     C                   Z-ADD     I             FUNULT
057400930803     C                   ENDIF                                                  <-- 2 ---
057500930803     C                   ENDDO                                                  <-- 1 ---
057600930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
057700930806     C                   MOVEL     FUV(1)        VFD001
057800930806     C                   MOVEL     FUV(2)        VFD002
057900930806     C                   MOVEL     FUV(3)        VFD003
058000930806     C                   MOVEL     FUV(4)        VFD004
058100930806     C                   MOVEL     FUV(5)        VFD005
058200930806     C                   MOVEL     FUV(6)        VFD006
058300930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
058400930803     C     FAT(24)       ANDEQ     'N'
058500930806     C                   MOVEL     FUV(7)        VFD007
058600930806     C                   MOVEL     FUV(8)        VFD008
058700930803     C                   ELSE                                                   <-- 1 -->
058800930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
058900930803     C     FAT(24)       ANDEQ     'S'
059000930806     C                   MOVEL     FDE(23)       VFD007
059100930806     C                   MOVEL     FDE(24)       VFD008
059200930803     C                   ELSE                                                   <-- 2 -->
059300930806     C                   MOVEL     FUV(7)        VFD007
059400930803     C     FAT(23)       IFEQ      'S'
059500930806     C                   MOVEL     FDE(23)       VFD008
059600930803     C                   ELSE
059700930806     C                   MOVEL     FDE(24)       VFD008
059800930805     C                   ENDIF
059900930805     C                   ENDIF                                                  <-- 2 ---
060000930805     C                   ENDIF                                                  <-- 1 ---
060100930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
060200930803     C                   ENDSR
060300930803     C*================================================================
060400930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
060500930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
060600930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
060700930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
060800930803     C*================================================================
060900930803     C     CONFUN        BEGSR
061000930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
061100930803     C     1             DO        31            I                              --- 1 -->
061200930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
061300930805     C* CONTROLLO SE IL TASTO E' ATTIVO
061400930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
061500930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
061600930803     C                   EXSR      CARMSG
061700930803     C                   MOVEL     '1'           $ERR
061800930803     C                   ENDIF                                                  <-- 3 ---
061900930803     C                   ENDIF                                                  <-- 2 ---
062000930803     C                   ENDDO                                                  <-- 1 ---
062100930803     C                   ENDSR
062200930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062300950308TNSB35R- Tasto funzionale non valido.                                          1
062400950308TNSB35R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
062500950308TNSB35R- Opzione non valida.                                                   3
062600950308TNSB35R- Opzione abilitata ma non gestita. Avvertire CED.                      4
062700950308TNSB35R- Inizio elenco.                                                        5
062800950308TNSB35R- Fine elenco.                                                          6
062900950308TNSB35R- Data inizio errata.                                                   7
063000950308TNSB35R- Data fine errata.                                                     8
063100950308TNSB35R- Data inizio superiore alla data fine.                                 9
063200950308TNSB35R- Data inizio maggiore della data piu alta consentita.                 10
063300950308TNSB35R- Data fine maggiore della data piu alta consentita.                   11
063400950308TNSB35R- Codice filiale errato                                                12
063500950308TNSB35R- Codice area errato                                                   13
063600950308TNSB35R- Codice divisione errato                                              14
063700950309TNSB35R- Tab. '$3' non trovata. Impossibile continuare.                       15
063800950308TNSB35R- 16                                                                   16
063900950308TNSB35R- 17                                                                   17
064000950308TNSB35R- 18                                                                   18
064100950308TNSB35R- Immettere solo una selezione per filiale, area o divisione           19
064200950308TNSB35R- 20                                                                   20
064300950308TNSB35R- 21                                                                   21
064400950308TNSB35R- 22                                                                   22
064500930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
064600930806F1    -                   -    NN     NN
064700930723F2    -                   -    NN     NN
064800930921F3    -F3=Fine            -    SS     SS
064900930723F4    -                   -    NN     NN
065000930723F5    -                   -    NN     NN
065100931026F6    -F6=Lancio          -    SS     SS
065200930723F7    -                   -    NN     NN
065300930723F8    -                   -    NN     NN
065400930723F9    -                   -    NN     NN
065500930723F10   -                   -    NN     NN
065600930723F11   -                   -    NN     NN
065700931115F12   -F12=Ritorno        -    SS     SS
065800930723F13   -                   -    NN     NN
065900930723F14   -                   -    NN     NN
066000930723F15   -                   -    NN     NN
066100930723F16   -                   -    NN     NN
066200930723F17   -                   -    NN     NN
066300930723F18   -                   -    NN     NN
066400930723F19   -                   -    NN     NN
066500930723F20   -                   -    NN     NN
066600930804F21   -                   -    NN     NN
066700930927F22   -                   -    NN     NN
066800930723F23   -                   -    NN     NN
066900930803F24   -F24=Altri tasti    -    NN     NN
067000930921ENTER -                   -    SS     SS
067100930802ROLLUP-                   -    NN     NN
067200930802ROLLDN-                   -    NN     NN
067300930802HELP  -                   -    NN     NN
067400930802PRINT -                   -    NN     NN
067500930802BSPACE-                   -    NN     NN
067600930802CLEAR -                   -    NN     NN
