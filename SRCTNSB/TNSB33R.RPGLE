000100930923     H*===============================================================*
000200110331     H*  controllo bolle non trasferte dda TITASP       - FILTRO -    *
000300930923     H*===============================================================*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500110331     FTNSB33D   CF   E             WORKSTN INFDS(DEVDS1)
000600110331     F                                     SFILE(SB33DVM:MSGULT)
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
007400110331     d
007500110331     d dtaiso          s               d
007600110411     d dtasys          s               d   inz(*sys) datfmt(*iso)
007700110331     d dtaeur          s               d   datfmt(*eur)
007800930922     D*================================================================
007900000000     D* ARCHITETTURA
008000930922     D KPJBA         E DS
008100930923     D*================================================================
008200931008     D PARAM           DS                  INZ
008300950308     D  PARDTI                 1      8  0                                      DT.INI
008400950308     D  PARDTF                 9     16  0                                      DT.FIN
008500110411     d* S=Escludi in stampa le C.A.più recenti di
008600110331     D  v1cca                 17     17
008700110411     d* anno c.A. da cui non stampare
008800110411     D  paranca               18     21  0
008900000000     C*================================================================
009000940826     C* MAIN - PROGRAMMA FILTRO
009100930922     C*================================================================
009200931008     C*
009300000000     C* CICLO FINO A CHE:
009400930922DO  1C     $FINE         DOUEQ     '1'
009500000000     C*
009600931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009700000000     C*
009800930721     C* CONTROLLA GESTIONE VIDEO
009900930922IF  2C     $GES          IFEQ      'V1'
010000930806     C                   EXSR      GESV1                                        *PARAMETRI
010100930922E   2C                   ENDIF
010200000000     C*
010300930922E   1C                   ENDDO
010400000000     C*
010500931018     C* FINE MAIN DEL PGM
010600000000     C                   SETON                                        LR
010700930922     C*================================================================
010800930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
010900930922     C*================================================================
011000930922     C     GESV1         BEGSR
011100930922     C*
011200930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011300930922     C                   EXSR      CARV1                                        *CARICA VIDEO
011400930922E   1C                   ENDIF
011500930922     C*
011600931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011700931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011800931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011900931018E   1C                   ENDIF
012000931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012100930922     C*
012200110331     C                   EXFMT     SB33DV1                                      *PARAMETRI
012300930922     C*
012400930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012500930922     C*
012600930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012700931008SL  2C                   SELECT
012800930922     C     KEY           WHENEQ    ENTER
012900930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013000930922     C     KEY           WHENEQ    F6
013100930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013200930922     C                   OTHER
013300930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013400930922E   2C                   ENDSL
013500930922E   1C                   ENDIF
013600930922     C*
013700930922     C                   ENDSR
013800930713     C*================================================================
013900930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
014000930713     C*================================================================
014100930806     C     CARV1         BEGSR
014200930713     C*
014300930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014400930713     C*
014500930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014600930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014700930713     C*
014800110331     C                   WRITE     SB33DVT                                      *TESTATA
014900110331     C                   WRITE     SB33DVF                                      *FUNZIONI
015000930713     C*
015100930713     C                   ENDSR
015200930804     C*================================================================
015300930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
015400930804     C*================================================================
015500930806     C     ENTV1         BEGSR
015600930806     C*
015700930806     C* CONTROLLA I CAMPI IMMESSI
015800930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015900930804     C*
016000930804     C                   ENDSR
016100000000     C*================================================================
016200930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016300000000     C*================================================================
016400930126     C     F06GES        BEGSR
016500930923     C*
016600930924     C* CONTROLLA I CAMPI IMMESSI
016700930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016800930923     C*
016900930923     C* NON CI SONO ERRORI
017000930923IF  1C     $ERR          IFEQ      '0'
017100930923     C*
017200930923     C* LANCIO PROGRAMMA DI STAMPA
017300931115     C                   Z-ADD     W1CDTI        PARDTI
017400931115     C                   Z-ADD     W1CDTF        PARDTF
017500110411     c                   move      v1ddca        paranca
017600930923     C*
017700110331     C                   MOVEL     'SB32'        KCOAZ
017800930923     C                   MOVEL     PARAM         KPJBU
017900930923     C                   CALL      'BCH10'
018000930923     C                   PARM                    KPJBA
018100930923     C*
018200930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
018300930923E   1C                   ENDIF
018400930127     C*
018500000000     C                   ENDSR
018600930205     C*================================================================
018700930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018800930205     C*================================================================
018900930906     C     DECODI        BEGSR
019000930205     C*
019100931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
019200950308     C                   SETOFF                                       6061
019300930712     C*
019400931115     C* CONTROLLO DATA INIZIO
019500950308     C                   Z-ADD     V1CDTI        G08DAT
019600950308     C                   CALL      'XSRDA8'
019700950308     C                   PARM                    G08DS
019800951017     C                   Z-ADD     G08DAT        V1CDTI
019900950308     C     G08ERR        IFEQ      '1'
020000931115     C                   MOVEL     '1'           $ERR
020100931115     C                   MOVEL     MSG(7)        $MSG
020200931115     C                   EXSR      CARMSG
020300931115     C                   SETON                                        60
020400931115     C                   ELSE
020500950309     C     G08INV        IFGT      DATMAX
020600950308     C                   MOVEL     '1'           $ERR
020700950308     C                   MOVEL     MSG(10)       $MSG
020800950308     C                   EXSR      CARMSG
020900950308     C                   SETON                                        60
021000950308     C                   ELSE
021100950308     C                   Z-ADD     G08INV        W1CDTI            8 0
021200931115     C                   ENDIF
021300950308     C                   ENDIF
021400930922     C*
021500931115     C* CONTROLLO DATA FINE
021600950308     C                   Z-ADD     V1CDTF        G08DAT
021700950308     C                   CALL      'XSRDA8'
021800950308     C                   PARM                    G08DS
021900951017     C                   Z-ADD     G08DAT        V1CDTF
022000950308     C     G08ERR        IFEQ      '1'
022100930922     C                   MOVEL     '1'           $ERR
022200930922     C                   MOVEL     MSG(8)        $MSG
022300930922     C                   EXSR      CARMSG
022400931115     C                   SETON                                        61
022500931115     C                   ELSE
022600950309     C     G08INV        IFGT      DATMAX
022700950308     C                   MOVEL     '1'           $ERR
022800950308     C                   MOVEL     MSG(11)       $MSG
022900950308     C                   EXSR      CARMSG
023000950308     C                   SETON                                        61
023100950308     C                   ELSE
023200950308     C                   Z-ADD     G08INV        W1CDTF            8 0
023300950308     C                   ENDIF
023400931115     C                   ENDIF
023500931008     C*
023600931008     C* ANNO/MESE INIZIALE NON SUPERIORE A QUELLO FINALE
023700931115     C     $ERR          IFEQ      '0'
023800931115     C     W1CDTI        IFGT      W1CDTF
023900931008     C                   MOVEL     '1'           $ERR
024000940905     C                   MOVEL     MSG(9)        $MSG
024100931008     C                   EXSR      CARMSG
024200931115     C                   SETON                                        6061
024300931115     C                   ENDIF
024400931115     C                   ENDIF
024500930923     C*
024600931008     C                   ENDSR
024700020920     C*--------------------------------------------------------------------------------------------*
024800020920     C* REPERISCE I DATI UTENTE
024900020920     C*--------------------------------------------------------------------------------------------*
025000020920     C     REPDATIUTE    BEGSR
025100020920     C*
025200020920     C* INIZIALIZZA VARIABILI DI WRK
025300020920     C                   CLEAR                   TIBS34DS
025400020920     C                   CLEAR                   AZUTEDS
025500020920     C                   CLEAR                   DDATIUTE
025600020920     C*
025700020920     C     *DTAARA       DEFINE    §azute        azuteds
025800020920     C     *DTAARA       DEFINE    §datiute      ddatiute
025900020920     C                   IN(E)     *DTAARA
026000020920if  1C                   IF        %Error
026100020920     c                   EVAL      I34Tla = 'L'
026200020920     C                   CALL      'TIBS34R'
026300020920     C                   PARM                    Tibs34Ds
026400020920     C                   IN        *DTAARA
026500020920e   1C                   ENDIF
026600020920     C*
026700020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
026800020920     C                   MOVEL(P)  RSUT          DSFIRS           20
026900020920     C*
027000020920     C                   ENDSR
027100930923     C*================================================================
027200931008     C* *INZSR  - ROUTINE INIZIALE
027300000000     C*================================================================
027400930713     C     *INZSR        BEGSR
027500000000     C*
027600000000     C* RICEVIMENTO PARAMETRI
027700000000     C     *ENTRY        PLIST
027800930922     C                   PARM                    KPJBA
027900950308     C* DEFINISCO CAMPI
028000950308     C     *LIKE         DEFINE    TBLKUT        KTBUTE
028100950308     C     *LIKE         DEFINE    TBLCOD        KTBCOD
028200950308     C     *LIKE         DEFINE    TBLKEY        KTBKEY
028300950308     C*
028400950308     C* CHIAVE LETTURA TABEL00F - COMPLETA
028500950308     C     KEYTAB        KLIST
028600950308     C                   KFLD                    KTBUTE
028700950308     C                   KFLD                    KTBCOD
028800950308     C                   KFLD                    KTBKEY
028900930714     C*
029000930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
029100930921     C                   EXSR      INZSTD
029200931008     C*
029300931008     C* IMPOSTO CAMPI TESTATA VIDEO
029400931008     C                   MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
029500951019     C*=============
029600110331     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE
029700110331     C                   Z-ADD     1             KTBUTE
029800110331     C                   MOVEL     '$3'          KTBCOD
029900110331     C                   MOVEL     '1'           KTBKEY
030000110331     C     KEYTAB        CHAIN(N)  TABEL                              99
030100110331     C     *IN99         IFEQ      '0'
030200110331     C                   MOVEL     TBLUNI        DS$3
030300110331     c                   move      §$3d10        dtaiso
030400110331     c                   subdur    §$3mp0:*M     dtaiso
030500110331     c                   move      dtaiso        datmax            8 0
030600110331     C                   ELSE
030700110331     C                   MOVEL     '1'           $ERR
030800110331     C                   MOVEL     MSG(15)       $MSG                           *TAB NON TROVATA
030900110331     C                   EXSR      CARMSG
031000110331     C                   SETON                                        606162
031100110331     C                   ENDIF
031200110331     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO P > SAV
031300110331     C* MENO I MESI PER I QUALI LA BOLLA DEVE RIMANRE IN TNTASP
031400110331     c                   subdur    1:*m          dtaiso
031500110331     c                   move      dtaiso        dtaeur
031600110331     c                   move      dtaeur        v1cdtf
031700110331     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE X LE BOLLE POSTE
031800110331     c                   move      §$3d00        dtaiso
031900110331     c                   subdur    §$3mpp:*M     dtaiso
032000110331     c                   move      dtaiso        dptmax            8 0
032100110331     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO POSTE > SAV
032200110331     C* MENO I MESI PER I QUALI LA BOLLA POSTE DEVE RIMANRE IN TNTASP
032300110331     c                   subdur    1:*m          dtaiso
032400110331     c                   move      dtaiso        dtaeur
032500110331     c                   move      dtaeur        v1cdtf
032600110331
032700951019     C* COME DATA INIZIALE IMPOSTA 1° GEN. '90
032800951019     C                   Z-ADD     01011990      V1CDTI                         *DATA INIZIO PROPOST
032900110331     c                   eval      v1cca='SS'
033000110411     c* cerco udate - 6 anni
033100110411     c                   eval      dtaiso=dtasys
033200110411     c                   subdur    5:*y          dtaiso
033300110411     c                   eval      dtaeur=dtaiso
033400110411     c                   movel     dtaeur        v1ddca
033500110411     c                   movel     0101          v1ddca
033600110331     c
033700930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
033800930923     C*
033900000000     C                   ENDSR
034000930924     C*================================================================
034100930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034300930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
034400930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
034500930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
034600930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034800930924     C*================================================================
034900930805     C*
035000930714     C*================================================================
035100930923     C* INZSTD - ROUTINE INIZIALE STANDARD
035200930714     C*================================================================
035300930803     C     INZSTD        BEGSR
035400930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035500930802     C                   CALL      'XKEYS'
035600930802     C                   PARM                    DSKEYS
035700930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
035800930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
035900930714     C     XMSGPA        PLIST
036000930714     C                   PARM                    DSMSMS
036100930714     C                   PARM                    DSMSPG
036200930714     C                   PARM                    DSMSKE
036300930714     C                   PARM                    DSMSER
036400930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
036500110331     C                   MOVEL     'TNSB33R'     DSMSPG                         *NOME PGM CHIAMANTE
036600110331     C                   MOVEL     'TNSB33R'     VMCQUE                         *CODA MESSAGGI DESTI
036700930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
036800930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
036900930805     C* INIZIALIZZO FLAG CONTROLLO PGM
037000930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
037100930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
037200930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
037300930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
037400930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
037500930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
037600930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
037700930714     C* LANCIO ROUTINE PER VERIFICARE SE
037800930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
037900930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
038000020920     C                   EXSR      REPDATIUTE
038100930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
038200930721     C                   BITOFF    '01234567'    POSMI1            1
038300930721     C                   BITOFF    '01234567'    POSMI2            1
038400930721     C                   BITOFF    '01234567'    POSMA1            1
038500930721     C                   BITOFF    '01234567'    POSMA2            1
038600930721     C                   BITON     '34'          POSMI1            1            *18
038700930721     C                   BITON     '7'           POSMI2            1            *01
038800930721     C                   BITON     '34'          POSMA1            1            *18
038900930721     C                   BITON     '0'           POSMA2            1            *80
039000930721     C                   MOVE      POSMI2        POSMIN            2
039100930721     C                   MOVEL     POSMI1        POSMIN
039200930721     C                   MOVE      POSMA2        POSMAX            2
039300930721     C                   MOVEL     POSMA1        POSMAX
039400930714     C                   ENDSR
039500930804     C*================================================================
039600930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
039700930804     C*================================================================
039800930804     C     FUNSTD        BEGSR
039900930804     C                   SELECT
040000930804     C     KEY           WHENEQ    F3
040100930804     C                   EXSR      F03STD
040200931115     C     KEY           WHENEQ    F12
040300931115     C                   EXSR      F12STD
040400930804     C     KEY           WHENEQ    F24
040500930804     C                   EXSR      F24STD
040600930804     C                   OTHER
040700930804     C                   MOVEL     MSG(2)        $MSG
040800930804     C                   EXSR      CARMSG
040900930804     C                   ENDSL
041000930804     C                   ENDSR
041100930804     C*================================================================
041200930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
041300930804     C*================================================================
041400930804     C     F03STD        BEGSR
041500930923     C*
041600930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
041700930927     C                   MOVEL     '1'           $FINE
041800930927     C*
041900930927     C                   ENDSR
042000931115     C*================================================================
042100931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
042200931115     C*================================================================
042300931115     C     F12STD        BEGSR
042400931115     C* IMPOSTO FLAG DI FIN PGM
042500931115     C                   MOVEL     '1'           $FINE
042600931115     C                   ENDSR
042700930804     C*================================================================
042800930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
042900930804     C*================================================================
043000930804     C     F24STD        BEGSR
043100930923     C*
043200930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
043300930804     C                   EXSR      OUTFUN
043400930923     C*
043500930804     C                   ENDSR
043600930721     C*================================================================
043700930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
043800930721     C*          CODA MESSAGGI DEL PGM
043900930721     C*================================================================
044000930721     C     CARMSG        BEGSR
044100930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
044200930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
044300930721     C     MSGULT        IFEQ      0
044400930721     C                   MOVEL     '1'           *IN40
044500110331     C                   WRITE     SB33DVK
044600930721     C                   MOVEL     '0'           *IN40
044700930805     C                   ENDIF
044800930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
044900930721     C                   MOVEL     $MSG          DSMSMS
045000930721     C                   CALL      'XMSG'        XMSGPA
045100930721     C     DSMSER        IFEQ      0
045200930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
045300930805     C                   ENDIF
045400930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
045500930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
045600110331     C                   WRITE     SB33DVM                                      *SFL MESS
045700930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
045800930721     C                   MOVEL     *BLANKS       $MSG
045900930721     C                   ENDSR
046000930721     C*================================================================
046100930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
046200930721     C*================================================================
046300930721     C     OUTMSG        BEGSR
046400930721     C* VISUALIZZAZIONE MESSAGGIO
046500930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
046600110331     C                   WRITE     SB33DVK                                      *CONTR MESS
046700930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
046800930721     C                   Z-ADD     *ZEROS        MSGULT
046900930721     C                   ENDSR
047000930803     C*================================================================
047100930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
047200930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
047300930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
047400930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
047500930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
047600930803     C*================================================================
047700930803     C     CARFUN        BEGSR
047800930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
047900930923     C     1             DO        31            I                 5 0          --- 1 -->
048000930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
048100930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
048200930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
048300020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
048400930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
048500930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
048600930803     C                   ELSE                                                   *INTERROG.
048700930803     C                   MOVEL     FUNASI        FAT(I)
048800930803     C                   ENDIF
048900930803     C                   ELSE                                                   <-- 2 --> *FILIALE
049000930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
049100930803     C                   MOVEL     FUNAFG        FAT(I)
049200930803     C                   ELSE                                                   *INTERROG.
049300930922     C                   MOVEL     FUNAFI        FAT(I)
049400930803     C                   ENDIF
049500930803     C                   ENDIF                                                  <-- 2 ---
049600930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
049700930806     C                   MOVEL     USER          WEDP              3            *PROFILO
049800930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
049900930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
050000930806     C                   ELSE
050100930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
050200930806     C                   ENDIF
050300930806     C                   ENDIF
050400930803     C                   ENDDO                                                  <-- 1 ---
050500930803     C                   ENDSR
050600930803     C*================================================================
050700930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
050800930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
050900930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
051000930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
051100930803     C*================================================================
051200930803     C     OUTFUN        BEGSR
051300930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
051400930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
051500930803     C                   ENDDO
051600930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
051700930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
051800930803     C     1             DO        31            I
051900930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
052000930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
052100930803     C                   ADD       1             FUNNAT
052200930803     C                   ENDIF
052300930803     C                   ENDDO
052400930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
052500930803     C     FUNNAT        IFGT      8
052600930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
052700930803     C                   ENDIF
052800930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
052900930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
053000930803     C                   Z-ADD     8             FUNVIS            5 0
053100930803     C     FAT(23)       IFEQ      'S'
053200930803     C                   SUB       1             FUNVIS
053300930803     C                   ENDIF
053400930803     C     FAT(24)       IFEQ      'S'
053500930803     C                   SUB       1             FUNVIS
053600930803     C                   ENDIF
053700930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
053800930923     C                   Z-ADD     *ZEROS        W                 5 0
053900930803     C                   ADD       1             FUNULT
054000930803     C     FUNULT        IFGT      31
054100930803     C                   Z-ADD     1             FUNULT
054200930805     C                   ENDIF
054300930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
054400930803     C     FUNULT        DO        31            I                              --- 1 -->
054500930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
054600930803     C     W             IFLT      FUNVIS                                       --- 2 -->
054700930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
054800930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
054900930803     C     FDE(I)        ANDNE     *BLANKS
055000930805     C     I             IFEQ      23                                           *TOLGO F23
055100930805     C     I             OREQ      24                                           *E F24
055200930803     C                   ELSE
055300930803     C                   ADD       1             W
055400930803     C                   MOVEL     FDE(I)        FUV(W)
055500930803     C                   ENDIF
055600930803     C                   ENDIF                                                  <-- 3 ---
055700930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
055800930803     C                   Z-ADD     I             FUNULT
055900930803     C                   ENDIF                                                  <-- 2 ---
056000930803     C                   ENDDO                                                  <-- 1 ---
056100930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
056200930806     C                   MOVEL     FUV(1)        VFD001
056300930806     C                   MOVEL     FUV(2)        VFD002
056400930806     C                   MOVEL     FUV(3)        VFD003
056500930806     C                   MOVEL     FUV(4)        VFD004
056600930806     C                   MOVEL     FUV(5)        VFD005
056700930806     C                   MOVEL     FUV(6)        VFD006
056800930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
056900930803     C     FAT(24)       ANDEQ     'N'
057000930806     C                   MOVEL     FUV(7)        VFD007
057100930806     C                   MOVEL     FUV(8)        VFD008
057200930803     C                   ELSE                                                   <-- 1 -->
057300930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
057400930803     C     FAT(24)       ANDEQ     'S'
057500930806     C                   MOVEL     FDE(23)       VFD007
057600930806     C                   MOVEL     FDE(24)       VFD008
057700930803     C                   ELSE                                                   <-- 2 -->
057800930806     C                   MOVEL     FUV(7)        VFD007
057900930803     C     FAT(23)       IFEQ      'S'
058000930806     C                   MOVEL     FDE(23)       VFD008
058100930803     C                   ELSE
058200930806     C                   MOVEL     FDE(24)       VFD008
058300930805     C                   ENDIF
058400930805     C                   ENDIF                                                  <-- 2 ---
058500930805     C                   ENDIF                                                  <-- 1 ---
058600930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
058700930803     C                   ENDSR
058800930803     C*================================================================
058900930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
059000930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
059100930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
059200930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
059300930803     C*================================================================
059400930803     C     CONFUN        BEGSR
059500930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
059600930803     C     1             DO        31            I                              --- 1 -->
059700930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
059800930805     C* CONTROLLO SE IL TASTO E' ATTIVO
059900930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
060000930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
060100930803     C                   EXSR      CARMSG
060200930803     C                   MOVEL     '1'           $ERR
060300930803     C                   ENDIF                                                  <-- 3 ---
060400930803     C                   ENDIF                                                  <-- 2 ---
060500930803     C                   ENDDO                                                  <-- 1 ---
060600930803     C                   ENDSR
060700930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
060800110331TNSB33R- Tasto funzionale non valido.                                          1
060900110331TNSB33R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
061000110331TNSB33R- Opzione non valida.                                                   3
061100110331TNSB33R- Opzione abilitata ma non gestita. Avvertire CED.                      4
061200110331TNSB33R- Inizio elenco.                                                        5
061300110331TNSB33R- Fine elenco.                                                          6
061400110331TNSB33R- Data inizio errata.                                                   7
061500110331TNSB33R- Data fine errata.                                                     8
061600110331TNSB33R- Data inizio superiore alla data fine.                                 9
061700110331TNSB33R- Data inizio maggiore della data piu alta consentita.                 10
061800110331TNSB33R- Data fine maggiore della data piu alta consentita.                   11
061900110331
062000110331
062100110331
062200110331
062300110331TNSB33R- 16                                                                   16
062400110331TNSB33R- 17                                                                   17
062500110331TNSB33R- 18                                                                   18
062600110331
062700110331TNSB33R- 20                                                                   20
062800110331TNSB33R- 21                                                                   21
062900110331TNSB33R- 22                                                                   22
063000930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
063100930806F1    -                   -    NN     NN
063200930723F2    -                   -    NN     NN
063300930921F3    -F3=Fine            -    SS     SS
063400930723F4    -                   -    NN     NN
063500930723F5    -                   -    NN     NN
063600931026F6    -F6=Lancio          -    SS     SS
063700930723F7    -                   -    NN     NN
063800930723F8    -                   -    NN     NN
063900930723F9    -                   -    NN     NN
064000930723F10   -                   -    NN     NN
064100930723F11   -                   -    NN     NN
064200931115F12   -F12=Ritorno        -    SS     SS
064300930723F13   -                   -    NN     NN
064400930723F14   -                   -    NN     NN
064500930723F15   -                   -    NN     NN
064600930723F16   -                   -    NN     NN
064700930723F17   -                   -    NN     NN
064800930723F18   -                   -    NN     NN
064900930723F19   -                   -    NN     NN
065000930723F20   -                   -    NN     NN
065100930804F21   -                   -    NN     NN
065200930927F22   -                   -    NN     NN
065300930723F23   -                   -    NN     NN
065400930803F24   -F24=Altri tasti    -    NN     NN
065500930921ENTER -                   -    SS     SS
065600930802ROLLUP-                   -    NN     NN
065700930802ROLLDN-                   -    NN     NN
065800930802HELP  -                   -    NN     NN
065900930802PRINT -                   -    NN     NN
066000930802BSPACE-                   -    NN     NN
066100930802CLEAR -                   -    NN     NN
