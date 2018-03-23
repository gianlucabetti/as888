000100930923     H*===============================================================*
000200950309     H* - TRASFERIMENTO BOLLE DA TNTASP A SALVATAGGIO   - FILTRO -    *
000300930923     H*===============================================================*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500950309     FTNSB37D   CF   E             WORKSTN INFDS(DEVDS1)
000600950309     F                                     SFILE(SB37DVM:MSGULT)
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
008100020220     D  PARDFP                17     24  0                                      DT.FIN PT
008101050706     D  PARcop                25     27                                         DT.FIN PT
008102040913     D*================================================================
008103040913     d dtaiso          s               d
008104040913     d dtaeur          s               d   datfmt(*eur)
008200000000     C*================================================================
008300940826     C* MAIN - PROGRAMMA FILTRO
008400930922     C*================================================================
008500931008     C*
008600000000     C* CICLO FINO A CHE:
008700930922DO  1C     $FINE         DOUEQ     '1'
008800000000     C*
008900931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009000000000     C*
009100930721     C* CONTROLLA GESTIONE VIDEO
009200930922IF  2C     $GES          IFEQ      'V1'
009300930806     C                   EXSR      GESV1                                        *PARAMETRI
009400930922E   2C                   ENDIF
009500000000     C*
009600930922E   1C                   ENDDO
009700000000     C*
009800931018     C* FINE MAIN DEL PGM
009900000000     C                   SETON                                        LR
010000930922     C*================================================================
010100930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
010200930922     C*================================================================
010300930922     C     GESV1         BEGSR
010400930922     C*
010500930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010600930922     C                   EXSR      CARV1                                        *CARICA VIDEO
010700930922E   1C                   ENDIF
010800930922     C*
010900931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011000931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011100931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011200931018E   1C                   ENDIF
011300931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011400930922     C*
011500950309     C                   EXFMT     SB37DV1                                      *PARAMETRI
011600930922     C*
011700930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011800930922     C*
011900930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012000931008SL  2C                   SELECT
012100930922     C     KEY           WHENEQ    ENTER
012200930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012300930922     C     KEY           WHENEQ    F6
012400930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012500930922     C                   OTHER
012600930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012700930922E   2C                   ENDSL
012800930922E   1C                   ENDIF
012900930922     C*
013000930922     C                   ENDSR
013100930713     C*================================================================
013200930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
013300930713     C*================================================================
013400930806     C     CARV1         BEGSR
013500930713     C*
013600930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013700930713     C*
013800930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013900930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014000930713     C*
014100950309     C                   WRITE     SB37DVT                                      *TESTATA
014200950309     C                   WRITE     SB37DVF                                      *FUNZIONI
014300930713     C*
014400930713     C                   ENDSR
014500930804     C*================================================================
014600930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
014700930804     C*================================================================
014800930806     C     ENTV1         BEGSR
014900930806     C*
015000930806     C* CONTROLLA I CAMPI IMMESSI
015100930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015200930804     C*
015300930804     C                   ENDSR
015400000000     C*================================================================
015500930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015600000000     C*================================================================
015700930126     C     F06GES        BEGSR
015800930923     C*
015900930924     C* CONTROLLA I CAMPI IMMESSI
016000930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016100930923     C*
016200930923     C* NON CI SONO ERRORI
016300930923IF  1C     $ERR          IFEQ      '0'
016400930923     C*
016500930923     C* LANCIO PROGRAMMA DI STAMPA
016600931115     C                   Z-ADD     W1CDTI        PARDTI
016700931115     C                   Z-ADD     W1CDTF        PARDTF
016800020220     C                   Z-ADD     W1CDFP        PARDFP
016801050706     C                   eval      PARcop = v1ccop
016900930923     C*
017000950309     C                   MOVEL     'SB38'        KCOAZ
017100930923     C                   MOVEL     PARAM         KPJBU
017200020308     C                   CALL      'BCH10'
017300020308 MOM C***                  CALL 'TNSB38C'
017400930923     C                   PARM                    KPJBA
017500930923     C*
017600930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
017700930923E   1C                   ENDIF
017800930127     C*
017900000000     C                   ENDSR
018000930205     C*================================================================
018100930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018200930205     C*================================================================
018300930906     C     DECODI        BEGSR
018400930205     C*
018500931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
018600040914     C                   SETOFF                                       606162
018700930712     C*
018800931115     C* CONTROLLO DATA INIZIO
018900950308     C                   Z-ADD     V1CDTI        G08DAT
019000950308     C                   CALL      'XSRDA8'
019100950308     C                   PARM                    G08DS
019200950308     C     G08ERR        IFEQ      '1'
019300931115     C                   MOVEL     '1'           $ERR
019400931115     C                   MOVEL     MSG(7)        $MSG
019500931115     C                   EXSR      CARMSG
019600931115     C                   SETON                                        60
019700931115     C                   ELSE
019800950309     C     G08INV        IFGT      DATMAX
019900950308     C                   MOVEL     '1'           $ERR
020000950308     C                   MOVEL     MSG(10)       $MSG
020100950308     C                   EXSR      CARMSG
020200950308     C                   SETON                                        60
020300950308     C                   ELSE
020400950308     C                   Z-ADD     G08INV        W1CDTI            8 0
020500931115     C                   ENDIF
020600950308     C                   ENDIF
020700930922     C*
020800931115     C* CONTROLLO DATA FINE
020900950308     C                   Z-ADD     V1CDTF        G08DAT
021000950308     C                   CALL      'XSRDA8'
021100950308     C                   PARM                    G08DS
021200950308     C     G08ERR        IFEQ      '1'
021300930922     C                   MOVEL     '1'           $ERR
021400930922     C                   MOVEL     MSG(8)        $MSG
021500930922     C                   EXSR      CARMSG
021600931115     C                   SETON                                        61
021700931115     C                   ELSE
021800950309     C     G08INV        IFGT      DATMAX
021900950308     C                   MOVEL     '1'           $ERR
022000950308     C                   MOVEL     MSG(11)       $MSG
022100950308     C                   EXSR      CARMSG
022200950308     C                   SETON                                        61
022300950308     C                   ELSE
022400950308     C                   Z-ADD     G08INV        W1CDTF            8 0
022500950308     C                   ENDIF
022600931115     C                   ENDIF
022700020219     C*
022800020219     C* CONTROLLO DATA FINE X BOLLE POSTE
022900020219     C                   Z-ADD     V1CDFP        G08DAT
023000020219     C                   CALL      'XSRDA8'
023100020219     C                   PARM                    G08DS
023200020219     C     G08ERR        IFEQ      '1'
023300020219     C                   MOVEL     '1'           $ERR
023400020219     C                   MOVEL     MSG(8)        $MSG
023500020219     C                   EXSR      CARMSG
023600020219     C                   SETON                                        62
023700020219     C                   ELSE
023800020220     C     G08INV        IFGT      DPTMAX
023900020219     C                   MOVEL     '1'           $ERR
024000020219     C                   MOVEL     MSG(11)       $MSG
024100020219     C                   EXSR      CARMSG
024200020219     C                   SETON                                        62
024300020219     C                   ELSE
024400020219     C                   Z-ADD     G08INV        W1CDFP            8 0
024500020219     C                   ENDIF
024600020219     C                   ENDIF
024700931008     C*
024800931008     C* ANNO/MESE INIZIALE NON SUPERIORE A QUELLO FINALE
024900931115     C     $ERR          IFEQ      '0'
025000931115     C     W1CDTI        IFGT      W1CDTF
025100020219     C     W1CDTI        ORGT      W1CDFP
025200931008     C                   MOVEL     '1'           $ERR
025300940905     C                   MOVEL     MSG(9)        $MSG
025400931008     C                   EXSR      CARMSG
025500931115     C                   SETON                                        6061
025600931115     C                   ENDIF
025700931115     C                   ENDIF
025701100412     C* DALLA TAB. $5 RECUPERO L'anno di archiviazione fatturazione traini
025702100412     c* se non archiviato non faccio lanciare la pulizia
025703100412     C                   Z-ADD     1             KTBUTE
025704100412     C                   MOVEL     '$5'          KTBCOD
025705100412     C                   MOVEL     w1cdtf        com004            4
025706100412     C                   MOVEL(p)  com004        KTBKEY
025707100412     C     KEYTAB        CHAIN(N)  TABEL                              99
025708100412     C     *IN99         IFEQ      '1'
025714100412     C                   MOVEL     '1'           $ERR
025715100412     C                   MOVEL     MSG(16)       $MSG                           *TAB NON TROVATA
025716100412     C                   EXSR      CARMSG
025717100412     C                   SETON                                        606162
025718100412     C                   ENDIF
025800930923     C*
025900931008     C                   ENDSR
026000020920     C*--------------------------------------------------------------------------------------------*
026100020920     C* REPERISCE I DATI UTENTE
026200020920     C*--------------------------------------------------------------------------------------------*
026300020920     C     REPDATIUTE    BEGSR
026400020920     C*
026500020920     C* INIZIALIZZA VARIABILI DI WRK
026600020920     C                   CLEAR                   TIBS34DS
026700020920     C                   CLEAR                   AZUTEDS
026800020920     C                   CLEAR                   DDATIUTE
026900020920     C*
027000020920     C     *DTAARA       DEFINE    §azute        azuteds
027100020920     C     *DTAARA       DEFINE    §datiute      ddatiute
027200020920     C                   IN(E)     *DTAARA
027300020920if  1C                   IF        %Error
027400020920     c                   EVAL      I34Tla = 'L'
027500020920     C                   CALL      'TIBS34R'
027600020920     C                   PARM                    Tibs34Ds
027700020920     C                   IN        *DTAARA
027800020920e   1C                   ENDIF
027900020920     C*
028000020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
028100020920     C                   MOVEL(P)  RSUT          DSFIRS           20
028200020920     C*
028300020920     C                   ENDSR
028400930923     C*================================================================
028500931008     C* *INZSR  - ROUTINE INIZIALE
028600000000     C*================================================================
028700930713     C     *INZSR        BEGSR
028800000000     C*
028900000000     C* RICEVIMENTO PARAMETRI
029000000000     C     *ENTRY        PLIST
029100930922     C                   PARM                    KPJBA
029200950308     C* DEFINISCO CAMPI
029300950308     C     *LIKE         DEFINE    TBLKUT        KTBUTE
029400950308     C     *LIKE         DEFINE    TBLCOD        KTBCOD
029500950308     C     *LIKE         DEFINE    TBLKEY        KTBKEY
029600950308     C*
029700950308     C* CHIAVE LETTURA TABEL00F - COMPLETA
029800950308     C     KEYTAB        KLIST
029900950308     C                   KFLD                    KTBUTE
030000950308     C                   KFLD                    KTBCOD
030100950308     C                   KFLD                    KTBKEY
030200930714     C*
030300930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
030400930921     C                   EXSR      INZSTD
030500931008     C*
030600931008     C* IMPOSTO CAMPI TESTATA VIDEO
030700931008     C                   MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
030800950308     C*
030900950308     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE
031000950308     C                   Z-ADD     1             KTBUTE
031100950308     C                   MOVEL     '$3'          KTBCOD
031200950308     C                   MOVEL     '1'           KTBKEY
031300950308     C     KEYTAB        CHAIN(N)  TABEL                              99
031400950308     C     *IN99         IFEQ      '0'
031500950308     C                   MOVEL     TBLUNI        DS$3
031501040913     c                   move      §$3d10        dtaiso
031502040913     c                   subdur    §$3mp0:*M     dtaiso
031503040914     c                   move      dtaiso        datmax            8 0
032500950309     C                   ELSE
032600950309     C                   MOVEL     '1'           $ERR
032700950309     C                   MOVEL     MSG(15)       $MSG                           *TAB NON TROVATA
032800950309     C                   EXSR      CARMSG
032900020219     C                   SETON                                        606162
033000950309     C                   ENDIF
033200020220     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO P > SAV
033300950309     C* MENO I MESI PER I QUALI LA BOLLA DEVE RIMANRE IN TNTASP
035401040913     c                   subdur    1:*m          dtaiso
035402040913     c                   move      dtaiso        dtaeur
035403040913     c                   move      dtaeur        v1cdtf
035600020220     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE X LE BOLLE POSTE
037201040913     c                   move      §$3d00        dtaiso
037202040913     c                   subdur    §$3mpp:*M     dtaiso
037203040914     c                   move      dtaiso        dptmax            8 0
037400020219     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO POSTE > SAV
037500020219     C* MENO I MESI PER I QUALI LA BOLLA POSTE DEVE RIMANRE IN TNTASP
037501040913     c                   subdur    1:*m          dtaiso
037502040913     c                   move      dtaiso        dtaeur
037503040913     c                   move      dtaeur        v1cdfp
039700020220     C*
039800020220     C* INFINE PROPONGO LA DATA INIZIALE
039900950310     C                   Z-ADD     01011990      V1CDTI                         *DATA INIZIO PROPOST
040000020220     C*
040001050706     c                   eval      v1ccop = 'MOV'
040100950309     C* EMETTO FORMATO VIDEO
040200930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
040300930923     C*
040400000000     C                   ENDSR
040500930924     C*================================================================
040600930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040800930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
040900930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
041000930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
041100930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
041200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
041300930924     C*================================================================
041400930805     C*
041500930714     C*================================================================
041600930923     C* INZSTD - ROUTINE INIZIALE STANDARD
041700930714     C*================================================================
041800930803     C     INZSTD        BEGSR
041900930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
042000930802     C                   CALL      'XKEYS'
042100930802     C                   PARM                    DSKEYS
042200930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
042300930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
042400930714     C     XMSGPA        PLIST
042500930714     C                   PARM                    DSMSMS
042600930714     C                   PARM                    DSMSPG
042700930714     C                   PARM                    DSMSKE
042800930714     C                   PARM                    DSMSER
042900930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
043000950309     C                   MOVEL     'TNSB37R'     DSMSPG                         *NOME PGM CHIAMANTE
043100950309     C                   MOVEL     'TNSB37R'     VMCQUE                         *CODA MESSAGGI DESTI
043200930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
043300930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
043400930805     C* INIZIALIZZO FLAG CONTROLLO PGM
043500930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
043600930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
043700930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
043800930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
043900930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
044000930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
044100930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
044200930714     C* LANCIO ROUTINE PER VERIFICARE SE
044300930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
044400930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
044500020920     C                   EXSR      REPDATIUTE
044600930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
044700930721     C                   BITOFF    '01234567'    POSMI1            1
044800930721     C                   BITOFF    '01234567'    POSMI2            1
044900930721     C                   BITOFF    '01234567'    POSMA1            1
045000930721     C                   BITOFF    '01234567'    POSMA2            1
045100930721     C                   BITON     '34'          POSMI1            1            *18
045200930721     C                   BITON     '7'           POSMI2            1            *01
045300930721     C                   BITON     '34'          POSMA1            1            *18
045400930721     C                   BITON     '0'           POSMA2            1            *80
045500930721     C                   MOVE      POSMI2        POSMIN            2
045600930721     C                   MOVEL     POSMI1        POSMIN
045700930721     C                   MOVE      POSMA2        POSMAX            2
045800930721     C                   MOVEL     POSMA1        POSMAX
045900930714     C                   ENDSR
046000930804     C*================================================================
046100930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
046200930804     C*================================================================
046300930804     C     FUNSTD        BEGSR
046400930804     C                   SELECT
046500930804     C     KEY           WHENEQ    F3
046600930804     C                   EXSR      F03STD
046700931115     C     KEY           WHENEQ    F12
046800931115     C                   EXSR      F12STD
046900930804     C     KEY           WHENEQ    F24
047000930804     C                   EXSR      F24STD
047100930804     C                   OTHER
047200930804     C                   MOVEL     MSG(2)        $MSG
047300930804     C                   EXSR      CARMSG
047400930804     C                   ENDSL
047500930804     C                   ENDSR
047600930804     C*================================================================
047700930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
047800930804     C*================================================================
047900930804     C     F03STD        BEGSR
048000930923     C*
048100930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
048200930927     C                   MOVEL     '1'           $FINE
048300930927     C*
048400930927     C                   ENDSR
048500931115     C*================================================================
048600931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
048700931115     C*================================================================
048800931115     C     F12STD        BEGSR
048900931115     C* IMPOSTO FLAG DI FIN PGM
049000931115     C                   MOVEL     '1'           $FINE
049100931115     C                   ENDSR
049200930804     C*================================================================
049300930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
049400930804     C*================================================================
049500930804     C     F24STD        BEGSR
049600930923     C*
049700930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
049800930804     C                   EXSR      OUTFUN
049900930923     C*
050000930804     C                   ENDSR
050100930721     C*================================================================
050200930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
050300930721     C*          CODA MESSAGGI DEL PGM
050400930721     C*================================================================
050500930721     C     CARMSG        BEGSR
050600930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
050700930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
050800930721     C     MSGULT        IFEQ      0
050900930721     C                   MOVEL     '1'           *IN40
051000950309     C                   WRITE     SB37DVK
051100930721     C                   MOVEL     '0'           *IN40
051200930805     C                   ENDIF
051300930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
051400930721     C                   MOVEL     $MSG          DSMSMS
051500930721     C                   CALL      'XMSG'        XMSGPA
051600930721     C     DSMSER        IFEQ      0
051700930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
051800930805     C                   ENDIF
051900930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
052000930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
052100950309     C                   WRITE     SB37DVM                                      *SFL MESS
052200930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
052300930721     C                   MOVEL     *BLANKS       $MSG
052400930721     C                   ENDSR
052500930721     C*================================================================
052600930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
052700930721     C*================================================================
052800930721     C     OUTMSG        BEGSR
052900930721     C* VISUALIZZAZIONE MESSAGGIO
053000930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
053100950309     C                   WRITE     SB37DVK                                      *CONTR MESS
053200930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
053300930721     C                   Z-ADD     *ZEROS        MSGULT
053400930721     C                   ENDSR
053500930803     C*================================================================
053600930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
053700930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
053800930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
053900930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
054000930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
054100930803     C*================================================================
054200930803     C     CARFUN        BEGSR
054300930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
054400930923     C     1             DO        31            I                 5 0          --- 1 -->
054500930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
054600930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
054700930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
054800020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
054900930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
055000930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
055100930803     C                   ELSE                                                   *INTERROG.
055200930803     C                   MOVEL     FUNASI        FAT(I)
055300930803     C                   ENDIF
055400930803     C                   ELSE                                                   <-- 2 --> *FILIALE
055500930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
055600930803     C                   MOVEL     FUNAFG        FAT(I)
055700930803     C                   ELSE                                                   *INTERROG.
055800930922     C                   MOVEL     FUNAFI        FAT(I)
055900930803     C                   ENDIF
056000930803     C                   ENDIF                                                  <-- 2 ---
056100930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
056200930806     C                   MOVEL     USER          WEDP              3            *PROFILO
056300930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
056400930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
056500930806     C                   ELSE
056600930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
056700930806     C                   ENDIF
056800930806     C                   ENDIF
056900930803     C                   ENDDO                                                  <-- 1 ---
057000930803     C                   ENDSR
057100930803     C*================================================================
057200930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
057300930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
057400930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
057500930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
057600930803     C*================================================================
057700930803     C     OUTFUN        BEGSR
057800930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
057900930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
058000930803     C                   ENDDO
058100930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
058200930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
058300930803     C     1             DO        31            I
058400930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
058500930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
058600930803     C                   ADD       1             FUNNAT
058700930803     C                   ENDIF
058800930803     C                   ENDDO
058900930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
059000930803     C     FUNNAT        IFGT      8
059100930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
059200930803     C                   ENDIF
059300930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
059400930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
059500930803     C                   Z-ADD     8             FUNVIS            5 0
059600930803     C     FAT(23)       IFEQ      'S'
059700930803     C                   SUB       1             FUNVIS
059800930803     C                   ENDIF
059900930803     C     FAT(24)       IFEQ      'S'
060000930803     C                   SUB       1             FUNVIS
060100930803     C                   ENDIF
060200930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
060300930923     C                   Z-ADD     *ZEROS        W                 5 0
060400930803     C                   ADD       1             FUNULT
060500930803     C     FUNULT        IFGT      31
060600930803     C                   Z-ADD     1             FUNULT
060700930805     C                   ENDIF
060800930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
060900930803     C     FUNULT        DO        31            I                              --- 1 -->
061000930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
061100930803     C     W             IFLT      FUNVIS                                       --- 2 -->
061200930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
061300930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
061400930803     C     FDE(I)        ANDNE     *BLANKS
061500930805     C     I             IFEQ      23                                           *TOLGO F23
061600930805     C     I             OREQ      24                                           *E F24
061700930803     C                   ELSE
061800930803     C                   ADD       1             W
061900930803     C                   MOVEL     FDE(I)        FUV(W)
062000930803     C                   ENDIF
062100930803     C                   ENDIF                                                  <-- 3 ---
062200930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
062300930803     C                   Z-ADD     I             FUNULT
062400930803     C                   ENDIF                                                  <-- 2 ---
062500930803     C                   ENDDO                                                  <-- 1 ---
062600930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
062700930806     C                   MOVEL     FUV(1)        VFD001
062800930806     C                   MOVEL     FUV(2)        VFD002
062900930806     C                   MOVEL     FUV(3)        VFD003
063000930806     C                   MOVEL     FUV(4)        VFD004
063100930806     C                   MOVEL     FUV(5)        VFD005
063200930806     C                   MOVEL     FUV(6)        VFD006
063300930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
063400930803     C     FAT(24)       ANDEQ     'N'
063500930806     C                   MOVEL     FUV(7)        VFD007
063600930806     C                   MOVEL     FUV(8)        VFD008
063700930803     C                   ELSE                                                   <-- 1 -->
063800930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
063900930803     C     FAT(24)       ANDEQ     'S'
064000930806     C                   MOVEL     FDE(23)       VFD007
064100930806     C                   MOVEL     FDE(24)       VFD008
064200930803     C                   ELSE                                                   <-- 2 -->
064300930806     C                   MOVEL     FUV(7)        VFD007
064400930803     C     FAT(23)       IFEQ      'S'
064500930806     C                   MOVEL     FDE(23)       VFD008
064600930803     C                   ELSE
064700930806     C                   MOVEL     FDE(24)       VFD008
064800930805     C                   ENDIF
064900930805     C                   ENDIF                                                  <-- 2 ---
065000930805     C                   ENDIF                                                  <-- 1 ---
065100930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
065200930803     C                   ENDSR
065300930803     C*================================================================
065400930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
065500930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
065600930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
065700930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
065800930803     C*================================================================
065900930803     C     CONFUN        BEGSR
066000930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
066100930803     C     1             DO        31            I                              --- 1 -->
066200930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
066300930805     C* CONTROLLO SE IL TASTO E' ATTIVO
066400930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
066500930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
066600930803     C                   EXSR      CARMSG
066700930803     C                   MOVEL     '1'           $ERR
066800930803     C                   ENDIF                                                  <-- 3 ---
066900930803     C                   ENDIF                                                  <-- 2 ---
067000930803     C                   ENDDO                                                  <-- 1 ---
067100930803     C                   ENDSR
067200930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
067300950309TNSB37R- Tasto funzionale non valido.                                          1
067400950309TNSB37R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
067500950309TNSB37R- Opzione non valida.                                                   3
067600950309TNSB37R- Opzione abilitata ma non gestita. Avvertire CED.                      4
067700950309TNSB37R- Inizio elenco.                                                        5
067800950309TNSB37R- Fine elenco.                                                          6
067900950309TNSB37R- Data inizio errata.                                                   7
068000950309TNSB37R- Data fine errata.                                                     8
068100950309TNSB37R- Data inizio superiore alla data fine.                                 9
068200950309TNSB37R- Data inizio maggiore della data piu alta consentita.                 10
068300950309TNSB37R- Data fine maggiore della data piu alta consentita.                   11
068400950309TNSB37R- Codice filiale errato                                                12
068500950309TNSB37R- Codice area errato                                                   13
068600950309TNSB37R- Codice divisione errato                                              14
068700100412TNSB37R- Tab. '$3' non trovata. Impossibile continuare.                       15
068800100412TNSB37R- Tab. '$5' non trovata. Impossibile continuare.                       16
068900950309TNSB37R- 17                                                                   17
069000950309TNSB37R- 18                                                                   18
069100950309TNSB37R- Immettere solo una selezione per filiale, area o divisione           19
069200950309TNSB37R- 20                                                                   20
069300950309TNSB37R- 21                                                                   21
069400950309TNSB37R- 22                                                                   22
069500930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
069600930806F1    -                   -    NN     NN
069700930723F2    -                   -    NN     NN
069800930921F3    -F3=Fine            -    SS     SS
069900930723F4    -                   -    NN     NN
070000930723F5    -                   -    NN     NN
070100931026F6    -F6=Lancio          -    SS     SS
070200930723F7    -                   -    NN     NN
070300930723F8    -                   -    NN     NN
070400930723F9    -                   -    NN     NN
070500930723F10   -                   -    NN     NN
070600930723F11   -                   -    NN     NN
070700931115F12   -F12=Ritorno        -    SS     SS
070800930723F13   -                   -    NN     NN
070900930723F14   -                   -    NN     NN
071000930723F15   -                   -    NN     NN
071100930723F16   -                   -    NN     NN
071200930723F17   -                   -    NN     NN
071300930723F18   -                   -    NN     NN
071400930723F19   -                   -    NN     NN
071500930723F20   -                   -    NN     NN
071600930804F21   -                   -    NN     NN
071700930927F22   -                   -    NN     NN
071800930723F23   -                   -    NN     NN
071900930803F24   -F24=Altri tasti    -    NN     NN
072000930921ENTER -                   -    SS     SS
072100930802ROLLUP-                   -    NN     NN
072200930802ROLLDN-                   -    NN     NN
072300930802HELP  -                   -    NN     NN
072400930802PRINT -                   -    NN     NN
072500930802BSPACE-                   -    NN     NN
072600930802CLEAR -                   -    NN     NN
