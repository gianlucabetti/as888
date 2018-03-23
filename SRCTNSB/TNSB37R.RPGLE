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
003400161221     D* DS TABELLA $5 (DATE allegati Fatture Intragruppo)
003500161221     D DS$5          E DS
003600930922     D*================================================================
003700950309     D* DS PER LANCIO PGM 'XSRDA8' (DATA>GIORNI) + (INVERSIONE DATA)
003800950308     D G08DS           DS                  INZ
003900950308     D  G08DAT                 1      8  0
004000950308     D  G08INV                 9     16  0
004100950308     D  G08ERR                17     17
004200950308     D  G08TGI                18     22  0
004300950309     D*================================================================
004400950309     D* DS PER LANCIO PGM 'XSRGI8' (GIORNI>DATA)
004500950309     D GI8DS           DS                  INZ
004600950309     D  GI8DAT                 1      8  0
004700950309     D  GI8INV                 9     16  0
004800950309     D  GI8TGI                17     21  0
004900930922     D*================================================================
005000000000     D* DS PER GESTIONE TASTI FUNZIONALI
005100930802     D DSKEYS        E DS
005200930803     D  KEYRIG                 1     40
005300930922     D*================================================================
005400930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005500930806     D                SDS
005600930806     D* NOME UTENTE
005700930806     D  USER                 254    263
005800930922     D*================================================================
005900930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006000000000     D DEVDS1          DS
006100930720     D* CHIAVE FUNZIONALE BATTUTA
006200000000     D  KEY                  369    369
006300930721     D* POSIZIONE CURSORE (IN HEX)
006400930721     D  CURSOR               370    371
006500950308     D***************************************************************
006600950308     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
006700950308     D DS§DAT          DS
006800950308     D  DSSEC                  1      2  0
006900950308     D  DSANN                  3      4  0
007000950308     D  DSMES                  5      6  0
007100950308     D  DSGIO                  7      8  0
007200950308     D  DSAAS                  1      4  0
007300950308     D  DSMGS                  5      8  0
007400950308     D  DSDAT6                 3      8  0
007500950308     D  DSDAT8                 1      8  0
007600930922     D*================================================================
007700000000     D* ARCHITETTURA
007800930922     D KPJBA         E DS
007900930923     D*================================================================
008000931008     D PARAM           DS                  INZ
008100950308     D  PARDTI                 1      8  0                                      DT.INI
008200950308     D  PARDTF                 9     16  0                                      DT.FIN
008300020220     D  PARDFP                17     24  0                                      DT.FIN PT
008400050706     D  PARcop                25     27                                         DT.FIN PT
008500040913     D*================================================================
008600040913     d dtaiso          s               d
008700040913     d dtaeur          s               d   datfmt(*eur)
008800000000     C*================================================================
008900940826     C* MAIN - PROGRAMMA FILTRO
009000930922     C*================================================================
009100931008     C*
009200000000     C* CICLO FINO A CHE:
009300930922DO  1C     $FINE         DOUEQ     '1'
009400000000     C*
009500931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009600000000     C*
009700930721     C* CONTROLLA GESTIONE VIDEO
009800930922IF  2C     $GES          IFEQ      'V1'
009900930806     C                   EXSR      GESV1                                        *PARAMETRI
010000930922E   2C                   ENDIF
010100000000     C*
010200930922E   1C                   ENDDO
010300000000     C*
010400931018     C* FINE MAIN DEL PGM
010500000000     C                   SETON                                        LR
010600930922     C*================================================================
010700930923     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
010800930922     C*================================================================
010900930922     C     GESV1         BEGSR
011000930922     C*
011100930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011200930922     C                   EXSR      CARV1                                        *CARICA VIDEO
011300930922E   1C                   ENDIF
011400930922     C*
011500931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011600931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011700931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011800931018E   1C                   ENDIF
011900931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012000930922     C*
012100950309     C                   EXFMT     SB37DV1                                      *PARAMETRI
012200930922     C*
012300930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012400930922     C*
012500930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012600931008SL  2C                   SELECT
012700930922     C     KEY           WHENEQ    ENTER
012800930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012900930922     C     KEY           WHENEQ    F6
013000930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013100930922     C                   OTHER
013200930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013300930922E   2C                   ENDSL
013400930922E   1C                   ENDIF
013500930922     C*
013600930922     C                   ENDSR
013700930713     C*================================================================
013800930923     C* CARV1 - CARICA VIDEO 'PARAMETRI'
013900930713     C*================================================================
014000930806     C     CARV1         BEGSR
014100930713     C*
014200930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014300930713     C*
014400930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014500930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014600930713     C*
014700950309     C                   WRITE     SB37DVT                                      *TESTATA
014800950309     C                   WRITE     SB37DVF                                      *FUNZIONI
014900930713     C*
015000930713     C                   ENDSR
015100930804     C*================================================================
015200930923     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
015300930804     C*================================================================
015400930806     C     ENTV1         BEGSR
015500930806     C*
015600930806     C* CONTROLLA I CAMPI IMMESSI
015700930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015800930804     C*
015900930804     C                   ENDSR
016000000000     C*================================================================
016100930923     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016200000000     C*================================================================
016300930126     C     F06GES        BEGSR
016400930923     C*
016500930924     C* CONTROLLA I CAMPI IMMESSI
016600930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016700930923     C*
016800930923     C* NON CI SONO ERRORI
016900930923IF  1C     $ERR          IFEQ      '0'
017000930923     C*
017100930923     C* LANCIO PROGRAMMA DI STAMPA
017200931115     C                   Z-ADD     W1CDTI        PARDTI
017300931115     C                   Z-ADD     W1CDTF        PARDTF
017400020220     C                   Z-ADD     W1CDFP        PARDFP
017500050706     C                   eval      PARcop = v1ccop
017600930923     C*
017700950309     C                   MOVEL     'SB38'        KCOAZ
017800930923     C                   MOVEL     PARAM         KPJBU
017900020308     C                   CALL      'BCH10'
018000020308 MOM C***                  CALL 'TNSB38C'
018100930923     C                   PARM                    KPJBA
018200930923     C*
018300930923     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
018400930923E   1C                   ENDIF
018500930127     C*
018600000000     C                   ENDSR
018700930205     C*================================================================
018800930923     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018900930205     C*================================================================
019000930906     C     DECODI        BEGSR
019100930205     C*
019200931018     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
019300040914     C                   SETOFF                                       606162
019400930712     C*
019500931115     C* CONTROLLO DATA INIZIO
019600950308     C                   Z-ADD     V1CDTI        G08DAT
019700950308     C                   CALL      'XSRDA8'
019800950308     C                   PARM                    G08DS
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
021900950308     C     G08ERR        IFEQ      '1'
022000930922     C                   MOVEL     '1'           $ERR
022100930922     C                   MOVEL     MSG(8)        $MSG
022200930922     C                   EXSR      CARMSG
022300931115     C                   SETON                                        61
022400931115     C                   ELSE
022500950309     C     G08INV        IFGT      DATMAX
022600950308     C                   MOVEL     '1'           $ERR
022700950308     C                   MOVEL     MSG(11)       $MSG
022800950308     C                   EXSR      CARMSG
022900950308     C                   SETON                                        61
023000950308     C                   ELSE
023100950308     C                   Z-ADD     G08INV        W1CDTF            8 0
023200950308     C                   ENDIF
023300931115     C                   ENDIF
023400020219     C*
023500020219     C* CONTROLLO DATA FINE X BOLLE POSTE
023600020219     C                   Z-ADD     V1CDFP        G08DAT
023700020219     C                   CALL      'XSRDA8'
023800020219     C                   PARM                    G08DS
023900020219     C     G08ERR        IFEQ      '1'
024000020219     C                   MOVEL     '1'           $ERR
024100020219     C                   MOVEL     MSG(8)        $MSG
024200020219     C                   EXSR      CARMSG
024300020219     C                   SETON                                        62
024400020219     C                   ELSE
024500020220     C     G08INV        IFGT      DPTMAX
024600020219     C                   MOVEL     '1'           $ERR
024700020219     C                   MOVEL     MSG(11)       $MSG
024800020219     C                   EXSR      CARMSG
024900020219     C                   SETON                                        62
025000020219     C                   ELSE
025100020219     C                   Z-ADD     G08INV        W1CDFP            8 0
025200020219     C                   ENDIF
025300020219     C                   ENDIF
025400931008     C*
025500931008     C* ANNO/MESE INIZIALE NON SUPERIORE A QUELLO FINALE
025600931115     C     $ERR          IFEQ      '0'
025700931115     C     W1CDTI        IFGT      W1CDTF
025800020219     C     W1CDTI        ORGT      W1CDFP
025900931008     C                   MOVEL     '1'           $ERR
026000940905     C                   MOVEL     MSG(9)        $MSG
026100931008     C                   EXSR      CARMSG
026200931115     C                   SETON                                        6061
026300931115     C                   ENDIF
026400931115     C                   ENDIF
026500170113      **
026600100412     C* DALLA TAB. $5 RECUPERO L'anno di archiviazione fatturazione traini
026700100412     c* se non archiviato non faccio lanciare la pulizia
026800100412     C                   Z-ADD     1             KTBUTE
026900100412     C                   MOVEL     '$5'          KTBCOD
027000170110     C                   MOVEL     w1cdtf        annomese          6
027100170113     C                   MOVEL     annomese      anno4             4 0
027200170113      **
027300170113      ** da dopo il 2016 -  controlla la tabella $5
027400170113     c                   if        anno4 > 2016
027500170110     c                   do        3             tipi              3 0
027600170113      **
027700170110     c                   if        tipi = 1
027800170110     c                   move      'P'           tipo              1
027900170110     c                   elseif    tipi = 2
028000170110     c                   move      'T'           tipo
028100170110     c                   elseif    tipi = 3
028200170110     c                   move      'A'           tipo
028300170110     c                   end
028400170110      ** fino all'anno mese elaborati
028500170110     C                   eval      KTBKEY = tipo + annomese
028600100412     C     KEYTAB        CHAIN(N)  TABEL                              99
028700170110      *
028800170110      ** se non ne ha trovato almeno uno dei 3 tipi  --> segnala l'errore
028900100412     C     *IN99         IFEQ      '1'
029000170217     C     *IN99         OREQ      '0'
029100170217     C     TBLFLG        ANDNE     *blank
029200100412     C                   MOVEL     '1'           $ERR
029300100412     C                   MOVEL     MSG(16)       $MSG                           *TAB NON TROVATA
029400100412     C                   EXSR      CARMSG
029500100412     C                   SETON                                        606162
029600170110     c                   leave
029700100412     C                   ENDIF
029800170113     C*
029900170110     c                   enddo
030000170113     c                   endIF
030100930923     C*
030200931008     C                   ENDSR
030300020920     C*--------------------------------------------------------------------------------------------*
030400020920     C* REPERISCE I DATI UTENTE
030500020920     C*--------------------------------------------------------------------------------------------*
030600020920     C     REPDATIUTE    BEGSR
030700020920     C*
030800020920     C* INIZIALIZZA VARIABILI DI WRK
030900020920     C                   CLEAR                   TIBS34DS
031000020920     C                   CLEAR                   AZUTEDS
031100020920     C                   CLEAR                   DDATIUTE
031200020920     C*
031300020920     C     *DTAARA       DEFINE    §azute        azuteds
031400020920     C     *DTAARA       DEFINE    §datiute      ddatiute
031500020920     C                   IN(E)     *DTAARA
031600020920if  1C                   IF        %Error
031700020920     c                   EVAL      I34Tla = 'L'
031800020920     C                   CALL      'TIBS34R'
031900020920     C                   PARM                    Tibs34Ds
032000020920     C                   IN        *DTAARA
032100020920e   1C                   ENDIF
032200020920     C*
032300020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
032400020920     C                   MOVEL(P)  RSUT          DSFIRS           20
032500020920     C*
032600020920     C                   ENDSR
032700930923     C*================================================================
032800931008     C* *INZSR  - ROUTINE INIZIALE
032900000000     C*================================================================
033000930713     C     *INZSR        BEGSR
033100000000     C*
033200000000     C* RICEVIMENTO PARAMETRI
033300000000     C     *ENTRY        PLIST
033400930922     C                   PARM                    KPJBA
033500950308     C* DEFINISCO CAMPI
033600950308     C     *LIKE         DEFINE    TBLKUT        KTBUTE
033700950308     C     *LIKE         DEFINE    TBLCOD        KTBCOD
033800950308     C     *LIKE         DEFINE    TBLKEY        KTBKEY
033900950308     C*
034000950308     C* CHIAVE LETTURA TABEL00F - COMPLETA
034100950308     C     KEYTAB        KLIST
034200950308     C                   KFLD                    KTBUTE
034300950308     C                   KFLD                    KTBCOD
034400950308     C                   KFLD                    KTBKEY
034500930714     C*
034600930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
034700930921     C                   EXSR      INZSTD
034800931008     C*
034900931008     C* IMPOSTO CAMPI TESTATA VIDEO
035000931008     C                   MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
035100950308     C*
035200950308     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE
035300950308     C                   Z-ADD     1             KTBUTE
035400950308     C                   MOVEL     '$3'          KTBCOD
035500950308     C                   MOVEL     '1'           KTBKEY
035600950308     C     KEYTAB        CHAIN(N)  TABEL                              99
035700950308     C     *IN99         IFEQ      '0'
035800950308     C                   MOVEL     TBLUNI        DS$3
035900040913     c                   move      §$3d10        dtaiso
036000040913     c                   subdur    §$3mp0:*M     dtaiso
036100040914     c                   move      dtaiso        datmax            8 0
036200950309     C                   ELSE
036300950309     C                   MOVEL     '1'           $ERR
036400950309     C                   MOVEL     MSG(15)       $MSG                           *TAB NON TROVATA
036500950309     C                   EXSR      CARMSG
036600020219     C                   SETON                                        606162
036700950309     C                   ENDIF
036800020220     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO P > SAV
036900950309     C* MENO I MESI PER I QUALI LA BOLLA DEVE RIMANRE IN TNTASP
037000040913     c                   subdur    1:*m          dtaiso
037100040913     c                   move      dtaiso        dtaeur
037200040913     c                   move      dtaeur        v1cdtf
037300020220     C* DALLA TAB. $3 RECUPERO LA DATA PIU' ALTA ACCETTABILE X LE BOLLE POSTE
037400040913     c                   move      §$3d00        dtaiso
037500040913     c                   subdur    §$3mpp:*M     dtaiso
037600040914     c                   move      dtaiso        dptmax            8 0
037700020219     C* COME DATA FINALE IMPOSTO LA DATA FINALE ULT. LANCIO POSTE > SAV
037800020219     C* MENO I MESI PER I QUALI LA BOLLA POSTE DEVE RIMANRE IN TNTASP
037900040913     c                   subdur    1:*m          dtaiso
038000040913     c                   move      dtaiso        dtaeur
038100040913     c                   move      dtaeur        v1cdfp
038200020220     C*
038300020220     C* INFINE PROPONGO LA DATA INIZIALE
038400950310     C                   Z-ADD     01011990      V1CDTI                         *DATA INIZIO PROPOST
038500020220     C*
038600050706     c                   eval      v1ccop = 'MOV'
038700950309     C* EMETTO FORMATO VIDEO
038800930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
038900930923     C*
039000000000     C                   ENDSR
039100930924     C*================================================================
039200930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039300930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039400930924     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
039500930924     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
039600930924     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
039700930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039800930924     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039900930924     C*================================================================
040000930805     C*
040100930714     C*================================================================
040200930923     C* INZSTD - ROUTINE INIZIALE STANDARD
040300930714     C*================================================================
040400930803     C     INZSTD        BEGSR
040500930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
040600930802     C                   CALL      'XKEYS'
040700930802     C                   PARM                    DSKEYS
040800930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
040900930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
041000930714     C     XMSGPA        PLIST
041100930714     C                   PARM                    DSMSMS
041200930714     C                   PARM                    DSMSPG
041300930714     C                   PARM                    DSMSKE
041400930714     C                   PARM                    DSMSER
041500930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
041600950309     C                   MOVEL     'TNSB37R'     DSMSPG                         *NOME PGM CHIAMANTE
041700950309     C                   MOVEL     'TNSB37R'     VMCQUE                         *CODA MESSAGGI DESTI
041800930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
041900930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
042000930805     C* INIZIALIZZO FLAG CONTROLLO PGM
042100930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
042200930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
042300930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
042400930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
042500930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
042600930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
042700930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
042800930714     C* LANCIO ROUTINE PER VERIFICARE SE
042900930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
043000930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
043100020920     C                   EXSR      REPDATIUTE
043200930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
043300930721     C                   BITOFF    '01234567'    POSMI1            1
043400930721     C                   BITOFF    '01234567'    POSMI2            1
043500930721     C                   BITOFF    '01234567'    POSMA1            1
043600930721     C                   BITOFF    '01234567'    POSMA2            1
043700930721     C                   BITON     '34'          POSMI1            1            *18
043800930721     C                   BITON     '7'           POSMI2            1            *01
043900930721     C                   BITON     '34'          POSMA1            1            *18
044000930721     C                   BITON     '0'           POSMA2            1            *80
044100930721     C                   MOVE      POSMI2        POSMIN            2
044200930721     C                   MOVEL     POSMI1        POSMIN
044300930721     C                   MOVE      POSMA2        POSMAX            2
044400930721     C                   MOVEL     POSMA1        POSMAX
044500930714     C                   ENDSR
044600930804     C*================================================================
044700930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
044800930804     C*================================================================
044900930804     C     FUNSTD        BEGSR
045000930804     C                   SELECT
045100930804     C     KEY           WHENEQ    F3
045200930804     C                   EXSR      F03STD
045300931115     C     KEY           WHENEQ    F12
045400931115     C                   EXSR      F12STD
045500930804     C     KEY           WHENEQ    F24
045600930804     C                   EXSR      F24STD
045700930804     C                   OTHER
045800930804     C                   MOVEL     MSG(2)        $MSG
045900930804     C                   EXSR      CARMSG
046000930804     C                   ENDSL
046100930804     C                   ENDSR
046200930804     C*================================================================
046300930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
046400930804     C*================================================================
046500930804     C     F03STD        BEGSR
046600930923     C*
046700930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
046800930927     C                   MOVEL     '1'           $FINE
046900930927     C*
047000930927     C                   ENDSR
047100931115     C*================================================================
047200931115     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
047300931115     C*================================================================
047400931115     C     F12STD        BEGSR
047500931115     C* IMPOSTO FLAG DI FIN PGM
047600931115     C                   MOVEL     '1'           $FINE
047700931115     C                   ENDSR
047800930804     C*================================================================
047900930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
048000930804     C*================================================================
048100930804     C     F24STD        BEGSR
048200930923     C*
048300930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
048400930804     C                   EXSR      OUTFUN
048500930923     C*
048600930804     C                   ENDSR
048700930721     C*================================================================
048800930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
048900930721     C*          CODA MESSAGGI DEL PGM
049000930721     C*================================================================
049100930721     C     CARMSG        BEGSR
049200930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
049300930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
049400930721     C     MSGULT        IFEQ      0
049500930721     C                   MOVEL     '1'           *IN40
049600950309     C                   WRITE     SB37DVK
049700930721     C                   MOVEL     '0'           *IN40
049800930805     C                   ENDIF
049900930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
050000930721     C                   MOVEL     $MSG          DSMSMS
050100930721     C                   CALL      'XMSG'        XMSGPA
050200930721     C     DSMSER        IFEQ      0
050300930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
050400930805     C                   ENDIF
050500930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
050600930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
050700950309     C                   WRITE     SB37DVM                                      *SFL MESS
050800930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
050900930721     C                   MOVEL     *BLANKS       $MSG
051000930721     C                   ENDSR
051100930721     C*================================================================
051200930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
051300930721     C*================================================================
051400930721     C     OUTMSG        BEGSR
051500930721     C* VISUALIZZAZIONE MESSAGGIO
051600930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
051700950309     C                   WRITE     SB37DVK                                      *CONTR MESS
051800930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
051900930721     C                   Z-ADD     *ZEROS        MSGULT
052000930721     C                   ENDSR
052100930803     C*================================================================
052200930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
052300930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
052400930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
052500930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
052600930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
052700930803     C*================================================================
052800930803     C     CARFUN        BEGSR
052900930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
053000930923     C     1             DO        31            I                 5 0          --- 1 -->
053100930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
053200930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
053300930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
053400020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
053500930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
053600930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
053700930803     C                   ELSE                                                   *INTERROG.
053800930803     C                   MOVEL     FUNASI        FAT(I)
053900930803     C                   ENDIF
054000930803     C                   ELSE                                                   <-- 2 --> *FILIALE
054100930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
054200930803     C                   MOVEL     FUNAFG        FAT(I)
054300930803     C                   ELSE                                                   *INTERROG.
054400930922     C                   MOVEL     FUNAFI        FAT(I)
054500930803     C                   ENDIF
054600930803     C                   ENDIF                                                  <-- 2 ---
054700930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
054800930806     C                   MOVEL     USER          WEDP              3            *PROFILO
054900930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
055000930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
055100930806     C                   ELSE
055200930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
055300930806     C                   ENDIF
055400930806     C                   ENDIF
055500930803     C                   ENDDO                                                  <-- 1 ---
055600930803     C                   ENDSR
055700930803     C*================================================================
055800930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
055900930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
056000930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
056100930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
056200930803     C*================================================================
056300930803     C     OUTFUN        BEGSR
056400930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
056500930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
056600930803     C                   ENDDO
056700930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
056800930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
056900930803     C     1             DO        31            I
057000930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
057100930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
057200930803     C                   ADD       1             FUNNAT
057300930803     C                   ENDIF
057400930803     C                   ENDDO
057500930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
057600930803     C     FUNNAT        IFGT      8
057700930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
057800930803     C                   ENDIF
057900930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
058000930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
058100930803     C                   Z-ADD     8             FUNVIS            5 0
058200930803     C     FAT(23)       IFEQ      'S'
058300930803     C                   SUB       1             FUNVIS
058400930803     C                   ENDIF
058500930803     C     FAT(24)       IFEQ      'S'
058600930803     C                   SUB       1             FUNVIS
058700930803     C                   ENDIF
058800930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
058900930923     C                   Z-ADD     *ZEROS        W                 5 0
059000930803     C                   ADD       1             FUNULT
059100930803     C     FUNULT        IFGT      31
059200930803     C                   Z-ADD     1             FUNULT
059300930805     C                   ENDIF
059400930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
059500930803     C     FUNULT        DO        31            I                              --- 1 -->
059600930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
059700930803     C     W             IFLT      FUNVIS                                       --- 2 -->
059800930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
059900930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
060000930803     C     FDE(I)        ANDNE     *BLANKS
060100930805     C     I             IFEQ      23                                           *TOLGO F23
060200930805     C     I             OREQ      24                                           *E F24
060300930803     C                   ELSE
060400930803     C                   ADD       1             W
060500930803     C                   MOVEL     FDE(I)        FUV(W)
060600930803     C                   ENDIF
060700930803     C                   ENDIF                                                  <-- 3 ---
060800930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
060900930803     C                   Z-ADD     I             FUNULT
061000930803     C                   ENDIF                                                  <-- 2 ---
061100930803     C                   ENDDO                                                  <-- 1 ---
061200930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
061300930806     C                   MOVEL     FUV(1)        VFD001
061400930806     C                   MOVEL     FUV(2)        VFD002
061500930806     C                   MOVEL     FUV(3)        VFD003
061600930806     C                   MOVEL     FUV(4)        VFD004
061700930806     C                   MOVEL     FUV(5)        VFD005
061800930806     C                   MOVEL     FUV(6)        VFD006
061900930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
062000930803     C     FAT(24)       ANDEQ     'N'
062100930806     C                   MOVEL     FUV(7)        VFD007
062200930806     C                   MOVEL     FUV(8)        VFD008
062300930803     C                   ELSE                                                   <-- 1 -->
062400930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
062500930803     C     FAT(24)       ANDEQ     'S'
062600930806     C                   MOVEL     FDE(23)       VFD007
062700930806     C                   MOVEL     FDE(24)       VFD008
062800930803     C                   ELSE                                                   <-- 2 -->
062900930806     C                   MOVEL     FUV(7)        VFD007
063000930803     C     FAT(23)       IFEQ      'S'
063100930806     C                   MOVEL     FDE(23)       VFD008
063200930803     C                   ELSE
063300930806     C                   MOVEL     FDE(24)       VFD008
063400930805     C                   ENDIF
063500930805     C                   ENDIF                                                  <-- 2 ---
063600930805     C                   ENDIF                                                  <-- 1 ---
063700930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
063800930803     C                   ENDSR
063900930803     C*================================================================
064000930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
064100930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
064200930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
064300930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
064400930803     C*================================================================
064500930803     C     CONFUN        BEGSR
064600930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
064700930803     C     1             DO        31            I                              --- 1 -->
064800930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
064900930805     C* CONTROLLO SE IL TASTO E' ATTIVO
065000930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
065100930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
065200930803     C                   EXSR      CARMSG
065300930803     C                   MOVEL     '1'           $ERR
065400930803     C                   ENDIF                                                  <-- 3 ---
065500930803     C                   ENDIF                                                  <-- 2 ---
065600930803     C                   ENDDO                                                  <-- 1 ---
065700930803     C                   ENDSR
065800930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
065900950309TNSB37R- Tasto funzionale non valido.                                          1
066000950309TNSB37R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
066100950309TNSB37R- Opzione non valida.                                                   3
066200950309TNSB37R- Opzione abilitata ma non gestita. Avvertire CED.                      4
066300950309TNSB37R- Inizio elenco.                                                        5
066400950309TNSB37R- Fine elenco.                                                          6
066500950309TNSB37R- Data inizio errata.                                                   7
066600950309TNSB37R- Data fine errata.                                                     8
066700950309TNSB37R- Data inizio superiore alla data fine.                                 9
066800950309TNSB37R- Data inizio maggiore della data piu alta consentita.                 10
066900950309TNSB37R- Data fine maggiore della data piu alta consentita.                   11
067000950309TNSB37R- Codice filiale errato                                                12
067100950309TNSB37R- Codice area errato                                                   13
067200950309TNSB37R- Codice divisione errato                                              14
067300100412TNSB37R- Tab. '$3' non trovata. Impossibile continuare.                       15
067400100412TNSB37R- Tab. '$5' non trovata. Impossibile continuare.                       16
067500950309TNSB37R- 17                                                                   17
067600950309TNSB37R- 18                                                                   18
067700950309TNSB37R- Immettere solo una selezione per filiale, area o divisione           19
067800950309TNSB37R- 20                                                                   20
067900950309TNSB37R- 21                                                                   21
068000950309TNSB37R- 22                                                                   22
068100930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
068200930806F1    -                   -    NN     NN
068300930723F2    -                   -    NN     NN
068400930921F3    -F3=Fine            -    SS     SS
068500930723F4    -                   -    NN     NN
068600930723F5    -                   -    NN     NN
068700931026F6    -F6=Lancio          -    SS     SS
068800930723F7    -                   -    NN     NN
068900930723F8    -                   -    NN     NN
069000930723F9    -                   -    NN     NN
069100930723F10   -                   -    NN     NN
069200930723F11   -                   -    NN     NN
069300931115F12   -F12=Ritorno        -    SS     SS
069400930723F13   -                   -    NN     NN
069500930723F14   -                   -    NN     NN
069600930723F15   -                   -    NN     NN
069700930723F16   -                   -    NN     NN
069800930723F17   -                   -    NN     NN
069900930723F18   -                   -    NN     NN
070000930723F19   -                   -    NN     NN
070100930723F20   -                   -    NN     NN
070200930804F21   -                   -    NN     NN
070300930927F22   -                   -    NN     NN
070400930723F23   -                   -    NN     NN
070500930803F24   -F24=Altri tasti    -    NN     NN
070600930921ENTER -                   -    SS     SS
070700930802ROLLUP-                   -    NN     NN
070800930802ROLLDN-                   -    NN     NN
070900930802HELP  -                   -    NN     NN
071000930802PRINT -                   -    NN     NN
071100930802BSPACE-                   -    NN     NN
071200930802CLEAR -                   -    NN     NN
