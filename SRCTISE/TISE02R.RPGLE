000100950713     H*------------------------------------------------------------------------*
000200950713     H*                LANCIO AUTOMATICO 2° BLOCCO STATISTICHE                 *
000300980403     H* NOTE: Non si può lanciare in prova perchè forza il s.informativo       *
000400950713     H*------------------------------------------------------------------------*
000500950713     H DECEDIT('0,') DATEDIT(*DMY.)
000600950713     F*------------------------------------------------------------------------*
000700950713     F* DATA BASE
000800950713     F*------------------------------------------------------------------------*
000900020118     FTISE02D   CF   E             WORKSTN INFDS(DEVDS1)
001000020118     F                                     SFILE(SE02DVM:MSGULT)
001100950713     D*------------------------------------------------------------------------*
001200950713     D* SCHIERE
001300950713     D*------------------------------------------------------------------------*
001400950713     D*-------------------
001500950713     D* MESSAGGI E FUNZIONI
001600950713     D*-------------------
001700950713     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001800950713     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900950713     D*-------------------
002000950713     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100950713     D*-------------------
002200950713     D FFU             S              6    DIM(31)                              *FUNZIONE
002300950713     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
002400950713     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500950713     D*-------------------
002600950713     D* TABELLA DESCRIZIONI A VIDEO
002700950713     D*-------------------
002800950713     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900950713     D*-------------------
003000950713     D* TABELLA TASTI FUNZIONALI PREMUTI
003100950713     D*-------------------
003200950713     D KEYS            S              1    DIM(40)
003300950713     D*------------------------------------------------------------------------*
003400950713     D* INPUT
003500950713     D*------------------------------------------------------------------------*
003600950713     D*-------------------
003700950713     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800950713     D*-------------------
003900950713     D DSFUN         E DS
004000950713     D  FUNRIG                 1     40
004100950713     D*-------------------
004200950713     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004300950713     D*-------------------
004400950713     D DSMSG         E DS
004500950713     D*-------------------
004600950713     D* DS PER GESTIONE TASTI FUNZIONALI
004700950713     D*-------------------
004800950713     D DSKEYS        E DS
004900950713     D  KEYRIG                 1     40
005000950713     D*-------------------
005100950713     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005200950713     D*-------------------
005300950713     D                SDS
005400950713     D  USER                 254    263
005500950713     D*-------------------
005600950713     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005700950713     D*-------------------
005800950713     D DEVDS1          DS
005900950713     D  KEY                  369    369
006000950713     D  CURSOR               370    371
006100950713     D*-------------------
006200950713     D* DS "XSRDA8"
006300950713     D*-------------------
006400950713     D WLBDA8          DS                  INZ
006500950713     D  G08DAT                 1      8  0
006600950713     D  G08INV                 9     16  0
006700950713     D  G08ERR                17     17
006800950713     D  G08TGI                18     22  0
006900990610     D*-------------------
007000990610     D* DS DATA GENERICA (8)
007100990610     D*-------------------
007200990610     D                 DS                  INZ
007300990610     D  DSANN                  1      4  0
007400990610     D  DSMES                  5      6  0
007500990610     D  DSGIO                  7      8  0
007600990610     D  DSDAT                  1      8  0
007700950713     D*-------------------
007800950713     D* ARCHITETTURA
007900950713     D*-------------------
008000950713     D KPJBA         E DS
008100980403     D  SIFPRV                98     98
008200950713     D*-------------------
008300020916     D* DS REPERIMENTO DATI UTENTE
008400020916     D*-------------------
008500020916     D TIBS34DS      E DS                                                       *Profili utente
008600020916     D DDATIUTE      E DS                                                       *Dati utente
008700020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800950713     D*-------------------
008900990610     D* DS  R I C A V I
009000950713     D*-------------------
009100950713     D PARRIC          DS                  INZ
009200990610     D  RICDAT                 1      6  0
009300990610     D  RICANI                 1      4  0
009400990610     D  RICMMI                 5      6  0
009500990610     D  RICANF                 7     10  0
009600990610     D  RICMMF                11     12  0
009700990610     D  RICDA2                 7     12  0
009800990610     D  RICDET                13     13
009900990610     D  RICFIL                14     16  0
010000990610     D  RICARE                17     19  0
010100990610     D  RICDIV                20     20
010200990610     D  RICSER                21     21
010300990610     D  RICPRO                22     22
010400990610     D  RICRNC                23     23
010500990610     D  RICFPC                24     24
010600990610     D  RICIOE                25     25
010700000707     D  RICPOS                26     26
010800980403     D***
010900980403     D* COSTANTI
011000980403     D***
011100980403     D LIB201          C                   CONST('GAITRA201 ')
011200950713     C*------------------------------------------------------------------------*
011300950713     C* MAIN LINE
011400950713     C*------------------------------------------------------------------------*
011500950713     C*
011600950713     C* CICLO FINO A CHE:
011700950713DO  1C     $FINE         DOUEQ     '1'
011800950713     C*
011900950713     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012000950713     C*
012100950713     C* CONTROLLA GESTIONE VIDEO
012200950713IF  2C     $GES          IFEQ      'V1'
012300950713     C                   EXSR      GESV1                                        *PARAMETRI
012400950713E   2C                   ENDIF
012500950713     C*
012600950713E   1C                   ENDDO
012700950713     C*
012800950713     C* FINE MAIN DEL PGM
012900021203     C                   MOVEL     WKNSIF        KNSIF
013000950713     C                   SETON                                        LR
013100950713     C*------------------------------------------------------------------------*
013200950713     C* GESV1 - GESTIONE VIDEO UNO
013300950713     C*------------------------------------------------------------------------*
013400950713     C     GESV1         BEGSR
013500950713     C*
013600950713IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
013700950713     C                   EXSR      CARV1                                        *CARICA VIDEO
013800950713E   1C                   ENDIF
013900950713     C*
014000950713IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
014100950713     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014200950713     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014300950713E   1C                   ENDIF
014400950713     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014500950713     C*
014600020118     C                   EXFMT     SE02DV1                                      *PARAMETRI
014700950713     C*
014800950713     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
014900950713     C*
015000950713IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015100950713SL  2C                   SELECT
015200950713     C     KEY           WHENEQ    ENTER
015300950713     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015400950713     C     KEY           WHENEQ    F6
015500950713     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
015600950713     C                   OTHER
015700950713     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015800950713E   2C                   ENDSL
015900950713E   1C                   ENDIF
016000950713     C*
016100950713     C                   ENDSR
016200950713     C*------------------------------------------------------------------------*
016300950713     C* CARV1 - CARICA VIDEO UNO
016400950713     C*------------------------------------------------------------------------*
016500950713     C     CARV1         BEGSR
016600950713     C*
016700950713     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016800950713     C*
016900950713     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017000950713     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017100950713     C*
017200020118     C                   WRITE     SE02DVT                                      *TESTATA
017300020118     C                   WRITE     SE02DVF                                      *FUNZIONI
017400950713     C*
017500950713     C                   ENDSR
017600950713     C*------------------------------------------------------------------------*
017700950713     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017800950713     C*------------------------------------------------------------------------*
017900950713     C     ENTV1         BEGSR
018000950713     C*
018100950713     C* CONTROLLA I CAMPI IMMESSI
018200950713     C                   EXSR      DECODI
018300950713     C*
018400950713     C                   ENDSR
018500950713     C*------------------------------------------------------------------------*
018600950713     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
018700950713     C*------------------------------------------------------------------------*
018800950713     C     F06GES        BEGSR
018900950713     C*
019000950713     C* CONTROLLA I CAMPI IMMESSI
019100950713     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
019200950713     C*
019300950713     C* NON CI SONO ERRORI
019400950713IF  1C     $ERR          IFEQ      '0'
019500950713     C*--------------------
019600950713     C* R I C A V I
019700950713     C*--------------------
019800980403     C                   MOVEL     LIB201        KNSIF
019900950713IF  2C     V1CRIC        IFEQ      '1'
020000950713     C                   EXSR      LANRIC
020100950713E   2C                   ENDIF
020200950713     C*
020300950713     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
020400950713E   1C                   ENDIF
020500950713     C*
020600950713     C                   ENDSR
020700950713     C*------------------------------------------------------------------------*
020800950713     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020900950713     C*------------------------------------------------------------------------*
021000950713     C     DECODI        BEGSR
021100950713     C*
021200950713     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
021300950713     C                   SETOFF                                       6061
021400950713     C                   SETOFF                                       63
021500950713DO  1C                   DO
021600980403     C*
021700980403     C* NON LANCIABILE DA MENU' DI PROVA
021800980403IF  2C     SIFPRV        IFEQ      'P'
021900980403     C                   MOVEL     '1'           $ERR
022000980403     C                   MOVEL     MSG(11)       $MSG
022100980403     C                   EXSR      CARMSG
022200980403E   2C                   ENDIF
022300950713     C*--------------------
022400950713     C* ANNO
022500950713     C*--------------------
022600950713IF  2C     V1CANN        IFEQ      *ZEROS
022700950713     C                   MOVEL     '1'           $ERR
022800950713     C                   MOVEL     MSG(8)        $MSG
022900950713     C                   EXSR      CARMSG
023000950713     C                   SETON                                        60
023100950713     C                   LEAVE
023200950713E   2C                   ENDIF
023300950713     C*--------------------
023400950713     C* MESE
023500950713     C*--------------------
023600950713     C                   MOVEL     V1CANN        ANNMES            6 0
023700950713     C                   MOVE      V1CMES        ANNMES
023800950713     C                   MOVEL     ANNMES        G08INV
023900950713     C                   MOVE      01            G08INV
024000950713     C                   Z-ADD     *ZEROS        G08DAT
024100950713     C                   MOVEL     '3'           G08ERR
024200950713     C                   CALL      'XSRDA8'
024300950713     C                   PARM                    WLBDA8
024400950713IF  2C     G08ERR        IFEQ      '1'
024500950713     C                   MOVEL     '1'           $ERR
024600950713     C                   MOVEL     MSG(9)        $MSG
024700950713     C                   EXSR      CARMSG
024800950713     C                   SETON                                        61
024900950713     C                   LEAVE
025000950713E   2C                   ENDIF
025100950713     C*--------------------
025200950713     C* SCELTA RICAVI
025300950713     C*--------------------
025400950713IF  2C     V1CRIC        IFNE      *BLANKS
025500950713     C     V1CRIC        ANDNE     '1'
025600950713     C                   MOVEL     '1'           $ERR
025700950713     C                   MOVEL     MSG(10)       $MSG
025800950713     C                   EXSR      CARMSG
025900950713     C                   SETON                                        62
026000950713     C                   LEAVE
026100950713E   2C                   ENDIF
026200950713     C*
026300950713E   1C                   ENDDO
026400950713     C*
026500950713     C                   ENDSR
026600950713     C*------------------------------------------------------------------------*
026700950713     C* LANRIC - LANCIA LA STATISTICA RICAVI
026800950713     C*------------------------------------------------------------------------*
026900950713     C     LANRIC        BEGSR
027000950713     C***
027100950713     C* 1° LANCIO
027200950713     C***
027300950713     C                   CLEAR                   KPJBU
027400990610     C                   Z-ADD     V1CANN        RICANI                         *ANNO INIZIALE
027500950713     C                   Z-ADD     01            RICMMI                         *MESE INIZIALE
027600990610     C                   Z-ADD     V1CANN        RICANF                         *ANNO FINALE
027700950713     C                   Z-ADD     V1CMES        RICMMF                         *MESE FINALE
027800950713     C                   MOVEL     'N'           RICDET                         *NO DETTAGLIO FIL/FIL
027900950713     C                   MOVEL     'S'           RICRNC                         *AGGIORNA NOTE E RETT
028000950713     C                   MOVEL     'S'           RICFPC                         *CREA FILE PC
028100950713     C                   MOVEL     'I'           RICIOE                         *DATI ITALIA
028200000707     C                   MOVEL     'N'           RICPOS                         *NO DATI POSTE
028300020118     C                   MOVEL     'SE56'        KCOAZ
028400950713     C                   MOVEL     PARRIC        KPJBU
028500950713     C                   CALL      'BCH10'
028600950713     C                   PARM                    KPJBA
028700950713     C***
028800950713     C* 2° LANCIO
028900950713     C***
029000950713     C                   MOVEL     'S'           RICFPC                         *CREA FILE PC
029100960110     C                   MOVEL     'M'           RICIOE                         *DATI IMPORT
029200020118     C                   MOVEL     'SE56'        KCOAZ
029300950713     C                   MOVEL     PARRIC        KPJBU
029400950713     C                   CALL      'BCH10'
029500950713     C                   PARM                    KPJBA
029600960110     C***
029700000830     C* 3° LANCIO
029800960110     C***
029900960110     C                   MOVEL     'S'           RICFPC                         *CREA FILE PC
030000960110     C                   MOVEL     'X'           RICIOE                         *DATI EXPORT
030100020118     C                   MOVEL     'SE56'        KCOAZ
030200960110     C                   MOVEL     PARRIC        KPJBU
030300960110     C                   CALL      'BCH10'
030400960110     C                   PARM                    KPJBA
030500000830     C***
030600000830     C* 4° LANCIO
030700000830     C***
030800000830     C                   MOVEL     'S'           RICFPC                         *CREA FILE PC
030900000830     C                   MOVEL     'I'           RICIOE                         *DATI ITALIA
031000000830     C                   MOVEL     'S'           RICPOS                         *SI DATI POSTE
031100020118     C                   MOVEL     'SE56'        KCOAZ
031200000830     C                   MOVEL     PARRIC        KPJBU
031300000830     C                   CALL      'BCH10'
031400000830     C                   PARM                    KPJBA
031500950713     C*
031600950713     C                   ENDSR
031700020916     C*--------------------------------------------------------------------------------------------*
031800020916     C* REPERISCE I DATI UTENTE
031900020916     C*--------------------------------------------------------------------------------------------*
032000020916     C     REPDATIUTE    BEGSR
032100020916     C*
032200020916     C* INIZIALIZZA VARIABILI DI WRK
032300020916     C                   CLEAR                   TIBS34DS
032400020916     C                   CLEAR                   AZUTEDS
032500020916     C                   CLEAR                   DDATIUTE
032600020916     C*
032700020916     C     *DTAARA       DEFINE    §azute        azuteds
032800020916     C     *DTAARA       DEFINE    §datiute      ddatiute
032900020916     C                   IN(E)     *DTAARA
033000020916if  1C                   IF        %Error
033100020916     c                   EVAL      I34Tla = 'L'
033200020916     C                   CALL      'TIBS34R'
033300020916     C                   PARM                    Tibs34Ds
033400020916     C                   IN        *DTAARA
033500020916e   1C                   ENDIF
033600020916     C*
033700020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033800020916     C                   MOVEL(P)  RSUT          DSFIRS           20
033900020916     C*
034000020916     C                   ENDSR
034100950713     C*------------------------------------------------------------------------*
034200950713     C* *INZSR  - ROUTINE INIZIALE
034300950713     C*------------------------------------------------------------------------*
034400950713     C     *INZSR        BEGSR
034500950713     C*
034600950713     C* RICEVIMENTO PARAMETRI
034700950713     C     *ENTRY        PLIST
034800950713     C                   PARM                    KPJBA
034900021203     C*
035000021203     C     *LIKE         DEFINE    KNSIF         WKNSIF
035100021203     C                   EVAL      WKNSIF = KNSIF
035200990610     C*
035300990610     C* REPERISCE LA DATA CORRENTE
035400990610     C     *LIKE         DEFINE    G08INV        DATCOR
035500990610     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
035600990610     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
035700990610     C                   Z-ADD     N8            G08DAT
035800990610     C                   Z-ADD     *ZEROS        G08INV
035900990610     C                   MOVEL     '0'           G08ERR
036000990610     C                   CALL      'XSRDA8'
036100990610     C                   PARM                    WLBDA8
036200990610     C                   Z-ADD     G08INV        DATCOR                         *DATA CORRENTE AA/M/G
036300950713     C*
036400950713     C* ESEGUE OPERAZIONI INIZIALI STANDARD
036500950713     C                   EXSR      INZSTD
036600950713     C*
036700950713     C* IMPOSTAZIONE INIZIALE VIDEO
036800950713     C                   EXSR      IMPVID
036900950713     C*
037000950713     C* EMETTO FORMATO VIDEO
037100950713     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
037200950713     C*
037300950713     C                   ENDSR
037400950713     C*------------------------------------------------------------------------*
037500950713     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
037600950713     C*------------------------------------------------------------------------*
037700950713     C     IMPVID        BEGSR
037800990610     C*--------------------
037900990610     C* ANNO/MESE (ANNO/MESE CORRENTE - 1)
038000990610     C*--------------------
038100990610     C                   Z-ADD     DATCOR        DSDAT
038200990610     C                   Z-ADD     DSANN         AAAA              4 0
038300990610     C                   Z-ADD     DSMES         MMMM              2 0
038400990610     C                   SUB       1             MMMM
038500990610IF  1C     MMMM          IFEQ      *ZEROS                                       *SE GENNAIO
038600990610     C                   Z-ADD     12            MMMM                           *IMPOSTO DICEMBRE
038700990610     C                   SUB       1             AAAA                           *DELL'ANNO PRECEDENTE
038800990610E   1C                   ENDIF
038900990610     C                   Z-ADD     AAAA          V1CANN
039000990610     C                   Z-ADD     MMMM          V1CMES
039100950713     C*--------------------
039200950713     C* SCELTA STATISTICHE
039300950713     C*--------------------
039400950713     C                   MOVEL     '1'           V1CRIC
039500950713     C*
039600950713     C                   ENDSR
039700950713     C*------------------------------------------------------------------------*
039800950713     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
039900950713     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040000950713     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
040100950713     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
040200950713     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
040300950713     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040400950713     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040500950713     C*------------------------------------------------------------------------*
040600950713     C* INZSTD - ROUTINE INIZIALE STANDARD
040700950713     C*------------------------------------------------------------------------*
040800950713     C     INZSTD        BEGSR
040900950713     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
041000950713     C                   CALL      'XKEYS'
041100950713     C                   PARM                    DSKEYS
041200950713     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
041300950713     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
041400950713     C     XMSGPA        PLIST
041500950713     C                   PARM                    DSMSMS
041600950713     C                   PARM                    DSMSPG
041700950713     C                   PARM                    DSMSKE
041800950713     C                   PARM                    DSMSER
041900950713     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
042000020118     C                   MOVEL     'TISE02R'     DSMSPG                         *NOME PGM CHIAMANTE
042100020118     C                   MOVEL     'TISE02R'     VMCQUE                         *CODA MESSAGGI DESTIN
042200950713     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
042300950713     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
042400950713     C* INIZIALIZZO FLAG CONTROLLO PGM
042500950713     C                   MOVEL     '0'           $ERR              1            *ERRORE
042600950713     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
042700950713     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
042800950713     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
042900950713     C                   MOVEL     '0'           $FINE             1            *FINE PGM
043000950713     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
043100950713     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
043200950713     C* LANCIO ROUTINE PER VERIFICARE SE
043300950713     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
043400950713     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
043500020916     C                   EXSR      REPDATIUTE
043600950713     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
043700950713     C                   BITOFF    '01234567'    POSMI1            1
043800950713     C                   BITOFF    '01234567'    POSMI2            1
043900950713     C                   BITOFF    '01234567'    POSMA1            1
044000950713     C                   BITOFF    '01234567'    POSMA2            1
044100950713     C                   BITON     '34'          POSMI1            1            *18
044200950713     C                   BITON     '7'           POSMI2            1            *01
044300950713     C                   BITON     '34'          POSMA1            1            *18
044400950713     C                   BITON     '0'           POSMA2            1            *80
044500950713     C                   MOVE      POSMI2        POSMIN            2
044600950713     C                   MOVEL     POSMI1        POSMIN
044700950713     C                   MOVE      POSMA2        POSMAX            2
044800950713     C                   MOVEL     POSMA1        POSMAX
044900950713     C                   ENDSR
045000950713     C*------------------------------------------------------------------------*
045100950713     C* FUNSTD - LANCIA SUBROUTINE STANDARD
045200950713     C*------------------------------------------------------------------------*
045300950713     C     FUNSTD        BEGSR
045400950713     C                   SELECT
045500950713     C     KEY           WHENEQ    F3
045600950713     C                   EXSR      F03STD
045700950713     C     KEY           WHENEQ    F12
045800950713     C                   EXSR      F12STD
045900950713     C     KEY           WHENEQ    F24
046000950713     C                   EXSR      F24STD
046100950713     C                   OTHER
046200950713     C                   MOVEL     MSG(2)        $MSG
046300950713     C                   EXSR      CARMSG
046400950713     C                   ENDSL
046500950713     C                   ENDSR
046600950713     C*------------------------------------------------------------------------*
046700950713     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
046800950713     C*------------------------------------------------------------------------*
046900950713     C     F03STD        BEGSR
047000950713     C*
047100950713     C* IMPOSTO FLAG DI FINE PROGRAMMA
047200950713     C                   MOVEL     '1'           $FINE
047300950713     C*
047400950713     C                   ENDSR
047500950713     C*------------------------------------------------------------------------*
047600950713     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
047700950713     C*------------------------------------------------------------------------*
047800950713     C     F12STD        BEGSR
047900950713     C*
048000950713     C* IMPOSTO FLAG DI FINE PROGRAMMA
048100950713     C                   MOVEL     '1'           $FINE
048200950713     C*
048300950713     C                   ENDSR
048400950713     C*------------------------------------------------------------------------*
048500950713     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
048600950713     C*------------------------------------------------------------------------*
048700950713     C     F24STD        BEGSR
048800950713     C*
048900950713     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
049000950713     C                   EXSR      OUTFUN
049100950713     C*
049200950713     C                   ENDSR
049300950713     C*------------------------------------------------------------------------*
049400950713     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
049500950713     C*          CODA MESSAGGI DEL PGM
049600950713     C*------------------------------------------------------------------------*
049700950713     C     CARMSG        BEGSR
049800950713     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
049900950713     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
050000950713     C     MSGULT        IFEQ      0
050100950713     C                   MOVEL     '1'           *IN40
050200020118     C                   WRITE     SE02DVK
050300950713     C                   MOVEL     '0'           *IN40
050400950713     C                   ENDIF
050500950713     C* INVIA MESSAGGIO ALLA CODA DEL PGM
050600950713     C                   MOVEL     $MSG          DSMSMS
050700950713     C                   CALL      'XMSG'        XMSGPA
050800950713     C     DSMSER        IFEQ      0
050900950713     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
051000950713     C                   ENDIF
051100950713     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
051200950713     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
051300020118     C                   WRITE     SE02DVM                                      *SFL MESS
051400950713     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
051500950713     C                   MOVEL     *BLANKS       $MSG
051600950713     C                   ENDSR
051700950713     C*------------------------------------------------------------------------*
051800950713     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
051900950713     C*------------------------------------------------------------------------*
052000950713     C     OUTMSG        BEGSR
052100950713     C* VISUALIZZAZIONE MESSAGGIO
052200950713     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
052300020118     C                   WRITE     SE02DVK                                      *CONTR MESS
052400950713     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
052500950713     C                   Z-ADD     *ZEROS        MSGULT
052600950713     C                   ENDSR
052700950713     C*------------------------------------------------------------------------*
052800950713     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
052900950713     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
053000950713     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
053100950713     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
053200950713     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
053300950713     C*------------------------------------------------------------------------*
053400950713     C     CARFUN        BEGSR
053500950713     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
053600950713     C     1             DO        31            I                 5 0          --- 1 -->
053700950713     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
053800950713     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
053900950713     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
054000020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
054100950713     C     $TGES         IFEQ      'G'                                          *GESTIONE
054200950713     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
054300950713     C                   ELSE                                                   *INTERROG.
054400950713     C                   MOVEL     FUNASI        FAT(I)
054500950713     C                   ENDIF
054600950713     C                   ELSE                                                   <-- 2 --> *FILIALE
054700950713     C     $TGES         IFEQ      'G'                                          *GESTIONE
054800950713     C                   MOVEL     FUNAFG        FAT(I)
054900950713     C                   ELSE                                                   *INTERROG.
055000950713     C                   MOVEL     FUNAFI        FAT(I)
055100950713     C                   ENDIF
055200950713     C                   ENDIF                                                  <-- 2 ---
055300950713     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
055400950713     C                   MOVEL     USER          WEDP              3            *PROFILO
055500950713     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
055600950713     C                   MOVEL     'S'           FAT(I)                         *ABILITO
055700950713     C                   ELSE
055800950713     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
055900950713     C                   ENDIF
056000950713     C                   ENDIF
056100950713     C                   ENDDO                                                  <-- 1 ---
056200950713     C                   ENDSR
056300950713     C*------------------------------------------------------------------------*
056400950713     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
056500950713     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
056600950713     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
056700950713     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
056800950713     C*------------------------------------------------------------------------*
056900950713     C     OUTFUN        BEGSR
057000950713     C     1             DO        8             I                              *PULIZIA FUNZIONI A
057100950713     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
057200950713     C                   ENDDO
057300950713     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
057400950713     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
057500950713     C     1             DO        31            I
057600950713     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
057700950713     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
057800950713     C                   ADD       1             FUNNAT
057900950713     C                   ENDIF
058000950713     C                   ENDDO
058100950713     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
058200950713     C     FUNNAT        IFGT      8
058300950713     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
058400950713     C                   ENDIF
058500950713     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
058600950713     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
058700950713     C                   Z-ADD     8             FUNVIS            5 0
058800950713     C     FAT(23)       IFEQ      'S'
058900950713     C                   SUB       1             FUNVIS
059000950713     C                   ENDIF
059100950713     C     FAT(24)       IFEQ      'S'
059200950713     C                   SUB       1             FUNVIS
059300950713     C                   ENDIF
059400950713     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
059500950713     C                   Z-ADD     *ZEROS        W                 5 0
059600950713     C                   ADD       1             FUNULT
059700950713     C     FUNULT        IFGT      31
059800950713     C                   Z-ADD     1             FUNULT
059900950713     C                   ENDIF
060000950713     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
060100950713     C     FUNULT        DO        31            I                              --- 1 -->
060200950713     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
060300950713     C     W             IFLT      FUNVIS                                       --- 2 -->
060400950713     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
060500950713     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
060600950713     C     FDE(I)        ANDNE     *BLANKS
060700950713     C     I             IFEQ      23                                           *TOLGO F23
060800950713     C     I             OREQ      24                                           *E F24
060900950713     C                   ELSE
061000950713     C                   ADD       1             W
061100950713     C                   MOVEL     FDE(I)        FUV(W)
061200950713     C                   ENDIF
061300950713     C                   ENDIF                                                  <-- 3 ---
061400950713     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
061500950713     C                   Z-ADD     I             FUNULT
061600950713     C                   ENDIF                                                  <-- 2 ---
061700950713     C                   ENDDO                                                  <-- 1 ---
061800950713     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
061900950713     C                   MOVEL     FUV(1)        VFD001
062000950713     C                   MOVEL     FUV(2)        VFD002
062100950713     C                   MOVEL     FUV(3)        VFD003
062200950713     C                   MOVEL     FUV(4)        VFD004
062300950713     C                   MOVEL     FUV(5)        VFD005
062400950713     C                   MOVEL     FUV(6)        VFD006
062500950713     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
062600950713     C     FAT(24)       ANDEQ     'N'
062700950713     C                   MOVEL     FUV(7)        VFD007
062800950713     C                   MOVEL     FUV(8)        VFD008
062900950713     C                   ELSE                                                   <-- 1 -->
063000950713     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
063100950713     C     FAT(24)       ANDEQ     'S'
063200950713     C                   MOVEL     FDE(23)       VFD007
063300950713     C                   MOVEL     FDE(24)       VFD008
063400950713     C                   ELSE                                                   <-- 2 -->
063500950713     C                   MOVEL     FUV(7)        VFD007
063600950713     C     FAT(23)       IFEQ      'S'
063700950713     C                   MOVEL     FDE(23)       VFD008
063800950713     C                   ELSE
063900950713     C                   MOVEL     FDE(24)       VFD008
064000950713     C                   ENDIF
064100950713     C                   ENDIF                                                  <-- 2 ---
064200950713     C                   ENDIF                                                  <-- 1 ---
064300950713     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
064400950713     C                   ENDSR
064500950713     C*------------------------------------------------------------------------*
064600950713     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
064700950713     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
064800950713     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
064900950713     C*                   - ERRORE SE FUNZIONE NON ATTIVA
065000950713     C*------------------------------------------------------------------------*
065100950713     C     CONFUN        BEGSR
065200950713     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
065300950713     C     1             DO        31            I                              --- 1 -->
065400950713     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
065500950713     C* CONTROLLO SE IL TASTO E' ATTIVO
065600950713     C     FAT(I)        IFNE      'S'                                          --- 3 -->
065700950713     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
065800950713     C                   EXSR      CARMSG
065900950713     C                   MOVEL     '1'           $ERR
066000950713     C                   ENDIF                                                  <-- 3 ---
066100950713     C                   ENDIF                                                  <-- 2 ---
066200950713     C                   ENDDO                                                  <-- 1 ---
066300950713     C                   ENDSR
066400950713     O*------------------------------------------------------------------------*
066500950713** ======== SCHIERA: MSG  (MESSAGGI)                       ================
066600020118TISE02R- Tasto funzionale non valido.                                          1
066700020118TISE02R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
066800020118TISE02R- Opzione non valida.                                                   3
066900020118TISE02R- Opzione abilitata ma non gestita. Avvertire CED.                      4
067000020118TISE02R- Inizio elenco.                                                        5
067100020118TISE02R- Fine elenco.                                                          6
067200020118TISE02R- Non si possono lanciare le statistiche di gruppo in SDI !!!           7
067300020118TISE02R- Anno errato                                                           8
067400020118TISE02R- Mese errato                                                           9
067500020118TISE02R- Scelta errata                                                        10
067600020118TISE02R- Impossibile lanciare questo programma da un s.informativo di prova   11
067700020118TISE02R- 12
067800020118TISE02R- 13
067900020118TISE02R- 14
068000020118TISE02R- 15
068100020118TISE02R- 16
068200020118TISE02R- 17
068300020118TISE02R- 18
068400020118TISE02R- 19
068500020118TISE02R- 20
068600950713** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
068700950713F1    -                   -    NN     NN
068800950713F2    -                   -    NN     NN
068900950713F3    -F3=Fine            -    SS     SS
069000950713F4    -                   -    NN     NN
069100950713F5    -                   -    NN     NN
069200950713F6    -F6=Lancio          -    SS     SS
069300950713F7    -                   -    NN     NN
069400950713F8    -                   -    NN     NN
069500950713F9    -                   -    NN     NN
069600950713F10   -                   -    NN     NN
069700950713F11   -                   -    NN     NN
069800950713F12   -F12-Ritorno        -    SS     SS
069900950713F13   -                   -    NN     NN
070000950713F14   -                   -    NN     NN
070100950713F15   -                   -    NN     NN
070200950713F16   -                   -    NN     NN
070300950713F17   -                   -    NN     NN
070400950713F18   -                   -    NN     NN
070500950713F19   -                   -    NN     NN
070600950713F20   -                   -    NN     NN
070700950713F21   -                   -    NN     NN
070800950713F22   -                   -    NN     NN
070900950713F23   -                   -    NN     NN
071000950713F24   -F24=Altri tasti    -    NN     NN
071100950713ENTER -                   -    SS     SS
071200950713ROLLUP-                   -    NN     NN
071300950713ROLLDN-                   -    NN     NN
071400950713HELP  -                   -    NN     NN
071500950713PRINT -                   -    NN     NN
071600950713BSPACE-                   -    NN     NN
071700950713CLEAR -                   -    NN     NN
