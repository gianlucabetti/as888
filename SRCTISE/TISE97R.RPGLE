000100000000     H*------------------------------------------------------------------------*
000200960822     H* IMPORTAZIONE BUDGET DI FILIALE DA PC- RICHIESTA PARAMETRI              *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800020115     FTISE97D   CF   E             WORKSTN INFDS(DEVDS1)
000900020115     F                                     SFILE(SE97DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001400000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001600000000     D FFU             S              6    DIM(31)                              *FUNZIONE
001700000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
001800000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
001900000000     D* TABELLA DESCRIZIONI A VIDEO
002000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002200000000     D KEYS            S              1    DIM(40)
002300000000     D*------------------------------------------------------------------------*
002400000000     D* INPUT
002500000000     D*------------------------------------------------------------------------*
002600000000     D*-------------
002700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002800000000     D*-------------
002900000000     D DSFUN         E DS
003000000000     D  FUNRIG                 1     40
003100000000     D*-------------
003200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003300000000     D*-------------
003400000000     D DSMSG         E DS
003500000000     D*-------------
003600020916     D* DS REPERIMENTO DATI UTENTE
003700020916     D*-------------------
003800020916     D TIBS34DS      E DS                                                       *Profili utente
003900020916     D DDATIUTE      E DS                                                       *Dati utente
004000020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004100000000     D*-------------
004200000000     D* DS PER GESTIONE TASTI FUNZIONALI
004300000000     D*-------------
004400000000     D DSKEYS        E DS
004500000000     D  KEYRIG                 1     40
004600000000     D*-------------
004700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800000000     D*-------------
004900000000     D                SDS
005000000000     D  USER                 254    263                                         UTENTE
005100000000     D*-------------
005200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300000000     D*-------------
005400000000     D DEVDS1          DS
005500000000     D* CHIAVE FUNZIONALE BATTUTA
005600000000     D  KEY                  369    369
005700000000     D* POSIZIONE CURSORE (IN HEX)
005800000000     D  CURSOR               370    371
005900000000     D*-------------
006000000000     D* ARCHITETTURA
006100000000     D*-------------
006200000000     D KPJBA         E DS
006300000000     D*-------------
006400000000     D* PARAMETRI DI PASSAGGIO BATCH
006500000000     D*-------------
006600000000     D PARAM           DS                  INZ
006700960108     D  PARANN                 1      4  0                                      ANNO
006800960108     D  PARAGG                 5      5                                         AGGIOR
006900000000     C*------------------------------------------------------------------------*
007000000000     C* MAIN LINE
007100000000     C*------------------------------------------------------------------------*
007200000000     C*
007300000000     C* CICLO FINO A CHE:
007400000000DO  1C     $FINE         DOUEQ     '1'
007500000000     C*
007600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
007700000000     C*
007800000000     C* CONTROLLA GESTIONE VIDEO
007900000000IF  2C     $GES          IFEQ      'V1'
008000000000     C                   EXSR      GESV1                                        *PARAMETRI
008100000000E   2C                   ENDIF
008200000000     C*
008300000000E   1C                   ENDDO
008400000000     C*
008500000000     C* FINE MAIN DEL PGM
008600000000     C                   SETON                                        LR
008700000000     C*------------------------------------------------------------------------*
008800000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
008900000000     C*------------------------------------------------------------------------*
009000000000     C     GESV1         BEGSR
009100000000     C*
009200000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
009300000000     C                   EXSR      CARV1                                        *CARICA VIDEO
009400000000E   1C                   ENDIF
009500000000     C*
009600000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
009700000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
009800000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
009900000000E   1C                   ENDIF
010000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010100000000     C*
010200020115     C                   EXFMT     SE97DV1                                      *PARAMETRI
010300000000     C*
010400000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
010500000000     C*
010600000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
010700000000SL  2C                   SELECT
010800000000     C     KEY           WHENEQ    ENTER
010900000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011000000000     C     KEY           WHENEQ    F6
011100000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
011200000000     C                   OTHER
011300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
011400000000E   2C                   ENDSL
011500000000E   1C                   ENDIF
011600000000     C*
011700000000     C                   ENDSR
011800000000     C*------------------------------------------------------------------------*
011900000000     C* CARV1 - CARICA VIDEO 'PARAMETRI'
012000000000     C*------------------------------------------------------------------------*
012100000000     C     CARV1         BEGSR
012200000000     C*
012300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
012400000000     C*
012500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012700000000     C*
012800020115     C                   WRITE     SE97DVT                                      *TESTATA
012900020115     C                   WRITE     SE97DVF                                      *FUNZIONI
013000000000     C*
013100000000     C                   ENDSR
013200000000     C*------------------------------------------------------------------------*
013300000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
013400000000     C*------------------------------------------------------------------------*
013500000000     C     ENTV1         BEGSR
013600000000     C*
013700000000     C* CONTROLLA I CAMPI IMMESSI
013800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*------------------------------------------------------------------------*
014200000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
014300000000     C*------------------------------------------------------------------------*
014400000000     C     F06GES        BEGSR
014500000000     C*
014600000000     C* CONTROLLA I CAMPI IMMESSI
014700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
014800000000     C*
014900000000     C* NON CI SONO ERRORI
015000000000IF  1C     $ERR          IFEQ      '0'
015100000000     C*
015200000000     C* LANCIO PROGRAMMA DI STAMPA
015300000000     C                   Z-ADD     V1CANN        PARANN                         *ANNO DA CARICARE
015400000000     C                   MOVEL     V1CAGG        PARAGG                         *AGGIORNAMENTO (S/N)
015500000000     C*
015600020115     C                   MOVEL     'SE98'        KCOAZ
015700000000     C                   MOVEL     PARAM         KPJBU
015800000000     C                   CALL      'BCH10'
015900000000     C                   PARM                    KPJBA
016000000000     C*
016100000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
016200000000E   1C                   ENDIF
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
016700000000     C*------------------------------------------------------------------------*
016800000000     C     DECODI        BEGSR
016900000000     C*
017000000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
017100000000     C                   SETOFF                                       6061
017200000000     C*
017300000000     C* CONTROLLO ANNO OBBLIGATORIO
017400000000IF  1C     V1CANN        IFEQ      *ZEROS
017500000000     C                   MOVEL     '1'           $ERR
017600000000     C                   MOVEL     MSG(7)        $MSG
017700000000     C                   EXSR      CARMSG
017800000000     C                   SETON                                        60
017900000000E   1C                   ENDIF
018000000000     C*
018100000000     C* CONTROLLO FLAG AGGIORNAMENTO
018200000000IF  1C     $ERR          IFEQ      '0'
018300000000IF  2C     V1CAGG        IFEQ      'S'
018400000000     C     V1CAGG        OREQ      'N'
018500000000X   2C                   ELSE
018600000000     C                   MOVEL     '1'           $ERR
018700000000     C                   MOVEL     MSG(8)        $MSG
018800000000     C                   EXSR      CARMSG
018900000000     C                   SETON                                        61
019000000000E   2C                   ENDIF
019100000000E   1C                   ENDIF
019200000000     C*
019300000000     C                   ENDSR
019400020916     C*--------------------------------------------------------------------------------------------*
019500020916     C* REPERISCE I DATI UTENTE
019600020916     C*--------------------------------------------------------------------------------------------*
019700020916     C     REPDATIUTE    BEGSR
019800020916     C*
019900020916     C* INIZIALIZZA VARIABILI DI WRK
020000020916     C                   CLEAR                   TIBS34DS
020100020916     C                   CLEAR                   AZUTEDS
020200020916     C                   CLEAR                   DDATIUTE
020300020916     C*
020400020916     C     *DTAARA       DEFINE    §azute        azuteds
020500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
020600020916     C                   IN(E)     *DTAARA
020700020916if  1C                   IF        %Error
020800020916     c                   EVAL      I34Tla = 'L'
020900020916     C                   CALL      'TIBS34R'
021000020916     C                   PARM                    Tibs34Ds
021100020916     C                   IN        *DTAARA
021200020916e   1C                   ENDIF
021300020916     C*
021400020916     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* *INZSR  - ROUTINE INIZIALE
021700000000     C*------------------------------------------------------------------------*
021800000000     C     *INZSR        BEGSR
021900000000     C*
022000000000     C* RICEVIMENTO PARAMETRI
022100000000     C     *ENTRY        PLIST
022200000000     C                   PARM                    KPJBA
022300000000     C*
022400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
022500000000     C                   EXSR      INZSTD
022600000000     C*
022700000000     C* IMPOSTAZIONI A VIDEO
022800020916     C                   MOVEL     RSUT          VTDRSU                         *RAGIONE SOC UTENTE
022900960108     C                   MOVE      UYEAR         V1CANN                         *ANNO CORRENTE
023000960108IF  1C     UYEAR         IFGT      60
023100960108     C                   MOVEL     19            V1CANN                         *SECOLO
023200960108X   1C                   ELSE
023300960108     C                   MOVEL     20            V1CANN                         *SECOLO
023400960108E   1C                   ENDIF
023500000000     C                   MOVEL     'N'           V1CAGG                         *AGGIORAMENTO
023600000000     C*
023700000000     C* EMETTO FORMATO VIDEO
023800000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
023900000000     C*
024000000000     C                   ENDSR
024100000000     C*------------------------------------------------------------------------*
024200000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024300000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024400000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
024500000000     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
024600000000     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
024700000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024800000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024900000000     C*------------------------------------------------------------------------*
025000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
025100000000     C*------------------------------------------------------------------------*
025200000000     C     INZSTD        BEGSR
025300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
025400000000     C                   CALL      'XKEYS'
025500000000     C                   PARM                    DSKEYS
025600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
025700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
025800000000     C     XMSGPA        PLIST
025900000000     C                   PARM                    DSMSMS
026000000000     C                   PARM                    DSMSPG
026100000000     C                   PARM                    DSMSKE
026200000000     C                   PARM                    DSMSER
026300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
026400020115     C                   MOVEL     'TISE97R'     DSMSPG                         *NOME PGM CHIAMANTE
026500020115     C                   MOVEL     'TISE97R'     VMCQUE                         *CODA MESSAGGI DESTIN
026600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
026700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
026800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
026900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
027000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
027100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
027200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
027300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
027400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
027500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
027600000000     C* LANCIO ROUTINE PER VERIFICARE SE
027700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
027800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
027900020916     C                   EXSR      REPDATIUTE
028000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
028100000000     C                   BITOFF    '01234567'    POSMI1            1
028200000000     C                   BITOFF    '01234567'    POSMI2            1
028300000000     C                   BITOFF    '01234567'    POSMA1            1
028400000000     C                   BITOFF    '01234567'    POSMA2            1
028500000000     C                   BITON     '34'          POSMI1            1            *18
028600000000     C                   BITON     '7'           POSMI2            1            *01
028700000000     C                   BITON     '34'          POSMA1            1            *18
028800000000     C                   BITON     '0'           POSMA2            1            *80
028900000000     C                   MOVE      POSMI2        POSMIN            2
029000000000     C                   MOVEL     POSMI1        POSMIN
029100000000     C                   MOVE      POSMA2        POSMAX            2
029200000000     C                   MOVEL     POSMA1        POSMAX
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
029600000000     C*------------------------------------------------------------------------*
029700000000     C     FUNSTD        BEGSR
029800000000     C                   SELECT
029900000000     C     KEY           WHENEQ    F3
030000000000     C                   EXSR      F03STD
030100000000     C     KEY           WHENEQ    F12
030200000000     C                   EXSR      F12STD
030300000000     C     KEY           WHENEQ    F24
030400000000     C                   EXSR      F24STD
030500000000     C                   OTHER
030600000000     C                   MOVEL     MSG(2)        $MSG
030700000000     C                   EXSR      CARMSG
030800000000     C                   ENDSL
030900000000     C                   ENDSR
031000000000     C*------------------------------------------------------------------------*
031100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
031200000000     C*------------------------------------------------------------------------*
031300000000     C     F03STD        BEGSR
031400000000     C*
031500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
031600000000     C                   MOVEL     '1'           $FINE
031700000000     C*
031800000000     C                   ENDSR
031900000000     C*------------------------------------------------------------------------*
032000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
032100000000     C*------------------------------------------------------------------------*
032200000000     C     F12STD        BEGSR
032300000000     C*
032400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
032500000000     C                   MOVEL     '1'           $FINE
032600000000     C*
032700000000     C                   ENDSR
032800000000     C*------------------------------------------------------------------------*
032900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
033000000000     C*------------------------------------------------------------------------*
033100000000     C     F24STD        BEGSR
033200000000     C*
033300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
033400000000     C                   EXSR      OUTFUN
033500000000     C*
033600000000     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
033900000000     C*          CODA MESSAGGI DEL PGM
034000000000     C*------------------------------------------------------------------------*
034100000000     C     CARMSG        BEGSR
034200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
034300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
034400000000     C     MSGULT        IFEQ      0
034500000000     C                   MOVEL     '1'           *IN40
034600020115     C                   WRITE     SE97DVK
034700000000     C                   MOVEL     '0'           *IN40
034800000000     C                   ENDIF
034900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
035000000000     C                   MOVEL     $MSG          DSMSMS
035100000000     C                   CALL      'XMSG'        XMSGPA
035200000000     C     DSMSER        IFEQ      0
035300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
035400000000     C                   ENDIF
035500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
035600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
035700020115     C                   WRITE     SE97DVM                                      *SFL MESS
035800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
035900000000     C                   MOVEL     *BLANKS       $MSG
036000000000     C                   ENDSR
036100000000     C*------------------------------------------------------------------------*
036200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
036300000000     C*------------------------------------------------------------------------*
036400000000     C     OUTMSG        BEGSR
036500000000     C* VISUALIZZAZIONE MESSAGGIO
036600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
036700020115     C                   WRITE     SE97DVK                                      *CONTR MESS
036800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
036900000000     C                   Z-ADD     *ZEROS        MSGULT
037000000000     C                   ENDSR
037100000000     C*------------------------------------------------------------------------*
037200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
037300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
037400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
037500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
037600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
037700000000     C*------------------------------------------------------------------------*
037800000000     C     CARFUN        BEGSR
037900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
038000000000     C     1             DO        31            I                 5 0          --- 1 -->
038100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
038200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
038300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
038400020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
038500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
038600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
038700000000     C                   ELSE                                                   *INTERROG.
038800000000     C                   MOVEL     FUNASI        FAT(I)
038900000000     C                   ENDIF
039000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
039100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
039200000000     C                   MOVEL     FUNAFG        FAT(I)
039300000000     C                   ELSE                                                   *INTERROG.
039400000000     C                   MOVEL     FUNAFI        FAT(I)
039500000000     C                   ENDIF
039600000000     C                   ENDIF                                                  <-- 2 ---
039700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
039800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
039900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
040000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
040100000000     C                   ELSE
040200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
040300000000     C                   ENDIF
040400000000     C                   ENDIF
040500000000     C                   ENDDO                                                  <-- 1 ---
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
040900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
041000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
041100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
041200000000     C*------------------------------------------------------------------------*
041300000000     C     OUTFUN        BEGSR
041400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
041500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
041600000000     C                   ENDDO
041700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
041800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
041900000000     C     1             DO        31            I
042000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
042100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
042200000000     C                   ADD       1             FUNNAT
042300000000     C                   ENDIF
042400000000     C                   ENDDO
042500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
042600000000     C     FUNNAT        IFGT      8
042700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
042800000000     C                   ENDIF
042900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
043000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
043100000000     C                   Z-ADD     8             FUNVIS            5 0
043200000000     C     FAT(23)       IFEQ      'S'
043300000000     C                   SUB       1             FUNVIS
043400000000     C                   ENDIF
043500000000     C     FAT(24)       IFEQ      'S'
043600000000     C                   SUB       1             FUNVIS
043700000000     C                   ENDIF
043800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
043900000000     C                   Z-ADD     *ZEROS        W                 5 0
044000000000     C                   ADD       1             FUNULT
044100000000     C     FUNULT        IFGT      31
044200000000     C                   Z-ADD     1             FUNULT
044300000000     C                   ENDIF
044400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
044500000000     C     FUNULT        DO        31            I                              --- 1 -->
044600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
044700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
044800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
044900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
045000000000     C     FDE(I)        ANDNE     *BLANKS
045100000000     C     I             IFEQ      23                                           *TOLGO F23
045200000000     C     I             OREQ      24                                           *E F24
045300000000     C                   ELSE
045400000000     C                   ADD       1             W
045500000000     C                   MOVEL     FDE(I)        FUV(W)
045600000000     C                   ENDIF
045700000000     C                   ENDIF                                                  <-- 3 ---
045800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
045900000000     C                   Z-ADD     I             FUNULT
046000000000     C                   ENDIF                                                  <-- 2 ---
046100000000     C                   ENDDO                                                  <-- 1 ---
046200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
046300000000     C                   MOVEL     FUV(1)        VFD001
046400000000     C                   MOVEL     FUV(2)        VFD002
046500000000     C                   MOVEL     FUV(3)        VFD003
046600000000     C                   MOVEL     FUV(4)        VFD004
046700000000     C                   MOVEL     FUV(5)        VFD005
046800000000     C                   MOVEL     FUV(6)        VFD006
046900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
047000000000     C     FAT(24)       ANDEQ     'N'
047100000000     C                   MOVEL     FUV(7)        VFD007
047200000000     C                   MOVEL     FUV(8)        VFD008
047300000000     C                   ELSE                                                   <-- 1 -->
047400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
047500000000     C     FAT(24)       ANDEQ     'S'
047600000000     C                   MOVEL     FDE(23)       VFD007
047700000000     C                   MOVEL     FDE(24)       VFD008
047800000000     C                   ELSE                                                   <-- 2 -->
047900000000     C                   MOVEL     FUV(7)        VFD007
048000000000     C     FAT(23)       IFEQ      'S'
048100000000     C                   MOVEL     FDE(23)       VFD008
048200000000     C                   ELSE
048300000000     C                   MOVEL     FDE(24)       VFD008
048400000000     C                   ENDIF
048500000000     C                   ENDIF                                                  <-- 2 ---
048600000000     C                   ENDIF                                                  <-- 1 ---
048700000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
048800000000     C                   ENDSR
048900000000     C*------------------------------------------------------------------------*
049000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
049100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
049200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
049300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
049400000000     C*------------------------------------------------------------------------*
049500000000     C     CONFUN        BEGSR
049600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
049700000000     C     1             DO        31            I                              --- 1 -->
049800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
049900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
050000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
050100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
050200000000     C                   EXSR      CARMSG
050300000000     C                   MOVEL     '1'           $ERR
050400000000     C                   ENDIF                                                  <-- 3 ---
050500000000     C                   ENDIF                                                  <-- 2 ---
050600000000     C                   ENDDO                                                  <-- 1 ---
050700000000     C                   ENDSR
050800000000     O*------------------------------------------------------------------------*
050900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
051000020115TISE97R- Tasto funzionale non valido.                                          1
051100020115TISE97R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
051200020115TISE97R- Opzione non valida.                                                   3
051300020115TISE97R- Opzione abilitata ma non gestita. Avvertire CED.                      4
051400020115TISE97R- Inizio elenco.                                                        5
051500020115TISE97R- Fine elenco.                                                          6
051600020115TISE97R- Anno da caricare obbligatorio                                         7
051700020115TISE97R- 'S' per aggiornare i dati già esistenti, 'N' per non aggiornarli      8
051800020115TISE97R- 9
051900020115TISE97R- 10
052000020115TISE97R- 11
052100020115TISE97R- 12
052200020115TISE97R- 13
052300020115TISE97R- 14
052400020115TISE97R- 15
052500020115TISE97R- 16
052600020115TISE97R- 17
052700020115TISE97R- 18
052800020115TISE97R- 19
052900020115TISE97R- 20
053000000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
053100000000F1    -                   -    NN     NN
053200000000F2    -                   -    NN     NN
053300000000F3    -F3=Fine            -    SS     SS
053400000000F4    -                   -    NN     NN
053500000000F5    -                   -    NN     NN
053600000000F6    -F6=Lancio          -    SS     SS
053700000000F7    -                   -    NN     NN
053800000000F8    -                   -    NN     NN
053900000000F9    -                   -    NN     NN
054000000000F10   -                   -    NN     NN
054100000000F11   -                   -    NN     NN
054200000000F12   -F12-Ritorno        -    SS     SS
054300000000F13   -                   -    NN     NN
054400000000F14   -                   -    NN     NN
054500000000F15   -                   -    NN     NN
054600000000F16   -                   -    NN     NN
054700000000F17   -                   -    NN     NN
054800000000F18   -                   -    NN     NN
054900000000F19   -                   -    NN     NN
055000000000F20   -                   -    NN     NN
055100000000F21   -                   -    NN     NN
055200000000F22   -                   -    NN     NN
055300000000F23   -                   -    NN     NN
055400000000F24   -F24=Altri tasti    -    NN     NN
055500000000ENTER -                   -    SS     SS
055600000000ROLLUP-                   -    NN     NN
055700000000ROLLDN-                   -    NN     NN
055800000000HELP  -                   -    NN     NN
055900000000PRINT -                   -    NN     NN
056000000000BSPACE-                   -    NN     NN
056100000000CLEAR -                   -    NN     NN
