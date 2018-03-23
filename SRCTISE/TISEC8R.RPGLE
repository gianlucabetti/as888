000100960903     H*------------------------------------------------------------------------*
000200960903     H* IMPORTAZIONE BUDGET COMMERCIALE DA PC - RICHIESTA PARAMETRI            *
000300960903     H*------------------------------------------------------------------------*
000400960903     H DECEDIT('0,') DATEDIT(*DMY.)
000500960903     F*------------------------------------------------------------------------*
000600960903     F* DATA BASE
000700960903     F*------------------------------------------------------------------------*
000800020114     FTISEC8D   CF   E             WORKSTN INFDS(DEVDS1)
000900020114     F                                     SFILE(SEC8DVM:MSGULT)
001000960903     D*------------------------------------------------------------------------*
001100960903     D* SCHIERE
001200960903     D*------------------------------------------------------------------------*
001300960903     D*-------------
001400960903     D* MESSAGGI E FUNZIONI
001500960903     D*-------------
001600960903     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700960903     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800960903     D*-------------
001900960903     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000960903     D*-------------
002100960903     D FFU             S              6    DIM(31)                              *FUNZIONE
002200960903     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
002300960903     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400960903     D*-------------
002500960903     D* TABELLA DESCRIZIONI A VIDEO
002600960903     D*-------------
002700960903     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800960903     D*-------------
002900960903     D* TABELLA TASTI FUNZIONALI PREMUTI
003000960903     D*-------------
003100960903     D KEYS            S              1    DIM(40)
003200960903     D*------------------------------------------------------------------------*
003300960903     D* INPUT
003400960903     D*------------------------------------------------------------------------*
003500960903     D*-------------
003600960903     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700960903     D*-------------
003800960903     D DSFUN         E DS
003900960903     D  FUNRIG                 1     40
004000960903     D*-------------
004100960903     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200960903     D*-------------
004300960903     D DSMSG         E DS
004400960903     D*-------------
004500960903     D* DS PER GESTIONE TASTI FUNZIONALI
004600960903     D*-------------
004700960903     D DSKEYS        E DS
004800960903     D  KEYRIG                 1     40
004900960903     D*-------------
005000960903     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100960903     D*-------------
005200960903     D                SDS
005300960903     D  USER                 254    263
005400960903     D*-------------
005500960903     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600960903     D*-------------
005700960903     D DEVDS1          DS
005800960903     D  KEY                  369    369
005900960903     D  CURSOR               370    371
006000960903     D*-------------
006100960903     D* ARCHITETTURA
006200960903     D*-------------
006300960903     D KPJBA         E DS
006400960903     D*-------------
006500020916     D* DS REPERIMENTO DATI UTENTE
006600020916     D*-------------------
006700020916     D TIBS34DS      E DS                                                       *Profili utente
006800020916     D DDATIUTE      E DS                                                       *Dati utente
006900020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000960903     D*-------------
007100960903     D* PARAMETRI DI LANCIO
007200960903     D*-------------
007300960903     D PARAM           DS                  INZ
007400960903     D  PARANN                 1      4  0
007500960903     D  PARAGG                 5      5
007600960903     C*------------------------------------------------------------------------*
007700960903     C* MAIN LINE
007800960903     C*------------------------------------------------------------------------*
007900960903     C*
008000960903     C* CICLO FINO A CHE:
008100960903DO  1C     $FINE         DOUEQ     '1'
008200960903     C*
008300960903     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008400960903     C*
008500960903     C* CONTROLLA GESTIONE VIDEO
008600960903IF  2C     $GES          IFEQ      'V1'
008700960903     C                   EXSR      GESV1                                        *PARAMETRI
008800960903E   2C                   ENDIF
008900960903     C*
009000960903E   1C                   ENDDO
009100960903     C*
009200960903     C                   SETON                                        LR
009300960903     C*------------------------------------------------------------------------*
009400960903     C* GESV1 - GESTIONE VIDEO UNO
009500960903     C*------------------------------------------------------------------------*
009600960903     C     GESV1         BEGSR
009700960903     C*
009800960903IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
009900960903     C                   EXSR      CARV1                                        *CARICA VIDEO
010000960903E   1C                   ENDIF
010100960903     C*
010200960903IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
010300960903     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010400960903     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010500960903E   1C                   ENDIF
010600960903     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010700960903     C*
010800020114     C                   EXFMT     SEC8DV1                                      *PARAMETRI
010900960903     C*
011000960903     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
011100960903     C*
011200960903IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011300960903SL  2C                   SELECT
011400960903     C     KEY           WHENEQ    ENTER
011500960903     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011600960903     C     KEY           WHENEQ    F6
011700960903     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
011800960903     C                   OTHER
011900960903     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012000960903E   2C                   ENDSL
012100960903E   1C                   ENDIF
012200960903     C*
012300960903     C                   ENDSR
012400960903     C*------------------------------------------------------------------------*
012500960903     C* CARV1 - CARICA VIDEO UNO
012600960903     C*------------------------------------------------------------------------*
012700960903     C     CARV1         BEGSR
012800960903     C*
012900960903     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013000960903     C*
013100960903     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013200960903     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013300960903     C*
013400020114     C                   WRITE     SEC8DVT                                      *TESTATA
013500020114     C                   WRITE     SEC8DVF                                      *FUNZIONI
013600960903     C*
013700960903     C                   ENDSR
013800960903     C*------------------------------------------------------------------------*
013900960903     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
014000960903     C*------------------------------------------------------------------------*
014100960903     C     ENTV1         BEGSR
014200960903     C*
014300960903     C* CONTROLLA I CAMPI IMMESSI
014400960903     C                   EXSR      DECODI
014500960903     C*
014600960903     C                   ENDSR
014700960903     C*------------------------------------------------------------------------*
014800960903     C* F06GES - GESTIONE TASTO F06 ==> CONFERMA LANCIO
014900960903     C*------------------------------------------------------------------------*
015000960903     C     F06GES        BEGSR
015100960903     C*
015200960903     C* CONTROLLA I CAMPI IMMESSI
015300960903     C                   EXSR      DECODI
015400960903     C*
015500960903     C* LANCIO PROGRAMMA DI STAMPA
015600960903IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
015700960903     C                   Z-ADD     V1CANN        PARANN                         *ANNO
015800960903     C                   MOVEL     V1CAGG        PARAGG                         *FLAG AGGIORNAMENTO
015900960903     C*
016000020114     C                   MOVEL     'SEC9'        KCOAZ
016100960903     C                   MOVEL     PARAM         KPJBU
016200960903     C                   CALL      'BCH10'
016300960903     C                   PARM                    KPJBA
016400960903     C*
016500960903     C                   MOVEL     '1'           $FINE                          *USCITA PGM
016600960903E   1C                   ENDIF
016700960903     C*
016800960903     C                   ENDSR
016900960903     C*------------------------------------------------------------------------*
017000960903     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017100960903     C*------------------------------------------------------------------------*
017200960903     C     DECODI        BEGSR
017300960903     C*
017400960903     C* REIMPOSTA INDICATORI DI ERRORE SUI CAMPI
017500960903     C                   SETOFF                                       6061
017600960903     C*--------------------
017700960903     C* ANNO DA CARICARE
017800960903     C*--------------------
017900960903IF  1C     $ERR          IFEQ      '0'
018000960903IF  2C     V1CANN        IFEQ      *ZEROS
018100960903     C                   MOVEL     '1'           $ERR
018200960903     C                   MOVEL     MSG(7)        $MSG
018300960903     C                   EXSR      CARMSG
018400960903     C                   SETON                                        60
018500960903E   2C                   ENDIF
018600960903E   1C                   ENDIF
018700960903     C*--------------------
018800960903     C* FLAG AGGIORNAMENTO
018900960903     C*--------------------
019000960903IF  1C     $ERR          IFEQ      '0'
019100960903IF  2C     V1CAGG        IFEQ      'S'
019200960903     C     V1CAGG        OREQ      'N'
019300960903X   2C                   ELSE
019400960903     C                   MOVEL     '1'           $ERR
019500960903     C                   MOVEL     MSG(8)        $MSG
019600960903     C                   EXSR      CARMSG
019700960903     C                   SETON                                        61
019800960903E   2C                   ENDIF
019900960903E   1C                   ENDIF
020000960903     C*
020100960903     C                   ENDSR
020200960903     C*------------------------------------------------------------------------*
020300960903     C* IMPVID - IMPOSTAZIONI INIZIALE VIDEO
020400960903     C*------------------------------------------------------------------------*
020500960903     C     IMPVID        BEGSR
020600960903     C*
020700960903     C                   MOVE      UYEAR         V1CANN                         *ANNO CORRENTE
020800960903IF  1C     UYEAR         IFGT      60
020900960903     C                   MOVEL     19            V1CANN                         *SECOLO
021000960903X   1C                   ELSE
021100960903     C                   MOVEL     20            V1CANN                         *SECOLO
021200960903E   1C                   ENDIF
021300960903     C                   MOVEL     'N'           V1CAGG                         *AGGIORAMENTO
021400960903     C*
021500960903     C                   ENDSR
021600020916     C*--------------------------------------------------------------------------------------------*
021700020916     C* REPERISCE I DATI UTENTE
021800020916     C*--------------------------------------------------------------------------------------------*
021900020916     C     REPDATIUTE    BEGSR
022000020916     C*
022100020916     C* INIZIALIZZA VARIABILI DI WRK
022200020916     C                   CLEAR                   TIBS34DS
022300020916     C                   CLEAR                   AZUTEDS
022400020916     C                   CLEAR                   DDATIUTE
022500020916     C*
022600020916     C     *DTAARA       DEFINE    §azute        azuteds
022700020916     C     *DTAARA       DEFINE    §datiute      ddatiute
022800020916     C                   IN(E)     *DTAARA
022900020916if  1C                   IF        %Error
023000020916     c                   EVAL      I34Tla = 'L'
023100020916     C                   CALL      'TIBS34R'
023200020916     C                   PARM                    Tibs34Ds
023300020916     C                   IN        *DTAARA
023400020916e   1C                   ENDIF
023500020916     C*
023600020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
023700020916     C                   MOVEL(P)  RSUT          DSFIRS
023800020916     C*
023900020916     C                   ENDSR
024000960903     C*------------------------------------------------------------------------*
024100960903     C* *INZSR - OPERAZIONI INIZIALI
024200960903     C*------------------------------------------------------------------------*
024300960903     C     *INZSR        BEGSR
024400960903     C*--------------------
024500960903     C* RICEVIMENTO PARAMETRI
024600960903     C*--------------------
024700960903     C     *ENTRY        PLIST
024800960903     C                   PARM                    KPJBA
024900960903     C*
025000960903     C* OPERAZIONI INIZIALI STANDARD
025100960903     C                   EXSR      INZSTD
025200960903     C*
025300960903     C* IMPOSTAZIONI INIZIALI VIDEO
025400960903     C                   EXSR      IMPVID
025500960903     C*
025600960903     C* EMISSIONE FORMATO VIDEO
025700960903     C                   EXSR      CARV1
025800960903     C*
025900960903     C                   ENDSR
026000960903     C*------------------------------------------------------------------------*
026100960903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026200960903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026300960903     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
026400960903     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
026500960903     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
026600960903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026700960903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
026800960903     C*------------------------------------------------------------------------*
026900960903     C* INZSTD - ROUTINE INIZIALE STANDARD
027000960903     C*------------------------------------------------------------------------*
027100960903     C     INZSTD        BEGSR
027200960903     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
027300960903     C                   CALL      'XKEYS'
027400960903     C                   PARM                    DSKEYS
027500960903     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
027600960903     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
027700960903     C     XMSGPA        PLIST
027800960903     C                   PARM                    DSMSMS
027900960903     C                   PARM                    DSMSPG
028000960903     C                   PARM                    DSMSKE
028100960903     C                   PARM                    DSMSER
028200960903     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
028300020114     C                   MOVEL     'TISEC8R'     DSMSPG                         *NOME PGM CHIAMANTE
028400020114     C                   MOVEL     'TISEC8R'     VMCQUE                         *CODA MESSAGGI DESTIN
028500960903     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
028600960903     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
028700960903     C* INIZIALIZZO FLAG CONTROLLO PGM
028800960903     C                   MOVEL     '0'           $ERR              1            *ERRORE
028900960903     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
029000960903     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
029100960903     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
029200960903     C                   MOVEL     '0'           $FINE             1            *FINE PGM
029300960903     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
029400960903     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
029500960903     C* LANCIO ROUTINE PER VERIFICARE SE
029600960903     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
029700960903     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
029800020916     C                   EXSR      REPDATIUTE
029900960903     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
030000960903     C                   BITOFF    '01234567'    POSMI1            1
030100960903     C                   BITOFF    '01234567'    POSMI2            1
030200960903     C                   BITOFF    '01234567'    POSMA1            1
030300960903     C                   BITOFF    '01234567'    POSMA2            1
030400960903     C                   BITON     '34'          POSMI1            1            *18
030500960903     C                   BITON     '7'           POSMI2            1            *01
030600960903     C                   BITON     '34'          POSMA1            1            *18
030700960903     C                   BITON     '0'           POSMA2            1            *80
030800960903     C                   MOVE      POSMI2        POSMIN            2
030900960903     C                   MOVEL     POSMI1        POSMIN
031000960903     C                   MOVE      POSMA2        POSMAX            2
031100960903     C                   MOVEL     POSMA1        POSMAX
031200960903     C                   ENDSR
031300960903     C*------------------------------------------------------------------------*
031400960903     C* FUNSTD - LANCIA SUBROUTINE STANDARD
031500960903     C*------------------------------------------------------------------------*
031600960903     C     FUNSTD        BEGSR
031700960903     C                   SELECT
031800960903     C     KEY           WHENEQ    F3
031900960903     C                   EXSR      F03STD
032000960903     C     KEY           WHENEQ    F12
032100960903     C                   EXSR      F12STD
032200960903     C     KEY           WHENEQ    F24
032300960903     C                   EXSR      F24STD
032400960903     C                   OTHER
032500960903     C                   MOVEL     MSG(2)        $MSG
032600960903     C                   EXSR      CARMSG
032700960903     C                   ENDSL
032800960903     C                   ENDSR
032900960903     C*------------------------------------------------------------------------*
033000960903     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
033100960903     C*------------------------------------------------------------------------*
033200960903     C     F03STD        BEGSR
033300960903     C*
033400960903     C* IMPOSTO FLAG DI FINE PROGRAMMA
033500960903     C                   MOVEL     '1'           $FINE
033600960903     C*
033700960903     C                   ENDSR
033800960903     C*------------------------------------------------------------------------*
033900960903     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
034000960903     C*------------------------------------------------------------------------*
034100960903     C     F12STD        BEGSR
034200960903     C*
034300960903     C* IMPOSTO FLAG DI FINE PROGRAMMA
034400960903     C                   MOVEL     '1'           $FINE
034500960903     C*
034600960903     C                   ENDSR
034700960903     C*------------------------------------------------------------------------*
034800960903     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
034900960903     C*------------------------------------------------------------------------*
035000960903     C     F24STD        BEGSR
035100960903     C*
035200960903     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
035300960903     C                   EXSR      OUTFUN
035400960903     C*
035500960903     C                   ENDSR
035600960903     C*------------------------------------------------------------------------*
035700960903     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
035800960903     C*          CODA MESSAGGI DEL PGM
035900960903     C*------------------------------------------------------------------------*
036000960903     C     CARMSG        BEGSR
036100960903     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
036200960903     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
036300960903     C     MSGULT        IFEQ      0
036400960903     C                   MOVEL     '1'           *IN40
036500020114     C                   WRITE     SEC8DVK
036600960903     C                   MOVEL     '0'           *IN40
036700960903     C                   ENDIF
036800960903     C* INVIA MESSAGGIO ALLA CODA DEL PGM
036900960903     C                   MOVEL     $MSG          DSMSMS
037000960903     C                   CALL      'XMSG'        XMSGPA
037100960903     C     DSMSER        IFEQ      0
037200960903     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
037300960903     C                   ENDIF
037400960903     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
037500960903     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
037600020114     C                   WRITE     SEC8DVM                                      *SFL MESS
037700960903     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
037800960903     C                   MOVEL     *BLANKS       $MSG
037900960903     C                   ENDSR
038000960903     C*------------------------------------------------------------------------*
038100960903     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
038200960903     C*------------------------------------------------------------------------*
038300960903     C     OUTMSG        BEGSR
038400960903     C* VISUALIZZAZIONE MESSAGGIO
038500960903     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
038600020114     C                   WRITE     SEC8DVK                                      *CONTR MESS
038700960903     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
038800960903     C                   Z-ADD     *ZEROS        MSGULT
038900960903     C                   ENDSR
039000960903     C*------------------------------------------------------------------------*
039100960903     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
039200960903     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
039300960903     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
039400960903     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
039500960903     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
039600960903     C*------------------------------------------------------------------------*
039700960903     C     CARFUN        BEGSR
039800960903     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
039900960903     C     1             DO        31            I                 5 0          --- 1 -->
040000960903     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
040100960903     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
040200960903     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
040300020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
040400960903     C     $TGES         IFEQ      'G'                                          *GESTIONE
040500960903     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
040600960903     C                   ELSE                                                   *INTERROG.
040700960903     C                   MOVEL     FUNASI        FAT(I)
040800960903     C                   ENDIF
040900960903     C                   ELSE                                                   <-- 2 --> *FILIALE
041000960903     C     $TGES         IFEQ      'G'                                          *GESTIONE
041100960903     C                   MOVEL     FUNAFG        FAT(I)
041200960903     C                   ELSE                                                   *INTERROG.
041300960903     C                   MOVEL     FUNAFI        FAT(I)
041400960903     C                   ENDIF
041500960903     C                   ENDIF                                                  <-- 2 ---
041600960903     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
041700960903     C                   MOVEL     USER          WEDP              3            *PROFILO
041800960903     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
041900960903     C                   MOVEL     'S'           FAT(I)                         *ABILITO
042000960903     C                   ELSE
042100960903     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
042200960903     C                   ENDIF
042300960903     C                   ENDIF
042400960903     C                   ENDDO                                                  <-- 1 ---
042500960903     C                   ENDSR
042600960903     C*------------------------------------------------------------------------*
042700960903     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
042800960903     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
042900960903     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
043000960903     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
043100960903     C*------------------------------------------------------------------------*
043200960903     C     OUTFUN        BEGSR
043300960903     C     1             DO        8             I                              *PULIZIA FUNZIONI A
043400960903     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
043500960903     C                   ENDDO
043600960903     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
043700960903     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
043800960903     C     1             DO        31            I
043900960903     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
044000960903     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
044100960903     C                   ADD       1             FUNNAT
044200960903     C                   ENDIF
044300960903     C                   ENDDO
044400960903     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
044500960903     C     FUNNAT        IFGT      8
044600960903     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
044700960903     C                   ENDIF
044800960903     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
044900960903     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
045000960903     C                   Z-ADD     8             FUNVIS            5 0
045100960903     C     FAT(23)       IFEQ      'S'
045200960903     C                   SUB       1             FUNVIS
045300960903     C                   ENDIF
045400960903     C     FAT(24)       IFEQ      'S'
045500960903     C                   SUB       1             FUNVIS
045600960903     C                   ENDIF
045700960903     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
045800960903     C                   Z-ADD     *ZEROS        W                 5 0
045900960903     C                   ADD       1             FUNULT
046000960903     C     FUNULT        IFGT      31
046100960903     C                   Z-ADD     1             FUNULT
046200960903     C                   ENDIF
046300960903     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
046400960903     C     FUNULT        DO        31            I                              --- 1 -->
046500960903     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
046600960903     C     W             IFLT      FUNVIS                                       --- 2 -->
046700960903     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
046800960903     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
046900960903     C     FDE(I)        ANDNE     *BLANKS
047000960903     C     I             IFEQ      23                                           *TOLGO F23
047100960903     C     I             OREQ      24                                           *E F24
047200960903     C                   ELSE
047300960903     C                   ADD       1             W
047400960903     C                   MOVEL     FDE(I)        FUV(W)
047500960903     C                   ENDIF
047600960903     C                   ENDIF                                                  <-- 3 ---
047700960903     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
047800960903     C                   Z-ADD     I             FUNULT
047900960903     C                   ENDIF                                                  <-- 2 ---
048000960903     C                   ENDDO                                                  <-- 1 ---
048100960903     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
048200960903     C                   MOVEL     FUV(1)        VFD001
048300960903     C                   MOVEL     FUV(2)        VFD002
048400960903     C                   MOVEL     FUV(3)        VFD003
048500960903     C                   MOVEL     FUV(4)        VFD004
048600960903     C                   MOVEL     FUV(5)        VFD005
048700960903     C                   MOVEL     FUV(6)        VFD006
048800960903     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
048900960903     C     FAT(24)       ANDEQ     'N'
049000960903     C                   MOVEL     FUV(7)        VFD007
049100960903     C                   MOVEL     FUV(8)        VFD008
049200960903     C                   ELSE                                                   <-- 1 -->
049300960903     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
049400960903     C     FAT(24)       ANDEQ     'S'
049500960903     C                   MOVEL     FDE(23)       VFD007
049600960903     C                   MOVEL     FDE(24)       VFD008
049700960903     C                   ELSE                                                   <-- 2 -->
049800960903     C                   MOVEL     FUV(7)        VFD007
049900960903     C     FAT(23)       IFEQ      'S'
050000960903     C                   MOVEL     FDE(23)       VFD008
050100960903     C                   ELSE
050200960903     C                   MOVEL     FDE(24)       VFD008
050300960903     C                   ENDIF
050400960903     C                   ENDIF                                                  <-- 2 ---
050500960903     C                   ENDIF                                                  <-- 1 ---
050600960903     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
050700960903     C                   ENDSR
050800960903     C*------------------------------------------------------------------------*
050900960903     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
051000960903     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
051100960903     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
051200960903     C*                   - ERRORE SE FUNZIONE NON ATTIVA
051300960903     C*------------------------------------------------------------------------*
051400960903     C     CONFUN        BEGSR
051500960903     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
051600960903     C     1             DO        31            I                              --- 1 -->
051700960903     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
051800960903     C* CONTROLLO SE IL TASTO E' ATTIVO
051900960903     C     FAT(I)        IFNE      'S'                                          --- 3 -->
052000960903     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
052100960903     C                   EXSR      CARMSG
052200960903     C                   MOVEL     '1'           $ERR
052300960903     C                   ENDIF                                                  <-- 3 ---
052400960903     C                   ENDIF                                                  <-- 2 ---
052500960903     C                   ENDDO                                                  <-- 1 ---
052600960903     C                   ENDSR
052700960903     O*------------------------------------------------------------------------*
052800960903** ======== SCHIERA: MSG  (MESSAGGI)                       ================
052900020114TISEC8R- Tasto funzionale non valido.                                          1
053000020114TISEC8R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
053100020114TISEC8R- Opzione non valida.                                                   3
053200020114TISEC8R- Opzione abilitata ma non gestita. Avvertire CED.                      4
053300020114TISEC8R- Inizio elenco.                                                        5
053400020114TISEC8R- Fine elenco.                                                          6
053500020114TISEC8R- Anno da caricare obbligatorio                                         7
053600020114TISEC8R- 'S' per aggiornare i dati già esistenti, 'N' per non aggiornarli      8
053700020114TISEC8R- 9
053800020114TISEC8R- 10
053900020114TISEC8R- 11
054000020114TISEC8R- 12
054100020114TISEC8R- 13
054200020114TISEC8R- 14
054300020114TISEC8R- 15
054400020114TISEC8R- 16
054500020114TISEC8R- 17
054600020114TISEC8R- 18
054700020114TISEC8R- 19
054800020114TISEC8R- 20
054900960903** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
055000960903F1    -                   -    NN     NN
055100960903F2    -                   -    NN     NN
055200960903F3    -F3=Fine            -    SS     SS
055300960903F4    -                   -    NN     NN
055400960903F5    -                   -    NN     NN
055500960903F6    -F6=Lancio          -    SS     SS
055600960903F7    -                   -    NN     NN
055700960903F8    -                   -    NN     NN
055800960903F9    -                   -    NN     NN
055900960903F10   -                   -    NN     NN
056000960903F11   -                   -    NN     NN
056100960903F12   -F12-Ritorno        -    SS     SS
056200960903F13   -                   -    NN     NN
056300960903F14   -                   -    NN     NN
056400960903F15   -                   -    NN     NN
056500960903F16   -                   -    NN     NN
056600960903F17   -                   -    NN     NN
056700960903F18   -                   -    NN     NN
056800960903F19   -                   -    NN     NN
056900960903F20   -                   -    NN     NN
057000960903F21   -                   -    NN     NN
057100960903F22   -                   -    NN     NN
057200960903F23   -                   -    NN     NN
057300960903F24   -F24=Altri tasti    -    NN     NN
057400960903ENTER -                   -    SS     SS
057500960903ROLLUP-                   -    NN     NN
057600960903ROLLDN-                   -    NN     NN
057700960903HELP  -                   -    NN     NN
057800960903PRINT -                   -    NN     NN
057900960903BSPACE-                   -    NN     NN
058000960903CLEAR -                   -    NN     NN
