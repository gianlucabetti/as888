000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICHE FASCE DI RICAVO/COLLI E BANCALI  - RICHIESTA PARAMETRI     *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800010510     FTISE04D   CF   E             WORKSTN INFDS(DEVDS1)
000900010510     F                                     SFILE(SE04DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*--------------------
001400000000     D* MESSAGGI E FUNZIONI
001500000000     D*--------------------
001600980403     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
001700000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800000000     D*--------------------
001900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000000000     D*--------------------
002100000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002200000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
002300000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400000000     D*--------------------
002500000000     D* TABELLA DESCRIZIONI A VIDEO
002600000000     D*--------------------
002700000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800000000     D*--------------------
002900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003000000000     D*--------------------
003100000000     D KEYS            S              1    DIM(40)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500000000     D*--------------------
003600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700000000     D*--------------------
003800000000     D DSFUN         E DS
003900000000     D  FUNRIG                 1     40
004000000000     D*--------------------
004100000000     D* DS PER GESTIONE TASTI FUNZIONALI
004200000000     D*--------------------
004300000000     D DSKEYS        E DS
004400000000     D  KEYRIG                 1     40
004500000000     D*--------------------
004600000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004700000000     D*--------------------
004800000000     D                SDS
004900000000     D  USER                 254    263
005000000000     D*--------------------
005100000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005200000000     D*--------------------
005300000000     D DEVDS1          DS
005400000000     D  KEY                  369    369
005500000000     D  CURSOR               370    371
005600000000     D*--------------------
005700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005800000000     D*--------------------
005900000000     D DSMSG         E DS
006000000000     D*--------------------
006100000000     D* ARCHITETTURA
006200000000     D*--------------------
006300000000     D KPJBA         E DS
006400000000     D*--------------------
006500020916     D* DS REPERIMENTO DATI UTENTE
006600000000     D*--------------------
006700020916     D TIBS34DS      E DS                                                       *Profili utente
006800020916     D DDATIUTE      E DS                                                       *Dati utente
006900020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000000000     D*--------------------
007100000000     D* PARAMETRI DI LANCIO
007200000000     D*--------------------
007300000000     D PARAM           DS                  INZ
007400000000     D  PARDAI                 1      6  0
007500000000     D  PARDAF                 7     12  0
007600171219     D  PARBAN                13     13
007700171219     D  PARFIL                14     14
007800000000     D*--------------------
007900000000     D* DS "XSRDAT" - CONTROLLO DATA (8)
008000000000     D*--------------------
008100000000     D WLBDA8          DS                  INZ
008200000000     D  G08DAT                 1      8  0
008300000000     D  G08INV                 9     16  0
008400000000     D  G08ERR                17     17
008500000000     D  G08TGI                18     22  0
008600000000     C*------------------------------------------------------------------------*
008700000000     C* MAIN LINE
008800000000     C*------------------------------------------------------------------------*
008900000000     C*
009000000000     C* CICLO FINO A CHE:
009100000000DO  1C     $FINE         DOUEQ     '1'
009200000000     C*
009300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009400000000     C*
009500000000     C* CONTROLLA GESTIONE VIDEO
009600000000IF  2C     $GES          IFEQ      'V1'
009700000000     C                   EXSR      GESV1                                        *PARAMETRI
009800000000E   2C                   ENDIF
009900000000     C*
010000000000E   1C                   ENDDO
010100000000     C*
010200000000     C                   SETON                                        LR
010300000000     C*------------------------------------------------------------------------*
010400000000     C* GESV1 - GESTIONE VIDEO PARAMETRI
010500000000     C*------------------------------------------------------------------------*
010600000000     C     GESV1         BEGSR
010700000000     C*
010800000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
010900000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011000000000E   1C                   ENDIF
011100000000     C*
011200000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
011300000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011400000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011500000000E   1C                   ENDIF
011600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011700000000     C*
011800010510     C                   EXFMT     SE04DV1                                      *PARAMETRI
011900000000     C*
012000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
012100000000     C*
012200000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012300000000SL  2C                   SELECT
012400000000     C     KEY           WHENEQ    ENTER
012500000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012600000000     C     KEY           WHENEQ    F6
012700000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012800000000     C                   OTHER
012900000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013000000000E   2C                   ENDSL
013100000000E   1C                   ENDIF
013200000000     C*
013300000000     C                   ENDSR
013400000000     C*------------------------------------------------------------------------*
013500000000     C* CARV1 - CARICA VIDEO PARAMETRI
013600000000     C*------------------------------------------------------------------------*
013700000000     C     CARV1         BEGSR
013800000000     C*
013900000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014000000000     C*
014100000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014200000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014300000000     C*
014400010510     C                   WRITE     SE04DVT                                      *TESTATA
014500010510     C                   WRITE     SE04DVF                                      *FUNZIONI
014600000000     C*
014700000000     C                   ENDSR
014800000000     C*------------------------------------------------------------------------*
014900000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO PARAMETRI
015000000000     C*------------------------------------------------------------------------*
015100000000     C     ENTV1         BEGSR
015200000000     C*
015300000000     C* CONTROLLA I CAMPI IMMESSI
015400000000     C                   EXSR      DECODI
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> LANCIO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     F06GES        BEGSR
016100000000     C*
016200000000     C* CONTROLLA I CAMPI IMMESSI
016300000000     C                   EXSR      DECODI
016400000000     C*
016500000000     C* LANCIO IL PROGRAMMA DI STAMPA
016600000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
016700000000     C                   Z-ADD     V1CDAI        PARDAI                         *ANNO INIZIALE
016800000000     C                   Z-ADD     V1CDAF        PARDAF                         *ANNO FINALE
016900000000     C                   MOVEL     V1CBAN        PARBAN                         *STATISTICA BANCALI
017000080625     C                   MOVEL     V1CFIL        PARFIL                         *STAT.BANCALI FILE
017100000000     C*
017200010510     C                   MOVEL     'SE05'        KCOAZ
017300000000     C                   MOVEL     PARAM         KPJBU
017400000000     C                   CALL      'BCH10'
017500000000     C                   PARM                    KPJBA
017600000000     C*
017700000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
017800000000E   1C                   ENDIF
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018300000000     C*------------------------------------------------------------------------*
018400000000     C     DECODI        BEGSR
018500000000     C*
018600000000     C* REIMPOSTA INDICATORI DI ERRORE
018700171219     C                   SETOFF                                       606163
018800000000     C*--------------------
018900000000     C* ANNO/MESE INIZIALE
019000000000     C*--------------------
019100000000IF  1C     $ERR          IFEQ      '0'
019200000000     C                   MOVEL     V1CDAI        G08INV
019300000000     C                   MOVE      01            G08INV
019400000000     C                   Z-ADD     *ZEROS        G08DAT
019500000000     C                   MOVEL     '3'           G08ERR
019600000000     C                   CALL      'XSRDA8'
019700000000     C                   PARM                    WLBDA8
019800000000IF  2C     G08ERR        IFEQ      '1'
019900000000     C                   MOVEL     '1'           $ERR
020000171219     C                   MOVEL     MSG(3)        $MSG
020100000000     C                   EXSR      CARMSG
020200000000     C                   SETON                                        60
020300000000E   2C                   ENDIF
020400000000E   1C                   ENDIF
020500000000     C*--------------------
020600000000     C* ANNO/MESE FINALE
020700000000     C*--------------------
020800000000IF  1C     $ERR          IFEQ      '0'
020900000000     C                   MOVEL     V1CDAF        G08INV
021000000000     C                   MOVE      01            G08INV
021100000000     C                   Z-ADD     *ZEROS        G08DAT
021200000000     C                   MOVEL     '3'           G08ERR
021300000000     C                   CALL      'XSRDA8'
021400000000     C                   PARM                    WLBDA8
021500000000IF  2C     G08ERR        IFEQ      '1'
021600000000     C                   MOVEL     '1'           $ERR
021700171219     C                   MOVEL     MSG(3)        $MSG
021800000000     C                   EXSR      CARMSG
021900000000     C                   SETON                                        61
022000000000E   2C                   ENDIF
022100000000E   1C                   ENDIF
022200000000     C*--------------------
022300000000     C* ANNO/MESE INIZIALE E FINALE
022400000000     C*--------------------
022500000000IF  1C     $ERR          IFEQ      '0'
022600000000IF  2C     V1CDAF        IFLT      V1CDAI
022700000000     C                   MOVEL     '1'           $ERR
022800171219     C                   MOVEL     MSG(4)        $MSG
022900000000     C                   EXSR      CARMSG
023000000000     C                   SETON                                        6061
023100000000E   2C                   ENDIF
023200000000E   1C                   ENDIF
023300000000     C*--------------------
023400000000     C* SCELTA STATISTICA BANCALI
023500000000     C*--------------------
023600000000IF  1C     $ERR          IFEQ      '0'
023700000000IF  2C     V1CBAN        IFNE      *BLANKS
023800000000     C     V1CBAN        ANDNE     '1'
023900000000     C                   MOVEL     '1'           $ERR
024000171219     C                   MOVEL     MSG(5)        $MSG
024100000000     C                   EXSR      CARMSG
024200171219     C                   SETON                                        63
024300000000E   2C                   ENDIF
024400000000E   1C                   ENDIF
024500000000     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
024900000000     C*------------------------------------------------------------------------*
025000000000     C     IMPVID        BEGSR
025100000614     C*
025200000000     C                   MOVEL     '1'           V1CBAN
025300080910     C                   MOVEL     'S'           V1CFIL
025400000000     C*
025500000000     C                   ENDSR
025600020916     C*--------------------------------------------------------------------------------------------*
025700020916     C* REPERISCE I DATI UTENTE
025800020916     C*--------------------------------------------------------------------------------------------*
025900020916     C     REPDATIUTE    BEGSR
026000020916     C*
026100020916     C* INIZIALIZZA VARIABILI DI WRK
026200020916     C                   CLEAR                   TIBS34DS
026300020916     C                   CLEAR                   AZUTEDS
026400020916     C                   CLEAR                   DDATIUTE
026500020916     C*
026600020916     C     *DTAARA       DEFINE    §azute        azuteds
026700020916     C     *DTAARA       DEFINE    §datiute      ddatiute
026800020916     C                   IN(E)     *DTAARA
026900020916if  1C                   IF        %Error
027000020916     c                   EVAL      I34Tla = 'L'
027100020916     C                   CALL      'TIBS34R'
027200020916     C                   PARM                    Tibs34Ds
027300020916     C                   IN        *DTAARA
027400020916e   1C                   ENDIF
027500020916     C*
027600020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
027700020916     C                   MOVEL(P)  RSUT          DSFIRS           20
027800020916     C*
027900020916     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100000000     C* *INZSR - IMPOSTAZIONI INIZIALI
028200000000     C*------------------------------------------------------------------------*
028300000000     C     *INZSR        BEGSR
028400000000     C*--------------------
028500000000     C* RICEVIMENTO PARAMETRI
028600000000     C*--------------------
028700000000     C     *ENTRY        PLIST
028800000000     C                   PARM                    KPJBA
028900000000     C*
029000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
029100000000     C                   EXSR      INZSTD
029200000000     C*
029300000000     C* IMPOSTAZIONE INIZIALE VIDEO
029400000000     C                   EXSR      IMPVID
029500000000     C*
029600000000     C* EMESSIONE FORMATO VIDEO
029700000000     C                   EXSR      CARV1
029800000000     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030300000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
030400000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
030500000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
030600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030800000000     C*------------------------------------------------------------------------*
030900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
031000000000     C*------------------------------------------------------------------------*
031100000000     C     INZSTD        BEGSR
031200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
031300000000     C                   CALL      'XKEYS'
031400000000     C                   PARM                    DSKEYS
031500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
031600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
031700000000     C     XMSGPA        PLIST
031800000000     C                   PARM                    DSMSMS
031900000000     C                   PARM                    DSMSPG
032000000000     C                   PARM                    DSMSKE
032100000000     C                   PARM                    DSMSER
032200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
032300010510     C                   MOVEL     'TISE04R'     DSMSPG                         *NOME PGM CHIAMANTE
032400010510     C                   MOVEL     'TISE04R'     VMCQUE                         *CODA MESSAGGI DESTIN
032500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
032600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
032700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
032800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
032900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
033000000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
033100000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
033200000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
033300000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
033400000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
033500000000     C* LANCIO ROUTINE PER VERIFICARE SE
033600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
033700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
033800020916     C                   EXSR      REPDATIUTE
033900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
034000000000     C                   BITOFF    '01234567'    POSMI1            1
034100000000     C                   BITOFF    '01234567'    POSMI2            1
034200000000     C                   BITOFF    '01234567'    POSMA1            1
034300000000     C                   BITOFF    '01234567'    POSMA2            1
034400000000     C                   BITON     '34'          POSMI1            1            *18
034500000000     C                   BITON     '7'           POSMI2            1            *01
034600000000     C                   BITON     '34'          POSMA1            1            *18
034700000000     C                   BITON     '0'           POSMA2            1            *80
034800000000     C                   MOVE      POSMI2        POSMIN            2
034900000000     C                   MOVEL     POSMI1        POSMIN
035000000000     C                   MOVE      POSMA2        POSMAX            2
035100000000     C                   MOVEL     POSMA1        POSMAX
035200000000     C                   ENDSR
035300000000     C*------------------------------------------------------------------------*
035400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
035500000000     C*------------------------------------------------------------------------*
035600000000     C     FUNSTD        BEGSR
035700000000     C                   SELECT
035800000000     C     KEY           WHENEQ    F3
035900000000     C                   EXSR      F03STD
036000000000     C     KEY           WHENEQ    F12
036100000000     C                   EXSR      F12STD
036200000000     C     KEY           WHENEQ    F24
036300000000     C                   EXSR      F24STD
036400000000     C                   OTHER
036500000000     C                   MOVEL     MSG(2)        $MSG
036600000000     C                   EXSR      CARMSG
036700000000     C                   ENDSL
036800000000     C                   ENDSR
036900000000     C*------------------------------------------------------------------------*
037000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
037100000000     C*------------------------------------------------------------------------*
037200000000     C     F03STD        BEGSR
037300000000     C*
037400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
037500000000     C                   MOVEL     '1'           $FINE
037600000000     C*
037700000000     C                   ENDSR
037800000000     C*------------------------------------------------------------------------*
037900000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
038000000000     C*------------------------------------------------------------------------*
038100000000     C     F12STD        BEGSR
038200000000     C*
038300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
038400000000     C                   MOVEL     '1'           $FINE
038500000000     C*
038600000000     C                   ENDSR
038700000000     C*------------------------------------------------------------------------*
038800000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
038900000000     C*------------------------------------------------------------------------*
039000000000     C     F24STD        BEGSR
039100000000     C*
039200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
039300000000     C                   EXSR      OUTFUN
039400000000     C*
039500000000     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
039800000000     C*          CODA MESSAGGI DEL PGM
039900000000     C*------------------------------------------------------------------------*
040000000000     C     CARMSG        BEGSR
040100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
040200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
040300000000     C     MSGULT        IFEQ      0
040400000000     C                   MOVEL     '1'           *IN40
040500010510     C                   WRITE     SE04DVK
040600000000     C                   MOVEL     '0'           *IN40
040700000000     C                   ENDIF
040800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
040900000000     C                   MOVEL     $MSG          DSMSMS
041000000000     C                   CALL      'XMSG'        XMSGPA
041100000000     C     DSMSER        IFEQ      0
041200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
041300000000     C                   ENDIF
041400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
041500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
041600010510     C                   WRITE     SE04DVM                                      *SFL MESS
041700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
041800000000     C                   MOVEL     *BLANKS       $MSG
041900000000     C                   ENDSR
042000000000     C*------------------------------------------------------------------------*
042100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
042200000000     C*------------------------------------------------------------------------*
042300000000     C     OUTMSG        BEGSR
042400000000     C* VISUALIZZAZIONE MESSAGGIO
042500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
042600010510     C                   WRITE     SE04DVK                                      *CONTR MESS
042700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
042800000000     C                   Z-ADD     *ZEROS        MSGULT
042900000000     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
043200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
043300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
043400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
043500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
043600000000     C*------------------------------------------------------------------------*
043700000000     C     CARFUN        BEGSR
043800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
043900000000     C     1             DO        31            I                 5 0          --- 1 -->
044000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
044100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
044200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
044300020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
044400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
044500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
044600000000     C                   ELSE                                                   *INTERROG.
044700000000     C                   MOVEL     FUNASI        FAT(I)
044800000000     C                   ENDIF
044900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
045000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
045100000000     C                   MOVEL     FUNAFG        FAT(I)
045200000000     C                   ELSE                                                   *INTERROG.
045300000000     C                   MOVEL     FUNAFI        FAT(I)
045400000000     C                   ENDIF
045500000000     C                   ENDIF                                                  <-- 2 ---
045600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
045700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
045800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
045900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
046000000000     C                   ELSE
046100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
046200000000     C                   ENDIF
046300000000     C                   ENDIF
046400000000     C                   ENDDO                                                  <-- 1 ---
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
046800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
046900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
047000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
047100000000     C*------------------------------------------------------------------------*
047200000000     C     OUTFUN        BEGSR
047300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
047400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
047500000000     C                   ENDDO
047600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
047700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
047800000000     C     1             DO        31            I
047900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
048000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
048100000000     C                   ADD       1             FUNNAT
048200000000     C                   ENDIF
048300000000     C                   ENDDO
048400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
048500000000     C     FUNNAT        IFGT      8
048600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
048700000000     C                   ENDIF
048800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
048900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
049000000000     C                   Z-ADD     8             FUNVIS            5 0
049100000000     C     FAT(23)       IFEQ      'S'
049200000000     C                   SUB       1             FUNVIS
049300000000     C                   ENDIF
049400000000     C     FAT(24)       IFEQ      'S'
049500000000     C                   SUB       1             FUNVIS
049600000000     C                   ENDIF
049700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
049800000000     C                   Z-ADD     *ZEROS        W                 5 0
049900000000     C                   ADD       1             FUNULT
050000000000     C     FUNULT        IFGT      31
050100000000     C                   Z-ADD     1             FUNULT
050200000000     C                   ENDIF
050300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
050400000000     C     FUNULT        DO        31            I                              --- 1 -->
050500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
050600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
050700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
050800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
050900000000     C     FDE(I)        ANDNE     *BLANKS
051000000000     C     I             IFEQ      23                                           *TOLGO F23
051100000000     C     I             OREQ      24                                           *E F24
051200000000     C                   ELSE
051300000000     C                   ADD       1             W
051400000000     C                   MOVEL     FDE(I)        FUV(W)
051500000000     C                   ENDIF
051600000000     C                   ENDIF                                                  <-- 3 ---
051700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
051800000000     C                   Z-ADD     I             FUNULT
051900000000     C                   ENDIF                                                  <-- 2 ---
052000000000     C                   ENDDO                                                  <-- 1 ---
052100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
052200000000     C                   MOVEL     FUV(1)        VFD001
052300000000     C                   MOVEL     FUV(2)        VFD002
052400000000     C                   MOVEL     FUV(3)        VFD003
052500000000     C                   MOVEL     FUV(4)        VFD004
052600000000     C                   MOVEL     FUV(5)        VFD005
052700000000     C                   MOVEL     FUV(6)        VFD006
052800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
052900000000     C     FAT(24)       ANDEQ     'N'
053000000000     C                   MOVEL     FUV(7)        VFD007
053100000000     C                   MOVEL     FUV(8)        VFD008
053200000000     C                   ELSE                                                   <-- 1 -->
053300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
053400000000     C     FAT(24)       ANDEQ     'S'
053500000000     C                   MOVEL     FDE(23)       VFD007
053600000000     C                   MOVEL     FDE(24)       VFD008
053700000000     C                   ELSE                                                   <-- 2 -->
053800000000     C                   MOVEL     FUV(7)        VFD007
053900000000     C     FAT(23)       IFEQ      'S'
054000000000     C                   MOVEL     FDE(23)       VFD008
054100000000     C                   ELSE
054200000000     C                   MOVEL     FDE(24)       VFD008
054300000000     C                   ENDIF
054400000000     C                   ENDIF                                                  <-- 2 ---
054500000000     C                   ENDIF                                                  <-- 1 ---
054600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
054700000000     C                   ENDSR
054800000000     C*------------------------------------------------------------------------*
054900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
055000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
055100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
055200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
055300000000     C*------------------------------------------------------------------------*
055400000000     C     CONFUN        BEGSR
055500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
055600000000     C     1             DO        31            I                              --- 1 -->
055700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
055800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
055900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
056000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
056100000000     C                   EXSR      CARMSG
056200000000     C                   MOVEL     '1'           $ERR
056300000000     C                   ENDIF                                                  <-- 3 ---
056400000000     C                   ENDIF                                                  <-- 2 ---
056500000000     C                   ENDDO                                                  <-- 1 ---
056600000000     C                   ENDSR
056700000000     O*------------------------------------------------------------------------*
056800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
056900010510TISE04R- Tasto funzionale non valido.                                          1
057000010510TISE04R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
057100171219TISE04R- Anno/mese errato                                                      3
057200171219TISE04R- Anno/mese finale deve essere maggiore/uguale ad Anno/Mese iniziale    4
057300171219TISE04R- Scelta errata                                                         5
057400171219TISE04R-06
057500171219TISE04R-07
057600171219TISE04R-08
057700171219TISE04R-09
057800171219TISE04R-10
057900171219TISE04R-11
058000171219TISE04R-12
058100010510TISE04R-13
058200010510TISE04R-14
058300010510TISE04R-15
058400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
058500000000F1    -                   -    NN     NN
058600000000F2    -                   -    NN     NN
058700000000F3    -F3=Fine            -    SS     SS
058800000000F4    -                   -    NN     NN
058900000000F5    -                   -    NN     NN
059000000000F6    -F6=Lancio          -    SS     SS
059100000000F7    -                   -    NN     NN
059200000000F8    -                   -    NN     NN
059300000000F9    -                   -    NN     NN
059400000000F10   -                   -    NN     NN
059500000000F11   -                   -    NN     NN
059600000000F12   -F12-Ritorno        -    SS     SS
059700000000F13   -                   -    NN     NN
059800000000F14   -                   -    NN     NN
059900000000F15   -                   -    NN     NN
060000000000F16   -                   -    NN     NN
060100000000F17   -                   -    NN     NN
060200000000F18   -                   -    NN     NN
060300000000F19   -                   -    NN     NN
060400000000F20   -                   -    NN     NN
060500000000F21   -                   -    NN     NN
060600000000F22   -                   -    NN     NN
060700000000F23   -                   -    NN     NN
060800000000F24   -F24=Altri tasti    -    NN     NN
060900000000ENTER -                   -    SS     SS
061000000000ROLLUP-                   -    NN     NN
061100000000ROLLDN-                   -    NN     NN
061200000000HELP  -                   -    NN     NN
061300000000PRINT -                   -    NN     NN
061400000000BSPACE-                   -    NN     NN
061500000000CLEAR -                   -    NN     NN
