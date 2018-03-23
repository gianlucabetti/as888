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
007600000000     D  PARRIC                13     13
007700000000     D  PARCOL                14     14
007800000000     D  PARBAN                15     15
007900000614     D  PARPT                 16     16
008000080625     D  PARFIL                17     17
008100000000     D*--------------------
008200000000     D* DS "XSRDAT" - CONTROLLO DATA (8)
008300000000     D*--------------------
008400000000     D WLBDA8          DS                  INZ
008500000000     D  G08DAT                 1      8  0
008600000000     D  G08INV                 9     16  0
008700000000     D  G08ERR                17     17
008800000000     D  G08TGI                18     22  0
008900000000     C*------------------------------------------------------------------------*
009000000000     C* MAIN LINE
009100000000     C*------------------------------------------------------------------------*
009200000000     C*
009300000000     C* CICLO FINO A CHE:
009400000000DO  1C     $FINE         DOUEQ     '1'
009500000000     C*
009600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009700000000     C*
009800000000     C* CONTROLLA GESTIONE VIDEO
009900000000IF  2C     $GES          IFEQ      'V1'
010000000000     C                   EXSR      GESV1                                        *PARAMETRI
010100000000E   2C                   ENDIF
010200000000     C*
010300000000E   1C                   ENDDO
010400000000     C*
010500000000     C                   SETON                                        LR
010600000000     C*------------------------------------------------------------------------*
010700000000     C* GESV1 - GESTIONE VIDEO PARAMETRI
010800000000     C*------------------------------------------------------------------------*
010900000000     C     GESV1         BEGSR
011000000000     C*
011100000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
011200000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011300000000E   1C                   ENDIF
011400000000     C*
011500000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
011600000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011700000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011800000000E   1C                   ENDIF
011900000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012000000000     C*
012100010510     C                   EXFMT     SE04DV1                                      *PARAMETRI
012200000000     C*
012300000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
012400000000     C*
012500000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012600000000SL  2C                   SELECT
012700000000     C     KEY           WHENEQ    ENTER
012800000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012900000000     C     KEY           WHENEQ    F6
013000000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013100000000     C                   OTHER
013200000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013300000000E   2C                   ENDSL
013400000000E   1C                   ENDIF
013500000000     C*
013600000000     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* CARV1 - CARICA VIDEO PARAMETRI
013900000000     C*------------------------------------------------------------------------*
014000000000     C     CARV1         BEGSR
014100000000     C*
014200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014300000000     C*
014400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014600000000     C*
014700010510     C                   WRITE     SE04DVT                                      *TESTATA
014800010510     C                   WRITE     SE04DVF                                      *FUNZIONI
014900000000     C*
015000000000     C                   ENDSR
015100000000     C*------------------------------------------------------------------------*
015200000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO PARAMETRI
015300000000     C*------------------------------------------------------------------------*
015400000000     C     ENTV1         BEGSR
015500000000     C*
015600000000     C* CONTROLLA I CAMPI IMMESSI
015700000000     C                   EXSR      DECODI
015800000000     C*
015900000000     C                   ENDSR
016000000000     C*------------------------------------------------------------------------*
016100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> LANCIO
016200000000     C*------------------------------------------------------------------------*
016300000000     C     F06GES        BEGSR
016400000000     C*
016500000000     C* CONTROLLA I CAMPI IMMESSI
016600000000     C                   EXSR      DECODI
016700000000     C*
016800000000     C* LANCIO IL PROGRAMMA DI STAMPA
016900000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017000000000     C                   Z-ADD     V1CDAI        PARDAI                         *ANNO INIZIALE
017100000000     C                   Z-ADD     V1CDAF        PARDAF                         *ANNO FINALE
017200000000     C                   MOVEL     V1CRIC        PARRIC                         *STATISTICA RICAVI
017300000000     C                   MOVEL     V1CCOL        PARCOL                         *STATISTICA COLLI
017400000000     C                   MOVEL     V1CBAN        PARBAN                         *STATISTICA BANCALI
017500000614     C                   MOVEL     VIDPT         PARPT                          *STATISTICA X POSTE
017600080625     C                   MOVEL     V1CFIL        PARFIL                         *STAT.BANCALI FILE
017700000000     C*
017800010510     C                   MOVEL     'SE05'        KCOAZ
017900000000     C                   MOVEL     PARAM         KPJBU
018000000000     C                   CALL      'BCH10'
018100000000     C                   PARM                    KPJBA
018200000000     C*
018300000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
018400000000E   1C                   ENDIF
018500000000     C*
018600000000     C                   ENDSR
018700000000     C*------------------------------------------------------------------------*
018800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018900000000     C*------------------------------------------------------------------------*
019000000000     C     DECODI        BEGSR
019100000000     C*
019200000000     C* REIMPOSTA INDICATORI DI ERRORE
019300000614     C                   SETOFF                                       606171
019400000000     C                   SETOFF                                       626364
019500080625     C                   SETOFF                                       65
019600000000     C*--------------------
019700000000     C* ANNO/MESE INIZIALE
019800000000     C*--------------------
019900000000IF  1C     $ERR          IFEQ      '0'
020000000000     C                   MOVEL     V1CDAI        G08INV
020100000000     C                   MOVE      01            G08INV
020200000000     C                   Z-ADD     *ZEROS        G08DAT
020300000000     C                   MOVEL     '3'           G08ERR
020400000000     C                   CALL      'XSRDA8'
020500000000     C                   PARM                    WLBDA8
020600000000IF  2C     G08ERR        IFEQ      '1'
020700000000     C                   MOVEL     '1'           $ERR
020800000000     C                   MOVEL     MSG(7)        $MSG
020900000000     C                   EXSR      CARMSG
021000000000     C                   SETON                                        60
021100000000E   2C                   ENDIF
021200000000E   1C                   ENDIF
021300000000     C*--------------------
021400000000     C* ANNO/MESE FINALE
021500000000     C*--------------------
021600000000IF  1C     $ERR          IFEQ      '0'
021700000000     C                   MOVEL     V1CDAF        G08INV
021800000000     C                   MOVE      01            G08INV
021900000000     C                   Z-ADD     *ZEROS        G08DAT
022000000000     C                   MOVEL     '3'           G08ERR
022100000000     C                   CALL      'XSRDA8'
022200000000     C                   PARM                    WLBDA8
022300000000IF  2C     G08ERR        IFEQ      '1'
022400000000     C                   MOVEL     '1'           $ERR
022500000000     C                   MOVEL     MSG(7)        $MSG
022600000000     C                   EXSR      CARMSG
022700000000     C                   SETON                                        61
022800000000E   2C                   ENDIF
022900000000E   1C                   ENDIF
023000000000     C*--------------------
023100000000     C* ANNO/MESE INIZIALE E FINALE
023200000000     C*--------------------
023300000000IF  1C     $ERR          IFEQ      '0'
023400000000IF  2C     V1CDAF        IFLT      V1CDAI
023500000000     C                   MOVEL     '1'           $ERR
023600000000     C                   MOVEL     MSG(8)        $MSG
023700000000     C                   EXSR      CARMSG
023800000000     C                   SETON                                        6061
023900000000E   2C                   ENDIF
024000000000E   1C                   ENDIF
024100000000     C*--------------------
024200000000     C* SCELTA STATISTICA FASCE DI RICAVO
024300000000     C*--------------------
024400000000IF  1C     $ERR          IFEQ      '0'
024500000000IF  2C     V1CRIC        IFNE      *BLANKS
024600000000     C     V1CRIC        ANDNE     '1'
024700000000     C                   MOVEL     '1'           $ERR
024800000000     C                   MOVEL     MSG(10)       $MSG
024900000000     C                   EXSR      CARMSG
025000000000     C                   SETON                                        62
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C*--------------------
025400000000     C* SCELTA STATISTICA FASCE DI COLLI
025500000000     C*--------------------
025600000000IF  1C     $ERR          IFEQ      '0'
025700000000IF  2C     V1CCOL        IFNE      *BLANKS
025800000000     C     V1CCOL        ANDNE     '1'
025900000000     C                   MOVEL     '1'           $ERR
026000000000     C                   MOVEL     MSG(10)       $MSG
026100000000     C                   EXSR      CARMSG
026200000000     C                   SETON                                        63
026300000000E   2C                   ENDIF
026400000000E   1C                   ENDIF
026500000000     C*--------------------
026600000000     C* SCELTA STATISTICA BANCALI
026700000000     C*--------------------
026800000000IF  1C     $ERR          IFEQ      '0'
026900000000IF  2C     V1CBAN        IFNE      *BLANKS
027000000000     C     V1CBAN        ANDNE     '1'
027100000000     C                   MOVEL     '1'           $ERR
027200000000     C                   MOVEL     MSG(10)       $MSG
027300000000     C                   EXSR      CARMSG
027400000000     C                   SETON                                        64
027500000000E   2C                   ENDIF
027600000000E   1C                   ENDIF
027700000614     C*
027800000614     C* CONTROLLO OBBLIGATORIETA' INDICAZIONE FLAG X POSTE
027900000614IF  1C     $ERR          IFEQ      '0'
028000000614IF  2C     VIDPT         IFNE      'S'
028100000614     C     VIDPT         ANDNE     'N'
028200000614     C                   SETON                                        71
028300000614     C                   MOVEL     MSG(9)        $MSG
028400000614     C                   EXSR      CARMSG
028500000614E   2C                   ENDIF
028600000614E   1C                   ENDIF
028700000000     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
029100000000     C*------------------------------------------------------------------------*
029200000000     C     IMPVID        BEGSR
029300080910      * verfiico se utente EDP
029400080910     c                   eval      *in10 = (%subst(knmus: 1: 3) = 'EDP')
029500000614     C*
029600000614     C* IMPOSTO DI DEFAULT IL FLAG POSTE A 'N' ( = BARTOLINI )
029700000614     C                   MOVEL     'N'           VIDPT
029800000000     C*--------------------
029900000000     C* SCELTA STATISTICHE
030000000000     C*--------------------
030100080910     C                   MOVEL     ' '           V1CRIC
030200080910     C                   MOVEL     ' '           V1CCOL
030300000000     C                   MOVEL     '1'           V1CBAN
030400080910     C*--------------------
030500080910     C* CREAZIONE FILE
030600080910     C*--------------------
030700080910     C                   MOVEL     'S'           V1CFIL
030800000000     C*
030900000000     C                   ENDSR
031000020916     C*--------------------------------------------------------------------------------------------*
031100020916     C* REPERISCE I DATI UTENTE
031200020916     C*--------------------------------------------------------------------------------------------*
031300020916     C     REPDATIUTE    BEGSR
031400020916     C*
031500020916     C* INIZIALIZZA VARIABILI DI WRK
031600020916     C                   CLEAR                   TIBS34DS
031700020916     C                   CLEAR                   AZUTEDS
031800020916     C                   CLEAR                   DDATIUTE
031900020916     C*
032000020916     C     *DTAARA       DEFINE    §azute        azuteds
032100020916     C     *DTAARA       DEFINE    §datiute      ddatiute
032200020916     C                   IN(E)     *DTAARA
032300020916if  1C                   IF        %Error
032400020916     c                   EVAL      I34Tla = 'L'
032500020916     C                   CALL      'TIBS34R'
032600020916     C                   PARM                    Tibs34Ds
032700020916     C                   IN        *DTAARA
032800020916e   1C                   ENDIF
032900020916     C*
033000020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033100020916     C                   MOVEL(P)  RSUT          DSFIRS           20
033200020916     C*
033300020916     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500000000     C* *INZSR - IMPOSTAZIONI INIZIALI
033600000000     C*------------------------------------------------------------------------*
033700000000     C     *INZSR        BEGSR
033800000000     C*--------------------
033900000000     C* RICEVIMENTO PARAMETRI
034000000000     C*--------------------
034100000000     C     *ENTRY        PLIST
034200000000     C                   PARM                    KPJBA
034300000000     C*
034400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
034500000000     C                   EXSR      INZSTD
034600000000     C*
034700000000     C* IMPOSTAZIONE INIZIALE VIDEO
034800000000     C                   EXSR      IMPVID
034900000000     C*
035000000000     C* EMESSIONE FORMATO VIDEO
035100000000     C                   EXSR      CARV1
035200000000     C*
035300000000     C                   ENDSR
035400000000     C*------------------------------------------------------------------------*
035500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035700000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
035800000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
035900000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
036000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036200000000     C*------------------------------------------------------------------------*
036300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
036400000000     C*------------------------------------------------------------------------*
036500000000     C     INZSTD        BEGSR
036600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
036700000000     C                   CALL      'XKEYS'
036800000000     C                   PARM                    DSKEYS
036900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
037000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
037100000000     C     XMSGPA        PLIST
037200000000     C                   PARM                    DSMSMS
037300000000     C                   PARM                    DSMSPG
037400000000     C                   PARM                    DSMSKE
037500000000     C                   PARM                    DSMSER
037600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
037700010510     C                   MOVEL     'TISE04R'     DSMSPG                         *NOME PGM CHIAMANTE
037800010510     C                   MOVEL     'TISE04R'     VMCQUE                         *CODA MESSAGGI DESTIN
037900000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
038000000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
038100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
038200000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
038300000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
038400000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
038500000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
038600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
038700000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
038800000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
038900000000     C* LANCIO ROUTINE PER VERIFICARE SE
039000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
039100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
039200020916     C                   EXSR      REPDATIUTE
039300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
039400000000     C                   BITOFF    '01234567'    POSMI1            1
039500000000     C                   BITOFF    '01234567'    POSMI2            1
039600000000     C                   BITOFF    '01234567'    POSMA1            1
039700000000     C                   BITOFF    '01234567'    POSMA2            1
039800000000     C                   BITON     '34'          POSMI1            1            *18
039900000000     C                   BITON     '7'           POSMI2            1            *01
040000000000     C                   BITON     '34'          POSMA1            1            *18
040100000000     C                   BITON     '0'           POSMA2            1            *80
040200000000     C                   MOVE      POSMI2        POSMIN            2
040300000000     C                   MOVEL     POSMI1        POSMIN
040400000000     C                   MOVE      POSMA2        POSMAX            2
040500000000     C                   MOVEL     POSMA1        POSMAX
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
040900000000     C*------------------------------------------------------------------------*
041000000000     C     FUNSTD        BEGSR
041100000000     C                   SELECT
041200000000     C     KEY           WHENEQ    F3
041300000000     C                   EXSR      F03STD
041400000000     C     KEY           WHENEQ    F12
041500000000     C                   EXSR      F12STD
041600000000     C     KEY           WHENEQ    F24
041700000000     C                   EXSR      F24STD
041800000000     C                   OTHER
041900000000     C                   MOVEL     MSG(2)        $MSG
042000000000     C                   EXSR      CARMSG
042100000000     C                   ENDSL
042200000000     C                   ENDSR
042300000000     C*------------------------------------------------------------------------*
042400000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
042500000000     C*------------------------------------------------------------------------*
042600000000     C     F03STD        BEGSR
042700000000     C*
042800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
042900000000     C                   MOVEL     '1'           $FINE
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
043400000000     C*------------------------------------------------------------------------*
043500000000     C     F12STD        BEGSR
043600000000     C*
043700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
043800000000     C                   MOVEL     '1'           $FINE
043900000000     C*
044000000000     C                   ENDSR
044100000000     C*------------------------------------------------------------------------*
044200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
044300000000     C*------------------------------------------------------------------------*
044400000000     C     F24STD        BEGSR
044500000000     C*
044600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
044700000000     C                   EXSR      OUTFUN
044800000000     C*
044900000000     C                   ENDSR
045000000000     C*------------------------------------------------------------------------*
045100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
045200000000     C*          CODA MESSAGGI DEL PGM
045300000000     C*------------------------------------------------------------------------*
045400000000     C     CARMSG        BEGSR
045500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
045600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
045700000000     C     MSGULT        IFEQ      0
045800000000     C                   MOVEL     '1'           *IN40
045900010510     C                   WRITE     SE04DVK
046000000000     C                   MOVEL     '0'           *IN40
046100000000     C                   ENDIF
046200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
046300000000     C                   MOVEL     $MSG          DSMSMS
046400000000     C                   CALL      'XMSG'        XMSGPA
046500000000     C     DSMSER        IFEQ      0
046600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
046700000000     C                   ENDIF
046800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
046900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
047000010510     C                   WRITE     SE04DVM                                      *SFL MESS
047100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
047200000000     C                   MOVEL     *BLANKS       $MSG
047300000000     C                   ENDSR
047400000000     C*------------------------------------------------------------------------*
047500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
047600000000     C*------------------------------------------------------------------------*
047700000000     C     OUTMSG        BEGSR
047800000000     C* VISUALIZZAZIONE MESSAGGIO
047900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
048000010510     C                   WRITE     SE04DVK                                      *CONTR MESS
048100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
048200000000     C                   Z-ADD     *ZEROS        MSGULT
048300000000     C                   ENDSR
048400000000     C*------------------------------------------------------------------------*
048500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
048600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
048700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
048800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
048900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
049000000000     C*------------------------------------------------------------------------*
049100000000     C     CARFUN        BEGSR
049200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
049300000000     C     1             DO        31            I                 5 0          --- 1 -->
049400000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
049500000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
049600000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
049700020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
049800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
049900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
050000000000     C                   ELSE                                                   *INTERROG.
050100000000     C                   MOVEL     FUNASI        FAT(I)
050200000000     C                   ENDIF
050300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
050400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
050500000000     C                   MOVEL     FUNAFG        FAT(I)
050600000000     C                   ELSE                                                   *INTERROG.
050700000000     C                   MOVEL     FUNAFI        FAT(I)
050800000000     C                   ENDIF
050900000000     C                   ENDIF                                                  <-- 2 ---
051000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
051100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
051200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
051300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
051400000000     C                   ELSE
051500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
051600000000     C                   ENDIF
051700000000     C                   ENDIF
051800000000     C                   ENDDO                                                  <-- 1 ---
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
052200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
052300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
052400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
052500000000     C*------------------------------------------------------------------------*
052600000000     C     OUTFUN        BEGSR
052700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
052800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
052900000000     C                   ENDDO
053000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
053100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
053200000000     C     1             DO        31            I
053300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
053400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
053500000000     C                   ADD       1             FUNNAT
053600000000     C                   ENDIF
053700000000     C                   ENDDO
053800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
053900000000     C     FUNNAT        IFGT      8
054000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
054100000000     C                   ENDIF
054200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
054300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
054400000000     C                   Z-ADD     8             FUNVIS            5 0
054500000000     C     FAT(23)       IFEQ      'S'
054600000000     C                   SUB       1             FUNVIS
054700000000     C                   ENDIF
054800000000     C     FAT(24)       IFEQ      'S'
054900000000     C                   SUB       1             FUNVIS
055000000000     C                   ENDIF
055100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
055200000000     C                   Z-ADD     *ZEROS        W                 5 0
055300000000     C                   ADD       1             FUNULT
055400000000     C     FUNULT        IFGT      31
055500000000     C                   Z-ADD     1             FUNULT
055600000000     C                   ENDIF
055700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
055800000000     C     FUNULT        DO        31            I                              --- 1 -->
055900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
056000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
056100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
056200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
056300000000     C     FDE(I)        ANDNE     *BLANKS
056400000000     C     I             IFEQ      23                                           *TOLGO F23
056500000000     C     I             OREQ      24                                           *E F24
056600000000     C                   ELSE
056700000000     C                   ADD       1             W
056800000000     C                   MOVEL     FDE(I)        FUV(W)
056900000000     C                   ENDIF
057000000000     C                   ENDIF                                                  <-- 3 ---
057100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
057200000000     C                   Z-ADD     I             FUNULT
057300000000     C                   ENDIF                                                  <-- 2 ---
057400000000     C                   ENDDO                                                  <-- 1 ---
057500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
057600000000     C                   MOVEL     FUV(1)        VFD001
057700000000     C                   MOVEL     FUV(2)        VFD002
057800000000     C                   MOVEL     FUV(3)        VFD003
057900000000     C                   MOVEL     FUV(4)        VFD004
058000000000     C                   MOVEL     FUV(5)        VFD005
058100000000     C                   MOVEL     FUV(6)        VFD006
058200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
058300000000     C     FAT(24)       ANDEQ     'N'
058400000000     C                   MOVEL     FUV(7)        VFD007
058500000000     C                   MOVEL     FUV(8)        VFD008
058600000000     C                   ELSE                                                   <-- 1 -->
058700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
058800000000     C     FAT(24)       ANDEQ     'S'
058900000000     C                   MOVEL     FDE(23)       VFD007
059000000000     C                   MOVEL     FDE(24)       VFD008
059100000000     C                   ELSE                                                   <-- 2 -->
059200000000     C                   MOVEL     FUV(7)        VFD007
059300000000     C     FAT(23)       IFEQ      'S'
059400000000     C                   MOVEL     FDE(23)       VFD008
059500000000     C                   ELSE
059600000000     C                   MOVEL     FDE(24)       VFD008
059700000000     C                   ENDIF
059800000000     C                   ENDIF                                                  <-- 2 ---
059900000000     C                   ENDIF                                                  <-- 1 ---
060000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
060100000000     C                   ENDSR
060200000000     C*------------------------------------------------------------------------*
060300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
060400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
060500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
060600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
060700000000     C*------------------------------------------------------------------------*
060800000000     C     CONFUN        BEGSR
060900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
061000000000     C     1             DO        31            I                              --- 1 -->
061100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
061200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
061300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
061400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
061500000000     C                   EXSR      CARMSG
061600000000     C                   MOVEL     '1'           $ERR
061700000000     C                   ENDIF                                                  <-- 3 ---
061800000000     C                   ENDIF                                                  <-- 2 ---
061900000000     C                   ENDDO                                                  <-- 1 ---
062000000000     C                   ENDSR
062100000000     O*------------------------------------------------------------------------*
062200000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062300010510TISE04R- Tasto funzionale non valido.                                          1
062400010510TISE04R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
062500010510TISE04R- Opzione non valida.                                                   3
062600010510TISE04R- Opzione abilitata ma non gestita. Avvertire CED.                      4
062700010510TISE04R- Inizio elenco.                                                        5
062800010510TISE04R- Fine elenco.                                                          6
062900010510TISE04R- Anno/mese errato                                                      7
063000010510TISE04R- Anno/mese finale deve essere maggiore/uguale ad Anno/Mese iniziale    8
063100010510TISE04R- Opzione obbligatoria                                                  9
063200010510TISE04R- Scelta errata                                                        10
063300010510TISE04R- Non si possono lanciare le statistiche di gruppo in SDI !!!          11
063400010510TISE04R-12
063500010510TISE04R-13
063600010510TISE04R-14
063700010510TISE04R-15
063800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
063900000000F1    -                   -    NN     NN
064000000000F2    -                   -    NN     NN
064100000000F3    -F3=Fine            -    SS     SS
064200000000F4    -                   -    NN     NN
064300000000F5    -                   -    NN     NN
064400000000F6    -F6=Lancio          -    SS     SS
064500000000F7    -                   -    NN     NN
064600000000F8    -                   -    NN     NN
064700000000F9    -                   -    NN     NN
064800000000F10   -                   -    NN     NN
064900000000F11   -                   -    NN     NN
065000000000F12   -F12-Ritorno        -    SS     SS
065100000000F13   -                   -    NN     NN
065200000000F14   -                   -    NN     NN
065300000000F15   -                   -    NN     NN
065400000000F16   -                   -    NN     NN
065500000000F17   -                   -    NN     NN
065600000000F18   -                   -    NN     NN
065700000000F19   -                   -    NN     NN
065800000000F20   -                   -    NN     NN
065900000000F21   -                   -    NN     NN
066000000000F22   -                   -    NN     NN
066100000000F23   -                   -    NN     NN
066200000000F24   -F24=Altri tasti    -    NN     NN
066300000000ENTER -                   -    SS     SS
066400000000ROLLUP-                   -    NN     NN
066500000000ROLLDN-                   -    NN     NN
066600000000HELP  -                   -    NN     NN
066700000000PRINT -                   -    NN     NN
066800000000BSPACE-                   -    NN     NN
066900000000CLEAR -                   -    NN     NN
