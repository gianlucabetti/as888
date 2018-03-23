000100970722     H*------------------------------------------------------------------------*
000200970723     H* CANCELLAZIONE SALDI IN SEDE - RICHIESTA PARAMETRI                      *
000300970722     H*------------------------------------------------------------------------*
000400970722     H DECEDIT('0,') DATEDIT(*DMY.)
000500970722     F*------------------------------------------------------------------------*
000600970722     F* DATA BASE
000700970722     F*------------------------------------------------------------------------*
000800970722     FTNSS41D   CF   E             WORKSTN INFDS(DEVDS1)
000900970722     F                                     SFILE(SS41DVM:MSGULT)
001000970722     D*------------------------------------------------------------------------*
001100970722     D* SCHIERE
001200970722     D*------------------------------------------------------------------------*
001300970722     D*-------------------
001400970722     D* SCHIERE A TEMPO DI COMPILAZIONE
001500970722     D*-------------------
001600970722     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700970722     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800970722     D*-------------------
001900970722     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000970722     D*-------------------
002100970722     D FFU             S              6    DIM(31)                              *FUNZIONE
002200970722     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300970722     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400970722     D*-------------------
002500970722     D* TABELLA DESCRIZIONI A VIDEO
002600970722     D*-------------------
002700970722     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800970722     D*-------------------
002900970722     D* TABELLA TASTI FUNZIONALI PREMUTI
003000970722     D*-------------------
003100970722     D KEYS            S              1    DIM(40)
003200970722     D*------------------------------------------------------------------------*
003300970722     D* INPUT
003400970722     D*------------------------------------------------------------------------*
003500970722     D*-------------------
003600970722     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700970722     D*-------------------
003800970722     D DSFUN         E DS
003900970722     D  FUNRIG                 1     40
004000970722     D*-------------------
004100970722     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200970722     D*-------------------
004300970722     D DSMSG         E DS
004400970722     D*-------------------
004500970722     D* DS PER GESTIONE TASTI FUNZIONALI
004600970722     D*-------------------
004700970722     D DSKEYS        E DS
004800970722     D  KEYRIG                 1     40
004900970722     D*-------------------
005000970722     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100970722     D*-------------------
005200970722     D                SDS
005300970722     D  USER                 254    263                                         UTENTE
005400970722     D*-------------------
005500970722     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600970722     D*-------------------
005700970722     D DEVDS1          DS
005800970722     D  KEY                  369    369
005900970722     D  CURSOR               370    371
006000970722     D*-------------------
006100020920     D* DS REPERIMENTO DATI UTENTE
006200970722     D*-------------------
006300020920     D TIBS34DS      E DS                                                       *Profili utente
006400020920     D DDATIUTE      E DS                                                       *Dati utente
006500020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006600970722     D*------------------
006700990526     D* DS CONTROLLA DATA (8)
006800970722     D*------------------
006900990526     D WLBDA8          DS                  INZ
007000990526     D  G08DAT                 1      8  0
007100990526     D  G08INV                 9     16  0
007200990526     D  G08ERR                17     17
007300990526     D  G08TGI                18     22  0
007400970723     D*------------------
007500970723     D* DS COMPOSIZIONE DATA GENERICA
007600970723     D*------------------
007700970723     D                 DS
007800990526     D  ANNO                   1      4  0
007900990526     D  MESE                   5      6  0
008000990526     D  DATA                   1      6  0
008100970722     D*------------------
008200970722     D* DS PARAMETRI LANCIO
008300970722     D*------------------
008400970722     D PARAM           DS                  INZ
008500990526     D  PARDAT                 1      6  0
008600990526     D  PARDA2                 7     12  0
008700970722     D*-------------------
008800970722     D* ARCHITETTURA
008900970722     D*-------------------
009000970722     D KPJBA         E DS
009100970722     C*------------------------------------------------------------------------*
009200970722     C* MAIN LINE
009300970722     C*------------------------------------------------------------------------*
009400970722     C*
009500970722     C* CICLO FINO A CHE:
009600970722DO  1C     $FINE         DOUEQ     '1'
009700970722     C*
009800970722     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009900970722     C*
010000970722     C* CONTROLLA GESTIONE VIDEO
010100970722IF  2C     $GES          IFEQ      'V1'
010200970722     C                   EXSR      GESV1                                        *PARAMETRI
010300970722E   2C                   ENDIF
010400970722     C*
010500970722E   1C                   ENDDO
010600970722     C*
010700970722     C                   SETON                                        LR
010800970722     C*------------------------------------------------------------------------*
010900970722     C* GESV1 - GESIONE PRIMO VIDEO
011000970722     C*------------------------------------------------------------------------*
011100970722     C     GESV1         BEGSR
011200970722     C*
011300970722IF  1C     $TESVD        IFEQ      '1'                                          TESTATA DA RIEMETTER
011400970722     C                   EXSR      CARV1                                        *CARICA VIDEO
011500970722E   1C                   ENDIF
011600970722     C*
011700970722IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011800970722     C                   MOVEL     *BLANKS       $MSG
011900970722     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012000970722E   1C                   ENDIF
012100970722     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012200970722     C*
012300970722     C                   EXFMT     SS41DV1                                      *PARAMETRI
012400970722     C*
012500970722     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012600970722     C*
012700970722IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012800970722SL  2C                   SELECT
012900970722     C     KEY           WHENEQ    ENTER
013000970722     C                   EXSR      ENTV1                                        *ENTER--> DECODIFICA
013100970722     C     KEY           WHENEQ    F6
013200970722     C                   EXSR      F06GES                                       *F06 --> LANCIO
013300970722     C                   OTHER
013400970722     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013500970722E   2C                   ENDSL
013600970722E   1C                   ENDIF
013700970722     C*
013800970722     C                   ENDSR
013900970722     C*------------------------------------------------------------------------*
014000970722     C* CARV1 - CARICAMENTO PRIMO VIDEO
014100970722     C*------------------------------------------------------------------------*
014200970722     C     CARV1         BEGSR
014300970722     C*
014400970722     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014500970722     C*
014600970722     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014700970722     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014800970722     C*
014900970722     C                   WRITE     SS41DVT                                      *TESTATA
015000970722     C                   WRITE     SS41DVF                                      *FUNZIONI
015100970722     C*
015200970722     C                   ENDSR
015300970722     C*------------------------------------------------------------------------*
015400970722     C* ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
015500970722     C*------------------------------------------------------------------------*
015600970722     C     ENTV1         BEGSR
015700970722     C*
015800970722     C* CONTROLLA I CAMPI IMMESSI
015900970722     C                   EXSR      DECODI
016000970722     C*
016100970722     C                   ENDSR
016200970722     C*------------------------------------------------------------------------*
016300970722     C* F06GES - GESTIONE TASTO F06 ==> CONFERMA LANCIO
016400970722     C*------------------------------------------------------------------------*
016500970722     C     F06GES        BEGSR
016600970722     C*
016700970722     C* CONTROLLA I CAMPI IMMESSI
016800970722     C                   EXSR      DECODI
016900970722     C*
017000970722     C* LANCIA PROGRAMMA DI GUIDA
017100970722IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017200970723     C                   Z-ADD     V1CDAT        PARDAT                         *AA/MM INIZIALE
017300970723     C                   Z-ADD     V1CDA2        PARDA2                         *AA/MM FINALE
017400970722     C                   MOVEL     'SS42'        KCOAZ
017500970722     C                   MOVEL     PARAM         KPJBU
017600970722     C                   CALL      'BCH10'
017700970722     C                   PARM                    KPJBA
017800970722     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
017900970722E   1C                   ENDIF
018000970722     C*
018100970722     C                   ENDSR
018200970722     C*------------------------------------------------------------------------*
018300970722     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018400970722     C*------------------------------------------------------------------------*
018500970722     C     DECODI        BEGSR
018600970722     C*
018700970723     C* REIMPOSTA INDICATORI DI ERRORE SUI CAMPI
018800970723     C                   SETOFF                                       6061
018900970723     C*--------------------
019000970723     C* ANNO/MESE INIZIALE
019100970723     C*--------------------
019200970723IF  1C     $ERR          IFEQ      '0'
019300990526     C                   MOVEL     V1CDAT        G08INV
019400990526     C                   MOVE      01            G08INV
019500990526     C                   MOVEL     '3'           G08ERR
019600990526     C                   CALL      'XSRDA8'
019700990526     C                   PARM                    WLBDA8
019800990526IF  2C     G08ERR        IFEQ      '1'
019900970723     C                   MOVEL     '1'           $ERR
020000970723     C                   MOVEL     MSG(7)        $MSG
020100970723     C                   EXSR      CARMSG
020200970723     C                   SETON                                        60
020300970723E   2C                   ENDIF
020400970723E   1C                   ENDIF
020500970723     C*--------------------
020600970723     C* ANNO/MESE FINALE
020700970723     C*--------------------
020800970723IF  1C     $ERR          IFEQ      '0'
020900990526     C                   MOVEL     V1CDA2        G08INV
021000990526     C                   MOVE      01            G08INV
021100990526     C                   MOVEL     '3'           G08ERR
021200990526     C                   CALL      'XSRDA8'
021300990526     C                   PARM                    WLBDA8
021400990526IF  2C     G08ERR        IFEQ      '1'
021500970723     C                   MOVEL     '1'           $ERR
021600970723     C                   MOVEL     MSG(7)        $MSG
021700970723     C                   EXSR      CARMSG
021800970723     C                   SETON                                        61
021900970723E   2C                   ENDIF
022000970723E   1C                   ENDIF
022100970723     C*--------------------
022200970723     C* ANNO/MESE INIZIALE + ANNO/MESE FINALE
022300970723     C*--------------------
022400970723     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
022500970723IF  1C     $ERR          IFEQ      '0'
022600970723IF  2C     V1CDA2        IFLT      V1CDAT
022700970723     C                   MOVEL     '1'           $ERR
022800970723     C                   MOVEL     MSG(8)        $MSG
022900970723     C                   EXSR      CARMSG
023000970723     C                   SETON                                        6061
023100970723E   2C                   ENDIF
023200970723E   1C                   ENDIF
023300970723     C*
023400970722     C                   ENDSR
023500020920     C*--------------------------------------------------------------------------------------------*
023600020920     C* REPERISCE I DATI UTENTE
023700020920     C*--------------------------------------------------------------------------------------------*
023800020920     C     REPDATIUTE    BEGSR
023900020920     C*
024000020920     C* INIZIALIZZA VARIABILI DI WRK
024100020920     C                   CLEAR                   TIBS34DS
024200020920     C                   CLEAR                   AZUTEDS
024300020920     C                   CLEAR                   DDATIUTE
024400020920     C*
024500020920     C     *DTAARA       DEFINE    §azute        azuteds
024600020920     C     *DTAARA       DEFINE    §datiute      ddatiute
024700020920     C                   IN(E)     *DTAARA
024800020920if  1C                   IF        %Error
024900020920     c                   EVAL      I34Tla = 'L'
025000020920     C                   CALL      'TIBS34R'
025100020920     C                   PARM                    Tibs34Ds
025200020920     C                   IN        *DTAARA
025300020920e   1C                   ENDIF
025400020920     C*
025500020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
025600020920     C                   MOVEL(P)  RSUT          DSFIRS           20
025700020920     C*
025800020920     C                   ENDSR
025900970722     C*------------------------------------------------------------------------*
026000970722     C* *INZSR - ROUTINE INIZIALE
026100970722     C*------------------------------------------------------------------------*
026200970722     C     *INZSR        BEGSR
026300970722     C*--------------------
026400970722     C* RICEVIMENTO PARAMETRI
026500970722     C*--------------------
026600970722     C     *ENTRY        PLIST
026700970722     C                   PARM                    KPJBA
026800970722     C*
026900970722     C* ESEGUE OPERAZIONI INIZIALI STANDARD
027000970722     C                   EXSR      INZSTD
027100970722     C*
027200970723     C* IMPOSTAZIONI INIZIALI VIDEO
027300970722     C                   EXSR      IMPVID
027400970722     C*
027500970722     C* EMETTE PRIMO FORMATO VIDEO
027600970722     C                   EXSR      CARV1
027700970722     C*
027800970722     C                   ENDSR
027900970723     C*------------------------------------------------------------------------*
028000970723     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
028100970723     C*------------------------------------------------------------------------*
028200970723     C     IMPVID        BEGSR
028300970723     C*---
028400970723     C* DATA FINALE   -> DICEMBRE DI DUE ANNI PRIMA
028500970723     C* DATA INIZIALE -> GENNAIO  DI TRE ANNI PRIMA
028600970723     C*---
028700990526     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
028800990526     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
028900990526     C                   MOVE      N8            N4                4 0          *ANNO (4)
029000990526     C*
029100990526     C     N4            SUB       2             ANNO
029200970723     C                   Z-ADD     12            MESE
029300970723     C                   Z-ADD     DATA          V1CDA2                         *DATA FINALE
029400970723     C*
029500970723     C     ANNO          SUB       1             ANNO
029600970723     C                   Z-ADD     01            MESE
029700970723     C                   Z-ADD     DATA          V1CDAT                         *DATA INIZIALE
029800970723     C*
029900970723     C                   ENDSR
030000970722     C*------------------------------------------------------------------------*
030100970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030200970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030300970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030400970722     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
030500970722     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
030600970722     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
030700970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030800970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
030900970722     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
031000970722     C*------------------------------------------------------------------------*
031100970722     C* INZSTD - ROUTINE INIZIALE STANDARD
031200970722     C*------------------------------------------------------------------------*
031300970722     C     INZSTD        BEGSR
031400970722     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
031500970722     C                   CALL      'XKEYS'
031600970722     C                   PARM                    DSKEYS
031700970722     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
031800970722     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
031900970722     C     XMSGPA        PLIST
032000970722     C                   PARM                    DSMSMS
032100970722     C                   PARM                    DSMSPG
032200970722     C                   PARM                    DSMSKE
032300970722     C                   PARM                    DSMSER
032400970722     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
032500970722     C                   MOVEL     'TNSS41R'     DSMSPG                         *NOME PGM CHIAMANTE
032600970722     C                   MOVEL     'TNSS41R'     VMCQUE                         *CODA MESSAGGI DESTI
032700970722     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
032800970722     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
032900970722     C* INIZIALIZZO FLAG CONTROLLO PGM
033000970722     C                   MOVEL     '0'           $ERR              1            *ERRORE
033100970722     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
033200970722     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
033300970722     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
033400970722     C                   MOVEL     '0'           $FINE             1            *FINE PGM
033500970722     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
033600970722     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
033700970722     C* LANCIO ROUTINE PER VERIFICARE SE
033800970722     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
033900970722     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
034000020920     C                   EXSR      REPDATIUTE
034100970722     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
034200970722     C                   BITOFF    '01234567'    POSMI1            1
034300970722     C                   BITOFF    '01234567'    POSMI2            1
034400970722     C                   BITOFF    '01234567'    POSMA1            1
034500970722     C                   BITOFF    '01234567'    POSMA2            1
034600970722     C                   BITON     '34'          POSMI1            1            *18
034700970722     C                   BITON     '7'           POSMI2            1            *01
034800970722     C                   BITON     '34'          POSMA1            1            *18
034900970722     C                   BITON     '0'           POSMA2            1            *80
035000970722     C                   MOVE      POSMI2        POSMIN            2
035100970722     C                   MOVEL     POSMI1        POSMIN
035200970722     C                   MOVE      POSMA2        POSMAX            2
035300970722     C                   MOVEL     POSMA1        POSMAX
035400970722     C                   ENDSR
035500970722     C*------------------------------------------------------------------------*
035600970722     C* FUNSTD - LANCIA SUBROUTINE STANDARD
035700970722     C*------------------------------------------------------------------------*
035800970722     C     FUNSTD        BEGSR
035900970722     C                   SELECT
036000970722     C     KEY           WHENEQ    F3
036100970722     C                   EXSR      F03STD
036200970722     C     KEY           WHENEQ    F12
036300970722     C                   EXSR      F12STD
036400970722     C     KEY           WHENEQ    F24
036500970722     C                   EXSR      F24STD
036600970722     C                   OTHER
036700970722     C                   MOVEL     MSG(2)        $MSG
036800970722     C                   EXSR      CARMSG
036900970722     C                   ENDSL
037000970722     C                   ENDSR
037100970722     C*------------------------------------------------------------------------*
037200970722     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
037300970722     C*------------------------------------------------------------------------*
037400970722     C     F03STD        BEGSR
037500970722     C*
037600970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
037700970722     C                   MOVEL     '1'           $FINE
037800970722     C*
037900970722     C                   ENDSR
038000970722     C*------------------------------------------------------------------------*
038100970722     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
038200970722     C*------------------------------------------------------------------------*
038300970722     C     F12STD        BEGSR
038400970722     C*
038500970722     C* IMPOSTO FLAG DI FINE PROGRAMMA
038600970722     C                   MOVEL     '1'           $FINE
038700970722     C*
038800970722     C                   ENDSR
038900970722     C*------------------------------------------------------------------------*
039000970722     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
039100970722     C*------------------------------------------------------------------------*
039200970722     C     F24STD        BEGSR
039300970722     C*
039400970722     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
039500970722     C                   EXSR      OUTFUN
039600970722     C*
039700970722     C                   ENDSR
039800970722     C*------------------------------------------------------------------------*
039900970722     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
040000970722     C*          CODA MESSAGGI DEL PGM
040100970722     C*------------------------------------------------------------------------*
040200970722     C     CARMSG        BEGSR
040300970722     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
040400970722     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
040500970722     C     MSGULT        IFEQ      0
040600970722     C                   MOVEL     '1'           *IN40
040700970722     C                   WRITE     SS41DVK
040800970722     C                   MOVEL     '0'           *IN40
040900970722     C                   ENDIF
041000970722     C* INVIA MESSAGGIO ALLA CODA DEL PGM
041100970722     C                   MOVEL     $MSG          DSMSMS
041200970722     C                   CALL      'XMSG'        XMSGPA
041300970722     C     DSMSER        IFEQ      0
041400970722     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
041500970722     C                   ENDIF
041600970722     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
041700970722     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
041800970722     C                   WRITE     SS41DVM                                      *SFL MESS
041900970722     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
042000970722     C                   MOVEL     *BLANKS       $MSG
042100970722     C                   ENDSR
042200970722     C*------------------------------------------------------------------------*
042300970722     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
042400970722     C*------------------------------------------------------------------------*
042500970722     C     OUTMSG        BEGSR
042600970722     C* VISUALIZZAZIONE MESSAGGIO
042700970722     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
042800970722     C                   WRITE     SS41DVK                                      *CONTR MESS
042900970722     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
043000970722     C                   Z-ADD     *ZEROS        MSGULT
043100970722     C                   ENDSR
043200970722     C*------------------------------------------------------------------------*
043300970722     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
043400970722     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
043500970722     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
043600970722     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
043700970722     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
043800970722     C*------------------------------------------------------------------------*
043900970722     C     CARFUN        BEGSR
044000970722     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
044100970722     C     1             DO        31            I                 5 0          --- 1 -->
044200970722     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
044300970722     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
044400970722     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
044500020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
044600970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
044700970722     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
044800970722     C                   ELSE                                                   *INTERROG.
044900970722     C                   MOVEL     FUNASI        FAT(I)
045000970722     C                   ENDIF
045100970722     C                   ELSE                                                   <-- 2 --> *FILIALE
045200970722     C     $TGES         IFEQ      'G'                                          *GESTIONE
045300970722     C                   MOVEL     FUNAFG        FAT(I)
045400970722     C                   ELSE                                                   *INTERROG.
045500970722     C                   MOVEL     FUNAFI        FAT(I)
045600970722     C                   ENDIF
045700970722     C                   ENDIF                                                  <-- 2 ---
045800970722     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
045900970722     C                   MOVEL     USER          WEDP              3            *PROFILO
046000970722     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
046100970722     C                   MOVEL     'S'           FAT(I)                         *ABILITO
046200970722     C                   ELSE
046300970722     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
046400970722     C                   ENDIF
046500970722     C                   ENDIF
046600970722     C                   ENDDO                                                  <-- 1 ---
046700970722     C                   ENDSR
046800970722     C*------------------------------------------------------------------------*
046900970722     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
047000970722     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
047100970722     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
047200970722     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
047300970722     C*------------------------------------------------------------------------*
047400970722     C     OUTFUN        BEGSR
047500970722     C     1             DO        8             I                              *PULIZIA FUNZIONI A
047600970722     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
047700970722     C                   ENDDO
047800970722     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
047900970722     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
048000970722     C     1             DO        31            I
048100970722     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
048200970722     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
048300970722     C                   ADD       1             FUNNAT
048400970722     C                   ENDIF
048500970722     C                   ENDDO
048600970722     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
048700970722     C     FUNNAT        IFGT      8
048800970722     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
048900970722     C                   ENDIF
049000970722     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
049100970722     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
049200970722     C                   Z-ADD     8             FUNVIS            5 0
049300970722     C     FAT(23)       IFEQ      'S'
049400970722     C                   SUB       1             FUNVIS
049500970722     C                   ENDIF
049600970722     C     FAT(24)       IFEQ      'S'
049700970722     C                   SUB       1             FUNVIS
049800970722     C                   ENDIF
049900970722     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
050000970722     C                   Z-ADD     *ZEROS        W                 5 0
050100970722     C                   ADD       1             FUNULT
050200970722     C     FUNULT        IFGT      31
050300970722     C                   Z-ADD     1             FUNULT
050400970722     C                   ENDIF
050500970722     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
050600970722     C     FUNULT        DO        31            I                              --- 1 -->
050700970722     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
050800970722     C     W             IFLT      FUNVIS                                       --- 2 -->
050900970722     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
051000970722     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
051100970722     C     FDE(I)        ANDNE     *BLANKS
051200970722     C     I             IFEQ      23                                           *TOLGO F23
051300970722     C     I             OREQ      24                                           *E F24
051400970722     C                   ELSE
051500970722     C                   ADD       1             W
051600970722     C                   MOVEL     FDE(I)        FUV(W)
051700970722     C                   ENDIF
051800970722     C                   ENDIF                                                  <-- 3 ---
051900970722     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
052000970722     C                   Z-ADD     I             FUNULT
052100970722     C                   ENDIF                                                  <-- 2 ---
052200970722     C                   ENDDO                                                  <-- 1 ---
052300970722     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
052400970722     C                   MOVEL     FUV(1)        VFD001
052500970722     C                   MOVEL     FUV(2)        VFD002
052600970722     C                   MOVEL     FUV(3)        VFD003
052700970722     C                   MOVEL     FUV(4)        VFD004
052800970722     C                   MOVEL     FUV(5)        VFD005
052900970722     C                   MOVEL     FUV(6)        VFD006
053000970722     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
053100970722     C     FAT(24)       ANDEQ     'N'
053200970722     C                   MOVEL     FUV(7)        VFD007
053300970722     C                   MOVEL     FUV(8)        VFD008
053400970722     C                   ELSE                                                   <-- 1 -->
053500970722     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
053600970722     C     FAT(24)       ANDEQ     'S'
053700970722     C                   MOVEL     FDE(23)       VFD007
053800970722     C                   MOVEL     FDE(24)       VFD008
053900970722     C                   ELSE                                                   <-- 2 -->
054000970722     C                   MOVEL     FUV(7)        VFD007
054100970722     C     FAT(23)       IFEQ      'S'
054200970722     C                   MOVEL     FDE(23)       VFD008
054300970722     C                   ELSE
054400970722     C                   MOVEL     FDE(24)       VFD008
054500970722     C                   ENDIF
054600970722     C                   ENDIF                                                  <-- 2 ---
054700970722     C                   ENDIF                                                  <-- 1 ---
054800970722     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
054900970722     C                   ENDSR
055000970722     C*------------------------------------------------------------------------*
055100970722     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
055200970722     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
055300970722     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
055400970722     C*                   - ERRORE SE FUNZIONE NON ATTIVA
055500970722     C*------------------------------------------------------------------------*
055600970722     C     CONFUN        BEGSR
055700970722     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
055800970722     C     1             DO        31            I                              --- 1 -->
055900970722     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
056000970722     C* CONTROLLO SE IL TASTO E' ATTIVO
056100970722     C     FAT(I)        IFNE      'S'                                          --- 3 -->
056200970722     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
056300970722     C                   EXSR      CARMSG
056400970722     C                   MOVEL     '1'           $ERR
056500970722     C                   ENDIF                                                  <-- 3 ---
056600970722     C                   ENDIF                                                  <-- 2 ---
056700970722     C                   ENDDO                                                  <-- 1 ---
056800970722     C                   ENDSR
056900970722     C*------------------------------------------------------------------------*
057000970722**   SCHIERA: MSG  (MESSAGGI)
057100970722TNSS41R- Tasto funzionale non valido.                                          1
057200970722TNSS41R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
057300970722TNSS41R- Opzione non valida.                                                   3
057400970722TNSS41R- Opzione abilitata ma non gestita. Avvertire CED.                      4
057500970722TNSS41R- Inizio elenco.                                                        5
057600970722TNSS41R- Fine elenco.                                                          6
057700970723TNSS41R- Anno/Mese errato                                                      7
057800970723TNSS41R- Anno/Mese finale inferiore all'Anno/Mese iniziale                     8
057900980403TNSS41R- 9
058000980403TNSS41R- Non si può lanciare questo programma in SDI !!!                      10
058100970722** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
058200970722F1    -                   -    NN     NN
058300970722F2    -                   -    NN     NN
058400970722F3    -F3=Fine            -    SS     SS
058500970722F4    -                   -    NN     NN
058600970722F5    -                   -    NN     NN
058700970724F6    -F6=Conferma        -    SS     SS
058800970722F7    -                   -    NN     NN
058900970722F8    -                   -    NN     NN
059000970722F9    -                   -    NN     NN
059100970722F10   -                   -    NN     NN
059200970722F11   -                   -    NN     NN
059300970722F12   -F12=Ritorno        -    SS     SS
059400970722F13   -                   -    NN     NN
059500970722F14   -                   -    NN     NN
059600970722F15   -                   -    NN     NN
059700970722F16   -                   -    NN     NN
059800970722F17   -                   -    NN     NN
059900970722F18   -                   -    NN     NN
060000970722F19   -                   -    NN     NN
060100970722F20   -                   -    NN     NN
060200970722F21   -                   -    NN     NN
060300970722F22   -                   -    NN     NN
060400970722F23   -                   -    NN     NN
060500970722F24   -F24=Altri tasti    -    NN     NN
060600970722ENTER -                   -    SS     SS
060700970722ROLLUP-                   -    NN     NN
060800970722ROLLDN-                   -    NN     NN
060900970722HELP  -                   -    NN     NN
061000970722PRINT -                   -    NN     NN
061100970722BSPACE-                   -    NN     NN
061200970722CLEAR -                   -    NN     NN
