000100970903     H*------------------------------------------------------------------------*
000200970903     H* PULIZIA VOCI E TARIFFE DI CONTO ECONOMICO - RICHIESTA PARAMETRI
000300970903     H*------------------------------------------------------------------------*
000400970903     H DECEDIT('0,') DATEDIT(*DMY.)
000500970903     F*------------------------------------------------------------------------*
000600970903     F* DATA BASE
000700970903     F*------------------------------------------------------------------------*
000800970903     FTREC84D   CF   E             WORKSTN INFDS(DEVDS1)
000900970903     F                                     SFILE(EC84DVM:MSGULT)
001000970903     D*------------------------------------------------------------------------*
001100970903     D* SCHIERE
001200970903     D*------------------------------------------------------------------------*
001300970903     D MSG             S             78    DIM(15) CTDATA PERRCD(1)
001400970903     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001500970903     D FFU             S              6    DIM(31)
001600970903     D FDE             S             19    DIM(31)
001700970903     D FAT             S              1    DIM(31)
001800970903     D FUV             S             19    DIM(8)
001900970903     D KEYS            S              1    DIM(40)
002000970903     D*------------------------------------------------------------------------*
002100970903     D* INPUT
002200970903     D*------------------------------------------------------------------------*
002300970903     D*-------------------
002400970903     D* RIDEFINIZIONE FUNZIONI-FUD-
002500970903     D*-------------------
002600970903     D DSFUN         E DS
002700970903     D  FUNRIG                 1     40
002800970903     D*-------------------
002900970903     D* DS "XMSG" - GESTIONE MESSAGGI PGM
003000970903     D*-------------------
003100970903     D DSMSG         E DS
003200970903     D*-------------------
003300970903     D* DS GESTIONE TASTI FUNZIONALI
003400970903     D*-------------------
003500970903     D DSKEYS        E DS
003600970903     D  KEYRIG                 1     40
003700970903     D*-------------------
003800970903     D* INFDS - IINFORMAZIONI PROGRAMMA
003900970903     D*-------------------
004000970903     D                SDS
004100970903     D  USER                 254    263
004200970903     D*-------------------
004300970903     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
004400970903     D*-------------------
004500970903     D DEVDS1          DS
004600970903     D  KEY                  369    369
004700970903     D  CURSOR               370    371
004800970903     D*-------------------
004900020923     D* DS REPERIMENTO DATI UTENTE
005000020923     D*-------------------
005100020923     D TIBS34DS      E DS
005200020923     D DDATIUTE      E DS
005300020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005400970903     D*-------------------
005500970903     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600970903     D*-------------------
005700970903     D WLBDA8          DS                  INZ
005800970903     D  G08DAT                 1      8  0
005900970903     D  G08INV                 9     16  0
006000970903     D  G08ERR                17     17
006100970903     D  G08TGI                18     22  0
006200970903     D*-------------------
006300970903     D* DS PARAMETRI DI LANCIO
006400970903     D*-------------------
006500970903     D PARAM           DS                  INZ
006600970903     D  PARDAT                 1      8  0
006700970903     D*-------------------
006800970903     D* ARCHITETTURA
006900970903     D*-------------------
007000970903     D KPJBA         E DS
007100970903     C*------------------------------------------------------------------------*
007200970903     C* MAIN LINES
007300970903     C*------------------------------------------------------------------------*
007400970903     C*
007500970903     C* CICLO FINO A FINE PGM
007600970903DO  1C     $FINE         DOUEQ     '1'
007700970903     C                   MOVEL     '0'           $ERR
007800970903     C*
007900970903     C* CONTROLLA GESTIONE VIDEO
008000970903IF  2C     $GES          IFEQ      'V1'
008100970903     C                   EXSR      GESV1
008200970903E   2C                   ENDIF
008300970903E   1C                   ENDDO
008400970903     C*
008500970903     C                   SETON                                        LR
008600970903     C*------------------------------------------------------------------------*
008700970903     C* GESV1 - GESTIONE PRIMO VIDEO
008800970903     C*------------------------------------------------------------------------*
008900970903     C     GESV1         BEGSR
009000970903     C*
009100970903IF  1C     $TESVD        IFEQ      '1'
009200970903     C                   EXSR      CARV1
009300970903E   1C                   ENDIF
009400970903     C*
009500970903IF  1C     MSGULT        IFEQ      *ZEROS
009600970903     C                   MOVEL     *BLANKS       $MSG
009700970903     C                   EXSR      CARMSG
009800970903E   1C                   ENDIF
009900970903     C                   EXSR      OUTMSG
010000970903     C*
010100970903     C                   EXFMT     EC84DV1
010200970903     C*
010300970903     C                   EXSR      CONFUN
010400970903     C*
010500970903IF  1C     $ERR          IFEQ      '0'
010600970903SL  2C                   SELECT
010700970903     C     KEY           WHENEQ    ENTER
010800970903     C                   EXSR      ENTV1
010900970903     C     KEY           WHENEQ    F6
011000970903     C                   EXSR      F06GES
011100970903     C                   OTHER
011200970903     C                   EXSR      FUNSTD
011300970903E   2C                   ENDSL
011400970903E   1C                   ENDIF
011500970903     C*
011600970903     C                   ENDSR
011700970903     C*------------------------------------------------------------------------*
011800970903     C* CARV1 - CARICA PRIMO VIDEO
011900970903     C*------------------------------------------------------------------------*
012000970903     C     CARV1         BEGSR
012100970903     C*
012200970903     C                   MOVEL     'V1'          $GES
012300970903     C*
012400970903     C                   EXSR      CARFUN
012500970903     C                   EXSR      OUTFUN
012600970903     C*
012700970903     C                   WRITE     EC84DVT
012800970903     C                   WRITE     EC84DVF
012900970903     C*
013000970903     C                   ENDSR
013100970903     C*------------------------------------------------------------------------*
013200970903     C* ENTV1 - GESTIONE TASTO ENTER
013300970903     C*------------------------------------------------------------------------*
013400970903     C     ENTV1         BEGSR
013500970903     C*
013600970903     C* CONTROLLA CAMPI IMMESSI
013700970903     C                   EXSR      DECODI
013800970903     C*
013900970903     C                   ENDSR
014000970903     C*------------------------------------------------------------------------*
014100970903     C* F06GES - GESTIONE TASTO FUNZIONALE ==> CONFERMA LANCIO
014200970903     C*------------------------------------------------------------------------*
014300970903     C     F06GES        BEGSR
014400970903     C*
014500970903     C* CONTROLLA I CAMPI IMMESSI
014600970903     C                   EXSR      DECODI
014700970903     C*
014800970903     C* LANCIA IL PROGRAMMA
014900970903IF  1C     $ERR          IFEQ      '0'
015000970903     C                   Z-ADD     DATV1C        PARDAT
015100970903     C                   MOVEL     'EC85'        KCOAZ
015200970903     C                   MOVEL     PARAM         KPJBU
015300970903     C                   CALL      'BCH10'
015400970903     C                   PARM                    KPJBA
015500970903     C*
015600970903     C                   MOVEL     '1'           $FINE
015700970903E   1C                   ENDIF
015800970903     C*
015900970903     C                   ENDSR
016000970903     C*------------------------------------------------------------------------*
016100970903     C* DECODI - CONTROLLA CAMPI E DECODIFICA
016200970903     C*------------------------------------------------------------------------*
016300970903     C     DECODI        BEGSR
016400970903     C*
016500970903     C* REIMPOSTA VARIABILI DI LAVORO
016600970903     C                   SETOFF                                       60
016700970903     C*--------------------
016800970903     C* DATA
016900970903     C*--------------------
017000970903     C     *LIKE         DEFINE    V1CDAT        DATV1C
017100970903     C                   Z-ADD     V1CDAT        G08DAT
017200970903     C                   Z-ADD     *ZEROS        G08INV
017300970903     C                   MOVEL     '0'           G08ERR
017400970903     C                   CALL      'XSRDA8'
017500970903     C                   PARM                    WLBDA8
017600970903     C                   Z-ADD     G08DAT        V1CDAT
017700970903     C                   Z-ADD     G08INV        DATV1C
017800970903IF  1C     G08ERR        IFEQ      '1'
017900970903     C                   SETON                                        60
018000970903     C                   MOVEL     '1'           $ERR
018100970903     C                   MOVEL     MSG(7)        $MSG
018200970903     C                   EXSR      CARMSG
018300970903E   1C                   ENDIF
018400970903     C*
018500970903     C                   ENDSR
018600970903     C*------------------------------------------------------------------------*
018700970903     C* IMPVID - IMPOSTAZIONI VIDEO INIZIALI
018800970903     C*------------------------------------------------------------------------*
018900970903     C     IMPVID        BEGSR
019000970903     C*
019100970903     C                   ENDSR
019200020923     C*-------------------------------------------------------------------------
019300020923     C* REPERISCE I DATI UTENTE
019400020923     C*-------------------------------------------------------------------------
019500020923     C     REPDATIUTE    BEGSR
019600020923     C*
019700020923     C* INIZIALIZZA VARIABILI DI WRK
019800020923     C                   CLEAR                   TIBS34DS
019900020923     C                   CLEAR                   AZUTEDS
020000020923     C                   CLEAR                   DDATIUTE
020100020923     C*
020200020923     C     *DTAARA       DEFINE    §azute        azuteds
020300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
020400020923     C                   IN(E)     *DTAARA
020500020923if  1C                   IF        %Error
020600020923     c                   EVAL      I34Tla = 'L'
020700020923     C                   CALL      'TIBS34R'
020800020923     C                   PARM                    Tibs34Ds
020900020923     C                   IN        *DTAARA
021000020923e   1C                   ENDIF
021100020923     C*
021200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
021300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
021400020923     C*
021500020923     C                   ENDSR
021600970903     C*------------------------------------------------------------------------*
021700970903     C* *INZSR  - OPERAZIONI INIZIALI
021800970903     C*------------------------------------------------------------------------*
021900970903     C     *INZSR        BEGSR
022000970903     C*--------------------
022100970903     C* RICEVIMENTO PARAMETRI
022200970903     C*--------------------
022300970903     C     *ENTRY        PLIST
022400970903     C                   PARM                    KPJBA
022500970903     C*
022600970903     C* OPERAZIONI INIZIALI STANDARD
022700970903     C                   EXSR      INZSTD
022800970903     C*
022900970903     C* IMPOSTAZIONI INIZIALI VIDEO
023000970903     C                   EXSR      IMPVID
023100970903     C*
023200970903     C* EMISSIONE FORMATO VIDEO
023300970903     C                   EXSR      CARV1
023400970903     C*
023500970903     C                   ENDSR
023600970903     C*------------------------------------------------------------------------*
023700970903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
023800970903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
023900970903     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
024000970903     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
024100970903     C* MODIFICARLA, NEL CASO FARE ATTENZIONE.
024200970903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024300970903     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
024400970903     C*------------------------------------------------------------------------*
024500970903     C* INZSTD - ROUTINE INIZIALE STANDARD
024600970903     C*------------------------------------------------------------------------*
024700970903     C     INZSTD        BEGSR
024800970903     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
024900970903     C                   CALL      'XKEYS'
025000970903     C                   PARM                    DSKEYS
025100970903     C                   MOVEA     KEYRIG        KEYS
025200970903     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
025300970903     C     XMSGPA        PLIST
025400970903     C                   PARM                    DSMSMS
025500970903     C                   PARM                    DSMSPG
025600970903     C                   PARM                    DSMSKE
025700970903     C                   PARM                    DSMSER
025800970903     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
025900970903     C                   MOVEL     'TREC84R'     DSMSPG
026000970903     C                   MOVEL     'TREC84R'     VMCQUE
026100970903     C                   MOVEL     *BLANKS       VMCKEY
026200970903     C                   MOVEL     *BLANKS       DSMSMS
026300970903     C* INIZIALIZZO FLAG CONTROLLO PGM
026400970903     C                   MOVEL     '0'           $ERR              1
026500970903     C                   MOVEL     '0'           $F03              1
026600970903     C                   MOVEL     *BLANKS       $GES              2
026700970903     C                   MOVEL     *BLANKS       $MSG             80
026800970903     C                   MOVEL     '0'           $FINE             1
026900970903     C                   MOVEL     '1'           $TESVD            1
027000970903     C                   MOVEL     'I'           $TGES             1
027100970903     C* LANCIO ROUTINE PER VERIFICARE SE
027200970903     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
027300970903     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
027400020923     C                   EXSR      REPDATIUTE
027500970903     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
027600970903     C                   BITOFF    '01234567'    POSMI1            1
027700970903     C                   BITOFF    '01234567'    POSMI2            1
027800970903     C                   BITOFF    '01234567'    POSMA1            1
027900970903     C                   BITOFF    '01234567'    POSMA2            1
028000970903     C                   BITON     '34'          POSMI1            1
028100970903     C                   BITON     '7'           POSMI2            1
028200970903     C                   BITON     '34'          POSMA1            1
028300970903     C                   BITON     '0'           POSMA2            1
028400970903     C                   MOVE      POSMI2        POSMIN            2
028500970903     C                   MOVEL     POSMI1        POSMIN
028600970903     C                   MOVE      POSMA2        POSMAX            2
028700970903     C                   MOVEL     POSMA1        POSMAX
028800970903     C                   ENDSR
028900970903     C*------------------------------------------------------------------------*
029000970903     C* FUNSTD - LANCIA SUBROUTINE STANDARD
029100970903     C*------------------------------------------------------------------------*
029200970903     C     FUNSTD        BEGSR
029300970903     C                   SELECT
029400970903     C     KEY           WHENEQ    F3
029500970903     C                   EXSR      F03STD
029600970903     C     KEY           WHENEQ    F12
029700970903     C                   EXSR      F12STD
029800970903     C     KEY           WHENEQ    F24
029900970903     C                   EXSR      F24STD
030000970903     C                   OTHER
030100970903     C                   MOVEL     MSG(2)        $MSG
030200970903     C                   EXSR      CARMSG
030300970903     C                   ENDSL
030400970903     C                   ENDSR
030500970903     C*------------------------------------------------------------------------*
030600970903     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
030700970903     C*------------------------------------------------------------------------*
030800970903     C     F03STD        BEGSR
030900970903     C*
031000970903     C* IMPOSTO FLAG DI FINE PROGRAMMA
031100970903     C                   MOVEL     '1'           $FINE
031200970903     C*
031300970903     C                   ENDSR
031400970903     C*------------------------------------------------------------------------*
031500970903     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
031600970903     C*------------------------------------------------------------------------*
031700970903     C     F12STD        BEGSR
031800970903     C*
031900970903     C* IMPOSTO FLAG DI FINE PROGRAMMA
032000970903     C                   MOVEL     '1'           $FINE
032100970903     C*
032200970903     C                   ENDSR
032300970903     C*------------------------------------------------------------------------*
032400970903     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
032500970903     C*------------------------------------------------------------------------*
032600970903     C     F24STD        BEGSR
032700970903     C*
032800970903     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
032900970903     C                   EXSR      OUTFUN
033000970903     C*
033100970903     C                   ENDSR
033200970903     C*------------------------------------------------------------------------*
033300970903     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
033400970903     C*          CODA MESSAGGI DEL PGM
033500970903     C*------------------------------------------------------------------------*
033600970903     C     CARMSG        BEGSR
033700970903     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
033800970903     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
033900970903     C     MSGULT        IFEQ      0
034000970903     C                   MOVEL     '1'           *IN40
034100970903     C                   WRITE     EC84DVK
034200970903     C                   MOVEL     '0'           *IN40
034300970903     C                   ENDIF
034400970903     C* INVIA MESSAGGIO ALLA CODA DEL PGM
034500970903     C                   MOVEL     $MSG          DSMSMS
034600970903     C                   CALL      'XMSG'        XMSGPA
034700970903     C     DSMSER        IFEQ      0
034800970903     C                   MOVEL     DSMSKE        VMCKEY
034900970903     C                   ENDIF
035000970903     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
035100970903     C                   ADD       1             MSGULT            4 0
035200970903     C                   WRITE     EC84DVM
035300970903     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
035400970903     C                   MOVEL     *BLANKS       $MSG
035500970903     C                   ENDSR
035600970903     C*------------------------------------------------------------------------*
035700970903     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
035800970903     C*------------------------------------------------------------------------*
035900970903     C     OUTMSG        BEGSR
036000970903     C* VISUALIZZAZIONE MESSAGGIO
036100970903     C                   MOVEL     '1'           *IN44
036200970903     C                   WRITE     EC84DVK
036300970903     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
036400970903     C                   Z-ADD     *ZEROS        MSGULT
036500970903     C                   ENDSR
036600970903     C*------------------------------------------------------------------------*
036700970903     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
036800970903     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
036900970903     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
037000970903     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
037100970903     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
037200970903     C*------------------------------------------------------------------------*
037300970903     C     CARFUN        BEGSR
037400970903     C                   Z-ADD     *ZEROS        FUNULT            5 0
037500970903     C     1             DO        31            I                 5 0
037600970903     C                   MOVEL     FUN(I)        FUNRIG
037700970903     C                   MOVEL     FUNFUN        FFU(I)
037800970903     C                   MOVEL     FUNDES        FDE(I)
037900020923     C     DUTLPO        IFEQ      'S'
038000970903     C     $TGES         IFEQ      'G'
038100970903     C                   MOVEL     FUNASG        FAT(I)
038200970903     C                   ELSE
038300970903     C                   MOVEL     FUNASI        FAT(I)
038400970903     C                   ENDIF
038500970903     C                   ELSE
038600970903     C     $TGES         IFEQ      'G'
038700970903     C                   MOVEL     FUNAFG        FAT(I)
038800970903     C                   ELSE
038900970903     C                   MOVEL     FUNAFI        FAT(I)
039000970903     C                   ENDIF
039100970903     C                   ENDIF
039200970903     C     FAT(I)        IFEQ      'E'
039300970903     C                   MOVEL     USER          WEDP              3
039400970903     C     WEDP          IFEQ      'EDP'
039500970903     C                   MOVEL     'S'           FAT(I)
039600970903     C                   ELSE
039700970903     C                   MOVEL     'N'           FAT(I)
039800970903     C                   ENDIF
039900970903     C                   ENDIF
040000970903     C                   ENDDO
040100970903     C                   ENDSR
040200970903     C*------------------------------------------------------------------------*
040300970903     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
040400970903     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
040500970903     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
040600970903     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
040700970903     C*------------------------------------------------------------------------*
040800970903     C     OUTFUN        BEGSR
040900970903     C     1             DO        8             I
041000970903     C                   MOVEL     *BLANKS       FUV(I)
041100970903     C                   ENDDO
041200970903     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
041300970903     C                   Z-ADD     *ZEROS        FUNNAT            2 0
041400970903     C     1             DO        31            I
041500970903     C     FAT(I)        IFEQ      'S'
041600970903     C     FDE(I)        ANDNE     *BLANKS
041700970903     C                   ADD       1             FUNNAT
041800970903     C                   ENDIF
041900970903     C                   ENDDO
042000970903     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
042100970903     C     FUNNAT        IFGT      8
042200970903     C                   MOVEL     'S'           FAT(24)
042300970903     C                   ENDIF
042400970903     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
042500970903     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
042600970903     C                   Z-ADD     8             FUNVIS            5 0
042700970903     C     FAT(23)       IFEQ      'S'
042800970903     C                   SUB       1             FUNVIS
042900970903     C                   ENDIF
043000970903     C     FAT(24)       IFEQ      'S'
043100970903     C                   SUB       1             FUNVIS
043200970903     C                   ENDIF
043300970903     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
043400970903     C                   Z-ADD     *ZEROS        W                 5 0
043500970903     C                   ADD       1             FUNULT
043600970903     C     FUNULT        IFGT      31
043700970903     C                   Z-ADD     1             FUNULT
043800970903     C                   ENDIF
043900970903     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
044000970903     C     FUNULT        DO        31            I
044100970903     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
044200970903     C     W             IFLT      FUNVIS
044300970903     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
044400970903     C     FAT(I)        IFEQ      'S'
044500970903     C     FDE(I)        ANDNE     *BLANKS
044600970903     C     I             IFEQ      23
044700970903     C     I             OREQ      24
044800970903     C                   ELSE
044900970903     C                   ADD       1             W
045000970903     C                   MOVEL     FDE(I)        FUV(W)
045100970903     C                   ENDIF
045200970903     C                   ENDIF
045300970903     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
045400970903     C                   Z-ADD     I             FUNULT
045500970903     C                   ENDIF
045600970903     C                   ENDDO
045700970903     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
045800970903     C                   MOVEL     FUV(1)        VFD001
045900970903     C                   MOVEL     FUV(2)        VFD002
046000970903     C                   MOVEL     FUV(3)        VFD003
046100970903     C                   MOVEL     FUV(4)        VFD004
046200970903     C                   MOVEL     FUV(5)        VFD005
046300970903     C                   MOVEL     FUV(6)        VFD006
046400970903     C     FAT(23)       IFEQ      'N'
046500970903     C     FAT(24)       ANDEQ     'N'
046600970903     C                   MOVEL     FUV(7)        VFD007
046700970903     C                   MOVEL     FUV(8)        VFD008
046800970903     C                   ELSE
046900970903     C     FAT(23)       IFEQ      'S'
047000970903     C     FAT(24)       ANDEQ     'S'
047100970903     C                   MOVEL     FDE(23)       VFD007
047200970903     C                   MOVEL     FDE(24)       VFD008
047300970903     C                   ELSE
047400970903     C                   MOVEL     FUV(7)        VFD007
047500970903     C     FAT(23)       IFEQ      'S'
047600970903     C                   MOVEL     FDE(23)       VFD008
047700970903     C                   ELSE
047800970903     C                   MOVEL     FDE(24)       VFD008
047900970903     C                   ENDIF
048000970903     C                   ENDIF
048100970903     C                   ENDIF
048200970903     C                   MOVEL     '0'           $TESVD
048300970903     C                   ENDSR
048400970903     C*------------------------------------------------------------------------*
048500970903     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
048600970903     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
048700970903     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
048800970903     C*                   - ERRORE SE FUNZIONE NON ATTIVA
048900970903     C*------------------------------------------------------------------------*
049000970903     C     CONFUN        BEGSR
049100970903     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
049200970903     C     1             DO        31            I
049300970903     C     KEY           IFEQ      KEYS(I)
049400970903     C* CONTROLLO SE IL TASTO E' ATTIVO
049500970903     C     FAT(I)        IFNE      'S'
049600970903     C                   MOVEL     MSG(1)        $MSG
049700970903     C                   EXSR      CARMSG
049800970903     C                   MOVEL     '1'           $ERR
049900970903     C                   ENDIF
050000970903     C                   ENDIF
050100970903     C                   ENDDO
050200970903     C                   ENDSR
050300970903     8*------------------------------------------------------------------------*
050400970903** MSG - MESSAGGI
050500970903TREC84R- Tasto funzionale non valido.                                          1
050600970903TREC84R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
050700970903TREC84R- Opzione non valida.                                                   3
050800970903TREC84R- Opzione abilitata ma non gestita. Avvertire CED.                      4
050900970903TREC84R- Inizio elenco.                                                        5
051000970903TREC84R- Fine elenco.                                                          6
051100970903TREC84R- Data errata                                                           7
051200970903TREC84R- 8
051300970903TREC84R- 9
051400970903TREC84R- 10
051500970903TREC84R- 11
051600970903TREC84R- 12
051700970903TREC84R- 13
051800970903TREC84R- 14
051900970903TREC84R- 15
052000970903** FUN - FUNZIONI         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROGAZIONE)
052100970903F1    -                   -    NN     NN
052200970903F2    -                   -    NN     NN
052300970903F3    -F3=Fine            -    SS     SS
052400970903F4    -                   -    NN     NN
052500970903F5    -                   -    NN     NN
052600970903F6    -F6=Lancio          -    SS     SS
052700970903F7    -                   -    NN     NN
052800970903F8    -                   -    NN     NN
052900970903F9    -                   -    NN     NN
053000970903F10   -                   -    NN     NN
053100970903F11   -                   -    NN     NN
053200970903F12   -F12=Ritorno        -    SS     SS
053300970903F13   -                   -    NN     NN
053400970903F14   -                   -    NN     NN
053500970903F15   -                   -    NN     NN
053600970903F16   -                   -    NN     NN
053700970903F17   -                   -    NN     NN
053800970903F18   -                   -    NN     NN
053900970903F19   -                   -    NN     NN
054000970903F20   -                   -    NN     NN
054100970903F21   -                   -    NN     NN
054200970903F22   -                   -    NN     NN
054300970903F23   -                   -    NN     NN
054400970903F24   -                   -    NN     NN
054500970903ENTER -                   -    SS     SS
054600970903ROLLUP-                   -    NN     NN
054700970903ROLLDN-                   -    NN     NN
054800970903HELP  -                   -    NN     NN
054900970903PRINT -                   -    NN     NN
055000970903BSPACE-                   -    NN     NN
055100970903CLEAR -                   -    NN     NN
