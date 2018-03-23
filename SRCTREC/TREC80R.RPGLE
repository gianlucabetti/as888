000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC80R
000300000000      *  Descrizione   :  Stampa competenze di conto economico
000400000000      *                   Programma filtro per la stampa
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  14 GIU 1994
000700000224      *----------------:-----------------------------------------------
000800000224      *  MODIFICA      :
000900000224      *----------------:
001000000224      *  Autore        :  FABRIZIO(FG)
001100000224      *  Data          :  24 FEB 2000
001200000224     F*------------------------------------------------------------------------*
001300000000     FTREC80D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(EC80DVM:MSGULT)
001500000000     F*---------------------------------------------------------------*
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001900000000     D FFU             S              6    DIM(31)
002000000000     D FDE             S             19    DIM(31)
002100000000     D FAT             S              1    DIM(31)
002200000000     D* TABELLA DESCRIZIONI A VIDEO
002300000000     D FUV             S             19    DIM(8)
002400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002500000000     D KEYS            S              1    DIM(40)
002600000000     D*---------------------------------------------------------------*
002700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002800000000     D DSFUN         E DS
002900000000     D  FUNRIG                 1     40
003000000000     D*---------------------------------------------------------------*
003100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003200000000     D DSMSG         E DS
003300000000     D*---------------------------------------------------------------*
003400020923     D* DS REPERIMENTO DATI UTENTE
003500020923     D TIBS34DS      E DS
003600020923     D DDATIUTE      E DS
003700020923     D AZUTEDS       E DS                  extname(AZUTE00F)
003800000000     D*---------------------------------------------------------------*
003900000224     D WLBDA8          DS                  INZ
004000000000     D  G08DAT                 1      8  0
004100000000     D  G08INV                 9     16  0
004200000000     D  G08ERR                17     17
004300000000     D  G08TGI                18     22  0
004400000000     D*---------------------------------------------------------------*
004500000000     D* DS PER GESTIONE TASTI FUNZIONALI
004600000000     D DSKEYS        E DS
004700000000     D  KEYRIG                 1     40
004800000000     D*---------------------------------------------------------------*
004900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005000000000     D                SDS
005100000000     D* NOME UTENTE
005200000000     D  USER                 254    263
005300000000     D*---------------------------------------------------------------*
005400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005500000000     D DEVDS1          DS
005600000000     D* CHIAVE FUNZIONALE BATTUTA
005700000000     D  KEY                  369    369
005800000000     D* POSIZIONE CURSORE (IN HEX)
005900000000     D  CURSOR               370    371
006000000000     D*---------------------------------------------------------------*
006100000000     D* ARCHITETTURA
006200000000     D KPJBA         E DS
006300000000     D*---------------------------------------------------------------*
006400000000     D PARAM           DS                  INZ
006500000224     D  V1CCMP                 1      3  0
006600000224     D  V2CCMP                 4      6  0
006700000224     D  V1CDDV                 7     14  0
006800000224     D  V2CDDV                15     22  0
006900000224     D  V1CFCR                23     23
007000000000     C*---------------------------------------------------------------*
007100000000     C* MAIN - PROGRAMMA FILTRO STATISTICA RICAVI
007200000000     C*---------------------------------------------------------------*
007300000000     C*
007400000000     C* CICLO FINO A CHE:
007500000000DO  1C     $FINE         DOUEQ     '1'
007600000000     C*
007700000000     C                   MOVEL     '0'           $ERR
007800000000     C*
007900000000     C* CONTROLLA GESTIONE VIDEO
008000000000IF  2C     $GES          IFEQ      'V1'
008100000000     C                   EXSR      GESV1
008200000000E   2C                   ENDIF
008300000000     C*
008400000000E   1C                   ENDDO
008500000000     C*
008600000000     C* FINE MAIN DEL PGM
008700000000     C                   SETON                                        LR
008800000000     C*---------------------------------------------------------------*
008900000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
009000000000     C*---------------------------------------------------------------*
009100000000     C     GESV1         BEGSR
009200000000     C*
009300000000IF  1C     $TESVD        IFEQ      '1'
009400000000     C                   EXSR      CARV1
009500000000E   1C                   ENDIF
009600000000     C*
009700000000IF  1C     MSGULT        IFEQ      *ZEROS
009800000000     C                   MOVEL     *BLANKS       $MSG
009900000000     C                   EXSR      CARMSG
010000000000E   1C                   ENDIF
010100000000     C                   EXSR      OUTMSG
010200000000     C*
010300000000     C                   EXFMT     EC80DV1
010400000000     C*
010500000000     C                   EXSR      CONFUN
010600000000     C*
010700000000IF  1C     $ERR          IFEQ      '0'
010800000000SL  2C                   SELECT
010900000000     C     KEY           WHENEQ    ENTER
011000000000     C                   EXSR      ENTV1
011100000000     C     KEY           WHENEQ    F6
011200000000     C                   EXSR      F06GES
011300000000     C                   OTHER
011400000000     C                   EXSR      FUNSTD
011500000000E   2C                   ENDSL
011600000000E   1C                   ENDIF
011700000000     C*
011800000000     C                   ENDSR
011900000000     C*---------------------------------------------------------------*
012000000000     C* CARV1 - CARICA VIDEO 'PARAMETRI'
012100000000     C*---------------------------------------------------------------*
012200000000     C     CARV1         BEGSR
012300000000     C*
012400000000     C                   MOVEL     'V1'          $GES
012500000000     C*
012600000000     C                   EXSR      CARFUN
012700000000     C                   EXSR      OUTFUN
012800000000     C*
012900000000     C                   WRITE     EC80DVT
013000000000     C                   WRITE     EC80DVF
013100000000     C*
013200000000     C                   ENDSR
013300000000     C*---------------------------------------------------------------*
013400000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
013500000000     C*---------------------------------------------------------------*
013600000000     C     ENTV1         BEGSR
013700000000     C*
013800000000     C* CONTROLLA I CAMPI IMMESSI
013900000000     C                   EXSR      DECODI
014000000000     C*
014100000000     C                   ENDSR
014200000000     C*---------------------------------------------------------------*
014300000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
014400000000     C*---------------------------------------------------------------*
014500000000     C     F06GES        BEGSR
014600000000     C*
014700000000     C* CONTROLLA I CAMPI IMMESSI
014800000000     C                   EXSR      DECODI
014900000224     C*
015000000224     C* MI SALVO IL VALORE PRESENTE IN DATA PRIMA DEL "F6"
015100000224     C                   Z-ADD     V1CDDV        SAVPAR            8 0
015200000224     C*
015300000224     C* NON CI SONO ERRORI
015400000224IF  1C     $ERR          IFEQ      '0'
015500000224     C*
015600000224     C* SE LA DATA E' VUOTA GLI IMPOSTO VALORI "OMNI-COMPRENSIVI" DI DATE
015700000224IF  1C     V1CDDV        IFEQ      *ZERO
015800000224     C                   Z-ADD     *HIVAL        V1CDDV
015900001102     C                   Z-ADD     *ZEROS        V2CDDV
016000000224     C                   ENDIF
016100000000     C*
016200000000     C* LANCIO PROGRAMMA STAMPA
016300000000     C                   MOVEL     'EC81'        KCOAZ
016400000000     C                   MOVEL     PARAM         KPJBU
016500001102     C                   CALL      'BCH10'
016600001102 MOM C*                  CALL      'TREC81R'
016700000000     C                   PARM                    KPJBA
016800000000     C*
016900000000     C                   MOVEL     '1'           $FINE
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*---------------------------------------------------------------*
017400000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017500000000     C*---------------------------------------------------------------*
017600000000     C     DECODI        BEGSR
017700000000     C*
017800000224     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
017900000224     C                   SETOFF                                       606162
018000000000     C                   SETOFF                                       66
018100000000     C*
018200000000     C*--------------------
018300000000     C* CODICE COMPETENZA
018400000000     C*--------------------
018500000000     C*
018600000000     C* SE IL CODICE FINALE E' VUOTO GLI METTO QUELLO INIZIALE
018700000000IF  1C     V2CCMP        IFEQ      *ZERO
018800000000     C                   Z-ADD     V1CCMP        V2CCMP
018900000000E   1C                   ENDIF
019000000000     C*
019100000000     C* SE IL CODICE INIZIALE E' VUOTO GLI METTO QUELLO INIZIALE
019200000000IF  1C     V1CCMP        IFEQ      *ZERO
019300000000     C                   Z-ADD     V2CCMP        V1CCMP
019400000000E   1C                   ENDIF
019500000000     C*
019600000000IF  1C     V1CCMP        IFGT      V2CCMP
019700000000     C                   MOVEL     '1'           $ERR
019800000000     C                   MOVEL     MSG(7)        $MSG
019900000000     C                   EXSR      CARMSG
020000000000     C                   SETON                                        6061
020100000000E   1C                   ENDIF
020200000000     C*--------------------
020300000224     C* DATA
020400000000     C*--------------------
020500000000IF  1C     $ERR          IFEQ      '0'
020600000224IF  2C     V1CDDV        IFNE      *ZERO
020700000224     C                   Z-ADD     V1CDDV        $DAT              8 0
020800000000     C                   EXSR      CHKDAT
020900000000IF  3C     $ERR          IFEQ      '1'
021000000224     C                   SETON                                        62
021100000224     C                   MOVEL     MSG(12)       $MSG
021200000000     C                   EXSR      CARMSG
021300000000     C                   ELSE
021400000224     C                   Z-ADD     V1CDDV        V2CDDV
021500000000E   3C                   ENDIF
021600000000E   2C                   ENDIF
021700000000E   1C                   ENDIF
021800000000     C*--------------------
021900000000     C* TIPO COMPETENZA
022000000000     C*--------------------
022100000000IF  1C     $ERR          IFEQ      '0'
022200000000IF  2C     V1CFCR        IFNE      'C'
022300000000     C     V1CFCR        ANDNE     'R'
022400000000     C     V1CFCR        ANDNE     *BLANKS
022500000000     C                   MOVEL     '1'           $ERR
022600000000     C                   MOVEL     MSG(10)       $MSG
022700000000     C                   EXSR      CARMSG
022800000000     C                   SETON                                        66
022900000000E   2C                   ENDIF
023000000000E   1C                   ENDIF
023100000000     C*
023200000000     C                   ENDSR
023300000224     C*------------------------------------------------------------------------*
023400000224     C* CHKDAT - CONTROLLA LA DATA
023500000224     C*          INPUT --> $DAT:  DATA
023600000224     C*------------------------------------------------------------------------*
023700000224     C     CHKDAT        BEGSR
023800000224     C*
023900000224     C* OBBLIGATORIA
024000000224IF  1C     $DAT          IFEQ      *ZEROS
024100000224     C                   MOVEL     '1'           $ERR
024200000224     C                   MOVEL     MSG(8)        $MSG
024300000224X   1C                   ELSE
024400000224     C* CORRETTA
024500000224     C                   Z-ADD     $DAT          G08DAT
024600000224     C                   Z-ADD     *ZEROS        G08INV
024700000224     C                   MOVEL     '0'           G08ERR
024800000224     C                   CALL      'XSRDA8'
024900000224     C                   PARM                    WLBDA8
025000000224     C*
025100000224IF  2C     G08ERR        IFEQ      '1'
025200000224     C                   MOVEL     '1'           $ERR
025300000224     C                   MOVEL     MSG(8)        $MSG
025400000224     C                   ELSE
025500000224     C                   Z-ADD     G08DAT        $DAT
025600000224E   2C                   ENDIF
025700000224E   1C                   ENDIF
025800000224     C*
025900000224     C                   ENDSR
026000000000     C*---------------------------------------------------------------*
026100000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
026200000000     C*---------------------------------------------------------------*
026300000000     C     IMPVID        BEGSR
026400000000     C*--------------------
026500000000     C* RAGIONE SOCIALE
026600000000     C*--------------------
026700000000     C                   MOVEL     DSFIRS        VTDRSU
026800000224     C*--------------------
026900000224     C* CAMPI A VIDEO
027000000224     C*--------------------
027100000224     C                   Z-ADD     DATCOR        V1CDDV
027200000224     C                   Z-ADD     DATCOR        V2CDDV
027300000000     C*
027400000000     C                   ENDSR
027500020923     C*-------------------------------------------------------------------------
027600020923     C* REPERISCE I DATI UTENTE
027700020923     C*-------------------------------------------------------------------------
027800020923     C     REPDATIUTE    BEGSR
027900020923     C*
028000020923     C* INIZIALIZZA VARIABILI DI WRK
028100020923     C                   CLEAR                   TIBS34DS
028200020923     C                   CLEAR                   AZUTEDS
028300020923     C                   CLEAR                   DDATIUTE
028400020923     C*
028500020923     C     *DTAARA       DEFINE    §azute        azuteds
028600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
028700020923     C                   IN(E)     *DTAARA
028800020923if  1C                   IF        %Error
028900020923     c                   EVAL      I34Tla = 'L'
029000020923     C                   CALL      'TIBS34R'
029100020923     C                   PARM                    Tibs34Ds
029200020923     C                   IN        *DTAARA
029300020923e   1C                   ENDIF
029400020923     C*
029500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
029700020923     C*
029800020923     C                   ENDSR
029900000000     C*---------------------------------------------------------------*
030000000000     C* *INZSR  - ROUTINE INIZIALE
030100000000     C*---------------------------------------------------------------*
030200000000     C     *INZSR        BEGSR
030300000000     C*
030400000000     C* RICEVIMENTO PARAMETRI
030500000000     C     *ENTRY        PLIST
030600000000     C                   PARM                    KPJBA
030700000224     C*
030800000224     C* IMPOSTA LA DATA CORRENTE
030900000224     C                   TIME                    N14              14 0
031000000224     C                   MOVE      N14           N8                8 0
031100000224     C                   Z-ADD     N8            G08DAT
031200000224     C                   Z-ADD     *ZEROS        G08INV
031300000224     C                   MOVEL     '0'           G08ERR
031400000224     C                   CALL      'XSRDA8'
031500000224     C                   PARM                    WLBDA8
031600000224     C*                  Z-ADD     G08INV        DATCOR            8 0
031700000224     C                   Z-ADD     G08DAT        DATCOR            8 0
031800000000     C*
031900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
032000000000     C                   EXSR      INZSTD
032100000000     C*
032200000000     C* IMPOSTAZIONI INIZIALI VIDEO
032300000000     C                   EXSR      IMPVID
032400000000     C*
032500000000     C* EMETTO FORMATO VIDEO
032600000000     C                   EXSR      CARV1
032700000000     C*
032800000000     C                   ENDSR
032900000000     C*------------------------------------------------------------------------*
033000000000     C*
033100000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033200000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033300000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
033400000000     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
033500000000     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
033600000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033700000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033800000000     C*
033900000000     C*---------------------------------------------------------------*
034000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
034100000000     C*---------------------------------------------------------------*
034200000000     C     INZSTD        BEGSR
034300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
034400000000     C                   CALL      'XKEYS'
034500000000     C                   PARM                    DSKEYS
034600000000     C                   MOVEA     KEYRIG        KEYS
034700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
034800000000     C     XMSGPA        PLIST
034900000000     C                   PARM                    DSMSMS
035000000000     C                   PARM                    DSMSPG
035100000000     C                   PARM                    DSMSKE
035200000000     C                   PARM                    DSMSER
035300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
035400000000     C                   MOVEL     'TREC80R'     DSMSPG
035500000000     C                   MOVEL     'TREC80R'     VMCQUE
035600000000     C                   MOVEL     *BLANKS       VMCKEY
035700000000     C                   MOVEL     *BLANKS       DSMSMS
035800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
035900000000     C                   MOVEL     '0'           $ERR              1
036000000000     C                   MOVEL     '0'           $F03              1
036100000000     C                   MOVEL     *BLANKS       $GES              2
036200000000     C                   MOVEL     *BLANKS       $MSG             80
036300000000     C                   MOVEL     '0'           $FINE             1
036400000000     C                   MOVEL     '1'           $TESVD            1
036500000000     C                   MOVEL     'I'           $TGES             1
036600000000     C* LANCIO ROUTINE PER VERIFICARE SE
036700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
036800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
036900020923     C                   EXSR      REPDATIUTE
037000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
037100000000     C                   BITOFF    '01234567'    POSMI1            1
037200000000     C                   BITOFF    '01234567'    POSMI2            1
037300000000     C                   BITOFF    '01234567'    POSMA1            1
037400000000     C                   BITOFF    '01234567'    POSMA2            1
037500000000     C                   BITON     '34'          POSMI1            1
037600000000     C                   BITON     '7'           POSMI2            1
037700000000     C                   BITON     '34'          POSMA1            1
037800000000     C                   BITON     '0'           POSMA2            1
037900000000     C                   MOVE      POSMI2        POSMIN            2
038000000000     C                   MOVEL     POSMI1        POSMIN
038100000000     C                   MOVE      POSMA2        POSMAX            2
038200000000     C                   MOVEL     POSMA1        POSMAX
038300000000     C                   ENDSR
038400000000     C*---------------------------------------------------------------*
038500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
038600000000     C*---------------------------------------------------------------*
038700000000     C     FUNSTD        BEGSR
038800000000     C                   SELECT
038900000000     C     KEY           WHENEQ    F3
039000000000     C                   EXSR      F03STD
039100000000     C     KEY           WHENEQ    F12
039200000000     C                   EXSR      F12STD
039300000000     C     KEY           WHENEQ    F24
039400000000     C                   EXSR      F24STD
039500000000     C                   OTHER
039600000000     C                   MOVEL     MSG(2)        $MSG
039700000000     C                   EXSR      CARMSG
039800000000     C                   ENDSL
039900000000     C                   ENDSR
040000000000     C*---------------------------------------------------------------*
040100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
040200000000     C*---------------------------------------------------------------*
040300000000     C     F03STD        BEGSR
040400000000     C*
040500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
040600000000     C                   MOVEL     '1'           $FINE
040700000000     C*
040800000000     C                   ENDSR
040900000000     C*---------------------------------------------------------------*
041000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
041100000000     C*---------------------------------------------------------------*
041200000000     C     F12STD        BEGSR
041300000000     C*
041400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
041500000000     C                   MOVEL     '1'           $FINE
041600000000     C*
041700000000     C                   ENDSR
041800000000     C*---------------------------------------------------------------*
041900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
042000000000     C*---------------------------------------------------------------*
042100000000     C     F24STD        BEGSR
042200000000     C*
042300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
042400000000     C                   EXSR      OUTFUN
042500000000     C*
042600000000     C                   ENDSR
042700000000     C*---------------------------------------------------------------*
042800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
042900000000     C*          CODA MESSAGGI DEL PGM
043000000000     C*---------------------------------------------------------------*
043100000000     C     CARMSG        BEGSR
043200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
043300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
043400000000     C     MSGULT        IFEQ      0
043500000000     C                   MOVEL     '1'           *IN40
043600000000     C                   WRITE     EC80DVK
043700000000     C                   MOVEL     '0'           *IN40
043800000000     C                   ENDIF
043900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
044000000000     C                   MOVEL     $MSG          DSMSMS
044100000000     C                   CALL      'XMSG'        XMSGPA
044200000000     C     DSMSER        IFEQ      0
044300000000     C                   MOVEL     DSMSKE        VMCKEY
044400000000     C                   ENDIF
044500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
044600000000     C                   ADD       1             MSGULT            4 0
044700000000     C                   WRITE     EC80DVM
044800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
044900000000     C                   MOVEL     *BLANKS       $MSG
045000000000     C                   ENDSR
045100000000     C*---------------------------------------------------------------*
045200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
045300000000     C*---------------------------------------------------------------*
045400000000     C     OUTMSG        BEGSR
045500000000     C* VISUALIZZAZIONE MESSAGGIO
045600000000     C                   MOVEL     '1'           *IN44
045700000000     C                   WRITE     EC80DVK
045800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
045900000000     C                   Z-ADD     *ZEROS        MSGULT
046000000000     C                   ENDSR
046100000000     C*---------------------------------------------------------------*
046200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
046300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
046400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
046500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
046600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
046700000000     C*---------------------------------------------------------------*
046800000000     C     CARFUN        BEGSR
046900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
047000000000     C     1             DO        31            I                 5 0
047100000000     C                   MOVEL     FUN(I)        FUNRIG
047200000000     C                   MOVEL     FUNFUN        FFU(I)
047300000000     C                   MOVEL     FUNDES        FDE(I)
047400020923     C     DUTLPO        IFEQ      'S'
047500000000     C     $TGES         IFEQ      'G'
047600000000     C                   MOVEL     FUNASG        FAT(I)
047700000000     C                   ELSE
047800000000     C                   MOVEL     FUNASI        FAT(I)
047900000000     C                   ENDIF
048000000000     C                   ELSE
048100000000     C     $TGES         IFEQ      'G'
048200000000     C                   MOVEL     FUNAFG        FAT(I)
048300000000     C                   ELSE
048400000000     C                   MOVEL     FUNAFI        FAT(I)
048500000000     C                   ENDIF
048600000000     C                   ENDIF
048700000000     C     FAT(I)        IFEQ      'E'
048800000000     C                   MOVEL     USER          WEDP              3
048900000000     C     WEDP          IFEQ      'EDP'
049000000000     C                   MOVEL     'S'           FAT(I)
049100000000     C                   ELSE
049200000000     C                   MOVEL     'N'           FAT(I)
049300000000     C                   ENDIF
049400000000     C                   ENDIF
049500000000     C                   ENDDO
049600000000     C                   ENDSR
049700000000     C*---------------------------------------------------------------*
049800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
049900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
050000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
050100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
050200000000     C*---------------------------------------------------------------*
050300000000     C     OUTFUN        BEGSR
050400000000     C     1             DO        8             I
050500000000     C                   MOVEL     *BLANKS       FUV(I)
050600000000     C                   ENDDO
050700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
050800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
050900000000     C     1             DO        31            I
051000000000     C     FAT(I)        IFEQ      'S'
051100000000     C     FDE(I)        ANDNE     *BLANKS
051200000000     C                   ADD       1             FUNNAT
051300000000     C                   ENDIF
051400000000     C                   ENDDO
051500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
051600000000     C     FUNNAT        IFGT      8
051700000000     C                   MOVEL     'S'           FAT(24)
051800000000     C                   ENDIF
051900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
052000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
052100000000     C                   Z-ADD     8             FUNVIS            5 0
052200000000     C     FAT(23)       IFEQ      'S'
052300000000     C                   SUB       1             FUNVIS
052400000000     C                   ENDIF
052500000000     C     FAT(24)       IFEQ      'S'
052600000000     C                   SUB       1             FUNVIS
052700000000     C                   ENDIF
052800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
052900000000     C                   Z-ADD     *ZEROS        W                 5 0
053000000000     C                   ADD       1             FUNULT
053100000000     C     FUNULT        IFGT      31
053200000000     C                   Z-ADD     1             FUNULT
053300000000     C                   ENDIF
053400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
053500000000     C     FUNULT        DO        31            I
053600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
053700000000     C     W             IFLT      FUNVIS
053800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
053900000000     C     FAT(I)        IFEQ      'S'
054000000000     C     FDE(I)        ANDNE     *BLANKS
054100000000     C     I             IFEQ      23
054200000000     C     I             OREQ      24
054300000000     C                   ELSE
054400000000     C                   ADD       1             W
054500000000     C                   MOVEL     FDE(I)        FUV(W)
054600000000     C                   ENDIF
054700000000     C                   ENDIF
054800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
054900000000     C                   Z-ADD     I             FUNULT
055000000000     C                   ENDIF
055100000000     C                   ENDDO
055200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
055300000000     C                   MOVEL     FUV(1)        VFD001
055400000000     C                   MOVEL     FUV(2)        VFD002
055500000000     C                   MOVEL     FUV(3)        VFD003
055600000000     C                   MOVEL     FUV(4)        VFD004
055700000000     C                   MOVEL     FUV(5)        VFD005
055800000000     C                   MOVEL     FUV(6)        VFD006
055900000000     C     FAT(23)       IFEQ      'N'
056000000000     C     FAT(24)       ANDEQ     'N'
056100000000     C                   MOVEL     FUV(7)        VFD007
056200000000     C                   MOVEL     FUV(8)        VFD008
056300000000     C                   ELSE
056400000000     C     FAT(23)       IFEQ      'S'
056500000000     C     FAT(24)       ANDEQ     'S'
056600000000     C                   MOVEL     FDE(23)       VFD007
056700000000     C                   MOVEL     FDE(24)       VFD008
056800000000     C                   ELSE
056900000000     C                   MOVEL     FUV(7)        VFD007
057000000000     C     FAT(23)       IFEQ      'S'
057100000000     C                   MOVEL     FDE(23)       VFD008
057200000000     C                   ELSE
057300000000     C                   MOVEL     FDE(24)       VFD008
057400000000     C                   ENDIF
057500000000     C                   ENDIF
057600000000     C                   ENDIF
057700000000     C                   MOVEL     '0'           $TESVD
057800000000     C                   ENDSR
057900000000     C*---------------------------------------------------------------*
058000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
058100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
058200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
058300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
058400000000     C*---------------------------------------------------------------*
058500000000     C     CONFUN        BEGSR
058600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
058700000000     C     1             DO        31            I
058800000000     C     KEY           IFEQ      KEYS(I)
058900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
059000000000     C     FAT(I)        IFNE      'S'
059100000000     C                   MOVEL     MSG(1)        $MSG
059200000000     C                   EXSR      CARMSG
059300000000     C                   MOVEL     '1'           $ERR
059400000000     C                   ENDIF
059500000000     C                   ENDIF
059600000000     C                   ENDDO
059700000000     C                   ENDSR
059800000000**   SCHIERA: MSG  (MESSAGGI)
059900000000TREC80R- Tasto funzionale non valido.                                          1
060000000000TREC80R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
060100000000TREC80R- Opzione non valida.                                                   3
060200000000TREC80R- Opzione abilitata ma non gestita. Avvertire CED.                      4
060300000000TREC80R- Inizio elenco.                                                        5
060400000000TREC80R- Fine elenco.                                                          6
060500000000TREC80R- Immettere competenza finale maggiore o uguale a quella iniziale       7
060600000000TREC80R- Immettere data formalmente valida                                     8
060700000000TREC80R- Immettere data finale maggiore o uguale a quella iniziale             9
060800000000TREC80R- Immettere tipo competenza valido                                     10
060900000000TREC80R- 11
061000000224TREC80R- Data non valida.                                                     12
061100000000TREC80R- 13
061200000000TREC80R- 14
061300000000TREC80R- 15
061400000000TREC80R- 16
061500000000TREC80R- 17
061600000000TREC80R- 18
061700000000TREC80R- 19
061800000000TREC80R- 20
061900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
062000000000F1    -                   -    NN     NN
062100000000F2    -                   -    NN     NN
062200000000F3    -F3=Fine            -    SS     SS
062300000000F4    -                   -    NN     NN
062400000000F5    -                   -    NN     NN
062500000000F6    -F6=Lancio          -    SS     SS
062600000000F7    -                   -    NN     NN
062700000000F8    -                   -    NN     NN
062800000000F9    -                   -    NN     NN
062900000000F10   -                   -    NN     NN
063000000000F11   -                   -    NN     NN
063100000000F12   -F12=Ritorno        -    SS     SS
063200000000F13   -                   -    NN     NN
063300000000F14   -                   -    NN     NN
063400000000F15   -                   -    NN     NN
063500000000F16   -                   -    NN     NN
063600000000F17   -                   -    NN     NN
063700000000F18   -                   -    NN     NN
063800000000F19   -                   -    NN     NN
063900000000F20   -                   -    NN     NN
064000000000F21   -                   -    NN     NN
064100000000F22   -                   -    NN     NN
064200000000F23   -                   -    NN     NN
064300000000F24   -F24=Altri tasti    -    NN     NN
064400000000ENTER -                   -    SS     SS
064500000000ROLLUP-                   -    NN     NN
064600000000ROLLDN-                   -    NN     NN
064700000000HELP  -                   -    NN     NN
064800000000PRINT -                   -    NN     NN
064900000000BSPACE-                   -    NN     NN
065000000000CLEAR -                   -    NN     NN
