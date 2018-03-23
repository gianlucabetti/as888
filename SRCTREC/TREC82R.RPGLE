000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC82R
000300000208      *  Descrizione   :  Stampa voci e tariffe di conto economico
000400000000      *                   Programma filtro per la stampa
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  15 GIU 1994
000700000000      *------------------------------------------------------------------------*
000800000000     FTREC82D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(EC82DVM:MSGULT)
001000000000     F*---------------------------------------------------------------*
001100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001400000000     D FFU             S              6    DIM(31)
001500000000     D FDE             S             19    DIM(31)
001600000000     D FAT             S              1    DIM(31)
001700000000     D* TABELLA DESCRIZIONI A VIDEO
001800000000     D FUV             S             19    DIM(8)
001900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002000000000     D KEYS            S              1    DIM(40)
002100000000     D*---------------------------------------------------------------*
002200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002300000000     D DSFUN         E DS
002400000000     D  FUNRIG                 1     40
002500000000     D*---------------------------------------------------------------*
002600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002700000000     D DSMSG         E DS
002800000000     D*---------------------------------------------------------------*
002900020923     D* DS REPERIMENTO DATI UTENTE
003000020923     D TIBS34DS      E DS
003100020923     D DDATIUTE      E DS
003200020923     D AZUTEDS       E DS                  extname(AZUTE00F)
003300000000     D*---------------------------------------------------------------*
003400000224     D WLBDA8          DS                  INZ
003500000000     D  G08DAT                 1      8  0
003600000000     D  G08INV                 9     16  0
003700000000     D  G08ERR                17     17
003800000000     D  G08TGI                18     22  0
003900000000     D*---------------------------------------------------------------*
004000000000     D* DS PER GESTIONE TASTI FUNZIONALI
004100000000     D DSKEYS        E DS
004200000000     D  KEYRIG                 1     40
004300000000     D*---------------------------------------------------------------*
004400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004500000000     D                SDS
004600000000     D* NOME UTENTE
004700000000     D  USER                 254    263
004800000000     D*---------------------------------------------------------------*
004900000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005000000000     D DEVDS1          DS
005100000000     D* CHIAVE FUNZIONALE BATTUTA
005200000000     D  KEY                  369    369
005300000000     D* POSIZIONE CURSORE (IN HEX)
005400000000     D  CURSOR               370    371
005500000000     D*---------------------------------------------------------------*
005600000000     D* ARCHITETTURA
005700000000     D KPJBA         E DS
005800000000     D*---------------------------------------------------------------*
005900000000     D PARAM           DS                  INZ
006000000000     D  V1CVOC                 1      3  0
006100000000     D  V2CVOC                 4      6  0
006200000000     D  V1CDDV                 7     14  0
006300000214     D  V1CCMP                15     17  0
006400000214     D  V2CCMP                18     20  0
006500000214     D  V1CSOD                21     21
006600000214     D  V1CCOV                22     22
006700000214     D  V2CDDV                23     30  0
006800000214     D  V1CSON                31     31
006900000000     C*---------------------------------------------------------------*
007000000000     C* MAIN - PROGRAMMA FILTRO STATISTICA RICAVI
007100000000     C*---------------------------------------------------------------*
007200000000     C*
007300000000     C* CICLO FINO A CHE:
007400000000DO  1C     $FINE         DOUEQ     '1'
007500000000     C*
007600000000     C                   MOVEL     '0'           $ERR
007700000000     C*
007800000000     C* CONTROLLA GESTIONE VIDEO
007900000000IF  2C     $GES          IFEQ      'V1'
008000000000     C                   EXSR      GESV1
008100000000E   2C                   ENDIF
008200000000     C*
008300000000E   1C                   ENDDO
008400000000     C*
008500000000     C* FINE MAIN DEL PGM
008600000000     C                   SETON                                        LR
008700000000     C*---------------------------------------------------------------*
008800000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
008900000000     C*---------------------------------------------------------------*
009000000000     C     GESV1         BEGSR
009100000000     C*
009200000000IF  1C     $TESVD        IFEQ      '1'
009300000000     C                   EXSR      CARV1
009400000000E   1C                   ENDIF
009500000000     C*
009600000000IF  1C     MSGULT        IFEQ      *ZEROS
009700000000     C                   MOVEL     *BLANKS       $MSG
009800000000     C                   EXSR      CARMSG
009900000000E   1C                   ENDIF
010000000000     C                   EXSR      OUTMSG
010100000000     C*
010200000000     C                   EXFMT     EC82DV1
010300000000     C*
010400000000     C                   EXSR      CONFUN
010500000000     C*
010600000000IF  1C     $ERR          IFEQ      '0'
010700000000SL  2C                   SELECT
010800000000     C     KEY           WHENEQ    ENTER
010900000000     C                   EXSR      ENTV1
011000000000     C     KEY           WHENEQ    F6
011100000000     C                   EXSR      F06GES
011200000000     C                   OTHER
011300000000     C                   EXSR      FUNSTD
011400000000E   2C                   ENDSL
011500000000E   1C                   ENDIF
011600000000     C*
011700000000     C                   ENDSR
011800000000     C*---------------------------------------------------------------*
011900000000     C* CARV1 - CARICA VIDEO 'PARAMETRI'
012000000000     C*---------------------------------------------------------------*
012100000000     C     CARV1         BEGSR
012200000000     C*
012300000000     C                   MOVEL     'V1'          $GES
012400000000     C*
012500000000     C                   EXSR      CARFUN
012600000000     C                   EXSR      OUTFUN
012700000000     C*
012800000000     C                   WRITE     EC82DVT
012900000000     C                   WRITE     EC82DVF
013000000000     C*
013100000000     C                   ENDSR
013200000000     C*---------------------------------------------------------------*
013300000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
013400000000     C*---------------------------------------------------------------*
013500000000     C     ENTV1         BEGSR
013600000000     C*
013700000000     C* CONTROLLA I CAMPI IMMESSI
013800000000     C                   EXSR      DECODI
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*---------------------------------------------------------------*
014200000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
014300000000     C*---------------------------------------------------------------*
014400000000     C     F06GES        BEGSR
014500000000     C*
014600000000     C* CONTROLLA I CAMPI IMMESSI
014700000215     C                   EXSR      DECODI
014800000215     C*
014900000215     C* MI SALVO IL VALORE PRESENTE IN DATA PRIMA DEL "F6"
015000000215     C                   Z-ADD     V1CDDV        SAVPAR            8 0
015100000000     C*
015200000000     C* NON CI SONO ERRORI
015300000000IF  1C     $ERR          IFEQ      '0'
015400000215     C*
015500000215     C* SE LA DATA E' VUOTA GLI IMPOSTO VALORI "OMNI-COMPRENSIVI" DI DATE
015600000215IF  1C     V1CDDV        IFEQ      *ZERO
015700000215     C                   Z-ADD     *HIVAL        V1CDDV
015800000215     C                   Z-ADD     *LOVAL        V2CDDV
015900000215     C                   ENDIF
016000000000     C*
016100000000     C* LANCIO PROGRAMMA STAMPA
016200000000     C                   MOVEL     'EC83'        KCOAZ
016300000000     C                   MOVEL     PARAM         KPJBU
016400040128     C                   CALL      'BCH10'
016500040128 MOM C*                  CALL      'TREC83R'
016600000000     C                   PARM                    KPJBA
016700000000     C*
016800000211     C*                  MOVEL     '1'           $FINE
016900000215     C                   Z-ADD     SAVPAR        V1CDDV
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*---------------------------------------------------------------*
017400000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017500000000     C*---------------------------------------------------------------*
017600000000     C     DECODI        BEGSR
017700000000     C*
017800000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
017900000000     C                   SETOFF                                       606162
018000000000     C                   SETOFF                                       666768
018100000208     C*
018200000208     C*--------------------
018300000208     C* DATA
018400000208     C*--------------------
018500000209     C* SE DATA A ZERO FORZO VALORE 'N' INFLAG DI INCLUSIONE TARIFFE
018600000209     C* (LO IMPOSTO QUI IN MODO TALE CHE L'UTENTE SE NE ACCORGA A VIDEO)
018700000208IF  1C     V1CDDV        IFEQ      *ZERO
018800000209     C                   MOVEL     'N'           V1CSON
018900000208     C                   ELSE
019000000208     C                   Z-ADD     V1CDDV        $DAT              8 0
019100000208     C                   EXSR      CHKDAT
019200000208     C     $ERR          IFEQ      '1'
019300000209     C                   SETON                                        62
019400000209     C                   MOVEL     MSG(12)       $MSG
019500000209     C                   EXSR      CARMSG
019600000209     C                   ELSE
019700000209     C                   Z-ADD     V1CDDV        V2CDDV
019800000208     C                   ENDIF
019900000208E   1C                   ENDIF
020000000208     C*
020100000208     C*--------------------
020200000208     C* CODICE VOCE
020300000208     C*--------------------
020400000000     C*
020500000000     C* SE IL CODICE FINALE E' VUOTO GLI METTO QUELLO INIZIALE
020600000000IF  1C     V2CVOC        IFEQ      *ZERO
020700000000     C                   Z-ADD     V1CVOC        V2CVOC
020800000000E   1C                   ENDIF
020900000000     C*
021000000000     C* SE IL CODICE INIZIALE E' VUOTO GLI METTO QUELLO INIZIALE
021100000000IF  1C     V1CVOC        IFEQ      *ZERO
021200000000     C                   Z-ADD     V2CVOC        V1CVOC
021300000000E   1C                   ENDIF
021400000000     C*
021500000000     C     V1CVOC        IFGT      V2CVOC
021600000000     C                   MOVEL     '1'           $ERR
021700000000     C                   MOVEL     MSG(11)       $MSG
021800000000     C                   EXSR      CARMSG
021900000000     C                   SETON                                        6061
022000000000     C                   ENDIF
022100000000     C*--------------------
022200000000     C* CODICE COMPETENZA
022300000000     C*--------------------
022400000000     C*
022500000000     C* SE IL CODICE FINALE E' VUOTO GLI METTO QUELLO INIZIALE
022600000000IF  1C     V2CCMP        IFEQ      *ZERO
022700000000     C                   Z-ADD     V1CCMP        V2CCMP
022800000000E   1C                   ENDIF
022900000000     C*
023000000209     C* SE IL CODICE INIZIALE E' VUOTO GLI METTO QUELLO FINALE
023100000000IF  1C     V1CCMP        IFEQ      *ZERO
023200000000     C                   Z-ADD     V2CCMP        V1CCMP
023300000000E   1C                   ENDIF
023400000000     C*
023500000000     C     V1CCMP        IFGT      V2CCMP
023600000000     C                   MOVEL     '1'           $ERR
023700000000     C                   MOVEL     MSG(7)        $MSG
023800000000     C                   EXSR      CARMSG
023900000000     C                   SETON                                        6768
024000000000     C                   ENDIF
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C* CHKDAT - CONTROLLA LA DATA
024500000000     C*          INPUT --> $DAT:  DATA
024600000000     C*------------------------------------------------------------------------*
024700000000     C     CHKDAT        BEGSR
024800000000     C*
024900000000     C* OBBLIGATORIA
025000000000IF  1C     $DAT          IFEQ      *ZEROS
025100000000     C                   MOVEL     '1'           $ERR
025200000000     C                   MOVEL     MSG(8)        $MSG
025300000000X   1C                   ELSE
025400000000     C* CORRETTA
025500000000     C                   Z-ADD     $DAT          G08DAT
025600000000     C                   Z-ADD     *ZEROS        G08INV
025700000000     C                   MOVEL     '0'           G08ERR
025800000000     C                   CALL      'XSRDA8'
025900000224     C                   PARM                    WLBDA8
026000000000     C*
026100000000IF  2C     G08ERR        IFEQ      '1'
026200000000     C                   MOVEL     '1'           $ERR
026300000000     C                   MOVEL     MSG(8)        $MSG
026400000000     C                   ELSE
026500000000     C                   Z-ADD     G08DAT        $DAT
026600000000E   2C                   ENDIF
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C                   ENDSR
027000000000     C*---------------------------------------------------------------*
027100000208     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
027200000000     C*---------------------------------------------------------------*
027300000000     C     IMPVID        BEGSR
027400000210     C*--------------------
027500000210     C* RAGIONE SOCIALE UTENTE "LANCIANTE"
027600000210     C*--------------------
027700000210     C                   MOVEL     DSFIRS        VTDRSU
027800000210     C*--------------------
027900000210     C* CAMPI A VIDEO
028000000210     C*--------------------
028100000208     C                   Z-ADD     DATCOR        V1CDDV
028200000209     C                   Z-ADD     DATCOR        V2CDDV
028300000208     C                   MOVEL     'D'           V1CSOD
028400000208     C                   MOVEL     'V'           V1CCOV
028500000209     C                   MOVEL     'N'           V1CSON
028600000000     C*
028700000000     C                   ENDSR
028800020923     C*-------------------------------------------------------------------------
028900020923     C* REPERISCE I DATI UTENTE
029000020923     C*-------------------------------------------------------------------------
029100020923     C     REPDATIUTE    BEGSR
029200020923     C*
029300020923     C* INIZIALIZZA VARIABILI DI WRK
029400020923     C                   CLEAR                   TIBS34DS
029500020923     C                   CLEAR                   AZUTEDS
029600020923     C                   CLEAR                   DDATIUTE
029700020923     C*
029800020923     C     *DTAARA       DEFINE    §azute        azuteds
029900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
030000020923     C                   IN(E)     *DTAARA
030100020923if  1C                   IF        %Error
030200020923     c                   EVAL      I34Tla = 'L'
030300020923     C                   CALL      'TIBS34R'
030400020923     C                   PARM                    Tibs34Ds
030500020923     C                   IN        *DTAARA
030600020923e   1C                   ENDIF
030700020923     C*
030800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
030900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
031000020923     C*
031100020923     C                   ENDSR
031200000000     C*---------------------------------------------------------------*
031300000000     C* *INZSR  - ROUTINE INIZIALE
031400000000     C*---------------------------------------------------------------*
031500000000     C     *INZSR        BEGSR
031600000000     C*
031700000000     C* RICEVIMENTO PARAMETRI
031800000000     C     *ENTRY        PLIST
031900000000     C                   PARM                    KPJBA
032000000208     C*
032100000208     C* IMPOSTA LA DATA CORRENTE
032200000208     C                   TIME                    N14              14 0
032300000208     C                   MOVE      N14           N8                8 0
032400000208     C                   Z-ADD     N8            G08DAT
032500000208     C                   Z-ADD     *ZEROS        G08INV
032600000208     C                   MOVEL     '0'           G08ERR
032700000208     C                   CALL      'XSRDA8'
032800000224     C                   PARM                    WLBDA8
032900000208     C*                  Z-ADD     G08INV        DATCOR            8 0
033000000208     C                   Z-ADD     G08DAT        DATCOR            8 0
033100000000     C*
033200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
033300000000     C                   EXSR      INZSTD
033400000000     C*
033500000000     C* IMPOSTAZIONI INIZIALI VIDEO
033600000000     C                   EXSR      IMPVID
033700000000     C*
033800000000     C* EMETTO FORMATO VIDEO
033900000000     C                   EXSR      CARV1
034000000000     C*
034100000000     C                   ENDSR
034200000000     C*------------------------------------------------------------------------*
034300000000     C*
034400000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034500000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034600000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
034700000000     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
034800000000     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
034900000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035000000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035100000000     C*
035200000000     C*---------------------------------------------------------------*
035300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
035400000000     C*---------------------------------------------------------------*
035500000000     C     INZSTD        BEGSR
035600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035700000000     C                   CALL      'XKEYS'
035800000000     C                   PARM                    DSKEYS
035900000000     C                   MOVEA     KEYRIG        KEYS
036000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
036100000000     C     XMSGPA        PLIST
036200000000     C                   PARM                    DSMSMS
036300000000     C                   PARM                    DSMSPG
036400000000     C                   PARM                    DSMSKE
036500000000     C                   PARM                    DSMSER
036600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
036700000000     C                   MOVEL     'TREC82R'     DSMSPG
036800000000     C                   MOVEL     'TREC82R'     VMCQUE
036900000000     C                   MOVEL     *BLANKS       VMCKEY
037000000000     C                   MOVEL     *BLANKS       DSMSMS
037100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
037200000000     C                   MOVEL     '0'           $ERR              1
037300000000     C                   MOVEL     '0'           $F03              1
037400000000     C                   MOVEL     *BLANKS       $GES              2
037500000000     C                   MOVEL     *BLANKS       $MSG             80
037600000000     C                   MOVEL     '0'           $FINE             1
037700000000     C                   MOVEL     '1'           $TESVD            1
037800000000     C                   MOVEL     'I'           $TGES             1
037900000000     C* LANCIO ROUTINE PER VERIFICARE SE
038000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
038100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
038200020923     C                   EXSR      REPDATIUTE
038300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
038400000000     C                   BITOFF    '01234567'    POSMI1            1
038500000000     C                   BITOFF    '01234567'    POSMI2            1
038600000000     C                   BITOFF    '01234567'    POSMA1            1
038700000000     C                   BITOFF    '01234567'    POSMA2            1
038800000000     C                   BITON     '34'          POSMI1            1
038900000000     C                   BITON     '7'           POSMI2            1
039000000000     C                   BITON     '34'          POSMA1            1
039100000000     C                   BITON     '0'           POSMA2            1
039200000000     C                   MOVE      POSMI2        POSMIN            2
039300000000     C                   MOVEL     POSMI1        POSMIN
039400000000     C                   MOVE      POSMA2        POSMAX            2
039500000000     C                   MOVEL     POSMA1        POSMAX
039600000000     C                   ENDSR
039700000000     C*---------------------------------------------------------------*
039800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
039900000000     C*---------------------------------------------------------------*
040000000000     C     FUNSTD        BEGSR
040100000000     C                   SELECT
040200000000     C     KEY           WHENEQ    F3
040300000000     C                   EXSR      F03STD
040400000000     C     KEY           WHENEQ    F12
040500000000     C                   EXSR      F12STD
040600000000     C     KEY           WHENEQ    F24
040700000000     C                   EXSR      F24STD
040800000000     C                   OTHER
040900000000     C                   MOVEL     MSG(2)        $MSG
041000000000     C                   EXSR      CARMSG
041100000000     C                   ENDSL
041200000000     C                   ENDSR
041300000000     C*---------------------------------------------------------------*
041400000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
041500000000     C*---------------------------------------------------------------*
041600000000     C     F03STD        BEGSR
041700000000     C*
041800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
041900000000     C                   MOVEL     '1'           $FINE
042000000000     C*
042100000000     C                   ENDSR
042200000000     C*---------------------------------------------------------------*
042300000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
042400000000     C*---------------------------------------------------------------*
042500000000     C     F12STD        BEGSR
042600000000     C*
042700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
042800000000     C                   MOVEL     '1'           $FINE
042900000000     C*
043000000000     C                   ENDSR
043100000000     C*---------------------------------------------------------------*
043200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
043300000000     C*---------------------------------------------------------------*
043400000000     C     F24STD        BEGSR
043500000000     C*
043600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
043700000000     C                   EXSR      OUTFUN
043800000000     C*
043900000000     C                   ENDSR
044000000000     C*---------------------------------------------------------------*
044100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
044200000000     C*          CODA MESSAGGI DEL PGM
044300000000     C*---------------------------------------------------------------*
044400000000     C     CARMSG        BEGSR
044500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
044600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
044700000000     C     MSGULT        IFEQ      0
044800000000     C                   MOVEL     '1'           *IN40
044900000000     C                   WRITE     EC82DVK
045000000000     C                   MOVEL     '0'           *IN40
045100000000     C                   ENDIF
045200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
045300000000     C                   MOVEL     $MSG          DSMSMS
045400000000     C                   CALL      'XMSG'        XMSGPA
045500000000     C     DSMSER        IFEQ      0
045600000000     C                   MOVEL     DSMSKE        VMCKEY
045700000000     C                   ENDIF
045800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
045900000000     C                   ADD       1             MSGULT            4 0
046000000000     C                   WRITE     EC82DVM
046100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
046200000000     C                   MOVEL     *BLANKS       $MSG
046300000000     C                   ENDSR
046400000000     C*---------------------------------------------------------------*
046500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
046600000000     C*---------------------------------------------------------------*
046700000000     C     OUTMSG        BEGSR
046800000000     C* VISUALIZZAZIONE MESSAGGIO
046900000000     C                   MOVEL     '1'           *IN44
047000000000     C                   WRITE     EC82DVK
047100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
047200000000     C                   Z-ADD     *ZEROS        MSGULT
047300000000     C                   ENDSR
047400000000     C*---------------------------------------------------------------*
047500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
047600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
047700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
047800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
047900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
048000000000     C*---------------------------------------------------------------*
048100000000     C     CARFUN        BEGSR
048200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
048300000000     C     1             DO        31            I                 5 0
048400000000     C                   MOVEL     FUN(I)        FUNRIG
048500000000     C                   MOVEL     FUNFUN        FFU(I)
048600000000     C                   MOVEL     FUNDES        FDE(I)
048700020923     C     DUTLPO        IFEQ      'S'
048800000000     C     $TGES         IFEQ      'G'
048900000000     C                   MOVEL     FUNASG        FAT(I)
049000000000     C                   ELSE
049100000000     C                   MOVEL     FUNASI        FAT(I)
049200000000     C                   ENDIF
049300000000     C                   ELSE
049400000000     C     $TGES         IFEQ      'G'
049500000000     C                   MOVEL     FUNAFG        FAT(I)
049600000000     C                   ELSE
049700000000     C                   MOVEL     FUNAFI        FAT(I)
049800000000     C                   ENDIF
049900000000     C                   ENDIF
050000000000     C     FAT(I)        IFEQ      'E'
050100000000     C                   MOVEL     USER          WEDP              3
050200000000     C     WEDP          IFEQ      'EDP'
050300000000     C                   MOVEL     'S'           FAT(I)
050400000000     C                   ELSE
050500000000     C                   MOVEL     'N'           FAT(I)
050600000000     C                   ENDIF
050700000000     C                   ENDIF
050800000000     C                   ENDDO
050900000000     C                   ENDSR
051000000000     C*---------------------------------------------------------------*
051100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
051200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
051300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
051400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
051500000000     C*---------------------------------------------------------------*
051600000000     C     OUTFUN        BEGSR
051700000000     C     1             DO        8             I
051800000000     C                   MOVEL     *BLANKS       FUV(I)
051900000000     C                   ENDDO
052000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
052100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
052200000000     C     1             DO        31            I
052300000000     C     FAT(I)        IFEQ      'S'
052400000000     C     FDE(I)        ANDNE     *BLANKS
052500000000     C                   ADD       1             FUNNAT
052600000000     C                   ENDIF
052700000000     C                   ENDDO
052800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
052900000000     C     FUNNAT        IFGT      8
053000000000     C                   MOVEL     'S'           FAT(24)
053100000000     C                   ENDIF
053200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
053300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
053400000000     C                   Z-ADD     8             FUNVIS            5 0
053500000000     C     FAT(23)       IFEQ      'S'
053600000000     C                   SUB       1             FUNVIS
053700000000     C                   ENDIF
053800000000     C     FAT(24)       IFEQ      'S'
053900000000     C                   SUB       1             FUNVIS
054000000000     C                   ENDIF
054100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
054200000000     C                   Z-ADD     *ZEROS        W                 5 0
054300000000     C                   ADD       1             FUNULT
054400000000     C     FUNULT        IFGT      31
054500000000     C                   Z-ADD     1             FUNULT
054600000000     C                   ENDIF
054700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
054800000000     C     FUNULT        DO        31            I
054900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
055000000000     C     W             IFLT      FUNVIS
055100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
055200000000     C     FAT(I)        IFEQ      'S'
055300000000     C     FDE(I)        ANDNE     *BLANKS
055400000000     C     I             IFEQ      23
055500000000     C     I             OREQ      24
055600000000     C                   ELSE
055700000000     C                   ADD       1             W
055800000000     C                   MOVEL     FDE(I)        FUV(W)
055900000000     C                   ENDIF
056000000000     C                   ENDIF
056100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
056200000000     C                   Z-ADD     I             FUNULT
056300000000     C                   ENDIF
056400000000     C                   ENDDO
056500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
056600000000     C                   MOVEL     FUV(1)        VFD001
056700000000     C                   MOVEL     FUV(2)        VFD002
056800000000     C                   MOVEL     FUV(3)        VFD003
056900000000     C                   MOVEL     FUV(4)        VFD004
057000000000     C                   MOVEL     FUV(5)        VFD005
057100000000     C                   MOVEL     FUV(6)        VFD006
057200000000     C     FAT(23)       IFEQ      'N'
057300000000     C     FAT(24)       ANDEQ     'N'
057400000000     C                   MOVEL     FUV(7)        VFD007
057500000000     C                   MOVEL     FUV(8)        VFD008
057600000000     C                   ELSE
057700000000     C     FAT(23)       IFEQ      'S'
057800000000     C     FAT(24)       ANDEQ     'S'
057900000000     C                   MOVEL     FDE(23)       VFD007
058000000000     C                   MOVEL     FDE(24)       VFD008
058100000000     C                   ELSE
058200000000     C                   MOVEL     FUV(7)        VFD007
058300000000     C     FAT(23)       IFEQ      'S'
058400000000     C                   MOVEL     FDE(23)       VFD008
058500000000     C                   ELSE
058600000000     C                   MOVEL     FDE(24)       VFD008
058700000000     C                   ENDIF
058800000000     C                   ENDIF
058900000000     C                   ENDIF
059000000000     C                   MOVEL     '0'           $TESVD
059100000000     C                   ENDSR
059200000000     C*---------------------------------------------------------------*
059300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
059400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
059500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
059600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
059700000000     C*---------------------------------------------------------------*
059800000000     C     CONFUN        BEGSR
059900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
060000000000     C     1             DO        31            I
060100000000     C     KEY           IFEQ      KEYS(I)
060200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
060300000000     C     FAT(I)        IFNE      'S'
060400000000     C                   MOVEL     MSG(1)        $MSG
060500000000     C                   EXSR      CARMSG
060600000000     C                   MOVEL     '1'           $ERR
060700000000     C                   ENDIF
060800000000     C                   ENDIF
060900000000     C                   ENDDO
061000000000     C                   ENDSR
061100000000**   SCHIERA: MSG  (MESSAGGI)
061200000000TREC82R- Tasto funzionale non valido.                                          1
061300000000TREC82R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
061400000000TREC82R- Opzione non valida.                                                   3
061500000000TREC82R- Opzione abilitata ma non gestita. Avvertire CED.                      4
061600000000TREC82R- Inizio elenco.                                                        5
061700000000TREC82R- Fine elenco.                                                          6
061800000000TREC82R- Immettere competenza finale maggiore o uguale a quella iniziale       7
061900000000TREC82R- Immettere data formalmente valida                                     8
062000000000TREC82R- Immettere data finale maggiore o uguale a quella iniziale             9
062100000000TREC82R- Immettere tipo voce valido                                           10
062200000000TREC82R- Immettere voce finale maggiore o uguale a quella iniziale            11
062300000209TREC82R- Data non valida.                                                     12
062400000000TREC82R- 13
062500000000TREC82R- 14
062600000000TREC82R- 15
062700000000TREC82R- 16
062800000000TREC82R- 17
062900000000TREC82R- 18
063000000000TREC82R- 19
063100000000TREC82R- 20
063200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
063300000000F1    -                   -    NN     NN
063400000000F2    -                   -    NN     NN
063500000000F3    -F3=Fine            -    SS     SS
063600000000F4    -                   -    NN     NN
063700000000F5    -                   -    NN     NN
063800000000F6    -F6=Lancio          -    SS     SS
063900000000F7    -                   -    NN     NN
064000000000F8    -                   -    NN     NN
064100000000F9    -                   -    NN     NN
064200000000F10   -                   -    NN     NN
064300000000F11   -                   -    NN     NN
064400000000F12   -F12=Ritorno        -    SS     SS
064500000000F13   -                   -    NN     NN
064600000000F14   -                   -    NN     NN
064700000000F15   -                   -    NN     NN
064800000000F16   -                   -    NN     NN
064900000000F17   -                   -    NN     NN
065000000000F18   -                   -    NN     NN
065100000000F19   -                   -    NN     NN
065200000000F20   -                   -    NN     NN
065300000000F21   -                   -    NN     NN
065400000000F22   -                   -    NN     NN
065500000000F23   -                   -    NN     NN
065600000000F24   -F24=Altri tasti    -    NN     NN
065700000000ENTER -                   -    SS     SS
065800000000ROLLUP-                   -    NN     NN
065900000000ROLLDN-                   -    NN     NN
066000000000HELP  -                   -    NN     NN
066100000000PRINT -                   -    NN     NN
066200000000BSPACE-                   -    NN     NN
066300000000CLEAR -                   -    NN     NN
