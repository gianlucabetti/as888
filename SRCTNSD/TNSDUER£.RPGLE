000100020325     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020325      *------------------------------------------------------------------------*
000300020325      * STAMPA DOTAZIONI UNITA' EDP PER PUNTO OPERATIVO - PARAMETRI            *
000400020325      *------------------------------------------------------------------------*
000500020325
000600000000     FTNSDUED   CF   E             WORKSTN INFDS(DEVDS1)
000700000000     F                                     SFILE(SDUEDVM:MSGULT)
000800020528     FAZORG21l  IF   E           K DISK
000900020528     fazorg01l  if   e           k disk
001000020325
001100000000     D*------------------------------------------------------------------------*
001200000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001300000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001400000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001600000000     D FFU             S              6    DIM(31)                              *FUNZIONE
001700000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
001800000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
001900000000     D* TABELLA DESCRIZIONI A VIDEO
002000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002200000000     D KEYS            S              1    DIM(40)
002300000000     D*------------------------------------------------------------------------*
002400000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002500000000     D DSFUN         E DS
002600000000     D  FUNRIG                 1     40
002700000000     D*------------------------------------------------------------------------*
002800000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002900000000     D DSMSG         E DS
003000000000     D*------------------------------------------------------------------------*
003100000000     D* PARAMETRI CHIAMATA ROUTINE XFIL (RITORNO FILIALE IN CUI SONO)
003200020709     D**!!! DSFIL         E DS
003300000000     D*------------------------------------------------------------------------*
003400000000     D* DS PER GESTIONE TASTI FUNZIONALI
003500000000     D DSKEYS        E DS
003600000000     D  KEYRIG                 1     40
003700000000     D*------------------------------------------------------------------------*
003800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
003900000000     D                SDS
004000000000     D  USER                 254    263
004100000000     D*------------------------------------------------------------------------*
004200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
004300000000     D DEVDS1          DS
004400000000     D* CHIAVE FUNZIONALE BATTUTA
004500000000     D  KEY                  369    369
004600000000     D* POSIZIONE CURSORE (IN HEX)
004700000000     D  CURSOR               370    371
004800000000     D*------------------------------------------------------------------------*
004900000000     D* ARCHITETTURA
005000000000     D KPJBA         E DS
005100000000     D*------------------------------------------------------------------------*
005200000000     D PARAM           DS                  INZ
005300000000     D  PARTIP                 1      2  0                                      TIPO
005400000000     D  PARMAG                 3      5  0                                      FILAIL
005500000000     D  PARLIF                 6      6                                         LIVELL
005600951103     D  PARDIF                 7      7                                         DIFF
005700000000     D*------------------------------------------------------------------------*
005800000000     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
005900000000     D DSUNIT        E DS
006000020325
006100020325     d tibs02ds      e ds
006200020709     d ddatiute      e ds
006300020709     d azuteds       e ds                  extname(azute00f)
006400020709     d tibs34ds      e ds                  inz
006500020325
006600000000     C*------------------------------------------------------------------------*
006700000000     C* MAIN LINE
006800000000     C*------------------------------------------------------------------------*
006900000000     C*
007000000000     C* CICLO FINO A CHE:
007100000000DO  1C     $FINE         DOUEQ     '1'
007200000000     C*
007300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
007400000000     C*
007500000000     C* CONTROLLA GESTIONE VIDEO
007600000000IF  2C     $GES          IFEQ      'V1'
007700000000     C                   EXSR      GESV1                                        *PARAMETRI
007800000000E   2C                   ENDIF
007900000000     C*
008000000000E   1C                   ENDDO
008100000000     C*
008200000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
008300000000IF  1C     $F03          IFEQ      '1'
008400000000     C                   MOVEL     '1'           DSUF03
008500000000E   1C                   ENDIF
008600000000     C*
008700000000     C* FINE MAIN DEL PGM
008800000000     C                   MOVEL     DSUNIT        KPJBU
008900000000     C*
009000000000     C                   SETON                                        LR
009100000000     C*------------------------------------------------------------------------*
009200000000     C* GESV1 - GESTIONE VIDEO PARAMETRI
009300000000     C*------------------------------------------------------------------------*
009400000000     C     GESV1         BEGSR
009500000000     C*
009600000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
009700000000     C                   EXSR      CARV1                                        *CARICA VIDEO
009800000000E   1C                   ENDIF
009900000000     C*
010000000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010100000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010200000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010300000000E   1C                   ENDIF
010400000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010500000000     C*
010600000000     C                   EXFMT     SDUEDV1                                      *PARAMETRI
010700000000     C*
010800000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
010900000000     C*
011000000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011100000000SL  2C                   SELECT
011200000000     C     KEY           WHENEQ    ENTER
011300000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011400000000     C     KEY           WHENEQ    F6
011500000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
011600000000     C     KEY           WHENEQ    F22
011700000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
011800000000     C                   OTHER
011900000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012000000000E   2C                   ENDSL
012100000000E   1C                   ENDIF
012200000000     C*
012300000000     C                   ENDSR
012400000000     C*------------------------------------------------------------------------*
012500000000     C* CARV1 - CARICA VIDEO PARAMETRI
012600000000     C*------------------------------------------------------------------------*
012700000000     C     CARV1         BEGSR
012800000000     C*
012900000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013000000000     C*
013100000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013200000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013300000000     C*
013400000000     C                   WRITE     SDUEDVT                                      *TESTATA
013500000000     C                   WRITE     SDUEDVF                                      *FUNZIONI
013600000000     C*
013700000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
013800000000     C*
013900000000     C                   ENDSR
014000000000     C*------------------------------------------------------------------------*
014100000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
014200000000     C*------------------------------------------------------------------------*
014300000000     C     ENTV1         BEGSR
014400000000     C*
014500000000     C* CONTROLLA I CAMPI IMMESSI
014600000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
014700000000     C*
014800000000     C                   ENDSR
014900000000     C*------------------------------------------------------------------------*
015000000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015100000000     C*------------------------------------------------------------------------*
015200000000     C     F06GES        BEGSR
015300000000     C*
015400000000     C* CONTROLLA I CAMPI IMMESSI
015500000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015600000000     C*
015700000000     C* NON CI SONO ERRORI
015800000000IF  1C     $ERR          IFEQ      '0'
015900000000     C*
016000000000     C* LANCIO PROGRAMMA DI STAMPA
016100000000     C                   MOVEL     V1CTIP        PARTIP                         *TIPO UNITA'
016200000000     C                   MOVEL     V1CMAG        PARMAG                         *FILAILE
016300000000     C                   MOVEL     V1CLIF        PARLIF                         *LIVELLO FILIALE
016400951103     C                   MOVEL     V1CDIF        PARDIF                         *SOLO PER DIFFERENZE
016500000000     C*
016600000000     C                   MOVEL     'SDUF'        KCOAZ
016700000000     C                   MOVEL     PARAM         KPJBU
016800000000     C                   CALL      'BCH10'
016900000000     C                   PARM                    KPJBA
017000000000     C*
017100000000     C                   MOVEL     MSG(19)       $MSG                           *STAMPA LANCIATA ...
017200000000     C                   EXSR      CARMSG
017300000000     C*
017400000000E   1C                   ENDIF
017500000000     C*
017600000000     C                   ENDSR
017700000000     C*------------------------------------------------------------------------*
017800000000     C* F22GES -GEST.TASTO FUNZ. F22 ==> STAMPE
017900000000     C*------------------------------------------------------------------------*
018000000000     C     F22GES        BEGSR
018100000000     C*
018200000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
018300000000     C                   Z-ADD     80            LENGH            15 5
018400000000     C                   CALL      'QCAEXEC'
018500000000     C                   PARM                    CMD(1)
018600000000     C                   PARM                    LENGH
018700000000     C*
018800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
018900000000     C*
019000000000     C                   ENDSR
019100000000     C*------------------------------------------------------------------------*
019200000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019300000000     C*------------------------------------------------------------------------*
019400000000     C     DECODI        BEGSR
019500000000     C*
019600000000     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
019700941219     C                   SETOFF                                       616273
019800951103     C                   SETOFF                                       74
019900000000     C*--------------------
020000000000     C* TIPO UNITA'
020100000000     C*--------------------
020200000000IF  1C     $ERR          IFEQ      '0'
020300000000IF  2C     V1CTIP        IFNE      *BLANKS                                      *SELEZIONATO
020400000000     C     V1CTIP        ANDNE     *ZEROS
020500000000     C                   MOVEL     V1CTIP        WTIP              2
020600000000     C                   MOVEL     *BLANKS       WDTIP            30
020700000000     C                   EXSR      CHKTIP
020800000000     C                   MOVEL     WTIP          V1CTIP
020900000000     C                   MOVEL     WDTIP         V1DTIP
021000000000     C*
021100000000IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
021200000000     C                   MOVEL     MSG(8)        $MSG
021300000000     C                   EXSR      CARMSG
021400000000     C                   SETON                                        61
021500000000E   3C                   ENDIF
021600000000     C*
021700000000X   2C                   ELSE                                                   *NON SELEZIONATO
021800000000     C                   MOVEL     *ZEROS        V1CTIP
021900000000     C                   MOVEL     *BLANKS       V1DTIP
022000000000E   2C                   ENDIF
022100000000E   1C                   ENDIF
022200000000     C*--------------------
022300000000     C* MAGAZZINO
022400000000     C*--------------------
022500000000IF  1C     $ERR          IFEQ      '0'
022600000000IF  2C     V1CMAG        IFNE      *BLANKS                                      *SELEZIONATO
022700000000     C     V1CMAG        ANDNE     *ZEROS
022800000000     C                   MOVEL     V1CMAG        WMAG              3
022900000000     C                   MOVEL     *BLANKS       WDMAG            30
023000000000     C                   EXSR      CHKMAG
023100000000     C                   MOVEL     WMAG          V1CMAG
023200000000     C                   MOVEL     WDMAG         V1DMAG
023300000000     C*
023400000000IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
023500000000     C                   MOVEL     MSG(9)        $MSG
023600000000     C                   EXSR      CARMSG
023700000000     C                   SETON                                        62
023800000000E   3C                   ENDIF
023900000000     C*
024000000000X   2C                   ELSE                                                   *NON SELEZIOANTO
024100000000     C                   MOVEL     *ZEROS        V1CMAG
024200000000     C                   MOVEL     *BLANKS       V1DMAG
024300000000E   2C                   ENDIF
024400000000E   1C                   ENDIF
024500000000     C*--------------------
024600000000     C* LIVELLO FILIALE
024700000000     C*--------------------
024800000000IF  1C     $ERR          IFEQ      '0'
024900000000IF  2C     V1CLIF        IFEQ      '1'
025000000000     C     V1CLIF        OREQ      '2'
025100000000     C     V1CLIF        OREQ      'T'
025200000000     C                   ELSE
025300000000     C                   MOVEL     '1'           $ERR
025400941219     C                   MOVEL     MSG(7)        $MSG
025500000000     C                   EXSR      CARMSG
025600000000     C                   SETON                                        73
025700951103E   2C                   ENDIF
025800000000E   1C                   ENDIF
025900951103     C*--------------------
026000951103     C* STAMPA SOLO PER DIFFERENZA
026100951103     C*--------------------
026200951103IF  1C     $ERR          IFEQ      '0'
026300951103IF  2C     V1CDIF        IFEQ      'S'
026400951103     C     V1CDIF        OREQ      'N'
026500951103     C                   ELSE
026600951103     C                   MOVEL     '1'           $ERR
026700951103     C                   MOVEL     MSG(10)       $MSG
026800951103     C                   EXSR      CARMSG
026900951103     C                   SETON                                        74
027000951103E   2C                   ENDIF
027100951103E   1C                   ENDIF
027200951103     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C* CHKTIP - CONTROLLA IL TIPO DI UNITA'
027600000000     C*           INPUT:  WTIP  - VALORE IMMESSO A VIDEO
027700000000     C*           OUTPUT  WDTIP - DECODIFICA TIPO UNITA'
027800000000     C*------------------------------------------------------------------------*
027900000000     C     CHKTIP        BEGSR
028000000000     C*
028100000000     C* CONTROLLO SE E' STATO INSERITO '?'
028200020325     C     '?'           SCAN      WTIP                                   99
028300000000     C*
028400000000     C* E' STATO INSERITO: LANCIO IL PGM DI INTERROGAZIONE TABELLE
028500020325     c                   if        *in99                                        --- 1 -->
028600020325     c                   clear                   TIBS02DS
028700020325     c                   movel     'R'           t02mod
028800020325     c                   movel     knsif         t02sif
028900020325     c                   movel     'UNT'         t02cod
029000020325     c                   call      'TIBS02R'
029100020325     c                   parm                    KPJBA
029200020325     c                   parm                    TIBS02DS
029300020325     c                   movel     T02ke1        wtip
029400020325     c                   movel     t02uni        wdtip
029500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
029600020325     c                   endif                                                  <-- 1 ---
029700000000     C*
029800000000     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN TABELLA
029900020325     c                   clear                   TIBS02DS
030000020325     c                   movel     'C'           T02mod
030100020325     c                   movel     knsif         t02sif
030200020325     c                   movel     'UNT'         t02cod
030300020325     c                   movel(p)  wtip          T02ke1
030400020325     c                   call      'TIBS02R'
030500020325     c                   parm                    KPJBA
030600020325     c                   parm                    TIBS02DS
030700020325     c                   if        t02err <> *blanks
030800020325     c                   eval      $err = '1'
030900020325     c                   clear                   wdtip
031000020325     c                   else
031100020325     c                   movel     t02uni        wdtip
031200020325     c                   endif
031300000000     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600000000     C* CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
031700000000     C*           INPUT:  WMAG  - VALORE IMMESSO A VIDEO
031800000000     C*           OUTPUT  WDMAG - DECODIFICA MAGAZZINO UBIACAZIONE
031900000000     C*------------------------------------------------------------------------*
032000000000     C     CHKMAG        BEGSR
032100000000     C*
032200000000     C* CONTROLLO SE E' STATO INSERITO '?'
032300000000     C     '?'           SCAN      WMAG          A                 1 0    99
032400000000     C*
032500000000     C* E' STATO INSERITO; LANCIO IL PGM DI INTERROGAZIONE LINEE ARRIVO
032600000000IF  1C     *IN99         IFEQ      '1'
032700000000     C                   MOVEL     *BLANKS       §COD1
032800000000     C                   MOVE      *BLANKS       §DES1
032900000000     C                   CALL      'TNSD20R'
033000000000     C                   PARM                    §COD1             3
033100000000     C                   PARM                    §TIP              1
033200000000     C                   PARM                    §DES1            25
033300000000     C*
033400000000     C                   MOVEL     §COD1         WMAG
033500000000     C                   MOVEL     §DES1         WDMAG
033600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
033700000000     C*
033800000000     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN AZORG
033900000000X   1C                   ELSE
034000000000     C                   MOVE      WMAG          ORGFIL
034100020528     C     KEYORG        CHAIN     AZORG21l                           99
034200000000IF  2C     *IN99         IFEQ      '0'
034300000000     C     ORGFVA        ANDEQ     *BLANKS
034400000000     C                   MOVEL     ORGFIL        WMAG
034500000000     C                   MOVEL     ORGDES        WDMAG
034600000000X   2C                   ELSE
034700020528     c     keyorg        chain     azorg01l
034800020528     c                   if        %found(azorg01l) and orgfva = *blanks
034900020528     c                   movel     orgfil        wmag
035000020528     c                   movel     orgdes        wdmag
035100020528     c                   else
035200000000     C                   MOVEL     '1'           $ERR
035300000000     C                   MOVEL     *ALL'*'       WDMAG
035400020528     c                   endif
035500000000E   2C                   ENDIF
035600000000E   1C                   ENDIF
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C* *INZSR - ROUTINE INIZIALE
036100000000     C*------------------------------------------------------------------------*
036200000000     C     *INZSR        BEGSR
036300000000     C*
036400000000     C* RICEVIMENTO PARAMETRI
036500000000     C     *ENTRY        PLIST
036600000000     C                   PARM                    KPJBA
036700000000     C                   MOVEL     KPJBU         DSUNIT
036800000000     C*
036900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
037000000000     C     KEYORG        KLIST
037100000000     C                   KFLD                    ORGFIL
037200020709
037300020709     c     *dtaara       define    §azute        azuteds
037400020709     c     *dtaara       define    §datiute      ddatiute
037500020709
037600020709     c                   in(E)     *dtaara
037700020709     c                   if        %Error or rsut = *blanks
037800020709     c                   clear                   tibs34ds
037900020709     c                   call      'TIBS34R'
038000020709     c                   parm                    Tibs34Ds
038100020709     c                   in        *dtaara
038200020709     c                   endif
038300000000     C*
038400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
038500000000     C                   EXSR      INZSTD
038600000000     C*
038700000000     C* IMPOSTO CAMPI TESTATA VIDEO
038800020709     C**!!!              MOVEL     DSFIRS        VTDRSU                         *RAG.SOC.UTENTE
038900020709     c                   Movel     Rsut          VtdRsu                         *RAG.SOC.UTENTE
039000000000     C*
039100000000     C* IMPOSTO CAMPI VIDEO RICHIESTA PARAMETRI
039200000000     C                   MOVEL     'T'           V1CLIF                         *LIVELLO FILIALI
039300951103     C                   MOVEL     'N'           V1CDIF                         *SOLO PER DIFFERENZE
039400000000     C*
039500000000     C* EMETTO FORMATO VIDEO
039600000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
039700000000     C*
039800000000     C                   ENDSR
039900000000     C*------------------------------------------------------------------------*
040000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040200000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
040300000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
040400000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
040500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040700000000     C*------------------------------------------------------------------------*
040800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
040900000000     C*------------------------------------------------------------------------*
041000000000     C     INZSTD        BEGSR
041100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
041200000000     C                   CALL      'XKEYS'
041300000000     C                   PARM                    DSKEYS
041400000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
041500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
041600000000     C     XMSGPA        PLIST
041700000000     C                   PARM                    DSMSMS
041800000000     C                   PARM                    DSMSPG
041900000000     C                   PARM                    DSMSKE
042000000000     C                   PARM                    DSMSER
042100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
042200000000     C                   MOVEL     'TNSDUER'     DSMSPG                         *NOME PGM CHIAMANTE
042300000000     C                   MOVEL     'TNSDUER'     VMCQUE                         *CODA MESSAGGI DESTI
042400000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
042500000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
042600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
042700000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
042800000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
042900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
043000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
043100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
043200000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
043300000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
043400000000     C* LANCIO ROUTINE PER VERIFICARE SE
043500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
043600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
043700020709     C**!!!              CALL      'XFIL'
043800020709     C**!!!              PARM                    DSFIL
043900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
044000000000     C                   BITOFF    '01234567'    POSMI1            1
044100000000     C                   BITOFF    '01234567'    POSMI2            1
044200000000     C                   BITOFF    '01234567'    POSMA1            1
044300000000     C                   BITOFF    '01234567'    POSMA2            1
044400000000     C                   BITON     '34'          POSMI1            1            *18
044500000000     C                   BITON     '7'           POSMI2            1            *01
044600000000     C                   BITON     '34'          POSMA1            1            *18
044700000000     C                   BITON     '0'           POSMA2            1            *80
044800000000     C                   MOVE      POSMI2        POSMIN            2
044900000000     C                   MOVEL     POSMI1        POSMIN
045000000000     C                   MOVE      POSMA2        POSMAX            2
045100000000     C                   MOVEL     POSMA1        POSMAX
045200000000     C                   ENDSR
045300000000     C*------------------------------------------------------------------------*
045400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
045500000000     C*------------------------------------------------------------------------*
045600000000     C     FUNSTD        BEGSR
045700000000     C                   SELECT
045800000000     C     KEY           WHENEQ    F3
045900000000     C                   EXSR      F03STD
046000000000     C     KEY           WHENEQ    F12
046100000000     C                   EXSR      F12STD
046200000000     C     KEY           WHENEQ    F24
046300000000     C                   EXSR      F24STD
046400000000     C                   OTHER
046500000000     C                   MOVEL     MSG(2)        $MSG
046600000000     C                   EXSR      CARMSG
046700000000     C                   ENDSL
046800000000     C                   ENDSR
046900000000     C*------------------------------------------------------------------------*
047000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
047100000000     C*------------------------------------------------------------------------*
047200000000     C     F03STD        BEGSR
047300000000     C*
047400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
047500000000     C                   MOVEL     '1'           $F03
047600000000     C*
047700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
047800000000     C                   MOVEL     '1'           $FINE
047900000000     C*
048000000000     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
048300000000     C*------------------------------------------------------------------------*
048400000000     C     F12STD        BEGSR
048500000000     C*
048600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
048700000000     C                   MOVEL     '1'           $FINE
048800000000     C*
048900000000     C                   ENDSR
049000000000     C*------------------------------------------------------------------------*
049100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
049200000000     C*------------------------------------------------------------------------*
049300000000     C     F24STD        BEGSR
049400000000     C*
049500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
049600000000     C                   EXSR      OUTFUN
049700000000     C*
049800000000     C                   ENDSR
049900000000     C*------------------------------------------------------------------------*
050000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
050100000000     C*          CODA MESSAGGI DEL PGM
050200000000     C*------------------------------------------------------------------------*
050300000000     C     CARMSG        BEGSR
050400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
050500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
050600000000     C     MSGULT        IFEQ      0
050700000000     C                   MOVEL     '1'           *IN40
050800000000     C                   WRITE     SDUEDVK
050900000000     C                   MOVEL     '0'           *IN40
051000000000     C                   ENDIF
051100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
051200000000     C                   MOVEL     $MSG          DSMSMS
051300000000     C                   CALL      'XMSG'        XMSGPA
051400000000     C     DSMSER        IFEQ      0
051500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
051600000000     C                   ENDIF
051700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
051800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
051900000000     C                   WRITE     SDUEDVM                                      *SFL MESS
052000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
052100000000     C                   MOVEL     *BLANKS       $MSG
052200000000     C                   ENDSR
052300000000     C*------------------------------------------------------------------------*
052400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
052500000000     C*------------------------------------------------------------------------*
052600000000     C     OUTMSG        BEGSR
052700000000     C* VISUALIZZAZIONE MESSAGGIO
052800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
052900000000     C                   WRITE     SDUEDVK                                      *CONTR MESS
053000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
053100000000     C                   Z-ADD     *ZEROS        MSGULT
053200000000     C                   ENDSR
053300000000     C*------------------------------------------------------------------------*
053400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
053500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
053600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
053700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
053800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
053900000000     C*------------------------------------------------------------------------*
054000000000     C     CARFUN        BEGSR
054100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
054200000000     C     1             DO        31            I                 5 0          --- 1 -->
054300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
054400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
054500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
054600020709     C**!!!DSFITI        IFEQ      'S'                                          --- 2 --> *SEDE
054700020709     c                   If        DutLpo = 'S'
054800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
054900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
055000000000     C                   ELSE                                                   *INTERROG.
055100000000     C                   MOVEL     FUNASI        FAT(I)
055200000000     C                   ENDIF
055300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
055400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
055500000000     C                   MOVEL     FUNAFG        FAT(I)
055600000000     C                   ELSE                                                   *INTERROG.
055700000000     C                   MOVEL     FUNAFI        FAT(I)
055800000000     C                   ENDIF
055900000000     C                   ENDIF                                                  <-- 2 ---
056000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
056100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
056200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
056300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
056400000000     C                   ELSE
056500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
056600000000     C                   ENDIF
056700000000     C                   ENDIF
056800000000     C                   ENDDO                                                  <-- 1 ---
056900000000     C                   ENDSR
057000000000     C*------------------------------------------------------------------------*
057100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
057200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
057300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
057400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
057500000000     C*------------------------------------------------------------------------*
057600000000     C     OUTFUN        BEGSR
057700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
057800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
057900000000     C                   ENDDO
058000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
058100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
058200000000     C     1             DO        31            I
058300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
058400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
058500000000     C                   ADD       1             FUNNAT
058600000000     C                   ENDIF
058700000000     C                   ENDDO
058800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
058900000000     C     FUNNAT        IFGT      8
059000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
059100000000     C                   ENDIF
059200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
059300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
059400000000     C                   Z-ADD     8             FUNVIS            5 0
059500000000     C     FAT(23)       IFEQ      'S'
059600000000     C                   SUB       1             FUNVIS
059700000000     C                   ENDIF
059800000000     C     FAT(24)       IFEQ      'S'
059900000000     C                   SUB       1             FUNVIS
060000000000     C                   ENDIF
060100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
060200000000     C                   Z-ADD     *ZEROS        W                 5 0
060300000000     C                   ADD       1             FUNULT
060400000000     C     FUNULT        IFGT      31
060500000000     C                   Z-ADD     1             FUNULT
060600000000     C                   ENDIF
060700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
060800000000     C     FUNULT        DO        31            I                              --- 1 -->
060900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
061000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
061100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
061200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
061300000000     C     FDE(I)        ANDNE     *BLANKS
061400000000     C     I             IFEQ      23                                           *TOLGO F23
061500000000     C     I             OREQ      24                                           *E F24
061600000000     C                   ELSE
061700000000     C                   ADD       1             W
061800000000     C                   MOVEL     FDE(I)        FUV(W)
061900000000     C                   ENDIF
062000000000     C                   ENDIF                                                  <-- 3 ---
062100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
062200000000     C                   Z-ADD     I             FUNULT
062300000000     C                   ENDIF                                                  <-- 2 ---
062400000000     C                   ENDDO                                                  <-- 1 ---
062500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
062600000000     C                   MOVEL     FUV(1)        VFD001
062700000000     C                   MOVEL     FUV(2)        VFD002
062800000000     C                   MOVEL     FUV(3)        VFD003
062900000000     C                   MOVEL     FUV(4)        VFD004
063000000000     C                   MOVEL     FUV(5)        VFD005
063100000000     C                   MOVEL     FUV(6)        VFD006
063200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
063300000000     C     FAT(24)       ANDEQ     'N'
063400000000     C                   MOVEL     FUV(7)        VFD007
063500000000     C                   MOVEL     FUV(8)        VFD008
063600000000     C                   ELSE                                                   <-- 1 -->
063700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
063800000000     C     FAT(24)       ANDEQ     'S'
063900000000     C                   MOVEL     FDE(23)       VFD007
064000000000     C                   MOVEL     FDE(24)       VFD008
064100000000     C                   ELSE                                                   <-- 2 -->
064200000000     C                   MOVEL     FUV(7)        VFD007
064300000000     C     FAT(23)       IFEQ      'S'
064400000000     C                   MOVEL     FDE(23)       VFD008
064500000000     C                   ELSE
064600000000     C                   MOVEL     FDE(24)       VFD008
064700000000     C                   ENDIF
064800000000     C                   ENDIF                                                  <-- 2 ---
064900000000     C                   ENDIF                                                  <-- 1 ---
065000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
065100000000     C                   ENDSR
065200000000     C*------------------------------------------------------------------------*
065300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
065400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
065500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
065600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
065700000000     C*------------------------------------------------------------------------*
065800000000     C     CONFUN        BEGSR
065900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
066000000000     C     1             DO        31            I                              --- 1 -->
066100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
066200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
066300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
066400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
066500000000     C                   EXSR      CARMSG
066600000000     C                   MOVEL     '1'           $ERR
066700000000     C                   ENDIF                                                  <-- 3 ---
066800000000     C                   ENDIF                                                  <-- 2 ---
066900000000     C                   ENDDO                                                  <-- 1 ---
067000000000     C                   ENDSR
067100000000     O*------------------------------------------------------------------------*
067200000000**       MSG - MESSAGGI
067300000000TNSDUER- Tasto funzionale non valido.                                          1
067400000000TNSDUER- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
067500000000TNSDUER- Opzione non valida.                                                   3
067600000000TNSDUER- Opzione abilitata ma non gestita. Avvertire CED.                      4
067700000000TNSDUER- Inizio elenco.                                                        5
067800000000TNSDUER- Fine elenco.                                                          6
067900941219TNSDUER- Immettere 1 per stampare solo i 1° liv, 2 per i 2° liv, T per tutti   7
068000000000TNSDUER- Tipo unità errato                                                     8
068100000000TNSDUER- Magazzino errato                                                      9
068200951103TNSDUER- 'S' per stampare solo le unità con differenze nelle Qtà in dotazione 10
068300000000TNSDUER- 11
068400000000TNSDUER- 12
068500941219TNSDUER- 13
068600000000TNSDUER- 14
068700941219TNSDUER- 15
068800000000TNSDUER- 16
068900000000TNSDUER- 17
069000000000TNSDUER- 18
069100000000TNSDUER- Stampa lanciata ...                                                  19
069200941219TNSDUER- 20
069300000000** ======== SCHIERA: CMD  (COMANDI)                        ================
069400000000WRKSPLF
069500000000
069600000000
069700000000
069800000000
069900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
070000000000F1    -                   -    NN     NN
070100000000F2    -                   -    NN     NN
070200000000F3    -F3=Fine            -    SS     SS
070300000000F4    -                   -    NN     NN
070400000000F5    -                   -    NN     NN
070500000000F6    -F6=Lancio          -    SS     SS
070600000000F7    -                   -    NN     NN
070700000000F8    -                   -    NN     NN
070800000000F9    -                   -    NN     NN
070900000000F10   -                   -    NN     NN
071000000000F11   -                   -    NN     NN
071100000000F12   -F12-Ritorno        -    SS     SS
071200000000F13   -                   -    NN     NN
071300000000F14   -                   -    NN     NN
071400000000F15   -                   -    NN     NN
071500000000F16   -                   -    NN     NN
071600000000F17   -                   -    NN     NN
071700000000F18   -                   -    NN     NN
071800000000F19   -                   -    NN     NN
071900000000F20   -                   -    NN     NN
072000000000F21   -                   -    NN     NN
072100000000F22   -F22=Gestione stampe-    SS     SS
072200000000F23   -                   -    NN     NN
072300000000F24   -F24=Altri tasti    -    NN     NN
072400000000ENTER -                   -    SS     SS
072500000000ROLLUP-                   -    NN     NN
072600000000ROLLDN-                   -    NN     NN
072700000000HELP  -                   -    NN     NN
072800000000PRINT -                   -    NN     NN
072900000000BSPACE-                   -    NN     NN
073000000000CLEAR -                   -    NN     NN
