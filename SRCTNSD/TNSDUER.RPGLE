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
003100000000     D* DS PER GESTIONE TASTI FUNZIONALI
003200000000     D DSKEYS        E DS
003300000000     D  KEYRIG                 1     40
003400000000     D*------------------------------------------------------------------------*
003500000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
003600000000     D                SDS
003700000000     D  USER                 254    263
003800000000     D*------------------------------------------------------------------------*
003900000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
004000000000     D DEVDS1          DS
004100000000     D* CHIAVE FUNZIONALE BATTUTA
004200000000     D  KEY                  369    369
004300000000     D* POSIZIONE CURSORE (IN HEX)
004400000000     D  CURSOR               370    371
004500000000     D*------------------------------------------------------------------------*
004600000000     D* ARCHITETTURA
004700000000     D KPJBA         E DS
004800000000     D*------------------------------------------------------------------------*
004900000000     D PARAM           DS                  INZ
005000000000     D  PARTIP                 1      2  0                                      TIPO
005100000000     D  PARMAG                 3      5  0                                      FILAIL
005200000000     D  PARLIF                 6      6                                         LIVELL
005300951103     D  PARDIF                 7      7                                         DIFF
005400000000     D*------------------------------------------------------------------------*
005500000000     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
005600000000     D DSUNIT        E DS
005700020325
005800020325     d tibs02ds      e ds
005900020709     d ddatiute      e ds
006000020709     d azuteds       e ds                  extname(azute00f)
006100020709     d tibs34ds      e ds                  inz
006200020325
006300000000     C*------------------------------------------------------------------------*
006400000000     C* MAIN LINE
006500000000     C*------------------------------------------------------------------------*
006600000000     C*
006700000000     C* CICLO FINO A CHE:
006800000000DO  1C     $FINE         DOUEQ     '1'
006900000000     C*
007000000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
007100000000     C*
007200000000     C* CONTROLLA GESTIONE VIDEO
007300000000IF  2C     $GES          IFEQ      'V1'
007400000000     C                   EXSR      GESV1                                        *PARAMETRI
007500000000E   2C                   ENDIF
007600000000     C*
007700000000E   1C                   ENDDO
007800000000     C*
007900000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
008000000000IF  1C     $F03          IFEQ      '1'
008100000000     C                   MOVEL     '1'           DSUF03
008200000000E   1C                   ENDIF
008300000000     C*
008400000000     C* FINE MAIN DEL PGM
008500000000     C                   MOVEL     DSUNIT        KPJBU
008600000000     C*
008700000000     C                   SETON                                        LR
008800000000     C*------------------------------------------------------------------------*
008900000000     C* GESV1 - GESTIONE VIDEO PARAMETRI
009000000000     C*------------------------------------------------------------------------*
009100000000     C     GESV1         BEGSR
009200000000     C*
009300000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
009400000000     C                   EXSR      CARV1                                        *CARICA VIDEO
009500000000E   1C                   ENDIF
009600000000     C*
009700000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
009800000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
009900000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010000000000E   1C                   ENDIF
010100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010200000000     C*
010300000000     C                   EXFMT     SDUEDV1                                      *PARAMETRI
010400000000     C*
010500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
010600020717
010700020717      * Se Profilo non OK devo uscire dal pgm
010800020717     c                   If        *In76
010900020717     c                   Eval      $err = '1'
011000020717     c                   Eval      $fine = '1'
011100020717     c                   EndIf
011200000000     C*
011300000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011400000000SL  2C                   SELECT
011500000000     C     KEY           WHENEQ    ENTER
011600000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011700000000     C     KEY           WHENEQ    F6
011800000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
011900000000     C     KEY           WHENEQ    F22
012000000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
012100000000     C                   OTHER
012200000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012300000000E   2C                   ENDSL
012400000000E   1C                   ENDIF
012500000000     C*
012600000000     C                   ENDSR
012700000000     C*------------------------------------------------------------------------*
012800000000     C* CARV1 - CARICA VIDEO PARAMETRI
012900000000     C*------------------------------------------------------------------------*
013000000000     C     CARV1         BEGSR
013100000000     C*
013200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013300000000     C*
013400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013600000000     C*
013700000000     C                   WRITE     SDUEDVT                                      *TESTATA
013800000000     C                   WRITE     SDUEDVF                                      *FUNZIONI
013900000000     C*
014000000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
014100000000     C*
014200000000     C                   ENDSR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
014500000000     C*------------------------------------------------------------------------*
014600000000     C     ENTV1         BEGSR
014700000000     C*
014800000000     C* CONTROLLA I CAMPI IMMESSI
014900000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015000000000     C*
015100000000     C                   ENDSR
015200000000     C*------------------------------------------------------------------------*
015300000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015400000000     C*------------------------------------------------------------------------*
015500000000     C     F06GES        BEGSR
015600000000     C*
015700000000     C* CONTROLLA I CAMPI IMMESSI
015800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015900000000     C*
016000000000     C* NON CI SONO ERRORI
016100000000IF  1C     $ERR          IFEQ      '0'
016200000000     C*
016300000000     C* LANCIO PROGRAMMA DI STAMPA
016400000000     C                   MOVEL     V1CTIP        PARTIP                         *TIPO UNITA'
016500000000     C                   MOVEL     V1CMAG        PARMAG                         *FILAILE
016600000000     C                   MOVEL     V1CLIF        PARLIF                         *LIVELLO FILIALE
016700951103     C                   MOVEL     V1CDIF        PARDIF                         *SOLO PER DIFFERENZE
016800000000     C*
016900000000     C                   MOVEL     'SDUF'        KCOAZ
017000000000     C                   MOVEL     PARAM         KPJBU
017100000000     C                   CALL      'BCH10'
017200000000     C                   PARM                    KPJBA
017300000000     C*
017400000000     C                   MOVEL     MSG(19)       $MSG                           *STAMPA LANCIATA ...
017500000000     C                   EXSR      CARMSG
017600000000     C*
017700000000E   1C                   ENDIF
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* F22GES -GEST.TASTO FUNZ. F22 ==> STAMPE
018200000000     C*------------------------------------------------------------------------*
018300000000     C     F22GES        BEGSR
018400000000     C*
018500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
018600000000     C                   Z-ADD     80            LENGH            15 5
018700000000     C                   CALL      'QCAEXEC'
018800000000     C                   PARM                    CMD(1)
018900000000     C                   PARM                    LENGH
019000000000     C*
019100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
019200000000     C*
019300000000     C                   ENDSR
019400000000     C*------------------------------------------------------------------------*
019500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019600000000     C*------------------------------------------------------------------------*
019700000000     C     DECODI        BEGSR
019800000000     C*
019900000000     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
020000941219     C                   SETOFF                                       616273
020100951103     C                   SETOFF                                       74
020200000000     C*--------------------
020300000000     C* TIPO UNITA'
020400000000     C*--------------------
020500000000IF  1C     $ERR          IFEQ      '0'
020600000000IF  2C     V1CTIP        IFNE      *BLANKS                                      *SELEZIONATO
020700000000     C     V1CTIP        ANDNE     *ZEROS
020800000000     C                   MOVEL     V1CTIP        WTIP              2
020900000000     C                   MOVEL     *BLANKS       WDTIP            30
021000000000     C                   EXSR      CHKTIP
021100000000     C                   MOVEL     WTIP          V1CTIP
021200000000     C                   MOVEL     WDTIP         V1DTIP
021300000000     C*
021400000000IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
021500000000     C                   MOVEL     MSG(8)        $MSG
021600000000     C                   EXSR      CARMSG
021700000000     C                   SETON                                        61
021800000000E   3C                   ENDIF
021900000000     C*
022000000000X   2C                   ELSE                                                   *NON SELEZIONATO
022100000000     C                   MOVEL     *ZEROS        V1CTIP
022200000000     C                   MOVEL     *BLANKS       V1DTIP
022300000000E   2C                   ENDIF
022400000000E   1C                   ENDIF
022500000000     C*--------------------
022600000000     C* MAGAZZINO
022700000000     C*--------------------
022800000000IF  1C     $ERR          IFEQ      '0'
022900000000IF  2C     V1CMAG        IFNE      *BLANKS                                      *SELEZIONATO
023000000000     C     V1CMAG        ANDNE     *ZEROS
023100000000     C                   MOVEL     V1CMAG        WMAG              3
023200000000     C                   MOVEL     *BLANKS       WDMAG            30
023300000000     C                   EXSR      CHKMAG
023400000000     C                   MOVEL     WMAG          V1CMAG
023500000000     C                   MOVEL     WDMAG         V1DMAG
023600000000     C*
023700000000IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
023800000000     C                   MOVEL     MSG(9)        $MSG
023900000000     C                   EXSR      CARMSG
024000000000     C                   SETON                                        62
024100000000E   3C                   ENDIF
024200000000     C*
024300000000X   2C                   ELSE                                                   *NON SELEZIOANTO
024400000000     C                   MOVEL     *ZEROS        V1CMAG
024500000000     C                   MOVEL     *BLANKS       V1DMAG
024600000000E   2C                   ENDIF
024700000000E   1C                   ENDIF
024800000000     C*--------------------
024900000000     C* LIVELLO FILIALE
025000000000     C*--------------------
025100000000IF  1C     $ERR          IFEQ      '0'
025200000000IF  2C     V1CLIF        IFEQ      '1'
025300000000     C     V1CLIF        OREQ      '2'
025400000000     C     V1CLIF        OREQ      'T'
025500000000     C                   ELSE
025600000000     C                   MOVEL     '1'           $ERR
025700941219     C                   MOVEL     MSG(7)        $MSG
025800000000     C                   EXSR      CARMSG
025900000000     C                   SETON                                        73
026000951103E   2C                   ENDIF
026100000000E   1C                   ENDIF
026200951103     C*--------------------
026300951103     C* STAMPA SOLO PER DIFFERENZA
026400951103     C*--------------------
026500951103IF  1C     $ERR          IFEQ      '0'
026600951103IF  2C     V1CDIF        IFEQ      'S'
026700951103     C     V1CDIF        OREQ      'N'
026800951103     C                   ELSE
026900951103     C                   MOVEL     '1'           $ERR
027000951103     C                   MOVEL     MSG(10)       $MSG
027100951103     C                   EXSR      CARMSG
027200951103     C                   SETON                                        74
027300951103E   2C                   ENDIF
027400951103E   1C                   ENDIF
027500951103     C*
027600000000     C                   ENDSR
027700000000     C*------------------------------------------------------------------------*
027800000000     C* CHKTIP - CONTROLLA IL TIPO DI UNITA'
027900000000     C*           INPUT:  WTIP  - VALORE IMMESSO A VIDEO
028000000000     C*           OUTPUT  WDTIP - DECODIFICA TIPO UNITA'
028100000000     C*------------------------------------------------------------------------*
028200000000     C     CHKTIP        BEGSR
028300000000     C*
028400000000     C* CONTROLLO SE E' STATO INSERITO '?'
028500020325     C     '?'           SCAN      WTIP                                   99
028600000000     C*
028700000000     C* E' STATO INSERITO: LANCIO IL PGM DI INTERROGAZIONE TABELLE
028800020325     c                   if        *in99                                        --- 1 -->
028900020325     c                   clear                   TIBS02DS
029000020325     c                   movel     'R'           t02mod
029100020325     c                   movel     knsif         t02sif
029200020325     c                   movel     'UNT'         t02cod
029300020325     c                   call      'TIBS02R'
029400020325     c                   parm                    KPJBA
029500020325     c                   parm                    TIBS02DS
029600020325     c                   movel     T02ke1        wtip
029700020325     c                   movel     t02uni        wdtip
029800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
029900020325     c                   endif                                                  <-- 1 ---
030000000000     C*
030100000000     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN TABELLA
030200020325     c                   clear                   TIBS02DS
030300020325     c                   movel     'C'           T02mod
030400020325     c                   movel     knsif         t02sif
030500020325     c                   movel     'UNT'         t02cod
030600020325     c                   movel(p)  wtip          T02ke1
030700020325     c                   call      'TIBS02R'
030800020325     c                   parm                    KPJBA
030900020325     c                   parm                    TIBS02DS
031000020325     c                   if        t02err <> *blanks
031100020325     c                   eval      $err = '1'
031200020325     c                   clear                   wdtip
031300020325     c                   else
031400020325     c                   movel     t02uni        wdtip
031500020325     c                   endif
031600000000     C*
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
032000000000     C*           INPUT:  WMAG  - VALORE IMMESSO A VIDEO
032100000000     C*           OUTPUT  WDMAG - DECODIFICA MAGAZZINO UBIACAZIONE
032200000000     C*------------------------------------------------------------------------*
032300000000     C     CHKMAG        BEGSR
032400000000     C*
032500000000     C* CONTROLLO SE E' STATO INSERITO '?'
032600000000     C     '?'           SCAN      WMAG          A                 1 0    99
032700000000     C*
032800000000     C* E' STATO INSERITO; LANCIO IL PGM DI INTERROGAZIONE LINEE ARRIVO
032900000000IF  1C     *IN99         IFEQ      '1'
033000000000     C                   MOVEL     *BLANKS       §COD1
033100000000     C                   MOVE      *BLANKS       §DES1
033200000000     C                   CALL      'TNSD20R'
033300000000     C                   PARM                    §COD1             3
033400000000     C                   PARM                    §TIP              1
033500000000     C                   PARM                    §DES1            25
033600000000     C*
033700000000     C                   MOVEL     §COD1         WMAG
033800000000     C                   MOVEL     §DES1         WDMAG
033900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
034000000000     C*
034100000000     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN AZORG
034200000000X   1C                   ELSE
034300000000     C                   MOVE      WMAG          ORGFIL
034400020528     C     KEYORG        CHAIN     AZORG21l                           99
034500000000IF  2C     *IN99         IFEQ      '0'
034600000000     C     ORGFVA        ANDEQ     *BLANKS
034700000000     C                   MOVEL     ORGFIL        WMAG
034800000000     C                   MOVEL     ORGDES        WDMAG
034900000000X   2C                   ELSE
035000020528     c     keyorg        chain     azorg01l
035100020528     c                   if        %found(azorg01l) and orgfva = *blanks
035200020528     c                   movel     orgfil        wmag
035300020528     c                   movel     orgdes        wdmag
035400020528     c                   else
035500000000     C                   MOVEL     '1'           $ERR
035600000000     C                   MOVEL     *ALL'*'       WDMAG
035700020528     c                   endif
035800000000E   2C                   ENDIF
035900000000E   1C                   ENDIF
036000000000     C*
036100000000     C                   ENDSR
036200000000     C*------------------------------------------------------------------------*
036300000000     C* *INZSR - ROUTINE INIZIALE
036400000000     C*------------------------------------------------------------------------*
036500000000     C     *INZSR        BEGSR
036600000000     C*
036700000000     C* RICEVIMENTO PARAMETRI
036800000000     C     *ENTRY        PLIST
036900000000     C                   PARM                    KPJBA
037000000000     C                   MOVEL     KPJBU         DSUNIT
037100000000     C*
037200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
037300000000     C     KEYORG        KLIST
037400000000     C                   KFLD                    ORGFIL
037500020709
037600020709     c     *dtaara       define    §azute        azuteds
037700020709     c     *dtaara       define    §datiute      ddatiute
037800020709
037900020709     c                   in(E)     *dtaara
038000020709     c                   if        %Error or rsut = *blanks
038100020709     c                   clear                   tibs34ds
038200020709     c                   call      'TIBS34R'
038300020709     c                   parm                    Tibs34Ds
038400020709     c                   in        *dtaara
038500020709     c                   endif
038600020717
038700020717      * Controllo il profilo utente
038800020717     c                   ExSr      CtrUte
038900000000     C*
039000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
039100000000     C                   EXSR      INZSTD
039200000000     C*
039300000000     C* IMPOSTO CAMPI TESTATA VIDEO
039400020709     c                   Movel     Rsut          VtdRsu                         *RAG.SOC.UTENTE
039500000000     C*
039600000000     C* IMPOSTO CAMPI VIDEO RICHIESTA PARAMETRI
039700000000     C                   MOVEL     'T'           V1CLIF                         *LIVELLO FILIALI
039800951103     C                   MOVEL     'N'           V1CDIF                         *SOLO PER DIFFERENZE
039900000000     C*
040000000000     C* EMETTO FORMATO VIDEO
040100000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
040200000000     C*
040300000000     C                   ENDSR
040400020717      *------------------------------------------------------------------------*
040500020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
040600020717      *------------------------------------------------------------------------*
040700020717     c     CtrUte        BegSr
040800020717
040900020717     c                   Eval      *In76 = *Off
041000020717
041100020717      * Controllo se il profilo è ok da tibs34
041200020717     c                   If        DutErr = 'E'
041300020717     c                   eval      $Msg = DutMsg
041400020717     c                   ExSr      CarMsg
041500020717     c                   eval      *In76 = *On
041600020717     c                   endIf
041700020717
041800020717      * Controllo se il profilo di sede
041900020717     c                   If        DutLpo <> 'S'
042000020717     c                   Eval      $Msg = Msg(11)
042100020717     c                   ExSr      CarMsg
042200020717     c                   Eval      *In76 = *On
042300020717     c                   EndIf
042400020717
042500020717     c                   endSr
042600000000     C*------------------------------------------------------------------------*
042700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042900000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
043000000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
043100000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
043200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043400000000     C*------------------------------------------------------------------------*
043500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
043600000000     C*------------------------------------------------------------------------*
043700000000     C     INZSTD        BEGSR
043800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
043900000000     C                   CALL      'XKEYS'
044000000000     C                   PARM                    DSKEYS
044100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
044200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
044300000000     C     XMSGPA        PLIST
044400000000     C                   PARM                    DSMSMS
044500000000     C                   PARM                    DSMSPG
044600000000     C                   PARM                    DSMSKE
044700000000     C                   PARM                    DSMSER
044800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
044900000000     C                   MOVEL     'TNSDUER'     DSMSPG                         *NOME PGM CHIAMANTE
045000000000     C                   MOVEL     'TNSDUER'     VMCQUE                         *CODA MESSAGGI DESTI
045100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
045200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
045300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
045400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
045500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
045600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
045700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
045800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
045900000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
046000000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
046100000000     C* LANCIO ROUTINE PER VERIFICARE SE
046200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
046300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
046400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
046500000000     C                   BITOFF    '01234567'    POSMI1            1
046600000000     C                   BITOFF    '01234567'    POSMI2            1
046700000000     C                   BITOFF    '01234567'    POSMA1            1
046800000000     C                   BITOFF    '01234567'    POSMA2            1
046900000000     C                   BITON     '34'          POSMI1            1            *18
047000000000     C                   BITON     '7'           POSMI2            1            *01
047100000000     C                   BITON     '34'          POSMA1            1            *18
047200000000     C                   BITON     '0'           POSMA2            1            *80
047300000000     C                   MOVE      POSMI2        POSMIN            2
047400000000     C                   MOVEL     POSMI1        POSMIN
047500000000     C                   MOVE      POSMA2        POSMAX            2
047600000000     C                   MOVEL     POSMA1        POSMAX
047700000000     C                   ENDSR
047800000000     C*------------------------------------------------------------------------*
047900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
048000000000     C*------------------------------------------------------------------------*
048100000000     C     FUNSTD        BEGSR
048200000000     C                   SELECT
048300000000     C     KEY           WHENEQ    F3
048400000000     C                   EXSR      F03STD
048500000000     C     KEY           WHENEQ    F12
048600000000     C                   EXSR      F12STD
048700000000     C     KEY           WHENEQ    F24
048800000000     C                   EXSR      F24STD
048900000000     C                   OTHER
049000000000     C                   MOVEL     MSG(2)        $MSG
049100000000     C                   EXSR      CARMSG
049200000000     C                   ENDSL
049300000000     C                   ENDSR
049400000000     C*------------------------------------------------------------------------*
049500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
049600000000     C*------------------------------------------------------------------------*
049700000000     C     F03STD        BEGSR
049800000000     C*
049900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
050000000000     C                   MOVEL     '1'           $F03
050100000000     C*
050200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
050300000000     C                   MOVEL     '1'           $FINE
050400000000     C*
050500000000     C                   ENDSR
050600000000     C*------------------------------------------------------------------------*
050700000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
050800000000     C*------------------------------------------------------------------------*
050900000000     C     F12STD        BEGSR
051000000000     C*
051100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
051200000000     C                   MOVEL     '1'           $FINE
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
051700000000     C*------------------------------------------------------------------------*
051800000000     C     F24STD        BEGSR
051900000000     C*
052000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
052100000000     C                   EXSR      OUTFUN
052200000000     C*
052300000000     C                   ENDSR
052400000000     C*------------------------------------------------------------------------*
052500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
052600000000     C*          CODA MESSAGGI DEL PGM
052700000000     C*------------------------------------------------------------------------*
052800000000     C     CARMSG        BEGSR
052900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
053000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
053100000000     C     MSGULT        IFEQ      0
053200000000     C                   MOVEL     '1'           *IN40
053300000000     C                   WRITE     SDUEDVK
053400000000     C                   MOVEL     '0'           *IN40
053500000000     C                   ENDIF
053600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
053700000000     C                   MOVEL     $MSG          DSMSMS
053800000000     C                   CALL      'XMSG'        XMSGPA
053900000000     C     DSMSER        IFEQ      0
054000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
054100000000     C                   ENDIF
054200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
054300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
054400000000     C                   WRITE     SDUEDVM                                      *SFL MESS
054500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
054600000000     C                   MOVEL     *BLANKS       $MSG
054700000000     C                   ENDSR
054800000000     C*------------------------------------------------------------------------*
054900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
055000000000     C*------------------------------------------------------------------------*
055100000000     C     OUTMSG        BEGSR
055200000000     C* VISUALIZZAZIONE MESSAGGIO
055300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
055400000000     C                   WRITE     SDUEDVK                                      *CONTR MESS
055500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
055600000000     C                   Z-ADD     *ZEROS        MSGULT
055700000000     C                   ENDSR
055800000000     C*------------------------------------------------------------------------*
055900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
056000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
056100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
056200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
056300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
056400000000     C*------------------------------------------------------------------------*
056500000000     C     CARFUN        BEGSR
056600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
056700000000     C     1             DO        31            I                 5 0          --- 1 -->
056800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
056900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
057000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
057100020709     c                   If        DutLpo = 'S'
057200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
057300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
057400000000     C                   ELSE                                                   *INTERROG.
057500000000     C                   MOVEL     FUNASI        FAT(I)
057600000000     C                   ENDIF
057700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
057800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
057900000000     C                   MOVEL     FUNAFG        FAT(I)
058000000000     C                   ELSE                                                   *INTERROG.
058100000000     C                   MOVEL     FUNAFI        FAT(I)
058200000000     C                   ENDIF
058300000000     C                   ENDIF                                                  <-- 2 ---
058400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
058500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
058600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
058700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
058800000000     C                   ELSE
058900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
059000000000     C                   ENDIF
059100000000     C                   ENDIF
059200000000     C                   ENDDO                                                  <-- 1 ---
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
059600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
059700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
059800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
059900000000     C*------------------------------------------------------------------------*
060000000000     C     OUTFUN        BEGSR
060100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
060200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
060300000000     C                   ENDDO
060400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
060500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
060600000000     C     1             DO        31            I
060700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
060800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
060900000000     C                   ADD       1             FUNNAT
061000000000     C                   ENDIF
061100000000     C                   ENDDO
061200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
061300000000     C     FUNNAT        IFGT      8
061400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
061500000000     C                   ENDIF
061600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
061700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
061800000000     C                   Z-ADD     8             FUNVIS            5 0
061900000000     C     FAT(23)       IFEQ      'S'
062000000000     C                   SUB       1             FUNVIS
062100000000     C                   ENDIF
062200000000     C     FAT(24)       IFEQ      'S'
062300000000     C                   SUB       1             FUNVIS
062400000000     C                   ENDIF
062500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
062600000000     C                   Z-ADD     *ZEROS        W                 5 0
062700000000     C                   ADD       1             FUNULT
062800000000     C     FUNULT        IFGT      31
062900000000     C                   Z-ADD     1             FUNULT
063000000000     C                   ENDIF
063100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
063200000000     C     FUNULT        DO        31            I                              --- 1 -->
063300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
063400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
063500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
063600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
063700000000     C     FDE(I)        ANDNE     *BLANKS
063800000000     C     I             IFEQ      23                                           *TOLGO F23
063900000000     C     I             OREQ      24                                           *E F24
064000000000     C                   ELSE
064100000000     C                   ADD       1             W
064200000000     C                   MOVEL     FDE(I)        FUV(W)
064300000000     C                   ENDIF
064400000000     C                   ENDIF                                                  <-- 3 ---
064500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
064600000000     C                   Z-ADD     I             FUNULT
064700000000     C                   ENDIF                                                  <-- 2 ---
064800000000     C                   ENDDO                                                  <-- 1 ---
064900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
065000000000     C                   MOVEL     FUV(1)        VFD001
065100000000     C                   MOVEL     FUV(2)        VFD002
065200000000     C                   MOVEL     FUV(3)        VFD003
065300000000     C                   MOVEL     FUV(4)        VFD004
065400000000     C                   MOVEL     FUV(5)        VFD005
065500000000     C                   MOVEL     FUV(6)        VFD006
065600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
065700000000     C     FAT(24)       ANDEQ     'N'
065800000000     C                   MOVEL     FUV(7)        VFD007
065900000000     C                   MOVEL     FUV(8)        VFD008
066000000000     C                   ELSE                                                   <-- 1 -->
066100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
066200000000     C     FAT(24)       ANDEQ     'S'
066300000000     C                   MOVEL     FDE(23)       VFD007
066400000000     C                   MOVEL     FDE(24)       VFD008
066500000000     C                   ELSE                                                   <-- 2 -->
066600000000     C                   MOVEL     FUV(7)        VFD007
066700000000     C     FAT(23)       IFEQ      'S'
066800000000     C                   MOVEL     FDE(23)       VFD008
066900000000     C                   ELSE
067000000000     C                   MOVEL     FDE(24)       VFD008
067100000000     C                   ENDIF
067200000000     C                   ENDIF                                                  <-- 2 ---
067300000000     C                   ENDIF                                                  <-- 1 ---
067400000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
067500000000     C                   ENDSR
067600000000     C*------------------------------------------------------------------------*
067700000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
067800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
067900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
068000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
068100000000     C*------------------------------------------------------------------------*
068200000000     C     CONFUN        BEGSR
068300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
068400000000     C     1             DO        31            I                              --- 1 -->
068500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
068600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
068700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
068800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
068900000000     C                   EXSR      CARMSG
069000000000     C                   MOVEL     '1'           $ERR
069100000000     C                   ENDIF                                                  <-- 3 ---
069200000000     C                   ENDIF                                                  <-- 2 ---
069300000000     C                   ENDDO                                                  <-- 1 ---
069400000000     C                   ENDSR
069500000000     O*------------------------------------------------------------------------*
069600000000**       MSG - MESSAGGI
069700000000TNSDUER- Tasto funzionale non valido.                                          1
069800000000TNSDUER- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
069900000000TNSDUER- Opzione non valida.                                                   3
070000000000TNSDUER- Opzione abilitata ma non gestita. Avvertire CED.                      4
070100000000TNSDUER- Inizio elenco.                                                        5
070200000000TNSDUER- Fine elenco.                                                          6
070300941219TNSDUER- Immettere 1 per stampare solo i 1° liv, 2 per i 2° liv, T per tutti   7
070400000000TNSDUER- Tipo unità errato                                                     8
070500000000TNSDUER- Magazzino errato                                                      9
070600951103TNSDUER- 'S' per stampare solo le unità con differenze nelle Qtà in dotazione 10
070700020717TNSDUER- Profilo non abilitato alla funzione                                  11
070800000000TNSDUER- 12
070900941219TNSDUER- 13
071000000000TNSDUER- 14
071100941219TNSDUER- 15
071200000000TNSDUER- 16
071300000000TNSDUER- 17
071400000000TNSDUER- 18
071500000000TNSDUER- Stampa lanciata ...                                                  19
071600941219TNSDUER- 20
071700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
071800000000WRKSPLF
071900000000
072000000000
072100000000
072200000000
072300000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
072400000000F1    -                   -    NN     NN
072500000000F2    -                   -    NN     NN
072600000000F3    -F3=Fine            -    SS     SS
072700000000F4    -                   -    NN     NN
072800000000F5    -                   -    NN     NN
072900000000F6    -F6=Lancio          -    SS     SS
073000000000F7    -                   -    NN     NN
073100000000F8    -                   -    NN     NN
073200000000F9    -                   -    NN     NN
073300000000F10   -                   -    NN     NN
073400000000F11   -                   -    NN     NN
073500000000F12   -F12-Ritorno        -    SS     SS
073600000000F13   -                   -    NN     NN
073700000000F14   -                   -    NN     NN
073800000000F15   -                   -    NN     NN
073900000000F16   -                   -    NN     NN
074000000000F17   -                   -    NN     NN
074100000000F18   -                   -    NN     NN
074200000000F19   -                   -    NN     NN
074300000000F20   -                   -    NN     NN
074400000000F21   -                   -    NN     NN
074500000000F22   -F22=Gestione stampe-    SS     SS
074600000000F23   -                   -    NN     NN
074700000000F24   -F24=Altri tasti    -    NN     NN
074800000000ENTER -                   -    SS     SS
074900000000ROLLUP-                   -    NN     NN
075000000000ROLLDN-                   -    NN     NN
075100000000HELP  -                   -    NN     NN
075200000000PRINT -                   -    NN     NN
075300000000BSPACE-                   -    NN     NN
075400000000CLEAR -                   -    NN     NN
