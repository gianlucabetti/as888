000100020321     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020321      *------------------------------------------------------------------------*
000300020321      * GESTIONE MOVIMENTI UNITA' EDP - SCELTA
000400020321      *------------------------------------------------------------------------*
000500020321
000600020321      *------------------------------------------------------------------------*
000700020321      * DATA BASE
000800020321      *------------------------------------------------------------------------*
000900941115     FTNSDU4D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU4DVS:SFLULT)
001100941115     F                                     SFILE(SDU4DVM:MSGULT)
001200930712     FUNMOV01L  IF   E           K DISK
001300930712     FUNMOV02L  IF   E           K DISK
001400930712     F                                     RENAME(UNMOV000:UNMOV2)
001500931116     FUNANA01L  IF   E           K DISK
001600941128     FUNANA02L  IF   E           K DISK
001700941128     F                                     RENAME(UNANA000:UNANA2)
001800020321     funana07l  if   e           k disk    rename(unana000:unana7)
001900020529     FAZORG21l  IF   E           K DISK
002000020529     fazorg01l  if   e           k disk
002100020321
002200020321      *------------------------------------------------------------------------*
002300020321      * SCHIERE
002400020321      *------------------------------------------------------------------------*
002500020321      * SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002600020717     D MSG             S             78    DIM(21) CTDATA PERRCD(1)             *MESSAGGI
002700930802     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002800930802     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002900930802     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
003000930802     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003100020321      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003200930803     D FFU             S              6    DIM(31)                              *FUNZIONE
003300930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003400930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003500020321      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003600930803     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003700930803     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003800930803     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
003900930803     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004000020321      * TABELLA DESCRIZIONI A VIDEO
004100930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004200930802     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004300020321      * TABELLA TASTI FUNZIONALI PREMUTI
004400930802     D KEYS            S              1    DIM(40)
004500020321
004600020321      *------------------------------------------------------------------------*
004700020321      * INPUT
004800020321      *------------------------------------------------------------------------*
004900020321      * RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005000930723     D DSOPZ         E DS
005100930803     D  OPZRIG                 1     40
005200020321      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005300930723     D DSFUN         E DS
005400930803     D  FUNRIG                 1     40
005500020321      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005600930127     D DSMSG         E DS
005700020321      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
005800930712     D DSUNIT        E DS
005900020321      *------------------
006000020321      * DS "XSRDA8" - CONTROLLA DATA (8)
006100020321      *------------------
006200971010     D WLBDA8          DS
006300971010     D  G08DAT                 1      8  0
006400971010     D  G08INV                 9     16  0
006500971010     D  G08ERR                17     17
006600971010     D  G08TGI                18     22  0
006700020321      * DS PER GESTIONE TASTI FUNZIONALI
006800930802     D DSKEYS        E DS
006900930803     D  KEYRIG                 1     40
007000020321      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007100930806     D                SDS
007200020321      * NOME UTENTE
007300930806     D  USER                 254    263
007400020321      * RIDEFINIZIONE INFDS DEL FILE VIDEO
007500000000     D DEVDS1          DS
007600020321      * CHIAVE FUNZIONALE BATTUTA
007700000000     D  KEY                  369    369
007800020321      * POSIZIONE CURSORE (IN HEX)
007900930721     D  CURSOR               370    371
008000020321      * RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008100930720     D  FRTSFL               378    379B 0
008200020321      * ARCHITETTURA
008300000000     D KPJBA         E DS
008400020320
008500020320     d tibs02ds      e ds
008600020320
008700020709     d ddatiute      e ds
008800020709     d azuteds       e ds                  extname(azute00f)
008900020709     d tibs34ds      e ds                  inz
009000020321      *------------------------------------------------------------------------*
009100020321      * MAIN LINE
009200020321      *------------------------------------------------------------------------*
009300020321
009400000000     C* CICLO FINO A CHE:
009500930721     C     $FINE         DOUEQ     '1'                                          --- 1 -->
009600000000     C*
009700930805     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009800000000     C*
009900930721     C* CONTROLLA GESTIONE VIDEO
010000930806     C     $GES          IFEQ      'VS'
010100930806     C                   EXSR      GESVS                                        *SUBFILE
010200930805     C                   ENDIF
010300000000     C*
010400930721     C                   ENDDO                                                  <-- 1 ---
010500930923     C*
010600930923     C     $F03          IFEQ      '1'                                          *SE PREMUTO F03
010700930923     C                   MOVEL     '1'           DSUF03                         *LO IMPOSTO NELLA DS
010800930923     C                   ENDIF                                                  *DELLA PROCEDURA
010900000000     C*
011000940105     C                   MOVEL     DSUNIT        KPJBU
011100000000     C                   SETON                                        LR
011200930713     C*================================================================
011300930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011400930713     C*================================================================
011500930806     C     CARVS         BEGSR
011600930713     C*
011700930806     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
011800930713     C*
011900930804     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
012000930804     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
012100930804     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
012200930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012300930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012400930713     C*
012500941115     C                   WRITE     SDU4DVT                                      *TESTATA
012600941115     C                   WRITE     SDU4DVO                                      *OPZIONI
012700941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
012800930713     C*
012900930806     C                   EXSR      POSSFL                                       *CARICA SFL
013000930713     C*
013100930713     C                   ENDSR
013200930713     C*================================================================
013300930806     C* GESVS  - GESTIONE SUBFILE
013400930713     C*================================================================
013500930806     C     GESVS         BEGSR
013600930714     C*
013700930805     C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013800941115     C                   WRITE     SDU4DVT                                      *TESTATA
013900941115     C                   WRITE     SDU4DVO                                      *OPZIONI
014000941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
014100930714     C                   END
014200930713     C*
014300930805     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
014400930713     C*
014500930805     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
014600020717
014700020717      * Se Profilo non OK devo uscire dal pgm
014800020717     c                   If        *In76
014900020717     c                   Eval      $err = '1'
015000020717     c                   Eval      $fine = '1'
015100020717     c                   EndIf
015200930805     C*
015300930721     C     $ERR          IFEQ      '0'
015400930713     C                   SELECT
015500930804     C     KEY           WHENEQ    F6
015600930804     C                   EXSR      F06GES
015700930804     C     KEY           WHENEQ    F21
015800930804     C                   EXSR      F21GES
015900930804     C     KEY           WHENEQ    F22
016000930804     C                   EXSR      F22GES
016100930804     C                   OTHER
016200930804     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
016300930713     C                   ENDSL
016400930713     C                   ENDIF
016500930713     C*
016600930713     C                   ENDSR
016700000000     C*================================================================
016800930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016900000000     C*================================================================
017000930126     C     F06GES        BEGSR
017100930127     C*
017200930805     C                   MOVEL     'F06'         WOPZ1             3            *OPZIONE INS.
017300941115     C                   MOVEL     'TNSDU5R'     WPGM              8            *PGM INSERIMENTO
017400930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
017500930128     C*
017600930805     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
017700930127     C*
017800000000     C                   ENDSR
017900930128     C*================================================================
018000930804     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
018100930128     C*================================================================
018200930128     C     F21GES        BEGSR
018300930128     C*
018400940105     C                   MOVEL     DSUNIT        KPJBU
018500941115     C                   CALL      'TNSDU8R'                                    *PGM STAMPA MOVIM.
018600930209     C                   PARM                    KPJBA
018700940105     C                   MOVEL     KPJBU         DSUNIT
018800940105     C*
018900940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
019000940105     C     DSUF03        IFEQ      '1'
019100940105     C                   MOVEL     '1'           $FINE
019200940105     C                   ENDIF
019300930128     C*
019400930128     C                   ENDSR
019500000000     C*================================================================
019600930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
019700000000     C*================================================================
019800000000     C     F22GES        BEGSR
019900000000     C*
020000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020100000000     C                   Z-ADD     80            LENGH            15 5
020200000000     C                   CALL      'QCAEXEC'
020300000000     C                   PARM                    CMD(1)
020400000000     C                   PARM                    LENGH
020500000000     C*
020600000000     C                   ENDSR
020700000000     C*================================================================
020800930803     C*  GESOPZ - GESTIONE OPZIONI
020900000000     C*================================================================
021000930803     C     GESOPZ        BEGSR
021100930804     C*
021200930804     C                   SELECT
021300930806     C     VSCOPZ        WHENEQ    ' 2'
021400930804     C                   EXSR      O02GES
021500930806     C     VSCOPZ        WHENEQ    ' 4'
021600930804     C                   EXSR      O04GES
021700930806     C     VSCOPZ        WHENEQ    ' 5'
021800930804     C                   EXSR      O05GES
021900930806     C     VSCOPZ        WHENEQ    ' 7'
022000930804     C                   EXSR      O07GES
022100940105     C     VSCOPZ        WHENEQ    ' 9'
022200940105     C                   EXSR      O09GES
022300930804     C                   OTHER
022400930804     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
022500930805     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
022600930804     C                   ENDSL
022700930804     C*
022800000000     C                   ENDSR
022900000000     C*================================================================
023000000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
023100000000     C*================================================================
023200000000     C     O02GES        BEGSR
023300930128     C*
023400930806     C     VSCATB        IFEQ      'A'
023500930721     C                   MOVEL     '1'           $ERR
023600930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
023700930721     C                   EXSR      CARMSG
023800930201     C                   END
023900931217     C*
024000931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
024100931217     C*
024200940104     C     VSHPRG        IFLT      20000
024300931217     C                   MOVEL     '1'           $ERR
024400931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
024500931217     C                   EXSR      CARMSG
024600931217     C                   END
024700930215     C*
024800931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
024900000000     C*
025000930201     C                   MOVEL     'O02'         WOPZ1
025100941115     C                   MOVEL     'TNSDU5R'     WPGM                           *PGM MODIFICA
025200930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
025300000000     C*
025400930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
025500930128     C*
025600931217     C                   ENDIF                                                  <-- 2 ---
025700931217     C                   ENDIF                                                  <-- 1 ---
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*================================================================
026100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
026200000000     C*================================================================
026300000000     C     O04GES        BEGSR
026400931217     C*
026500931217     C* E' POSSIBILE CANCELLARE SOLO L'ULTIMO MOVIMENTO
026600020322     C                   Z-ADD     VSCTIP        UNMTIP                         *TIPO
026700931217     C                   Z-ADD     VSCCOD        UNMCOD                         *CODICE
026800971010     C                   Z-ADD     VSCDTM        G08DAT
026900971010     C                   Z-ADD     *ZEROS        G08INV
027000971013     C                   MOVEL     '0'           G08ERR
027100971010     C                   CALL      'XSRDA8'
027200971010     C                   PARM                    WLBDA8
027300971010     C                   Z-ADD     G08INV        UNMDTM
027400931217     C                   Z-ADD     VSHPRG        UNMPRG                         *PROGR.MOV.
027500931217     C                   MOVEL     'A'           UNMTMO                         *TIPO MOV.
027600940504     C     KEYUM1        SETGT     UNMOV01L                           99        *FINE-ARCHIV
027700931217     C  N99              READ      UNMOV01L                               99
027800931217     C     *IN99         IFEQ      '0'
027900931217     C     UNMCOD        ANDEQ     VSCCOD
028000931217     C     UNMPRG        ANDLT     9000000
028100931217     C                   MOVEL     '1'           $ERR
028200931217     C                   MOVEL     MSG(15)       $MSG                           *ANN.SOLO ULT.MOV.
028300931217     C                   EXSR      CARMSG
028400931217     C                   END
028500930128     C*
028600930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
028700930806     C     VSCATB        IFEQ      'A'
028800930721     C                   MOVEL     '1'           $ERR
028900930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
029000930721     C                   EXSR      CARMSG
029100930201     C                   END
029200931217     C*
029300931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
029400931217     C*
029500931217     C     VSHPRG        IFGE      9000000
029600931217     C                   MOVEL     '1'           $ERR
029700931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
029800931217     C                   EXSR      CARMSG
029900931217     C                   END
030000930215     C*
030100931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
030200000000     C*
030300971013     C* LANCIO IL PGM DI GESTIONE
030400930201     C                   MOVEL     'O04'         WOPZ1
030500941115     C                   MOVEL     'TNSDU5R'     WPGM
030600930201     C                   EXSR      LANPGM
030700000000     C*
030800930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
030900930128     C*
031000931217     C                   END                                                    <-- 2 ---
031100931217     C                   END                                                    <-- 1 ---
031200000000     C*
031300000000     C                   ENDSR
031400000000     C*================================================================
031500000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031600000000     C*================================================================
031700000000     C     O05GES        BEGSR
031800000000     C*
031900000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
032000930201     C                   MOVEL     'O05'         WOPZ1
032100941115     C                   MOVEL     'TNSDU5R'     WPGM
032200930201     C                   EXSR      LANPGM
032300000000     C*
032400000000     C                   ENDSR
032500930128     C*================================================================
032600930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
032700930128     C*================================================================
032800930128     C     O07GES        BEGSR
032900930128     C*
033000930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033100930806     C     VSCATB        IFNE      'A'
033200930721     C                   MOVEL     '1'           $ERR
033300930721     C                   MOVEL     MSG(13)       $MSG
033400930721     C                   EXSR      CARMSG
033500930805     C                   ENDIF
033600930215     C*
033700930721     C     $ERR          IFEQ      '0'                                          --- 1 -->
033800930128     C*
033900930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
034000930201     C                   MOVEL     'O07'         WOPZ1
034100941115     C                   MOVEL     'TNSDU5R'     WPGM
034200930201     C                   EXSR      LANPGM
034300930128     C*
034400930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
034500930128     C                   Z-ADD     1             SWMODI
034600930128     C*
034700930805     C                   ENDIF                                                  <-- 1 ---
034800930128     C*
034900930128     C                   ENDSR
035000940105     C*================================================================
035100940105     C*  O09GES - GESTIONE OPZIONE 9=ANAGRAFICA
035200940105     C*================================================================
035300940105     C     O09GES        BEGSR
035400940105     C*
035500940105     C* LANCIO IL PGM PER VISUALIZZARE DETTAGLIO ANAGRAFICA
035600940105     C                   MOVEL     'O05'         WOPZ1                          *INTERROGAZIONE
035700941115     C                   MOVEL     'TNSDU2R'     WPGM                           *PGM DETT.ANAGRAFICA
035800940105     C                   EXSR      LANPGM
035900940105     C*
036000940105     C                   ENDSR
036100930205     C*================================================================
036200930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
036300930205     C*================================================================
036400930205     C     NEWPOS        BEGSR
036500930205     C*
036600930205     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
036700930205     C*         1= RICHIESTO POSIZIONAM.
036800930205     C                   Z-ADD     0             SWPOSI            7 0
036900930205     C*
037000930205     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037100930205     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037200930205     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037300930205     C                   Z-ADD     0             SWMODI            1 0
037400930205     C*
037500930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
037600930806     C     VCCTIP        IFNE      DEPTIP                                       --- 1 -->
037700930806     C     VCCCOD        ORNE      DEPCOD
037800941128     C     VCCMAT        ORNE      DEPMAT
037900930806     C     VCCMAG        ORNE      DEPMAG
038000930806     C     VCCDTM        ORNE      DEPDTM
038100930205     C*
038200930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
038300930205     C                   EXSR      CHKPAR
038400930205     C*
038500930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
038600930721     C     $ERR          IFEQ      '0'                                          --- 2 -->
038700930205     C                   Z-ADD     1             SWPOSI
038800930806     C                   MOVE      VCCTIP        DEPTIP
038900930806     C                   Z-ADD     VCCCOD        DEPCOD
039000941128     C                   MOVEL     VCCMAT        DEPMAT
039100930806     C                   MOVE      VCCMAG        DEPMAG
039200930806     C                   Z-ADD     VCCDTM        DEPDTM
039300930805     C                   ENDIF                                                  <-- 2 ---
039400930205     C*
039500930805     C                   ENDIF                                                  <-- 1 ---
039600930205     C*
039700930205     C                   ENDSR
039800000000     C*================================================================
039900000000     C* LETTURA RECORD FILE DA CARICARE
040000000000     C*================================================================
040100000000     C     LETFIL        BEGSR
040200000000     C*
040300930203     C* LETTURE SUCCESSIVE ALLA PRIMA
040400930712     C     WTIPRI        IFEQ      'COD'
040500930806     C                   EXSR      READ01
040600930805     C                   ENDIF
040700930212     C*
040800930712     C     WTIPRI        IFEQ      'MAG'
040900930806     C                   EXSR      READ02
041000930805     C                   ENDIF
041100000000     C*
041200000000     C                   ENDSR
041300000000     C*================================================================
041400000000     C* SETFIL - SETLL DEL FILE DA CARICARE
041500000000     C*================================================================
041600000000     C     SETFIL        BEGSR
041700000000     C*
041800930202     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
041900930712     C     WTIPRI        IFEQ      'COD'
042000020325     C                   move      vCCTIP        UNMTIP
042100930806     C                   Z-ADD     VCCCOD        UNMCOD
042200930806     C                   Z-ADD     WCCDTM        UNMDTM
042300930712     C                   Z-ADD     *ZEROS        UNMPRG
042400931217     C                   MOVEL     *BLANKS       UNMTMO
042500020321     c                   if        unmdtm <> *zeros
042600930712     C     KEYUM1        SETLL     UNMOV01L                           99        *FINE-ARCHIV
042700020321     c                   else
042800020322     c     keyum11       setll     unmov01l                           99
042900020321     c                   endif
043000930806     C  N99              EXSR      READ01
043100930212     C                   END
043200930212     C*
043300930712     C     WTIPRI        IFEQ      'MAG'
043400930806     C                   Z-ADD     WCCMAG        UNMMAG
043500930806     C                   Z-ADD     WCCDTM        UNMDTM
043600020403     c                   if        unmdtm <> *zeros
043700930712     C     KEYUM2        SETLL     UNMOV02L                           99        *FINE-ARCHIV
043800020403     c                   else
043900020403     c     keyum21       setll     unmov02l                           99
044000020403     c                   endif
044100930806     C  N99              EXSR      READ02
044200930212     C                   END
044300000000     C*
044400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
044500930203     C     *IN99         IFEQ      '1'
044600000000     C                   Z-ADD     1             SFLEOF
044700930203     C                   END
044800000000     C*
044900000000     C                   ENDSR
045000930806     C*================================================================
045100930806     C* READ01 - LEGGE RECORD SUCCESSIVO SU UNMOV01L
045200930806     C*================================================================
045300930806     C     READ01        BEGSR
045400930806     C*
045500930806     C                   MOVEL     '0'           RECOK             1
045600930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
045700930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
045800930806     C                   Z-ADD     80            LENGH            15 5
045900930806     C                   CALL      'QCAEXEC'
046000930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
046100930806     C                   PARM                    LENGH
046200930806     C                   ENDIF                                                  <-- 2 ---
046300930806     C                   MOVEL     '0'           RECOK             1
046400020322     C     keyum11       READe     UNMOV01L                             9899
046500930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
046600930806     C                   Z-ADD     1             SFLEOF
046700930806     C                   ELSE                                                   <-- 2 -->
046800020321     C**!!!UNMCOD        ifNE      VCCCOD
046900020321     C**!!!              Z-ADD     1             SFLEOF
047000020321     C**!!!              ENDIF                                                  <-- 3 ---
047100930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
047200930806     C                   MOVEL     '1'           RECOK
047300930806     C                   ENDIF
047400971014     C                   ENDIF                                                  <-- 2 ---
047500930806     C                   ENDDO                                                  <-- 1 ---
047600930806     C*
047700930806     C                   ENDSR
047800930806     C*================================================================
047900930806     C* READ02 - LEGGE RECORD SUCCESSIVO SU UNMOV02L
048000930806     C*================================================================
048100930806     C     READ02        BEGSR
048200930806     C*
048300930806     C                   MOVEL     '0'           RECOK
048400930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
048500930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
048600930806     C                   Z-ADD     80            LENGH            15 5
048700930806     C                   CALL      'QCAEXEC'
048800930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
048900930806     C                   PARM                    LENGH
049000930806     C                   ENDIF                                                  <-- 2 ---
049100930806     C                   MOVEL     '0'           RECOK
049200020403     C     keyum21       READe     UNMOV02L                             9899
049300930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
049400930806     C                   Z-ADD     1             SFLEOF
049500930806     C                   ELSE                                                   <-- 2 -->
049600930806     C     UNMMAG        IFNE      WCCMAG                                       --- 3 -->
049700930806     C                   Z-ADD     1             SFLEOF
049800930806     C                   ELSE                                                   <-- 3 -->
049900930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
050000930806     C                   MOVEL     '1'           RECOK
050100930806     C                   ENDIF
050200930806     C                   ENDIF                                                  <-- 3 ---
050300930806     C                   ENDIF                                                  <-- 2 ---
050400930806     C                   ENDDO                                                  <-- 1 ---
050500930806     C*
050600930806     C                   ENDSR
050700000000     C*================================================================
050800000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
050900000000     C*================================================================
051000000000     C     IRESFL        BEGSR
051100000000     C*
051200930806     C                   MOVEL     UNMATB        VSCATB
051300930806     C                   MOVE      UNMTIP        VSCTIP
051400930806     C                   Z-ADD     UNMCOD        VSCCOD
051500930712     C                   MOVE      UNMTIP        WTIP              2            !
051600930712     C                   EXSR      CHKTIP                                       !DECOD.TIPO UNITA
051700930806     C                   MOVEL     WDTIP         VSDTIP                         !
051800930806     C                   Z-ADD     UNMMAG        VSCMAG
051900930712     C                   MOVE      UNMMAG        WMAG                           !
052000930712     C                   EXSR      CHKMAG                                       !DECOD.MAGAZZINO
052100930806     C                   MOVEL     WDMAG         VSDMAG                         !
052200971010     C                   Z-ADD     UNMDTM        G08INV
052300971010     C                   Z-ADD     *ZEROS        G08DAT
052400971013     C                   MOVEL     '3'           G08ERR
052500971010     C                   CALL      'XSRDA8'
052600971010     C                   PARM                    WLBDA8
052700971010     C                   Z-ADD     G08DAT        VSCDTM
052800930806     C                   MOVE      UNMCAU        VSCCAU
052900930712     C                   MOVE      UNMCAU        WCAU              4            !
053000930712     C                   EXSR      CHKCAU                                       !DECOD.CAUSALE MOV.
053100930806     C                   MOVEL     WDCAU         VSDCAU                         !
053200971013     C                   MOVEL     UNMTMO        VSCTMO
053300930806     C                   Z-ADD     UNMPRG        VSHPRG
053400000000     C*
053500000000     C                   ENDSR
053600020321      *================================================================
053700020321      *  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
053800020321      *================================================================
053900930205     C     CHKPAR        BEGSR
054000020321
054100020321      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
054200930212     C                   SETOFF                                       9091
054300020321
054400020321      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
054500930712     C                   SETOFF                                       808182
054600941128     C                   SETOFF                                       8384
054700020321
054800020321      * IMPOSTO A BLANKS DESCRIZIONI PARAMETRI DI INIZIO LISTA
054900930806     C                   MOVEL     *BLANKS       VCDMAG
055000020321
055100020321      * AZZERA CAMPI DI LAVORO NUMERICI
055200940104     C                   Z-ADD     *ZEROS        WCCMAG            3 0
055300971010     C                   Z-ADD     *ZEROS        WCCDTM            8 0
055400020321
055500020321      * SE UGUALE A ZEROS IMPOSTO BLANKS
055600930806     C     VCCTIP        IFEQ      *ZEROS
055700930806     C                   MOVEL     *BLANKS       VCCTIP
055800930805     C                   ENDIF
055900930806     C     VCCMAG        IFEQ      *ZEROS
056000930806     C                   MOVEL     *BLANKS       VCCMAG
056100930805     C                   ENDIF
056200020321
056300020321      * SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE 'COD'
056400930806     C     VCCTIP        IFEQ      *BLANKS                                      --- 1 -->
056500930806     C     VCCCOD        ANDEQ     *ZEROS
056600941128     C     VCCMAT        ANDEQ     *BLANKS
056700941128     C     VCCMAG        ANDEQ     *BLANKS
056800930806     C     VCCDTM        ANDEQ     *ZEROS
056900930805     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
057000930212     C                   SETON                                        90
057100020321
057200930212     C                   ELSE                                                   <-- 1 -->
057300020321
057400020321      * ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
057500930806     C     VCCTIP        IFNE      *BLANKS
057600930806     C     VCCCOD        ORNE      *ZEROS
057700941128     C     VCCMAT        ORNE      *BLANKS
057800930806     C     VCCMAG        IFNE      *BLANKS
057900930721     C                   MOVEL     MSG(7)        $MSG
058000930721     C                   EXSR      CARMSG
058100930721     C                   MOVEL     '1'           $ERR
058200930712     C                   SETON                                        808182
058300930805     C                   ENDIF
058400930805     C                   ENDIF
058500020321
058600930721     C     $ERR          IFEQ      '0'                                          --- 2 -->
058700020321
058800020321      * CONTROLLO CHE TIPO DI RICERCA E' RICHIESTA
058900020321     C     VCCTIP        IFNE      *BLANKS                                      --- 3 -->
059000930712     C                   MOVEL     'COD'         WTIPRI
059100930215     C                   SETON                                        90
059200020321      * SE E' PER COD, CONTROLLO CHE IL TIPO UNITA ESISTA
059300930806     C                   MOVE      VCCTIP        WTIP
059400930805     C                   EXSR      CHKTIP                                       *CONTROLLA TIPO UNIT
059500020321     C     $ERR          IFEQ      '1'                                          --- 4 -->
059600930721     C                   MOVEL     '1'           $ERR
059700930721     C                   MOVEL     MSG(9)        $MSG
059800930721     C                   EXSR      CARMSG
059900930215     C                   SETON                                        80
060000020321     C                   ELSE                                                   <-- 4 -->
060100930806     C                   MOVE      WTIP          VCCTIP
060200020321     C                   ENDIF                                                  <-- 4 ---
060300020321     C                   ENDIF                                                  <-- 3 ---
060400020321
060500020321      * SE PER MATRICOLA CONTROLLO VADO A REPERIRE IL CODICE UNITA' E CONTROLLO
060600020321      * CHE LA MATRICOLA SIA UNIVOCA (TIP+MAT) SE NO ERRORE
060700020321IF  1C     VCCMAT        IFNE      *BLANKS                                      --- 3 -->
060800020321     c     vcctip        andne     *blanks
060900941128     C                   MOVEL     VCCTIP        UNATIP
061000941128     C                   MOVEL     VCCMAT        UNAMAT
061100941128     C     KEYUN2        SETLL     UNANA02L
061200941128     C     KEYUN2        READE     UNANA02L                               97
061300941128     C                   Z-ADD     *ZEROS        QTATEM            4 0          *QUANTITA' TIP+MAT <
061400020321DO  2C     *IN97         DOWEQ     *OFF                                         --- 4 -->
061500941128     C                   ADD       1             QTATEM
061600941128     C     KEYUN2        READE     UNANA02L                               97
061700020321E   2C                   ENDDO                                                  <-- 4 ---
061800020321
061900020321      * TIPO + MATRICOLA INSESITENTE
062000020321IF  2C     QTATEM        IFEQ      *ZEROS                                       --- 4 -->
062100941128     C                   MOVEL     '1'           $ERR
062200941128     C                   MOVEL     MSG(17)       $MSG
062300941128     C                   EXSR      CARMSG
062400941128     C                   SETON                                        8480
062500020321X   2C                   ELSE                                                   <-- 4 -->
062600020321
062700020321      * TIPO + MATRICOLA NON UNIVOCA
062800020321IF  3C     QTATEM        IFGT      1                                            --- 5 -->
062900941128     C                   MOVEL     '1'           $ERR
063000941128     C                   MOVEL     MSG(18)       $MSG
063100941128     C                   EXSR      CARMSG
063200941128     C                   SETON                                        8480
063300020321X   3C                   ELSE                                                   <-- 5 -->
063400941128     C                   Z-ADD     UNACOD        VCCCOD
063500941128     C                   MOVEL     *BLANKS       VCCMAT
063600020321E   3C                   ENDIF                                                  <-- 5 ---
063700020321E   2C                   ENDIF                                                  <-- 4 ---
063800020321E   1C                   ENDIF                                                  <-- 3 ---
063900020320
064000020321      * per sola matricola cerco il codice unità sull'anagrafico unità
064100020321      * e verifico che sia univoca altrimenti errore
064200020321     c                   if        vcctip = *blanks and vccmat <> *blanks       --- 3 -->
064300020321     c                   clear                   qtatem
064400020321     c                   eval      wtipri = 'COD'
064500020321     c                   eval      *in90 = *on
064600020321     c     vccmat        setll     unana07l
064700020321      * se non esiste matricola errore
064800020321     c                   if        not %equal(unana07l)                         --- 4 -->
064900020321     c                   eval      $err = *on
065000020321     c                   eval      $msg = msg(19)
065100020321     c                   exsr      carmsg
065200020321     c                   eval      *in84 = *on
065300020321      * controllo se univoca
065400020321     c                   else                                                   <-- 4 -->
065500020321     c                   do        *hival                                       --- 5 -->
065600020321     c     vccmat        reade     unana07l
065700020321      * fine file
065800020321     c                   if        %eof(unana07l)                               --- 6 -->
065900020321     c                   leave
066000020321     c                   endif                                                  <-- 6 ---
066100020321     c                   if        qtatem = *zeros                              --- 6 -->
066200020321     c                   add       1             qtatem
066300020321     c                   eval      vcccod = unacod
066400020325     c                   movel     unatip        vcctip
066500020321     c                   clear                   vccmat
066600020321     c                   else                                                   <-- 6 -->
066700020321     c                   eval      $err = *on
066800020321     c                   eval      $msg = msg(20)
066900020321     c                   exsr      carmsg
067000020321     c                   eval      *in84 = *on
067100020321     c                   leave
067200020321     c                   endif                                                  <-- 6 ---
067300020321     c                   enddo                                                  <-- 5 ---
067400020321     c                   endif                                                  <-- 4 ---
067500020321     c                   endif                                                  <-- 3 ---
067600020321
067700930806     C     VCCMAG        IFNE      *BLANKS
067800930712     C                   MOVEL     'MAG'         WTIPRI
067900930212     C                   SETON                                        91
068000020321      * SE E' PER MAG, CONTROLLO CHE ESISTA
068100930806     C                   MOVE      VCCMAG        WMAG
068200930805     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
068300930721     C     $ERR          IFEQ      '1'
068400930721     C                   MOVEL     '1'           $ERR
068500930721     C                   MOVEL     MSG(8)        $MSG
068600930721     C                   EXSR      CARMSG
068700930712     C                   SETON                                        82
068800930712     C                   ELSE
068900930806     C                   MOVE      WMAG          VCCMAG
069000940104     C                   MOVE      WMAG          WCCMAG
069100930806     C                   MOVEL     WDMAG         VCDMAG
069200930805     C                   ENDIF
069300930805     C                   ENDIF
069400020321
069500930805     C                   ENDIF                                                  <-- 2 ---
069600930805     C                   ENDIF                                                  <-- 1 ---
069700020321
069800020321      * CONTROLLO LA DATA DI INIZIO
069900930806     C     VCCDTM        IFNE      *ZEROS
070000971010     C                   Z-ADD     VCCDTM        WDAT              8 0
070100930805     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
070200930805     C     $ERR          IFEQ      '1'                                          *DATA ERRATA
070300930712     C                   SETON                                        83
070400930805     C                   ELSE                                                   *DATA OK
070500971010     C                   Z-ADD     VCCDTM        G08DAT
070600971010     C                   Z-ADD     *ZEROS        G08INV
070700971010     C                   MOVEL     '0'           G08ERR
070800971010     C                   CALL      'XSRDA8'
070900971010     C                   PARM                    WLBDA8
071000971013     C                   Z-ADD     G08DAT        VCCDTM
071100971013     C                   Z-ADD     G08INV        WCCDTM
071200930805     C                   ENDIF
071300930805     C                   ENDIF
071400020321
071500020321      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
071600020321      * CURSORE SUL CAMPO DEL CONTROL
071700930721     C     $ERR          IFEQ      '1'
071800930805     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
071900930715     C                   ELSE
072000930805     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
072100930715     C                   END
072200020321
072300930205     C                   ENDSR
072400020321      *================================================================
072500020321      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
072600020321      *================================================================
072700930712     C     CHKTIP        BEGSR
072800020321
072900020321      * CONTROLLO SE E' STATO INSERITO '?'
073000020321     C     '?'           SCAN      WTIP                                   99
073100020321
073200020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
073300020321     c                   if        *in99                                        --- 1 -->
073400020321     c                   clear                   TIBS02DS
073500020321     c                   movel     'R'           t02mod
073600020321     c                   movel     knsif         t02sif
073700020321     c                   movel     'UNT'         t02cod
073800020321     c                   call      'TIBS02R'
073900020321     c                   parm                    KPJBA
074000020321     c                   parm                    TIBS02DS
074100020321     c                   movel     T02ke1        wtip
074200020321     c                   movel     t02uni        wdtip            30
074300020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
074400020321     c                   endif                                                  <-- 1 ---
074500020321
074600020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
074700020321     c                   clear                   TIBS02DS
074800020321     c                   movel     'C'           T02mod
074900020321     c                   movel     knsif         t02sif
075000020321     c                   movel     'UNT'         t02cod
075100020322     c                   movel(p)  wtip          T02ke1
075200020321     c                   call      'TIBS02R'
075300020321     c                   parm                    KPJBA
075400020321     c                   parm                    TIBS02DS
075500020321     c                   if        t02err <> *blanks
075600020321     c                   eval      $err = '1'
075700020321     c                   clear                   wdtip
075800020321     c                   else
075900020321     c                   movel     t02uni        wdtip
076000020321     c                   endif
076100020321
076200930712     C                   ENDSR
076300020321      *================================================================
076400020321      *  CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
076500020321      *================================================================
076600930712     C     CHKCAU        BEGSR
076700020321
076800020321      * CONTROLLO SE E' STATO INSERITO '?'
076900020321     C     '?'           SCAN      WCAU                                   99
077000020321
077100020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
077200020321     c                   if        *in99                                        --- 1 -->
077300020321     c                   clear                   TIBS02DS
077400020321     c                   movel     'R'           t02mod
077500020321     c                   movel     knsif         t02sif
077600020321     c                   movel     'UNM'         t02cod
077700020321     c                   call      'TIBS02R'
077800020321     c                   parm                    KPJBA
077900020321     c                   parm                    TIBS02DS
078000020321     c                   movel     T02ke1        wcau
078100020321     c                   movel     t02uni        wdcau            30
078200020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
078300020321     c                   endif                                                  <-- 1 ---
078400020321
078500020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
078600020321     c                   clear                   TIBS02DS
078700020321     c                   movel     'C'           T02mod
078800020321     c                   movel     knsif         t02sif
078900020321     c                   movel     'UNM'         t02cod
079000020321     c                   movel(p)  wcau          T02ke1
079100020321     c                   call      'TIBS02R'
079200020321     c                   parm                    KPJBA
079300020321     c                   parm                    TIBS02DS
079400020321     c                   if        t02err <> *blanks
079500020321     c                   eval      $err = '1'
079600020321     c                   clear                   wdcau
079700020321     c                   else
079800020321     c                   movel     t02uni        wdcau
079900020321     c                   endif
080000020321
080100930712     C                   ENDSR
080200930712     C*================================================================
080300930712     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
080400930712     C*================================================================
080500930712     C     CHKMAG        BEGSR
080600930712     C*
080700930712     C* CONTROLLO SE E' STATO INSERITO '?'
080800930712     C     '?'           SCAN      WMAG          A                 1 0    99
080900930712     C*
081000930712     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
081100930712     C     *IN99         IFEQ      '1'                                          --- 1 -->
081200930712     C                   MOVEL     *BLANKS       §COD1             3
081300930712     C                   MOVE      *BLANKS       §DES1
081400941115     C                   CALL      'TNSD20R'
081500930712     C                   PARM                    §COD1
081600930712     C                   PARM                    §TIP              1
081700930712     C                   PARM                    §DES1            25
081800930712     C                   MOVEL     §COD1         WMAG              3
081900930712     C                   MOVEL     §DES1         WDMAG            30
082000930714     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
082100930712     C*
082200930805     C                   ENDIF                                                  <-- 1 ---
082300930712     C*
082400930712     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
082500930712     C                   MOVE      WMAG          ORGFIL
082600020529     C     KEYORG        CHAIN     AZORG21l                           99
082700930712     C     *IN99         IFEQ      '0'                                          --- 2 -->
082800930712     C                   MOVEL     ORGFIL        WMAG
082900930712     C                   MOVEL     ORGDES        WDMAG
083000930712     C                   ELSE                                                   <-- 2 -->
083100020529     c     keyorg        chain     azorg01l
083200020529     c                   if        %found(azorg01l)
083300020529     c                   movel     orgfil        wmag
083400020529     c                   movel     orgdes        wdmag
083500020529     c                   else
083600930721     C                   MOVEL     '1'           $ERR
083700930712     C                   MOVEL     *ALL'*'       WDMAG
083800020529     c                   endif
083900930805     C                   ENDIF                                                  <-- 2 ---
084000930712     C*
084100930712     C                   ENDSR
084200930712     C*================================================================
084300930712     C* CHKDAT - CONTROLLA LA DATA
084400930712     C*================================================================
084500930712     C     CHKDAT        BEGSR
084600930712     C*
084700971010     C                   Z-ADD     WDAT          G08DAT
084800971010     C                   Z-ADD     *ZEROS        G08INV
084900971010     C                   MOVEL     '0'           G08ERR
085000971010     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
085100971010     C                   PARM                    WLBDA8
085200971013     C                   Z-ADD     G08DAT        WDAT
085300971010     C     G08ERR        IFNE      '0'
085400930721     C                   MOVEL     '1'           $ERR
085500930721     C                   MOVEL     MSG(10)       $MSG
085600930721     C                   EXSR      CARMSG
085700930805     C                   ENDIF
085800930712     C*
085900930712     C                   ENDSR
086000930201     C*================================================================
086100930201     C* LANPGM - LANCIA UN PROGRAMMA
086200930201     C*================================================================
086300930201     C     LANPGM        BEGSR
086400930201     C*
086500930201     C* IMPOSTO PARAMETRI PER IL PGM
086600930712     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
086700930712     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
086800930712     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
086900930712     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
087000930806     C                   Z-ADD     VSCTIP        DSUTIP                         *TIPO
087100930806     C                   Z-ADD     VSCCOD        DSUCOD                         *CODICE
087200930806     C                   Z-ADD     VSCMAG        DSUMAG                         *MAGAZZINO
087300930806     C                   Z-ADD     VSHPRG        DSUPRG                         *PROGR.MOV.
087400931217     C                   MOVEL     VSCTMO        DSUTMO                         *TIPO MOVIM.
087500971010     C                   Z-ADD     VSCDTM        G08DAT
087600971010     C                   Z-ADD     *ZEROS        G08INV
087700971010     C                   MOVEL     '0'           G08ERR
087800971010     C                   CALL      'XSRDA8'
087900971010     C                   PARM                    WLBDA8
088000971010     C                   Z-ADD     G08INV        DSUDTM                         *DATA MOVIMENTO
088100930201     C*
088200930201     C* LANCIO PROGRAMMA
088300940105     C                   MOVEL     DSUNIT        KPJBU
088400930312     C                   CALL      WPGM
088500940105     C                   PARM                    KPJBA
088600940105     C                   MOVEL     KPJBU         DSUNIT
088700930201     C*
088800930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
088900930712     C     DSUF03        IFEQ      '1'
089000930721     C                   MOVEL     '1'           $F03
089100930721     C                   MOVEL     '1'           $ERR
089200940105     C                   MOVEL     '1'           $FINE
089300930201     C                   ELSE
089400930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
089500930721     C                   MOVEL     DSUMSG        $MSG
089600930721     C                   EXSR      CARMSG
089700930805     C                   ENDIF
089800930201     C*
089900930201     C                   ENDSR
090000000000     C*================================================================
090100930713     C*  *INZSR - ROUTINE INIZIALE
090200000000     C*================================================================
090300930713     C     *INZSR        BEGSR
090400000000     C*
090500000000     C* RICEVIMENTO PARAMETRI
090600000000     C     *ENTRY        PLIST
090700940105     C                   PARM                    KPJBA
090800940105     C                   MOVEL     KPJBU         DSUNIT
090900020321
091000020321      * CHIAVE LETTURA UNANA01L
091100931116     C     KEYUN1        KLIST
091200020322     C                   KFLD                    UNATIP
091300931116     C                   KFLD                    UNACOD
091400020321
091500020321      * CHIAVE LETTURA UNANA02L
091600941128     C     KEYUN2        KLIST
091700941128     C                   KFLD                    UNATIP
091800941128     C                   KFLD                    UNAMAT
091900020321
092000020321      * CHIAVE LETTURA UNMOV01L
092100930712     C     KEYUM1        KLIST
092200020322     C                   KFLD                    UNMTIP
092300930712     C                   KFLD                    UNMCOD
092400930712     C                   KFLD                    UNMDTM
092500930712     C                   KFLD                    UNMPRG
092600931217     C                   KFLD                    UNMTMO
092700020322
092800020322      * CHIAVE LETTURA UNMOV01L
092900020322     C     KEYUM11       KLIST
093000020322     C                   KFLD                    UNMTIP
093100020322     C                   KFLD                    UNMCOD
093200020321
093300020321      * CHIAVE LETTURA UNMOV02L
093400930712     C     KEYUM2        KLIST
093500930712     C                   KFLD                    UNMMAG
093600930712     C                   KFLD                    UNMDTM
093700020403      * CHIAVE LETTURA UNMOV02L
093800020403     C     KEYUM21       KLIST
093900020403     C                   KFLD                    UNMMAG
094000930310     C*
094100020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
094200930310     C     KEYORG        KLIST
094300930712     C                   KFLD                    ORGFIL
094400000000     C*
094500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
094600971014     C     *LIKE         DEFINE    VCCTIP        DEPTIP
094700930806     C     *LIKE         DEFINE    VCCCOD        DEPCOD
094800941128     C     *LIKE         DEFINE    VCCMAT        DEPMAT
094900930806     C     *LIKE         DEFINE    VCCMAG        DEPMAG
095000930806     C     *LIKE         DEFINE    VCCDTM        DEPDTM
095100971014     C*
095200971014     C* PARAMETRI DI ENTRATA IN DEPOSITI
095300971014     C     *LIKE         DEFINE    DSUPRO        DEPPRO
095400971014     C     *LIKE         DEFINE    DSUOP0        DEPOP0
095500971014     C                   MOVEL     DSUPRO        DEPPRO
095600971014     C                   MOVEL     DSUOP0        DEPOP0
095700940105     C*
095800940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DELL'ANAGRAF. UNITA'
095900940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
096000940105     C     DSUPRO        IFEQ      'A'
096100940105     C                   MOVEL     OPZ(9)        OPZRIG                         *GEST.MOVIMENTI
096200940105     C                   MOVEL     'N'           OPZASG
096300940105     C                   MOVEL     'N'           OPZASI
096400940105     C                   MOVEL     'N'           OPZAFG
096500940105     C                   MOVEL     'N'           OPZAFI
096600940105     C                   MOVEL     OPZRIG        OPZ(9)
096700940105     C                   END
096800930714     C*
096900930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
097000930803     C                   EXSR      INZSTD
097100930803     C*
097200930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
097300930803     C                   EXSR      INZSFL
097400930714     C*
097500930806     C* IMPOSTO CAMPI TESTATA VIDEO
097600940105     C     DSUOP0        IFEQ      ' 2'
097700930806     C                   MOVEL     '    GEST'    VTDING
097800930806     C                   MOVE      'IONE    '    VTDING
097900930923     C                   MOVEL     'G'           $TGES
098000930714     C                   ELSE
098100930806     C                   MOVEL     ' INTERRO'    VTDING
098200930806     C                   MOVE      'GAZIONE '    VTDING
098300930923     C                   MOVEL     'I'           $TGES
098400930805     C                   ENDIF
098500931116     C*
098600930713     C*
098700930720     C* SE NON SONO STATI PASSATI PARAMETRI IN INPUT EMETTO VIDEATA PARAMETRI
098800930805     C     DSUTIP        IFEQ      *ZEROS                                       --- 1 -->
098900930806     C                   MOVEL     *BLANKS       VCCTIP
099000930806     C                   Z-ADD     *ZEROS        VCCCOD
099100930806     C                   MOVEL     *BLANKS       VCCMAG
099200930806     C                   Z-ADD     *ZEROS        VCCDTM
099300931116     C*
099400931116     C* SE PASSATI CONTROLLO I PARAMETRI IN INPUT AL PGM
099500930805     C                   ELSE                                                   <-- 1 -->
099600020322     c                   z-add     dsutip        unatip
099700931116     C                   Z-ADD     DSUCOD        UNACOD
099800931116     C     KEYUN1        CHAIN     UNANA01L                           99
099900931116     C*
100000931116     C* SE ANAGRAFICA ANNULLATA, PERMETTO SOLO LA VISUALIZZAZIONE
100100931116     C     *IN99         IFEQ      *OFF
100200931116     C     UNAATB        ANDEQ     'A'
100300931116     C                   MOVEL     ' INTERRO'    VTDING
100400931116     C                   MOVE      'GAZIONE '    VTDING
100500931116     C                   MOVEL     'I'           $TGES
100600931116     C                   ENDIF
100700971014     C                   ENDIF                                                  <-- 1 ---
100800931116     C*
100900971014     C                   MOVE      DSUTIP        VCCTIP                         *VIDEO
101000930806     C                   Z-ADD     DSUCOD        VCCCOD
101100930806     C                   MOVEL     *BLANKS       VCCMAG
101200930806     C                   Z-ADD     *ZEROS        VCCDTM
101300971014     C                   MOVE      VCCTIP        DEPTIP                         *DEPOSITI
101400971014     C                   Z-ADD     VCCCOD        DEPCOD
101500971014     C                   MOVE      VCCMAG        DEPMAG
101600971014     C                   Z-ADD     VCCDTM        DEPDTM
101700930805     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
101800930721     C     $ERR          IFEQ      '1'
101900930712     C                   MOVEL     '1'           DSUERR
102000930721     C                   MOVEL     $MSG          DSUMSG
102100931015     C                   MOVEL     '1'           $FINE
102200930805     C                   ENDIF
102300940104     C*
102400940104     C* EMETTO VIDEATA SUBFILE
102500971014     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
102600930806     C                   EXSR      CARVS                                        *CARICA VIDEO SUBFIL
102700000000     C*
102800000000     C                   ENDSR
102900971014     C*================================================================
103000971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103100971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103200930805     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
103300930805     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
103400930805     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
103500971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103600971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103700930714     C*================================================================
103800930803     C* INZSTD - ROUTINE INIZIALE STANDARD
103900930714     C*================================================================
104000930803     C     INZSTD        BEGSR
104100930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
104200930802     C                   CALL      'XKEYS'
104300930802     C                   PARM                    DSKEYS
104400930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
104500930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
104600930714     C     XMSGPA        PLIST
104700930714     C                   PARM                    DSMSMS
104800930714     C                   PARM                    DSMSPG
104900930714     C                   PARM                    DSMSKE
105000930714     C                   PARM                    DSMSER
105100930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
105200941115     C                   MOVEL     'TNSDU4R'     DSMSPG                         *NOME PGM CHIAMANTE
105300941115     C                   MOVEL     'TNSDU4R'     VMCQUE                         *CODA MESSAGGI DESTI
105400930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
105500930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
105600930805     C* INIZIALIZZO FLAG CONTROLLO PGM
105700930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
105800930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
105900930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
106000930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
106100930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
106200930721     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
106300930923     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
106400930714     C* LANCIO ROUTINE PER VERIFICARE SE
106500930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
106600930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
106700020709
106800020709     c     *dtaara       define    §azute        azuteds
106900020709     c     *dtaara       define    §datiute      ddatiute
107000020709
107100020709     c                   in(E)     *dtaara
107200020709     c                   if        %Error or rsut = *blanks
107300020709     c                   clear                   tibs34ds
107400020709     c                   call      'TIBS34R'
107500020709     c                   parm                    Tibs34Ds
107600020709     c                   in        *dtaara
107700020709     c                   endif
107800020717
107900020717      * Controllo il profilo utente
108000020717     c                   ExSr      CtrUte
108100020717
108200930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
108300930721     C                   BITOFF    '01234567'    POSMI1            1
108400930721     C                   BITOFF    '01234567'    POSMI2            1
108500930721     C                   BITOFF    '01234567'    POSMA1            1
108600930721     C                   BITOFF    '01234567'    POSMA2            1
108700930721     C                   BITON     '34'          POSMI1            1            *18
108800930721     C                   BITON     '7'           POSMI2            1            *01
108900930721     C                   BITON     '34'          POSMA1            1            *18
109000930721     C                   BITON     '0'           POSMA2            1            *80
109100930721     C                   MOVE      POSMI2        POSMIN            2
109200930721     C                   MOVEL     POSMI1        POSMIN
109300930721     C                   MOVE      POSMA2        POSMAX            2
109400930721     C                   MOVEL     POSMA1        POSMAX
109500930714     C                   ENDSR
109600020717      *------------------------------------------------------------------------*
109700020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
109800020717      *------------------------------------------------------------------------*
109900020717     c     CtrUte        BegSr
110000020717
110100020717     c                   Eval      *In76 = *Off
110200020717
110300020717      * Controllo se il profilo è ok da tibs34
110400020717     c                   If        DutErr = 'E'
110500020717     c                   eval      $Msg = DutMsg
110600020717     c                   ExSr      CarMsg
110700020717     c                   eval      *In76 = *On
110800020717     c                   endIf
110900020717
111000020717      * Controllo se il profilo di sede
111100020717     c                   If        DutLpo <> 'S'
111200020717     c                   Eval      $Msg = Msg(21)
111300020717     c                   ExSr      CarMsg
111400020717     c                   Eval      *In76 = *On
111500020717     c                   EndIf
111600020717
111700020717     c                   endSr
111800930804     C*================================================================
111900930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
112000930804     C*================================================================
112100930804     C     FUNSTD        BEGSR
112200930804     C                   SELECT
112300930804     C     KEY           WHENEQ    F3
112400930804     C                   EXSR      F03STD
112500930804     C     KEY           WHENEQ    F12
112600930804     C                   EXSR      F12STD
112700930804     C     KEY           WHENEQ    F24
112800930804     C                   EXSR      F24STD
112900930804     C                   OTHER
113000930804     C                   MOVEL     MSG(2)        $MSG
113100930804     C                   EXSR      CARMSG
113200930804     C                   ENDSL
113300930804     C                   ENDSR
113400930804     C*================================================================
113500930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
113600930804     C*================================================================
113700930804     C     F03STD        BEGSR
113800930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
113900930923     C                   MOVEL     '1'           $F03
114000930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
114100930804     C                   MOVEL     '1'           $FINE
114200930804     C                   ENDSR
114300930804     C*================================================================
114400930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
114500930804     C*================================================================
114600930804     C     F12STD        BEGSR
114700930805     C* IMPOSTO FLAG DI FIN PGM
114800930804     C                   MOVEL     '1'           $FINE
114900930804     C                   ENDSR
115000930804     C*================================================================
115100930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
115200930804     C*================================================================
115300930804     C     F24STD        BEGSR
115400930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
115500930804     C                   EXSR      OUTFUN
115600930804     C                   ENDSR
115700930804     C*================================================================
115800930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
115900930804     C*================================================================
116000930804     C     INZSFL        BEGSR
116100930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
116200930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
116300930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
116400930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
116500930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
116600930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
116700930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
116800930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
116900930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
117000930804     C                   Z-ADD     12            SFLPAG
117100930804     C                   ENDSR
117200930804     C*================================================================
117300930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
117400930804     C*================================================================
117500930804     C     FUNSFL        BEGSR
117600930804     C                   SELECT
117700930804     C     KEY           WHENEQ    ENTER
117800930804     C                   EXSR      ENTSFL
117900930805     C     KEY           WHENEQ    F3
118000930805     C                   EXSR      F03STD
118100930804     C     KEY           WHENEQ    F5
118200930804     C                   EXSR      F05STD
118300930805     C     KEY           WHENEQ    F12
118400930805     C                   EXSR      F12STD
118500930804     C     KEY           WHENEQ    F17
118600930804     C                   EXSR      F17STD
118700930804     C     KEY           WHENEQ    F18
118800930804     C                   EXSR      F18STD
118900930804     C     KEY           WHENEQ    F23
119000930804     C                   EXSR      F23STD
119100930805     C     KEY           WHENEQ    F24
119200930805     C                   EXSR      F24STD
119300930804     C     KEY           WHENEQ    ROLLUP
119400930804     C                   EXSR      RUPSTD
119500930804     C     KEY           WHENEQ    ROLLDN
119600930804     C                   EXSR      RDNSTD
119700930804     C                   OTHER
119800930804     C                   MOVEL     MSG(2)        $MSG
119900930804     C                   EXSR      CARMSG
120000930804     C                   ENDSL
120100930804     C                   ENDSR
120200930804     C*================================================================
120300930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
120400930804     C*================================================================
120500930804     C     F05STD        BEGSR
120600930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
120700930804     C                   EXSR      POSSFL
120800930804     C                   ENDSR
120900930804     C*================================================================
121000930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
121100930804     C*================================================================
121200930804     C     F17STD        BEGSR
121300930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
121400930806     C                   Z-ADD     1             VCCNBR
121500930804     C                   ENDSR
121600930804     C*================================================================
121700930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
121800930804     C*================================================================
121900930804     C     F18STD        BEGSR
122000930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
122100930806     C                   Z-ADD     SFLULT        VCCNBR
122200930804     C                   ENDSR
122300930804     C*================================================================
122400930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
122500930804     C*================================================================
122600930804     C     F23STD        BEGSR
122700930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
122800930804     C                   EXSR      OUTOPZ
122900930804     C                   ENDSR
123000930804     C*================================================================
123100930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
123200930804     C*================================================================
123300930804     C     RUPSTD        BEGSR
123400930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
123500930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
123600930805     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
123700930805     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
123800930804     C                   ELSE
123900930805     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
124000930805     C                   ENDIF
124100930804     C                   ENDSR
124200930804     C*================================================================
124300930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
124400930804     C*================================================================
124500930804     C     RDNSTD        BEGSR
124600930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
124700930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
124800930804     C     CURSOR        IFGE      POSMIN
124900930804     C     CURSOR        ANDLE     POSMAX
125000930804     C                   ELSE
125100930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
125200930804     C                   MOVEL     '1'           $ERR
125300930804     C                   MOVEL     MSG(5)        $MSG
125400930804     C                   EXSR      CARMSG
125500930805     C                   ENDIF
125600930804     C                   ENDSR
125700930804     C*================================================================
125800930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
125900930804     C*================================================================
126000930804     C     ENTSFL        BEGSR
126100930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
126200930804     C                   EXSR      NEWPOS
126300930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
126400930804     C     $ERR          IFEQ      '0'                                          --- 1 -->
126500930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
126600930804     C     SWPOSI        IFNE      0                                            --- 2 -->
126700930804     C                   EXSR      POSSFL
126800930804     C                   ELSE                                                   <-- 2 -->
126900930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
127000930804     C     SFLULT        IFNE      0                                            --- 3 -->
127100930804     C                   EXSR      OPZSFL
127200930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
127300930804     C     SWMODI        IFNE      0                                            --- 4 -->
127400930804     C                   EXSR      POSSFL
127500930805     C                   ENDIF                                                  <-- 4 ---
127600930805     C                   ENDIF                                                  <-- 3 ---
127700930805     C                   ENDIF                                                  <-- 2 ---
127800930805     C                   ENDIF                                                  <-- 1 ---
127900930804     C                   ENDSR
128000930804     C*================================================================
128100930804     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
128200930804     C*          CARICARE
128300930804     C*================================================================
128400930804     C     POSSFL        BEGSR
128500930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
128600930804     C                   EXSR      CLRSFL
128700930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
128800930804     C                   EXSR      SETFIL
128900930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
129000930804     C     SFLEOF        IFEQ      0
129100930804     C                   EXSR      CARSFL
129200930805     C                   ENDIF
129300930804     C                   ENDSR
129400000000     C*================================================================
129500930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
129600000000     C*================================================================
129700930804     C     OPZSFL        BEGSR
129800930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
129900930802     C                   MOVEL     'S'           OPZOK             1
130000000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
130100000000     C                   Z-ADD     0             SFLVAR            5 0
130200000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
130300000000     C                   Z-ADD     0             SFLERR
130400000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
130500000000     C                   Z-ADD     SFLULT        WFLULT
130600000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
130700941115     C                   READC     SDU4DVS                                49
130800930806     C     *IN49         DOWEQ     '0'                                          --- 1 -->
130900000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
131000000000     C                   ADD       1             SFLVAR
131100930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
131200930805     C     OPZOK         IFEQ      'S'                                          --- 2 -->
131300930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
131400930806     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
131500930802     C                   EXSR      CONOPZ
131600930802     C     OPZOK         IFEQ      'S'
131700930802     C                   EXSR      GESOPZ
131800930805     C                   ENDIF
131900930805     C                   ENDIF                                                  <-- 3 ---
132000000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
132100000000     C*            2. PULIZIA SCELTA IN SFL
132200930805     C     OPZOK         IFEQ      'S'                                          --- 3 -->
132300000000     C                   MOVEL     '0'           *IN41
132400930806     C                   MOVEL     *BLANKS       VSCOPZ
132500930805     C                   ELSE                                                   <-- 3 -->
132600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
132700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
132800000000     C                   MOVEL     '1'           *IN41
132900000000     C                   Z-ADD     SFLULT        SFLERR
133000930805     C                   ENDIF                                                  <-- 3 ---
133100930805     C                   ELSE                                                   <-- 2 -->
133200000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
133300000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
133400000000     C                   MOVEL     '1'           *IN41
133500930805     C                   ENDIF                                                  <-- 2 ---
133600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
133700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
133800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
133900930803     C     OPZOK         IFNE      'S'
134000930806     C                   Z-ADD     SFLERR        VCCNBR
134100000000     C                   ELSE
134200930806     C                   Z-ADD     SFLULT        VCCNBR
134300930805     C                   ENDIF
134400000000     C* AGGIORNO RECORD SFL
134500941115     C                   UPDATE    SDU4DVS
134600941115     C                   READC     SDU4DVS                                49
134700930805     C                   ENDDO                                                  <-- 1 ---
134800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
134900000000     C                   Z-ADD     WFLULT        SFLULT
135000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
135100000000     C     SFLVAR        IFEQ      0
135200020404     C**!!!              MOVEL     '1'           $FINE
135300930805     C                   ENDIF
135400000000     C                   ENDSR
135500930805     C*================================================================
135600930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
135700000000     C*================================================================
135800000000     C     CLRSFL        BEGSR
135900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
136000000000     C* (0=NON FINE FILE  1=FINE FILE)
136100000000     C                   Z-ADD     0             SFLEOF
136200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
136300000000     C                   Z-ADD     0             SFLULT
136400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
136500000000     C                   MOVEL     '1'           *IN40
136600941115     C                   WRITE     SDU4DVC
136700000000     C                   MOVEL     '0'           *IN40
136800000000     C                   ENDSR
136900000000     C*================================================================
137000000000     C* CARSFL - CARICAMENTO SFL
137100000000     C*================================================================
137200000000     C     CARSFL        BEGSR
137300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
137400000000     C     SFLULT        IFEQ      0
137500000000     C                   EXSR      SETFIL
137600930805     C                   ENDIF
137700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
137800000000     C                   Z-ADD     SFLULT        WFLULT
137900000000     C* CICLO:
138000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
138100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
138200000000     C                   Z-ADD     1             SFLRIG
138300930805     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
138400000000     C     SFLEOF        ANDEQ     0
138500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
138600930806     C                   MOVEL     *BLANKS       VSCOPZ
138700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
138800000000     C                   EXSR      IRESFL
138900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
139000000000     C                   ADD       1             SFLULT
139100000000     C* SCRITTURA RECORD SFL
139200941115     C                   WRITE     SDU4DVS
139300000000     C                   ADD       1             SFLRIG
139400000000     C* LETTURA FILE DA CARICARE
139500000000     C                   EXSR      LETFIL
139600930805     C                   ENDDO                                                  <-- 1 ---
139700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
139800000000     C     WFLULT        IFNE      SFLULT
139900930806     C                   Z-ADD     WFLULT        VCCNBR
140000930806     C                   ADD       1             VCCNBR
140100930805     C                   ENDIF
140200930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
140300930805     C     SFLEOF        IFEQ      1                                            --- 1 -->
140400930721     C     SFLULT        ANDGT     *ZEROS
140500930721     C                   MOVEL     '1'           $ERR
140600930804     C                   MOVEL     MSG(6)        $MSG
140700930721     C                   EXSR      CARMSG
140800930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
140900020321     C**!!!SFLULT        CHAIN     SDU4DVS                            99
141000020321     C**!!!*IN99         IFEQ      '0'
141100020321     C**!!!              SETON                                        45
141200020321     C**!!!              UPDATE    SDU4DVS
141300020321     C**!!!              SETOFF                                       45
141400020321     C**!!!              ENDIF
141500930805     C                   ENDIF                                                  <-- 1 ---
141600000000     C                   ENDSR
141700000000     C*================================================================
141800930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
141900000000     C*================================================================
142000000000     C     OUTSFL        BEGSR
142100930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
142200930721     C     MSGULT        IFNE      *ZEROS
142300930721     C                   EXSR      OUTMSG
142400930805     C                   ENDIF
142500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
142600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
142700000000     C     SFLEOF        IFEQ      0
142800000000     C                   MOVEL     '0'           *IN42
142900000000     C                   ELSE
143000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
143100000000     C                   MOVEL     '1'           *IN42
143200930805     C                   ENDIF
143300930720     C* EMISSIONE SUBFILE
143400000000     C     SFLULT        IFLE      0
143500930720     C*  --> CASO DI SFL VUOTO
143600930720     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
143700941115     C                   WRITE     SDU4DVV                                      *'NON VI SONO DATI'
143800941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
143900941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
144000000000     C                   ELSE
144100930720     C*  --> CASO DI SFL CON RECORDS
144200941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
144300941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
144400930805     C                   ENDIF
144500930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
144600930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
144700930806     C                   Z-ADD     FRTSFL        VCCNBR
144800000000     C                   ENDSR
144900930721     C*================================================================
145000930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
145100930721     C*          CODA MESSAGGI DEL PGM
145200930721     C*================================================================
145300930721     C     CARMSG        BEGSR
145400930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
145500930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
145600930721     C     MSGULT        IFEQ      0
145700930721     C                   MOVEL     '1'           *IN40
145800941115     C                   WRITE     SDU4DVK
145900930721     C                   MOVEL     '0'           *IN40
146000930805     C                   ENDIF
146100930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
146200930721     C                   MOVEL     $MSG          DSMSMS
146300930721     C                   CALL      'XMSG'        XMSGPA
146400930721     C     DSMSER        IFEQ      0
146500930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
146600930805     C                   ENDIF
146700930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
146800931015     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
146900941115     C                   WRITE     SDU4DVM                                      *SFL MESS
147000930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
147100930721     C                   MOVEL     *BLANKS       $MSG
147200930721     C                   ENDSR
147300930721     C*================================================================
147400930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
147500930721     C*================================================================
147600930721     C     OUTMSG        BEGSR
147700930721     C* VISUALIZZAZIONE MESSAGGIO
147800930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
147900941115     C                   WRITE     SDU4DVK                                      *CONTR MESS
148000930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
148100930721     C                   Z-ADD     *ZEROS        MSGULT
148200930721     C                   ENDSR
148300930803     C*================================================================
148400930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
148500930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
148600930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
148700930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
148800930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
148900930803     C*================================================================
149000930803     C     CARFUN        BEGSR
149100930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
149200931015     C     1             DO        31            I                 5 0          --- 1 -->
149300930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
149400930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
149500930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
149600020709     c                   If        DutLpo = 'S'
149700930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
149800930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
149900930803     C                   ELSE                                                   *INTERROG.
150000930803     C                   MOVEL     FUNASI        FAT(I)
150100930803     C                   ENDIF
150200930803     C                   ELSE                                                   <-- 2 --> *FILIALE
150300930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
150400930803     C                   MOVEL     FUNAFG        FAT(I)
150500930803     C                   ELSE                                                   *INTERROG.
150600930803     C                   MOVEL     FUNAFI        FAT(I)
150700930803     C                   ENDIF
150800930803     C                   ENDIF                                                  <-- 2 ---
150900930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
151000930806     C                   MOVEL     USER          WEDP              3            *PROFILO
151100930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
151200930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
151300930806     C                   ELSE
151400930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
151500930806     C                   ENDIF
151600930806     C                   ENDIF
151700930803     C                   ENDDO                                                  <-- 1 ---
151800930803     C                   ENDSR
151900930803     C*================================================================
152000930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
152100930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
152200930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
152300930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
152400930803     C*================================================================
152500930803     C     OUTFUN        BEGSR
152600930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
152700930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
152800930803     C                   ENDDO
152900930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
153000930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
153100930803     C     1             DO        31            I
153200930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
153300930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
153400930803     C                   ADD       1             FUNNAT
153500930803     C                   ENDIF
153600930803     C                   ENDDO
153700930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
153800930803     C     FUNNAT        IFGT      8
153900930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
154000930803     C                   ENDIF
154100930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
154200930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
154300930803     C                   Z-ADD     8             FUNVIS            5 0
154400930803     C     FAT(23)       IFEQ      'S'
154500930803     C                   SUB       1             FUNVIS
154600930803     C                   ENDIF
154700930803     C     FAT(24)       IFEQ      'S'
154800930803     C                   SUB       1             FUNVIS
154900930803     C                   ENDIF
155000930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
155100931015     C                   Z-ADD     *ZEROS        W                 5 0
155200930803     C                   ADD       1             FUNULT
155300930803     C     FUNULT        IFGT      31
155400930803     C                   Z-ADD     1             FUNULT
155500930805     C                   ENDIF
155600930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
155700930803     C     FUNULT        DO        31            I                              --- 1 -->
155800930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
155900930803     C     W             IFLT      FUNVIS                                       --- 2 -->
156000930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
156100930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
156200930803     C     FDE(I)        ANDNE     *BLANKS
156300930805     C     I             IFEQ      23                                           *TOLGO F23
156400930805     C     I             OREQ      24                                           *E F24
156500930803     C                   ELSE
156600930803     C                   ADD       1             W
156700930803     C                   MOVEL     FDE(I)        FUV(W)
156800930803     C                   ENDIF
156900930803     C                   ENDIF                                                  <-- 3 ---
157000930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
157100930803     C                   Z-ADD     I             FUNULT
157200930803     C                   ENDIF                                                  <-- 2 ---
157300930803     C                   ENDDO                                                  <-- 1 ---
157400930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
157500930806     C                   MOVEL     FUV(1)        VFD001
157600930806     C                   MOVEL     FUV(2)        VFD002
157700930806     C                   MOVEL     FUV(3)        VFD003
157800930806     C                   MOVEL     FUV(4)        VFD004
157900930806     C                   MOVEL     FUV(5)        VFD005
158000930806     C                   MOVEL     FUV(6)        VFD006
158100930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
158200930803     C     FAT(24)       ANDEQ     'N'
158300930806     C                   MOVEL     FUV(7)        VFD007
158400930806     C                   MOVEL     FUV(8)        VFD008
158500930803     C                   ELSE                                                   <-- 1 -->
158600930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
158700930803     C     FAT(24)       ANDEQ     'S'
158800930806     C                   MOVEL     FDE(23)       VFD007
158900930806     C                   MOVEL     FDE(24)       VFD008
159000930803     C                   ELSE                                                   <-- 2 -->
159100930806     C                   MOVEL     FUV(7)        VFD007
159200930803     C     FAT(23)       IFEQ      'S'
159300930806     C                   MOVEL     FDE(23)       VFD008
159400930803     C                   ELSE
159500930806     C                   MOVEL     FDE(24)       VFD008
159600930805     C                   ENDIF
159700930805     C                   ENDIF                                                  <-- 2 ---
159800930805     C                   ENDIF                                                  <-- 1 ---
159900930803     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
160000930803     C                   ENDSR
160100930803     C*================================================================
160200930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
160300930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
160400930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
160500930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
160600930803     C*================================================================
160700930803     C     CONFUN        BEGSR
160800930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
160900930803     C     1             DO        31            I                              --- 1 -->
161000930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
161100930805     C* CONTROLLO SE IL TASTO E' ATTIVO
161200930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
161300930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
161400930803     C                   EXSR      CARMSG
161500930803     C                   MOVEL     '1'           $ERR
161600930803     C                   ENDIF                                                  <-- 3 ---
161700930803     C                   ENDIF                                                  <-- 2 ---
161800930803     C                   ENDDO                                                  <-- 1 ---
161900930803     C                   ENDSR
162000930802     C*================================================================
162100930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
162200930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
162300930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
162400930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
162500930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
162600930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
162700930802     C*================================================================
162800930802     C     CAROPZ        BEGSR
162900930803     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
163000930802     C     1             DO        24            I                              --- 1 -->
163100930803     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
163200930805     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
163300930805     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
163400930805     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
163500020709     c                   If        DutLpo = 'S'
163600930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
163700930805     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
163800930802     C                   ELSE                                                   *INTERROG.
163900930805     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
164000930802     C                   ENDIF
164100930802     C                   ELSE                                                   <-- 2 --> *FILIALE
164200930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
164300930805     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
164400930802     C                   ELSE                                                   *INTERROG.
164500930805     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
164600930802     C                   ENDIF
164700930802     C                   ENDIF                                                  <-- 2 ---
164800930806     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
164900930806     C                   MOVEL     USER          WEDP              3            *PROFILO
165000930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
165100930806     C                   MOVEL     'S'           OAT(I)                         *ABILITO
165200930806     C                   ELSE
165300930806     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
165400930806     C                   ENDIF
165500930806     C                   ENDIF
165600930802     C                   ENDDO                                                  <-- 1 ---
165700930802     C                   ENDSR
165800930802     C*================================================================
165900930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
166000930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
166100930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
166200930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
166300930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
166400930802     C*================================================================
166500930802     C     OUTOPZ        BEGSR
166600930805     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
166700930805     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
166800930802     C                   ENDDO
166900930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
167000930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
167100930803     C                   ADD       1             OPZULT
167200930803     C     OPZULT        IFGT      24
167300930803     C                   Z-ADD     1             OPZULT
167400930803     C                   END
167500930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
167600930802     C     1             DO        24            I
167700930805     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
167800930805     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
167900930802     C                   ADD       1             OPZNAT
168000930802     C                   ENDIF
168100930802     C                   ENDDO
168200930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
168300931015     C                   Z-ADD     0             W
168400930802     C     OPZULT        DO        24            I                              --- 1 -->
168500930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
168600930802     C     W             IFLT      8                                            --- 2 -->
168700930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
168800930805     C     OAT(I)        IFEQ      'S'
168900930803     C     ODE(I)        ANDNE     *BLANKS
169000930802     C                   ADD       1             W
169100930803     C                   MOVEL     ODE(I)        OPV(W)
169200930805     C                   ENDIF
169300930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
169400930802     C                   Z-ADD     I             OPZULT
169500930802     C                   ENDIF                                                  <-- 2 ---
169600930802     C                   ENDDO                                                  <-- 1 ---
169700930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
169800930806     C                   MOVEL     OPV(1)        VOD001
169900930806     C                   MOVEL     OPV(2)        VOD002
170000930806     C                   MOVEL     OPV(3)        VOD003
170100930806     C                   MOVEL     OPV(4)        VOD004
170200930806     C                   MOVEL     OPV(5)        VOD005
170300930806     C                   MOVEL     OPV(6)        VOD006
170400930806     C                   MOVEL     OPV(7)        VOD007
170500930806     C                   MOVEL     OPV(8)        VOD008
170600930802     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
170700930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
170800930923     C     OPZNAT        IFGT      8
170900930923     C                   MOVEL     'S'           FAT(23)
171000930923     C                   ENDIF
171100930802     C                   ENDSR
171200000000     C*================================================================
171300930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
171400930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
171500930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
171600930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
171700000000     C*================================================================
171800930802     C     CONOPZ        BEGSR
171900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
172000930803     C                   MOVEL     *BLANKS       OPZOK             1
172100930805     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
172200930806     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
172300930806     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
172400930805     C     OAT(I)        IFEQ      'S'
172500930806     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
172600940105     C                   MOVEL     'S'           OPZOK
172700930805     C                   ELSE
172800930802     C                   MOVEL     'N'           OPZOK             1
172900930805     C                   ENDIF
173000930802     C                   ENDIF                                                  <-- 2 ---
173100930802     C                   ENDDO                                                  <-- 1 ---
173200930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
173300930803     C     OPZOK         IFNE      'S'
173400930804     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
173500930803     C                   EXSR      CARMSG
173600930803     C                   MOVEL     '1'           $ERR
173700930803     C                   MOVEL     'N'           OPZOK             1
173800930803     C                   ENDIF
173900000000     C                   ENDSR
174000930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
174100941115TNSDU4R- Tasto funzionale non valido.                                          1
174200941115TNSDU4R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
174300941115TNSDU4R- Opzione non valida.                                                   3
174400941115TNSDU4R- Opzione abilitata ma non gestita. Avvertire CED.                      4
174500941115TNSDU4R- Inizio elenco.                                                        5
174600941115TNSDU4R- Fine elenco.                                                          6
174700941115TNSDU4R- Accettato solo un parametro di inizio elenco.                         7
174800941115TNSDU4R- Magazzino errato.                                                     8
174900941115TNSDU4R- Tipo unità errato.                                                    9
175000941115TNSDU4R- Data errata.                                                         10
175100941115TNSDU4R- Codice unità non accettabile.                                        11
175200960930TNSDU4R- Movimento annullato, consentito solo il ripristino.                  12
175300960930TNSDU4R- Movimento non annullato, impossibile ripristinare.                   13
175400960930TNSDU4R- Richiesta funzione per movimento inesistente.                        14
175500941115TNSDU4R- Si può annullare solo l'ultimo movimento.                            15
175600941115TNSDU4R- Movimento di tipo vecchio, impossibile annullare o modificare.       16
175700960930TNSDU4R- Tipo e matricola inesistente in anagrafica                           17
175800960930TNSDU4R- Tipo e matricola non univoca: posizionarsi per tipo e codice         18
175900020321TNSDU4R- Matricola inesistente in anagrafica                                  19
176000020321TNSDU4R- Matricola non univoca                                                20
176100020717TNSDU4R- Profilo non abilitato alla funzione                                  21
176200000000** ======== SCHIERA: CMD  (COMANDI)                        ================
176300000000WRKSPLF
176400971104DSPRCDLCK FILE(*LIBL/UNMOV00F)
176500000000
176600000000
176700940105
176800930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
176900930803- 1-1 -                   -    NN     NN
177000930803- 2-2 -2=Modifica         -    SN     NN
177100930803- 3-3 -                   -    NN     NN
177200930806- 4-4 -4=Cancellazione    -    SN     NN
177300930803- 5-5 -5=Visualizzazione  -    SS     SS
177400930803- 6-6 -                   -    NN     NN
177500930803- 7-7 -7=Ripristino       -    SN     NN
177600940104- 8-8 -                   -    NN     NN
177700971013- 9-9 -9=Unità            -    SS     SS
177800930806-10-  -                   -    NN     NN
177900930806-11-  -                   -    NN     NN
178000930806-12-  -                   -    NN     NN
178100930803-13-  -                   -    NN     NN
178200930803-14-  -                   -    NN     NN
178300930803-15-  -                   -    NN     NN
178400930803-16-  -                   -    NN     NN
178500930803-17-  -                   -    NN     NN
178600930803-18-  -                   -    NN     NN
178700930803-19-  -                   -    NN     NN
178800930803-20-  -                   -    NN     NN
178900930803-21-  -                   -    NN     NN
179000930803-22-  -                   -    NN     NN
179100930803-23-  -                   -    NN     NN
179200930803-24-  -                   -    NN     NN
179300930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
179400930723F1    -                   -    NN     NN
179500930723F2    -                   -    NN     NN
179600930802F3    -F3=Fine            -    SS     SS
179700930723F4    -                   -    NN     NN
179800971013F5    -F5=Rivisualizza    -    SS     NN
179900930723F6    -F6=Inserimento     -    SN     NN
180000930723F7    -                   -    NN     NN
180100930723F8    -                   -    NN     NN
180200930723F9    -                   -    NN     NN
180300930723F10   -                   -    NN     NN
180400930723F11   -                   -    NN     NN
180500930802F12   -F12=Ritorno        -    SS     SS
180600930806F13   -                   -    NN     NN
180700930806F14   -                   -    NN     NN
180800930723F15   -                   -    NN     NN
180900930723F16   -                   -    NN     NN
181000930802F17   -F17=Inizio dati    -    SS     SS
181100930802F18   -F18=Fine dati      -    SS     SS
181200930723F19   -                   -    NN     NN
181300930723F20   -                   -    NN     NN
181400971013F21   -F21=Stampa         -    SS     NN
181500940105F22   -F22=Gestione stampe-    NN     NN
181600930803F23   -F23=Altre opzioni  -    NN     NN
181700930803F24   -F24=Altri tasti    -    NN     NN
181800930802ENTER -                   -    SS     SS
181900930802ROLLUP-                   -    SS     SS
182000930802ROLLDN-                   -    SS     SS
182100930802HELP  -                   -    NN     NN
182200930802PRINT -                   -    NN     NN
182300930802BSPACE-                   -    NN     NN
182400930802CLEAR -                   -    NN     NN
