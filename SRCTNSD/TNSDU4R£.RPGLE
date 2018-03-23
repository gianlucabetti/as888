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
001300100920     FUNMOV02L  IF   E           K DISK    RENAME(UNMOV000:UNMOV2)
001400100920     FUNMOV03L  IF   E           K DISK    RENAME(UNMOV000:UNMOV3)
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
009000100910
009100100910     d dataiso         s               d   datfmt(*iso)
009200100910     d dataeur         s               d   datfmt(*eur)
009300020321      *------------------------------------------------------------------------*
009400020321      * MAIN LINE
009500020321      *------------------------------------------------------------------------*
009600020321
009700000000     C* CICLO FINO A CHE:
009800930721     C     $FINE         DOUEQ     '1'                                          --- 1 -->
009900000000     C*
010000930805     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010100000000     C*
010200930721     C* CONTROLLA GESTIONE VIDEO
010300930806     C     $GES          IFEQ      'VS'
010400930806     C                   EXSR      GESVS                                        *SUBFILE
010500930805     C                   ENDIF
010600000000     C*
010700930721     C                   ENDDO                                                  <-- 1 ---
010800930923     C*
010900930923     C     $F03          IFEQ      '1'                                          *SE PREMUTO F03
011000930923     C                   MOVEL     '1'           DSUF03                         *LO IMPOSTO NELLA DS
011100930923     C                   ENDIF                                                  *DELLA PROCEDURA
011200000000     C*
011300940105     C                   MOVEL     DSUNIT        KPJBU
011400000000     C                   SETON                                        LR
011500930713     C*================================================================
011600930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011700930713     C*================================================================
011800930806     C     CARVS         BEGSR
011900930713     C*
012000930806     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
012100930713     C*
012200930804     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
012300930804     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
012400930804     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
012500930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012600930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012700930713     C*
012800941115     C                   WRITE     SDU4DVT                                      *TESTATA
012900941115     C                   WRITE     SDU4DVO                                      *OPZIONI
013000941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
013100930713     C*
013200930806     C                   EXSR      POSSFL                                       *CARICA SFL
013300930713     C*
013400930713     C                   ENDSR
013500930713     C*================================================================
013600930806     C* GESVS  - GESTIONE SUBFILE
013700930713     C*================================================================
013800930806     C     GESVS         BEGSR
013900930714     C*
014000930805     C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
014100941115     C                   WRITE     SDU4DVT                                      *TESTATA
014200941115     C                   WRITE     SDU4DVO                                      *OPZIONI
014300941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
014400930714     C                   END
014500930713     C*
014600930805     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
014700930713     C*
014800930805     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
014900020717
015000020717      * Se Profilo non OK devo uscire dal pgm
015100020717     c                   If        *In76
015200020717     c                   Eval      $err = '1'
015300020717     c                   Eval      $fine = '1'
015400020717     c                   EndIf
015500930805     C*
015600930721     C     $ERR          IFEQ      '0'
015700930713     C                   SELECT
015800930804     C     KEY           WHENEQ    F6
015900930804     C                   EXSR      F06GES
016000930804     C     KEY           WHENEQ    F21
016100930804     C                   EXSR      F21GES
016200930804     C     KEY           WHENEQ    F22
016300930804     C                   EXSR      F22GES
016400930804     C                   OTHER
016500930804     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
016600930713     C                   ENDSL
016700930713     C                   ENDIF
016800930713     C*
016900930713     C                   ENDSR
017000000000     C*================================================================
017100930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
017200000000     C*================================================================
017300930126     C     F06GES        BEGSR
017400930127     C*
017500930805     C                   MOVEL     'F06'         WOPZ1             3            *OPZIONE INS.
017600941115     C                   MOVEL     'TNSDU5R'     WPGM              8            *PGM INSERIMENTO
017700930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
017800930128     C*
017900930805     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
018000930127     C*
018100000000     C                   ENDSR
018200930128     C*================================================================
018300930804     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
018400930128     C*================================================================
018500930128     C     F21GES        BEGSR
018600930128     C*
018700940105     C                   MOVEL     DSUNIT        KPJBU
018800941115     C                   CALL      'TNSDU8R'                                    *PGM STAMPA MOVIM.
018900930209     C                   PARM                    KPJBA
019000940105     C                   MOVEL     KPJBU         DSUNIT
019100940105     C*
019200940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
019300940105     C     DSUF03        IFEQ      '1'
019400940105     C                   MOVEL     '1'           $FINE
019500940105     C                   ENDIF
019600930128     C*
019700930128     C                   ENDSR
019800000000     C*================================================================
019900930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
020000000000     C*================================================================
020100000000     C     F22GES        BEGSR
020200000000     C*
020300000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020400000000     C                   Z-ADD     80            LENGH            15 5
020500000000     C                   CALL      'QCAEXEC'
020600000000     C                   PARM                    CMD(1)
020700000000     C                   PARM                    LENGH
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*================================================================
021100930803     C*  GESOPZ - GESTIONE OPZIONI
021200000000     C*================================================================
021300930803     C     GESOPZ        BEGSR
021400930804     C*
021500930804     C                   SELECT
021600930806     C     VSCOPZ        WHENEQ    ' 2'
021700930804     C                   EXSR      O02GES
021800930806     C     VSCOPZ        WHENEQ    ' 4'
021900930804     C                   EXSR      O04GES
022000930806     C     VSCOPZ        WHENEQ    ' 5'
022100930804     C                   EXSR      O05GES
022200930806     C     VSCOPZ        WHENEQ    ' 7'
022300930804     C                   EXSR      O07GES
022400940105     C     VSCOPZ        WHENEQ    ' 9'
022500940105     C                   EXSR      O09GES
022600930804     C                   OTHER
022700930804     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
022800930805     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
022900930804     C                   ENDSL
023000930804     C*
023100000000     C                   ENDSR
023200000000     C*================================================================
023300000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
023400000000     C*================================================================
023500000000     C     O02GES        BEGSR
023600930128     C*
023700930806     C     VSCATB        IFEQ      'A'
023800930721     C                   MOVEL     '1'           $ERR
023900930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
024000930721     C                   EXSR      CARMSG
024100930201     C                   END
024200931217     C*
024300931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
024400931217     C*
024500940104     C     VSHPRG        IFLT      20000
024600931217     C                   MOVEL     '1'           $ERR
024700931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
024800931217     C                   EXSR      CARMSG
024900931217     C                   END
025000930215     C*
025100931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
025200000000     C*
025300930201     C                   MOVEL     'O02'         WOPZ1
025400941115     C                   MOVEL     'TNSDU5R'     WPGM                           *PGM MODIFICA
025500930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
025600000000     C*
025700930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
025800930128     C*
025900931217     C                   ENDIF                                                  <-- 2 ---
026000931217     C                   ENDIF                                                  <-- 1 ---
026100000000     C*
026200000000     C                   ENDSR
026300000000     C*================================================================
026400000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
026500000000     C*================================================================
026600000000     C     O04GES        BEGSR
026700931217     C*
026800931217     C* E' POSSIBILE CANCELLARE SOLO L'ULTIMO MOVIMENTO
026900020322     C                   Z-ADD     VSCTIP        UNMTIP                         *TIPO
027000931217     C                   Z-ADD     VSCCOD        UNMCOD                         *CODICE
027100971010     C                   Z-ADD     VSCDTM        G08DAT
027200971010     C                   Z-ADD     *ZEROS        G08INV
027300971013     C                   MOVEL     '0'           G08ERR
027400971010     C                   CALL      'XSRDA8'
027500971010     C                   PARM                    WLBDA8
027600971010     C                   Z-ADD     G08INV        UNMDTM
027700931217     C                   Z-ADD     VSHPRG        UNMPRG                         *PROGR.MOV.
027800931217     C                   MOVEL     'A'           UNMTMO                         *TIPO MOV.
027900940504     C     KEYUM1        SETGT     UNMOV01L                           99        *FINE-ARCHIV
028000931217     C  N99              READ      UNMOV01L                               99
028100931217     C     *IN99         IFEQ      '0'
028200931217     C     UNMCOD        ANDEQ     VSCCOD
028300931217     C     UNMPRG        ANDLT     9000000
028400931217     C                   MOVEL     '1'           $ERR
028500931217     C                   MOVEL     MSG(15)       $MSG                           *ANN.SOLO ULT.MOV.
028600931217     C                   EXSR      CARMSG
028700931217     C                   END
028800930128     C*
028900930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
029000930806     C     VSCATB        IFEQ      'A'
029100930721     C                   MOVEL     '1'           $ERR
029200930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
029300930721     C                   EXSR      CARMSG
029400930201     C                   END
029500931217     C*
029600931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
029700931217     C*
029800931217     C     VSHPRG        IFGE      9000000
029900931217     C                   MOVEL     '1'           $ERR
030000931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
030100931217     C                   EXSR      CARMSG
030200931217     C                   END
030300930215     C*
030400931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
030500000000     C*
030600971013     C* LANCIO IL PGM DI GESTIONE
030700930201     C                   MOVEL     'O04'         WOPZ1
030800941115     C                   MOVEL     'TNSDU5R'     WPGM
030900930201     C                   EXSR      LANPGM
031000000000     C*
031100930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031200930128     C*
031300931217     C                   END                                                    <-- 2 ---
031400931217     C                   END                                                    <-- 1 ---
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*================================================================
031800000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031900000000     C*================================================================
032000000000     C     O05GES        BEGSR
032100000000     C*
032200000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
032300930201     C                   MOVEL     'O05'         WOPZ1
032400941115     C                   MOVEL     'TNSDU5R'     WPGM
032500930201     C                   EXSR      LANPGM
032600000000     C*
032700000000     C                   ENDSR
032800930128     C*================================================================
032900930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
033000930128     C*================================================================
033100930128     C     O07GES        BEGSR
033200930128     C*
033300930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033400930806     C     VSCATB        IFNE      'A'
033500930721     C                   MOVEL     '1'           $ERR
033600930721     C                   MOVEL     MSG(13)       $MSG
033700930721     C                   EXSR      CARMSG
033800930805     C                   ENDIF
033900930215     C*
034000930721     C     $ERR          IFEQ      '0'                                          --- 1 -->
034100930128     C*
034200930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
034300930201     C                   MOVEL     'O07'         WOPZ1
034400941115     C                   MOVEL     'TNSDU5R'     WPGM
034500930201     C                   EXSR      LANPGM
034600930128     C*
034700930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
034800930128     C                   Z-ADD     1             SWMODI
034900930128     C*
035000930805     C                   ENDIF                                                  <-- 1 ---
035100930128     C*
035200930128     C                   ENDSR
035300940105     C*================================================================
035400940105     C*  O09GES - GESTIONE OPZIONE 9=ANAGRAFICA
035500940105     C*================================================================
035600940105     C     O09GES        BEGSR
035700940105     C*
035800940105     C* LANCIO IL PGM PER VISUALIZZARE DETTAGLIO ANAGRAFICA
035900940105     C                   MOVEL     'O05'         WOPZ1                          *INTERROGAZIONE
036000941115     C                   MOVEL     'TNSDU2R'     WPGM                           *PGM DETT.ANAGRAFICA
036100940105     C                   EXSR      LANPGM
036200940105     C*
036300940105     C                   ENDSR
036400930205     C*================================================================
036500930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
036600930205     C*================================================================
036700930205     C     NEWPOS        BEGSR
036800930205     C*
036900930205     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
037000930205     C*         1= RICHIESTO POSIZIONAM.
037100930205     C                   Z-ADD     0             SWPOSI            7 0
037200930205     C*
037300930205     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037400930205     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037500930205     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037600930205     C                   Z-ADD     0             SWMODI            1 0
037700930205     C*
037800930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
037900930806     C     VCCTIP        IFNE      DEPTIP                                       --- 1 -->
038000930806     C     VCCCOD        ORNE      DEPCOD
038100941128     C     VCCMAT        ORNE      DEPMAT
038200930806     C     VCCMAG        ORNE      DEPMAG
038300930806     C     VCCDTM        ORNE      DEPDTM
038400930205     C*
038500930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
038600930205     C                   EXSR      CHKPAR
038700930205     C*
038800930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
038900930721     C     $ERR          IFEQ      '0'                                          --- 2 -->
039000930205     C                   Z-ADD     1             SWPOSI
039100930806     C                   MOVE      VCCTIP        DEPTIP
039200930806     C                   Z-ADD     VCCCOD        DEPCOD
039300941128     C                   MOVEL     VCCMAT        DEPMAT
039400930806     C                   MOVE      VCCMAG        DEPMAG
039500930806     C                   Z-ADD     VCCDTM        DEPDTM
039600930805     C                   ENDIF                                                  <-- 2 ---
039700930205     C*
039800930805     C                   ENDIF                                                  <-- 1 ---
039900930205     C*
040000930205     C                   ENDSR
040100000000     C*================================================================
040200000000     C* LETTURA RECORD FILE DA CARICARE
040300000000     C*================================================================
040400000000     C     LETFIL        BEGSR
040500000000     C*
040600930203     C* LETTURE SUCCESSIVE ALLA PRIMA
040700930712     C     WTIPRI        IFEQ      'COD'
040800930806     C                   EXSR      READ01
040900930805     C                   ENDIF
041000930212     C*
041100930712     C     WTIPRI        IFEQ      'MAG'
041200930806     C                   EXSR      READ02
041300930805     C                   ENDIF
041400000000     C*
041500000000     C                   ENDSR
041600000000     C*================================================================
041700000000     C* SETFIL - SETLL DEL FILE DA CARICARE
041800000000     C*================================================================
041900000000     C     SETFIL        BEGSR
042000000000     C*
042100930202     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
042200930712     C     WTIPRI        IFEQ      'COD'
042300020325     C                   move      vCCTIP        UNMTIP
042400930806     C                   Z-ADD     VCCCOD        UNMCOD
042500930806     C                   Z-ADD     WCCDTM        UNMDTM
042600930712     C                   Z-ADD     *ZEROS        UNMPRG
042700931217     C                   MOVEL     *BLANKS       UNMTMO
042800020321     c                   if        unmdtm <> *zeros
042900100920     C     KEYUM3        SETLL     UNMOV03L                           99        *FINE-ARCHIV
043000020321     c                   else
043100100920     c     keyum11       setll     unmov03l                           99
043200020321     c                   endif
043300930806     C  N99              EXSR      READ01
043400930212     C                   END
043500930212     C*
043600930712     C     WTIPRI        IFEQ      'MAG'
043700930806     C                   Z-ADD     WCCMAG        UNMMAG
043800930806     C                   Z-ADD     WCCDTM        UNMDTM
043900020403     c                   if        unmdtm <> *zeros
044000930712     C     KEYUM2        SETLL     UNMOV02L                           99        *FINE-ARCHIV
044100020403     c                   else
044200020403     c     keyum21       setll     unmov02l                           99
044300020403     c                   endif
044400930806     C  N99              EXSR      READ02
044500930212     C                   END
044600000000     C*
044700000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
044800930203     C     *IN99         IFEQ      '1'
044900000000     C                   Z-ADD     1             SFLEOF
045000930203     C                   END
045100000000     C*
045200000000     C                   ENDSR
045300930806     C*================================================================
045400930806     C* READ01 - LEGGE RECORD SUCCESSIVO SU UNMOV01L
045500930806     C*================================================================
045600930806     C     READ01        BEGSR
045700930806     C*
045800930806     C                   MOVEL     '0'           RECOK             1
045900930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
046000930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
046100930806     C                   Z-ADD     80            LENGH            15 5
046200930806     C                   CALL      'QCAEXEC'
046300930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
046400930806     C                   PARM                    LENGH
046500930806     C                   ENDIF                                                  <-- 2 ---
046600930806     C                   MOVEL     '0'           RECOK             1
046700100920     C     keyum11       READe     UNMOV03L                             9899
046800930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
046900930806     C                   Z-ADD     1             SFLEOF
047000930806     C                   ELSE                                                   <-- 2 -->
047100020321     C**!!!UNMCOD        ifNE      VCCCOD
047200020321     C**!!!              Z-ADD     1             SFLEOF
047300020321     C**!!!              ENDIF                                                  <-- 3 ---
047400930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
047500930806     C                   MOVEL     '1'           RECOK
047600930806     C                   ENDIF
047700971014     C                   ENDIF                                                  <-- 2 ---
047800930806     C                   ENDDO                                                  <-- 1 ---
047900930806     C*
048000930806     C                   ENDSR
048100930806     C*================================================================
048200930806     C* READ02 - LEGGE RECORD SUCCESSIVO SU UNMOV02L
048300930806     C*================================================================
048400930806     C     READ02        BEGSR
048500930806     C*
048600930806     C                   MOVEL     '0'           RECOK
048700930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
048800930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
048900930806     C                   Z-ADD     80            LENGH            15 5
049000930806     C                   CALL      'QCAEXEC'
049100930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
049200930806     C                   PARM                    LENGH
049300930806     C                   ENDIF                                                  <-- 2 ---
049400930806     C                   MOVEL     '0'           RECOK
049500020403     C     keyum21       READe     UNMOV02L                             9899
049600930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
049700930806     C                   Z-ADD     1             SFLEOF
049800930806     C                   ELSE                                                   <-- 2 -->
049900930806     C     UNMMAG        IFNE      WCCMAG                                       --- 3 -->
050000930806     C                   Z-ADD     1             SFLEOF
050100930806     C                   ELSE                                                   <-- 3 -->
050200930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
050300930806     C                   MOVEL     '1'           RECOK
050400930806     C                   ENDIF
050500930806     C                   ENDIF                                                  <-- 3 ---
050600930806     C                   ENDIF                                                  <-- 2 ---
050700930806     C                   ENDDO                                                  <-- 1 ---
050800930806     C*
050900930806     C                   ENDSR
051000000000     C*================================================================
051100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
051200000000     C*================================================================
051300000000     C     IRESFL        BEGSR
051400000000     C*
051500930806     C                   MOVEL     UNMATB        VSCATB
051600930806     C                   MOVE      UNMTIP        VSCTIP
051700930806     C                   Z-ADD     UNMCOD        VSCCOD
051800100903     a* Reperisco matricola
051900100903     c                   z-add     unmtip        unatip
052000100903     C                   Z-ADD     unmCOD        UNACOD
052100100903     C     KEYUN1        CHAIN     UNANA01L                           99
052200100903     c                   if        %found(unana01l)
052300100903     c                   eval      vscmat=unamat
052400100903     c                   else
052500100903     c                   eval      vscmat=*all'?'
052600100903     c                   endif
052700100903     c
052800100903     C                   MOVE      UNMTIP        WTIP              2            !
052900930712     C                   EXSR      CHKTIP                                       !DECOD.TIPO UNITA
053000930806     C                   MOVEL     WDTIP         VSDTIP                         !
053100930806     C                   Z-ADD     UNMMAG        VSCMAG
053200930712     C                   MOVE      UNMMAG        WMAG                           !
053300930712     C                   EXSR      CHKMAG                                       !DECOD.MAGAZZINO
053400930806     C                   MOVEL     WDMAG         VSDMAG                         !
053500971010     C                   Z-ADD     UNMDTM        G08INV
053600971010     C                   Z-ADD     *ZEROS        G08DAT
053700971013     C                   MOVEL     '3'           G08ERR
053800971010     C                   CALL      'XSRDA8'
053900971010     C                   PARM                    WLBDA8
054000971010     C                   Z-ADD     G08DAT        VSCDTM
054100930806     C                   MOVE      UNMCAU        VSCCAU
054200930712     C                   MOVE      UNMCAU        WCAU              4            !
054300930712     C                   EXSR      CHKCAU                                       !DECOD.CAUSALE MOV.
054400100903     C**                 MOVEL     WDCAU         VSDCAU                         !
054500971013     C                   MOVEL     UNMTMO        VSCTMO
054600930806     C                   Z-ADD     UNMPRG        VSHPRG
054700000000     C*
054800000000     C                   ENDSR
054900020321      *================================================================
055000020321      *  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
055100020321      *================================================================
055200930205     C     CHKPAR        BEGSR
055300020321
055400020321      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
055500930212     C                   SETOFF                                       9091
055600020321
055700020321      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
055800930712     C                   SETOFF                                       808182
055900941128     C                   SETOFF                                       8384
056000020321
056100020321      * IMPOSTO A BLANKS DESCRIZIONI PARAMETRI DI INIZIO LISTA
056200930806     C                   MOVEL     *BLANKS       VCDMAG
056300020321
056400020321      * AZZERA CAMPI DI LAVORO NUMERICI
056500940104     C                   Z-ADD     *ZEROS        WCCMAG            3 0
056600971010     C                   Z-ADD     *ZEROS        WCCDTM            8 0
056700020321
056800020321      * SE UGUALE A ZEROS IMPOSTO BLANKS
056900930806     C     VCCTIP        IFEQ      *ZEROS
057000930806     C                   MOVEL     *BLANKS       VCCTIP
057100930805     C                   ENDIF
057200930806     C     VCCMAG        IFEQ      *ZEROS
057300930806     C                   MOVEL     *BLANKS       VCCMAG
057400930805     C                   ENDIF
057500020321
057600020321      * SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE 'COD'
057700930806     C     VCCTIP        IFEQ      *BLANKS                                      --- 1 -->
057800930806     C     VCCCOD        ANDEQ     *ZEROS
057900941128     C     VCCMAT        ANDEQ     *BLANKS
058000941128     C     VCCMAG        ANDEQ     *BLANKS
058100930806     C     VCCDTM        ANDEQ     *ZEROS
058200930805     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
058300930212     C                   SETON                                        90
058400020321
058500930212     C                   ELSE                                                   <-- 1 -->
058600020321
058700020321      * ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
058800930806     C     VCCTIP        IFNE      *BLANKS
058900930806     C     VCCCOD        ORNE      *ZEROS
059000941128     C     VCCMAT        ORNE      *BLANKS
059100930806     C     VCCMAG        IFNE      *BLANKS
059200930721     C                   MOVEL     MSG(7)        $MSG
059300930721     C                   EXSR      CARMSG
059400930721     C                   MOVEL     '1'           $ERR
059500930712     C                   SETON                                        808182
059600930805     C                   ENDIF
059700930805     C                   ENDIF
059800020321
059900930721     C     $ERR          IFEQ      '0'                                          --- 2 -->
060000020321
060100020321      * CONTROLLO CHE TIPO DI RICERCA E' RICHIESTA
060200020321     C     VCCTIP        IFNE      *BLANKS                                      --- 3 -->
060300930712     C                   MOVEL     'COD'         WTIPRI
060400930215     C                   SETON                                        90
060500020321      * SE E' PER COD, CONTROLLO CHE IL TIPO UNITA ESISTA
060600930806     C                   MOVE      VCCTIP        WTIP
060700930805     C                   EXSR      CHKTIP                                       *CONTROLLA TIPO UNIT
060800020321     C     $ERR          IFEQ      '1'                                          --- 4 -->
060900930721     C                   MOVEL     '1'           $ERR
061000930721     C                   MOVEL     MSG(9)        $MSG
061100930721     C                   EXSR      CARMSG
061200930215     C                   SETON                                        80
061300020321     C                   ELSE                                                   <-- 4 -->
061400930806     C                   MOVE      WTIP          VCCTIP
061500020321     C                   ENDIF                                                  <-- 4 ---
061600020321     C                   ENDIF                                                  <-- 3 ---
061700020321
061800020321      * SE PER MATRICOLA CONTROLLO VADO A REPERIRE IL CODICE UNITA' E CONTROLLO
061900020321      * CHE LA MATRICOLA SIA UNIVOCA (TIP+MAT) SE NO ERRORE
062000020321IF  1C     VCCMAT        IFNE      *BLANKS                                      --- 3 -->
062100020321     c     vcctip        andne     *blanks
062200941128     C                   MOVEL     VCCTIP        UNATIP
062300941128     C                   MOVEL     VCCMAT        UNAMAT
062400941128     C     KEYUN2        SETLL     UNANA02L
062500941128     C     KEYUN2        READE     UNANA02L                               97
062600941128     C                   Z-ADD     *ZEROS        QTATEM            4 0          *QUANTITA' TIP+MAT <
062700020321DO  2C     *IN97         DOWEQ     *OFF                                         --- 4 -->
062800941128     C                   ADD       1             QTATEM
062900941128     C     KEYUN2        READE     UNANA02L                               97
063000020321E   2C                   ENDDO                                                  <-- 4 ---
063100020321
063200020321      * TIPO + MATRICOLA INSESITENTE
063300020321IF  2C     QTATEM        IFEQ      *ZEROS                                       --- 4 -->
063400941128     C                   MOVEL     '1'           $ERR
063500941128     C                   MOVEL     MSG(17)       $MSG
063600941128     C                   EXSR      CARMSG
063700941128     C                   SETON                                        8480
063800020321X   2C                   ELSE                                                   <-- 4 -->
063900020321
064000020321      * TIPO + MATRICOLA NON UNIVOCA
064100020321IF  3C     QTATEM        IFGT      1                                            --- 5 -->
064200941128     C                   MOVEL     '1'           $ERR
064300941128     C                   MOVEL     MSG(18)       $MSG
064400941128     C                   EXSR      CARMSG
064500941128     C                   SETON                                        8480
064600020321X   3C                   ELSE                                                   <-- 5 -->
064700941128     C                   Z-ADD     UNACOD        VCCCOD
064800941128     C                   MOVEL     *BLANKS       VCCMAT
064900020321E   3C                   ENDIF                                                  <-- 5 ---
065000020321E   2C                   ENDIF                                                  <-- 4 ---
065100020321E   1C                   ENDIF                                                  <-- 3 ---
065200020320
065300020321      * per sola matricola cerco il codice unità sull'anagrafico unità
065400020321      * e verifico che sia univoca altrimenti errore
065500020321     c                   if        vcctip = *blanks and vccmat <> *blanks       --- 3 -->
065600020321     c                   clear                   qtatem
065700020321     c                   eval      wtipri = 'COD'
065800020321     c                   eval      *in90 = *on
065900020321     c     vccmat        setll     unana07l
066000020321      * se non esiste matricola errore
066100020321     c                   if        not %equal(unana07l)                         --- 4 -->
066200020321     c                   eval      $err = *on
066300020321     c                   eval      $msg = msg(19)
066400020321     c                   exsr      carmsg
066500020321     c                   eval      *in84 = *on
066600020321      * controllo se univoca
066700020321     c                   else                                                   <-- 4 -->
066800020321     c                   do        *hival                                       --- 5 -->
066900020321     c     vccmat        reade     unana07l
067000020321      * fine file
067100020321     c                   if        %eof(unana07l)                               --- 6 -->
067200020321     c                   leave
067300020321     c                   endif                                                  <-- 6 ---
067400020321     c                   if        qtatem = *zeros                              --- 6 -->
067500020321     c                   add       1             qtatem
067600020321     c                   eval      vcccod = unacod
067700020325     c                   movel     unatip        vcctip
067800020321     c                   clear                   vccmat
067900020321     c                   else                                                   <-- 6 -->
068000020321     c                   eval      $err = *on
068100020321     c                   eval      $msg = msg(20)
068200020321     c                   exsr      carmsg
068300020321     c                   eval      *in84 = *on
068400020321     c                   leave
068500020321     c                   endif                                                  <-- 6 ---
068600020321     c                   enddo                                                  <-- 5 ---
068700020321     c                   endif                                                  <-- 4 ---
068800020321     c                   endif                                                  <-- 3 ---
068900020321
069000930806     C     VCCMAG        IFNE      *BLANKS
069100930712     C                   MOVEL     'MAG'         WTIPRI
069200930212     C                   SETON                                        91
069300020321      * SE E' PER MAG, CONTROLLO CHE ESISTA
069400930806     C                   MOVE      VCCMAG        WMAG
069500930805     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
069600930721     C     $ERR          IFEQ      '1'
069700930721     C                   MOVEL     '1'           $ERR
069800930721     C                   MOVEL     MSG(8)        $MSG
069900930721     C                   EXSR      CARMSG
070000930712     C                   SETON                                        82
070100930712     C                   ELSE
070200930806     C                   MOVE      WMAG          VCCMAG
070300940104     C                   MOVE      WMAG          WCCMAG
070400930806     C                   MOVEL     WDMAG         VCDMAG
070500930805     C                   ENDIF
070600930805     C                   ENDIF
070700020321
070800930805     C                   ENDIF                                                  <-- 2 ---
070900930805     C                   ENDIF                                                  <-- 1 ---
071000020321
071100020321      * CONTROLLO LA DATA DI INIZIO
071200930806     C     VCCDTM        IFNE      *ZEROS
071300971010     C                   Z-ADD     VCCDTM        WDAT              8 0
071400930805     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
071500930805     C     $ERR          IFEQ      '1'                                          *DATA ERRATA
071600930712     C                   SETON                                        83
071700930805     C                   ELSE                                                   *DATA OK
071800971010     C                   Z-ADD     VCCDTM        G08DAT
071900971010     C                   Z-ADD     *ZEROS        G08INV
072000971010     C                   MOVEL     '0'           G08ERR
072100971010     C                   CALL      'XSRDA8'
072200971010     C                   PARM                    WLBDA8
072300971013     C                   Z-ADD     G08DAT        VCCDTM
072400971013     C                   Z-ADD     G08INV        WCCDTM
072500930805     C                   ENDIF
072600930805     C                   ENDIF
072700020321
072800020321      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
072900020321      * CURSORE SUL CAMPO DEL CONTROL
073000930721     C     $ERR          IFEQ      '1'
073100930805     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
073200930715     C                   ELSE
073300930805     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
073400930715     C                   END
073500020321
073600930205     C                   ENDSR
073700020321      *================================================================
073800020321      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
073900020321      *================================================================
074000930712     C     CHKTIP        BEGSR
074100020321
074200020321      * CONTROLLO SE E' STATO INSERITO '?'
074300020321     C     '?'           SCAN      WTIP                                   99
074400020321
074500020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
074600020321     c                   if        *in99                                        --- 1 -->
074700020321     c                   clear                   TIBS02DS
074800020321     c                   movel     'R'           t02mod
074900020321     c                   movel     knsif         t02sif
075000020321     c                   movel     'UNT'         t02cod
075100020321     c                   call      'TIBS02R'
075200020321     c                   parm                    KPJBA
075300020321     c                   parm                    TIBS02DS
075400020321     c                   movel     T02ke1        wtip
075500020321     c                   movel     t02uni        wdtip            30
075600020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
075700020321     c                   endif                                                  <-- 1 ---
075800020321
075900020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
076000020321     c                   clear                   TIBS02DS
076100020321     c                   movel     'C'           T02mod
076200020321     c                   movel     knsif         t02sif
076300020321     c                   movel     'UNT'         t02cod
076400020322     c                   movel(p)  wtip          T02ke1
076500020321     c                   call      'TIBS02R'
076600020321     c                   parm                    KPJBA
076700020321     c                   parm                    TIBS02DS
076800020321     c                   if        t02err <> *blanks
076900020321     c                   eval      $err = '1'
077000020321     c                   clear                   wdtip
077100020321     c                   else
077200020321     c                   movel     t02uni        wdtip
077300020321     c                   endif
077400020321
077500930712     C                   ENDSR
077600020321      *================================================================
077700020321      *  CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
077800020321      *================================================================
077900930712     C     CHKCAU        BEGSR
078000020321
078100020321      * CONTROLLO SE E' STATO INSERITO '?'
078200020321     C     '?'           SCAN      WCAU                                   99
078300020321
078400020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
078500020321     c                   if        *in99                                        --- 1 -->
078600020321     c                   clear                   TIBS02DS
078700020321     c                   movel     'R'           t02mod
078800020321     c                   movel     knsif         t02sif
078900020321     c                   movel     'UNM'         t02cod
079000020321     c                   call      'TIBS02R'
079100020321     c                   parm                    KPJBA
079200020321     c                   parm                    TIBS02DS
079300020321     c                   movel     T02ke1        wcau
079400020321     c                   movel     t02uni        wdcau            30
079500020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
079600020321     c                   endif                                                  <-- 1 ---
079700020321
079800020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
079900020321     c                   clear                   TIBS02DS
080000020321     c                   movel     'C'           T02mod
080100020321     c                   movel     knsif         t02sif
080200020321     c                   movel     'UNM'         t02cod
080300020321     c                   movel(p)  wcau          T02ke1
080400020321     c                   call      'TIBS02R'
080500020321     c                   parm                    KPJBA
080600020321     c                   parm                    TIBS02DS
080700020321     c                   if        t02err <> *blanks
080800020321     c                   eval      $err = '1'
080900020321     c                   clear                   wdcau
081000020321     c                   else
081100020321     c                   movel     t02uni        wdcau
081200020321     c                   endif
081300020321
081400930712     C                   ENDSR
081500930712     C*================================================================
081600930712     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
081700930712     C*================================================================
081800930712     C     CHKMAG        BEGSR
081900930712     C*
082000930712     C* CONTROLLO SE E' STATO INSERITO '?'
082100930712     C     '?'           SCAN      WMAG          A                 1 0    99
082200930712     C*
082300930712     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
082400930712     C     *IN99         IFEQ      '1'                                          --- 1 -->
082500930712     C                   MOVEL     *BLANKS       §COD1             3
082600930712     C                   MOVE      *BLANKS       §DES1
082700941115     C                   CALL      'TNSD20R'
082800930712     C                   PARM                    §COD1
082900930712     C                   PARM                    §TIP              1
083000930712     C                   PARM                    §DES1            25
083100930712     C                   MOVEL     §COD1         WMAG              3
083200930712     C                   MOVEL     §DES1         WDMAG            30
083300930714     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
083400930712     C*
083500930805     C                   ENDIF                                                  <-- 1 ---
083600930712     C*
083700930712     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
083800930712     C                   MOVE      WMAG          ORGFIL
083900020529     C     KEYORG        CHAIN     AZORG21l                           99
084000930712     C     *IN99         IFEQ      '0'                                          --- 2 -->
084100930712     C                   MOVEL     ORGFIL        WMAG
084200930712     C                   MOVEL     ORGDES        WDMAG
084300930712     C                   ELSE                                                   <-- 2 -->
084400020529     c     keyorg        chain     azorg01l
084500020529     c                   if        %found(azorg01l)
084600020529     c                   movel     orgfil        wmag
084700020529     c                   movel     orgdes        wdmag
084800020529     c                   else
084900930721     C                   MOVEL     '1'           $ERR
085000930712     C                   MOVEL     *ALL'*'       WDMAG
085100020529     c                   endif
085200930805     C                   ENDIF                                                  <-- 2 ---
085300930712     C*
085400930712     C                   ENDSR
085500930712     C*================================================================
085600930712     C* CHKDAT - CONTROLLA LA DATA
085700930712     C*================================================================
085800930712     C     CHKDAT        BEGSR
085900930712     C*
086000971010     C                   Z-ADD     WDAT          G08DAT
086100971010     C                   Z-ADD     *ZEROS        G08INV
086200971010     C                   MOVEL     '0'           G08ERR
086300971010     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
086400971010     C                   PARM                    WLBDA8
086500971013     C                   Z-ADD     G08DAT        WDAT
086600971010     C     G08ERR        IFNE      '0'
086700930721     C                   MOVEL     '1'           $ERR
086800930721     C                   MOVEL     MSG(10)       $MSG
086900930721     C                   EXSR      CARMSG
087000930805     C                   ENDIF
087100930712     C*
087200930712     C                   ENDSR
087300930201     C*================================================================
087400930201     C* LANPGM - LANCIA UN PROGRAMMA
087500930201     C*================================================================
087600930201     C     LANPGM        BEGSR
087700930201     C*
087800930201     C* IMPOSTO PARAMETRI PER IL PGM
087900930712     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
088000930712     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
088100930712     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
088200930712     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
088300930806     C                   Z-ADD     VSCTIP        DSUTIP                         *TIPO
088400930806     C                   Z-ADD     VSCCOD        DSUCOD                         *CODICE
088500930806     C                   Z-ADD     VSCMAG        DSUMAG                         *MAGAZZINO
088600930806     C                   Z-ADD     VSHPRG        DSUPRG                         *PROGR.MOV.
088700931217     C                   MOVEL     VSCTMO        DSUTMO                         *TIPO MOVIM.
088800971010     C                   Z-ADD     VSCDTM        G08DAT
088900971010     C                   Z-ADD     *ZEROS        G08INV
089000971010     C                   MOVEL     '0'           G08ERR
089100971010     C                   CALL      'XSRDA8'
089200971010     C                   PARM                    WLBDA8
089300971010     C                   Z-ADD     G08INV        DSUDTM                         *DATA MOVIMENTO
089400930201     C*
089500930201     C* LANCIO PROGRAMMA
089600940105     C                   MOVEL     DSUNIT        KPJBU
089700930312     C                   CALL      WPGM
089800940105     C                   PARM                    KPJBA
089900940105     C                   MOVEL     KPJBU         DSUNIT
090000930201     C*
090100930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
090200930712     C     DSUF03        IFEQ      '1'
090300930721     C                   MOVEL     '1'           $F03
090400930721     C                   MOVEL     '1'           $ERR
090500940105     C                   MOVEL     '1'           $FINE
090600930201     C                   ELSE
090700930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
090800930721     C                   MOVEL     DSUMSG        $MSG
090900930721     C                   EXSR      CARMSG
091000930805     C                   ENDIF
091100930201     C*
091200930201     C                   ENDSR
091300000000     C*================================================================
091400930713     C*  *INZSR - ROUTINE INIZIALE
091500000000     C*================================================================
091600930713     C     *INZSR        BEGSR
091700000000     C*
091800000000     C* RICEVIMENTO PARAMETRI
091900000000     C     *ENTRY        PLIST
092000940105     C                   PARM                    KPJBA
092100940105     C                   MOVEL     KPJBU         DSUNIT
092200020321
092300020321      * CHIAVE LETTURA UNANA01L
092400931116     C     KEYUN1        KLIST
092500020322     C                   KFLD                    UNATIP
092600931116     C                   KFLD                    UNACOD
092700020321
092800020321      * CHIAVE LETTURA UNANA02L
092900941128     C     KEYUN2        KLIST
093000941128     C                   KFLD                    UNATIP
093100941128     C                   KFLD                    UNAMAT
093200020321
093300020321      * CHIAVE LETTURA UNMOV01L
093400930712     C     KEYUM1        KLIST
093500020322     C                   KFLD                    UNMTIP
093600930712     C                   KFLD                    UNMCOD
093700930712     C                   KFLD                    UNMDTM
093800930712     C                   KFLD                    UNMPRG
093900931217     C                   KFLD                    UNMTMO
094000020322
094100100920      * CHIAVE LETTURA UNMOV03L
094200020322     C     KEYUM11       KLIST
094300020322     C                   KFLD                    UNMTIP
094400020322     C                   KFLD                    UNMCOD
094500100920     C     KEYUM3        KLIST
094600100920     C                   KFLD                    UNMTIP
094700100920     C                   KFLD                    UNMCOD
094800100920     C                   KFLD                    UNMDTM
094900020321
095000020321      * CHIAVE LETTURA UNMOV02L
095100930712     C     KEYUM2        KLIST
095200930712     C                   KFLD                    UNMMAG
095300930712     C                   KFLD                    UNMDTM
095400020403      * CHIAVE LETTURA UNMOV02L
095500020403     C     KEYUM21       KLIST
095600020403     C                   KFLD                    UNMMAG
095700930310     C*
095800020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
095900930310     C     KEYORG        KLIST
096000930712     C                   KFLD                    ORGFIL
096100000000     C*
096200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
096300971014     C     *LIKE         DEFINE    VCCTIP        DEPTIP
096400930806     C     *LIKE         DEFINE    VCCCOD        DEPCOD
096500941128     C     *LIKE         DEFINE    VCCMAT        DEPMAT
096600930806     C     *LIKE         DEFINE    VCCMAG        DEPMAG
096700930806     C     *LIKE         DEFINE    VCCDTM        DEPDTM
096800971014     C*
096900971014     C* PARAMETRI DI ENTRATA IN DEPOSITI
097000971014     C     *LIKE         DEFINE    DSUPRO        DEPPRO
097100971014     C     *LIKE         DEFINE    DSUOP0        DEPOP0
097200971014     C                   MOVEL     DSUPRO        DEPPRO
097300971014     C                   MOVEL     DSUOP0        DEPOP0
097400940105     C*
097500940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DELL'ANAGRAF. UNITA'
097600940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
097700940105     C     DSUPRO        IFEQ      'A'
097800940105     C                   MOVEL     OPZ(9)        OPZRIG                         *GEST.MOVIMENTI
097900940105     C                   MOVEL     'N'           OPZASG
098000940105     C                   MOVEL     'N'           OPZASI
098100940105     C                   MOVEL     'N'           OPZAFG
098200940105     C                   MOVEL     'N'           OPZAFI
098300940105     C                   MOVEL     OPZRIG        OPZ(9)
098400940105     C                   END
098500930714     C*
098600930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
098700930803     C                   EXSR      INZSTD
098800930803     C*
098900930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
099000930803     C                   EXSR      INZSFL
099100930714     C*
099200930806     C* IMPOSTO CAMPI TESTATA VIDEO
099300940105     C     DSUOP0        IFEQ      ' 2'
099400930806     C                   MOVEL     '    GEST'    VTDING
099500930806     C                   MOVE      'IONE    '    VTDING
099600930923     C                   MOVEL     'G'           $TGES
099700930714     C                   ELSE
099800930806     C                   MOVEL     ' INTERRO'    VTDING
099900930806     C                   MOVE      'GAZIONE '    VTDING
100000930923     C                   MOVEL     'I'           $TGES
100100930805     C                   ENDIF
100200931116     C*
100300930713     C*
100400930720     C* SE NON SONO STATI PASSATI PARAMETRI IN INPUT EMETTO VIDEATA PARAMETRI
100500930805     C     DSUTIP        IFEQ      *ZEROS                                       --- 1 -->
100600930806     C                   MOVEL     *BLANKS       VCCTIP
100700930806     C                   Z-ADD     *ZEROS        VCCCOD
100800930806     C                   MOVEL     *BLANKS       VCCMAG
100900930806     C                   Z-ADD     *ZEROS        VCCDTM
101000931116     C*
101100931116     C* SE PASSATI CONTROLLO I PARAMETRI IN INPUT AL PGM
101200930805     C                   ELSE                                                   <-- 1 -->
101300020322     c                   z-add     dsutip        unatip
101400931116     C                   Z-ADD     DSUCOD        UNACOD
101500931116     C     KEYUN1        CHAIN     UNANA01L                           99
101600931116     C*
101700931116     C* SE ANAGRAFICA ANNULLATA, PERMETTO SOLO LA VISUALIZZAZIONE
101800931116     C     *IN99         IFEQ      *OFF
101900931116     C     UNAATB        ANDEQ     'A'
102000931116     C                   MOVEL     ' INTERRO'    VTDING
102100931116     C                   MOVE      'GAZIONE '    VTDING
102200931116     C                   MOVEL     'I'           $TGES
102300931116     C                   ENDIF
102400971014     C                   ENDIF                                                  <-- 1 ---
102500931116     C*
102600971014     C                   MOVE      DSUTIP        VCCTIP                         *VIDEO
102700930806     C                   Z-ADD     DSUCOD        VCCCOD
102800930806     C                   MOVEL     *BLANKS       VCCMAG
102900930806     C                   Z-ADD     *ZEROS        VCCDTM
103000100910     c*
103100100910     c                   if        dsudtm>0
103200100910     c                   eval      dataiso=%date(dsudtm)
103300100910     c                   eval      dataeur=dataiso
103400100910     c                   eval      vccdtm=%dec(dataeur)
103500100910     c                   endif
103600971014     C                   MOVE      VCCTIP        DEPTIP                         *DEPOSITI
103700971014     C                   Z-ADD     VCCCOD        DEPCOD
103800971014     C                   MOVE      VCCMAG        DEPMAG
103900971014     C                   Z-ADD     VCCDTM        DEPDTM
104000930805     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
104100930721     C     $ERR          IFEQ      '1'
104200930712     C                   MOVEL     '1'           DSUERR
104300930721     C                   MOVEL     $MSG          DSUMSG
104400931015     C                   MOVEL     '1'           $FINE
104500930805     C                   ENDIF
104600940104     C*
104700940104     C* EMETTO VIDEATA SUBFILE
104800971014     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
104900930806     C                   EXSR      CARVS                                        *CARICA VIDEO SUBFIL
105000000000     C*
105100000000     C                   ENDSR
105200971014     C*================================================================
105300971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
105400971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
105500930805     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
105600930805     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
105700930805     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
105800971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
105900971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
106000930714     C*================================================================
106100930803     C* INZSTD - ROUTINE INIZIALE STANDARD
106200930714     C*================================================================
106300930803     C     INZSTD        BEGSR
106400930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
106500930802     C                   CALL      'XKEYS'
106600930802     C                   PARM                    DSKEYS
106700930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
106800930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
106900930714     C     XMSGPA        PLIST
107000930714     C                   PARM                    DSMSMS
107100930714     C                   PARM                    DSMSPG
107200930714     C                   PARM                    DSMSKE
107300930714     C                   PARM                    DSMSER
107400930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
107500941115     C                   MOVEL     'TNSDU4R'     DSMSPG                         *NOME PGM CHIAMANTE
107600941115     C                   MOVEL     'TNSDU4R'     VMCQUE                         *CODA MESSAGGI DESTI
107700930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
107800930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
107900930805     C* INIZIALIZZO FLAG CONTROLLO PGM
108000930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
108100930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
108200930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
108300930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
108400930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
108500930721     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
108600930923     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
108700930714     C* LANCIO ROUTINE PER VERIFICARE SE
108800930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
108900930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
109000020709
109100020709     c     *dtaara       define    §azute        azuteds
109200020709     c     *dtaara       define    §datiute      ddatiute
109300020709
109400020709     c                   in(E)     *dtaara
109500020709     c                   if        %Error or rsut = *blanks
109600020709     c                   clear                   tibs34ds
109700020709     c                   call      'TIBS34R'
109800020709     c                   parm                    Tibs34Ds
109900020709     c                   in        *dtaara
110000020709     c                   endif
110100020717
110200020717      * Controllo il profilo utente
110300020717     c                   ExSr      CtrUte
110400020717
110500930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
110600930721     C                   BITOFF    '01234567'    POSMI1            1
110700930721     C                   BITOFF    '01234567'    POSMI2            1
110800930721     C                   BITOFF    '01234567'    POSMA1            1
110900930721     C                   BITOFF    '01234567'    POSMA2            1
111000930721     C                   BITON     '34'          POSMI1            1            *18
111100930721     C                   BITON     '7'           POSMI2            1            *01
111200930721     C                   BITON     '34'          POSMA1            1            *18
111300930721     C                   BITON     '0'           POSMA2            1            *80
111400930721     C                   MOVE      POSMI2        POSMIN            2
111500930721     C                   MOVEL     POSMI1        POSMIN
111600930721     C                   MOVE      POSMA2        POSMAX            2
111700930721     C                   MOVEL     POSMA1        POSMAX
111800930714     C                   ENDSR
111900020717      *------------------------------------------------------------------------*
112000020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
112100020717      *------------------------------------------------------------------------*
112200020717     c     CtrUte        BegSr
112300020717
112400020717     c                   Eval      *In76 = *Off
112500020717
112600020717      * Controllo se il profilo è ok da tibs34
112700020717     c                   If        DutErr = 'E'
112800020717     c                   eval      $Msg = DutMsg
112900020717     c                   ExSr      CarMsg
113000020717     c                   eval      *In76 = *On
113100020717     c                   endIf
113200020717
113300020717      * Controllo se il profilo di sede
113400020717     c                   If        DutLpo <> 'S'
113500020717     c                   Eval      $Msg = Msg(21)
113600020717     c                   ExSr      CarMsg
113700020717     c                   Eval      *In76 = *On
113800020717     c                   EndIf
113900020717
114000020717     c                   endSr
114100930804     C*================================================================
114200930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
114300930804     C*================================================================
114400930804     C     FUNSTD        BEGSR
114500930804     C                   SELECT
114600930804     C     KEY           WHENEQ    F3
114700930804     C                   EXSR      F03STD
114800930804     C     KEY           WHENEQ    F12
114900930804     C                   EXSR      F12STD
115000930804     C     KEY           WHENEQ    F24
115100930804     C                   EXSR      F24STD
115200930804     C                   OTHER
115300930804     C                   MOVEL     MSG(2)        $MSG
115400930804     C                   EXSR      CARMSG
115500930804     C                   ENDSL
115600930804     C                   ENDSR
115700930804     C*================================================================
115800930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
115900930804     C*================================================================
116000930804     C     F03STD        BEGSR
116100930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
116200930923     C                   MOVEL     '1'           $F03
116300930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
116400930804     C                   MOVEL     '1'           $FINE
116500930804     C                   ENDSR
116600930804     C*================================================================
116700930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
116800930804     C*================================================================
116900930804     C     F12STD        BEGSR
117000930805     C* IMPOSTO FLAG DI FIN PGM
117100930804     C                   MOVEL     '1'           $FINE
117200930804     C                   ENDSR
117300930804     C*================================================================
117400930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
117500930804     C*================================================================
117600930804     C     F24STD        BEGSR
117700930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
117800930804     C                   EXSR      OUTFUN
117900930804     C                   ENDSR
118000930804     C*================================================================
118100930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
118200930804     C*================================================================
118300930804     C     INZSFL        BEGSR
118400930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
118500930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
118600930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
118700930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
118800930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
118900930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
119000930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
119100930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
119200930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
119300930804     C                   Z-ADD     12            SFLPAG
119400930804     C                   ENDSR
119500930804     C*================================================================
119600930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
119700930804     C*================================================================
119800930804     C     FUNSFL        BEGSR
119900930804     C                   SELECT
120000930804     C     KEY           WHENEQ    ENTER
120100930804     C                   EXSR      ENTSFL
120200930805     C     KEY           WHENEQ    F3
120300930805     C                   EXSR      F03STD
120400930804     C     KEY           WHENEQ    F5
120500930804     C                   EXSR      F05STD
120600930805     C     KEY           WHENEQ    F12
120700930805     C                   EXSR      F12STD
120800930804     C     KEY           WHENEQ    F17
120900930804     C                   EXSR      F17STD
121000930804     C     KEY           WHENEQ    F18
121100930804     C                   EXSR      F18STD
121200930804     C     KEY           WHENEQ    F23
121300930804     C                   EXSR      F23STD
121400930805     C     KEY           WHENEQ    F24
121500930805     C                   EXSR      F24STD
121600930804     C     KEY           WHENEQ    ROLLUP
121700930804     C                   EXSR      RUPSTD
121800930804     C     KEY           WHENEQ    ROLLDN
121900930804     C                   EXSR      RDNSTD
122000930804     C                   OTHER
122100930804     C                   MOVEL     MSG(2)        $MSG
122200930804     C                   EXSR      CARMSG
122300930804     C                   ENDSL
122400930804     C                   ENDSR
122500930804     C*================================================================
122600930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
122700930804     C*================================================================
122800930804     C     F05STD        BEGSR
122900930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
123000930804     C                   EXSR      POSSFL
123100930804     C                   ENDSR
123200930804     C*================================================================
123300930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
123400930804     C*================================================================
123500930804     C     F17STD        BEGSR
123600930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
123700930806     C                   Z-ADD     1             VCCNBR
123800930804     C                   ENDSR
123900930804     C*================================================================
124000930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
124100930804     C*================================================================
124200930804     C     F18STD        BEGSR
124300930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
124400930806     C                   Z-ADD     SFLULT        VCCNBR
124500930804     C                   ENDSR
124600930804     C*================================================================
124700930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
124800930804     C*================================================================
124900930804     C     F23STD        BEGSR
125000930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
125100930804     C                   EXSR      OUTOPZ
125200930804     C                   ENDSR
125300930804     C*================================================================
125400930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
125500930804     C*================================================================
125600930804     C     RUPSTD        BEGSR
125700930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
125800930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
125900930805     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
126000930805     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
126100930804     C                   ELSE
126200930805     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
126300930805     C                   ENDIF
126400930804     C                   ENDSR
126500930804     C*================================================================
126600930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
126700930804     C*================================================================
126800930804     C     RDNSTD        BEGSR
126900930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
127000930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
127100930804     C     CURSOR        IFGE      POSMIN
127200930804     C     CURSOR        ANDLE     POSMAX
127300930804     C                   ELSE
127400930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
127500930804     C                   MOVEL     '1'           $ERR
127600930804     C                   MOVEL     MSG(5)        $MSG
127700930804     C                   EXSR      CARMSG
127800930805     C                   ENDIF
127900930804     C                   ENDSR
128000930804     C*================================================================
128100930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
128200930804     C*================================================================
128300930804     C     ENTSFL        BEGSR
128400930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
128500930804     C                   EXSR      NEWPOS
128600930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
128700930804     C     $ERR          IFEQ      '0'                                          --- 1 -->
128800930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
128900930804     C     SWPOSI        IFNE      0                                            --- 2 -->
129000930804     C                   EXSR      POSSFL
129100930804     C                   ELSE                                                   <-- 2 -->
129200930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
129300930804     C     SFLULT        IFNE      0                                            --- 3 -->
129400930804     C                   EXSR      OPZSFL
129500930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
129600930804     C     SWMODI        IFNE      0                                            --- 4 -->
129700930804     C                   EXSR      POSSFL
129800930805     C                   ENDIF                                                  <-- 4 ---
129900930805     C                   ENDIF                                                  <-- 3 ---
130000930805     C                   ENDIF                                                  <-- 2 ---
130100930805     C                   ENDIF                                                  <-- 1 ---
130200930804     C                   ENDSR
130300930804     C*================================================================
130400930804     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
130500930804     C*          CARICARE
130600930804     C*================================================================
130700930804     C     POSSFL        BEGSR
130800930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
130900930804     C                   EXSR      CLRSFL
131000930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
131100930804     C                   EXSR      SETFIL
131200930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
131300930804     C     SFLEOF        IFEQ      0
131400930804     C                   EXSR      CARSFL
131500930805     C                   ENDIF
131600930804     C                   ENDSR
131700000000     C*================================================================
131800930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
131900000000     C*================================================================
132000930804     C     OPZSFL        BEGSR
132100930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
132200930802     C                   MOVEL     'S'           OPZOK             1
132300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
132400000000     C                   Z-ADD     0             SFLVAR            5 0
132500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
132600000000     C                   Z-ADD     0             SFLERR
132700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
132800000000     C                   Z-ADD     SFLULT        WFLULT
132900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
133000941115     C                   READC     SDU4DVS                                49
133100930806     C     *IN49         DOWEQ     '0'                                          --- 1 -->
133200000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
133300000000     C                   ADD       1             SFLVAR
133400930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
133500930805     C     OPZOK         IFEQ      'S'                                          --- 2 -->
133600930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
133700930806     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
133800930802     C                   EXSR      CONOPZ
133900930802     C     OPZOK         IFEQ      'S'
134000930802     C                   EXSR      GESOPZ
134100930805     C                   ENDIF
134200930805     C                   ENDIF                                                  <-- 3 ---
134300000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
134400000000     C*            2. PULIZIA SCELTA IN SFL
134500930805     C     OPZOK         IFEQ      'S'                                          --- 3 -->
134600000000     C                   MOVEL     '0'           *IN41
134700930806     C                   MOVEL     *BLANKS       VSCOPZ
134800930805     C                   ELSE                                                   <-- 3 -->
134900000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
135000000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
135100000000     C                   MOVEL     '1'           *IN41
135200000000     C                   Z-ADD     SFLULT        SFLERR
135300930805     C                   ENDIF                                                  <-- 3 ---
135400930805     C                   ELSE                                                   <-- 2 -->
135500000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
135600000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
135700000000     C                   MOVEL     '1'           *IN41
135800930805     C                   ENDIF                                                  <-- 2 ---
135900000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
136000000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
136100000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
136200930803     C     OPZOK         IFNE      'S'
136300930806     C                   Z-ADD     SFLERR        VCCNBR
136400000000     C                   ELSE
136500930806     C                   Z-ADD     SFLULT        VCCNBR
136600930805     C                   ENDIF
136700000000     C* AGGIORNO RECORD SFL
136800941115     C                   UPDATE    SDU4DVS
136900941115     C                   READC     SDU4DVS                                49
137000930805     C                   ENDDO                                                  <-- 1 ---
137100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
137200000000     C                   Z-ADD     WFLULT        SFLULT
137300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
137400000000     C     SFLVAR        IFEQ      0
137500020404     C**!!!              MOVEL     '1'           $FINE
137600930805     C                   ENDIF
137700000000     C                   ENDSR
137800930805     C*================================================================
137900930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
138000000000     C*================================================================
138100000000     C     CLRSFL        BEGSR
138200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
138300000000     C* (0=NON FINE FILE  1=FINE FILE)
138400000000     C                   Z-ADD     0             SFLEOF
138500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
138600000000     C                   Z-ADD     0             SFLULT
138700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
138800000000     C                   MOVEL     '1'           *IN40
138900941115     C                   WRITE     SDU4DVC
139000000000     C                   MOVEL     '0'           *IN40
139100000000     C                   ENDSR
139200000000     C*================================================================
139300000000     C* CARSFL - CARICAMENTO SFL
139400000000     C*================================================================
139500000000     C     CARSFL        BEGSR
139600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
139700000000     C     SFLULT        IFEQ      0
139800000000     C                   EXSR      SETFIL
139900930805     C                   ENDIF
140000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
140100000000     C                   Z-ADD     SFLULT        WFLULT
140200000000     C* CICLO:
140300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
140400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
140500000000     C                   Z-ADD     1             SFLRIG
140600930805     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
140700000000     C     SFLEOF        ANDEQ     0
140800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
140900930806     C                   MOVEL     *BLANKS       VSCOPZ
141000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
141100000000     C                   EXSR      IRESFL
141200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
141300000000     C                   ADD       1             SFLULT
141400000000     C* SCRITTURA RECORD SFL
141500941115     C                   WRITE     SDU4DVS
141600000000     C                   ADD       1             SFLRIG
141700000000     C* LETTURA FILE DA CARICARE
141800000000     C                   EXSR      LETFIL
141900930805     C                   ENDDO                                                  <-- 1 ---
142000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
142100000000     C     WFLULT        IFNE      SFLULT
142200930806     C                   Z-ADD     WFLULT        VCCNBR
142300930806     C                   ADD       1             VCCNBR
142400930805     C                   ENDIF
142500930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
142600930805     C     SFLEOF        IFEQ      1                                            --- 1 -->
142700930721     C     SFLULT        ANDGT     *ZEROS
142800930721     C                   MOVEL     '1'           $ERR
142900930804     C                   MOVEL     MSG(6)        $MSG
143000930721     C                   EXSR      CARMSG
143100930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
143200020321     C**!!!SFLULT        CHAIN     SDU4DVS                            99
143300020321     C**!!!*IN99         IFEQ      '0'
143400020321     C**!!!              SETON                                        45
143500020321     C**!!!              UPDATE    SDU4DVS
143600020321     C**!!!              SETOFF                                       45
143700020321     C**!!!              ENDIF
143800930805     C                   ENDIF                                                  <-- 1 ---
143900000000     C                   ENDSR
144000000000     C*================================================================
144100930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
144200000000     C*================================================================
144300000000     C     OUTSFL        BEGSR
144400930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
144500930721     C     MSGULT        IFNE      *ZEROS
144600930721     C                   EXSR      OUTMSG
144700930805     C                   ENDIF
144800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
144900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
145000000000     C     SFLEOF        IFEQ      0
145100000000     C                   MOVEL     '0'           *IN42
145200000000     C                   ELSE
145300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
145400000000     C                   MOVEL     '1'           *IN42
145500930805     C                   ENDIF
145600930720     C* EMISSIONE SUBFILE
145700000000     C     SFLULT        IFLE      0
145800930720     C*  --> CASO DI SFL VUOTO
145900930720     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
146000941115     C                   WRITE     SDU4DVV                                      *'NON VI SONO DATI'
146100941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
146200941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
146300000000     C                   ELSE
146400930720     C*  --> CASO DI SFL CON RECORDS
146500941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
146600941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
146700930805     C                   ENDIF
146800930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
146900930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
147000930806     C                   Z-ADD     FRTSFL        VCCNBR
147100000000     C                   ENDSR
147200930721     C*================================================================
147300930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
147400930721     C*          CODA MESSAGGI DEL PGM
147500930721     C*================================================================
147600930721     C     CARMSG        BEGSR
147700930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
147800930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
147900930721     C     MSGULT        IFEQ      0
148000930721     C                   MOVEL     '1'           *IN40
148100941115     C                   WRITE     SDU4DVK
148200930721     C                   MOVEL     '0'           *IN40
148300930805     C                   ENDIF
148400930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
148500930721     C                   MOVEL     $MSG          DSMSMS
148600930721     C                   CALL      'XMSG'        XMSGPA
148700930721     C     DSMSER        IFEQ      0
148800930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
148900930805     C                   ENDIF
149000930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
149100931015     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
149200941115     C                   WRITE     SDU4DVM                                      *SFL MESS
149300930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
149400930721     C                   MOVEL     *BLANKS       $MSG
149500930721     C                   ENDSR
149600930721     C*================================================================
149700930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
149800930721     C*================================================================
149900930721     C     OUTMSG        BEGSR
150000930721     C* VISUALIZZAZIONE MESSAGGIO
150100930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
150200941115     C                   WRITE     SDU4DVK                                      *CONTR MESS
150300930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
150400930721     C                   Z-ADD     *ZEROS        MSGULT
150500930721     C                   ENDSR
150600930803     C*================================================================
150700930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
150800930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
150900930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
151000930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
151100930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
151200930803     C*================================================================
151300930803     C     CARFUN        BEGSR
151400930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
151500931015     C     1             DO        31            I                 5 0          --- 1 -->
151600930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
151700930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
151800930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
151900020709     c                   If        DutLpo = 'S'
152000930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
152100930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
152200930803     C                   ELSE                                                   *INTERROG.
152300930803     C                   MOVEL     FUNASI        FAT(I)
152400930803     C                   ENDIF
152500930803     C                   ELSE                                                   <-- 2 --> *FILIALE
152600930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
152700930803     C                   MOVEL     FUNAFG        FAT(I)
152800930803     C                   ELSE                                                   *INTERROG.
152900930803     C                   MOVEL     FUNAFI        FAT(I)
153000930803     C                   ENDIF
153100930803     C                   ENDIF                                                  <-- 2 ---
153200930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
153300930806     C                   MOVEL     USER          WEDP              3            *PROFILO
153400930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
153500930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
153600930806     C                   ELSE
153700930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
153800930806     C                   ENDIF
153900930806     C                   ENDIF
154000930803     C                   ENDDO                                                  <-- 1 ---
154100930803     C                   ENDSR
154200930803     C*================================================================
154300930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
154400930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
154500930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
154600930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
154700930803     C*================================================================
154800930803     C     OUTFUN        BEGSR
154900930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
155000930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
155100930803     C                   ENDDO
155200930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
155300930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
155400930803     C     1             DO        31            I
155500930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
155600930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
155700930803     C                   ADD       1             FUNNAT
155800930803     C                   ENDIF
155900930803     C                   ENDDO
156000930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
156100930803     C     FUNNAT        IFGT      8
156200930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
156300930803     C                   ENDIF
156400930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
156500930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
156600930803     C                   Z-ADD     8             FUNVIS            5 0
156700930803     C     FAT(23)       IFEQ      'S'
156800930803     C                   SUB       1             FUNVIS
156900930803     C                   ENDIF
157000930803     C     FAT(24)       IFEQ      'S'
157100930803     C                   SUB       1             FUNVIS
157200930803     C                   ENDIF
157300930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
157400931015     C                   Z-ADD     *ZEROS        W                 5 0
157500930803     C                   ADD       1             FUNULT
157600930803     C     FUNULT        IFGT      31
157700930803     C                   Z-ADD     1             FUNULT
157800930805     C                   ENDIF
157900930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
158000930803     C     FUNULT        DO        31            I                              --- 1 -->
158100930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
158200930803     C     W             IFLT      FUNVIS                                       --- 2 -->
158300930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
158400930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
158500930803     C     FDE(I)        ANDNE     *BLANKS
158600930805     C     I             IFEQ      23                                           *TOLGO F23
158700930805     C     I             OREQ      24                                           *E F24
158800930803     C                   ELSE
158900930803     C                   ADD       1             W
159000930803     C                   MOVEL     FDE(I)        FUV(W)
159100930803     C                   ENDIF
159200930803     C                   ENDIF                                                  <-- 3 ---
159300930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
159400930803     C                   Z-ADD     I             FUNULT
159500930803     C                   ENDIF                                                  <-- 2 ---
159600930803     C                   ENDDO                                                  <-- 1 ---
159700930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
159800930806     C                   MOVEL     FUV(1)        VFD001
159900930806     C                   MOVEL     FUV(2)        VFD002
160000930806     C                   MOVEL     FUV(3)        VFD003
160100930806     C                   MOVEL     FUV(4)        VFD004
160200930806     C                   MOVEL     FUV(5)        VFD005
160300930806     C                   MOVEL     FUV(6)        VFD006
160400930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
160500930803     C     FAT(24)       ANDEQ     'N'
160600930806     C                   MOVEL     FUV(7)        VFD007
160700930806     C                   MOVEL     FUV(8)        VFD008
160800930803     C                   ELSE                                                   <-- 1 -->
160900930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
161000930803     C     FAT(24)       ANDEQ     'S'
161100930806     C                   MOVEL     FDE(23)       VFD007
161200930806     C                   MOVEL     FDE(24)       VFD008
161300930803     C                   ELSE                                                   <-- 2 -->
161400930806     C                   MOVEL     FUV(7)        VFD007
161500930803     C     FAT(23)       IFEQ      'S'
161600930806     C                   MOVEL     FDE(23)       VFD008
161700930803     C                   ELSE
161800930806     C                   MOVEL     FDE(24)       VFD008
161900930805     C                   ENDIF
162000930805     C                   ENDIF                                                  <-- 2 ---
162100930805     C                   ENDIF                                                  <-- 1 ---
162200930803     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
162300930803     C                   ENDSR
162400930803     C*================================================================
162500930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
162600930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
162700930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
162800930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
162900930803     C*================================================================
163000930803     C     CONFUN        BEGSR
163100930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
163200930803     C     1             DO        31            I                              --- 1 -->
163300930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
163400930805     C* CONTROLLO SE IL TASTO E' ATTIVO
163500930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
163600930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
163700930803     C                   EXSR      CARMSG
163800930803     C                   MOVEL     '1'           $ERR
163900930803     C                   ENDIF                                                  <-- 3 ---
164000930803     C                   ENDIF                                                  <-- 2 ---
164100930803     C                   ENDDO                                                  <-- 1 ---
164200930803     C                   ENDSR
164300930802     C*================================================================
164400930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
164500930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
164600930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
164700930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
164800930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
164900930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
165000930802     C*================================================================
165100930802     C     CAROPZ        BEGSR
165200930803     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
165300930802     C     1             DO        24            I                              --- 1 -->
165400930803     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
165500930805     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
165600930805     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
165700930805     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
165800020709     c                   If        DutLpo = 'S'
165900930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
166000930805     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
166100930802     C                   ELSE                                                   *INTERROG.
166200930805     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
166300930802     C                   ENDIF
166400930802     C                   ELSE                                                   <-- 2 --> *FILIALE
166500930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
166600930805     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
166700930802     C                   ELSE                                                   *INTERROG.
166800930805     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
166900930802     C                   ENDIF
167000930802     C                   ENDIF                                                  <-- 2 ---
167100930806     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
167200930806     C                   MOVEL     USER          WEDP              3            *PROFILO
167300930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
167400930806     C                   MOVEL     'S'           OAT(I)                         *ABILITO
167500930806     C                   ELSE
167600930806     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
167700930806     C                   ENDIF
167800930806     C                   ENDIF
167900930802     C                   ENDDO                                                  <-- 1 ---
168000930802     C                   ENDSR
168100930802     C*================================================================
168200930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
168300930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
168400930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
168500930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
168600930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
168700930802     C*================================================================
168800930802     C     OUTOPZ        BEGSR
168900930805     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
169000930805     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
169100930802     C                   ENDDO
169200930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
169300930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
169400930803     C                   ADD       1             OPZULT
169500930803     C     OPZULT        IFGT      24
169600930803     C                   Z-ADD     1             OPZULT
169700930803     C                   END
169800930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
169900930802     C     1             DO        24            I
170000930805     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
170100930805     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
170200930802     C                   ADD       1             OPZNAT
170300930802     C                   ENDIF
170400930802     C                   ENDDO
170500930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
170600931015     C                   Z-ADD     0             W
170700930802     C     OPZULT        DO        24            I                              --- 1 -->
170800930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
170900930802     C     W             IFLT      8                                            --- 2 -->
171000930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
171100930805     C     OAT(I)        IFEQ      'S'
171200930803     C     ODE(I)        ANDNE     *BLANKS
171300930802     C                   ADD       1             W
171400930803     C                   MOVEL     ODE(I)        OPV(W)
171500930805     C                   ENDIF
171600930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
171700930802     C                   Z-ADD     I             OPZULT
171800930802     C                   ENDIF                                                  <-- 2 ---
171900930802     C                   ENDDO                                                  <-- 1 ---
172000930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
172100930806     C                   MOVEL     OPV(1)        VOD001
172200930806     C                   MOVEL     OPV(2)        VOD002
172300930806     C                   MOVEL     OPV(3)        VOD003
172400930806     C                   MOVEL     OPV(4)        VOD004
172500930806     C                   MOVEL     OPV(5)        VOD005
172600930806     C                   MOVEL     OPV(6)        VOD006
172700930806     C                   MOVEL     OPV(7)        VOD007
172800930806     C                   MOVEL     OPV(8)        VOD008
172900930802     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
173000930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
173100930923     C     OPZNAT        IFGT      8
173200930923     C                   MOVEL     'S'           FAT(23)
173300930923     C                   ENDIF
173400930802     C                   ENDSR
173500000000     C*================================================================
173600930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
173700930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
173800930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
173900930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
174000000000     C*================================================================
174100930802     C     CONOPZ        BEGSR
174200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174300930803     C                   MOVEL     *BLANKS       OPZOK             1
174400930805     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
174500930806     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
174600930806     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
174700930805     C     OAT(I)        IFEQ      'S'
174800930806     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
174900940105     C                   MOVEL     'S'           OPZOK
175000930805     C                   ELSE
175100930802     C                   MOVEL     'N'           OPZOK             1
175200930805     C                   ENDIF
175300930802     C                   ENDIF                                                  <-- 2 ---
175400930802     C                   ENDDO                                                  <-- 1 ---
175500930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
175600930803     C     OPZOK         IFNE      'S'
175700930804     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
175800930803     C                   EXSR      CARMSG
175900930803     C                   MOVEL     '1'           $ERR
176000930803     C                   MOVEL     'N'           OPZOK             1
176100930803     C                   ENDIF
176200000000     C                   ENDSR
176300930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
176400941115TNSDU4R- Tasto funzionale non valido.                                          1
176500941115TNSDU4R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
176600941115TNSDU4R- Opzione non valida.                                                   3
176700941115TNSDU4R- Opzione abilitata ma non gestita. Avvertire CED.                      4
176800941115TNSDU4R- Inizio elenco.                                                        5
176900941115TNSDU4R- Fine elenco.                                                          6
177000941115TNSDU4R- Accettato solo un parametro di inizio elenco.                         7
177100941115TNSDU4R- Magazzino errato.                                                     8
177200941115TNSDU4R- Tipo unità errato.                                                    9
177300941115TNSDU4R- Data errata.                                                         10
177400941115TNSDU4R- Codice unità non accettabile.                                        11
177500960930TNSDU4R- Movimento annullato, consentito solo il ripristino.                  12
177600960930TNSDU4R- Movimento non annullato, impossibile ripristinare.                   13
177700960930TNSDU4R- Richiesta funzione per movimento inesistente.                        14
177800941115TNSDU4R- Si può annullare solo l'ultimo movimento.                            15
177900941115TNSDU4R- Movimento di tipo vecchio, impossibile annullare o modificare.       16
178000960930TNSDU4R- Tipo e matricola inesistente in anagrafica                           17
178100960930TNSDU4R- Tipo e matricola non univoca: posizionarsi per tipo e codice         18
178200020321TNSDU4R- Matricola inesistente in anagrafica                                  19
178300020321TNSDU4R- Matricola non univoca                                                20
178400020717TNSDU4R- Profilo non abilitato alla funzione                                  21
178500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
178600000000WRKSPLF
178700971104DSPRCDLCK FILE(*LIBL/UNMOV00F)
178800000000
178900000000
179000940105
179100930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
179200930803- 1-1 -                   -    NN     NN
179300930803- 2-2 -2=Modifica         -    SN     NN
179400930803- 3-3 -                   -    NN     NN
179500930806- 4-4 -4=Cancellazione    -    SN     NN
179600930803- 5-5 -5=Visualizzazione  -    SS     SS
179700930803- 6-6 -                   -    NN     NN
179800930803- 7-7 -7=Ripristino       -    SN     NN
179900940104- 8-8 -                   -    NN     NN
180000971013- 9-9 -9=Unità            -    SS     SS
180100930806-10-  -                   -    NN     NN
180200930806-11-  -                   -    NN     NN
180300930806-12-  -                   -    NN     NN
180400930803-13-  -                   -    NN     NN
180500930803-14-  -                   -    NN     NN
180600930803-15-  -                   -    NN     NN
180700930803-16-  -                   -    NN     NN
180800930803-17-  -                   -    NN     NN
180900930803-18-  -                   -    NN     NN
181000930803-19-  -                   -    NN     NN
181100930803-20-  -                   -    NN     NN
181200930803-21-  -                   -    NN     NN
181300930803-22-  -                   -    NN     NN
181400930803-23-  -                   -    NN     NN
181500930803-24-  -                   -    NN     NN
181600930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
181700930723F1    -                   -    NN     NN
181800930723F2    -                   -    NN     NN
181900930802F3    -F3=Fine            -    SS     SS
182000930723F4    -                   -    NN     NN
182100971013F5    -F5=Rivisualizza    -    SS     NN
182200930723F6    -F6=Inserimento     -    SN     NN
182300930723F7    -                   -    NN     NN
182400930723F8    -                   -    NN     NN
182500930723F9    -                   -    NN     NN
182600930723F10   -                   -    NN     NN
182700930723F11   -                   -    NN     NN
182800930802F12   -F12=Ritorno        -    SS     SS
182900930806F13   -                   -    NN     NN
183000930806F14   -                   -    NN     NN
183100930723F15   -                   -    NN     NN
183200930723F16   -                   -    NN     NN
183300930802F17   -F17=Inizio dati    -    SS     SS
183400930802F18   -F18=Fine dati      -    SS     SS
183500930723F19   -                   -    NN     NN
183600930723F20   -                   -    NN     NN
183700971013F21   -F21=Stampa         -    SS     NN
183800940105F22   -F22=Gestione stampe-    NN     NN
183900930803F23   -F23=Altre opzioni  -    NN     NN
184000930803F24   -F24=Altri tasti    -    NN     NN
184100930802ENTER -                   -    SS     SS
184200930802ROLLUP-                   -    SS     SS
184300930802ROLLDN-                   -    SS     SS
184400930802HELP  -                   -    NN     NN
184500930802PRINT -                   -    NN     NN
184600930802BSPACE-                   -    NN     NN
184700930802CLEAR -                   -    NN     NN
