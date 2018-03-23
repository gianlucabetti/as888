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
001500110406     FUNMOV04L  IF   E           K DISK    RENAME(UNMOV000:UNMOV4)
001600931116     FUNANA01L  IF   E           K DISK
001700941128     FUNANA02L  IF   E           K DISK
001800941128     F                                     RENAME(UNANA000:UNANA2)
001900020321     funana07l  if   e           k disk    rename(unana000:unana7)
002000020529     FAZORG21l  IF   E           K DISK
002100020529     fazorg01l  if   e           k disk
002200020321
002300020321      *------------------------------------------------------------------------*
002400020321      * SCHIERE
002500020321      *------------------------------------------------------------------------*
002600020321      * SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002700110407     D MSG             S             78    DIM(22) CTDATA PERRCD(1)             *MESSAGGI
002800930802     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002900930802     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
003000930802     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
003100930802     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003200020321      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003300930803     D FFU             S              6    DIM(31)                              *FUNZIONE
003400930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003500930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003600020321      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003700930803     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003800930803     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003900930803     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
004000930803     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004100020321      * TABELLA DESCRIZIONI A VIDEO
004200930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004300930802     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004400020321      * TABELLA TASTI FUNZIONALI PREMUTI
004500930802     D KEYS            S              1    DIM(40)
004600020321
004700020321      *------------------------------------------------------------------------*
004800020321      * INPUT
004900020321      *------------------------------------------------------------------------*
005000020321      * RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005100930723     D DSOPZ         E DS
005200930803     D  OPZRIG                 1     40
005300020321      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005400930723     D DSFUN         E DS
005500930803     D  FUNRIG                 1     40
005600020321      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005700930127     D DSMSG         E DS
005800020321      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
005900930712     D DSUNIT        E DS
006000020321      *------------------
006100020321      * DS "XSRDA8" - CONTROLLA DATA (8)
006200020321      *------------------
006300971010     D WLBDA8          DS
006400971010     D  G08DAT                 1      8  0
006500971010     D  G08INV                 9     16  0
006600971010     D  G08ERR                17     17
006700971010     D  G08TGI                18     22  0
006800020321      * DS PER GESTIONE TASTI FUNZIONALI
006900930802     D DSKEYS        E DS
007000930803     D  KEYRIG                 1     40
007100020321      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007200930806     D                SDS
007300020321      * NOME UTENTE
007400930806     D  USER                 254    263
007500020321      * RIDEFINIZIONE INFDS DEL FILE VIDEO
007600000000     D DEVDS1          DS
007700020321      * CHIAVE FUNZIONALE BATTUTA
007800000000     D  KEY                  369    369
007900020321      * POSIZIONE CURSORE (IN HEX)
008000930721     D  CURSOR               370    371
008100020321      * RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008200930720     D  FRTSFL               378    379B 0
008300020321      * ARCHITETTURA
008400000000     D KPJBA         E DS
008500020320
008600020320     d tibs02ds      e ds
008700020320
008800020709     d ddatiute      e ds
008900020709     d azuteds       e ds                  extname(azute00f)
009000020709     d tibs34ds      e ds                  inz
009100100910
009200100910     d dataiso         s               d   datfmt(*iso)
009300100910     d dataeur         s               d   datfmt(*eur)
009400110406     d savtip          s                   like(unmtip)
009500020321      *------------------------------------------------------------------------*
009600020321      * MAIN LINE
009700020321      *------------------------------------------------------------------------*
009800020321
009900000000     C* CICLO FINO A CHE:
010000930721     C     $FINE         DOUEQ     '1'                                          --- 1 -->
010100000000     C*
010200930805     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010300000000     C*
010400930721     C* CONTROLLA GESTIONE VIDEO
010500930806     C     $GES          IFEQ      'VS'
010600930806     C                   EXSR      GESVS                                        *SUBFILE
010700930805     C                   ENDIF
010800000000     C*
010900930721     C                   ENDDO                                                  <-- 1 ---
011000930923     C*
011100930923     C     $F03          IFEQ      '1'                                          *SE PREMUTO F03
011200930923     C                   MOVEL     '1'           DSUF03                         *LO IMPOSTO NELLA DS
011300930923     C                   ENDIF                                                  *DELLA PROCEDURA
011400000000     C*
011500940105     C                   MOVEL     DSUNIT        KPJBU
011600000000     C                   SETON                                        LR
011700930713     C*================================================================
011800930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011900930713     C*================================================================
012000930806     C     CARVS         BEGSR
012100930713     C*
012200930806     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
012300930713     C*
012400930804     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
012500930804     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
012600930804     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
012700930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
012800930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
012900930713     C*
013000941115     C                   WRITE     SDU4DVT                                      *TESTATA
013100941115     C                   WRITE     SDU4DVO                                      *OPZIONI
013200941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
013300930713     C*
013400930806     C                   EXSR      POSSFL                                       *CARICA SFL
013500930713     C*
013600930713     C                   ENDSR
013700930713     C*================================================================
013800930806     C* GESVS  - GESTIONE SUBFILE
013900930713     C*================================================================
014000930806     C     GESVS         BEGSR
014100930714     C*
014200930805     C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
014300941115     C                   WRITE     SDU4DVT                                      *TESTATA
014400941115     C                   WRITE     SDU4DVO                                      *OPZIONI
014500941115     C                   WRITE     SDU4DVF                                      *FUNZIONI
014600930714     C                   END
014700930713     C*
014800930805     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
014900930713     C*
015000930805     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
015100020717
015200020717      * Se Profilo non OK devo uscire dal pgm
015300020717     c                   If        *In76
015400020717     c                   Eval      $err = '1'
015500020717     c                   Eval      $fine = '1'
015600020717     c                   EndIf
015700930805     C*
015800930721     C     $ERR          IFEQ      '0'
015900930713     C                   SELECT
016000930804     C     KEY           WHENEQ    F6
016100930804     C                   EXSR      F06GES
016200930804     C     KEY           WHENEQ    F21
016300930804     C                   EXSR      F21GES
016400930804     C     KEY           WHENEQ    F22
016500930804     C                   EXSR      F22GES
016600930804     C                   OTHER
016700930804     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
016800930713     C                   ENDSL
016900930713     C                   ENDIF
017000930713     C*
017100930713     C                   ENDSR
017200000000     C*================================================================
017300930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
017400000000     C*================================================================
017500930126     C     F06GES        BEGSR
017600930127     C*
017700930805     C                   MOVEL     'F06'         WOPZ1             3            *OPZIONE INS.
017800941115     C                   MOVEL     'TNSDU5R'     WPGM              8            *PGM INSERIMENTO
017900930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
018000930128     C*
018100930805     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
018200930127     C*
018300000000     C                   ENDSR
018400930128     C*================================================================
018500930804     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
018600930128     C*================================================================
018700930128     C     F21GES        BEGSR
018800930128     C*
018900940105     C                   MOVEL     DSUNIT        KPJBU
019000941115     C                   CALL      'TNSDU8R'                                    *PGM STAMPA MOVIM.
019100930209     C                   PARM                    KPJBA
019200940105     C                   MOVEL     KPJBU         DSUNIT
019300940105     C*
019400940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
019500940105     C     DSUF03        IFEQ      '1'
019600940105     C                   MOVEL     '1'           $FINE
019700940105     C                   ENDIF
019800930128     C*
019900930128     C                   ENDSR
020000000000     C*================================================================
020100930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
020200000000     C*================================================================
020300000000     C     F22GES        BEGSR
020400000000     C*
020500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020600000000     C                   Z-ADD     80            LENGH            15 5
020700000000     C                   CALL      'QCAEXEC'
020800000000     C                   PARM                    CMD(1)
020900000000     C                   PARM                    LENGH
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*================================================================
021300930803     C*  GESOPZ - GESTIONE OPZIONI
021400000000     C*================================================================
021500930803     C     GESOPZ        BEGSR
021600930804     C*
021700930804     C                   SELECT
021800930806     C     VSCOPZ        WHENEQ    ' 2'
021900930804     C                   EXSR      O02GES
022000930806     C     VSCOPZ        WHENEQ    ' 4'
022100930804     C                   EXSR      O04GES
022200930806     C     VSCOPZ        WHENEQ    ' 5'
022300930804     C                   EXSR      O05GES
022400930806     C     VSCOPZ        WHENEQ    ' 7'
022500930804     C                   EXSR      O07GES
022600940105     C     VSCOPZ        WHENEQ    ' 9'
022700940105     C                   EXSR      O09GES
022800930804     C                   OTHER
022900930804     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
023000930805     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
023100930804     C                   ENDSL
023200930804     C*
023300000000     C                   ENDSR
023400000000     C*================================================================
023500000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
023600000000     C*================================================================
023700000000     C     O02GES        BEGSR
023800930128     C*
023900930806     C     VSCATB        IFEQ      'A'
024000930721     C                   MOVEL     '1'           $ERR
024100930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
024200930721     C                   EXSR      CARMSG
024300930201     C                   END
024400931217     C*
024500931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
024600931217     C*
024700940104     C     VSHPRG        IFLT      20000
024800931217     C                   MOVEL     '1'           $ERR
024900931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
025000931217     C                   EXSR      CARMSG
025100931217     C                   END
025200930215     C*
025300931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
025400000000     C*
025500930201     C                   MOVEL     'O02'         WOPZ1
025600941115     C                   MOVEL     'TNSDU5R'     WPGM                           *PGM MODIFICA
025700930805     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
025800000000     C*
025900930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
026000930128     C*
026100931217     C                   ENDIF                                                  <-- 2 ---
026200931217     C                   ENDIF                                                  <-- 1 ---
026300000000     C*
026400000000     C                   ENDSR
026500000000     C*================================================================
026600000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
026700000000     C*================================================================
026800000000     C     O04GES        BEGSR
026900931217     C*
027000931217     C* E' POSSIBILE CANCELLARE SOLO L'ULTIMO MOVIMENTO
027100020322     C                   Z-ADD     VSCTIP        UNMTIP                         *TIPO
027200931217     C                   Z-ADD     VSCCOD        UNMCOD                         *CODICE
027300971010     C                   Z-ADD     VSCDTM        G08DAT
027400971010     C                   Z-ADD     *ZEROS        G08INV
027500971013     C                   MOVEL     '0'           G08ERR
027600971010     C                   CALL      'XSRDA8'
027700971010     C                   PARM                    WLBDA8
027800971010     C                   Z-ADD     G08INV        UNMDTM
027900931217     C                   Z-ADD     VSHPRG        UNMPRG                         *PROGR.MOV.
028000931217     C                   MOVEL     'A'           UNMTMO                         *TIPO MOV.
028100940504     C     KEYUM1        SETGT     UNMOV01L                           99        *FINE-ARCHIV
028200931217     C  N99              READ      UNMOV01L                               99
028300931217     C     *IN99         IFEQ      '0'
028400931217     C     UNMCOD        ANDEQ     VSCCOD
028500931217     C     UNMPRG        ANDLT     9000000
028600931217     C                   MOVEL     '1'           $ERR
028700931217     C                   MOVEL     MSG(15)       $MSG                           *ANN.SOLO ULT.MOV.
028800931217     C                   EXSR      CARMSG
028900931217     C                   END
029000930128     C*
029100930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
029200930806     C     VSCATB        IFEQ      'A'
029300930721     C                   MOVEL     '1'           $ERR
029400930805     C                   MOVEL     MSG(12)       $MSG                           *GIA' ANNULLATO
029500930721     C                   EXSR      CARMSG
029600930201     C                   END
029700931217     C*
029800931217     C     $ERR          IFEQ      '0'                                          --- 1 -->
029900931217     C*
030000931217     C     VSHPRG        IFGE      9000000
030100931217     C                   MOVEL     '1'           $ERR
030200931217     C                   MOVEL     MSG(16)       $MSG                           *MOV. TIPO VECCHIO
030300931217     C                   EXSR      CARMSG
030400931217     C                   END
030500930215     C*
030600931217     C     $ERR          IFEQ      '0'                                          --- 2 -->
030700000000     C*
030800971013     C* LANCIO IL PGM DI GESTIONE
030900930201     C                   MOVEL     'O04'         WOPZ1
031000941115     C                   MOVEL     'TNSDU5R'     WPGM
031100930201     C                   EXSR      LANPGM
031200000000     C*
031300930805     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031400930128     C*
031500931217     C                   END                                                    <-- 2 ---
031600931217     C                   END                                                    <-- 1 ---
031700000000     C*
031800000000     C                   ENDSR
031900000000     C*================================================================
032000000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
032100000000     C*================================================================
032200000000     C     O05GES        BEGSR
032300000000     C*
032400000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
032500930201     C                   MOVEL     'O05'         WOPZ1
032600941115     C                   MOVEL     'TNSDU5R'     WPGM
032700930201     C                   EXSR      LANPGM
032800000000     C*
032900000000     C                   ENDSR
033000930128     C*================================================================
033100930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
033200930128     C*================================================================
033300930128     C     O07GES        BEGSR
033400930128     C*
033500930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033600930806     C     VSCATB        IFNE      'A'
033700930721     C                   MOVEL     '1'           $ERR
033800930721     C                   MOVEL     MSG(13)       $MSG
033900930721     C                   EXSR      CARMSG
034000930805     C                   ENDIF
034100930215     C*
034200930721     C     $ERR          IFEQ      '0'                                          --- 1 -->
034300930128     C*
034400930201     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
034500930201     C                   MOVEL     'O07'         WOPZ1
034600941115     C                   MOVEL     'TNSDU5R'     WPGM
034700930201     C                   EXSR      LANPGM
034800930128     C*
034900930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
035000930128     C                   Z-ADD     1             SWMODI
035100930128     C*
035200930805     C                   ENDIF                                                  <-- 1 ---
035300930128     C*
035400930128     C                   ENDSR
035500940105     C*================================================================
035600940105     C*  O09GES - GESTIONE OPZIONE 9=ANAGRAFICA
035700940105     C*================================================================
035800940105     C     O09GES        BEGSR
035900940105     C*
036000940105     C* LANCIO IL PGM PER VISUALIZZARE DETTAGLIO ANAGRAFICA
036100940105     C                   MOVEL     'O05'         WOPZ1                          *INTERROGAZIONE
036200941115     C                   MOVEL     'TNSDU2R'     WPGM                           *PGM DETT.ANAGRAFICA
036300940105     C                   EXSR      LANPGM
036400940105     C*
036500940105     C                   ENDSR
036600930205     C*================================================================
036700930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
036800930205     C*================================================================
036900930205     C     NEWPOS        BEGSR
037000930205     C*
037100930205     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
037200930205     C*         1= RICHIESTO POSIZIONAM.
037300930205     C                   Z-ADD     0             SWPOSI            7 0
037400930205     C*
037500930205     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037600930205     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037700930205     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037800930205     C                   Z-ADD     0             SWMODI            1 0
037900930205     C*
038000930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
038100930806     C     VCCTIP        IFNE      DEPTIP                                       --- 1 -->
038200930806     C     VCCCOD        ORNE      DEPCOD
038300941128     C     VCCMAT        ORNE      DEPMAT
038400930806     C     VCCMAG        ORNE      DEPMAG
038500930806     C     VCCDTM        ORNE      DEPDTM
038600930205     C*
038700930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
038800930205     C                   EXSR      CHKPAR
038900930205     C*
039000930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
039100930721     C     $ERR          IFEQ      '0'                                          --- 2 -->
039200930205     C                   Z-ADD     1             SWPOSI
039300930806     C                   MOVE      VCCTIP        DEPTIP
039400930806     C                   Z-ADD     VCCCOD        DEPCOD
039500941128     C                   MOVEL     VCCMAT        DEPMAT
039600930806     C                   MOVE      VCCMAG        DEPMAG
039700930806     C                   Z-ADD     VCCDTM        DEPDTM
039800930805     C                   ENDIF                                                  <-- 2 ---
039900930205     C*
040000930805     C                   ENDIF                                                  <-- 1 ---
040100930205     C*
040200930205     C                   ENDSR
040300000000     C*================================================================
040400000000     C* LETTURA RECORD FILE DA CARICARE
040500000000     C*================================================================
040600000000     C     LETFIL        BEGSR
040700000000     C*
040800930203     C* LETTURE SUCCESSIVE ALLA PRIMA
040900930712     C     WTIPRI        IFEQ      'COD'
041000930806     C                   EXSR      READ01
041100930805     C                   ENDIF
041200930212     C*
041300930712     C     WTIPRI        IFEQ      'MAG'
041400930806     C                   EXSR      READ02
041500930805     C                   ENDIF
041600000000     C*
041700110406     C     WTIPRI        IFEQ      'MAT'
041800110406     C                   EXSR      READ04
041900110406     C                   ENDIF
042000000000     C                   ENDSR
042100000000     C*================================================================
042200000000     C* SETFIL - SETLL DEL FILE DA CARICARE
042300000000     C*================================================================
042400000000     C     SETFIL        BEGSR
042500000000     C*
042600930202     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
042700930712     C     WTIPRI        IFEQ      'COD'
042800020325     C                   move      vCCTIP        UNMTIP
042900930806     C                   Z-ADD     VCCCOD        UNMCOD
043000930806     C                   Z-ADD     WCCDTM        UNMDTM
043100930712     C                   Z-ADD     *ZEROS        UNMPRG
043200931217     C                   MOVEL     *BLANKS       UNMTMO
043300020321     c                   if        unmdtm <> *zeros
043400100920     C     KEYUM3        SETLL     UNMOV03L                           99        *FINE-ARCHIV
043500020321     c                   else
043600100920     c     keyum11       setll     unmov03l                           99
043700020321     c                   endif
043800930806     C  N99              EXSR      READ01
043900930212     C                   END
044000110406     c
044100110406     C     WTIPRI        IFEQ      'MAT'
044200110406     C                   move      savTIP        UNMTIP
044300110406     C                   movel     VCCMAT        UNMMAT
044400110406     C                   Z-ADD     WCCDTM        UNMDTM
044500110406     c                   if        unmdtm <> *zeros
044600110406     C     KEYUM4        SETLL     UNMOV04L                           99        *FINE-ARCHIV
044700110406     c                   else
044800110406     c     keyum44       setll     unmov04l                           99
044900110406     c                   endif
045000110406     C  N99              EXSR      READ04
045100110406     C                   END
045200930212     C*
045300930712     C     WTIPRI        IFEQ      'MAG'
045400930806     C                   Z-ADD     WCCMAG        UNMMAG
045500930806     C                   Z-ADD     WCCDTM        UNMDTM
045600020403     c                   if        unmdtm <> *zeros
045700930712     C     KEYUM2        SETLL     UNMOV02L                           99        *FINE-ARCHIV
045800020403     c                   else
045900020403     c     keyum21       setll     unmov02l                           99
046000020403     c                   endif
046100930806     C  N99              EXSR      READ02
046200930212     C                   END
046300000000     C*
046400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
046500930203     C     *IN99         IFEQ      '1'
046600000000     C                   Z-ADD     1             SFLEOF
046700930203     C                   END
046800000000     C*
046900000000     C                   ENDSR
047000930806     C*================================================================
047100110406     C* READ01 - LEGGE RECORD SUCCESSIVO SU UNMOV03L
047200930806     C*================================================================
047300930806     C     READ01        BEGSR
047400930806     C*
047500930806     C                   MOVEL     '0'           RECOK             1
047600930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
047700930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
047800930806     C                   Z-ADD     80            LENGH            15 5
047900930806     C                   CALL      'QCAEXEC'
048000930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
048100930806     C                   PARM                    LENGH
048200930806     C                   ENDIF                                                  <-- 2 ---
048300930806     C                   MOVEL     '0'           RECOK             1
048400100920     C     keyum11       READe     UNMOV03L                             9899
048500930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
048600930806     C                   Z-ADD     1             SFLEOF
048700930806     C                   ELSE                                                   <-- 2 -->
048800020321     C**!!!UNMCOD        ifNE      VCCCOD
048900020321     C**!!!              Z-ADD     1             SFLEOF
049000020321     C**!!!              ENDIF                                                  <-- 3 ---
049100930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
049200930806     C                   MOVEL     '1'           RECOK
049300930806     C                   ENDIF
049400971014     C                   ENDIF                                                  <-- 2 ---
049500930806     C                   ENDDO                                                  <-- 1 ---
049600930806     C*
049700930806     C                   ENDSR
049800930806     C*================================================================
049900930806     C* READ02 - LEGGE RECORD SUCCESSIVO SU UNMOV02L
050000930806     C*================================================================
050100930806     C     READ02        BEGSR
050200930806     C*
050300930806     C                   MOVEL     '0'           RECOK
050400930806     C     RECOK         DOUEQ     '0'                                          --- 1 -->
050500930806     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
050600930806     C                   Z-ADD     80            LENGH            15 5
050700930806     C                   CALL      'QCAEXEC'
050800930806     C                   PARM                    CMD(2)                         *DSPRCDLCK
050900930806     C                   PARM                    LENGH
051000930806     C                   ENDIF                                                  <-- 2 ---
051100930806     C                   MOVEL     '0'           RECOK
051200020403     C     keyum21       READe     UNMOV02L                             9899
051300930806     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
051400930806     C                   Z-ADD     1             SFLEOF
051500930806     C                   ELSE                                                   <-- 2 -->
051600930806     C     UNMMAG        IFNE      WCCMAG                                       --- 3 -->
051700930806     C                   Z-ADD     1             SFLEOF
051800930806     C                   ELSE                                                   <-- 3 -->
051900930806     C     *IN98         IFEQ      '1'                                                    *FINE FILE
052000930806     C                   MOVEL     '1'           RECOK
052100930806     C                   ENDIF
052200930806     C                   ENDIF                                                  <-- 3 ---
052300930806     C                   ENDIF                                                  <-- 2 ---
052400930806     C                   ENDDO                                                  <-- 1 ---
052500930806     C*
052600930806     C                   ENDSR
052700110406     C*================================================================
052800110406     C* READ04 - LEGGE RECORD SUCCESSIVO SU UNMOV04L
052900110406     C*================================================================
053000110406     C     READ04        BEGSR
053100110406     C*
053200110406     C                   MOVEL     '0'           RECOK             1
053300110406     C     RECOK         DOUEQ     '0'                                          --- 1 -->
053400110406     C     *IN98         IFEQ      '1'                                          --- 2 --> *BLOCCATO
053500110406     C                   Z-ADD     80            LENGH            15 5
053600110406     C                   CALL      'QCAEXEC'
053700110406     C                   PARM                    CMD(2)                         *DSPRCDLCK
053800110406     C                   PARM                    LENGH
053900110406     C                   ENDIF                                                  <-- 2 ---
054000110406     C                   MOVEL     '0'           RECOK             1
054100110406     C     keyum44       READe     UNMOV04L                             9899
054200110406     C     *IN99         IFEQ      '1'                                          --- 2 --> *FINE FILE
054300110406     C                   Z-ADD     1             SFLEOF
054400110406     C                   ELSE                                                   <-- 2 -->
054500110406     C     *IN98         IFEQ      '1'                                                    *FINE FILE
054600110406     C                   MOVEL     '1'           RECOK
054700110406     C                   ENDIF
054800110406     C                   ENDIF                                                  <-- 2 ---
054900110406     C                   ENDDO                                                  <-- 1 ---
055000110406     C*
055100110406     C                   ENDSR
055200000000     C*================================================================
055300000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
055400000000     C*================================================================
055500000000     C     IRESFL        BEGSR
055600000000     C*
055700930806     C                   MOVEL     UNMATB        VSCATB
055800930806     C                   MOVE      UNMTIP        VSCTIP
055900930806     C                   Z-ADD     UNMCOD        VSCCOD
056000100903     a* Reperisco matricola
056100110407     c**                 z-add     unmtip        unatip
056200110407     C**                 Z-ADD     unmCOD        UNACOD
056300110407     C**   KEYUN1        CHAIN     UNANA01L                           99
056400110407     c**                 if        %found(unana01l)
056500110407     c**                 eval      vscmat=unamat
056600110407     c**                 else
056700110407     c**                 eval      vscmat=*all'?'
056800110407     c**                 endif
056900100903     c
057000110407     C                   movel     unmmat        vscmat
057100100903     C                   MOVE      UNMTIP        WTIP              2            !
057200930712     C                   EXSR      CHKTIP                                       !DECOD.TIPO UNITA
057300930806     C                   MOVEL     WDTIP         VSDTIP                         !
057400930806     C                   Z-ADD     UNMMAG        VSCMAG
057500930712     C                   MOVE      UNMMAG        WMAG                           !
057600930712     C                   EXSR      CHKMAG                                       !DECOD.MAGAZZINO
057700930806     C                   MOVEL     WDMAG         VSDMAG                         !
057800971010     C                   Z-ADD     UNMDTM        G08INV
057900971010     C                   Z-ADD     *ZEROS        G08DAT
058000971013     C                   MOVEL     '3'           G08ERR
058100971010     C                   CALL      'XSRDA8'
058200971010     C                   PARM                    WLBDA8
058300971010     C                   Z-ADD     G08DAT        VSCDTM
058400930806     C                   MOVE      UNMCAU        VSCCAU
058500930712     C                   MOVE      UNMCAU        WCAU              4            !
058600930712     C                   EXSR      CHKCAU                                       !DECOD.CAUSALE MOV.
058700100903     C**                 MOVEL     WDCAU         VSDCAU                         !
058800971013     C                   MOVEL     UNMTMO        VSCTMO
058900930806     C                   Z-ADD     UNMPRG        VSHPRG
059000000000     C*
059100000000     C                   ENDSR
059200020321      *================================================================
059300020321      *  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
059400020321      *================================================================
059500930205     C     CHKPAR        BEGSR
059600020321
059700020321      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
059800110406     C                   SETOFF                                       909192
059900020321
060000020321      * IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
060100930712     C                   SETOFF                                       808182
060200941128     C                   SETOFF                                       8384
060300020321
060400020321      * IMPOSTO A BLANKS DESCRIZIONI PARAMETRI DI INIZIO LISTA
060500930806     C                   MOVEL     *BLANKS       VCDMAG
060600110407     c                   clear                   savtip
060700020321
060800020321      * AZZERA CAMPI DI LAVORO NUMERICI
060900940104     C                   Z-ADD     *ZEROS        WCCMAG            3 0
061000971010     C                   Z-ADD     *ZEROS        WCCDTM            8 0
061100020321
061200020321      * SE UGUALE A ZEROS IMPOSTO BLANKS
061300930806     C     VCCTIP        IFEQ      *ZEROS
061400930806     C                   MOVEL     *BLANKS       VCCTIP
061500930805     C                   ENDIF
061600930806     C     VCCMAG        IFEQ      *ZEROS
061700930806     C                   MOVEL     *BLANKS       VCCMAG
061800930805     C                   ENDIF
061900020321
062000020321      * SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE 'COD'
062100110407    1C     VCCTIP        IFEQ      *BLANKS
062200930806     C     VCCCOD        ANDEQ     *ZEROS
062300941128     C     VCCMAT        ANDEQ     *BLANKS
062400941128     C     VCCMAG        ANDEQ     *BLANKS
062500930806     C     VCCDTM        ANDEQ     *ZEROS
062600930805     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
062700930212     C                   SETON                                        90
062800020321
062900110407   x1C                   ELSE
063000020321
063100020321      * ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
063200110407    2C     VCCTIP        IFNE      *BLANKS
063300930806     C     VCCCOD        ORNE      *ZEROS
063400110407     C     VCCMAT        ifne      *BLANKS
063500110407    3C     VCCMAG        orne      *BLANKS
063600930721     C                   MOVEL     MSG(7)        $MSG
063700930721     C                   EXSR      CARMSG
063800930721     C                   MOVEL     '1'           $ERR
063900110407     C                   SETON                                        808182
064000110407     C                   SETON                                        84
064100110407    3C                   ENDIF
064200110407    2C                   ENDIF
064300110407     C     VCCMAT        ifne      *BLANKS
064400110407    3C     VCCMAG        andne     *BLANKS
064500110407     C                   MOVEL     MSG(7)        $MSG
064600110407     C                   EXSR      CARMSG
064700110407     C                   MOVEL     '1'           $ERR
064800110407     C                   SETON                                        8482
064900110407    3C                   ENDIF
065000020321
065100110407    2C     $ERR          IFEQ      '0'
065200020321
065300020321      * CONTROLLO CHE TIPO DI RICERCA E' RICHIESTA
065400110407    3C     VCCTIP        IFNE      *BLANKS
065500930712     C                   MOVEL     'COD'         WTIPRI
065600930215     C                   SETON                                        90
065700020321      * SE E' PER COD, CONTROLLO CHE IL TIPO UNITA ESISTA
065800930806     C                   MOVE      VCCTIP        WTIP
065900930805     C                   EXSR      CHKTIP                                       *CONTROLLA TIPO UNIT
066000110407    4C     $ERR          IFEQ      '1'
066100930721     C                   MOVEL     '1'           $ERR
066200930721     C                   MOVEL     MSG(9)        $MSG
066300930721     C                   EXSR      CARMSG
066400930215     C                   SETON                                        80
066500110407     C                   ELSE
066600930806     C                   MOVE      WTIP          VCCTIP
066700110407    4C                   ENDIF
066800110407    3C                   ENDIF
066900020321
067000020321      * SE PER MATRICOLA CONTROLLO VADO A REPERIRE IL CODICE UNITA' E CONTROLLO
067100020321      * CHE LA MATRICOLA SIA UNIVOCA (TIP+MAT) SE NO ERRORE
067200110407IF  3C     VCCMAT        IFNE      *BLANKS
067300020321     c     vcctip        andne     *blanks
067400941128     C                   MOVEL     VCCTIP        UNATIP
067500941128     C                   MOVEL     VCCMAT        UNAMAT
067600941128     C     KEYUN2        SETLL     UNANA02L
067700941128     C     KEYUN2        READE     UNANA02L                               97
067800941128     C                   Z-ADD     *ZEROS        QTATEM            4 0          *QUANTITA' TIP+MAT <
067900110407DO  4C     *IN97         DOWEQ     *OFF                                         --- 4 -->
068000941128     C                   ADD       1             QTATEM
068100941128     C     KEYUN2        READE     UNANA02L                               97
068200110407E   4C                   ENDDO                                                  <-- 4 ---
068300020321
068400110406      * TIPO + MATRICOLA INESISTENTE
068500110407IF  4C     QTATEM        IFEQ      *ZEROS                                       --- 4 -->
068600110407     c* non segnalo errore se trovo almeno un movimenti
068700110407     C                   MOVEL     VCCTIP        UNmTIP
068800110407     C                   MOVEL     VCCMAT        UNmMAT
068900110407     c
069000110407     C     KEYUm44       SETLL     UNmov04L
069100110407    5C                   if        %equal(unmov04l)                             --- 4 -->
069200110407     C                   ADD       1             QTATEM
069300110407    5C                   endif                                                  <-- 4 ---
069400110407E   4C                   endif                                                  <-- 4 ---
069500110407     c
069600110407IF  4C     QTATEM        IFEQ      *ZEROS
069700941128     C                   MOVEL     '1'           $ERR
069800941128     C                   MOVEL     MSG(17)       $MSG
069900941128     C                   EXSR      CARMSG
070000941128     C                   SETON                                        8480
070100110407X   4C                   ELSE
070200020321
070300020321      * TIPO + MATRICOLA NON UNIVOCA
070400110407IF  5C     QTATEM        IFGT      1
070500941128     C                   MOVEL     '1'           $ERR
070600941128     C                   MOVEL     MSG(18)       $MSG
070700941128     C                   EXSR      CARMSG
070800941128     C                   SETON                                        8480
070900110407X   5C                   ELSE
071000110406     C                   MOVEL     'MAT'         WTIPRI
071100110406     c                   setoff                                       90
071200110406     c                   seton                                        92
071300110406     C***                Z-ADD     UNACOD        VCCCOD
071400110406     C                   movel     vcctip        savtip
071500110406     C***                MOVEL     *BLANKS       VCCMAT
071600110407E   5C                   ENDIF
071700110407E   4C                   ENDIF
071800110407E   3C                   ENDIF
071900020320
072000020321      * per sola matricola cerco il codice unità sull'anagrafico unità
072100020321      * e verifico che sia univoca altrimenti errore
072200110407    3c                   if        vcctip = *blanks and vccmat <> *blanks
072300020321     c                   clear                   qtatem
072400110406     c***                eval      wtipri = 'COD'
072500110406     c                   eval      wtipri = 'MAT'
072600110406     c                   eval      *in92 = *on
072700020321     c     vccmat        setll     unana07l
072800020321      * se non esiste matricola errore
072900110407      *  verifico se almeno presente nei movimenti
073000110407      *
073100110407    4c                   if        not %equal(unana07l)
073200110407     c     vccmat        setll     unmov04l
073300110407    5c                   if        not %equal(unmov04l)
073400110407     c
073500020321     c                   eval      $err = *on
073600020321     c                   eval      $msg = msg(19)
073700020321     c                   exsr      carmsg
073800020321     c                   eval      *in84 = *on
073900110407   x5c                   else
074000110407     c*
074100110407     c                   exsr      ctrMov
074200110407    5c                   endif
074300110407     c
074400020321      * controllo se univoca
074500110407   x4c                   else                                                   <-- 4 -->
074600020321     c                   do        *hival                                       --- 5 -->
074700020321     c     vccmat        reade     unana07l
074800020321      * fine file
074900020321     c                   if        %eof(unana07l)                               --- 6 -->
075000020321     c                   leave
075100020321     c                   endif                                                  <-- 6 ---
075200020321     c                   if        qtatem = *zeros                              --- 6 -->
075300020321     c                   add       1             qtatem
075400110406     C                   Z-ADD     unatip        savtip
075500110406     c**                 eval      vcccod = unacod
075600110406     c**                 movel     unatip        vcctip
075700110406     c**                 clear                   vccmat
075800020321     c                   else                                                   <-- 6 -->
075900020321     c                   eval      $err = *on
076000020321     c                   eval      $msg = msg(20)
076100020321     c                   exsr      carmsg
076200020321     c                   eval      *in84 = *on
076300020321     c                   leave
076400020321     c                   endif                                                  <-- 6 ---
076500020321     c                   enddo                                                  <-- 5 ---
076600020321     c                   endif                                                  <-- 4 ---
076700020321     c                   endif                                                  <-- 3 ---
076800020321
076900930806     C     VCCMAG        IFNE      *BLANKS
077000930712     C                   MOVEL     'MAG'         WTIPRI
077100930212     C                   SETON                                        91
077200020321      * SE E' PER MAG, CONTROLLO CHE ESISTA
077300930806     C                   MOVE      VCCMAG        WMAG
077400930805     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
077500930721     C     $ERR          IFEQ      '1'
077600930721     C                   MOVEL     '1'           $ERR
077700930721     C                   MOVEL     MSG(8)        $MSG
077800930721     C                   EXSR      CARMSG
077900930712     C                   SETON                                        82
078000930712     C                   ELSE
078100930806     C                   MOVE      WMAG          VCCMAG
078200940104     C                   MOVE      WMAG          WCCMAG
078300930806     C                   MOVEL     WDMAG         VCDMAG
078400930805     C                   ENDIF
078500930805     C                   ENDIF
078600020321
078700930805     C                   ENDIF                                                  <-- 2 ---
078800930805     C                   ENDIF                                                  <-- 1 ---
078900020321
079000020321      * CONTROLLO LA DATA DI INIZIO
079100930806     C     VCCDTM        IFNE      *ZEROS
079200971010     C                   Z-ADD     VCCDTM        WDAT              8 0
079300930805     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
079400930805     C     $ERR          IFEQ      '1'                                          *DATA ERRATA
079500930712     C                   SETON                                        83
079600930805     C                   ELSE                                                   *DATA OK
079700971010     C                   Z-ADD     VCCDTM        G08DAT
079800971010     C                   Z-ADD     *ZEROS        G08INV
079900971010     C                   MOVEL     '0'           G08ERR
080000971010     C                   CALL      'XSRDA8'
080100971010     C                   PARM                    WLBDA8
080200971013     C                   Z-ADD     G08DAT        VCCDTM
080300971013     C                   Z-ADD     G08INV        WCCDTM
080400930805     C                   ENDIF
080500930805     C                   ENDIF
080600020321
080700020321      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
080800020321      * CURSORE SUL CAMPO DEL CONTROL
080900930721     C     $ERR          IFEQ      '1'
081000930805     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
081100930715     C                   ELSE
081200930805     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
081300930715     C                   END
081400020321
081500930205     C                   ENDSR
081600110407      *================================================================
081700110407      *  controllo sse matricola presente nei movimenti
081800110407      *================================================================
081900110407     C     CtrMOV        BEGSR
082000110407     c
082100110407    1c                   do        *hival                                       --- 5 -->
082200110407     c     vccmat        reade     unmov04l
082300110407      * fine file
082400110407    2c                   if        %eof(unmov04l)                               --- 6 -->
082500110407     c                   leave
082600110407    2c                   endif                                                  <-- 6 ---
082700110407     c
082800110407    2c                   if        qtatem = *zeros                              --- 6 -->
082900110407     c                   add       1             qtatem
083000110407     C                   Z-ADD     unmtip        savtip
083100110407   x2c                   else                                                   <-- 6 -->
083200110407    3c                   if        unmtip<>savtip
083300110407     c                   eval      $err = *on
083400110407     c                   eval      $msg = msg(22)
083500110407     c                   exsr      carmsg
083600110407     c                   eval      *in84 = *on
083700110407     c                   leave
083800110407    3c                   endif                                                  <-- 6 ---
083900110407    2c                   endif                                                  <-- 6 ---
084000110407    1c                   enddo                                                  <-- 5 ---
084100110407     c
084200110407     C                   ENDSR
084300020321      *================================================================
084400020321      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
084500020321      *================================================================
084600930712     C     CHKTIP        BEGSR
084700020321
084800020321      * CONTROLLO SE E' STATO INSERITO '?'
084900020321     C     '?'           SCAN      WTIP                                   99
085000020321
085100020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
085200020321     c                   if        *in99                                        --- 1 -->
085300020321     c                   clear                   TIBS02DS
085400020321     c                   movel     'R'           t02mod
085500020321     c                   movel     knsif         t02sif
085600020321     c                   movel     'UNT'         t02cod
085700020321     c                   call      'TIBS02R'
085800020321     c                   parm                    KPJBA
085900020321     c                   parm                    TIBS02DS
086000020321     c                   movel     T02ke1        wtip
086100020321     c                   movel     t02uni        wdtip            30
086200020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
086300020321     c                   endif                                                  <-- 1 ---
086400020321
086500020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
086600020321     c                   clear                   TIBS02DS
086700020321     c                   movel     'C'           T02mod
086800020321     c                   movel     knsif         t02sif
086900020321     c                   movel     'UNT'         t02cod
087000020322     c                   movel(p)  wtip          T02ke1
087100020321     c                   call      'TIBS02R'
087200020321     c                   parm                    KPJBA
087300020321     c                   parm                    TIBS02DS
087400020321     c                   if        t02err <> *blanks
087500020321     c                   eval      $err = '1'
087600020321     c                   clear                   wdtip
087700020321     c                   else
087800020321     c                   movel     t02uni        wdtip
087900020321     c                   endif
088000020321
088100930712     C                   ENDSR
088200020321      *================================================================
088300020321      *  CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
088400020321      *================================================================
088500930712     C     CHKCAU        BEGSR
088600020321
088700020321      * CONTROLLO SE E' STATO INSERITO '?'
088800020321     C     '?'           SCAN      WCAU                                   99
088900020321
089000020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
089100020321     c                   if        *in99                                        --- 1 -->
089200020321     c                   clear                   TIBS02DS
089300020321     c                   movel     'R'           t02mod
089400020321     c                   movel     knsif         t02sif
089500020321     c                   movel     'UNM'         t02cod
089600020321     c                   call      'TIBS02R'
089700020321     c                   parm                    KPJBA
089800020321     c                   parm                    TIBS02DS
089900020321     c                   movel     T02ke1        wcau
090000020321     c                   movel     t02uni        wdcau            30
090100020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
090200020321     c                   endif                                                  <-- 1 ---
090300020321
090400020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
090500020321     c                   clear                   TIBS02DS
090600020321     c                   movel     'C'           T02mod
090700020321     c                   movel     knsif         t02sif
090800020321     c                   movel     'UNM'         t02cod
090900020321     c                   movel(p)  wcau          T02ke1
091000020321     c                   call      'TIBS02R'
091100020321     c                   parm                    KPJBA
091200020321     c                   parm                    TIBS02DS
091300020321     c                   if        t02err <> *blanks
091400020321     c                   eval      $err = '1'
091500020321     c                   clear                   wdcau
091600020321     c                   else
091700020321     c                   movel     t02uni        wdcau
091800020321     c                   endif
091900020321
092000930712     C                   ENDSR
092100930712     C*================================================================
092200930712     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
092300930712     C*================================================================
092400930712     C     CHKMAG        BEGSR
092500930712     C*
092600930712     C* CONTROLLO SE E' STATO INSERITO '?'
092700930712     C     '?'           SCAN      WMAG          A                 1 0    99
092800930712     C*
092900930712     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
093000930712     C     *IN99         IFEQ      '1'                                          --- 1 -->
093100930712     C                   MOVEL     *BLANKS       §COD1             3
093200930712     C                   MOVE      *BLANKS       §DES1
093300941115     C                   CALL      'TNSD20R'
093400930712     C                   PARM                    §COD1
093500930712     C                   PARM                    §TIP              1
093600930712     C                   PARM                    §DES1            25
093700930712     C                   MOVEL     §COD1         WMAG              3
093800930712     C                   MOVEL     §DES1         WDMAG            30
093900930714     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
094000930712     C*
094100930805     C                   ENDIF                                                  <-- 1 ---
094200930712     C*
094300930712     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
094400930712     C                   MOVE      WMAG          ORGFIL
094500020529     C     KEYORG        CHAIN     AZORG21l                           99
094600930712     C     *IN99         IFEQ      '0'                                          --- 2 -->
094700930712     C                   MOVEL     ORGFIL        WMAG
094800930712     C                   MOVEL     ORGDES        WDMAG
094900930712     C                   ELSE                                                   <-- 2 -->
095000020529     c     keyorg        chain     azorg01l
095100020529     c                   if        %found(azorg01l)
095200020529     c                   movel     orgfil        wmag
095300020529     c                   movel     orgdes        wdmag
095400020529     c                   else
095500930721     C                   MOVEL     '1'           $ERR
095600930712     C                   MOVEL     *ALL'*'       WDMAG
095700020529     c                   endif
095800930805     C                   ENDIF                                                  <-- 2 ---
095900930712     C*
096000930712     C                   ENDSR
096100930712     C*================================================================
096200930712     C* CHKDAT - CONTROLLA LA DATA
096300930712     C*================================================================
096400930712     C     CHKDAT        BEGSR
096500930712     C*
096600971010     C                   Z-ADD     WDAT          G08DAT
096700971010     C                   Z-ADD     *ZEROS        G08INV
096800971010     C                   MOVEL     '0'           G08ERR
096900971010     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
097000971010     C                   PARM                    WLBDA8
097100971013     C                   Z-ADD     G08DAT        WDAT
097200971010     C     G08ERR        IFNE      '0'
097300930721     C                   MOVEL     '1'           $ERR
097400930721     C                   MOVEL     MSG(10)       $MSG
097500930721     C                   EXSR      CARMSG
097600930805     C                   ENDIF
097700930712     C*
097800930712     C                   ENDSR
097900930201     C*================================================================
098000930201     C* LANPGM - LANCIA UN PROGRAMMA
098100930201     C*================================================================
098200930201     C     LANPGM        BEGSR
098300930201     C*
098400930201     C* IMPOSTO PARAMETRI PER IL PGM
098500930712     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
098600930712     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
098700930712     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
098800930712     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
098900930806     C                   Z-ADD     VSCTIP        DSUTIP                         *TIPO
099000930806     C                   Z-ADD     VSCCOD        DSUCOD                         *CODICE
099100930806     C                   Z-ADD     VSCMAG        DSUMAG                         *MAGAZZINO
099200930806     C                   Z-ADD     VSHPRG        DSUPRG                         *PROGR.MOV.
099300931217     C                   MOVEL     VSCTMO        DSUTMO                         *TIPO MOVIM.
099400971010     C                   Z-ADD     VSCDTM        G08DAT
099500971010     C                   Z-ADD     *ZEROS        G08INV
099600971010     C                   MOVEL     '0'           G08ERR
099700971010     C                   CALL      'XSRDA8'
099800971010     C                   PARM                    WLBDA8
099900971010     C                   Z-ADD     G08INV        DSUDTM                         *DATA MOVIMENTO
100000930201     C*
100100930201     C* LANCIO PROGRAMMA
100200940105     C                   MOVEL     DSUNIT        KPJBU
100300930312     C                   CALL      WPGM
100400940105     C                   PARM                    KPJBA
100500940105     C                   MOVEL     KPJBU         DSUNIT
100600930201     C*
100700930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
100800930712     C     DSUF03        IFEQ      '1'
100900930721     C                   MOVEL     '1'           $F03
101000930721     C                   MOVEL     '1'           $ERR
101100940105     C                   MOVEL     '1'           $FINE
101200930201     C                   ELSE
101300930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
101400930721     C                   MOVEL     DSUMSG        $MSG
101500930721     C                   EXSR      CARMSG
101600930805     C                   ENDIF
101700930201     C*
101800930201     C                   ENDSR
101900000000     C*================================================================
102000930713     C*  *INZSR - ROUTINE INIZIALE
102100000000     C*================================================================
102200930713     C     *INZSR        BEGSR
102300000000     C*
102400000000     C* RICEVIMENTO PARAMETRI
102500000000     C     *ENTRY        PLIST
102600940105     C                   PARM                    KPJBA
102700940105     C                   MOVEL     KPJBU         DSUNIT
102800020321
102900020321      * CHIAVE LETTURA UNANA01L
103000931116     C     KEYUN1        KLIST
103100020322     C                   KFLD                    UNATIP
103200931116     C                   KFLD                    UNACOD
103300020321
103400020321      * CHIAVE LETTURA UNANA02L
103500941128     C     KEYUN2        KLIST
103600941128     C                   KFLD                    UNATIP
103700941128     C                   KFLD                    UNAMAT
103800020321
103900020321      * CHIAVE LETTURA UNMOV01L
104000930712     C     KEYUM1        KLIST
104100020322     C                   KFLD                    UNMTIP
104200930712     C                   KFLD                    UNMCOD
104300930712     C                   KFLD                    UNMDTM
104400930712     C                   KFLD                    UNMPRG
104500931217     C                   KFLD                    UNMTMO
104600020322
104700100920      * CHIAVE LETTURA UNMOV03L
104800020322     C     KEYUM11       KLIST
104900020322     C                   KFLD                    UNMTIP
105000020322     C                   KFLD                    UNMCOD
105100100920     C     KEYUM3        KLIST
105200100920     C                   KFLD                    UNMTIP
105300100920     C                   KFLD                    UNMCOD
105400100920     C                   KFLD                    UNMDTM
105500110406     C     KEYUM4        KLIST
105600110406     C                   KFLD                    UNMMAT
105700110406     C                   KFLD                    UNMTIP
105800110406     C                   KFLD                    UNMDTM
105900110406     C     KEYUM44       KLIST
106000110406     C                   KFLD                    UNMMAT
106100110406     C                   KFLD                    UNMTIP
106200020321
106300020321      * CHIAVE LETTURA UNMOV02L
106400930712     C     KEYUM2        KLIST
106500930712     C                   KFLD                    UNMMAG
106600930712     C                   KFLD                    UNMDTM
106700020403      * CHIAVE LETTURA UNMOV02L
106800020403     C     KEYUM21       KLIST
106900020403     C                   KFLD                    UNMMAG
107000930310     C*
107100020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
107200930310     C     KEYORG        KLIST
107300930712     C                   KFLD                    ORGFIL
107400000000     C*
107500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
107600971014     C     *LIKE         DEFINE    VCCTIP        DEPTIP
107700930806     C     *LIKE         DEFINE    VCCCOD        DEPCOD
107800941128     C     *LIKE         DEFINE    VCCMAT        DEPMAT
107900930806     C     *LIKE         DEFINE    VCCMAG        DEPMAG
108000930806     C     *LIKE         DEFINE    VCCDTM        DEPDTM
108100971014     C*
108200971014     C* PARAMETRI DI ENTRATA IN DEPOSITI
108300971014     C     *LIKE         DEFINE    DSUPRO        DEPPRO
108400971014     C     *LIKE         DEFINE    DSUOP0        DEPOP0
108500971014     C                   MOVEL     DSUPRO        DEPPRO
108600971014     C                   MOVEL     DSUOP0        DEPOP0
108700940105     C*
108800940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DELL'ANAGRAF. UNITA'
108900940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
109000940105     C     DSUPRO        IFEQ      'A'
109100940105     C                   MOVEL     OPZ(9)        OPZRIG                         *GEST.MOVIMENTI
109200940105     C                   MOVEL     'N'           OPZASG
109300940105     C                   MOVEL     'N'           OPZASI
109400940105     C                   MOVEL     'N'           OPZAFG
109500940105     C                   MOVEL     'N'           OPZAFI
109600940105     C                   MOVEL     OPZRIG        OPZ(9)
109700940105     C                   END
109800930714     C*
109900930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
110000930803     C                   EXSR      INZSTD
110100930803     C*
110200930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
110300930803     C                   EXSR      INZSFL
110400930714     C*
110500930806     C* IMPOSTO CAMPI TESTATA VIDEO
110600940105     C     DSUOP0        IFEQ      ' 2'
110700930806     C                   MOVEL     '    GEST'    VTDING
110800930806     C                   MOVE      'IONE    '    VTDING
110900930923     C                   MOVEL     'G'           $TGES
111000930714     C                   ELSE
111100930806     C                   MOVEL     ' INTERRO'    VTDING
111200930806     C                   MOVE      'GAZIONE '    VTDING
111300930923     C                   MOVEL     'I'           $TGES
111400930805     C                   ENDIF
111500931116     C*
111600930713     C*
111700930720     C* SE NON SONO STATI PASSATI PARAMETRI IN INPUT EMETTO VIDEATA PARAMETRI
111800930805     C     DSUTIP        IFEQ      *ZEROS                                       --- 1 -->
111900930806     C                   MOVEL     *BLANKS       VCCTIP
112000930806     C                   Z-ADD     *ZEROS        VCCCOD
112100930806     C                   MOVEL     *BLANKS       VCCMAG
112200930806     C                   Z-ADD     *ZEROS        VCCDTM
112300931116     C*
112400931116     C* SE PASSATI CONTROLLO I PARAMETRI IN INPUT AL PGM
112500930805     C                   ELSE                                                   <-- 1 -->
112600020322     c                   z-add     dsutip        unatip
112700931116     C                   Z-ADD     DSUCOD        UNACOD
112800931116     C     KEYUN1        CHAIN     UNANA01L                           99
112900931116     C*
113000931116     C* SE ANAGRAFICA ANNULLATA, PERMETTO SOLO LA VISUALIZZAZIONE
113100931116     C     *IN99         IFEQ      *OFF
113200931116     C     UNAATB        ANDEQ     'A'
113300931116     C                   MOVEL     ' INTERRO'    VTDING
113400931116     C                   MOVE      'GAZIONE '    VTDING
113500931116     C                   MOVEL     'I'           $TGES
113600931116     C                   ENDIF
113700971014     C                   ENDIF                                                  <-- 1 ---
113800931116     C*
113900971014     C                   MOVE      DSUTIP        VCCTIP                         *VIDEO
114000930806     C                   Z-ADD     DSUCOD        VCCCOD
114100930806     C                   MOVEL     *BLANKS       VCCMAG
114200930806     C                   Z-ADD     *ZEROS        VCCDTM
114300100910     c*
114400100910     c                   if        dsudtm>0
114500100910     c                   eval      dataiso=%date(dsudtm)
114600100910     c                   eval      dataeur=dataiso
114700100910     c                   eval      vccdtm=%dec(dataeur)
114800100910     c                   endif
114900971014     C                   MOVE      VCCTIP        DEPTIP                         *DEPOSITI
115000971014     C                   Z-ADD     VCCCOD        DEPCOD
115100971014     C                   MOVE      VCCMAG        DEPMAG
115200971014     C                   Z-ADD     VCCDTM        DEPDTM
115300930805     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
115400930721     C     $ERR          IFEQ      '1'
115500930712     C                   MOVEL     '1'           DSUERR
115600930721     C                   MOVEL     $MSG          DSUMSG
115700931015     C                   MOVEL     '1'           $FINE
115800930805     C                   ENDIF
115900940104     C*
116000940104     C* EMETTO VIDEATA SUBFILE
116100971014     C                   MOVEL     'COD'         WTIPRI            3            *TIPO RIC. PER CODIC
116200930806     C                   EXSR      CARVS                                        *CARICA VIDEO SUBFIL
116300000000     C*
116400000000     C                   ENDSR
116500971014     C*================================================================
116600971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
116700971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
116800930805     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
116900930805     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
117000930805     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
117100971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
117200971014     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
117300930714     C*================================================================
117400930803     C* INZSTD - ROUTINE INIZIALE STANDARD
117500930714     C*================================================================
117600930803     C     INZSTD        BEGSR
117700930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
117800930802     C                   CALL      'XKEYS'
117900930802     C                   PARM                    DSKEYS
118000930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
118100930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
118200930714     C     XMSGPA        PLIST
118300930714     C                   PARM                    DSMSMS
118400930714     C                   PARM                    DSMSPG
118500930714     C                   PARM                    DSMSKE
118600930714     C                   PARM                    DSMSER
118700930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
118800941115     C                   MOVEL     'TNSDU4R'     DSMSPG                         *NOME PGM CHIAMANTE
118900941115     C                   MOVEL     'TNSDU4R'     VMCQUE                         *CODA MESSAGGI DESTI
119000930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
119100930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
119200930805     C* INIZIALIZZO FLAG CONTROLLO PGM
119300930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
119400930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
119500930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
119600930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
119700930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
119800930721     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
119900930923     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
120000930714     C* LANCIO ROUTINE PER VERIFICARE SE
120100930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
120200930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
120300020709
120400020709     c     *dtaara       define    §azute        azuteds
120500020709     c     *dtaara       define    §datiute      ddatiute
120600020709
120700020709     c                   in(E)     *dtaara
120800020709     c                   if        %Error or rsut = *blanks
120900020709     c                   clear                   tibs34ds
121000020709     c                   call      'TIBS34R'
121100020709     c                   parm                    Tibs34Ds
121200020709     c                   in        *dtaara
121300020709     c                   endif
121400020717
121500020717      * Controllo il profilo utente
121600020717     c                   ExSr      CtrUte
121700020717
121800930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
121900930721     C                   BITOFF    '01234567'    POSMI1            1
122000930721     C                   BITOFF    '01234567'    POSMI2            1
122100930721     C                   BITOFF    '01234567'    POSMA1            1
122200930721     C                   BITOFF    '01234567'    POSMA2            1
122300930721     C                   BITON     '34'          POSMI1            1            *18
122400930721     C                   BITON     '7'           POSMI2            1            *01
122500930721     C                   BITON     '34'          POSMA1            1            *18
122600930721     C                   BITON     '0'           POSMA2            1            *80
122700930721     C                   MOVE      POSMI2        POSMIN            2
122800930721     C                   MOVEL     POSMI1        POSMIN
122900930721     C                   MOVE      POSMA2        POSMAX            2
123000930721     C                   MOVEL     POSMA1        POSMAX
123100930714     C                   ENDSR
123200020717      *------------------------------------------------------------------------*
123300020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
123400020717      *------------------------------------------------------------------------*
123500020717     c     CtrUte        BegSr
123600020717
123700020717     c                   Eval      *In76 = *Off
123800020717
123900020717      * Controllo se il profilo è ok da tibs34
124000020717     c                   If        DutErr = 'E'
124100020717     c                   eval      $Msg = DutMsg
124200020717     c                   ExSr      CarMsg
124300020717     c                   eval      *In76 = *On
124400020717     c                   endIf
124500020717
124600020717      * Controllo se il profilo di sede
124700020717     c                   If        DutLpo <> 'S'
124800020717     c                   Eval      $Msg = Msg(21)
124900020717     c                   ExSr      CarMsg
125000020717     c                   Eval      *In76 = *On
125100020717     c                   EndIf
125200020717
125300020717     c                   endSr
125400930804     C*================================================================
125500930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
125600930804     C*================================================================
125700930804     C     FUNSTD        BEGSR
125800930804     C                   SELECT
125900930804     C     KEY           WHENEQ    F3
126000930804     C                   EXSR      F03STD
126100930804     C     KEY           WHENEQ    F12
126200930804     C                   EXSR      F12STD
126300930804     C     KEY           WHENEQ    F24
126400930804     C                   EXSR      F24STD
126500930804     C                   OTHER
126600930804     C                   MOVEL     MSG(2)        $MSG
126700930804     C                   EXSR      CARMSG
126800930804     C                   ENDSL
126900930804     C                   ENDSR
127000930804     C*================================================================
127100930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
127200930804     C*================================================================
127300930804     C     F03STD        BEGSR
127400930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
127500930923     C                   MOVEL     '1'           $F03
127600930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
127700930804     C                   MOVEL     '1'           $FINE
127800930804     C                   ENDSR
127900930804     C*================================================================
128000930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
128100930804     C*================================================================
128200930804     C     F12STD        BEGSR
128300930805     C* IMPOSTO FLAG DI FIN PGM
128400930804     C                   MOVEL     '1'           $FINE
128500930804     C                   ENDSR
128600930804     C*================================================================
128700930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
128800930804     C*================================================================
128900930804     C     F24STD        BEGSR
129000930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
129100930804     C                   EXSR      OUTFUN
129200930804     C                   ENDSR
129300930804     C*================================================================
129400930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
129500930804     C*================================================================
129600930804     C     INZSFL        BEGSR
129700930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
129800930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
129900930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
130000930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
130100930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
130200930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
130300930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
130400930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
130500930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
130600930804     C                   Z-ADD     12            SFLPAG
130700930804     C                   ENDSR
130800930804     C*================================================================
130900930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
131000930804     C*================================================================
131100930804     C     FUNSFL        BEGSR
131200930804     C                   SELECT
131300930804     C     KEY           WHENEQ    ENTER
131400930804     C                   EXSR      ENTSFL
131500930805     C     KEY           WHENEQ    F3
131600930805     C                   EXSR      F03STD
131700930804     C     KEY           WHENEQ    F5
131800930804     C                   EXSR      F05STD
131900930805     C     KEY           WHENEQ    F12
132000930805     C                   EXSR      F12STD
132100930804     C     KEY           WHENEQ    F17
132200930804     C                   EXSR      F17STD
132300930804     C     KEY           WHENEQ    F18
132400930804     C                   EXSR      F18STD
132500930804     C     KEY           WHENEQ    F23
132600930804     C                   EXSR      F23STD
132700930805     C     KEY           WHENEQ    F24
132800930805     C                   EXSR      F24STD
132900930804     C     KEY           WHENEQ    ROLLUP
133000930804     C                   EXSR      RUPSTD
133100930804     C     KEY           WHENEQ    ROLLDN
133200930804     C                   EXSR      RDNSTD
133300930804     C                   OTHER
133400930804     C                   MOVEL     MSG(2)        $MSG
133500930804     C                   EXSR      CARMSG
133600930804     C                   ENDSL
133700930804     C                   ENDSR
133800930804     C*================================================================
133900930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
134000930804     C*================================================================
134100930804     C     F05STD        BEGSR
134200930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
134300930804     C                   EXSR      POSSFL
134400930804     C                   ENDSR
134500930804     C*================================================================
134600930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
134700930804     C*================================================================
134800930804     C     F17STD        BEGSR
134900930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
135000930806     C                   Z-ADD     1             VCCNBR
135100930804     C                   ENDSR
135200930804     C*================================================================
135300930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
135400930804     C*================================================================
135500930804     C     F18STD        BEGSR
135600930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
135700930806     C                   Z-ADD     SFLULT        VCCNBR
135800930804     C                   ENDSR
135900930804     C*================================================================
136000930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
136100930804     C*================================================================
136200930804     C     F23STD        BEGSR
136300930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
136400930804     C                   EXSR      OUTOPZ
136500930804     C                   ENDSR
136600930804     C*================================================================
136700930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
136800930804     C*================================================================
136900930804     C     RUPSTD        BEGSR
137000930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
137100930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
137200930805     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
137300930805     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
137400930804     C                   ELSE
137500930805     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
137600930805     C                   ENDIF
137700930804     C                   ENDSR
137800930804     C*================================================================
137900930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
138000930804     C*================================================================
138100930804     C     RDNSTD        BEGSR
138200930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
138300930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
138400930804     C     CURSOR        IFGE      POSMIN
138500930804     C     CURSOR        ANDLE     POSMAX
138600930804     C                   ELSE
138700930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
138800930804     C                   MOVEL     '1'           $ERR
138900930804     C                   MOVEL     MSG(5)        $MSG
139000930804     C                   EXSR      CARMSG
139100930805     C                   ENDIF
139200930804     C                   ENDSR
139300930804     C*================================================================
139400930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
139500930804     C*================================================================
139600930804     C     ENTSFL        BEGSR
139700930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
139800930804     C                   EXSR      NEWPOS
139900930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
140000930804     C     $ERR          IFEQ      '0'                                          --- 1 -->
140100930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
140200930804     C     SWPOSI        IFNE      0                                            --- 2 -->
140300930804     C                   EXSR      POSSFL
140400930804     C                   ELSE                                                   <-- 2 -->
140500930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
140600930804     C     SFLULT        IFNE      0                                            --- 3 -->
140700930804     C                   EXSR      OPZSFL
140800930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
140900930804     C     SWMODI        IFNE      0                                            --- 4 -->
141000930804     C                   EXSR      POSSFL
141100930805     C                   ENDIF                                                  <-- 4 ---
141200930805     C                   ENDIF                                                  <-- 3 ---
141300930805     C                   ENDIF                                                  <-- 2 ---
141400930805     C                   ENDIF                                                  <-- 1 ---
141500930804     C                   ENDSR
141600930804     C*================================================================
141700930804     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
141800930804     C*          CARICARE
141900930804     C*================================================================
142000930804     C     POSSFL        BEGSR
142100930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
142200930804     C                   EXSR      CLRSFL
142300930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
142400930804     C                   EXSR      SETFIL
142500930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
142600930804     C     SFLEOF        IFEQ      0
142700930804     C                   EXSR      CARSFL
142800930805     C                   ENDIF
142900930804     C                   ENDSR
143000000000     C*================================================================
143100930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
143200000000     C*================================================================
143300930804     C     OPZSFL        BEGSR
143400930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
143500930802     C                   MOVEL     'S'           OPZOK             1
143600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
143700000000     C                   Z-ADD     0             SFLVAR            5 0
143800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
143900000000     C                   Z-ADD     0             SFLERR
144000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
144100000000     C                   Z-ADD     SFLULT        WFLULT
144200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
144300941115     C                   READC     SDU4DVS                                49
144400930806     C     *IN49         DOWEQ     '0'                                          --- 1 -->
144500000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
144600000000     C                   ADD       1             SFLVAR
144700930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
144800930805     C     OPZOK         IFEQ      'S'                                          --- 2 -->
144900930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
145000930806     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
145100930802     C                   EXSR      CONOPZ
145200930802     C     OPZOK         IFEQ      'S'
145300930802     C                   EXSR      GESOPZ
145400930805     C                   ENDIF
145500930805     C                   ENDIF                                                  <-- 3 ---
145600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
145700000000     C*            2. PULIZIA SCELTA IN SFL
145800930805     C     OPZOK         IFEQ      'S'                                          --- 3 -->
145900000000     C                   MOVEL     '0'           *IN41
146000930806     C                   MOVEL     *BLANKS       VSCOPZ
146100930805     C                   ELSE                                                   <-- 3 -->
146200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
146300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
146400000000     C                   MOVEL     '1'           *IN41
146500000000     C                   Z-ADD     SFLULT        SFLERR
146600930805     C                   ENDIF                                                  <-- 3 ---
146700930805     C                   ELSE                                                   <-- 2 -->
146800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
146900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
147000000000     C                   MOVEL     '1'           *IN41
147100930805     C                   ENDIF                                                  <-- 2 ---
147200000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
147300000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
147400000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
147500930803     C     OPZOK         IFNE      'S'
147600930806     C                   Z-ADD     SFLERR        VCCNBR
147700000000     C                   ELSE
147800930806     C                   Z-ADD     SFLULT        VCCNBR
147900930805     C                   ENDIF
148000000000     C* AGGIORNO RECORD SFL
148100941115     C                   UPDATE    SDU4DVS
148200941115     C                   READC     SDU4DVS                                49
148300930805     C                   ENDDO                                                  <-- 1 ---
148400000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
148500000000     C                   Z-ADD     WFLULT        SFLULT
148600000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
148700000000     C     SFLVAR        IFEQ      0
148800020404     C**!!!              MOVEL     '1'           $FINE
148900930805     C                   ENDIF
149000000000     C                   ENDSR
149100930805     C*================================================================
149200930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
149300000000     C*================================================================
149400000000     C     CLRSFL        BEGSR
149500000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
149600000000     C* (0=NON FINE FILE  1=FINE FILE)
149700000000     C                   Z-ADD     0             SFLEOF
149800000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
149900000000     C                   Z-ADD     0             SFLULT
150000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
150100000000     C                   MOVEL     '1'           *IN40
150200941115     C                   WRITE     SDU4DVC
150300000000     C                   MOVEL     '0'           *IN40
150400000000     C                   ENDSR
150500000000     C*================================================================
150600000000     C* CARSFL - CARICAMENTO SFL
150700000000     C*================================================================
150800000000     C     CARSFL        BEGSR
150900000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
151000000000     C     SFLULT        IFEQ      0
151100000000     C                   EXSR      SETFIL
151200930805     C                   ENDIF
151300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
151400000000     C                   Z-ADD     SFLULT        WFLULT
151500000000     C* CICLO:
151600000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
151700000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
151800000000     C                   Z-ADD     1             SFLRIG
151900930805     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
152000000000     C     SFLEOF        ANDEQ     0
152100000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
152200930806     C                   MOVEL     *BLANKS       VSCOPZ
152300000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
152400000000     C                   EXSR      IRESFL
152500000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
152600000000     C                   ADD       1             SFLULT
152700000000     C* SCRITTURA RECORD SFL
152800941115     C                   WRITE     SDU4DVS
152900000000     C                   ADD       1             SFLRIG
153000000000     C* LETTURA FILE DA CARICARE
153100000000     C                   EXSR      LETFIL
153200930805     C                   ENDDO                                                  <-- 1 ---
153300000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
153400000000     C     WFLULT        IFNE      SFLULT
153500930806     C                   Z-ADD     WFLULT        VCCNBR
153600930806     C                   ADD       1             VCCNBR
153700930805     C                   ENDIF
153800930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
153900930805     C     SFLEOF        IFEQ      1                                            --- 1 -->
154000930721     C     SFLULT        ANDGT     *ZEROS
154100930721     C                   MOVEL     '1'           $ERR
154200930804     C                   MOVEL     MSG(6)        $MSG
154300930721     C                   EXSR      CARMSG
154400930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
154500020321     C**!!!SFLULT        CHAIN     SDU4DVS                            99
154600020321     C**!!!*IN99         IFEQ      '0'
154700020321     C**!!!              SETON                                        45
154800020321     C**!!!              UPDATE    SDU4DVS
154900020321     C**!!!              SETOFF                                       45
155000020321     C**!!!              ENDIF
155100930805     C                   ENDIF                                                  <-- 1 ---
155200000000     C                   ENDSR
155300000000     C*================================================================
155400930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
155500000000     C*================================================================
155600000000     C     OUTSFL        BEGSR
155700930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
155800930721     C     MSGULT        IFNE      *ZEROS
155900930721     C                   EXSR      OUTMSG
156000930805     C                   ENDIF
156100000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
156200000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
156300000000     C     SFLEOF        IFEQ      0
156400000000     C                   MOVEL     '0'           *IN42
156500000000     C                   ELSE
156600000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
156700000000     C                   MOVEL     '1'           *IN42
156800930805     C                   ENDIF
156900930720     C* EMISSIONE SUBFILE
157000000000     C     SFLULT        IFLE      0
157100930720     C*  --> CASO DI SFL VUOTO
157200930720     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
157300941115     C                   WRITE     SDU4DVV                                      *'NON VI SONO DATI'
157400941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
157500941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
157600000000     C                   ELSE
157700930720     C*  --> CASO DI SFL CON RECORDS
157800941115     C                   WRITE     SDU4DVC                                      *SFL CONTROL
157900941115     C                   READ      SDU4DVC                                99    *SFL CONTROL
158000930805     C                   ENDIF
158100930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
158200930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
158300930806     C                   Z-ADD     FRTSFL        VCCNBR
158400000000     C                   ENDSR
158500930721     C*================================================================
158600930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
158700930721     C*          CODA MESSAGGI DEL PGM
158800930721     C*================================================================
158900930721     C     CARMSG        BEGSR
159000930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
159100930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
159200930721     C     MSGULT        IFEQ      0
159300930721     C                   MOVEL     '1'           *IN40
159400941115     C                   WRITE     SDU4DVK
159500930721     C                   MOVEL     '0'           *IN40
159600930805     C                   ENDIF
159700930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
159800930721     C                   MOVEL     $MSG          DSMSMS
159900930721     C                   CALL      'XMSG'        XMSGPA
160000930721     C     DSMSER        IFEQ      0
160100930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
160200930805     C                   ENDIF
160300930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
160400931015     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
160500941115     C                   WRITE     SDU4DVM                                      *SFL MESS
160600930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
160700930721     C                   MOVEL     *BLANKS       $MSG
160800930721     C                   ENDSR
160900930721     C*================================================================
161000930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
161100930721     C*================================================================
161200930721     C     OUTMSG        BEGSR
161300930721     C* VISUALIZZAZIONE MESSAGGIO
161400930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
161500941115     C                   WRITE     SDU4DVK                                      *CONTR MESS
161600930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
161700930721     C                   Z-ADD     *ZEROS        MSGULT
161800930721     C                   ENDSR
161900930803     C*================================================================
162000930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
162100930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
162200930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
162300930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
162400930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
162500930803     C*================================================================
162600930803     C     CARFUN        BEGSR
162700930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
162800931015     C     1             DO        31            I                 5 0          --- 1 -->
162900930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
163000930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
163100930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
163200020709     c                   If        DutLpo = 'S'
163300930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
163400930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
163500930803     C                   ELSE                                                   *INTERROG.
163600930803     C                   MOVEL     FUNASI        FAT(I)
163700930803     C                   ENDIF
163800930803     C                   ELSE                                                   <-- 2 --> *FILIALE
163900930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
164000930803     C                   MOVEL     FUNAFG        FAT(I)
164100930803     C                   ELSE                                                   *INTERROG.
164200930803     C                   MOVEL     FUNAFI        FAT(I)
164300930803     C                   ENDIF
164400930803     C                   ENDIF                                                  <-- 2 ---
164500930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
164600930806     C                   MOVEL     USER          WEDP              3            *PROFILO
164700930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
164800930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
164900930806     C                   ELSE
165000930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
165100930806     C                   ENDIF
165200930806     C                   ENDIF
165300930803     C                   ENDDO                                                  <-- 1 ---
165400930803     C                   ENDSR
165500930803     C*================================================================
165600930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
165700930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
165800930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
165900930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
166000930803     C*================================================================
166100930803     C     OUTFUN        BEGSR
166200930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
166300930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
166400930803     C                   ENDDO
166500930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
166600930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
166700930803     C     1             DO        31            I
166800930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
166900930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
167000930803     C                   ADD       1             FUNNAT
167100930803     C                   ENDIF
167200930803     C                   ENDDO
167300930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
167400930803     C     FUNNAT        IFGT      8
167500930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
167600930803     C                   ENDIF
167700930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
167800930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
167900930803     C                   Z-ADD     8             FUNVIS            5 0
168000930803     C     FAT(23)       IFEQ      'S'
168100930803     C                   SUB       1             FUNVIS
168200930803     C                   ENDIF
168300930803     C     FAT(24)       IFEQ      'S'
168400930803     C                   SUB       1             FUNVIS
168500930803     C                   ENDIF
168600930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
168700931015     C                   Z-ADD     *ZEROS        W                 5 0
168800930803     C                   ADD       1             FUNULT
168900930803     C     FUNULT        IFGT      31
169000930803     C                   Z-ADD     1             FUNULT
169100930805     C                   ENDIF
169200930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
169300930803     C     FUNULT        DO        31            I                              --- 1 -->
169400930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
169500930803     C     W             IFLT      FUNVIS                                       --- 2 -->
169600930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
169700930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
169800930803     C     FDE(I)        ANDNE     *BLANKS
169900930805     C     I             IFEQ      23                                           *TOLGO F23
170000930805     C     I             OREQ      24                                           *E F24
170100930803     C                   ELSE
170200930803     C                   ADD       1             W
170300930803     C                   MOVEL     FDE(I)        FUV(W)
170400930803     C                   ENDIF
170500930803     C                   ENDIF                                                  <-- 3 ---
170600930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
170700930803     C                   Z-ADD     I             FUNULT
170800930803     C                   ENDIF                                                  <-- 2 ---
170900930803     C                   ENDDO                                                  <-- 1 ---
171000930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
171100930806     C                   MOVEL     FUV(1)        VFD001
171200930806     C                   MOVEL     FUV(2)        VFD002
171300930806     C                   MOVEL     FUV(3)        VFD003
171400930806     C                   MOVEL     FUV(4)        VFD004
171500930806     C                   MOVEL     FUV(5)        VFD005
171600930806     C                   MOVEL     FUV(6)        VFD006
171700930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
171800930803     C     FAT(24)       ANDEQ     'N'
171900930806     C                   MOVEL     FUV(7)        VFD007
172000930806     C                   MOVEL     FUV(8)        VFD008
172100930803     C                   ELSE                                                   <-- 1 -->
172200930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
172300930803     C     FAT(24)       ANDEQ     'S'
172400930806     C                   MOVEL     FDE(23)       VFD007
172500930806     C                   MOVEL     FDE(24)       VFD008
172600930803     C                   ELSE                                                   <-- 2 -->
172700930806     C                   MOVEL     FUV(7)        VFD007
172800930803     C     FAT(23)       IFEQ      'S'
172900930806     C                   MOVEL     FDE(23)       VFD008
173000930803     C                   ELSE
173100930806     C                   MOVEL     FDE(24)       VFD008
173200930805     C                   ENDIF
173300930805     C                   ENDIF                                                  <-- 2 ---
173400930805     C                   ENDIF                                                  <-- 1 ---
173500930803     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
173600930803     C                   ENDSR
173700930803     C*================================================================
173800930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
173900930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
174000930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
174100930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
174200930803     C*================================================================
174300930803     C     CONFUN        BEGSR
174400930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
174500930803     C     1             DO        31            I                              --- 1 -->
174600930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
174700930805     C* CONTROLLO SE IL TASTO E' ATTIVO
174800930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
174900930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
175000930803     C                   EXSR      CARMSG
175100930803     C                   MOVEL     '1'           $ERR
175200930803     C                   ENDIF                                                  <-- 3 ---
175300930803     C                   ENDIF                                                  <-- 2 ---
175400930803     C                   ENDDO                                                  <-- 1 ---
175500930803     C                   ENDSR
175600930802     C*================================================================
175700930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
175800930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
175900930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
176000930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
176100930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
176200930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
176300930802     C*================================================================
176400930802     C     CAROPZ        BEGSR
176500930803     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
176600930802     C     1             DO        24            I                              --- 1 -->
176700930803     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
176800930805     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
176900930805     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
177000930805     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
177100020709     c                   If        DutLpo = 'S'
177200930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
177300930805     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
177400930802     C                   ELSE                                                   *INTERROG.
177500930805     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
177600930802     C                   ENDIF
177700930802     C                   ELSE                                                   <-- 2 --> *FILIALE
177800930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
177900930805     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
178000930802     C                   ELSE                                                   *INTERROG.
178100930805     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
178200930802     C                   ENDIF
178300930802     C                   ENDIF                                                  <-- 2 ---
178400930806     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
178500930806     C                   MOVEL     USER          WEDP              3            *PROFILO
178600930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
178700930806     C                   MOVEL     'S'           OAT(I)                         *ABILITO
178800930806     C                   ELSE
178900930806     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
179000930806     C                   ENDIF
179100930806     C                   ENDIF
179200930802     C                   ENDDO                                                  <-- 1 ---
179300930802     C                   ENDSR
179400930802     C*================================================================
179500930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
179600930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
179700930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
179800930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
179900930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
180000930802     C*================================================================
180100930802     C     OUTOPZ        BEGSR
180200930805     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
180300930805     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
180400930802     C                   ENDDO
180500930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
180600930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
180700930803     C                   ADD       1             OPZULT
180800930803     C     OPZULT        IFGT      24
180900930803     C                   Z-ADD     1             OPZULT
181000930803     C                   END
181100930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
181200930802     C     1             DO        24            I
181300930805     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
181400930805     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
181500930802     C                   ADD       1             OPZNAT
181600930802     C                   ENDIF
181700930802     C                   ENDDO
181800930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
181900931015     C                   Z-ADD     0             W
182000930802     C     OPZULT        DO        24            I                              --- 1 -->
182100930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
182200930802     C     W             IFLT      8                                            --- 2 -->
182300930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
182400930805     C     OAT(I)        IFEQ      'S'
182500930803     C     ODE(I)        ANDNE     *BLANKS
182600930802     C                   ADD       1             W
182700930803     C                   MOVEL     ODE(I)        OPV(W)
182800930805     C                   ENDIF
182900930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
183000930802     C                   Z-ADD     I             OPZULT
183100930802     C                   ENDIF                                                  <-- 2 ---
183200930802     C                   ENDDO                                                  <-- 1 ---
183300930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
183400930806     C                   MOVEL     OPV(1)        VOD001
183500930806     C                   MOVEL     OPV(2)        VOD002
183600930806     C                   MOVEL     OPV(3)        VOD003
183700930806     C                   MOVEL     OPV(4)        VOD004
183800930806     C                   MOVEL     OPV(5)        VOD005
183900930806     C                   MOVEL     OPV(6)        VOD006
184000930806     C                   MOVEL     OPV(7)        VOD007
184100930806     C                   MOVEL     OPV(8)        VOD008
184200930802     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
184300930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
184400930923     C     OPZNAT        IFGT      8
184500930923     C                   MOVEL     'S'           FAT(23)
184600930923     C                   ENDIF
184700930802     C                   ENDSR
184800000000     C*================================================================
184900930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
185000930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
185100930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
185200930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
185300000000     C*================================================================
185400930802     C     CONOPZ        BEGSR
185500000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
185600930803     C                   MOVEL     *BLANKS       OPZOK             1
185700930805     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
185800930806     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
185900930806     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
186000930805     C     OAT(I)        IFEQ      'S'
186100930806     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
186200940105     C                   MOVEL     'S'           OPZOK
186300930805     C                   ELSE
186400930802     C                   MOVEL     'N'           OPZOK             1
186500930805     C                   ENDIF
186600930802     C                   ENDIF                                                  <-- 2 ---
186700930802     C                   ENDDO                                                  <-- 1 ---
186800930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
186900930803     C     OPZOK         IFNE      'S'
187000930804     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
187100930803     C                   EXSR      CARMSG
187200930803     C                   MOVEL     '1'           $ERR
187300930803     C                   MOVEL     'N'           OPZOK             1
187400930803     C                   ENDIF
187500000000     C                   ENDSR
187600930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
187700941115TNSDU4R- Tasto funzionale non valido.                                          1
187800941115TNSDU4R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
187900941115TNSDU4R- Opzione non valida.                                                   3
188000941115TNSDU4R- Opzione abilitata ma non gestita. Avvertire CED.                      4
188100941115TNSDU4R- Inizio elenco.                                                        5
188200941115TNSDU4R- Fine elenco.                                                          6
188300941115TNSDU4R- Accettato solo un parametro di inizio elenco.                         7
188400941115TNSDU4R- Magazzino errato.                                                     8
188500941115TNSDU4R- Tipo unità errato.                                                    9
188600941115TNSDU4R- Data errata.                                                         10
188700941115TNSDU4R- Codice unità non accettabile.                                        11
188800960930TNSDU4R- Movimento annullato, consentito solo il ripristino.                  12
188900960930TNSDU4R- Movimento non annullato, impossibile ripristinare.                   13
189000960930TNSDU4R- Richiesta funzione per movimento inesistente.                        14
189100941115TNSDU4R- Si può annullare solo l'ultimo movimento.                            15
189200941115TNSDU4R- Movimento di tipo vecchio, impossibile annullare o modificare.       16
189300110407TNSDU4R- Tipo e matricola inesistente in anagrafica e movimenti unità EDP     17
189400960930TNSDU4R- Tipo e matricola non univoca: posizionarsi per tipo e codice         18
189500110407TNSDU4R- Matricola inesistente in anagrafica e movimenti unità EDP            19
189600110407TNSDU4R- Matricola non univoca in anagrafica unità EDP                        20
189700020717TNSDU4R- Profilo non abilitato alla funzione                                  21
189800110407TNSDU4R- Matricola non univoca nei movimenti unità EDP                        22
189900000000** ======== SCHIERA: CMD  (COMANDI)                        ================
190000000000WRKSPLF
190100971104DSPRCDLCK FILE(*LIBL/UNMOV00F)
190200000000
190300000000
190400940105
190500930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
190600930803- 1-1 -                   -    NN     NN
190700930803- 2-2 -2=Modifica         -    SN     NN
190800930803- 3-3 -                   -    NN     NN
190900930806- 4-4 -4=Cancellazione    -    SN     NN
191000930803- 5-5 -5=Visualizzazione  -    SS     SS
191100930803- 6-6 -                   -    NN     NN
191200930803- 7-7 -7=Ripristino       -    SN     NN
191300940104- 8-8 -                   -    NN     NN
191400971013- 9-9 -9=Unità            -    SS     SS
191500930806-10-  -                   -    NN     NN
191600930806-11-  -                   -    NN     NN
191700930806-12-  -                   -    NN     NN
191800930803-13-  -                   -    NN     NN
191900930803-14-  -                   -    NN     NN
192000930803-15-  -                   -    NN     NN
192100930803-16-  -                   -    NN     NN
192200930803-17-  -                   -    NN     NN
192300930803-18-  -                   -    NN     NN
192400930803-19-  -                   -    NN     NN
192500930803-20-  -                   -    NN     NN
192600930803-21-  -                   -    NN     NN
192700930803-22-  -                   -    NN     NN
192800930803-23-  -                   -    NN     NN
192900930803-24-  -                   -    NN     NN
193000930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
193100930723F1    -                   -    NN     NN
193200930723F2    -                   -    NN     NN
193300930802F3    -F3=Fine            -    SS     SS
193400930723F4    -                   -    NN     NN
193500971013F5    -F5=Rivisualizza    -    SS     NN
193600930723F6    -F6=Inserimento     -    SN     NN
193700930723F7    -                   -    NN     NN
193800930723F8    -                   -    NN     NN
193900930723F9    -                   -    NN     NN
194000930723F10   -                   -    NN     NN
194100930723F11   -                   -    NN     NN
194200930802F12   -F12=Ritorno        -    SS     SS
194300930806F13   -                   -    NN     NN
194400930806F14   -                   -    NN     NN
194500930723F15   -                   -    NN     NN
194600930723F16   -                   -    NN     NN
194700930802F17   -F17=Inizio dati    -    SS     SS
194800930802F18   -F18=Fine dati      -    SS     SS
194900930723F19   -                   -    NN     NN
195000930723F20   -                   -    NN     NN
195100971013F21   -F21=Stampa         -    SS     NN
195200940105F22   -F22=Gestione stampe-    NN     NN
195300930803F23   -F23=Altre opzioni  -    NN     NN
195400930803F24   -F24=Altri tasti    -    NN     NN
195500930802ENTER -                   -    SS     SS
195600930802ROLLUP-                   -    SS     SS
195700930802ROLLDN-                   -    SS     SS
195800930802HELP  -                   -    NN     NN
195900930802PRINT -                   -    NN     NN
196000930802BSPACE-                   -    NN     NN
196100930802CLEAR -                   -    NN     NN
