000100100111     /*END
000200011108      *--------------------------------------------------------------------------------------------*
000300090519      * Gestione tabella Limiti per tipo servizio  'LTS'                                           *
000400011108      *--------------------------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600011108      *--------------------------------------------------------------------------------------------*
000700011108      * Data base                                                                                  *
000800011108      *--------------------------------------------------------------------------------------------*
000900090519     FTABEL00F  IF   E           K DISK
001000100111     FTNTBE01L  UF A E           K DISK
001100100111     FTNTB73D   CF   E             WORKSTN
001200011108      *--------------------------------------------------------------------------------------------*
001300011108      * Data structure                                                                             *
001400011108      *--------------------------------------------------------------------------------------------*
001500011108      * Schiere a tempo di compilazione
001600011108      *---
001700011108     D Opz             S             15    DIM(3) CTDATA PERRCD(1)              *Messaggi video
001800011108     D Msg             S             78    DIM(4) CTDATA PERRCD(1)              *Messaggi video
001900011106      *----------
002000011108      * Variabili
002100010925      *----------
002200011108     D $Fine           S              1    INZ('N')                             *fine programma
002300011108     D TipVid          S              1    INZ('1')                             *video in gestione
002400011108     D $CarVd1         S              1    INZ('1')                             *video in gestione
002500011108     D $CarVd2         S              1    INZ('0')                             *video in gestione
002600011108     D Win             S             99A
002700090520     D codut           S              1  0 INZ(1)
002800090519     d cod             s                   like(tblcod)
002900090519     d §key            s                   Like(tblkey)
003000011108      *---
003100011108      * Indici
003200011108      *---
003300011108     D X               S              2  0 INZ                                  *indice
003400011108     D Y               S              2  0 INZ                                  *indice
003500100111
003600100111      * -?Status ds?
003700100111     d Status         sds
003800100111     d  SDSpgm           *proc
003900100111
004000011108      *---
004100011108      * Architettura
004200011108      *---
004300011108     D Kpjba         E DS                                                       *architettura
004400020715
004500090608     d tibs02ds      e ds
004600020715     d DdatiUte      e ds
004700020715     d AzuteDs       e ds                  ExtName(Azute00f)
004800020715     d Tibs34Ds      e ds                  Inz
004900100111      *---
005000100111      * Tabelle
005100100111      *---
005200100111     D Dlts          E DS                                                       *reperimento dati
005300100111     D Ds5e          E DS                                                       *reperimento dati
005400011108      *--------------------------------------------------------------------------------------------*
005500011108      * Main lines
005600011108      *--------------------------------------------------------------------------------------------*
005700011108      *
005800011108      * Operazioni iniziali
005900011106     C                   EXSR      RutInz
006000011108      *
006100011108      * Gestione video
006200011106     C     $Fine         DOUEQ     'S'
006300011106     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile vuoto
006400011106     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile vuoto
006500011105     C                   ENDCS
006600011105     C                   ENDDO
006700011108      *
006800000000     C                   SETON                                        LR
006900011108      *--------------------------------------------------------------------------------------------*
007000090519      * GesVd1 - Gestione videata selezione tipo servizio  -
007100011108      *--------------------------------------------------------------------------------------------*
007200011106     C     GesVd1        BEGSR
007300010925      *
007400011108      * Inizializzazione videata
007500011106     C     $CarVd1       ifeq      *on
007600011106     C                   EXSR      CarVd1
007700011106     C                   move      *off          $CarVd1
007800010925     C                   endif
007900100111     C                   clear                   V2PzD
008000011106      * Scrivo la testata
008100090519     C                   WRITE     Tb73T01
008200011106      *
008300011108      * Se esistono errori sulla videata
008400011106      * emetto la write del formato a indicatori spenti per vedere
008500011106      * le eventuali decodifiche
008600011106     C     *IN25         ifeq      *on
008700011106     C                   movea     *In           Win
008800011106     C                   move      *ALL'0'       in3040           11
008900011106     C                   movea     IN3040        *in(40)
009000090519     C                   WRITE     Tb73V01
009100011106     C                   movea     Win           *In
009200010925     C                   endif
009300011106      *
009400090519     C                   EXFMT     Tb73V01
009500011002      *
009600090519     c     '?'           scan      V1tsp                                  50
009700011105      * Ricerca
009800011105     c                   select
009900011105      * F3=Fine
010000011106     C     *inKc         wheneq    *on
010100010925     C                   EXSR      F03Ges
010200011105      *
010300011106     C     *in50         wheneq    *on
010400011106     C                   EXSR      Search
010500011105     C                   other
010600010925      *
010700010925      * Controlli e decodifiche
010800010925     C                   EXSR      CtrD1
010900011105      *
011000011106     C     *IN25         ifeq      *off
011100010925      *
011200011108      * Gestisco il video di dettaglio
011300011105     C                   move      *on           $carvd2
011400011106     C                   move      '2'           TipVid
011500011105     C                   endif
011600011105      *
011700011105     C                   endsl
011800011105      *
011900011105      *
012000010925     C                   endsr
012100011108      *--------------------------------------------------------------------------------------------*
012200011108      * GesVd2 - Gestione videata dettaglio dati
012300011108      *--------------------------------------------------------------------------------------------*
012400011106     C     GesVd2        BEGSR
012500011105      *
012600011108      * Inizializzazione videata
012700011106     C     $CarVd2       ifeq      *on
012800011106     C                   EXSR      Carvd2
012900011106     C                   move      *off          $CarVd2
013000011105     C                   endif
013100011108      * Scrivo la testata
013200090519     C                   WRITE     Tb73T01
013300011105      *
013400011106      * Se esistono errori sulla videata
013500011105      * emetto la write del formato a indicatori spenti per vedere
013600011105      * le eventuali decodifiche
013700011106     C     *IN25         ifeq      *on
013800011106     C                   movea     *In           Win
013900011106     C                   move      *ALL'0'       in3040           11
014000011106     C                   movea     IN3040        *in(40)
014100090519     C                   WRITE     Tb73V02
014200011106     C                   movea     Win           *In
014300011106     C                   endif
014400011105      *
014500011106     C     *in04         ifeq      *on
014600090519     C                   WRITE     Tb73V02
014700011106     C                   EXFMT     PROTECT
014800011106      *
014900011106     C                   else
015000090519     C                   EXFMT     Tb73V02
015100011106     C                   endif
015200011108      *
015300011108     C                   Select
015400011108      * F3=Fine
015500011106     C     *inKc         wheneq    *on
015600011105     C                   EXSR      F03Ges
015700011108      * F12=ritorno
015800011106     C     *inKl         wheneq    *on
015900011105     C                   EXSR      F12Ges
016000011108      *
016100011108     C                   other
016200011105      *
016300011105      * Controlli e decodifiche
016400011105     C                   EXSR      CtrD2
016500011106      *
016600011106      * Non ci sono errori e ho chiesto conferma/annullamentoripristino
016700011106     c     *in25         ifeq      *off
016800011106     c     *inKf         ifeq      *on
016900011106     c     *inKe         oreq      *on
017000011106     c     *inKq         oreq      *on
017100011105      *
017200011106      * Eseguo aggiornamento
017300011105     C                   EXSR      AggTnt
017400011105     C                   endif
017500011106     C                   endif
017600011108      *
017700011105     C                   endsl
017800011105     C                   endsr
017900011108      *--------------------------------------------------------------------------------------------*
018000011108      * Search - Ricerca su campi video
018100011108      *--------------------------------------------------------------------------------------------*
018200010925     C     Search        BEGSR
018300011002      *
018400011106     C                   setoff                                       2540
018500010928      *
018600090608     c                   Clear                   tibs02ds
018700090608     c                   Eval      T02Mod = 'R'
018800090608     c                   Eval      T02Sif = Knsif
018900090608     c                   Eval      T02Cod = 'LTS'
019000090608     c                   Call      'TIBS02R'
019100090608     c                   Parm                    Kpjba
019200090608     c                   Parm                    tibs02ds
019300090608     c                   Movel     T02Ke1        v1tsp
019400090608      *
019500090519     C                   move      *blanks       V1tspD
019600090519     c                   move      '5E'          cod
019700090608     c                   movel(p)  v1tsp         §key
019800090608     c     ktab          chain     tabel00f
019900090608     c                   if        %found(tabel00f) and tblflg=' '
020000090608     C                   movel     tbluni        ds5e
020100090608     c                   movel     §5edes        v1tspd
020200090608     c                   endif
020300090608     c**                 exsr      cx§tab
020400090608     C**                 movel     §key          V1tsp
020500090608     C**                 movel     §des          V1tspD
020600090608     c
020700010928      *
020800010925     C                   ENDSR
020900011108      *--------------------------------------------------------------------------------------------*
021000011108      * F03Ges - Tasto funzionale F03 -> Fine programma
021100011108      *--------------------------------------------------------------------------------------------*
021200011106     C     F03Ges        BEGSR
021300011108      *
021400011106     C                   movel     'S'           $Fine                          *fine pgm
021500011108      *
021600010925     C                   ENDSR
021700011108      *--------------------------------------------------------------------------------------------*
021800011108      * F12Ges - Tasto funzionale f12 -> Ritorno
021900011108      *--------------------------------------------------------------------------------------------*
022000011106     C     F12Ges        BEGSR
022100011108      *
022200011106     C                   eval      TipVid = '1'
022300011108      *
022400010925     C                   ENDSR
022500011108      *--------------------------------------------------------------------------------------------*
022600011108      * CarVd1 - Carica primo video
022700011108      *--------------------------------------------------------------------------------------------*
022800011106     C     CarVd1        BEGSR
022900011108      *
023000011106     c                   setoff                                       25
023100011106     c                   setoff                                       010204
023200090519     C                   clear                   Tb73V01
023300090519     C                   move      '  ?'         V1tsp
023400011106      *
023500000000     C                   ENDSR
023600011108      *--------------------------------------------------------------------------------------------*
023700011108      * CarVd2 - Carica secondo video
023800011108      *--------------------------------------------------------------------------------------------*
023900011106     C     CarVd2        BEGSR
024000011106     C                   setoff                                       010204
024100011106      *
024200011106      * Aggancio la tabella, se trovo il codice sono in modifica
024300011108      * o ripristino (se record annullato), altrimenti in immissione
024400090519     c                   movel     V1tsp         CodTab            3
024500011106     c                   EXSR      ChnTnt
024600011106      *
024700011106      * MODIFICA/RIPRISTINO
024800011106     c  n21              do
024900011106     c                   seton                                        02
025000090519     c                   movel     Tbeuni        Dlts
025100090519     c                   eval      V2tsp =V1tsp
025200090519     c                   eval      V2tspD=v1tspd
025300090519     c                   z-add     §LTSLCLM      v2lclm
025400090519     c                   z-add     §LTSLCLF      v2lclf
025500090519     c                   z-add     §LTSLPKM      v2lpkm
025600090519     c                   z-add     §LTSLPKF      v2lpkf
025700090519     c                   z-add     §LTSLMCM      v2lmcm
025800090519     c                   z-add     §LTSLMCF      v2lmcf
025900100111     c                   eval      V2vas = §LTSvas
026000011106      *
026100011106     C     TbeAtb        ifeq      *blanks
026200011106     C                   setoff                                       04
026300011106     C                   eval      V2PzD = Opz(2)
026400011106     C                   else
026500011106     C                   seton                                        04
026600011106     C                   eval      V2PzD = Opz(3)
026700011106     C                   endif
026800011106     c                   enddo
026900011106      *
027000011106      * IMMISSIONE
027100011106     c   21              do
027200090519     c                   clear                   Tb73V02
027300090519     c                   movel     V1tsp         V2tsp
027400090519     c                   movel     V1tspd        V2tspd
027500011106     c                   seton                                        01
027600011106     C                   eval      V2PzD = Opz(1)
027700011106     c                   enddo
027800011106     c
027900011105     C                   ENDSR
028000011108      ************************************************************
028100011108      * Controllo e decodifica prima videata
028200011108      ************************************************************
028300010925     C     CtrD1         BEGSR
028400011106      *
028500011106     C                   setoff                                       2540
028600011106     c                   clear                   V1Msg
028700011106      *
028800011106      * Codice tabella obbligatorio
028900090519     c     V1tsp         ifeq      *blanks
029000090519     c                   seton                                        2540
029100011106     c                   eval      V1Msg = Msg(1)
029200011106      *
029300011106      * Se esiste decodifico
029400011106     c                   else
029500090519     c                   move      '5E'          cod
029600090519     c                   movel(p)  v1tsp         §key
029700090519     c     ktab          chain     tabel00f
029800090519     c                   if        %found(tabel00f) and tblflg=' '
029900090519     C                   movel     tbluni        ds5e
030000090519     c                   movel     §5edes        v1tspd
030100090519     c                   else
030200090519     c                   seton                                        2540
030300090519     c                   eval      V1Msg = Msg(1)
030400011106     c                   endif
030500090519     c                   endif
030600010925      *
030700010925     C                   ENDSR
030800011108      ************************************************************
030900011108      * Controllo e decodifica seconda videata
031000011108      ************************************************************
031100011105     C     CtrD2         BEGSR
031200090519     C                   setoff                                       2532
031300090519     C                   setoff                                       3334
031400090519     C                   setoff                                       3531
031500011106     C                   clear                   V2Msg
031600011106     C
031700011106      *
031800011106      * Controlli da effettuarsi se record non annullato
0319000111061a   C     *In04         ifeq      *off
032000011106      *
032100090519      * COLLI
032200090610      * Controllo che il numero colli  forz. non sia maggiore del max
0323000905192a   C     V2lclf        ifgt      V2lclm
032400090519     C                   seton                                        2531
032500011108     C                   eval      V2Msg = Msg(4)
0326000111062-   C                   endif
032700090519      * PESO KG
032800090519     c* controllo
032900090610      * Controllo che il peso  forz. non sia maggiore del max
0330000905192a   C     V2lpkf        ifgt      V2lpkm
033100090519     C                   seton                                        2532
033200090519     C                   eval      V2Msg = Msg(4)
0333000905192-   C                   endif
033400090519      * VOLUME Mc
033500090610      * Controllo che il volume forz. non sia maggiore del max
0336000905192a   C     V2lmcf        ifgt      V2lmcm
033700090519     C                   seton                                        2534
033800090519     C                   eval      V2Msg = Msg(4)
0339000905192-   C                   endif
034000011106      *
0341000111061-   C                   endif
034200011105      *
034300011105     C                   ENDSR
034400011108      ************************************************************
034500090519      * Aggiornamento tabella LTS
034600011108      ************************************************************
034700010925     C     AggTnt        BEGSR
034800010926      *
034900011105     C                   SELECT
035000011106      * F06=Conferma
035100011106     C     *InKf         wheneq    *on
035200011106      *
035300011105      * Immissione
035400011108     C     *in01         ifeq      *on
035500011105     C                   EXSR      RieTnt
035600011106     C                   move      *zeros        TbeFlt
035700011107     C                   move      'S'           TbeFtt
035800011108     c                   move      *zeros        Tbedtr
035900011105     C                   WRITE     tntbe000                             22
036000011106     C                   endif
036100011105      * Modifica
036200011106     C     *in02         Ifeq      *on
036300011106     C                   EXSR      RieTnt
036400011106     C                   UPDATE    tntbe000
036500011106     C                   endif
036600011105      *
036700011106      * F16=Annullamento
036800011105     C     *inkq         wheneq    *on
036900011106     C     *in04         andeq     *off
037000011105     C                   move      'A'           tbeatb
037100011107     C                   move      *blanks       tbeftr
037200011106     C                   UPDATE    tntbe000
037300011105      *
037400011106      * F5=Ripristino
037500011105     C     *inke         wheneq    *on
037600011106     C     *in04         andeq     *on
037700011105     C                   move      ' '           tbeatb
037800011107     C                   move      *blanks       tbeftr
037900011106     C                   UPDATE    tntbe000
038000011105      *
038100011105     C                   ENDSL
038200011108      * Torno alla prima videata che carico come da inizio pgm
038300011106     C                   move      '1'           TipVid
038400011106     C                   move      *on           $CarVd1
038500011106     C
038600010925     C                   ENDSR
038700011108      ************************************************************
038800011108      * Riempimento file tabella
038900011108      ************************************************************
039000010925     C     RieTnt        BEGSR
039100011106      *
039200011106      * Codice tabella
039300011106     c                   movel     *blanks       TbeAtb
039400011106     c                   move      Sif           TbeSif
039500011106     c                   move      'GE'          TbeApl
039600090519     c                   movel     'LTS'         TbeCod
039700090519     c                   movel     V2tsp         TbeKe1
039800011106     c                   movel     *blanks       TbeKe2
039900011105      *
040000090519     c                   clear                   Dlts
040100090519     c                   z-add     V2Lclm        §LTSLCLM
040200090519     c                   z-add     V2Lclf        §LTSLCLF
040300090519     c                   z-add     V2Lpkm        §LTSLPKM
040400090519     c                   z-add     V2Lpkf        §LTSLPKF
040500090519     c                   z-add     V2Lmcm        §LTSLMCM
040600090519     c                   z-add     V2Lmcf        §LTSLMCF
040700100111     c                   eval      §LTSvas = V2vas
040800090519     c                   movel     Dlts          TbeUni
040900011106      *
041000011106     c                   move      *blanks       TbeFtr
041100010925     C
041200010925     C                   ENDSR
041300011108      *--------------------------------------------------------------------------------------------*
041400011108      * Aggancio tabella
041500011108      *--------------------------------------------------------------------------------------------*
041600011106     c     ChnTnt        BEGSR
041700090519     c                   move      'LTS'         TbeCod
041800011106     c                   clear                   TbeKe1
041900011106     c                   movel     CodTab        TbeKe1
042000011106     c                   move      *blanks       TbeKe2
042100011106     c                   move      *blanks       TbeLin
042200011106     c                   movel     Knsif         TbeSif
042300011106     c     k01tbe5       chain     tntbe01l                           21
042400011106      * Se non ho trovato il record con il sistema informativo
042500011106      * che ho in linea, lo abblenco
042600011106     c   21              do
042700011106     c                   eval      TbeSif= *blanks
042800011106     c                   eval      Sif= *blanks
042900011106     c     k01tbe5       chain     tntbe01l                           21
043000011106     c  N21              eval      Sif= *blanks
043100011106     c                   enddo
043200011108      *
043300011106     c                   ENDSR
043400090519     c
043500090519     C* CALL AL PGM DI RICERCA TABELLE X§TABER -----------------------*
043600090519     C     CX§TAB        BEGSR
043700090519     C                   CLEAR                   §KEY
043800090519     C                   CALL      'X§TABER'
043900090520     C                   PARM                    CODUT
044000090519     C                   PARM                    COD
044100090519     C                   PARM                    §KEY
044200090519     C                   PARM                    §DES             25
044300090519     C                   ENDSR
044400011108      *--------------------------------------------------------------------------------------------*
044500011108      * RutInz - operazioni iniziali
044600011108      *--------------------------------------------------------------------------------------------*
044700011108     C     RutInz        BEGSR
044800011108      *
044900011108      * Ricevimento parametri
045000000000     C     *ENTRY        PLIST
045100000000     C                   PARM                    kpjba
045200011108      *
045300011108      * Chiavi di lettura
045400010925     c     K01tbe5       KLIST                                                  *tntbe01l
045500010925     c                   KFLD                    tbecod                          -tabella
045600010925     c                   KFLD                    tbeke1                          -chiave uno
045700010925     c                   KFLD                    tbeke2                          -chiave due
045800010925     c                   KFLD                    tbelin                          -lingua
045900010925     c                   KFLD                    tbesif                          -s.informativo
046000090519     C     Ktab          klist
046100090519     C                   kfld                    codut
046200090519     C                   kfld                    cod
046300090519     C                   kfld                    §key
046400010925      *
046500011108      * Reperisce dati utente
046600020715
046700020715     c     *dtaara       define    §azute        azuteds
046800020715     c     *dtaara       define    §datiute      ddatiute
046900020715
047000020715     c                   in(E)     *dtaara
047100020715     c                   if        %Error or rsut = *blanks
047200020715     c                   clear                   tibs34ds
047300020715     c                   call      'TIBS34R'
047400020715     c                   parm                    Tibs34Ds
047500020715     c                   in        *dtaara
047600020715     c                   endif
047700100111      *
047800100111      * Nome del programma
047900100111     c                   eval      V1Tpgm = SDSpgm
048000011106      *
048100011106      * Sistema informativo
048200011106     C                   movel     knsif         Sif              10
048300011106     C
048400000000     C                   ENDSR
048500000000     O*--------------------------------------------------------------------------------------------*
048600010926** -OPZ-
048700010926  Inserimento
048800010926   Modifica
048900010926  Annullato
049000000000** -MSG-
049100090519TNTB73R- Tipo Servizo mancante o errato
049200090519TNTB73R-
049300090519TNTB73R-
049400090610TNTB73R- Il valore massimo non può essere inferiore del valore forzabile
