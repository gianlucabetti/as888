000100091117     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200091117     h dftactgrp(*no)
000300091117     h bnddir('UBRTVNETA')
000400020208      *--------------------------------------------------------------------------------------------*
000500020208      *     MANUTENZIONE TABELLA 3NN PER AUTORIZZAZIONI VCONFERMA BOLLE                            *
000600020208      *--------------------------------------------------------------------------------------------*
000700020208      * Data base                                                                                  *
000800020208      *--------------------------------------------------------------------------------------------*
000900091117     fAzOrg01l  if   e           k disk
001000091117     fTabel00f  if   e           k disk
001100091117      *
001200091117     fTNTBE01L  if A e           k disk
001300091117     f                                     extfile(wLibFile1)  usropn
001400091117     f                                     rename(Tntbe000:tntbe01)
001500091117     fTNTBE02L  Uf   e           k disk
001600091117     f                                     extfile(wLibFile2)  usropn
001700091117      *
001800091117     fTnTb23d   cf   e             workstn
001900091117
002000091117       //--------------------------------------------------------------
002100091117       //?Definizione costanti.                                        ?
002200091117       //--------------------------------------------------------------
002300091117
002400091117       // - Costanti per la definizione delle schiere con i nomi
002500091117       //   degli iSeries da elaborare e delle relative librerie
002600091117     d c_NrSyst        c                   const(2)
002700091117     d c_NrLibr        c                   const(2)
002800091117
002900091117       //--------------------------------------------------------------
003000091117       //?Definizione schiere.                                         ?
003100091117       //--------------------------------------------------------------
003200091117
003300091117       // - iSeries  &  Librerie con entrambi i file tabelle
003400091117     d $iSystem        s                   like(currSysNetA)
003500091117     d                                     dim(c_NrSyst)
003600091117     d                                     ctdata   perrcd( 1)
003700091117     d $SisInf         s                   like(ds_Libr)
003800091117     d                                     dim(c_NrSyst)
003900091117     d                                     alt($iSystem)
004000020208      *---
004100020208      * Schiere memorizzazione dati correnti
004200020208      *---
004300020211     D Pou             S             03    DIM(19)
004400020320     D msg             S             78    DIM(6) CTDATA PERRCD(1)              *Messaggi video
004500091117
004600091117       //--------------------------------------------------------------
004700091117       //?Definizione aree dati.                                       ?
004800091117       //--------------------------------------------------------------
004900091117
005000091117       // - Dati utente
005100091117     d §AzUte        e ds                  extname(AZUTE00F)
005200091117     d                                     dtaara
005300091117     d §DatiUte      e ds                  extname(dDatiUte)
005400091117     d                                     dtaara
005500091117
005600091117       //--------------------------------------------------------------
005700091117       //?Definizione strutture dati.                                  ?
005800091117       //--------------------------------------------------------------
005900091117
006000091117     D Tntb22Ds      E DS                                                       *ds di procedura
006100091117     D Tibs69Ds      E DS                                                       *ds di procedura
006200091117     D Og143         E DS                                                       *ds di procedura
006300091117     D Ds3C          E DS                                                       *ds di procedura
006400091117     D Ds1B          E DS                                                       *ds di procedura
006500091117     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
006600091117     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
006700091117     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
006800091117     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
006900091117
007000091117       // - Parametri per Reperimento dati utente
007100091117     d TIBS34ds      e ds                  inz
007200091117
007300091117       // - Ridefinizione elenco librerie in elaborare le tabelle
007400091117     d ds_Libr         ds                  inz
007500091117     d  $Libr                        10    dim(c_NrLibr) inz
007600091117      *
007700091117      *---
007800091117      * architettura
007900091117      *---
008000091117     D kpjba         E DS                                                       *architettura
008100091117
008200091117       // - Status ds
008300091117     d Status         sds
008400091117     d  SDSpgm           *proc
008500091117
008600091117       //--------------------------------------------------------------
008700091117       //?Definizione variabili globali.                               ?
008800091117       //--------------------------------------------------------------
008900091117
009000091117       // - Flags booleani
009100091117     d $Fine           s               n   inz(*off)
009200091117     d $CarVd1         s               n   inz
009300091117     d $CarVd2         s               n   inz
009400091117
009500091117     d TipVid          s              1a   inz('0')                             *video in gestione
009600091117
009700091117       // - Indici di schiera
009800020208     D i               S              5  0 INZ                                  *indice
009900020208     D j               S              5  0 INZ                                  *indice
010000020208     D k               S              5  0 INZ                                  *indice
010100020208     D g               S              5  0 INZ                                  *indice
010200020208     D v               S              5  0 INZ                                  *indice
010300020208     D m               S              5  0 INZ                                  *indice
010400091117
010500091117       // - Nome del sistema
010600091117     d currSysNeta     s              8a   inz
010700091117
010800091117       // - Nome esteso Libreria/File dei 2 file tabella
010900091117     d wLibFile1       s             21a   inz
011000091117     d wLibFile2       s             21a   inz
011100091117
011200091117       // - Campi di comodo
011300091117     d w_iSystem       s              1  0 inz
011400091117     d w_SisInf        s              3  0 inz
011500091117
011600091117       //--------------------------------------------------------------
011700091117       //?Definizione prototipi procedure.                             ?
011800091117       //--------------------------------------------------------------
011900091117
012000091117       // - Reperimento dati utente
012100091117      /copy gaitrasrc/srcProtoPR,TIBS34R
012200091117
012300091117       // - Reperimento NETA sistema AS/400 corrente
012400091117     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
012500091117
012600020208      *--------------------------------------------------------------------------------------------*
012700020208      * Main lines
012800020208      *--------------------------------------------------------------------------------------------*
012900020208      *
013000020208      * Operazioni iniziali
013100020208      *
013200020208     C                   EXSR      RutInz
013300020208      *
013400020208      * Gestione video
013500020208      *
013600091117     C     $Fine         DOUEQ     *on
013700020208     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
013800020208     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile
013900020208     C                   ENDCS
014000020208     C                   ENDDO
014100020208      *
014200020208      *--------------------------------------------------------------------------------------------*
014300020208      * GesVd1 - Gestione videata immissione nuova tabella -
014400020208      *--------------------------------------------------------------------------------------------*
014500020208     C     GesVd1        BEGSR
014600020208      *
014700020208      * Inizializzazione videata
014800020208     C     $CarVd1       ifeq      *on
014900020208     C                   EXSR      CarVd1
015000020208     C                   move      *off          $CarVd1
015100020208     C                   endif
015200020208      *
015300091117     c                   write     Tb23t1
015400020208     C                   EXFMT     Tb23v1
015500020208      *
015600020208      *
015700020208     C                   select
015800020208      * F3=Fine
015900020208     C     *inKc         wheneq    *on
016000020208     C                   EXSR      F03Ges
016100020208      *
016200020208     C     *inKl         wheneq    *on
016300020208     C                   EXSR      F12Ges
016400020208     C                   other
016500020208      *
016600020208      * Controlli e decodifiche
016700020208     C                   EXSR      CtrV1
016800020208      *
016900020208     C     *IN28         ifeq      *off
017000020208     C     *inKf         andeq     *on
017100020208     C                   EXSR      GesAgg
017200020208     C                   endif
017300020208     C                   endsl
017400020208      *
017500020208     C                   endsr
017600020208      *--------------------------------------------------------------------------------------------*
017700020208      * CarVd1 - Carica primo video
017800020208      *--------------------------------------------------------------------------------------------*
017900020208     C     CarVd1        BEGSR
018000020208      *
018100091117     c                   eval      V1Topz  = '  INSERIMENTO  '
018200020208     C                   clear                   Tb23v1
018300020214      * imposto il campo solo se valorizzato e in questo caso
018400020214      * proteggo il campo
018500020214     C     b22ke1        ifne      *blanks
018600020214     C                   seton                                        38
018700020214     C                   movel     b22ke1        $Cod
018800020214     C                   move      $Cod          V1CCod
018900020214     C                   endif
019000020212      * decodifica
019100020212     C                   move      *blanks       V1DCod
019200020214     C     v1ccod        ifne      *zeros
019300020212     C                   move      V1CCod        $Cod              7 0
019400020212     C                   EXSR      DeCod
019500020212      *
019600020212 2a  C                   if        o69err = *blanks and acoflg =' '
019700020212     C                   movel     acorag        V1DCod
019800020212 2e  C                   endif
019900020214 2e  C                   endif
020000020208      *
020100020208     C                   ENDSR
020200020208      *--------------------------------------------------------------------------------------------*
020300020208      * Controllo e decodifica prima videata
020400020208      *--------------------------------------------------------------------------------------------*
020500020208     C     CtrV1         BEGSR
020600020208      *
020700020214     C                   setoff                                       283940
020800020212     C                   setoff                                       414243
020900020208     C                   setoff                                       515253
021000020208     C                   setoff                                       545556
021100020208     C                   setoff                                       575859
021200020208     C                   setoff                                       606162
021300020208     C                   setoff                                       636465
021400020208     C                   setoff                                       666768
021500020215     C                   setoff                                       6944
021600020208     C                   clear                   V1CMsg
021700020208      *
021800020214      * Ricerca estesa cu codice cliente se non protetto
021900020214 1a  C     v1dcod        ifne      *blanks
022000020214 1a  C     v1ccod        andeq     *zeros
022100020214     C     *in38         andne     *on
022200020208      *
022300020208      * Ricerca estesa
022400091117     C                   movel     rsut          paxdut           30
022500020214     C                   move      v1ccod        $campo7           7
022600020214     C                   movel(P)  $campo7       paxdmt           48
022700020208      *
022800020208     C                   z-add     0             paxsta
022900091117     C                   move      DUTkci        paxccc            4 0
023000020208     C                   clear                   paxdit
023100020208     C                   z-add     1             paxnum
023200020208      *
023300020208     C                   CALL      'XALFA3BR'
023400020208     C                   PARM                    PAXDUT
023500091117     C                   PARM      1             CODUT             1 0
023600020208     C                   PARM                    PAXDMT
023700020208     C                   PARM                    PAXCCC
023800020208     C                   PARM                    PAXSTA            1 0
023900020208     C                   PARM                    PAXFLR           90
024000020208     C                   PARM                    PAXDIT            3
024100020208     C                   PARM                    PAXNUM            2 0
024200020208     C                   PARM                    PAXKCM           80
024300020208     C                   PARM                    PAXKSM          140
024400020208     C                   PARM                    PAXKDM           60
024500020208      *
024600020208 2a  C     PaxSta        ifgt      -1
024700020214     C                   movel     paxksm        $campo7
024800020214     C                   move      $campo7       v1ccod
024900020214     C                   movel     paxdmt        v1dcod
025000020208 2-  C                   endif
025100020208      *
025200020208     C                   goto      Finesr
025300020214 1-  C                   endif
025400020208      *
025500020212      * Controllo validità codice
025600020214     C                   move      *blanks       v1dcod
025700020214 1a  C     v1ccod        ifne      *zeros
025800020208      *
025900020214     C                   move      v1ccod        $Cod              7 0
026000020208     C                   EXSR      DeCod
026100020208      *
026200020208 2a  C                   if        o69err = *blanks and acoflg =' '
026300020214     C                   movel     acorag        v1dcod
026400020212      *
026500020208 2e  C                   else
026600020214     C                   seton                                        2839
026700020212     C                   goto      Finesr
026800020208 2-  C                   endif
026900020208      *
027000020208 1-  C                   endif
027100020214      *
027200020214      * Nuovo codice cliente con cui confermare
027300020214 1a  C     v1dcli        ifne      *blanks
027400020214 1a  C     v1ccli        andeq     *zeros
027500020214      *
027600020214      * Ricerca estesa
027700020214     C
027800091117     C                   movel     rsut          paxdut           30
027900020214     C                   movel(P)  v1dcli        paxdmt           48
028000020214      *
028100020214     C                   z-add     0             paxsta
028200091117     C                   move      DUTkci        paxccc            4 0
028300020214     C                   clear                   paxdit
028400020214     C                   z-add     1             paxnum
028500020214      *
028600020214     C                   CALL      'XALFA3BR'
028700020214     C                   PARM                    PAXDUT
028800091117     C                   PARM      1             CODUT
028900020214     C                   PARM                    PAXDMT
029000020214     C                   PARM                    PAXCCC
029100020214     C                   PARM                    PAXSTA            1 0
029200020214     C                   PARM                    PAXFLR           90
029300020214     C                   PARM                    PAXDIT            3
029400020214     C                   PARM                    PAXNUM            2 0
029500020214     C                   PARM                    PAXKCM           80
029600020214     C                   PARM                    PAXKSM          140
029700020214     C                   PARM                    PAXKDM           60
029800020214      *
029900020214 2a  C     PaxSta        ifgt      -1
030000020214     C                   movel     paxksm        $campo7
030100020214     C                   move      $campo7       v1ccli
030200020214     C                   movel     paxdmt        v1dcli
030300020214 2-  C                   endif
030400020214      *
030500020214     C                   goto      Finesr
030600020214 1-  C                   endif
030700020214      *
030800020214      * Controllo validità codice
030900020214     C                   move      *blanks       v1dcli
031000020214 1a  C     v1ccli        ifne      *zeros
031100020214      *
031200020214     C                   move      v1ccli        $Cod              7 0
031300020214     C                   EXSR      DeCod
031400020214      *
031500020214 2a  C                   if        o69err = *blanks and acoflg =' '
031600020214     C                   movel     acorag        v1dcli
031700020214      *
031800020214      * Controllo che i due codici cliente sino diversi
031900020214 3a  C     v1ccli        ifeq      v1ccod
032000020214     C                   seton                                        2843
032100020214     C                   goto      Finesr
032200020214 3-  C                   endif
032300020214      *
032400020214      * Controllo che i due codici cliente siano entrambi
032500020214      * poste/non poste
032600020215     C     v1ccod        div       10000         $Pou1
032700020215     C     v1ccli        div       10000         $Pou2
032800020214     C                   exsr      CtrPos
032900020214 3a  C   24              do
033000020215     C                   seton                                        2841
033100020214     C                   goto      FineSr
033200020214 3-  C                   enddo
033300020214      *
033400020214 2e  C                   else
033500020214     C                   seton                                        2840
033600020214     C                   goto      Finesr
033700020214 2-  C                   endif
033800020214      *
033900020214 1-  C                   endif
034000020214      *
034100020214      * Controllo che i due codici siano poste/non poste
034200020214     C     v1ccli        ifne      *zeros
034300020214     C     v1ccod        div       10000         $Pou1
034400020214     C     v1ccli        div       10000         $Pou2
034500020214     C                   exsr      CtrPos
034600020214     C   24              do
034700020214     C                   seton                                        2841
034800020214     C                   goto      FineSr
034900020214     C                   enddo
035000020214     C                   endif
035100020214      *
035200020214     C
035300020211      *
035400020211      * Ci deve essere almeno un P.O diverso da zero
035500020212 1a  C     v1cp01        ifeq      *zeros
035600020212     C     v1cp02        andeq     *zeros
035700020212     C     v1cp03        andeq     *zeros
035800020212     C     v1cp04        andeq     *zeros
035900020212     C     v1cp05        andeq     *zeros
036000020212     C     v1cp06        andeq     *zeros
036100020212     C     v1cp07        andeq     *zeros
036200020212     C     v1cp08        andeq     *zeros
036300020212     C     v1cp09        andeq     *zeros
036400020212     C     v1cp10        andeq     *zeros
036500020212     C     v1cp11        andeq     *zeros
036600020212     C     v1cp12        andeq     *zeros
036700020212     C     v1cp13        andeq     *zeros
036800020212     C     v1cp14        andeq     *zeros
036900020212     C     v1cp15        andeq     *zeros
037000020212     C     v1cp16        andeq     *zeros
037100020212     C     v1cp17        andeq     *zeros
037200020212     C     v1cp18        andeq     *zeros
037300020212     C     v1cp19        andeq     *zeros
037400020212      *
037500020215     C                   seton                                        2844
037600020212     C                   movel     msg(1)        V1cmsg
037700020212     C                   goto      FineSr
037800020212     C                   endif
037900020208      *
038000020208      * Controllo P.O
038100020215      *
038200020215      * Mi imposto il pou del codice
038300020215     C     v1ccod        div       10000         $Pou1             3 0
038400020212      *
038500020215 1a  C     v1cp01        ifne      *zeros
038600020215     C     v1cp01        ifne      999
038700020215     C     v1cp01        chain     Azorg01l                           21
038800020215 2a  C   21              do
038900020215     C                   seton                                        2851
039000020215     C                   movel     msg(2)        V1cmsg
039100020215     C                   goto      FineSr
039200020215 2-  C                   enddo
039300020215     C                   endif
039400020215      *
039500020215 2a  C  n21              do
039600020215      * Controllo che il record non esista già in tabella
039700020215     C                   movel     *blanks       TbeKe1
039800020215     C                   movel     *blanks       TbeKe2
039900020215     C                   move      v1ccod        $Campo7
040000020215     C                   movel     $Campo7       TbeKe1
040100020215     C                   move      v1cp01        $Campo3           3
040200020215     C                   movel     $Campo3       TbeKe2
040300020215     C                   exsr      ChnTbe
040400020215 3a  C  n23              do
040500020215     C                   seton                                        2851
040600020215     C                   movel     msg(3)        V1cmsg
040700020215     C                   goto      FineSr
040800020215 3-  C                   enddo
040900020215      *
041000020215      * Controllo P.O e cliente poste o non poste
041100020215     C                   move      v1cp01        $Pou2             3 0
041200020215     C                   exsr      CtrPos
041300020215 3a  C   24              do
041400020215     C                   seton                                        285142
041500020215     C                   movel     msg(5)        V1cmsg
041600020215     C                   goto      FineSr
041700020215 3-  C                   enddo
041800020215 2-  C                   enddo
041900020215 1-  C                   endif
042000020215      *
042100020215 1a  C     v1cp01        ifeq      999
042200020215      * Controllo che il cliente non mantenga il numero spedizione
042300020215     C                   move      v1ccod        $Campo7
042400020215     C                   Exsr      CtrSpe
042500020320      *
042600020215 2a  C   25              do
042700020215     C                   seton                                        2851
042800020215     C                   movel     msg(4)        V1cmsg
042900020215     C                   goto      FineSr
043000020215 2-  C                   enddo
043100020320      *
043200020320     C     §3CCtm        ifne      *blanks
043300020320      * Controllo anche che il cliente non abbia un codice trattamento
043400020320      * tipo DISK 'B'
043500020320     C                   Exsr      CtrCdT
043600020320 2a  C   25              do
043700020320     C                   seton                                        2851
043800020320     C                   movel     msg(6)        V1cmsg
043900020320     C                   goto      FineSr
044000020320 2-  C                   enddo
044100020215 1-  C                   endif
044200020320 1-  C                   endif
044300020212      *
044400020212 1a  C     v1cp02        ifne      *zeros
044500020215     C     v1cp02        ifne      999
044600020212     C     v1cp02        chain     Azorg01l                           21
044700020212 2a  C   21              do
044800020212     C                   seton                                        2852
044900020212     C                   movel     msg(2)        V1cmsg
045000020212     C                   goto      FineSr
045100020212 2-  C                   enddo
045200020215     C                   endif
045300020212      *
045400020212 2a  C  n21              do
045500020212      * Controllo che il record non esista già in tabella
045600020212     C                   movel     *blanks       TbeKe1
045700020212     C                   movel     *blanks       TbeKe2
045800020212     C                   move      v1ccod        $Campo7
045900020212     C                   movel     $Campo7       TbeKe1
046000020212     C                   move      v1cp02        $Campo3           3
046100020212     C                   movel     $Campo3       TbeKe2
046200020212     C                   exsr      ChnTbe
046300020212 3a  C  n23              do
046400020212     C                   seton                                        2852
046500020212     C                   movel     msg(3)        V1cmsg
046600020212     C                   goto      FineSr
046700020212 3-  C                   enddo
046800020212      *
046900020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
047000020214     C                   move      v1cp02        $Pou2             3 0
047100020212     C                   exsr      CtrPos
047200020212 3a  C   24              do
047300020212     C                   seton                                        285242
047400020215     C                   movel     msg(5)        V1cmsg
047500020212     C                   goto      FineSr
047600020212 3-  C                   enddo
047700020212 2-  C                   enddo
047800020215 1-  C                   endif
047900020212      *
048000020215 1a  C     v1cp02        ifeq      999
048100020212      * Controllo che il cliente non mantenga il numero spedizione
048200020212     C                   move      v1ccod        $Campo7
048300020212     C                   Exsr      CtrSpe
048400020215 2a  C   25              do
048500020212     C                   seton                                        2852
048600020212     C                   movel     msg(4)        V1cmsg
048700020212     C                   goto      FineSr
048800020215 2-  C                   enddo
048900020320      *
049000020320     C     §3CCtm        ifne      *blanks
049100020320      * Controllo anche che il cliente non abbia un codice trattamento
049200020320      * tipo DISK 'B'
049300020320     C                   Exsr      CtrCdT
049400020320 2a  C   25              do
049500020320     C                   seton                                        2852
049600020320     C                   movel     msg(6)        V1cmsg
049700020320     C                   goto      FineSr
049800020320 2-  C                   enddo
049900020320 1-  C                   endif
050000020215 1-  C                   endif
050100020212      *
050200020212      *
050300020212 1a  C     v1cp03        ifne      *zeros
050400020215     C     v1cp03        ifne      999
050500020212     C     v1cp03        chain     Azorg01l                           21
050600020212 2a  C   21              do
050700020212     C                   seton                                        2853
050800020212     C                   movel     msg(2)        V1cmsg
050900020212     C                   goto      FineSr
051000020212 2-  C                   enddo
051100020215     C                   endif
051200020212      *
051300020212 2a  C  n21              do
051400020212      * Controllo che il record non esista già in tabella
051500020212     C                   movel     *blanks       TbeKe1
051600020212     C                   movel     *blanks       TbeKe2
051700020212     C                   move      v1ccod        $Campo7
051800020212     C                   movel     $Campo7       TbeKe1
051900020212     C                   move      v1cp03        $Campo3           3
052000020212     C                   movel     $Campo3       TbeKe2
052100020212     C                   exsr      ChnTbe
052200020212 3a  C  n23              do
052300020212     C                   seton                                        2853
052400020212     C                   movel     msg(3)        V1cmsg
052500020212     C                   goto      FineSr
052600020212 3-  C                   enddo
052700020212      *
052800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
052900020214     C                   move      v1cp03        $Pou2             3 0
053000020212     C                   exsr      CtrPos
053100020212 3a  C   24              do
053200020212     C                   seton                                        285342
053300020215     C                   movel     msg(5)        V1cmsg
053400020212     C                   goto      FineSr
053500020212 3-  C                   enddo
053600020212 2-  C                   enddo
053700020215     C                   endif
053800020212      *
053900020215 1a  C     v1cp03        ifeq      999
054000020212      * Controllo che il cliente non mantenga il numero spedizione
054100020212     C                   move      v1ccod        $Campo7
054200020212     C                   Exsr      CtrSpe
054300020215 2a  C   25              do
054400020212     C                   seton                                        2853
054500020212     C                   movel     msg(4)        V1cmsg
054600020212     C                   goto      FineSr
054700020215 2-  C                   enddo
054800020320      *
054900020320     C     §3CCtm        ifne      *blanks
055000020320      * Controllo anche che il cliente non abbia un codice trattamento
055100020320      * tipo DISK 'B'
055200020320     C                   Exsr      CtrCdT
055300020320 2a  C   25              do
055400020320     C                   seton                                        2853
055500020320     C                   movel     msg(6)        V1cmsg
055600020320     C                   goto      FineSr
055700020320 2-  C                   enddo
055800020320 1-  C                   endif
055900020212 1-  C                   endif
056000020212      *
056100020212 1a  C     v1cp04        ifne      *zeros
056200020215     C     v1cp04        ifne      999
056300020212     C     v1cp04        chain     Azorg01l                           21
056400020212 2a  C   21              do
056500020212     C                   seton                                        2854
056600020212     C                   movel     msg(2)        V1cmsg
056700020212     C                   goto      FineSr
056800020212 2-  C                   enddo
056900020215     C                   endif
057000020212      *
057100020212 2a  C  n21              do
057200020212      * Controllo che il record non esista già in tabella
057300020212     C                   movel     *blanks       TbeKe1
057400020212     C                   movel     *blanks       TbeKe2
057500020212     C                   move      v1ccod        $Campo7
057600020212     C                   movel     $Campo7       TbeKe1
057700020212     C                   move      v1cp04        $Campo3           3
057800020212     C                   movel     $Campo3       TbeKe2
057900020212     C                   exsr      ChnTbe
058000020212 3a  C  n23              do
058100020212     C                   seton                                        2854
058200020212     C                   movel     msg(3)        V1cmsg
058300020212     C                   goto      FineSr
058400020212 3-  C                   enddo
058500020212      *
058600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
058700020214     C                   move      v1cp04        $Pou2             3 0
058800020212     C                   exsr      CtrPos
058900020212 3a  C   24              do
059000020212     C                   seton                                        285442
059100020215     C                   movel     msg(5)        V1cmsg
059200020212     C                   goto      FineSr
059300020212 3-  C                   enddo
059400020212 2-  C                   enddo
059500020215     C                   endif
059600020212      *
059700020215 1a  C     v1cp04        ifeq      999
059800020212      * Controllo che il cliente non mantenga il numero spedizione
059900020212     C                   move      v1ccod        $Campo7
060000020212     C                   Exsr      CtrSpe
060100020215 2a  C   25              do
060200020212     C                   seton                                        2854
060300020212     C                   movel     msg(4)        V1cmsg
060400020212     C                   goto      FineSr
060500020215 2-  C                   enddo
060600020320      *
060700020320     C     §3CCtm        ifne      *blanks
060800020320      * Controllo anche che il cliente non abbia un codice trattamento
060900020320      * tipo DISK 'B'
061000020320     C                   Exsr      CtrCdT
061100020320 2a  C   25              do
061200020320     C                   seton                                        2854
061300020320     C                   movel     msg(6)        V1cmsg
061400020320     C                   goto      FineSr
061500020320 2-  C                   enddo
061600020320 1-  C                   endif
061700020215 1-  C                   endif
061800020212      *
061900020212 1a  C     v1cp05        ifne      *zeros
062000020215     C     v1cp05        ifne      999
062100020212     C     v1cp05        chain     Azorg01l                           21
062200020212 2a  C   21              do
062300020212     C                   seton                                        2855
062400020212     C                   movel     msg(2)        V1cmsg
062500020212     C                   goto      FineSr
062600020212 2-  C                   enddo
062700020215     C                   endif
062800020212      *
062900020212 2a  C  n21              do
063000020212      * Controllo che il record non esista già in tabella
063100020212     C                   movel     *blanks       TbeKe1
063200020212     C                   movel     *blanks       TbeKe2
063300020212     C                   move      v1ccod        $Campo7
063400020212     C                   movel     $Campo7       TbeKe1
063500020212     C                   move      v1cp05        $Campo3           3
063600020212     C                   movel     $Campo3       TbeKe2
063700020212     C                   exsr      ChnTbe
063800020212 3a  C  n23              do
063900020212     C                   seton                                        2855
064000020212     C                   movel     msg(3)        V1cmsg
064100020212     C                   goto      FineSr
064200020212 3-  C                   enddo
064300020212      *
064400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
064500020214     C                   move      v1cp05        $Pou2             3 0
064600020212     C                   exsr      CtrPos
064700020212 3a  C   24              do
064800020212     C                   seton                                        285542
064900020215     C                   movel     msg(5)        V1cmsg
065000020212     C                   goto      FineSr
065100020212 3-  C                   enddo
065200020212 2-  C                   enddo
065300020215     C                   endif
065400020212      *
065500020215 1a  C     v1cp05        ifeq      999
065600020212      * Controllo che il cliente non mantenga il numero spedizione
065700020212     C                   move      v1ccod        $Campo7
065800020212     C                   Exsr      CtrSpe
065900020215 2a  C   25              do
066000020212     C                   seton                                        2855
066100020212     C                   movel     msg(4)        V1cmsg
066200020212     C                   goto      FineSr
066300020215 2-  C                   enddo
066400020320      *
066500020320     C     §3CCtm        ifne      *blanks
066600020320      * Controllo anche che il cliente non abbia un codice trattamento
066700020320      * tipo DISK 'B'
066800020320     C                   Exsr      CtrCdT
066900020320 2a  C   25              do
067000020320     C                   seton                                        2855
067100020320     C                   movel     msg(6)        V1cmsg
067200020320     C                   goto      FineSr
067300020320 2-  C                   enddo
067400020320 1-  C                   endif
067500020215 1-  C                   endif
067600020212      *
067700020212 1a  C     v1cp06        ifne      *zeros
067800020215     C     v1cp06        ifne      999
067900020212     C     v1cp06        chain     Azorg01l                           21
068000020212 2a  C   21              do
068100020212     C                   seton                                        2856
068200020212     C                   movel     msg(2)        V1cmsg
068300020212     C                   goto      FineSr
068400020212 2-  C                   enddo
068500020215     C                   endif
068600020212      *
068700020212 2a  C  n21              do
068800020212      * Controllo che il record non esista già in tabella
068900020212     C                   movel     *blanks       TbeKe1
069000020212     C                   movel     *blanks       TbeKe2
069100020212     C                   move      v1ccod        $Campo7
069200020212     C                   movel     $Campo7       TbeKe1
069300020212     C                   move      v1cp06        $Campo3           3
069400020212     C                   movel     $Campo3       TbeKe2
069500020212     C                   exsr      ChnTbe
069600020212 3a  C  n23              do
069700020212     C                   seton                                        2856
069800020212     C                   movel     msg(3)        V1cmsg
069900020212     C                   goto      FineSr
070000020212 3-  C                   enddo
070100020212      *
070200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
070300020214     C                   move      v1cp06        $Pou2             3 0
070400020212     C                   exsr      CtrPos
070500020212 3a  C   24              do
070600020212     C                   seton                                        285642
070700020215     C                   movel     msg(5)        V1cmsg
070800020212     C                   goto      FineSr
070900020212 3-  C                   enddo
071000020212 2-  C                   enddo
071100020215 1-  C                   endif
071200020215      *
071300020215 1a  C     v1cp06        ifeq      999
071400020212      * Controllo che il cliente non mantenga il numero spedizione
071500020212     C                   move      v1ccod        $Campo7
071600020212     C                   Exsr      CtrSpe
071700020215 2a  C   25              do
071800020212     C                   seton                                        2856
071900020212     C                   movel     msg(4)        V1cmsg
072000020212     C                   goto      FineSr
072100020215 2-  C                   enddo
072200020320      *
072300020320     C     §3CCtm        ifne      *blanks
072400020320      * Controllo anche che il cliente non abbia un codice trattamento
072500020320      * tipo DISK 'B'
072600020320     C                   Exsr      CtrCdT
072700020320 2a  C   25              do
072800020320     C                   seton                                        2856
072900020320     C                   movel     msg(6)        V1cmsg
073000020320     C                   goto      FineSr
073100020320 2-  C                   enddo
073200020320 1-  C                   endif
073300020215 1-  C                   endif
073400020212      *
073500020212 1a  C     v1cp07        ifne      *zeros
073600020215     C     v1cp07        ifne      999
073700020212     C     v1cp07        chain     Azorg01l                           21
073800020212 2a  C   21              do
073900020212     C                   seton                                        2857
074000020212     C                   movel     msg(2)        V1cmsg
074100020212     C                   goto      FineSr
074200020212 2-  C                   enddo
074300020215     C                   endif
074400020212      *
074500020212 2a  C  n21              do
074600020212      * Controllo che il record non esista già in tabella
074700020212     C                   movel     *blanks       TbeKe1
074800020212     C                   movel     *blanks       TbeKe2
074900020212     C                   move      v1ccod        $Campo7
075000020212     C                   movel     $Campo7       TbeKe1
075100020212     C                   move      v1cp07        $Campo3           3
075200020212     C                   movel     $Campo3       TbeKe2
075300020212     C                   exsr      ChnTbe
075400020212 3a  C  n23              do
075500020212     C                   seton                                        2857
075600020212     C                   movel     msg(3)        V1cmsg
075700020212     C                   goto      FineSr
075800020212 3-  C                   enddo
075900020212      *
076000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
076100020214     C                   move      v1cp07        $Pou2             3 0
076200020212     C                   exsr      CtrPos
076300020212 3a  C   24              do
076400020212     C                   seton                                        285742
076500020215     C                   movel     msg(5)        V1cmsg
076600020212     C                   goto      FineSr
076700020212 3-  C                   enddo
076800020212 2-  C                   enddo
076900020215     C                   endif
077000020215      *
077100020215 1a  C     v1cp07        ifeq      999
077200020212      * Controllo che il cliente non mantenga il numero spedizione
077300020212     C                   move      v1ccod        $Campo7
077400020212     C                   Exsr      CtrSpe
077500020215 2a  C   25              do
077600020212     C                   seton                                        2857
077700020212     C                   movel     msg(4)        V1cmsg
077800020212     C                   goto      FineSr
077900020215 2-  C                   enddo
078000020320      *
078100020320     C     §3CCtm        ifne      *blanks
078200020320      * Controllo anche che il cliente non abbia un codice trattamento
078300020320      * tipo DISK 'B'
078400020320     C                   Exsr      CtrCdT
078500020320 2a  C   25              do
078600020320     C                   seton                                        2857
078700020320     C                   movel     msg(6)        V1cmsg
078800020320     C                   goto      FineSr
078900020320 2-  C                   enddo
079000020320 1-  C                   endif
079100020215 1-  C                   endif
079200020212      *
079300020212 1a  C     v1cp08        ifne      *zeros
079400020215     C     v1cp08        ifne      999
079500020212     C     v1cp08        chain     Azorg01l                           21
079600020212 2a  C   21              do
079700020212     C                   seton                                        2858
079800020212     C                   movel     msg(2)        V1cmsg
079900020212     C                   goto      FineSr
080000020212 2-  C                   enddo
080100020215     C                   endif
080200020212      *
080300020212 2a  C  n21              do
080400020212      * Controllo che il record non esista già in tabella
080500020212     C                   movel     *blanks       TbeKe1
080600020212     C                   movel     *blanks       TbeKe2
080700020212     C                   move      v1ccod        $Campo7
080800020212     C                   movel     $Campo7       TbeKe1
080900020212     C                   move      v1cp08        $Campo3           3
081000020212     C                   movel     $Campo3       TbeKe2
081100020212     C                   exsr      ChnTbe
081200020212 3a  C  n23              do
081300020212     C                   seton                                        2858
081400020212     C                   movel     msg(3)        V1cmsg
081500020212     C                   goto      FineSr
081600020212 3-  C                   enddo
081700020212      *
081800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
081900020214     C                   move      v1cp08        $Pou2             3 0
082000020212     C                   exsr      CtrPos
082100020212 3a  C   24              do
082200020212     C                   seton                                        285842
082300020215     C                   movel     msg(5)        V1cmsg
082400020212     C                   goto      FineSr
082500020212 3-  C                   enddo
082600020212 2-  C                   enddo
082700020215 1-  C                   endif
082800020215      *
082900020215 1a  C     v1cp08        ifeq      999
083000020212      * Controllo che il cliente non mantenga il numero spedizione
083100020212     C                   move      v1ccod        $Campo7
083200020212     C                   Exsr      CtrSpe
083300020215 2a  C   25              do
083400020212     C                   seton                                        2858
083500020212     C                   movel     msg(4)        V1cmsg
083600020212     C                   goto      FineSr
083700020215 2-  C                   enddo
083800020320      *
083900020320     C     §3CCtm        ifne      *blanks
084000020320      * Controllo anche che il cliente non abbia un codice trattamento
084100020320      * tipo DISK 'B'
084200020320     C                   Exsr      CtrCdT
084300020320 2a  C   25              do
084400020320     C                   seton                                        2858
084500020320     C                   movel     msg(6)        V1cmsg
084600020320     C                   goto      FineSr
084700020320 2-  C                   enddo
084800020320 1-  C                   endif
084900020215 1-  C                   endif
085000020212      *
085100020212 1a  C     v1cp09        ifne      *zeros
085200020215     C     v1cp09        ifne      999
085300020212     C     v1cp09        chain     Azorg01l                           21
085400020212 2a  C   21              do
085500020212     C                   seton                                        2859
085600020212     C                   movel     msg(2)        V1cmsg
085700020212     C                   goto      FineSr
085800020212 2-  C                   enddo
085900020215 1-  C                   endif
086000020212      *
086100020212 2a  C  n21              do
086200020212      * Controllo che il record non esista già in tabella
086300020212     C                   movel     *blanks       TbeKe1
086400020212     C                   movel     *blanks       TbeKe2
086500020212     C                   move      v1ccod        $Campo7
086600020212     C                   movel     $Campo7       TbeKe1
086700020212     C                   move      v1cp09        $Campo3           3
086800020212     C                   movel     $Campo3       TbeKe2
086900020212     C                   exsr      ChnTbe
087000020212 3a  C  n23              do
087100020212     C                   seton                                        2859
087200020212     C                   movel     msg(3)        V1cmsg
087300020212     C                   goto      FineSr
087400020212 3-  C                   enddo
087500020212      *
087600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
087700020214     C                   move      v1cp09        $Pou2             3 0
087800020212     C                   exsr      CtrPos
087900020212 3a  C   24              do
088000020212     C                   seton                                        285942
088100020215     C                   movel     msg(5)        V1cmsg
088200020212     C                   goto      FineSr
088300020212 3-  C                   enddo
088400020212 2-  C                   enddo
088500020215 1-  C                   endif
088600020215      *
088700020215 1a  C     v1cp09        ifeq      999
088800020212      * Controllo che il cliente non mantenga il numero spedizione
088900020212     C                   move      v1ccod        $Campo7
089000020212     C                   Exsr      CtrSpe
089100020215 2a  C   25              do
089200020212     C                   seton                                        2859
089300020212     C                   movel     msg(4)        V1cmsg
089400020212     C                   goto      FineSr
089500020215 2-  C                   enddo
089600020320      *
089700020320     C     §3CCtm        ifne      *blanks
089800020320      * Controllo anche che il cliente non abbia un codice trattamento
089900020320      * tipo DISK 'B'
090000020320     C                   Exsr      CtrCdT
090100020320 2a  C   25              do
090200020320     C                   seton                                        2859
090300020320     C                   movel     msg(6)        V1cmsg
090400020320     C                   goto      FineSr
090500020320 2-  C                   enddo
090600020320 1-  C                   endif
090700020215 1-  C                   endif
090800020212      *
090900020212 1a  C     v1cp10        ifne      *zeros
091000020215     C     v1cp10        ifne      999
091100020212     C     v1cp10        chain     Azorg01l                           21
091200020212 2a  C   21              do
091300020212     C                   seton                                        2860
091400020212     C                   movel     msg(2)        V1cmsg
091500020212     C                   goto      FineSr
091600020212 2-  C                   enddo
091700020215     C                   endif
091800020212      *
091900020212 2a  C  n21              do
092000020212      * Controllo che il record non esista già in tabella
092100020212     C                   movel     *blanks       TbeKe1
092200020212     C                   movel     *blanks       TbeKe2
092300020212     C                   move      v1ccod        $Campo7
092400020212     C                   movel     $Campo7       TbeKe1
092500020212     C                   move      v1cp10        $Campo3           3
092600020212     C                   movel     $Campo3       TbeKe2
092700020212     C                   exsr      ChnTbe
092800020212 3a  C  n23              do
092900020212     C                   seton                                        2860
093000020212     C                   movel     msg(3)        V1cmsg
093100020212     C                   goto      FineSr
093200020212 3-  C                   enddo
093300020212      *
093400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
093500020214     C                   move      v1cp10        $Pou2             3 0
093600020212     C                   exsr      CtrPos
093700020212 3a  C   24              do
093800020212     C                   seton                                        286042
093900020215     C                   movel     msg(5)        V1cmsg
094000020212     C                   goto      FineSr
094100020212 3-  C                   enddo
094200020212 2-  C                   enddo
094300020215     C                   endif
094400020215      *
094500020215 1a  C     v1cp10        ifeq      999
094600020212      * Controllo che il cliente non mantenga il numero spedizione
094700020212     C                   move      v1ccod        $Campo7
094800020212     C                   Exsr      CtrSpe
094900020215 2a  C   25              do
095000020212     C                   seton                                        2860
095100020212     C                   movel     msg(4)        V1cmsg
095200020212     C                   goto      FineSr
095300020215 2-  C                   enddo
095400020320      *
095500020320     C     §3CCtm        ifne      *blanks
095600020320      * Controllo anche che il cliente non abbia un codice trattamento
095700020320      * tipo DISK 'B'
095800020320     C                   Exsr      CtrCdT
095900020320 2a  C   25              do
096000020320     C                   seton                                        2860
096100020320     C                   movel     msg(6)        V1cmsg
096200020320     C                   goto      FineSr
096300020320 2-  C                   enddo
096400020320 1-  C                   endif
096500020215 1-  C                   endif
096600020212      *
096700020212 1a  C     v1cp11        ifne      *zeros
096800020215     C     v1cp11        ifne      999
096900020212     C     v1cp11        chain     Azorg01l                           21
097000020212 2a  C   21              do
097100020212     C                   seton                                        2861
097200020212     C                   movel     msg(2)        V1cmsg
097300020212     C                   goto      FineSr
097400020212 2-  C                   enddo
097500020215 1-  C                   endif
097600020212      *
097700020212 2a  C  n21              do
097800020212      * Controllo che il record non esista già in tabella
097900020212     C                   movel     *blanks       TbeKe1
098000020212     C                   movel     *blanks       TbeKe2
098100020212     C                   move      v1ccod        $Campo7
098200020212     C                   movel     $Campo7       TbeKe1
098300020212     C                   move      v1cp11        $Campo3           3
098400020212     C                   movel     $Campo3       TbeKe2
098500020212     C                   exsr      ChnTbe
098600020212 3a  C  n23              do
098700020212     C                   seton                                        2861
098800020212     C                   movel     msg(3)        V1cmsg
098900020212     C                   goto      FineSr
099000020212 3-  C                   enddo
099100020212      *
099200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
099300020214     C                   move      v1cp11        $Pou2             3 0
099400020212     C                   exsr      CtrPos
099500020212 3a  C   24              do
099600020212     C                   seton                                        286142
099700020215     C                   movel     msg(5)        V1cmsg
099800020212     C                   goto      FineSr
099900020212 3-  C                   enddo
100000020212 2-  C                   enddo
100100020215     C                   endif
100200020215      *
100300020215 1a  C     v1cp11        ifeq      999
100400020212      * Controllo che il cliente non mantenga il numero spedizione
100500020212     C                   move      v1ccod        $Campo7
100600020212     C                   Exsr      CtrSpe
100700020215 2a  C   25              do
100800020212     C                   seton                                        2861
100900020212     C                   movel     msg(4)        V1cmsg
101000020212     C                   goto      FineSr
101100020215 2-  C                   enddo
101200020320      *
101300020320     C     §3CCtm        ifne      *blanks
101400020320      * Controllo anche che il cliente non abbia un codice trattamento
101500020320      * tipo DISK 'B'
101600020320     C                   Exsr      CtrCdT
101700020320 2a  C   25              do
101800020320     C                   seton                                        2861
101900020320     C                   movel     msg(6)        V1cmsg
102000020320     C                   goto      FineSr
102100020320 2-  C                   enddo
102200020320 1-  C                   endif
102300020215 1-  C                   endif
102400020212      *
102500020212 1a  C     v1cp12        ifne      *zeros
102600020215     C     v1cp12        ifne      999
102700020212     C     v1cp12        chain     Azorg01l                           21
102800020212 2a  C   21              do
102900020212     C                   seton                                        2862
103000020212     C                   movel     msg(2)        V1cmsg
103100020212     C                   goto      FineSr
103200020212 2-  C                   enddo
103300020215     C                   endif
103400020212      *
103500020212 2a  C  n21              do
103600020212      * Controllo che il record non esista già in tabella
103700020212     C                   movel     *blanks       TbeKe1
103800020212     C                   movel     *blanks       TbeKe2
103900020212     C                   move      v1ccod        $Campo7
104000020212     C                   movel     $Campo7       TbeKe1
104100020212     C                   move      v1cp12        $Campo3           3
104200020212     C                   movel     $Campo3       TbeKe2
104300020212     C                   exsr      ChnTbe
104400020212 3a  C  n23              do
104500020212     C                   seton                                        2862
104600020212     C                   movel     msg(3)        V1cmsg
104700020212     C                   goto      FineSr
104800020212 3-  C                   enddo
104900020212      *
105000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
105100020214     C                   move      v1cp12        $Pou2             3 0
105200020212     C                   exsr      CtrPos
105300020212 3a  C   24              do
105400020212     C                   seton                                        286242
105500020215     C                   movel     msg(5)        V1cmsg
105600020212     C                   goto      FineSr
105700020212 3-  C                   enddo
105800020212 2-  C                   enddo
105900020215     C                   endif
106000020215      *
106100020215 1a  C     v1cp12        ifeq      999
106200020212      * Controllo che il cliente non mantenga il numero spedizione
106300020212     C                   move      v1ccod        $Campo7
106400020212     C                   Exsr      CtrSpe
106500020215 2a  C   25              do
106600020212     C                   seton                                        2862
106700020212     C                   movel     msg(4)        V1cmsg
106800020212     C                   goto      FineSr
106900020215 2-  C                   enddo
107000020320      *
107100020320     C     §3CCtm        ifne      *blanks
107200020320      * Controllo anche che il cliente non abbia un codice trattamento
107300020320      * tipo DISK 'B'
107400020320     C                   Exsr      CtrCdT
107500020320 2a  C   25              do
107600020320     C                   seton                                        2862
107700020320     C                   movel     msg(6)        V1cmsg
107800020320     C                   goto      FineSr
107900020320 2-  C                   enddo
108000020320 1-  C                   endif
108100020215 1-  C                   endif
108200020212      *
108300020212 1a  C     v1cp13        ifne      *zeros
108400020215     C     v1cp13        ifne      999
108500020212     C     v1cp13        chain     Azorg01l                           21
108600020212 2a  C   21              do
108700020212     C                   seton                                        2863
108800020212     C                   movel     msg(2)        V1cmsg
108900020212     C                   goto      FineSr
109000020212 2-  C                   enddo
109100020215 1-  C                   endif
109200020212      *
109300020212 2a  C  n21              do
109400020212      * Controllo che il record non esista già in tabella
109500020212     C                   movel     *blanks       TbeKe1
109600020212     C                   movel     *blanks       TbeKe2
109700020212     C                   move      v1ccod        $Campo7
109800020212     C                   movel     $Campo7       TbeKe1
109900020212     C                   move      v1cp13        $Campo3           3
110000020212     C                   movel     $Campo3       TbeKe2
110100020212     C                   exsr      ChnTbe
110200020212 3a  C  n23              do
110300020212     C                   seton                                        2863
110400020212     C                   movel     msg(3)        V1cmsg
110500020212     C                   goto      FineSr
110600020212 3-  C                   enddo
110700020212      *
110800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
110900020214     C                   move      v1cp13        $Pou2             3 0
111000020212     C                   exsr      CtrPos
111100020212 3a  C   24              do
111200020212     C                   seton                                        286342
111300020215     C                   movel     msg(5)        V1cmsg
111400020212     C                   goto      FineSr
111500020212 3-  C                   enddo
111600020212 2-  C                   enddo
111700020215     C                   endif
111800020215      *
111900020215 1a  C     v1cp13        ifeq      999
112000020212      * Controllo che il cliente non mantenga il numero spedizione
112100020212     C                   move      v1ccod        $Campo7
112200020212     C                   Exsr      CtrSpe
112300020215 2a  C   25              do
112400020212     C                   seton                                        2863
112500020212     C                   movel     msg(4)        V1cmsg
112600020212     C                   goto      FineSr
112700020215 2-  C                   enddo
112800020320      *
112900020320     C     §3CCtm        ifne      *blanks
113000020320      * Controllo anche che il cliente non abbia un codice trattamento
113100020320      * tipo DISK 'B'
113200020320     C                   Exsr      CtrCdT
113300020320 2a  C   25              do
113400020320     C                   seton                                        2863
113500020320     C                   movel     msg(6)        V1cmsg
113600020320     C                   goto      FineSr
113700020320 2-  C                   enddo
113800020320 1-  C                   endif
113900020215 1-  C                   endif
114000020212      *
114100020212 1a  C     v1cp14        ifne      *zeros
114200020215     C     v1cp14        ifne      999
114300020212     C     v1cp14        chain     Azorg01l                           21
114400020212 2a  C   21              do
114500020212     C                   seton                                        2864
114600020212     C                   movel     msg(2)        V1cmsg
114700020212     C                   goto      FineSr
114800020212 2-  C                   enddo
114900020215     C                   endif
115000020212      *
115100020212 2a  C  n21              do
115200020212      * Controllo che il record non esista già in tabella
115300020212     C                   movel     *blanks       TbeKe1
115400020212     C                   movel     *blanks       TbeKe2
115500020212     C                   move      v1ccod        $Campo7
115600020212     C                   movel     $Campo7       TbeKe1
115700020212     C                   move      v1cp14        $Campo3           3
115800020212     C                   movel     $Campo3       TbeKe2
115900020212     C                   exsr      ChnTbe
116000020212 3a  C  n23              do
116100020212     C                   seton                                        2864
116200020212     C                   movel     msg(3)        V1cmsg
116300020212     C                   goto      FineSr
116400020212 3-  C                   enddo
116500020212      *
116600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
116700020214     C                   move      v1cp14        $Pou2             3 0
116800020212     C                   exsr      CtrPos
116900020212 3a  C   24              do
117000020212     C                   seton                                        286442
117100020215     C                   movel     msg(5)        V1cmsg
117200020212     C                   goto      FineSr
117300020212 3-  C                   enddo
117400020212 2-  C                   enddo
117500020215     C                   endif
117600020215      *
117700020215 1a  C     v1cp14        ifeq      999
117800020212      * Controllo che il cliente non mantenga il numero spedizione
117900020212     C                   move      v1ccod        $Campo7
118000020212     C                   Exsr      CtrSpe
118100020215 2a  C   25              do
118200020212     C                   seton                                        2864
118300020212     C                   movel     msg(4)        V1cmsg
118400020212     C                   goto      FineSr
118500020215 2-  C                   enddo
118600020320      *
118700020320     C     §3CCtm        ifne      *blanks
118800020320      * Controllo anche che il cliente non abbia un codice trattamento
118900020320      * tipo DISK 'B'
119000020320     C                   Exsr      CtrCdT
119100020320 2a  C   25              do
119200020320     C                   seton                                        2864
119300020320     C                   movel     msg(6)        V1cmsg
119400020320     C                   goto      FineSr
119500020320 2-  C                   enddo
119600020320 1-  C                   endif
119700020215 1-  C                   endif
119800020212      *
119900020212 1a  C     v1cp15        ifne      *zeros
120000020215     C     v1cp15        ifne      999
120100020212     C     v1cp15        chain     Azorg01l                           21
120200020212 2a  C   21              do
120300020212     C                   seton                                        2865
120400020212     C                   movel     msg(2)        V1cmsg
120500020212     C                   goto      FineSr
120600020212 2-  C                   enddo
120700020215     C                   endif
120800020212      *
120900020212 2a  C  n21              do
121000020212      * Controllo che il record non esista già in tabella
121100020212     C                   movel     *blanks       TbeKe1
121200020212     C                   movel     *blanks       TbeKe2
121300020212     C                   move      v1ccod        $Campo7
121400020212     C                   movel     $Campo7       TbeKe1
121500020212     C                   move      v1cp15        $Campo3           3
121600020212     C                   movel     $Campo3       TbeKe2
121700020212     C                   exsr      ChnTbe
121800020212 3a  C  n23              do
121900020212     C                   seton                                        2865
122000020212     C                   movel     msg(3)        V1cmsg
122100020212     C                   goto      FineSr
122200020212 3-  C                   enddo
122300020212      *
122400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
122500020214     C                   move      v1cp15        $Pou2             3 0
122600020212     C                   exsr      CtrPos
122700020212 3a  C   24              do
122800020212     C                   seton                                        286542
122900020215     C                   movel     msg(5)        V1cmsg
123000020212     C                   goto      FineSr
123100020212 3-  C                   enddo
123200020212 2-  C                   enddo
123300020215     C                   endif
123400020215      *
123500020215 1a  C     v1cp15        ifeq      999
123600020212      * Controllo che il cliente non mantenga il numero spedizione
123700020212     C                   move      v1ccod        $Campo7
123800020212     C                   Exsr      CtrSpe
123900020215 2a  C   25              do
124000020212     C                   seton                                        2865
124100020212     C                   movel     msg(4)        V1cmsg
124200020212     C                   goto      FineSr
124300020215 2-  C                   enddo
124400020320      *
124500020320     C     §3CCtm        ifne      *blanks
124600020320      * Controllo anche che il cliente non abbia un codice trattamento
124700020320      * tipo DISK 'B'
124800020320     C                   Exsr      CtrCdT
124900020320 2a  C   25              do
125000020320     C                   seton                                        2865
125100020320     C                   movel     msg(6)        V1cmsg
125200020320     C                   goto      FineSr
125300020320 2-  C                   enddo
125400020320 1-  C                   endif
125500020215 1-  C                   endif
125600020212      *
125700020212 1a  C     v1cp16        ifne      *zeros
125800020215     C     v1cp16        ifne      999
125900020212     C     v1cp16        chain     Azorg01l                           21
126000020212 2a  C   21              do
126100020212     C                   seton                                        2866
126200020212     C                   movel     msg(2)        V1cmsg
126300020212     C                   goto      FineSr
126400020212 2-  C                   enddo
126500020215     C                   endif
126600020212      *
126700020212 2a  C  n21              do
126800020212      * Controllo che il record non esista già in tabella
126900020212     C                   movel     *blanks       TbeKe1
127000020212     C                   movel     *blanks       TbeKe2
127100020212     C                   move      v1ccod        $Campo7
127200020212     C                   movel     $Campo7       TbeKe1
127300020212     C                   move      v1cp16        $Campo3           3
127400020212     C                   movel     $Campo3       TbeKe2
127500020212     C                   exsr      ChnTbe
127600020212 3a  C  n23              do
127700020212     C                   seton                                        2866
127800020212     C                   movel     msg(3)        V1cmsg
127900020212     C                   goto      FineSr
128000020212 3-  C                   enddo
128100020212      *
128200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
128300020214     C                   move      v1cp16        $Pou2             3 0
128400020212     C                   exsr      CtrPos
128500020212 3a  C   24              do
128600020212     C                   seton                                        286642
128700020215     C                   movel     msg(5)        V1cmsg
128800020212     C                   goto      FineSr
128900020212 3-  C                   enddo
129000020212 2-  C                   enddo
129100020215 1-  C                   endif
129200020215      *
129300020215 1a  C     v1cp16        ifeq      999
129400020212      * Controllo che il cliente non mantenga il numero spedizione
129500020212     C                   move      v1ccod        $Campo7
129600020212     C                   Exsr      CtrSpe
129700020215 2a  C   25              do
129800020212     C                   seton                                        2866
129900020212     C                   movel     msg(4)        V1cmsg
130000020212     C                   goto      FineSr
130100020215 2-  C                   enddo
130200020320      *
130300020320     C     §3CCtm        ifne      *blanks
130400020320      * Controllo anche che il cliente non abbia un codice trattamento
130500020320      * tipo DISK 'B'
130600020320     C                   Exsr      CtrCdT
130700020320 2a  C   25              do
130800020320     C                   seton                                        2866
130900020320     C                   movel     msg(6)        V1cmsg
131000020320     C                   goto      FineSr
131100020320 2-  C                   enddo
131200020320 1-  C                   endif
131300020215 1-  C                   endif
131400020212      *
131500020212 1a  C     v1cp17        ifne      *zeros
131600020215     C     v1cp17        ifne      999
131700020212     C     v1cp17        chain     Azorg01l                           21
131800020212 2a  C   21              do
131900020212     C                   seton                                        2867
132000020212     C                   movel     msg(2)        V1cmsg
132100020212     C                   goto      FineSr
132200020212 2-  C                   enddo
132300020215 1-  C                   endif
132400020212      *
132500020212 2a  C  n21              do
132600020212      * Controllo che il record non esista già in tabella
132700020212     C                   movel     *blanks       TbeKe1
132800020212     C                   movel     *blanks       TbeKe2
132900020212     C                   move      v1ccod        $Campo7
133000020212     C                   movel     $Campo7       TbeKe1
133100020212     C                   move      v1cp17        $Campo3           3
133200020212     C                   movel     $Campo3       TbeKe2
133300020212     C                   exsr      ChnTbe
133400020212 3a  C  n23              do
133500020212     C                   seton                                        2867
133600020212     C                   movel     msg(3)        V1cmsg
133700020212     C                   goto      FineSr
133800020212 3-  C                   enddo
133900020212      *
134000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
134100020214     C                   move      v1cp17        $Pou2             3 0
134200020212     C                   exsr      CtrPos
134300020212 3a  C   24              do
134400020212     C                   seton                                        286742
134500020215     C                   movel     msg(5)        V1cmsg
134600020212     C                   goto      FineSr
134700020212 3-  C                   enddo
134800020212 2-  C                   enddo
134900020215     C                   endif
135000020215      *
135100020215 1a  C     v1cp17        ifeq      999
135200020212      * Controllo che il cliente non mantenga il numero spedizione
135300020212     C                   move      v1ccod        $Campo7
135400020212     C                   Exsr      CtrSpe
135500020215 2a  C   25              do
135600020212     C                   seton                                        2867
135700020212     C                   movel     msg(4)        V1cmsg
135800020212     C                   goto      FineSr
135900020215 2-  C                   enddo
136000020320      *
136100020320     C     §3CCtm        ifne      *blanks
136200020320      * Controllo anche che il cliente non abbia un codice trattamento
136300020320      * tipo DISK 'B'
136400020320     C                   Exsr      CtrCdT
136500020320 2a  C   25              do
136600020320     C                   seton                                        2867
136700020320     C                   movel     msg(6)        V1cmsg
136800020320     C                   goto      FineSr
136900020320 2-  C                   enddo
137000020320 1-  C                   endif
137100020215 1-  C                   endif
137200020212      *
137300020212 1a  C     v1cp18        ifne      *zeros
137400020215     C     v1cp18        ifne      999
137500020212     C     v1cp18        chain     Azorg01l                           21
137600020212 2a  C   21              do
137700020212     C                   seton                                        2868
137800020212     C                   movel     msg(2)        V1cmsg
137900020212     C                   goto      FineSr
138000020212 2-  C                   enddo
138100020215 1-  C                   endif
138200020212      *
138300020212 2a  C  n21              do
138400020212      * Controllo che il record non esista già in tabella
138500020212     C                   movel     *blanks       TbeKe1
138600020212     C                   movel     *blanks       TbeKe2
138700020212     C                   move      v1ccod        $Campo7
138800020212     C                   movel     $Campo7       TbeKe1
138900020212     C                   move      v1cp18        $Campo3           3
139000020212     C                   movel     $Campo3       TbeKe2
139100020212     C                   exsr      ChnTbe
139200020212 3a  C  n23              do
139300020212     C                   seton                                        2868
139400020212     C                   movel     msg(3)        V1cmsg
139500020212     C                   goto      FineSr
139600020212 3-  C                   enddo
139700020212      *
139800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
139900020214     C                   move      v1cp18        $Pou2             3 0
140000020212     C                   exsr      CtrPos
140100020212 3a  C   24              do
140200020212     C                   seton                                        286842
140300020215     C                   movel     msg(5)        V1cmsg
140400020212     C                   goto      FineSr
140500020212 3-  C                   enddo
140600020212 2-  C                   enddo
140700020215     C                   endif
140800020215      *
140900020215 1a  C     v1cp18        ifeq      999
141000020212      * Controllo che il cliente non mantenga il numero spedizione
141100020212     C                   move      v1ccod        $Campo7
141200020212     C                   Exsr      CtrSpe
141300020215 2a  C   25              do
141400020212     C                   seton                                        2868
141500020212     C                   movel     msg(4)        V1cmsg
141600020212     C                   goto      FineSr
141700020215 2-  C                   enddo
141800020320      *
141900020320     C     §3CCtm        ifne      *blanks
142000020320      * Controllo anche che il cliente non abbia un codice trattamento
142100020320      * tipo DISK 'B'
142200020320     C                   Exsr      CtrCdT
142300020320 2a  C   25              do
142400020320     C                   seton                                        2868
142500020320     C                   movel     msg(6)        V1cmsg
142600020320     C                   goto      FineSr
142700020320 2-  C                   enddo
142800020320 1-  C                   endif
142900020215 1-  C                   endif
143000020208      *
143100020212 1a  C     v1cp19        ifne      *zeros
143200020215     C     v1cp19        ifne      999
143300020212     C     v1cp19        chain     Azorg01l                           21
143400020212 2a  C   21              do
143500020212     C                   seton                                        2869
143600020212     C                   movel     msg(2)        V1cmsg
143700020212     C                   goto      FineSr
143800020212 2-  C                   enddo
143900020215     C                   endif
144000020212      *
144100020212 2a  C  n21              do
144200020212      * Controllo che il record non esista già in tabella
144300020212     C                   movel     *blanks       TbeKe1
144400020212     C                   movel     *blanks       TbeKe2
144500020212     C                   move      v1ccod        $Campo7
144600020212     C                   movel     $Campo7       TbeKe1
144700020212     C                   move      v1cp19        $Campo3           3
144800020212     C                   movel     $Campo3       TbeKe2
144900020212     C                   exsr      ChnTbe
145000020212 3a  C  n23              do
145100020212     C                   seton                                        2869
145200020212     C                   movel     msg(3)        V1cmsg
145300020212     C                   goto      FineSr
145400020212 3-  C                   enddo
145500020212      *
145600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
145700020214     C                   move      v1cp19        $Pou2             3 0
145800020212     C                   exsr      CtrPos
145900020212 3a  C   24              do
146000020212     C                   seton                                        286942
146100020215     C                   movel     msg(5)        V1cmsg
146200020212     C                   goto      FineSr
146300020212 3-  C                   enddo
146400020212 2-  C                   enddo
146500020215     C                   endif
146600020215      *
146700020215 1a  C     v1cp19        ifeq      999
146800020212      * Controllo che il cliente non mantenga il numero spedizione
146900020212     C                   move      v1ccod        $Campo7
147000020212     C                   Exsr      CtrSpe
147100020215 2a  C   25              do
147200020212     C                   seton                                        2869
147300020212     C                   movel     msg(4)        V1cmsg
147400020212     C                   goto      FineSr
147500020215 2-  C                   enddo
147600020320      *
147700020320     C     §3CCtm        ifne      *blanks
147800020320      * Controllo anche che il cliente non abbia un codice trattamento
147900020320      * tipo DISK 'B'
148000020320     C                   Exsr      CtrCdT
148100020320 2a  C   25              do
148200020320     C                   seton                                        2869
148300020320     C                   movel     msg(6)        V1cmsg
148400020320     C                   goto      FineSr
148500020320 2-  C                   enddo
148600020320 1-  C                   endif
148700020215 1-  C                   endif
148800020212      *
148900020208     C     FineSr        ENDSR
149000020208      *--------------------------------------------------------------------------------------------*
149100020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
149200020208      *--------------------------------------------------------------------------------------------*
149300020208     C     GesVd2        BEGSR
149400020208      *
149500020208      * Inizializzazione videata
149600020208     C     $CarVd2       ifeq      *on
149700020208     C                   EXSR      CarVd2
149800020208     C                   move      *off          $CarVd2
149900020208     C                   endif
150000020208      *
150100091117     c                   write     Tb23t1
150200020215     C                   EXFMT     Tb23v2
150300020208      *
150400020212      * Tasti funzione se non si è in visualizza
150500020212     C     b22Opz        ifne      '5'
150600020212      *
150700020212     C                   select
150800020212     C     *inKl         wheneq    *on
150900020212     C                   EXSR      F12Ges
151000020212     C                   other
151100020212      *
151200020212      * Controlli solo se si è in modifica
151300020212     C     b22Opz        ifeq      '2'
151400020208     C                   EXSR      CtrV2
151500020212     C                   endif
151600020212     C                   endsl
151700020212     C                   endif
151800020208      *
151900020212     C     b22Opz        ifne      '5'
152000020208     C     *IN28         ifeq      *off
152100020212     C     *inKf         andeq     *on
152200020208     C                   EXSR      GesAgg
152300020212     C                   endif
152400020212     C                   else
152500091117     C                   eval      $Fine = *on
152600020212     C                   endif
152700020211      *
152800020208     C                   endsr
152900020208      *--------------------------------------------------------------------------------------------*
153000020208      * CarVd2 - Carica primo video
153100020208      *--------------------------------------------------------------------------------------------*
153200020208     C     CarVd2        BEGSR
153300091117      *
153400091117     C                   clear                   Tb23v2
153500091117      *
153600091117      /free
153700091117         clear  V1Topz;
153800091117         select;
153900091117           when  B22opz = '2';
154000091117             V1Topz  = '   MODIFICA    ';
154100091117             *in60 = *off;
154200091117             *in63 = *off;
154300091117           when  B22opz = '4';
154400091117             V1Topz  = ' ANNULLAMENTO  ';
154500091117             *in60 = *on;
154600091117             *in63 = *off;
154700091117           when  B22opz = '5';
154800091117             V1Topz  = 'VISUALIZZAZIONE';
154900091117             *in60 = *on;
155000091117             *in63 = *on;
155100091117           when  B22opz = '7';
155200091117             V1Topz  = '  RIPRISTINO   ';
155300091117             *in60 = *on;
155400091117             *in63 = *off;
155500091117         endsl;
155600091117
155700091117         // - Reperimento dati tabella
155800091117         exsr  sr_ChainTNTBE;
155900091117
156000091117      /end-free
156100020211      *
156200020215      * Codice e decodifica
156300020215     C                   movel     b22ke1        $campo7           7
156400020215     C                   move      $campo7       V2ccod
156500020215     C                   move      $Campo7       $Cod              7 0
156600020215      *
156700020215     C                   EXSR      DeCod
156800020215     C                   if        o69err = *blanks and acoflg =' '
156900020215     C                   movel     acorag        v2dcod
157000020215     C                   endif
157100020215      *
157200020215      * P.O.
157300020215     C                   move      b22ke2        $campo3
157400020215     C                   movel     $campo3       V2CPou
157500020211      *
157600020211      * Nuovo codice e decodifica
157700020211     C                   movel     TbeUni        $campo7           7
157800020215     C                   move      $Campo7       v2ccli
157900020215      *
158000020215     C                   movel     *blanks       v2dcli
158100030704     c                   if        $CAMPO7 <> *blanks  and
158200030704     c                             $CAMPO7 <> *zeros
158300020211     C                   move      $Campo7       $Cod              7 0
158400020211     C                   EXSR      DeCod
158500020211     C                   if        o69err = *blanks and acoflg =' '
158600020215     C                   movel     acorag        v2dcli
158700020211     C                   endif
158800020215     C                   endif
158900020208      *
159000020208     C                   ENDSR
159100091117      *--------------------------------------------------------------------------------------------*
159200091117      * Lettura file TNTBE02L
159300091117      *--------------------------------------------------------------------------------------------*
159400091117     c     sr_ChainTNTBE BEGSR
159500091117      *
159600091117      * Leggo la tabella
159700091117     C                   move      '3NN'         TbeCod
159800091117     C                   movel     b22lin        TbeLin
159900091117     C                   movel     b22sif        TbeSif
160000091117     C                   movel     *blanks       TbeKe1
160100091117     C                   movel     b22ke1        TbeKe1
160200091117     C                   movel     *blanks       TbeKe2
160300091117     C                   movel     b22ke2        TbeKe2
160400091117     C     k02tbe5       chain     tntbe02l                           21
160500091117      *
160600091117     c                   ENDSR
160700020208      *--------------------------------------------------------------------------------------------*
160800020215      * Controllo seconda videata
160900020208      *--------------------------------------------------------------------------------------------*
161000020208     C     CtrV2         BEGSR
161100020214     C                   setoff                                       2862
161200020214      *
161300020214      * Nuovo codice cliente con cui confermare
161400020215 1a  C     v2dcli        ifne      *blanks
161500020215     C     v2ccli        andeq     *zeros
161600020214      *
161700020214      * Ricerca estesa
161800020214     C
161900091117     C                   movel     rsut          paxdut           30
162000020215     C                   movel(P)  v2dcli        paxdmt           48
162100020214      *
162200020214     C                   z-add     0             paxsta
162300091117     C                   move      DUTkci        paxccc            4 0
162400020214     C                   clear                   paxdit
162500020214     C                   z-add     1             paxnum
162600020214      *
162700020214     C                   CALL      'XALFA3BR'
162800020214     C                   PARM                    PAXDUT
162900091117     C                   PARM      1             CODUT
163000020214     C                   PARM                    PAXDMT
163100020214     C                   PARM                    PAXCCC
163200020214     C                   PARM                    PAXSTA            1 0
163300020214     C                   PARM                    PAXFLR           90
163400020214     C                   PARM                    PAXDIT            3
163500020214     C                   PARM                    PAXNUM            2 0
163600020214     C                   PARM                    PAXKCM           80
163700020214     C                   PARM                    PAXKSM          140
163800020214     C                   PARM                    PAXKDM           60
163900020214      *
164000020214 2a  C     PaxSta        ifgt      -1
164100020214     C                   movel     paxksm        $campo7
164200020215     C                   move      $campo7       v2ccli
164300020215     C                   movel     paxdmt        v2dcli
164400020214 2-  C                   endif
164500020214      *
164600020215     C                   goto      Finesr2
164700020214 1-  C                   endif
164800020214      *
164900020214      * Controllo validità codice
165000020215     C                   move      *blanks       v2dcli
165100020215 1a  C     v2ccli        ifne      *zeros
165200020214      *
165300020215     C                   move      v2ccli        $Cod              7 0
165400020214     C                   EXSR      DeCod
165500020214      *
165600020214 2a  C                   if        o69err = *blanks and acoflg =' '
165700020215     C                   movel     acorag        v2dcli
165800020214      *
165900020215      * Controllo che i due codici cliente siano diversi
166000020215 3a  C     v2ccli        ifeq      v2ccod
166100020214     C                   seton                                        2862
166200020215     C                   goto      Finesr2
166300020214 3-  C                   endif
166400020215      *
166500020215      * Controllo che i due codici cliente siano POSTE/non POSTE
166600020215     C     v2ccod        div       10000         $Pou1
166700020215     C     v2ccli        div       10000         $Pou2
166800020215     C                   exsr      CtrPos
166900020215 3a  C   24              do
167000020215     C                   seton                                        2861
167100020215     C                   goto      FineSr2
167200020215 3-  C                   enddo
167300020215      *
167400020214 2e  C                   else
167500020214     C                   seton                                        2862
167600020214 2-  C                   endif
167700020214 1-  C                   endif
167800020208      *
167900020215     C     Finesr2       ENDSR
168000020208      *--------------------------------------------------------------------------------------------*
168100020208      * Scrittura/ Aggiornamenti
168200020208      *--------------------------------------------------------------------------------------------*
168300020208     C     GesAgg        BEGSR
168400091117      *
168500091117      * Carico in schiera i P.O.
168600091117 1a  C     TipVid        ifeq      '1'
168700091117     C                   exsr      CarPou
168800091117 1-  C                   endif
168900091117      /free
169000091117
169100091117         //? N.B.                                                      ?
169200091117         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
169300091117         //   trasmissione (vedi flag TBEFTR).                        ?
169400091117         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
169500091117         //   subito nello stesso file di entrambi i S.I. - in due    ?
169600091117         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
169700091117
169800091117         // -?Ciclo di elaborazione per ogni sistema informativo?
169900091117         For  w_SisInf = 1  To  %elem($Libr);
170000091117
170100091117           // -?Apertura degli archivi?
170200091117           if  w_SisInf > 1;
170300091117             exsr  sr_OpenFileTab;
170400091117             // - Reperimento dati tabella
170500091117             exsr  sr_ChainTNTBE;
170600091117           endif;
170700091117
170800091117           // -?Inserim./Aggiorn.tab."3NN" = Autorizzaz. Conferma Bolle?  ?
170900091117      /end-free
171000091117      *
171100091117sel 1c                   SELECT
171200091117      *
171300091117      * Gestisco la SCRITTURA per ogni filiale
171400091117w   1c                   WHEN      TipVid  = '1'
171500091117      *
171600091117     C                   z-add     1             X                 2 0
171700091117      *
171800091117 2a  C                   If        $Po999 = *off
171900091117      *
172000091117 3a  C                   dow       Pou(X) <> *zeros
172100091117     C                   exsr      WriTbe
172200091117     C                   add       1             X
172300091117 3-  C                   enddo
172400091117      *
172500091117 2e  C                   Else
172600091117      *
172700091117     C                   move      '999'         Pou(X)
172800091117     C                   exsr      WriTbe
172900091117      *
173000091117 2-  C                   EndIf
173100091117      *
173200091117      * Gestisco l'AGGIORNAMENTO
173300091117w   1c                   WHEN      TipVid = '2'
173400020208      *
173500020211 2a  C                   Select
173600091117      * Modifica
173700091117     C     b22opz        wheneq    '2'
173800020215     C                   move      v2ccli        $cli              7
173900020208     C                   movel     $cli          TbeUni
174000091203     ***C                   move      *blanks       TbeFtr
174100091203      /free
174200091203                 if  w_SisInf = 1;
174300091203                   TBEftr = 'T';
174400091203                 else;
174500091203                   TBEftr = 'R';
174600091203                 endif;
174700091203      /end-free
174800020208     C                   update    tntbe000
174900091117      * Annullamento
175000091117     C     b22opz        wheneq    '4'
175100020208     C                   move      'A'           TbeAtb
175200020208     C                   move      *blanks       TbeFtr
175300020208     C                   update    tntbe000
175400091117      * Ripristino
175500091117     C     b22opz        wheneq    '7'
175600020208     C                   move      ' '           TbeAtb
175700020208     C                   move      *blanks       TbeFtr
175800091203      /free
175900091203               if  %found(TNTBE02L);
176000091203      /end-free
176100020208     C                   update    tntbe000
176200091203      /free
176300091203               else;
176400091203                 // - Record appena cancellato fisicamente
176500091203                 clear  TBEatb;
176600091203                 TBEsif = B22sif;
176700091203                 TBElin = B22lin;
176800091203                 TBEapl = $Apl;
176900091203                 TBEcod = '3NN';
177000091203                 TBEke1 = B22ke1;
177100091203                 TBEke2 = B22ke2;
177200091203                 TBEftt = 'S';
177300091203                 TBEflt = *zero;
177400091203                 TBEftr = *blank;
177500091203                 TBEdtr = *zero;
177600091203                 WRITE  TNTBE01;
177700091203               endif;
177800091203      /end-free
177900020211 2-  C                   endsl
178000091117      *
178100091117e   1C                   ENDSL
178200091117      /free
178300091117         EndFor;
178400091117      /end-free
178500091117     C                   eval      $Fine = *on
178600020212      *
178700020208     C                   ENDSR
178800091117      /free
178900091117
179000091117       //--------------------------------------------------------------
179100091117       //?Apertura dei files tabelle nel sistema informativo impostato.?
179200091117       //--------------------------------------------------------------
179300091117       BEGSR  sr_OpenFileTab;
179400091117
179500091117         // -?Chiusura (eventuale) archivi?
179600091117         if  %open(TNTBE01L);
179700091117           close  TNTBE01L;
179800091117         endif;
179900091117         if  %open(TNTBE02L);
180000091117           close  TNTBE02L;
180100091117         endif;
180200091117
180300091117         // -?Apertura archivi?
180400091117         ds_Libr  = $SisInf(w_iSystem);
180500091117         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
180600091117         open  TNTBE01L;
180700091117         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE02L';
180800091117         open  TNTBE02L;
180900091117
181000091117       ENDSR;
181100091117
181200091117      /end-free
181300020211      *--------------------------------------------------------------------------------------------*
181400020211      * CarPou - Caricamento in schiera dei punti operativi
181500020211      *--------------------------------------------------------------------------------------------*
181600020211     C     CarPou        BEGSR
181700020211     C                   move      *off          $Po999            1
181800020211     C                   z-add     1             X
181900020211     C                   movea     *zeros        Pou
182000020211      *
182100020211     C     V1CP01        ifne      *zeros
182200020211     C     V1CP01        ifeq      999
182300020211     C                   move      *on           $Po999
182400020211     C                   z-add     1             X
182500020211     C                   else
182600020211     C                   move      V1CP01        Pou(X)
182700020211     C                   add       1             X
182800020211     C                   endif
182900020211     C                   endif
183000020211      *
183100020211     C     V1CP02        ifne      *zeros
183200020212     C     $Po999        andne     *on
183300020211     C     V1CP02        ifeq      999
183400020211     C                   move      *on           $Po999
183500020211     C                   z-add     1             X
183600020211     C                   else
183700020215     C                   z-add     1             Y                 2 0
183800020215     C                   move      v1cp02        $campo3
183900020215     C     $campo3       lookup    Pou(y)                                 21
184000020215     C     *in21         ifeq      *off
184100020212     C                   move      V1CP02        Pou(X)
184200020211     C                   add       1             X
184300020211     C                   endif
184400020215     C                   endif
184500020211     C                   endif
184600020211      *
184700020211     C     V1CP03        ifne      *zeros
184800020212     C     $Po999        andne     *on
184900020211     C     V1CP03        ifeq      999
185000020211     C                   move      *on           $Po999
185100020211     C                   z-add     1             X
185200020211     C                   else
185300020215     C                   z-add     1             Y                 2 0
185400020215     C                   move      v1cp03        $campo3
185500020215     C     $campo3       lookup    Pou(y)                                 21
185600020215     C     *in21         ifeq      *off
185700020212     C                   move      V1CP03        Pou(X)
185800020211     C                   add       1             X
185900020211     C                   endif
186000020215     C                   endif
186100020211     C                   endif
186200020211      *
186300020211     C     V1CP04        ifne      *zeros
186400020212     C     $Po999        andne     *on
186500020211     C     V1CP04        ifeq      999
186600020211     C                   move      *on           $Po999
186700020211     C                   z-add     1             X
186800020211     C                   else
186900020215     C                   z-add     1             Y                 2 0
187000020215     C                   move      v1cp04        $campo3
187100020215     C     $campo3       lookup    Pou(y)                                 21
187200020215     C     *in21         ifeq      *off
187300020212     C                   move      V1CP04        Pou(X)
187400020211     C                   add       1             X
187500020211     C                   endif
187600020215     C                   endif
187700020211     C                   endif
187800020211      *
187900020211     C     V1CP05        ifne      *zeros
188000020212     C     $Po999        andne     *on
188100020211     C     V1CP05        ifeq      999
188200020211     C                   move      *on           $Po999
188300020211     C                   z-add     1             X
188400020211     C                   else
188500020215     C                   z-add     1             Y                 2 0
188600020215     C                   move      v1cp05        $campo3
188700020215     C     $campo3       lookup    Pou(y)                                 21
188800020215     C     *in21         ifeq      *off
188900020212     C                   move      V1CP05        Pou(X)
189000020211     C                   add       1             X
189100020211     C                   endif
189200020215     C                   endif
189300020211     C                   endif
189400020211      *
189500020211     C     V1CP06        ifne      *zeros
189600020212     C     $Po999        andne     *on
189700020211     C     V1CP06        ifeq      999
189800020211     C                   move      *on           $Po999
189900020211     C                   z-add     1             X
190000020211     C                   else
190100020215     C                   z-add     1             Y                 2 0
190200020215     C                   move      v1cp06        $campo3
190300020215     C     $campo3       lookup    Pou(y)                                 21
190400020215     C     *in21         ifeq      *off
190500020212     C                   move      V1CP06        Pou(X)
190600020211     C                   add       1             X
190700020211     C                   endif
190800020215     C                   endif
190900020211     C                   endif
191000020211      *
191100020211     C     V1CP07        ifne      *zeros
191200020212     C     $Po999        andne     *on
191300020211     C     V1CP07        ifeq      999
191400020211     C                   move      *on           $Po999
191500020211     C                   z-add     1             X
191600020211     C                   else
191700020215     C                   z-add     1             Y                 2 0
191800020215     C                   move      v1cp07        $campo3
191900020215     C     $campo3       lookup    Pou(y)                                 21
192000020215     C     *in21         ifeq      *off
192100020212     C                   move      V1CP07        Pou(X)
192200020211     C                   add       1             X
192300020211     C                   endif
192400020215     C                   endif
192500020211     C                   endif
192600020211      *
192700020211     C     V1CP08        ifne      *zeros
192800020212     C     $Po999        andne     *on
192900020211     C     V1CP08        ifeq      999
193000020211     C                   move      *on           $Po999
193100020211     C                   z-add     1             X
193200020211     C                   else
193300020215     C                   z-add     1             Y                 2 0
193400020215     C                   move      v1cp08        $campo3
193500020215     C     $campo3       lookup    Pou(y)                                 21
193600020215     C     *in21         ifeq      *off
193700020212     C                   move      V1CP08        Pou(X)
193800020211     C                   add       1             X
193900020211     C                   endif
194000020215     C                   endif
194100020211     C                   endif
194200020211      *
194300020211     C     V1CP09        ifne      *zeros
194400020212     C     $Po999        andne     *on
194500020211     C     V1CP09        ifeq      999
194600020211     C                   move      *on           $Po999
194700020211     C                   z-add     1             X
194800020211     C                   else
194900020215     C                   z-add     1             Y                 2 0
195000020215     C                   move      v1cp09        $campo3
195100020215     C     $campo3       lookup    Pou(y)                                 21
195200020215     C     *in21         ifeq      *off
195300020212     C                   move      V1CP09        Pou(X)
195400020211     C                   add       1             X
195500020211     C                   endif
195600020215     C                   endif
195700020211     C                   endif
195800020211      *
195900020211     C     V1CP10        ifne      *zeros
196000020212     C     $Po999        andne     *on
196100020211     C     V1CP10        ifeq      999
196200020211     C                   move      *on           $Po999
196300020211     C                   z-add     1             X
196400020211     C                   else
196500020215     C                   z-add     1             Y                 2 0
196600020215     C                   move      v1cp10        $campo3
196700020215     C     $campo3       lookup    Pou(y)                                 21
196800020215     C     *in21         ifeq      *off
196900020212     C                   move      V1CP10        Pou(X)
197000020211     C                   add       1             X
197100020211     C                   endif
197200020215     C                   endif
197300020211     C                   endif
197400020211      *
197500020211     C     V1CP11        ifne      *zeros
197600020212     C     $Po999        andne     *on
197700020211     C     V1CP11        ifeq      999
197800020211     C                   move      *on           $Po999
197900020211     C                   z-add     1             X
198000020211     C                   else
198100020215     C                   z-add     1             Y                 2 0
198200020215     C                   move      v1cp11        $campo3
198300020215     C     $campo3       lookup    Pou(y)                                 21
198400020215     C     *in21         ifeq      *off
198500020212     C                   move      V1CP11        Pou(X)
198600020211     C                   add       1             X
198700020211     C                   endif
198800020215     C                   endif
198900020211     C                   endif
199000020211      *
199100020211     C     V1CP12        ifne      *zeros
199200020212     C     $Po999        andne     *on
199300020211     C     V1CP12        ifeq      999
199400020211     C                   move      *on           $Po999
199500020211     C                   z-add     1             X
199600020211     C                   else
199700020215     C                   z-add     1             Y                 2 0
199800020215     C                   move      v1cp12        $campo3
199900020215     C     $campo3       lookup    Pou(y)                                 21
200000020215     C     *in21         ifeq      *off
200100020212     C                   move      V1CP12        Pou(X)
200200020211     C                   add       1             X
200300020211     C                   endif
200400020215     C                   endif
200500020211     C                   endif
200600020211      *
200700020211     C     V1CP13        ifne      *zeros
200800020212     C     $Po999        andne     *on
200900020211     C     V1CP13        ifeq      999
201000020211     C                   move      *on           $Po999
201100020211     C                   z-add     1             X
201200020211     C                   else
201300020215     C                   z-add     1             Y                 2 0
201400020215     C                   move      v1cp13        $campo3
201500020215     C     $campo3       lookup    Pou(y)                                 21
201600020215     C     *in21         ifeq      *off
201700020212     C                   move      V1CP13        Pou(X)
201800020211     C                   add       1             X
201900020211     C                   endif
202000020215     C                   endif
202100020211     C                   endif
202200020211      *
202300020211     C     V1CP14        ifne      *zeros
202400020212     C     $Po999        andne     *on
202500020211     C     V1CP14        ifeq      999
202600020211     C                   move      *on           $Po999
202700020211     C                   z-add     1             X
202800020211     C                   else
202900020215     C                   z-add     1             Y                 2 0
203000020215     C                   move      v1cp14        $campo3
203100020215     C     $campo3       lookup    Pou(y)                                 21
203200020215     C     *in21         ifeq      *off
203300020212     C                   move      V1CP14        Pou(X)
203400020211     C                   add       1             X
203500020211     C                   endif
203600020215     C                   endif
203700020211     C                   endif
203800020211      *
203900020211     C     V1CP15        ifne      *zeros
204000020212     C     $Po999        andne     *on
204100020211     C     V1CP15        ifeq      999
204200020211     C                   move      *on           $Po999
204300020211     C                   z-add     1             X
204400020211     C                   else
204500020215     C                   z-add     1             Y                 2 0
204600020215     C                   move      v1cp15        $campo3
204700020215     C     $campo3       lookup    Pou(y)                                 21
204800020215     C     *in21         ifeq      *off
204900020212     C                   move      V1CP15        Pou(X)
205000020211     C                   add       1             X
205100020211     C                   endif
205200020215     C                   endif
205300020211     C                   endif
205400020211      *
205500020211     C     V1CP16        ifne      *zeros
205600020212     C     $Po999        andne     *on
205700020211     C     V1CP16        ifeq      999
205800020211     C                   move      *on           $Po999
205900020211     C                   z-add     1             X
206000020211     C                   else
206100020215     C                   z-add     1             Y                 2 0
206200020215     C                   move      v1cp16        $campo3
206300020215     C     $campo3       lookup    Pou(y)                                 21
206400020215     C     *in21         ifeq      *off
206500020212     C                   move      V1CP16        Pou(X)
206600020211     C                   add       1             X
206700020211     C                   endif
206800020215     C                   endif
206900020211     C                   endif
207000020211      *
207100020211     C     V1CP17        ifne      *zeros
207200020212     C     $Po999        andne     *on
207300020211     C     V1CP17        ifeq      999
207400020211     C                   move      *on           $Po999
207500020211     C                   z-add     1             X
207600020211     C                   else
207700020215     C                   z-add     1             Y                 2 0
207800020215     C                   move      v1cp17        $campo3
207900020215     C     $campo3       lookup    Pou(y)                                 21
208000020215     C     *in21         ifeq      *off
208100020212     C                   move      V1CP17        Pou(X)
208200020211     C                   add       1             X
208300020211     C                   endif
208400020215     C                   endif
208500020211     C                   endif
208600020211      *
208700020211     C     V1CP18        ifne      *zeros
208800020212     C     $Po999        andne     *on
208900020211     C     V1CP18        ifeq      999
209000020211     C                   move      *on           $Po999
209100020211     C                   z-add     1             X
209200020211     C                   else
209300020215     C                   z-add     1             Y                 2 0
209400020215     C                   move      v1cp18        $campo3
209500020215     C     $campo3       lookup    Pou(y)                                 21
209600020215     C     *in21         ifeq      *off
209700020212     C                   move      V1CP18        Pou(X)
209800020211     C                   add       1             X
209900020211     C                   endif
210000020215     C                   endif
210100020211     C                   endif
210200020211      *
210300020211     C     V1CP19        ifne      *zeros
210400020212     C     $Po999        andne     *on
210500020211     C     V1CP19        ifeq      999
210600020211     C                   move      *on           $Po999
210700020211     C                   z-add     1             X
210800020211     C                   else
210900020215     C                   z-add     1             Y                 2 0
211000020215     C                   move      v1cp19        $campo3
211100020215     C     $campo3       lookup    Pou(y)                                 21
211200020215     C     *in21         ifeq      *off
211300020212     C                   move      V1CP19        Pou(X)
211400020211     C                   add       1             X
211500020211     C                   endif
211600020211     C                   endif
211700020215     C                   endif
211800020211      *
211900020211     C                   ENDSR
212000020211      *--------------------------------------------------------------------------------------------*
212100020211      * WriTbe - Scrittura tabella
212200020211      *--------------------------------------------------------------------------------------------*
212300020211     C     WriTbe        BEGSR
212400020211      *
212500020211     C                   clear                   Tntbe01
212600020211     C                   move      ' '           TbeAtb
212700020211     C                   movel     $Sif          TbeSif
212800020211     C                   movel     $Lin          TbeLin
212900020211     C                   movel     $Apl          TbeApl
213000020211     C                   move      '3NN'         TbeCod
213100020211     C                   move      *blanks       TbeKe1
213200020211     C                   move      *blanks       TbeKe2
213300020211      *
213400020211     C                   move      V1ccod        $Campo7           7
213500020211     C                   movel     $Campo7       TbeKe1
213600020211      *
213700020215     C     v1ccli        ifne      *zeros
213800020211     C                   move      V1ccli        $Campo7           7
213900020211     C                   movel     $Campo7       TbeUni
214000020215     C                   else
214100020215     C                   movel     *blanks       TbeUni
214200020215     C                   endif
214300020215      *
214400020211     C                   movel     POU(X)        TbeKe2
214500020211      *
214600020211     C                   move      'S'           TbeFtt
214700020211     C                   z-add     *zeros        TbeFlt
214800091203     ***C                   move      ' '           TbeFtr
214900091203      /free
215000091203                 if  w_SisInf = 1;
215100091203                   TBEftr = 'T';
215200091203                 else;
215300091203                   TBEftr = 'R';
215400091203                 endif;
215500091203      /end-free
215600020211     C                   move      *zeros        TbeDtr
215700020211      *
215800020211     C                   write     Tntbe01
215900020211      *
216000020211     C                   ENDSR
216100020208      *--------------------------------------------------------------------------------------------*
216200020208      * F03ges - Tasto funzionale f03 -> Fine programma
216300020208      *--------------------------------------------------------------------------------------------*
216400020208     C     F03ges        BEGSR
216500020208      *
216600091117     C                   eval      $Fine = *on                                  *fine pgm
216700020208     C                   clear                   tntb22ds
216800020208     C                   move      'F03'         b22Fun
216900020208      *
217000020208     C                   ENDSR
217100020208      *--------------------------------------------------------------------------------------------*
217200020208      * F12ges - Tasto funzionale f12 -> Ritorno
217300020208      *--------------------------------------------------------------------------------------------*
217400020208     C     F12ges        BEGSR
217500020208      *
217600091117     C                   eval      $Fine = *on                                  *fine pgm
217700020208      *
217800020208     C                   ENDSR
217900020208      *--------------------------------------------------------------------------------------------*
218000020208      * DeCod  - Decodifica codice cliente
218100020208      *--------------------------------------------------------------------------------------------*
218200020208     C     DeCod         BEGSR
218300020208      *
218400020208     C                   clear                   tibs69ds
218500091117     C                   movel     DUTkci        I69Kcc
218600020208     C                   movel     $Cod          I69Kac
218700020208     C                   movel     KnSif         I69Sif
218800020208     C                   call      'TIBS69R'
218900020208     C                   parm                    Tibs69Ds
219000020208     C                   parm                    DsAco
219100020208     C                   parm                    DsInd
219200020208     C                   parm                    DsClp
219300020208     C                   parm                    DsCls
219400020208      *
219500020208     C                   ENDSR
219600020212      *--------------------------------------------------------------------------------------------*
219700020212      * ChnTbe - Aggancio tabella per controllo esistenza record
219800020212      *--------------------------------------------------------------------------------------------*
219900020212     C     ChnTbe        BEGSR
220000020212     C                   setoff                                       23
220100020212     C     K01Tbe5       chain     Tntbe01l                           23
220200020212      *
220300020212     C                   ENDSR
220400020212      *--------------------------------------------------------------------------------------------*
220500020212      * CtrPos - Controllo clienti o poste o non  poste
220600020212      *--------------------------------------------------------------------------------------------*
220700020212     C     CtrPos        BEGSR
220800020212     C                   setoff                                       24
220900020212      * Controllo se poste o no il codice P.O
221000020212     C                   move      'N'           wcli1             1
221100020212     C     $Pou1         chain     azorg01l                           23
221200020212     C                   movel     Orgde3        og143
221300020215     C     §Ogntw        ifeq      'PPT'
221400020212     C                   move      'S'           Wcli1
221500020212     C                   else
221600020212     C                   move      'N'           Wcli1
221700020212     C                   endif
221800020212      *
221900020212      * Controllo se poste o no il cliente
222000020212     C                   move      'N'           wcli2             1
222100020214     C     $Pou2         chain     azorg01l                           23
222200020212     C                   movel     Orgde3        og143
222300020215     C     §Ogntw        ifeq      'PPT'
222400020212     C                   move      'S'           Wcli2
222500020212     C                   else
222600020212     C                   move      'N'           Wcli2
222700020212     C                   endif
222800020212      *
222900020212     C     Wcli1         ifne      Wcli2
223000020215     C     $pou2         andne     999
223100020212     C                   seton                                        24
223200020212     C                   endif
223300020212      *
223400020212     C                   ENDSR
223500020212      *--------------------------------------------------------------------------------------------*
223600020320      * CtrSpe-Controllo che il cliente non mantenga il num. spedizione
223700020212      *--------------------------------------------------------------------------------------------*
223800020212     C     CtrSpe        BEGSR
223900020212     C                   setoff                                       25
224000020320     C                   clear                   Ds3C
224100020212     C                   move      1             $Kut              1 0
224200020212     C                   move      '3C'          $Cod1             2
224300020212     C                   movel     *blanks       $Key              8
224400020212     C                   movel     $Campo7       $Key
224500020212     C     Ktabel        chain     Tabel00f                           21
224600020212     C                   movel     TblUni        Ds3C
224700020212     C     *in21         ifeq      *off
224800020212     C     §3CFsp        andeq     'S'
224900020212     C                   seton                                        25
225000020212     C                   endif
225100020212      *
225200020212     C                   ENDSR
225300020320      *--------------------------------------------------------------------------------------------*
225400020320      * CtrCdT-Controllo su cliente se il codice trattamento è tipo Disk 'B'
225500020320      *--------------------------------------------------------------------------------------------*
225600020320     C     CtrCdT        BEGSR
225700020320     C                   setoff                                       25
225800020320     C                   clear                   Ds1B
225900020320     C                   move      1             $Kut              1 0
226000020320     C                   move      '1B'          $Cod1             2
226100020320     C                   movel     *blanks       $Key              8
226200020320     C                   movel     §3CCtm        $Key
226300020320     C     Ktabel        chain     Tabel00f                           21
226400020320     C                   movel     TblUni        Ds1B
226500020320     C     *in21         ifeq      *off
226600020320     C     §1Bfg1        andeq     'S'
226700020320     C                   seton                                        25
226800020320     C                   endif
226900020320      *
227000020320     C                   ENDSR
227100020208      *--------------------------------------------------------------------------------------------*
227200020208      * RutInz - operazioni iniziali
227300020208      *--------------------------------------------------------------------------------------------*
227400020208     C     rutinz        BEGSR
227500020208      *
227600020208      * Ricevimento parametri
227700020208     C     *ENTRY        PLIST
227800020208     C                   PARM                    kpjba
227900020208     C                   PARM                    Tntb22Ds
228000091117
228100091117      /free
228200091117
228300091117         *inLR = *on;
228400091117
228500091117         // -?Verifica del sistema AS/400 corrente?
228600091117         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
228700091117           $Fine = *on;
228800091117           leavesr;
228900091117         endif;
229000091117
229100091117         // -?Impostazione elenco librerie in cui gestire le tabelle?
229200091117         //  ?(a seconda del sistema in cui si stà lavorando)?
229300091117         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
229400091117         if  w_iSystem = *zero;
229500091117           $Fine = *on;
229600091117           leavesr;
229700091117         endif;
229800091117
229900091117         // -?Reperimento dati job?
230000091117         exsr  sr_DatiJob;
230100091117
230200091117         // -?Impostazione nome programma a video?
230300091117         V1Tpgm = SDSpgm;
230400091117
230500091117         // -?Apertura file TABEL00F del 1° S.I. (sede)?
230600091117         w_SisInf = 1;
230700091117         exsr  sr_OpenFileTab;
230800091117
230900091117      /end-free
231000020208      *
231100020208      * A seconda dell'opzione imposto la videata da gestire
231200020208     C     B22Fun        ifeq      'F10'
231300020208     C                   move      '1'           TipVid
231400091117     C                   move      *on           $CarVd1
231500020208     C                   else
231600020208     C                   move      '2'           TipVid
231700091117     C                   move      *on           $CarVd2
231800020208     C                   endif
231900091117      *
232000091117     c                   clear                   V1Topz
232100020211      *
232200020211      * Reperisco testata tabella
232300020211     C                   movel     *blanks       Tbecod
232400020211     C                   move      *zeros        TbeKe1
232500020211     C                   move      '3NN'         TbeKe1
232600020211     C     K01Tbe2       chain     Tntbe01l                           21
232700020211      *
232800091117      * Se non esiste la testata tabella: non faccio nulla
232900091117     C   21              eval      $Fine = *on
233000020211      *
233100020211     C  n21              do
233200020211     C                   movel     TbeSif        $Sif             10
233300020211     C                   movel     TbeApl        $Apl              2
233400020211     C                   movel     TbeLin        $Lin              1
233500020215     C                   move      '3NN'         TbeCod
233600020211     C                   enddo
233700020211      *
233800020211     C                   EndSr
233900020211      *--------------------------------------------------------------------------------------------*
234000020208      *
234100020208      * chiavi di lettura
234200020208      *
234300020208     C     k02tbe4       KLIST                                                  *tntbe01l
234400020208     C                   KFLD                    tbecod                          -tabella
234500020208     C                   KFLD                    tbelin                          -chiave uno
234600020208     C                   KFLD                    tbesif                          -chiave due
234700020208     C                   KFLD                    tbeke1                          -lingua
234800020208      *
234900020208     C     k02tbe5       KLIST                                                  *tntbe01l
235000020208     C                   KFLD                    tbecod                          -tabella
235100020208     C                   KFLD                    tbelin                          -chiave uno
235200020208     C                   KFLD                    tbesif                          -chiave due
235300020208     C                   KFLD                    tbeke1                          -lingua
235400020208     C                   KFLD                    tbeke2                          -lingua
235500020208      *
235600020211     C     k01tbe2       KLIST                                                  *tntbe01l
235700020211     C                   KFLD                    tbecod                          -tabella
235800020211     C                   KFLD                    tbeke1                          -chiave uno
235900020212      *
236000020212     C     k01tbe5       KLIST                                                  *tntbe01l
236100020212     C                   KFLD                    TbeCod                          -tabella
236200020212     C                   KFLD                    tbeke1                          -chiave uno
236300020212     C                   KFLD                    tbeKe2                          -tabella
236400020212     C                   KFLD                    $lin                            -chiave uno
236500020212     C                   KFLD                    $sif                            -tabella
236600020212      *
236700020212     C     ktabel        KLIST                                                  *tntbe01l
236800020212     C                   KFLD                    $Kut                            -tabella
236900020212     C                   KFLD                    $Cod1                           -chiave uno
237000020212     C                   KFLD                    $key                            -tabella
237100091117      /free
237200091117
237300091117       //--------------------------------------------------------------
237400091117       //?Reperimento Dati del job (Utente/Operativi).                 ?
237500091117       //--------------------------------------------------------------
237600091117       BEGSR  sr_DatiJob;
237700091117
237800091117         in(e) §AzUte;
237900091117         if NOT %error;
238000091117           in(e) §DatiUte;
238100091117         endif;
238200091117         if %error or RSut = *blank;
238300091117           tibs34r ( tibs34ds );
238400091117           in §AzUte;
238500091117           in §DatiUte;
238600091117         endif;
238700091117
238800091117       ENDSR;
238900091117
239000091117      /end-free
239100091117
239200091117       //--------------------------------------------------------------
239300091117       //?Schiere a tempo di compilazione.                             ?
239400091117       //--------------------------------------------------------------
239500091117
239600091117** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con entrambi i file?
239700091127SETRAS  GAITRAGRU FILTRAGRU
239800091117AS888   GAITRAGRPSFILTRAGRPF
239900020212** Msg
240000020212Immettere almeno un punto operativo che puo' confermare le bolle del cliente
240100020212Punto operativo inesistente o errato
240200020212P.O. già abilitato per cliente
240300020212P.O. 999 non valido se cliente mantiene Num. spedizione
240400020215Il P.O. del cliente e i P.O scelti devono essere POSTE o NON POSTE
240500020320P.O. 999 non valido se cliente ha codice trattamento tipo Disk 'B'
