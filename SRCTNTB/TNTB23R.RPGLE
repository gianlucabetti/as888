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
015300020208     C                   EXFMT     Tb23v1
015400020208      *
015500020208      *
015600020208     C                   select
015700020208      * F3=Fine
015800020208     C     *inKc         wheneq    *on
015900020208     C                   EXSR      F03Ges
016000020208      *
016100020208     C     *inKl         wheneq    *on
016200020208     C                   EXSR      F12Ges
016300020208     C                   other
016400020208      *
016500020208      * Controlli e decodifiche
016600020208     C                   EXSR      CtrV1
016700020208      *
016800020208     C     *IN28         ifeq      *off
016900020208     C     *inKf         andeq     *on
017000020208     C                   EXSR      GesAgg
017100020208     C                   endif
017200020208     C                   endsl
017300020208      *
017400020208     C                   endsr
017500020208      *--------------------------------------------------------------------------------------------*
017600020208      * CarVd1 - Carica primo video
017700020208      *--------------------------------------------------------------------------------------------*
017800020208     C     CarVd1        BEGSR
017900020208      *
018000020208     C                   clear                   Tb23v1
018100091203     c                   eval      Vrsut   = RSut
018200091203     c                   eval      Vknsif  = knsif
018300091203     c                   eval      V1Tpgm  = SDSpgm
018400091203     c                   eval      V1Topz  = '  INSERIMENTO  '
018500020214      * imposto il campo solo se valorizzato e in questo caso
018600020214      * proteggo il campo
018700020214     C     b22ke1        ifne      *blanks
018800020214     C                   seton                                        38
018900020214     C                   movel     b22ke1        $Cod
019000020214     C                   move      $Cod          V1CCod
019100020214     C                   endif
019200020212      * decodifica
019300020212     C                   move      *blanks       V1DCod
019400020214     C     v1ccod        ifne      *zeros
019500020212     C                   move      V1CCod        $Cod              7 0
019600020212     C                   EXSR      DeCod
019700020212      *
019800020212 2a  C                   if        o69err = *blanks and acoflg =' '
019900020212     C                   movel     acorag        V1DCod
020000020212 2e  C                   endif
020100020214 2e  C                   endif
020200020208      *
020300020208     C                   ENDSR
020400020208      *--------------------------------------------------------------------------------------------*
020500020208      * Controllo e decodifica prima videata
020600020208      *--------------------------------------------------------------------------------------------*
020700020208     C     CtrV1         BEGSR
020800020208      *
020900020214     C                   setoff                                       283940
021000020212     C                   setoff                                       414243
021100020208     C                   setoff                                       515253
021200020208     C                   setoff                                       545556
021300020208     C                   setoff                                       575859
021400020208     C                   setoff                                       606162
021500020208     C                   setoff                                       636465
021600020208     C                   setoff                                       666768
021700020215     C                   setoff                                       6944
021800020208     C                   clear                   V1CMsg
021900020208      *
022000020214      * Ricerca estesa cu codice cliente se non protetto
022100020214 1a  C     v1dcod        ifne      *blanks
022200020214 1a  C     v1ccod        andeq     *zeros
022300020214     C     *in38         andne     *on
022400020208      *
022500020208      * Ricerca estesa
022600091117     C                   movel     rsut          paxdut           30
022700020214     C                   move      v1ccod        $campo7           7
022800020214     C                   movel(P)  $campo7       paxdmt           48
022900020208      *
023000020208     C                   z-add     0             paxsta
023100091117     C                   move      DUTkci        paxccc            4 0
023200020208     C                   clear                   paxdit
023300020208     C                   z-add     1             paxnum
023400020208      *
023500020208     C                   CALL      'XALFA3BR'
023600020208     C                   PARM                    PAXDUT
023700091117     C                   PARM      1             CODUT             1 0
023800020208     C                   PARM                    PAXDMT
023900020208     C                   PARM                    PAXCCC
024000020208     C                   PARM                    PAXSTA            1 0
024100020208     C                   PARM                    PAXFLR           90
024200020208     C                   PARM                    PAXDIT            3
024300020208     C                   PARM                    PAXNUM            2 0
024400020208     C                   PARM                    PAXKCM           80
024500020208     C                   PARM                    PAXKSM          140
024600020208     C                   PARM                    PAXKDM           60
024700020208      *
024800020208 2a  C     PaxSta        ifgt      -1
024900020214     C                   movel     paxksm        $campo7
025000020214     C                   move      $campo7       v1ccod
025100020214     C                   movel     paxdmt        v1dcod
025200020208 2-  C                   endif
025300020208      *
025400020208     C                   goto      Finesr
025500020214 1-  C                   endif
025600020208      *
025700020212      * Controllo validità codice
025800020214     C                   move      *blanks       v1dcod
025900020214 1a  C     v1ccod        ifne      *zeros
026000020208      *
026100020214     C                   move      v1ccod        $Cod              7 0
026200020208     C                   EXSR      DeCod
026300020208      *
026400020208 2a  C                   if        o69err = *blanks and acoflg =' '
026500020214     C                   movel     acorag        v1dcod
026600020212      *
026700020208 2e  C                   else
026800020214     C                   seton                                        2839
026900020212     C                   goto      Finesr
027000020208 2-  C                   endif
027100020208      *
027200020208 1-  C                   endif
027300020214      *
027400020214      * Nuovo codice cliente con cui confermare
027500020214 1a  C     v1dcli        ifne      *blanks
027600020214 1a  C     v1ccli        andeq     *zeros
027700020214      *
027800020214      * Ricerca estesa
027900020214     C
028000091117     C                   movel     rsut          paxdut           30
028100020214     C                   movel(P)  v1dcli        paxdmt           48
028200020214      *
028300020214     C                   z-add     0             paxsta
028400091117     C                   move      DUTkci        paxccc            4 0
028500020214     C                   clear                   paxdit
028600020214     C                   z-add     1             paxnum
028700020214      *
028800020214     C                   CALL      'XALFA3BR'
028900020214     C                   PARM                    PAXDUT
029000091117     C                   PARM      1             CODUT
029100020214     C                   PARM                    PAXDMT
029200020214     C                   PARM                    PAXCCC
029300020214     C                   PARM                    PAXSTA            1 0
029400020214     C                   PARM                    PAXFLR           90
029500020214     C                   PARM                    PAXDIT            3
029600020214     C                   PARM                    PAXNUM            2 0
029700020214     C                   PARM                    PAXKCM           80
029800020214     C                   PARM                    PAXKSM          140
029900020214     C                   PARM                    PAXKDM           60
030000020214      *
030100020214 2a  C     PaxSta        ifgt      -1
030200020214     C                   movel     paxksm        $campo7
030300020214     C                   move      $campo7       v1ccli
030400020214     C                   movel     paxdmt        v1dcli
030500020214 2-  C                   endif
030600020214      *
030700020214     C                   goto      Finesr
030800020214 1-  C                   endif
030900020214      *
031000020214      * Controllo validità codice
031100020214     C                   move      *blanks       v1dcli
031200020214 1a  C     v1ccli        ifne      *zeros
031300020214      *
031400020214     C                   move      v1ccli        $Cod              7 0
031500020214     C                   EXSR      DeCod
031600020214      *
031700020214 2a  C                   if        o69err = *blanks and acoflg =' '
031800020214     C                   movel     acorag        v1dcli
031900020214      *
032000020214      * Controllo che i due codici cliente sino diversi
032100020214 3a  C     v1ccli        ifeq      v1ccod
032200020214     C                   seton                                        2843
032300020214     C                   goto      Finesr
032400020214 3-  C                   endif
032500020214      *
032600020214      * Controllo che i due codici cliente siano entrambi
032700020214      * poste/non poste
032800020215     C     v1ccod        div       10000         $Pou1
032900020215     C     v1ccli        div       10000         $Pou2
033000020214     C                   exsr      CtrPos
033100020214 3a  C   24              do
033200020215     C                   seton                                        2841
033300020214     C                   goto      FineSr
033400020214 3-  C                   enddo
033500020214      *
033600020214 2e  C                   else
033700020214     C                   seton                                        2840
033800020214     C                   goto      Finesr
033900020214 2-  C                   endif
034000020214      *
034100020214 1-  C                   endif
034200020214      *
034300020214      * Controllo che i due codici siano poste/non poste
034400020214     C     v1ccli        ifne      *zeros
034500020214     C     v1ccod        div       10000         $Pou1
034600020214     C     v1ccli        div       10000         $Pou2
034700020214     C                   exsr      CtrPos
034800020214     C   24              do
034900020214     C                   seton                                        2841
035000020214     C                   goto      FineSr
035100020214     C                   enddo
035200020214     C                   endif
035300020214      *
035400020214     C
035500020211      *
035600020211      * Ci deve essere almeno un P.O diverso da zero
035700020212 1a  C     v1cp01        ifeq      *zeros
035800020212     C     v1cp02        andeq     *zeros
035900020212     C     v1cp03        andeq     *zeros
036000020212     C     v1cp04        andeq     *zeros
036100020212     C     v1cp05        andeq     *zeros
036200020212     C     v1cp06        andeq     *zeros
036300020212     C     v1cp07        andeq     *zeros
036400020212     C     v1cp08        andeq     *zeros
036500020212     C     v1cp09        andeq     *zeros
036600020212     C     v1cp10        andeq     *zeros
036700020212     C     v1cp11        andeq     *zeros
036800020212     C     v1cp12        andeq     *zeros
036900020212     C     v1cp13        andeq     *zeros
037000020212     C     v1cp14        andeq     *zeros
037100020212     C     v1cp15        andeq     *zeros
037200020212     C     v1cp16        andeq     *zeros
037300020212     C     v1cp17        andeq     *zeros
037400020212     C     v1cp18        andeq     *zeros
037500020212     C     v1cp19        andeq     *zeros
037600020212      *
037700020215     C                   seton                                        2844
037800020212     C                   movel     msg(1)        V1cmsg
037900020212     C                   goto      FineSr
038000020212     C                   endif
038100020208      *
038200020208      * Controllo P.O
038300020215      *
038400020215      * Mi imposto il pou del codice
038500020215     C     v1ccod        div       10000         $Pou1             3 0
038600020212      *
038700020215 1a  C     v1cp01        ifne      *zeros
038800020215     C     v1cp01        ifne      999
038900020215     C     v1cp01        chain     Azorg01l                           21
039000020215 2a  C   21              do
039100020215     C                   seton                                        2851
039200020215     C                   movel     msg(2)        V1cmsg
039300020215     C                   goto      FineSr
039400020215 2-  C                   enddo
039500020215     C                   endif
039600020215      *
039700020215 2a  C  n21              do
039800020215      * Controllo che il record non esista già in tabella
039900020215     C                   movel     *blanks       TbeKe1
040000020215     C                   movel     *blanks       TbeKe2
040100020215     C                   move      v1ccod        $Campo7
040200020215     C                   movel     $Campo7       TbeKe1
040300020215     C                   move      v1cp01        $Campo3           3
040400020215     C                   movel     $Campo3       TbeKe2
040500020215     C                   exsr      ChnTbe
040600020215 3a  C  n23              do
040700020215     C                   seton                                        2851
040800020215     C                   movel     msg(3)        V1cmsg
040900020215     C                   goto      FineSr
041000020215 3-  C                   enddo
041100020215      *
041200020215      * Controllo P.O e cliente poste o non poste
041300020215     C                   move      v1cp01        $Pou2             3 0
041400020215     C                   exsr      CtrPos
041500020215 3a  C   24              do
041600020215     C                   seton                                        285142
041700020215     C                   movel     msg(5)        V1cmsg
041800020215     C                   goto      FineSr
041900020215 3-  C                   enddo
042000020215 2-  C                   enddo
042100020215 1-  C                   endif
042200020215      *
042300020215 1a  C     v1cp01        ifeq      999
042400020215      * Controllo che il cliente non mantenga il numero spedizione
042500020215     C                   move      v1ccod        $Campo7
042600020215     C                   Exsr      CtrSpe
042700020320      *
042800020215 2a  C   25              do
042900020215     C                   seton                                        2851
043000020215     C                   movel     msg(4)        V1cmsg
043100020215     C                   goto      FineSr
043200020215 2-  C                   enddo
043300020320      *
043400020320     C     §3CCtm        ifne      *blanks
043500020320      * Controllo anche che il cliente non abbia un codice trattamento
043600020320      * tipo DISK 'B'
043700020320     C                   Exsr      CtrCdT
043800020320 2a  C   25              do
043900020320     C                   seton                                        2851
044000020320     C                   movel     msg(6)        V1cmsg
044100020320     C                   goto      FineSr
044200020320 2-  C                   enddo
044300020215 1-  C                   endif
044400020320 1-  C                   endif
044500020212      *
044600020212 1a  C     v1cp02        ifne      *zeros
044700020215     C     v1cp02        ifne      999
044800020212     C     v1cp02        chain     Azorg01l                           21
044900020212 2a  C   21              do
045000020212     C                   seton                                        2852
045100020212     C                   movel     msg(2)        V1cmsg
045200020212     C                   goto      FineSr
045300020212 2-  C                   enddo
045400020215     C                   endif
045500020212      *
045600020212 2a  C  n21              do
045700020212      * Controllo che il record non esista già in tabella
045800020212     C                   movel     *blanks       TbeKe1
045900020212     C                   movel     *blanks       TbeKe2
046000020212     C                   move      v1ccod        $Campo7
046100020212     C                   movel     $Campo7       TbeKe1
046200020212     C                   move      v1cp02        $Campo3           3
046300020212     C                   movel     $Campo3       TbeKe2
046400020212     C                   exsr      ChnTbe
046500020212 3a  C  n23              do
046600020212     C                   seton                                        2852
046700020212     C                   movel     msg(3)        V1cmsg
046800020212     C                   goto      FineSr
046900020212 3-  C                   enddo
047000020212      *
047100020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
047200020214     C                   move      v1cp02        $Pou2             3 0
047300020212     C                   exsr      CtrPos
047400020212 3a  C   24              do
047500020212     C                   seton                                        285242
047600020215     C                   movel     msg(5)        V1cmsg
047700020212     C                   goto      FineSr
047800020212 3-  C                   enddo
047900020212 2-  C                   enddo
048000020215 1-  C                   endif
048100020212      *
048200020215 1a  C     v1cp02        ifeq      999
048300020212      * Controllo che il cliente non mantenga il numero spedizione
048400020212     C                   move      v1ccod        $Campo7
048500020212     C                   Exsr      CtrSpe
048600020215 2a  C   25              do
048700020212     C                   seton                                        2852
048800020212     C                   movel     msg(4)        V1cmsg
048900020212     C                   goto      FineSr
049000020215 2-  C                   enddo
049100020320      *
049200020320     C     §3CCtm        ifne      *blanks
049300020320      * Controllo anche che il cliente non abbia un codice trattamento
049400020320      * tipo DISK 'B'
049500020320     C                   Exsr      CtrCdT
049600020320 2a  C   25              do
049700020320     C                   seton                                        2852
049800020320     C                   movel     msg(6)        V1cmsg
049900020320     C                   goto      FineSr
050000020320 2-  C                   enddo
050100020320 1-  C                   endif
050200020215 1-  C                   endif
050300020212      *
050400020212      *
050500020212 1a  C     v1cp03        ifne      *zeros
050600020215     C     v1cp03        ifne      999
050700020212     C     v1cp03        chain     Azorg01l                           21
050800020212 2a  C   21              do
050900020212     C                   seton                                        2853
051000020212     C                   movel     msg(2)        V1cmsg
051100020212     C                   goto      FineSr
051200020212 2-  C                   enddo
051300020215     C                   endif
051400020212      *
051500020212 2a  C  n21              do
051600020212      * Controllo che il record non esista già in tabella
051700020212     C                   movel     *blanks       TbeKe1
051800020212     C                   movel     *blanks       TbeKe2
051900020212     C                   move      v1ccod        $Campo7
052000020212     C                   movel     $Campo7       TbeKe1
052100020212     C                   move      v1cp03        $Campo3           3
052200020212     C                   movel     $Campo3       TbeKe2
052300020212     C                   exsr      ChnTbe
052400020212 3a  C  n23              do
052500020212     C                   seton                                        2853
052600020212     C                   movel     msg(3)        V1cmsg
052700020212     C                   goto      FineSr
052800020212 3-  C                   enddo
052900020212      *
053000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
053100020214     C                   move      v1cp03        $Pou2             3 0
053200020212     C                   exsr      CtrPos
053300020212 3a  C   24              do
053400020212     C                   seton                                        285342
053500020215     C                   movel     msg(5)        V1cmsg
053600020212     C                   goto      FineSr
053700020212 3-  C                   enddo
053800020212 2-  C                   enddo
053900020215     C                   endif
054000020212      *
054100020215 1a  C     v1cp03        ifeq      999
054200020212      * Controllo che il cliente non mantenga il numero spedizione
054300020212     C                   move      v1ccod        $Campo7
054400020212     C                   Exsr      CtrSpe
054500020215 2a  C   25              do
054600020212     C                   seton                                        2853
054700020212     C                   movel     msg(4)        V1cmsg
054800020212     C                   goto      FineSr
054900020215 2-  C                   enddo
055000020320      *
055100020320     C     §3CCtm        ifne      *blanks
055200020320      * Controllo anche che il cliente non abbia un codice trattamento
055300020320      * tipo DISK 'B'
055400020320     C                   Exsr      CtrCdT
055500020320 2a  C   25              do
055600020320     C                   seton                                        2853
055700020320     C                   movel     msg(6)        V1cmsg
055800020320     C                   goto      FineSr
055900020320 2-  C                   enddo
056000020320 1-  C                   endif
056100020212 1-  C                   endif
056200020212      *
056300020212 1a  C     v1cp04        ifne      *zeros
056400020215     C     v1cp04        ifne      999
056500020212     C     v1cp04        chain     Azorg01l                           21
056600020212 2a  C   21              do
056700020212     C                   seton                                        2854
056800020212     C                   movel     msg(2)        V1cmsg
056900020212     C                   goto      FineSr
057000020212 2-  C                   enddo
057100020215     C                   endif
057200020212      *
057300020212 2a  C  n21              do
057400020212      * Controllo che il record non esista già in tabella
057500020212     C                   movel     *blanks       TbeKe1
057600020212     C                   movel     *blanks       TbeKe2
057700020212     C                   move      v1ccod        $Campo7
057800020212     C                   movel     $Campo7       TbeKe1
057900020212     C                   move      v1cp04        $Campo3           3
058000020212     C                   movel     $Campo3       TbeKe2
058100020212     C                   exsr      ChnTbe
058200020212 3a  C  n23              do
058300020212     C                   seton                                        2854
058400020212     C                   movel     msg(3)        V1cmsg
058500020212     C                   goto      FineSr
058600020212 3-  C                   enddo
058700020212      *
058800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
058900020214     C                   move      v1cp04        $Pou2             3 0
059000020212     C                   exsr      CtrPos
059100020212 3a  C   24              do
059200020212     C                   seton                                        285442
059300020215     C                   movel     msg(5)        V1cmsg
059400020212     C                   goto      FineSr
059500020212 3-  C                   enddo
059600020212 2-  C                   enddo
059700020215     C                   endif
059800020212      *
059900020215 1a  C     v1cp04        ifeq      999
060000020212      * Controllo che il cliente non mantenga il numero spedizione
060100020212     C                   move      v1ccod        $Campo7
060200020212     C                   Exsr      CtrSpe
060300020215 2a  C   25              do
060400020212     C                   seton                                        2854
060500020212     C                   movel     msg(4)        V1cmsg
060600020212     C                   goto      FineSr
060700020215 2-  C                   enddo
060800020320      *
060900020320     C     §3CCtm        ifne      *blanks
061000020320      * Controllo anche che il cliente non abbia un codice trattamento
061100020320      * tipo DISK 'B'
061200020320     C                   Exsr      CtrCdT
061300020320 2a  C   25              do
061400020320     C                   seton                                        2854
061500020320     C                   movel     msg(6)        V1cmsg
061600020320     C                   goto      FineSr
061700020320 2-  C                   enddo
061800020320 1-  C                   endif
061900020215 1-  C                   endif
062000020212      *
062100020212 1a  C     v1cp05        ifne      *zeros
062200020215     C     v1cp05        ifne      999
062300020212     C     v1cp05        chain     Azorg01l                           21
062400020212 2a  C   21              do
062500020212     C                   seton                                        2855
062600020212     C                   movel     msg(2)        V1cmsg
062700020212     C                   goto      FineSr
062800020212 2-  C                   enddo
062900020215     C                   endif
063000020212      *
063100020212 2a  C  n21              do
063200020212      * Controllo che il record non esista già in tabella
063300020212     C                   movel     *blanks       TbeKe1
063400020212     C                   movel     *blanks       TbeKe2
063500020212     C                   move      v1ccod        $Campo7
063600020212     C                   movel     $Campo7       TbeKe1
063700020212     C                   move      v1cp05        $Campo3           3
063800020212     C                   movel     $Campo3       TbeKe2
063900020212     C                   exsr      ChnTbe
064000020212 3a  C  n23              do
064100020212     C                   seton                                        2855
064200020212     C                   movel     msg(3)        V1cmsg
064300020212     C                   goto      FineSr
064400020212 3-  C                   enddo
064500020212      *
064600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
064700020214     C                   move      v1cp05        $Pou2             3 0
064800020212     C                   exsr      CtrPos
064900020212 3a  C   24              do
065000020212     C                   seton                                        285542
065100020215     C                   movel     msg(5)        V1cmsg
065200020212     C                   goto      FineSr
065300020212 3-  C                   enddo
065400020212 2-  C                   enddo
065500020215     C                   endif
065600020212      *
065700020215 1a  C     v1cp05        ifeq      999
065800020212      * Controllo che il cliente non mantenga il numero spedizione
065900020212     C                   move      v1ccod        $Campo7
066000020212     C                   Exsr      CtrSpe
066100020215 2a  C   25              do
066200020212     C                   seton                                        2855
066300020212     C                   movel     msg(4)        V1cmsg
066400020212     C                   goto      FineSr
066500020215 2-  C                   enddo
066600020320      *
066700020320     C     §3CCtm        ifne      *blanks
066800020320      * Controllo anche che il cliente non abbia un codice trattamento
066900020320      * tipo DISK 'B'
067000020320     C                   Exsr      CtrCdT
067100020320 2a  C   25              do
067200020320     C                   seton                                        2855
067300020320     C                   movel     msg(6)        V1cmsg
067400020320     C                   goto      FineSr
067500020320 2-  C                   enddo
067600020320 1-  C                   endif
067700020215 1-  C                   endif
067800020212      *
067900020212 1a  C     v1cp06        ifne      *zeros
068000020215     C     v1cp06        ifne      999
068100020212     C     v1cp06        chain     Azorg01l                           21
068200020212 2a  C   21              do
068300020212     C                   seton                                        2856
068400020212     C                   movel     msg(2)        V1cmsg
068500020212     C                   goto      FineSr
068600020212 2-  C                   enddo
068700020215     C                   endif
068800020212      *
068900020212 2a  C  n21              do
069000020212      * Controllo che il record non esista già in tabella
069100020212     C                   movel     *blanks       TbeKe1
069200020212     C                   movel     *blanks       TbeKe2
069300020212     C                   move      v1ccod        $Campo7
069400020212     C                   movel     $Campo7       TbeKe1
069500020212     C                   move      v1cp06        $Campo3           3
069600020212     C                   movel     $Campo3       TbeKe2
069700020212     C                   exsr      ChnTbe
069800020212 3a  C  n23              do
069900020212     C                   seton                                        2856
070000020212     C                   movel     msg(3)        V1cmsg
070100020212     C                   goto      FineSr
070200020212 3-  C                   enddo
070300020212      *
070400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
070500020214     C                   move      v1cp06        $Pou2             3 0
070600020212     C                   exsr      CtrPos
070700020212 3a  C   24              do
070800020212     C                   seton                                        285642
070900020215     C                   movel     msg(5)        V1cmsg
071000020212     C                   goto      FineSr
071100020212 3-  C                   enddo
071200020212 2-  C                   enddo
071300020215 1-  C                   endif
071400020215      *
071500020215 1a  C     v1cp06        ifeq      999
071600020212      * Controllo che il cliente non mantenga il numero spedizione
071700020212     C                   move      v1ccod        $Campo7
071800020212     C                   Exsr      CtrSpe
071900020215 2a  C   25              do
072000020212     C                   seton                                        2856
072100020212     C                   movel     msg(4)        V1cmsg
072200020212     C                   goto      FineSr
072300020215 2-  C                   enddo
072400020320      *
072500020320     C     §3CCtm        ifne      *blanks
072600020320      * Controllo anche che il cliente non abbia un codice trattamento
072700020320      * tipo DISK 'B'
072800020320     C                   Exsr      CtrCdT
072900020320 2a  C   25              do
073000020320     C                   seton                                        2856
073100020320     C                   movel     msg(6)        V1cmsg
073200020320     C                   goto      FineSr
073300020320 2-  C                   enddo
073400020320 1-  C                   endif
073500020215 1-  C                   endif
073600020212      *
073700020212 1a  C     v1cp07        ifne      *zeros
073800020215     C     v1cp07        ifne      999
073900020212     C     v1cp07        chain     Azorg01l                           21
074000020212 2a  C   21              do
074100020212     C                   seton                                        2857
074200020212     C                   movel     msg(2)        V1cmsg
074300020212     C                   goto      FineSr
074400020212 2-  C                   enddo
074500020215     C                   endif
074600020212      *
074700020212 2a  C  n21              do
074800020212      * Controllo che il record non esista già in tabella
074900020212     C                   movel     *blanks       TbeKe1
075000020212     C                   movel     *blanks       TbeKe2
075100020212     C                   move      v1ccod        $Campo7
075200020212     C                   movel     $Campo7       TbeKe1
075300020212     C                   move      v1cp07        $Campo3           3
075400020212     C                   movel     $Campo3       TbeKe2
075500020212     C                   exsr      ChnTbe
075600020212 3a  C  n23              do
075700020212     C                   seton                                        2857
075800020212     C                   movel     msg(3)        V1cmsg
075900020212     C                   goto      FineSr
076000020212 3-  C                   enddo
076100020212      *
076200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
076300020214     C                   move      v1cp07        $Pou2             3 0
076400020212     C                   exsr      CtrPos
076500020212 3a  C   24              do
076600020212     C                   seton                                        285742
076700020215     C                   movel     msg(5)        V1cmsg
076800020212     C                   goto      FineSr
076900020212 3-  C                   enddo
077000020212 2-  C                   enddo
077100020215     C                   endif
077200020215      *
077300020215 1a  C     v1cp07        ifeq      999
077400020212      * Controllo che il cliente non mantenga il numero spedizione
077500020212     C                   move      v1ccod        $Campo7
077600020212     C                   Exsr      CtrSpe
077700020215 2a  C   25              do
077800020212     C                   seton                                        2857
077900020212     C                   movel     msg(4)        V1cmsg
078000020212     C                   goto      FineSr
078100020215 2-  C                   enddo
078200020320      *
078300020320     C     §3CCtm        ifne      *blanks
078400020320      * Controllo anche che il cliente non abbia un codice trattamento
078500020320      * tipo DISK 'B'
078600020320     C                   Exsr      CtrCdT
078700020320 2a  C   25              do
078800020320     C                   seton                                        2857
078900020320     C                   movel     msg(6)        V1cmsg
079000020320     C                   goto      FineSr
079100020320 2-  C                   enddo
079200020320 1-  C                   endif
079300020215 1-  C                   endif
079400020212      *
079500020212 1a  C     v1cp08        ifne      *zeros
079600020215     C     v1cp08        ifne      999
079700020212     C     v1cp08        chain     Azorg01l                           21
079800020212 2a  C   21              do
079900020212     C                   seton                                        2858
080000020212     C                   movel     msg(2)        V1cmsg
080100020212     C                   goto      FineSr
080200020212 2-  C                   enddo
080300020215     C                   endif
080400020212      *
080500020212 2a  C  n21              do
080600020212      * Controllo che il record non esista già in tabella
080700020212     C                   movel     *blanks       TbeKe1
080800020212     C                   movel     *blanks       TbeKe2
080900020212     C                   move      v1ccod        $Campo7
081000020212     C                   movel     $Campo7       TbeKe1
081100020212     C                   move      v1cp08        $Campo3           3
081200020212     C                   movel     $Campo3       TbeKe2
081300020212     C                   exsr      ChnTbe
081400020212 3a  C  n23              do
081500020212     C                   seton                                        2858
081600020212     C                   movel     msg(3)        V1cmsg
081700020212     C                   goto      FineSr
081800020212 3-  C                   enddo
081900020212      *
082000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
082100020214     C                   move      v1cp08        $Pou2             3 0
082200020212     C                   exsr      CtrPos
082300020212 3a  C   24              do
082400020212     C                   seton                                        285842
082500020215     C                   movel     msg(5)        V1cmsg
082600020212     C                   goto      FineSr
082700020212 3-  C                   enddo
082800020212 2-  C                   enddo
082900020215 1-  C                   endif
083000020215      *
083100020215 1a  C     v1cp08        ifeq      999
083200020212      * Controllo che il cliente non mantenga il numero spedizione
083300020212     C                   move      v1ccod        $Campo7
083400020212     C                   Exsr      CtrSpe
083500020215 2a  C   25              do
083600020212     C                   seton                                        2858
083700020212     C                   movel     msg(4)        V1cmsg
083800020212     C                   goto      FineSr
083900020215 2-  C                   enddo
084000020320      *
084100020320     C     §3CCtm        ifne      *blanks
084200020320      * Controllo anche che il cliente non abbia un codice trattamento
084300020320      * tipo DISK 'B'
084400020320     C                   Exsr      CtrCdT
084500020320 2a  C   25              do
084600020320     C                   seton                                        2858
084700020320     C                   movel     msg(6)        V1cmsg
084800020320     C                   goto      FineSr
084900020320 2-  C                   enddo
085000020320 1-  C                   endif
085100020215 1-  C                   endif
085200020212      *
085300020212 1a  C     v1cp09        ifne      *zeros
085400020215     C     v1cp09        ifne      999
085500020212     C     v1cp09        chain     Azorg01l                           21
085600020212 2a  C   21              do
085700020212     C                   seton                                        2859
085800020212     C                   movel     msg(2)        V1cmsg
085900020212     C                   goto      FineSr
086000020212 2-  C                   enddo
086100020215 1-  C                   endif
086200020212      *
086300020212 2a  C  n21              do
086400020212      * Controllo che il record non esista già in tabella
086500020212     C                   movel     *blanks       TbeKe1
086600020212     C                   movel     *blanks       TbeKe2
086700020212     C                   move      v1ccod        $Campo7
086800020212     C                   movel     $Campo7       TbeKe1
086900020212     C                   move      v1cp09        $Campo3           3
087000020212     C                   movel     $Campo3       TbeKe2
087100020212     C                   exsr      ChnTbe
087200020212 3a  C  n23              do
087300020212     C                   seton                                        2859
087400020212     C                   movel     msg(3)        V1cmsg
087500020212     C                   goto      FineSr
087600020212 3-  C                   enddo
087700020212      *
087800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
087900020214     C                   move      v1cp09        $Pou2             3 0
088000020212     C                   exsr      CtrPos
088100020212 3a  C   24              do
088200020212     C                   seton                                        285942
088300020215     C                   movel     msg(5)        V1cmsg
088400020212     C                   goto      FineSr
088500020212 3-  C                   enddo
088600020212 2-  C                   enddo
088700020215 1-  C                   endif
088800020215      *
088900020215 1a  C     v1cp09        ifeq      999
089000020212      * Controllo che il cliente non mantenga il numero spedizione
089100020212     C                   move      v1ccod        $Campo7
089200020212     C                   Exsr      CtrSpe
089300020215 2a  C   25              do
089400020212     C                   seton                                        2859
089500020212     C                   movel     msg(4)        V1cmsg
089600020212     C                   goto      FineSr
089700020215 2-  C                   enddo
089800020320      *
089900020320     C     §3CCtm        ifne      *blanks
090000020320      * Controllo anche che il cliente non abbia un codice trattamento
090100020320      * tipo DISK 'B'
090200020320     C                   Exsr      CtrCdT
090300020320 2a  C   25              do
090400020320     C                   seton                                        2859
090500020320     C                   movel     msg(6)        V1cmsg
090600020320     C                   goto      FineSr
090700020320 2-  C                   enddo
090800020320 1-  C                   endif
090900020215 1-  C                   endif
091000020212      *
091100020212 1a  C     v1cp10        ifne      *zeros
091200020215     C     v1cp10        ifne      999
091300020212     C     v1cp10        chain     Azorg01l                           21
091400020212 2a  C   21              do
091500020212     C                   seton                                        2860
091600020212     C                   movel     msg(2)        V1cmsg
091700020212     C                   goto      FineSr
091800020212 2-  C                   enddo
091900020215     C                   endif
092000020212      *
092100020212 2a  C  n21              do
092200020212      * Controllo che il record non esista già in tabella
092300020212     C                   movel     *blanks       TbeKe1
092400020212     C                   movel     *blanks       TbeKe2
092500020212     C                   move      v1ccod        $Campo7
092600020212     C                   movel     $Campo7       TbeKe1
092700020212     C                   move      v1cp10        $Campo3           3
092800020212     C                   movel     $Campo3       TbeKe2
092900020212     C                   exsr      ChnTbe
093000020212 3a  C  n23              do
093100020212     C                   seton                                        2860
093200020212     C                   movel     msg(3)        V1cmsg
093300020212     C                   goto      FineSr
093400020212 3-  C                   enddo
093500020212      *
093600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
093700020214     C                   move      v1cp10        $Pou2             3 0
093800020212     C                   exsr      CtrPos
093900020212 3a  C   24              do
094000020212     C                   seton                                        286042
094100020215     C                   movel     msg(5)        V1cmsg
094200020212     C                   goto      FineSr
094300020212 3-  C                   enddo
094400020212 2-  C                   enddo
094500020215     C                   endif
094600020215      *
094700020215 1a  C     v1cp10        ifeq      999
094800020212      * Controllo che il cliente non mantenga il numero spedizione
094900020212     C                   move      v1ccod        $Campo7
095000020212     C                   Exsr      CtrSpe
095100020215 2a  C   25              do
095200020212     C                   seton                                        2860
095300020212     C                   movel     msg(4)        V1cmsg
095400020212     C                   goto      FineSr
095500020215 2-  C                   enddo
095600020320      *
095700020320     C     §3CCtm        ifne      *blanks
095800020320      * Controllo anche che il cliente non abbia un codice trattamento
095900020320      * tipo DISK 'B'
096000020320     C                   Exsr      CtrCdT
096100020320 2a  C   25              do
096200020320     C                   seton                                        2860
096300020320     C                   movel     msg(6)        V1cmsg
096400020320     C                   goto      FineSr
096500020320 2-  C                   enddo
096600020320 1-  C                   endif
096700020215 1-  C                   endif
096800020212      *
096900020212 1a  C     v1cp11        ifne      *zeros
097000020215     C     v1cp11        ifne      999
097100020212     C     v1cp11        chain     Azorg01l                           21
097200020212 2a  C   21              do
097300020212     C                   seton                                        2861
097400020212     C                   movel     msg(2)        V1cmsg
097500020212     C                   goto      FineSr
097600020212 2-  C                   enddo
097700020215 1-  C                   endif
097800020212      *
097900020212 2a  C  n21              do
098000020212      * Controllo che il record non esista già in tabella
098100020212     C                   movel     *blanks       TbeKe1
098200020212     C                   movel     *blanks       TbeKe2
098300020212     C                   move      v1ccod        $Campo7
098400020212     C                   movel     $Campo7       TbeKe1
098500020212     C                   move      v1cp11        $Campo3           3
098600020212     C                   movel     $Campo3       TbeKe2
098700020212     C                   exsr      ChnTbe
098800020212 3a  C  n23              do
098900020212     C                   seton                                        2861
099000020212     C                   movel     msg(3)        V1cmsg
099100020212     C                   goto      FineSr
099200020212 3-  C                   enddo
099300020212      *
099400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
099500020214     C                   move      v1cp11        $Pou2             3 0
099600020212     C                   exsr      CtrPos
099700020212 3a  C   24              do
099800020212     C                   seton                                        286142
099900020215     C                   movel     msg(5)        V1cmsg
100000020212     C                   goto      FineSr
100100020212 3-  C                   enddo
100200020212 2-  C                   enddo
100300020215     C                   endif
100400020215      *
100500020215 1a  C     v1cp11        ifeq      999
100600020212      * Controllo che il cliente non mantenga il numero spedizione
100700020212     C                   move      v1ccod        $Campo7
100800020212     C                   Exsr      CtrSpe
100900020215 2a  C   25              do
101000020212     C                   seton                                        2861
101100020212     C                   movel     msg(4)        V1cmsg
101200020212     C                   goto      FineSr
101300020215 2-  C                   enddo
101400020320      *
101500020320     C     §3CCtm        ifne      *blanks
101600020320      * Controllo anche che il cliente non abbia un codice trattamento
101700020320      * tipo DISK 'B'
101800020320     C                   Exsr      CtrCdT
101900020320 2a  C   25              do
102000020320     C                   seton                                        2861
102100020320     C                   movel     msg(6)        V1cmsg
102200020320     C                   goto      FineSr
102300020320 2-  C                   enddo
102400020320 1-  C                   endif
102500020215 1-  C                   endif
102600020212      *
102700020212 1a  C     v1cp12        ifne      *zeros
102800020215     C     v1cp12        ifne      999
102900020212     C     v1cp12        chain     Azorg01l                           21
103000020212 2a  C   21              do
103100020212     C                   seton                                        2862
103200020212     C                   movel     msg(2)        V1cmsg
103300020212     C                   goto      FineSr
103400020212 2-  C                   enddo
103500020215     C                   endif
103600020212      *
103700020212 2a  C  n21              do
103800020212      * Controllo che il record non esista già in tabella
103900020212     C                   movel     *blanks       TbeKe1
104000020212     C                   movel     *blanks       TbeKe2
104100020212     C                   move      v1ccod        $Campo7
104200020212     C                   movel     $Campo7       TbeKe1
104300020212     C                   move      v1cp12        $Campo3           3
104400020212     C                   movel     $Campo3       TbeKe2
104500020212     C                   exsr      ChnTbe
104600020212 3a  C  n23              do
104700020212     C                   seton                                        2862
104800020212     C                   movel     msg(3)        V1cmsg
104900020212     C                   goto      FineSr
105000020212 3-  C                   enddo
105100020212      *
105200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
105300020214     C                   move      v1cp12        $Pou2             3 0
105400020212     C                   exsr      CtrPos
105500020212 3a  C   24              do
105600020212     C                   seton                                        286242
105700020215     C                   movel     msg(5)        V1cmsg
105800020212     C                   goto      FineSr
105900020212 3-  C                   enddo
106000020212 2-  C                   enddo
106100020215     C                   endif
106200020215      *
106300020215 1a  C     v1cp12        ifeq      999
106400020212      * Controllo che il cliente non mantenga il numero spedizione
106500020212     C                   move      v1ccod        $Campo7
106600020212     C                   Exsr      CtrSpe
106700020215 2a  C   25              do
106800020212     C                   seton                                        2862
106900020212     C                   movel     msg(4)        V1cmsg
107000020212     C                   goto      FineSr
107100020215 2-  C                   enddo
107200020320      *
107300020320     C     §3CCtm        ifne      *blanks
107400020320      * Controllo anche che il cliente non abbia un codice trattamento
107500020320      * tipo DISK 'B'
107600020320     C                   Exsr      CtrCdT
107700020320 2a  C   25              do
107800020320     C                   seton                                        2862
107900020320     C                   movel     msg(6)        V1cmsg
108000020320     C                   goto      FineSr
108100020320 2-  C                   enddo
108200020320 1-  C                   endif
108300020215 1-  C                   endif
108400020212      *
108500020212 1a  C     v1cp13        ifne      *zeros
108600020215     C     v1cp13        ifne      999
108700020212     C     v1cp13        chain     Azorg01l                           21
108800020212 2a  C   21              do
108900020212     C                   seton                                        2863
109000020212     C                   movel     msg(2)        V1cmsg
109100020212     C                   goto      FineSr
109200020212 2-  C                   enddo
109300020215 1-  C                   endif
109400020212      *
109500020212 2a  C  n21              do
109600020212      * Controllo che il record non esista già in tabella
109700020212     C                   movel     *blanks       TbeKe1
109800020212     C                   movel     *blanks       TbeKe2
109900020212     C                   move      v1ccod        $Campo7
110000020212     C                   movel     $Campo7       TbeKe1
110100020212     C                   move      v1cp13        $Campo3           3
110200020212     C                   movel     $Campo3       TbeKe2
110300020212     C                   exsr      ChnTbe
110400020212 3a  C  n23              do
110500020212     C                   seton                                        2863
110600020212     C                   movel     msg(3)        V1cmsg
110700020212     C                   goto      FineSr
110800020212 3-  C                   enddo
110900020212      *
111000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
111100020214     C                   move      v1cp13        $Pou2             3 0
111200020212     C                   exsr      CtrPos
111300020212 3a  C   24              do
111400020212     C                   seton                                        286342
111500020215     C                   movel     msg(5)        V1cmsg
111600020212     C                   goto      FineSr
111700020212 3-  C                   enddo
111800020212 2-  C                   enddo
111900020215     C                   endif
112000020215      *
112100020215 1a  C     v1cp13        ifeq      999
112200020212      * Controllo che il cliente non mantenga il numero spedizione
112300020212     C                   move      v1ccod        $Campo7
112400020212     C                   Exsr      CtrSpe
112500020215 2a  C   25              do
112600020212     C                   seton                                        2863
112700020212     C                   movel     msg(4)        V1cmsg
112800020212     C                   goto      FineSr
112900020215 2-  C                   enddo
113000020320      *
113100020320     C     §3CCtm        ifne      *blanks
113200020320      * Controllo anche che il cliente non abbia un codice trattamento
113300020320      * tipo DISK 'B'
113400020320     C                   Exsr      CtrCdT
113500020320 2a  C   25              do
113600020320     C                   seton                                        2863
113700020320     C                   movel     msg(6)        V1cmsg
113800020320     C                   goto      FineSr
113900020320 2-  C                   enddo
114000020320 1-  C                   endif
114100020215 1-  C                   endif
114200020212      *
114300020212 1a  C     v1cp14        ifne      *zeros
114400020215     C     v1cp14        ifne      999
114500020212     C     v1cp14        chain     Azorg01l                           21
114600020212 2a  C   21              do
114700020212     C                   seton                                        2864
114800020212     C                   movel     msg(2)        V1cmsg
114900020212     C                   goto      FineSr
115000020212 2-  C                   enddo
115100020215     C                   endif
115200020212      *
115300020212 2a  C  n21              do
115400020212      * Controllo che il record non esista già in tabella
115500020212     C                   movel     *blanks       TbeKe1
115600020212     C                   movel     *blanks       TbeKe2
115700020212     C                   move      v1ccod        $Campo7
115800020212     C                   movel     $Campo7       TbeKe1
115900020212     C                   move      v1cp14        $Campo3           3
116000020212     C                   movel     $Campo3       TbeKe2
116100020212     C                   exsr      ChnTbe
116200020212 3a  C  n23              do
116300020212     C                   seton                                        2864
116400020212     C                   movel     msg(3)        V1cmsg
116500020212     C                   goto      FineSr
116600020212 3-  C                   enddo
116700020212      *
116800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
116900020214     C                   move      v1cp14        $Pou2             3 0
117000020212     C                   exsr      CtrPos
117100020212 3a  C   24              do
117200020212     C                   seton                                        286442
117300020215     C                   movel     msg(5)        V1cmsg
117400020212     C                   goto      FineSr
117500020212 3-  C                   enddo
117600020212 2-  C                   enddo
117700020215     C                   endif
117800020215      *
117900020215 1a  C     v1cp14        ifeq      999
118000020212      * Controllo che il cliente non mantenga il numero spedizione
118100020212     C                   move      v1ccod        $Campo7
118200020212     C                   Exsr      CtrSpe
118300020215 2a  C   25              do
118400020212     C                   seton                                        2864
118500020212     C                   movel     msg(4)        V1cmsg
118600020212     C                   goto      FineSr
118700020215 2-  C                   enddo
118800020320      *
118900020320     C     §3CCtm        ifne      *blanks
119000020320      * Controllo anche che il cliente non abbia un codice trattamento
119100020320      * tipo DISK 'B'
119200020320     C                   Exsr      CtrCdT
119300020320 2a  C   25              do
119400020320     C                   seton                                        2864
119500020320     C                   movel     msg(6)        V1cmsg
119600020320     C                   goto      FineSr
119700020320 2-  C                   enddo
119800020320 1-  C                   endif
119900020215 1-  C                   endif
120000020212      *
120100020212 1a  C     v1cp15        ifne      *zeros
120200020215     C     v1cp15        ifne      999
120300020212     C     v1cp15        chain     Azorg01l                           21
120400020212 2a  C   21              do
120500020212     C                   seton                                        2865
120600020212     C                   movel     msg(2)        V1cmsg
120700020212     C                   goto      FineSr
120800020212 2-  C                   enddo
120900020215     C                   endif
121000020212      *
121100020212 2a  C  n21              do
121200020212      * Controllo che il record non esista già in tabella
121300020212     C                   movel     *blanks       TbeKe1
121400020212     C                   movel     *blanks       TbeKe2
121500020212     C                   move      v1ccod        $Campo7
121600020212     C                   movel     $Campo7       TbeKe1
121700020212     C                   move      v1cp15        $Campo3           3
121800020212     C                   movel     $Campo3       TbeKe2
121900020212     C                   exsr      ChnTbe
122000020212 3a  C  n23              do
122100020212     C                   seton                                        2865
122200020212     C                   movel     msg(3)        V1cmsg
122300020212     C                   goto      FineSr
122400020212 3-  C                   enddo
122500020212      *
122600020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
122700020214     C                   move      v1cp15        $Pou2             3 0
122800020212     C                   exsr      CtrPos
122900020212 3a  C   24              do
123000020212     C                   seton                                        286542
123100020215     C                   movel     msg(5)        V1cmsg
123200020212     C                   goto      FineSr
123300020212 3-  C                   enddo
123400020212 2-  C                   enddo
123500020215     C                   endif
123600020215      *
123700020215 1a  C     v1cp15        ifeq      999
123800020212      * Controllo che il cliente non mantenga il numero spedizione
123900020212     C                   move      v1ccod        $Campo7
124000020212     C                   Exsr      CtrSpe
124100020215 2a  C   25              do
124200020212     C                   seton                                        2865
124300020212     C                   movel     msg(4)        V1cmsg
124400020212     C                   goto      FineSr
124500020215 2-  C                   enddo
124600020320      *
124700020320     C     §3CCtm        ifne      *blanks
124800020320      * Controllo anche che il cliente non abbia un codice trattamento
124900020320      * tipo DISK 'B'
125000020320     C                   Exsr      CtrCdT
125100020320 2a  C   25              do
125200020320     C                   seton                                        2865
125300020320     C                   movel     msg(6)        V1cmsg
125400020320     C                   goto      FineSr
125500020320 2-  C                   enddo
125600020320 1-  C                   endif
125700020215 1-  C                   endif
125800020212      *
125900020212 1a  C     v1cp16        ifne      *zeros
126000020215     C     v1cp16        ifne      999
126100020212     C     v1cp16        chain     Azorg01l                           21
126200020212 2a  C   21              do
126300020212     C                   seton                                        2866
126400020212     C                   movel     msg(2)        V1cmsg
126500020212     C                   goto      FineSr
126600020212 2-  C                   enddo
126700020215     C                   endif
126800020212      *
126900020212 2a  C  n21              do
127000020212      * Controllo che il record non esista già in tabella
127100020212     C                   movel     *blanks       TbeKe1
127200020212     C                   movel     *blanks       TbeKe2
127300020212     C                   move      v1ccod        $Campo7
127400020212     C                   movel     $Campo7       TbeKe1
127500020212     C                   move      v1cp16        $Campo3           3
127600020212     C                   movel     $Campo3       TbeKe2
127700020212     C                   exsr      ChnTbe
127800020212 3a  C  n23              do
127900020212     C                   seton                                        2866
128000020212     C                   movel     msg(3)        V1cmsg
128100020212     C                   goto      FineSr
128200020212 3-  C                   enddo
128300020212      *
128400020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
128500020214     C                   move      v1cp16        $Pou2             3 0
128600020212     C                   exsr      CtrPos
128700020212 3a  C   24              do
128800020212     C                   seton                                        286642
128900020215     C                   movel     msg(5)        V1cmsg
129000020212     C                   goto      FineSr
129100020212 3-  C                   enddo
129200020212 2-  C                   enddo
129300020215 1-  C                   endif
129400020215      *
129500020215 1a  C     v1cp16        ifeq      999
129600020212      * Controllo che il cliente non mantenga il numero spedizione
129700020212     C                   move      v1ccod        $Campo7
129800020212     C                   Exsr      CtrSpe
129900020215 2a  C   25              do
130000020212     C                   seton                                        2866
130100020212     C                   movel     msg(4)        V1cmsg
130200020212     C                   goto      FineSr
130300020215 2-  C                   enddo
130400020320      *
130500020320     C     §3CCtm        ifne      *blanks
130600020320      * Controllo anche che il cliente non abbia un codice trattamento
130700020320      * tipo DISK 'B'
130800020320     C                   Exsr      CtrCdT
130900020320 2a  C   25              do
131000020320     C                   seton                                        2866
131100020320     C                   movel     msg(6)        V1cmsg
131200020320     C                   goto      FineSr
131300020320 2-  C                   enddo
131400020320 1-  C                   endif
131500020215 1-  C                   endif
131600020212      *
131700020212 1a  C     v1cp17        ifne      *zeros
131800020215     C     v1cp17        ifne      999
131900020212     C     v1cp17        chain     Azorg01l                           21
132000020212 2a  C   21              do
132100020212     C                   seton                                        2867
132200020212     C                   movel     msg(2)        V1cmsg
132300020212     C                   goto      FineSr
132400020212 2-  C                   enddo
132500020215 1-  C                   endif
132600020212      *
132700020212 2a  C  n21              do
132800020212      * Controllo che il record non esista già in tabella
132900020212     C                   movel     *blanks       TbeKe1
133000020212     C                   movel     *blanks       TbeKe2
133100020212     C                   move      v1ccod        $Campo7
133200020212     C                   movel     $Campo7       TbeKe1
133300020212     C                   move      v1cp17        $Campo3           3
133400020212     C                   movel     $Campo3       TbeKe2
133500020212     C                   exsr      ChnTbe
133600020212 3a  C  n23              do
133700020212     C                   seton                                        2867
133800020212     C                   movel     msg(3)        V1cmsg
133900020212     C                   goto      FineSr
134000020212 3-  C                   enddo
134100020212      *
134200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
134300020214     C                   move      v1cp17        $Pou2             3 0
134400020212     C                   exsr      CtrPos
134500020212 3a  C   24              do
134600020212     C                   seton                                        286742
134700020215     C                   movel     msg(5)        V1cmsg
134800020212     C                   goto      FineSr
134900020212 3-  C                   enddo
135000020212 2-  C                   enddo
135100020215     C                   endif
135200020215      *
135300020215 1a  C     v1cp17        ifeq      999
135400020212      * Controllo che il cliente non mantenga il numero spedizione
135500020212     C                   move      v1ccod        $Campo7
135600020212     C                   Exsr      CtrSpe
135700020215 2a  C   25              do
135800020212     C                   seton                                        2867
135900020212     C                   movel     msg(4)        V1cmsg
136000020212     C                   goto      FineSr
136100020215 2-  C                   enddo
136200020320      *
136300020320     C     §3CCtm        ifne      *blanks
136400020320      * Controllo anche che il cliente non abbia un codice trattamento
136500020320      * tipo DISK 'B'
136600020320     C                   Exsr      CtrCdT
136700020320 2a  C   25              do
136800020320     C                   seton                                        2867
136900020320     C                   movel     msg(6)        V1cmsg
137000020320     C                   goto      FineSr
137100020320 2-  C                   enddo
137200020320 1-  C                   endif
137300020215 1-  C                   endif
137400020212      *
137500020212 1a  C     v1cp18        ifne      *zeros
137600020215     C     v1cp18        ifne      999
137700020212     C     v1cp18        chain     Azorg01l                           21
137800020212 2a  C   21              do
137900020212     C                   seton                                        2868
138000020212     C                   movel     msg(2)        V1cmsg
138100020212     C                   goto      FineSr
138200020212 2-  C                   enddo
138300020215 1-  C                   endif
138400020212      *
138500020212 2a  C  n21              do
138600020212      * Controllo che il record non esista già in tabella
138700020212     C                   movel     *blanks       TbeKe1
138800020212     C                   movel     *blanks       TbeKe2
138900020212     C                   move      v1ccod        $Campo7
139000020212     C                   movel     $Campo7       TbeKe1
139100020212     C                   move      v1cp18        $Campo3           3
139200020212     C                   movel     $Campo3       TbeKe2
139300020212     C                   exsr      ChnTbe
139400020212 3a  C  n23              do
139500020212     C                   seton                                        2868
139600020212     C                   movel     msg(3)        V1cmsg
139700020212     C                   goto      FineSr
139800020212 3-  C                   enddo
139900020212      *
140000020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
140100020214     C                   move      v1cp18        $Pou2             3 0
140200020212     C                   exsr      CtrPos
140300020212 3a  C   24              do
140400020212     C                   seton                                        286842
140500020215     C                   movel     msg(5)        V1cmsg
140600020212     C                   goto      FineSr
140700020212 3-  C                   enddo
140800020212 2-  C                   enddo
140900020215     C                   endif
141000020215      *
141100020215 1a  C     v1cp18        ifeq      999
141200020212      * Controllo che il cliente non mantenga il numero spedizione
141300020212     C                   move      v1ccod        $Campo7
141400020212     C                   Exsr      CtrSpe
141500020215 2a  C   25              do
141600020212     C                   seton                                        2868
141700020212     C                   movel     msg(4)        V1cmsg
141800020212     C                   goto      FineSr
141900020215 2-  C                   enddo
142000020320      *
142100020320     C     §3CCtm        ifne      *blanks
142200020320      * Controllo anche che il cliente non abbia un codice trattamento
142300020320      * tipo DISK 'B'
142400020320     C                   Exsr      CtrCdT
142500020320 2a  C   25              do
142600020320     C                   seton                                        2868
142700020320     C                   movel     msg(6)        V1cmsg
142800020320     C                   goto      FineSr
142900020320 2-  C                   enddo
143000020320 1-  C                   endif
143100020215 1-  C                   endif
143200020208      *
143300020212 1a  C     v1cp19        ifne      *zeros
143400020215     C     v1cp19        ifne      999
143500020212     C     v1cp19        chain     Azorg01l                           21
143600020212 2a  C   21              do
143700020212     C                   seton                                        2869
143800020212     C                   movel     msg(2)        V1cmsg
143900020212     C                   goto      FineSr
144000020212 2-  C                   enddo
144100020215     C                   endif
144200020212      *
144300020212 2a  C  n21              do
144400020212      * Controllo che il record non esista già in tabella
144500020212     C                   movel     *blanks       TbeKe1
144600020212     C                   movel     *blanks       TbeKe2
144700020212     C                   move      v1ccod        $Campo7
144800020212     C                   movel     $Campo7       TbeKe1
144900020212     C                   move      v1cp19        $Campo3           3
145000020212     C                   movel     $Campo3       TbeKe2
145100020212     C                   exsr      ChnTbe
145200020212 3a  C  n23              do
145300020212     C                   seton                                        2869
145400020212     C                   movel     msg(3)        V1cmsg
145500020212     C                   goto      FineSr
145600020212 3-  C                   enddo
145700020212      *
145800020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
145900020214     C                   move      v1cp19        $Pou2             3 0
146000020212     C                   exsr      CtrPos
146100020212 3a  C   24              do
146200020212     C                   seton                                        286942
146300020215     C                   movel     msg(5)        V1cmsg
146400020212     C                   goto      FineSr
146500020212 3-  C                   enddo
146600020212 2-  C                   enddo
146700020215     C                   endif
146800020215      *
146900020215 1a  C     v1cp19        ifeq      999
147000020212      * Controllo che il cliente non mantenga il numero spedizione
147100020212     C                   move      v1ccod        $Campo7
147200020212     C                   Exsr      CtrSpe
147300020215 2a  C   25              do
147400020212     C                   seton                                        2869
147500020212     C                   movel     msg(4)        V1cmsg
147600020212     C                   goto      FineSr
147700020215 2-  C                   enddo
147800020320      *
147900020320     C     §3CCtm        ifne      *blanks
148000020320      * Controllo anche che il cliente non abbia un codice trattamento
148100020320      * tipo DISK 'B'
148200020320     C                   Exsr      CtrCdT
148300020320 2a  C   25              do
148400020320     C                   seton                                        2869
148500020320     C                   movel     msg(6)        V1cmsg
148600020320     C                   goto      FineSr
148700020320 2-  C                   enddo
148800020320 1-  C                   endif
148900020215 1-  C                   endif
149000020212      *
149100020208     C     FineSr        ENDSR
149200020208      *--------------------------------------------------------------------------------------------*
149300020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
149400020208      *--------------------------------------------------------------------------------------------*
149500020208     C     GesVd2        BEGSR
149600020208      *
149700020208      * Inizializzazione videata
149800020208     C     $CarVd2       ifeq      *on
149900020208     C                   EXSR      CarVd2
150000020208     C                   move      *off          $CarVd2
150100020208     C                   endif
150200020208      *
150300020215     C                   EXFMT     Tb23v2
150400020208      *
150500020212      * Tasti funzione se non si è in visualizza
150600020212     C     b22Opz        ifne      '5'
150700020212      *
150800020212     C                   select
150900020212     C     *inKl         wheneq    *on
151000020212     C                   EXSR      F12Ges
151100020212     C                   other
151200020212      *
151300020212      * Controlli solo se si è in modifica
151400020212     C     b22Opz        ifeq      '2'
151500020208     C                   EXSR      CtrV2
151600020212     C                   endif
151700020212     C                   endsl
151800020212     C                   endif
151900020208      *
152000020212     C     b22Opz        ifne      '5'
152100020208     C     *IN28         ifeq      *off
152200020212     C     *inKf         andeq     *on
152300020208     C                   EXSR      GesAgg
152400020212     C                   endif
152500020212     C                   else
152600091117     C                   eval      $Fine = *on
152700020212     C                   endif
152800020211      *
152900020208     C                   endsr
153000020208      *--------------------------------------------------------------------------------------------*
153100020208      * CarVd2 - Carica primo video
153200020208      *--------------------------------------------------------------------------------------------*
153300020208     C     CarVd2        BEGSR
153400091117      *
153500091117     C                   clear                   Tb23v2
153600091203     c                   eval      Vrsut   = RSut
153700091203     c                   eval      Vknsif  = knsif
153800091203     c                   eval      V1Tpgm  = SDSpgm
153900091117      *
154000091117      /free
154100091117         clear  V1Topz;
154200091117         select;
154300091117           when  B22opz = '2';
154400091117             V1Topz  = '   MODIFICA    ';
154500091117             *in60 = *off;
154600091117             *in63 = *off;
154700091117           when  B22opz = '4';
154800091117             V1Topz  = ' ANNULLAMENTO  ';
154900091117             *in60 = *on;
155000091117             *in63 = *off;
155100091117           when  B22opz = '5';
155200091117             V1Topz  = 'VISUALIZZAZIONE';
155300091117             *in60 = *on;
155400091117             *in63 = *on;
155500091117           when  B22opz = '7';
155600091117             V1Topz  = '  RIPRISTINO   ';
155700091117             *in60 = *on;
155800091117             *in63 = *off;
155900091117         endsl;
156000091117
156100091117         // - Reperimento dati tabella
156200091117         exsr  sr_ChainTNTBE;
156300091117
156400091117      /end-free
156500020211      *
156600020215      * Codice e decodifica
156700020215     C                   movel     b22ke1        $campo7           7
156800020215     C                   move      $campo7       V2ccod
156900020215     C                   move      $Campo7       $Cod              7 0
157000020215      *
157100020215     C                   EXSR      DeCod
157200020215     C                   if        o69err = *blanks and acoflg =' '
157300020215     C                   movel     acorag        v2dcod
157400020215     C                   endif
157500020215      *
157600020215      * P.O.
157700020215     C                   move      b22ke2        $campo3
157800020215     C                   movel     $campo3       V2CPou
157900020211      *
158000020211      * Nuovo codice e decodifica
158100020211     C                   movel     TbeUni        $campo7           7
158200020215     C                   move      $Campo7       v2ccli
158300020215      *
158400020215     C                   movel     *blanks       v2dcli
158500030704     c                   if        $CAMPO7 <> *blanks  and
158600030704     c                             $CAMPO7 <> *zeros
158700020211     C                   move      $Campo7       $Cod              7 0
158800020211     C                   EXSR      DeCod
158900020211     C                   if        o69err = *blanks and acoflg =' '
159000020215     C                   movel     acorag        v2dcli
159100020211     C                   endif
159200020215     C                   endif
159300020208      *
159400020208     C                   ENDSR
159500091117      *--------------------------------------------------------------------------------------------*
159600091117      * Lettura file TNTBE02L
159700091117      *--------------------------------------------------------------------------------------------*
159800091117     c     sr_ChainTNTBE BEGSR
159900091117      *
160000091117      * Leggo la tabella
160100091117     C                   move      '3NN'         TbeCod
160200091117     C                   movel     b22lin        TbeLin
160300091117     C                   movel     b22sif        TbeSif
160400091117     C                   movel     *blanks       TbeKe1
160500091117     C                   movel     b22ke1        TbeKe1
160600091117     C                   movel     *blanks       TbeKe2
160700091117     C                   movel     b22ke2        TbeKe2
160800091117     C     k02tbe5       chain     tntbe02l                           21
160900091203      /free
161000091203         if  *in21;
161100091203           clear  TBEuni;
161200091203         endif;
161300091203      /end-free
161400091117      *
161500091117     c                   ENDSR
161600020208      *--------------------------------------------------------------------------------------------*
161700020215      * Controllo seconda videata
161800020208      *--------------------------------------------------------------------------------------------*
161900020208     C     CtrV2         BEGSR
162000020214     C                   setoff                                       2862
162100020214      *
162200020214      * Nuovo codice cliente con cui confermare
162300020215 1a  C     v2dcli        ifne      *blanks
162400020215     C     v2ccli        andeq     *zeros
162500020214      *
162600020214      * Ricerca estesa
162700020214     C
162800091117     C                   movel     rsut          paxdut           30
162900020215     C                   movel(P)  v2dcli        paxdmt           48
163000020214      *
163100020214     C                   z-add     0             paxsta
163200091117     C                   move      DUTkci        paxccc            4 0
163300020214     C                   clear                   paxdit
163400020214     C                   z-add     1             paxnum
163500020214      *
163600020214     C                   CALL      'XALFA3BR'
163700020214     C                   PARM                    PAXDUT
163800091117     C                   PARM      1             CODUT
163900020214     C                   PARM                    PAXDMT
164000020214     C                   PARM                    PAXCCC
164100020214     C                   PARM                    PAXSTA            1 0
164200020214     C                   PARM                    PAXFLR           90
164300020214     C                   PARM                    PAXDIT            3
164400020214     C                   PARM                    PAXNUM            2 0
164500020214     C                   PARM                    PAXKCM           80
164600020214     C                   PARM                    PAXKSM          140
164700020214     C                   PARM                    PAXKDM           60
164800020214      *
164900020214 2a  C     PaxSta        ifgt      -1
165000020214     C                   movel     paxksm        $campo7
165100020215     C                   move      $campo7       v2ccli
165200020215     C                   movel     paxdmt        v2dcli
165300020214 2-  C                   endif
165400020214      *
165500020215     C                   goto      Finesr2
165600020214 1-  C                   endif
165700020214      *
165800020214      * Controllo validità codice
165900020215     C                   move      *blanks       v2dcli
166000020215 1a  C     v2ccli        ifne      *zeros
166100020214      *
166200020215     C                   move      v2ccli        $Cod              7 0
166300020214     C                   EXSR      DeCod
166400020214      *
166500020214 2a  C                   if        o69err = *blanks and acoflg =' '
166600020215     C                   movel     acorag        v2dcli
166700020214      *
166800020215      * Controllo che i due codici cliente siano diversi
166900020215 3a  C     v2ccli        ifeq      v2ccod
167000020214     C                   seton                                        2862
167100020215     C                   goto      Finesr2
167200020214 3-  C                   endif
167300020215      *
167400020215      * Controllo che i due codici cliente siano POSTE/non POSTE
167500020215     C     v2ccod        div       10000         $Pou1
167600020215     C     v2ccli        div       10000         $Pou2
167700020215     C                   exsr      CtrPos
167800020215 3a  C   24              do
167900020215     C                   seton                                        2861
168000020215     C                   goto      FineSr2
168100020215 3-  C                   enddo
168200020215      *
168300020214 2e  C                   else
168400020214     C                   seton                                        2862
168500020214 2-  C                   endif
168600020214 1-  C                   endif
168700020208      *
168800020215     C     Finesr2       ENDSR
168900020208      *--------------------------------------------------------------------------------------------*
169000020208      * Scrittura/ Aggiornamenti
169100020208      *--------------------------------------------------------------------------------------------*
169200020208     C     GesAgg        BEGSR
169300091117      *
169400091117      * Carico in schiera i P.O.
169500091117 1a  C     TipVid        ifeq      '1'
169600091117     C                   exsr      CarPou
169700091117 1-  C                   endif
169800091117      /free
169900091117
170000091117         //? N.B.                                                      ?
170100091117         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
170200091117         //   trasmissione (vedi flag TBEFTR).                        ?
170300091117         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
170400091117         //   subito nello stesso file di entrambi i S.I. - in due    ?
170500091117         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
170600091117
170700091117         // -?Ciclo di elaborazione per ogni sistema informativo?
170800091117         For  w_SisInf = 1  To  %elem($Libr);
170900091117
171000091117           // -?Apertura degli archivi?
171100091117           if  w_SisInf > 1;
171200091117             exsr  sr_OpenFileTab;
171300091117             // - Reperimento dati tabella
171400091117             exsr  sr_ChainTNTBE;
171500091117           endif;
171600091117
171700091117           // -?Inserim./Aggiorn.tab."3NN" = Autorizzaz. Conferma Bolle?  ?
171800091117      /end-free
171900091117      *
172000091117sel 1c                   SELECT
172100091117      *
172200091117      * Gestisco la SCRITTURA per ogni filiale
172300091117w   1c                   WHEN      TipVid  = '1'
172400091117      *
172500091117     C                   z-add     1             X                 2 0
172600091117      *
172700091117 2a  C                   If        $Po999 = *off
172800091117      *
172900091117 3a  C                   dow       Pou(X) <> *zeros
173000091117     C                   exsr      WriTbe
173100091117     C                   add       1             X
173200091117 3-  C                   enddo
173300091117      *
173400091117 2e  C                   Else
173500091117      *
173600091117     C                   move      '999'         Pou(X)
173700091117     C                   exsr      WriTbe
173800091117      *
173900091117 2-  C                   EndIf
174000091117      *
174100091117      * Gestisco l'AGGIORNAMENTO
174200091117w   1c                   WHEN      TipVid = '2'
174300020208      *
174400020211 2a  C                   Select
174500091117      * Modifica
174600091117     C     b22opz        wheneq    '2'
174700020215     C                   move      v2ccli        $cli              7
174800020208     C                   movel     $cli          TbeUni
174900091203     ***C                   move      *blanks       TbeFtr
175000091203      /free
175100091203                 if  w_SisInf = 1;
175200091203                   TBEftr = 'T';
175300091203                 else;
175400091203                   TBEftr = 'R';
175500091203                 endif;
175600091203                 TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
175700091203                                        : 1 : 8 ) );
175800091203
175900091203      /end-free
176000020208     C                   update    tntbe000
176100091117      * Annullamento
176200091117     C     b22opz        wheneq    '4'
176300020208     C                   move      'A'           TbeAtb
176400020208     C                   move      *blanks       TbeFtr
176500020208     C                   update    tntbe000
176600091117      * Ripristino
176700091117     C     b22opz        wheneq    '7'
176800020208     C                   move      ' '           TbeAtb
176900020208     C                   move      *blanks       TbeFtr
177000091203      /free
177100091203               if  %found(TNTBE02L);
177200091203      /end-free
177300020208     C                   update    tntbe000
177400091203      /free
177500091203               else;
177600091203                 // - Record appena cancellato fisicamente
177700091203                 clear  TBEatb;
177800091203                 TBEsif = B22sif;
177900091203                 TBElin = B22lin;
178000091203                 TBEapl = $Apl;
178100091203                 TBEcod = '3NN';
178200091203                 TBEke1 = B22ke1;
178300091203                 TBEke2 = B22ke2;
178400091203                 TBEftt = 'S';
178500091203                 TBEflt = *zero;
178600091203                 TBEftr = *blank;
178700091203                 TBEdtr = *zero;
178800091203                 WRITE  TNTBE01;
178900091203               endif;
179000091203      /end-free
179100020211 2-  C                   endsl
179200091117      *
179300091117e   1C                   ENDSL
179400091117      /free
179500091117         EndFor;
179600091117      /end-free
179700091117     C                   eval      $Fine = *on
179800020212      *
179900020208     C                   ENDSR
180000091117      /free
180100091117
180200091117       //--------------------------------------------------------------
180300091117       //?Apertura dei files tabelle nel sistema informativo impostato.?
180400091117       //--------------------------------------------------------------
180500091117       BEGSR  sr_OpenFileTab;
180600091117
180700091117         // -?Chiusura (eventuale) archivi?
180800091117         if  %open(TNTBE01L);
180900091117           close  TNTBE01L;
181000091117         endif;
181100091117         if  %open(TNTBE02L);
181200091117           close  TNTBE02L;
181300091117         endif;
181400091117
181500091117         // -?Apertura archivi?
181600091117         ds_Libr  = $SisInf(w_iSystem);
181700091117         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
181800091117         open  TNTBE01L;
181900091117         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE02L';
182000091117         open  TNTBE02L;
182100091117
182200091117       ENDSR;
182300091117
182400091117      /end-free
182500020211      *--------------------------------------------------------------------------------------------*
182600020211      * CarPou - Caricamento in schiera dei punti operativi
182700020211      *--------------------------------------------------------------------------------------------*
182800020211     C     CarPou        BEGSR
182900020211     C                   move      *off          $Po999            1
183000020211     C                   z-add     1             X
183100020211     C                   movea     *zeros        Pou
183200020211      *
183300020211     C     V1CP01        ifne      *zeros
183400020211     C     V1CP01        ifeq      999
183500020211     C                   move      *on           $Po999
183600020211     C                   z-add     1             X
183700020211     C                   else
183800020211     C                   move      V1CP01        Pou(X)
183900020211     C                   add       1             X
184000020211     C                   endif
184100020211     C                   endif
184200020211      *
184300020211     C     V1CP02        ifne      *zeros
184400020212     C     $Po999        andne     *on
184500020211     C     V1CP02        ifeq      999
184600020211     C                   move      *on           $Po999
184700020211     C                   z-add     1             X
184800020211     C                   else
184900020215     C                   z-add     1             Y                 2 0
185000020215     C                   move      v1cp02        $campo3
185100020215     C     $campo3       lookup    Pou(y)                                 21
185200020215     C     *in21         ifeq      *off
185300020212     C                   move      V1CP02        Pou(X)
185400020211     C                   add       1             X
185500020211     C                   endif
185600020215     C                   endif
185700020211     C                   endif
185800020211      *
185900020211     C     V1CP03        ifne      *zeros
186000020212     C     $Po999        andne     *on
186100020211     C     V1CP03        ifeq      999
186200020211     C                   move      *on           $Po999
186300020211     C                   z-add     1             X
186400020211     C                   else
186500020215     C                   z-add     1             Y                 2 0
186600020215     C                   move      v1cp03        $campo3
186700020215     C     $campo3       lookup    Pou(y)                                 21
186800020215     C     *in21         ifeq      *off
186900020212     C                   move      V1CP03        Pou(X)
187000020211     C                   add       1             X
187100020211     C                   endif
187200020215     C                   endif
187300020211     C                   endif
187400020211      *
187500020211     C     V1CP04        ifne      *zeros
187600020212     C     $Po999        andne     *on
187700020211     C     V1CP04        ifeq      999
187800020211     C                   move      *on           $Po999
187900020211     C                   z-add     1             X
188000020211     C                   else
188100020215     C                   z-add     1             Y                 2 0
188200020215     C                   move      v1cp04        $campo3
188300020215     C     $campo3       lookup    Pou(y)                                 21
188400020215     C     *in21         ifeq      *off
188500020212     C                   move      V1CP04        Pou(X)
188600020211     C                   add       1             X
188700020211     C                   endif
188800020215     C                   endif
188900020211     C                   endif
189000020211      *
189100020211     C     V1CP05        ifne      *zeros
189200020212     C     $Po999        andne     *on
189300020211     C     V1CP05        ifeq      999
189400020211     C                   move      *on           $Po999
189500020211     C                   z-add     1             X
189600020211     C                   else
189700020215     C                   z-add     1             Y                 2 0
189800020215     C                   move      v1cp05        $campo3
189900020215     C     $campo3       lookup    Pou(y)                                 21
190000020215     C     *in21         ifeq      *off
190100020212     C                   move      V1CP05        Pou(X)
190200020211     C                   add       1             X
190300020211     C                   endif
190400020215     C                   endif
190500020211     C                   endif
190600020211      *
190700020211     C     V1CP06        ifne      *zeros
190800020212     C     $Po999        andne     *on
190900020211     C     V1CP06        ifeq      999
191000020211     C                   move      *on           $Po999
191100020211     C                   z-add     1             X
191200020211     C                   else
191300020215     C                   z-add     1             Y                 2 0
191400020215     C                   move      v1cp06        $campo3
191500020215     C     $campo3       lookup    Pou(y)                                 21
191600020215     C     *in21         ifeq      *off
191700020212     C                   move      V1CP06        Pou(X)
191800020211     C                   add       1             X
191900020211     C                   endif
192000020215     C                   endif
192100020211     C                   endif
192200020211      *
192300020211     C     V1CP07        ifne      *zeros
192400020212     C     $Po999        andne     *on
192500020211     C     V1CP07        ifeq      999
192600020211     C                   move      *on           $Po999
192700020211     C                   z-add     1             X
192800020211     C                   else
192900020215     C                   z-add     1             Y                 2 0
193000020215     C                   move      v1cp07        $campo3
193100020215     C     $campo3       lookup    Pou(y)                                 21
193200020215     C     *in21         ifeq      *off
193300020212     C                   move      V1CP07        Pou(X)
193400020211     C                   add       1             X
193500020211     C                   endif
193600020215     C                   endif
193700020211     C                   endif
193800020211      *
193900020211     C     V1CP08        ifne      *zeros
194000020212     C     $Po999        andne     *on
194100020211     C     V1CP08        ifeq      999
194200020211     C                   move      *on           $Po999
194300020211     C                   z-add     1             X
194400020211     C                   else
194500020215     C                   z-add     1             Y                 2 0
194600020215     C                   move      v1cp08        $campo3
194700020215     C     $campo3       lookup    Pou(y)                                 21
194800020215     C     *in21         ifeq      *off
194900020212     C                   move      V1CP08        Pou(X)
195000020211     C                   add       1             X
195100020211     C                   endif
195200020215     C                   endif
195300020211     C                   endif
195400020211      *
195500020211     C     V1CP09        ifne      *zeros
195600020212     C     $Po999        andne     *on
195700020211     C     V1CP09        ifeq      999
195800020211     C                   move      *on           $Po999
195900020211     C                   z-add     1             X
196000020211     C                   else
196100020215     C                   z-add     1             Y                 2 0
196200020215     C                   move      v1cp09        $campo3
196300020215     C     $campo3       lookup    Pou(y)                                 21
196400020215     C     *in21         ifeq      *off
196500020212     C                   move      V1CP09        Pou(X)
196600020211     C                   add       1             X
196700020211     C                   endif
196800020215     C                   endif
196900020211     C                   endif
197000020211      *
197100020211     C     V1CP10        ifne      *zeros
197200020212     C     $Po999        andne     *on
197300020211     C     V1CP10        ifeq      999
197400020211     C                   move      *on           $Po999
197500020211     C                   z-add     1             X
197600020211     C                   else
197700020215     C                   z-add     1             Y                 2 0
197800020215     C                   move      v1cp10        $campo3
197900020215     C     $campo3       lookup    Pou(y)                                 21
198000020215     C     *in21         ifeq      *off
198100020212     C                   move      V1CP10        Pou(X)
198200020211     C                   add       1             X
198300020211     C                   endif
198400020215     C                   endif
198500020211     C                   endif
198600020211      *
198700020211     C     V1CP11        ifne      *zeros
198800020212     C     $Po999        andne     *on
198900020211     C     V1CP11        ifeq      999
199000020211     C                   move      *on           $Po999
199100020211     C                   z-add     1             X
199200020211     C                   else
199300020215     C                   z-add     1             Y                 2 0
199400020215     C                   move      v1cp11        $campo3
199500020215     C     $campo3       lookup    Pou(y)                                 21
199600020215     C     *in21         ifeq      *off
199700020212     C                   move      V1CP11        Pou(X)
199800020211     C                   add       1             X
199900020211     C                   endif
200000020215     C                   endif
200100020211     C                   endif
200200020211      *
200300020211     C     V1CP12        ifne      *zeros
200400020212     C     $Po999        andne     *on
200500020211     C     V1CP12        ifeq      999
200600020211     C                   move      *on           $Po999
200700020211     C                   z-add     1             X
200800020211     C                   else
200900020215     C                   z-add     1             Y                 2 0
201000020215     C                   move      v1cp12        $campo3
201100020215     C     $campo3       lookup    Pou(y)                                 21
201200020215     C     *in21         ifeq      *off
201300020212     C                   move      V1CP12        Pou(X)
201400020211     C                   add       1             X
201500020211     C                   endif
201600020215     C                   endif
201700020211     C                   endif
201800020211      *
201900020211     C     V1CP13        ifne      *zeros
202000020212     C     $Po999        andne     *on
202100020211     C     V1CP13        ifeq      999
202200020211     C                   move      *on           $Po999
202300020211     C                   z-add     1             X
202400020211     C                   else
202500020215     C                   z-add     1             Y                 2 0
202600020215     C                   move      v1cp13        $campo3
202700020215     C     $campo3       lookup    Pou(y)                                 21
202800020215     C     *in21         ifeq      *off
202900020212     C                   move      V1CP13        Pou(X)
203000020211     C                   add       1             X
203100020211     C                   endif
203200020215     C                   endif
203300020211     C                   endif
203400020211      *
203500020211     C     V1CP14        ifne      *zeros
203600020212     C     $Po999        andne     *on
203700020211     C     V1CP14        ifeq      999
203800020211     C                   move      *on           $Po999
203900020211     C                   z-add     1             X
204000020211     C                   else
204100020215     C                   z-add     1             Y                 2 0
204200020215     C                   move      v1cp14        $campo3
204300020215     C     $campo3       lookup    Pou(y)                                 21
204400020215     C     *in21         ifeq      *off
204500020212     C                   move      V1CP14        Pou(X)
204600020211     C                   add       1             X
204700020211     C                   endif
204800020215     C                   endif
204900020211     C                   endif
205000020211      *
205100020211     C     V1CP15        ifne      *zeros
205200020212     C     $Po999        andne     *on
205300020211     C     V1CP15        ifeq      999
205400020211     C                   move      *on           $Po999
205500020211     C                   z-add     1             X
205600020211     C                   else
205700020215     C                   z-add     1             Y                 2 0
205800020215     C                   move      v1cp15        $campo3
205900020215     C     $campo3       lookup    Pou(y)                                 21
206000020215     C     *in21         ifeq      *off
206100020212     C                   move      V1CP15        Pou(X)
206200020211     C                   add       1             X
206300020211     C                   endif
206400020215     C                   endif
206500020211     C                   endif
206600020211      *
206700020211     C     V1CP16        ifne      *zeros
206800020212     C     $Po999        andne     *on
206900020211     C     V1CP16        ifeq      999
207000020211     C                   move      *on           $Po999
207100020211     C                   z-add     1             X
207200020211     C                   else
207300020215     C                   z-add     1             Y                 2 0
207400020215     C                   move      v1cp16        $campo3
207500020215     C     $campo3       lookup    Pou(y)                                 21
207600020215     C     *in21         ifeq      *off
207700020212     C                   move      V1CP16        Pou(X)
207800020211     C                   add       1             X
207900020211     C                   endif
208000020215     C                   endif
208100020211     C                   endif
208200020211      *
208300020211     C     V1CP17        ifne      *zeros
208400020212     C     $Po999        andne     *on
208500020211     C     V1CP17        ifeq      999
208600020211     C                   move      *on           $Po999
208700020211     C                   z-add     1             X
208800020211     C                   else
208900020215     C                   z-add     1             Y                 2 0
209000020215     C                   move      v1cp17        $campo3
209100020215     C     $campo3       lookup    Pou(y)                                 21
209200020215     C     *in21         ifeq      *off
209300020212     C                   move      V1CP17        Pou(X)
209400020211     C                   add       1             X
209500020211     C                   endif
209600020215     C                   endif
209700020211     C                   endif
209800020211      *
209900020211     C     V1CP18        ifne      *zeros
210000020212     C     $Po999        andne     *on
210100020211     C     V1CP18        ifeq      999
210200020211     C                   move      *on           $Po999
210300020211     C                   z-add     1             X
210400020211     C                   else
210500020215     C                   z-add     1             Y                 2 0
210600020215     C                   move      v1cp18        $campo3
210700020215     C     $campo3       lookup    Pou(y)                                 21
210800020215     C     *in21         ifeq      *off
210900020212     C                   move      V1CP18        Pou(X)
211000020211     C                   add       1             X
211100020211     C                   endif
211200020215     C                   endif
211300020211     C                   endif
211400020211      *
211500020211     C     V1CP19        ifne      *zeros
211600020212     C     $Po999        andne     *on
211700020211     C     V1CP19        ifeq      999
211800020211     C                   move      *on           $Po999
211900020211     C                   z-add     1             X
212000020211     C                   else
212100020215     C                   z-add     1             Y                 2 0
212200020215     C                   move      v1cp19        $campo3
212300020215     C     $campo3       lookup    Pou(y)                                 21
212400020215     C     *in21         ifeq      *off
212500020212     C                   move      V1CP19        Pou(X)
212600020211     C                   add       1             X
212700020211     C                   endif
212800020211     C                   endif
212900020215     C                   endif
213000020211      *
213100020211     C                   ENDSR
213200020211      *--------------------------------------------------------------------------------------------*
213300020211      * WriTbe - Scrittura tabella
213400020211      *--------------------------------------------------------------------------------------------*
213500020211     C     WriTbe        BEGSR
213600020211      *
213700020211     C                   clear                   Tntbe01
213800020211     C                   move      ' '           TbeAtb
213900020211     C                   movel     $Sif          TbeSif
214000020211     C                   movel     $Lin          TbeLin
214100020211     C                   movel     $Apl          TbeApl
214200020211     C                   move      '3NN'         TbeCod
214300020211     C                   move      *blanks       TbeKe1
214400020211     C                   move      *blanks       TbeKe2
214500020211      *
214600020211     C                   move      V1ccod        $Campo7           7
214700020211     C                   movel     $Campo7       TbeKe1
214800020211      *
214900020215     C     v1ccli        ifne      *zeros
215000020211     C                   move      V1ccli        $Campo7           7
215100020211     C                   movel     $Campo7       TbeUni
215200020215     C                   else
215300020215     C                   movel     *blanks       TbeUni
215400020215     C                   endif
215500020215      *
215600020211     C                   movel     POU(X)        TbeKe2
215700020211      *
215800020211     C                   move      'S'           TbeFtt
215900020211     C                   z-add     *zeros        TbeFlt
216000091203     ***C                   move      ' '           TbeFtr
216100091203      /free
216200091203                 if  w_SisInf = 1;
216300091203                   TBEftr = 'T';
216400091203                 else;
216500091203                   TBEftr = 'R';
216600091203                 endif;
216700091203      /end-free
216800020211     C                   move      *zeros        TbeDtr
216900020211      *
217000020211     C                   write     Tntbe01
217100020211      *
217200020211     C                   ENDSR
217300020208      *--------------------------------------------------------------------------------------------*
217400020208      * F03ges - Tasto funzionale f03 -> Fine programma
217500020208      *--------------------------------------------------------------------------------------------*
217600020208     C     F03ges        BEGSR
217700020208      *
217800091117     C                   eval      $Fine = *on                                  *fine pgm
217900020208     C                   clear                   tntb22ds
218000020208     C                   move      'F03'         b22Fun
218100020208      *
218200020208     C                   ENDSR
218300020208      *--------------------------------------------------------------------------------------------*
218400020208      * F12ges - Tasto funzionale f12 -> Ritorno
218500020208      *--------------------------------------------------------------------------------------------*
218600020208     C     F12ges        BEGSR
218700020208      *
218800091117     C                   eval      $Fine = *on                                  *fine pgm
218900020208      *
219000020208     C                   ENDSR
219100020208      *--------------------------------------------------------------------------------------------*
219200020208      * DeCod  - Decodifica codice cliente
219300020208      *--------------------------------------------------------------------------------------------*
219400020208     C     DeCod         BEGSR
219500020208      *
219600020208     C                   clear                   tibs69ds
219700091117     C                   movel     DUTkci        I69Kcc
219800020208     C                   movel     $Cod          I69Kac
219900020208     C                   movel     KnSif         I69Sif
220000020208     C                   call      'TIBS69R'
220100020208     C                   parm                    Tibs69Ds
220200020208     C                   parm                    DsAco
220300020208     C                   parm                    DsInd
220400020208     C                   parm                    DsClp
220500020208     C                   parm                    DsCls
220600020208      *
220700020208     C                   ENDSR
220800020212      *--------------------------------------------------------------------------------------------*
220900020212      * ChnTbe - Aggancio tabella per controllo esistenza record
221000020212      *--------------------------------------------------------------------------------------------*
221100020212     C     ChnTbe        BEGSR
221200020212     C                   setoff                                       23
221300020212     C     K01Tbe5       chain     Tntbe01l                           23
221400020212      *
221500020212     C                   ENDSR
221600020212      *--------------------------------------------------------------------------------------------*
221700020212      * CtrPos - Controllo clienti o poste o non  poste
221800020212      *--------------------------------------------------------------------------------------------*
221900020212     C     CtrPos        BEGSR
222000020212     C                   setoff                                       24
222100020212      * Controllo se poste o no il codice P.O
222200020212     C                   move      'N'           wcli1             1
222300020212     C     $Pou1         chain     azorg01l                           23
222400020212     C                   movel     Orgde3        og143
222500020215     C     §Ogntw        ifeq      'PPT'
222600020212     C                   move      'S'           Wcli1
222700020212     C                   else
222800020212     C                   move      'N'           Wcli1
222900020212     C                   endif
223000020212      *
223100020212      * Controllo se poste o no il cliente
223200020212     C                   move      'N'           wcli2             1
223300020214     C     $Pou2         chain     azorg01l                           23
223400020212     C                   movel     Orgde3        og143
223500020215     C     §Ogntw        ifeq      'PPT'
223600020212     C                   move      'S'           Wcli2
223700020212     C                   else
223800020212     C                   move      'N'           Wcli2
223900020212     C                   endif
224000020212      *
224100020212     C     Wcli1         ifne      Wcli2
224200020215     C     $pou2         andne     999
224300020212     C                   seton                                        24
224400020212     C                   endif
224500020212      *
224600020212     C                   ENDSR
224700020212      *--------------------------------------------------------------------------------------------*
224800020320      * CtrSpe-Controllo che il cliente non mantenga il num. spedizione
224900020212      *--------------------------------------------------------------------------------------------*
225000020212     C     CtrSpe        BEGSR
225100020212     C                   setoff                                       25
225200020320     C                   clear                   Ds3C
225300020212     C                   move      1             $Kut              1 0
225400020212     C                   move      '3C'          $Cod1             2
225500020212     C                   movel     *blanks       $Key              8
225600020212     C                   movel     $Campo7       $Key
225700020212     C     Ktabel        chain     Tabel00f                           21
225800020212     C                   movel     TblUni        Ds3C
225900020212     C     *in21         ifeq      *off
226000020212     C     §3CFsp        andeq     'S'
226100020212     C                   seton                                        25
226200020212     C                   endif
226300020212      *
226400020212     C                   ENDSR
226500020320      *--------------------------------------------------------------------------------------------*
226600020320      * CtrCdT-Controllo su cliente se il codice trattamento è tipo Disk 'B'
226700020320      *--------------------------------------------------------------------------------------------*
226800020320     C     CtrCdT        BEGSR
226900020320     C                   setoff                                       25
227000020320     C                   clear                   Ds1B
227100020320     C                   move      1             $Kut              1 0
227200020320     C                   move      '1B'          $Cod1             2
227300020320     C                   movel     *blanks       $Key              8
227400020320     C                   movel     §3CCtm        $Key
227500020320     C     Ktabel        chain     Tabel00f                           21
227600020320     C                   movel     TblUni        Ds1B
227700020320     C     *in21         ifeq      *off
227800020320     C     §1Bfg1        andeq     'S'
227900020320     C                   seton                                        25
228000020320     C                   endif
228100020320      *
228200020320     C                   ENDSR
228300020208      *--------------------------------------------------------------------------------------------*
228400020208      * RutInz - operazioni iniziali
228500020208      *--------------------------------------------------------------------------------------------*
228600020208     C     rutinz        BEGSR
228700020208      *
228800020208      * Ricevimento parametri
228900020208     C     *ENTRY        PLIST
229000020208     C                   PARM                    kpjba
229100020208     C                   PARM                    Tntb22Ds
229200091117
229300091117      /free
229400091117
229500091117         *inLR = *on;
229600091117
229700091117         // -?Verifica del sistema AS/400 corrente?
229800091117         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
229900091117           $Fine = *on;
230000091117           leavesr;
230100091117         endif;
230200091117
230300091117         // -?Impostazione elenco librerie in cui gestire le tabelle?
230400091117         //  ?(a seconda del sistema in cui si stà lavorando)?
230500091117         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
230600091117         if  w_iSystem = *zero;
230700091117           $Fine = *on;
230800091117           leavesr;
230900091117         endif;
231000091117
231100091117         // -?Reperimento dati job?
231200091117         exsr  sr_DatiJob;
231300091117
231400091117         // -?Impostazione nome programma a video?
231500091117         V1Tpgm = SDSpgm;
231600091117
231700091117         // -?Apertura file TABEL00F del 1° S.I. (sede)?
231800091117         w_SisInf = 1;
231900091117         exsr  sr_OpenFileTab;
232000091117
232100091117      /end-free
232200020208      *
232300020208      * A seconda dell'opzione imposto la videata da gestire
232400020208     C     B22Fun        ifeq      'F10'
232500020208     C                   move      '1'           TipVid
232600091117     C                   move      *on           $CarVd1
232700020208     C                   else
232800020208     C                   move      '2'           TipVid
232900091117     C                   move      *on           $CarVd2
233000020208     C                   endif
233100091117      *
233200091117     c                   clear                   V1Topz
233300020211      *
233400020211      * Reperisco testata tabella
233500020211     C                   movel     *blanks       Tbecod
233600020211     C                   move      *zeros        TbeKe1
233700020211     C                   move      '3NN'         TbeKe1
233800020211     C     K01Tbe2       chain     Tntbe01l                           21
233900020211      *
234000091117      * Se non esiste la testata tabella: non faccio nulla
234100091117     C   21              eval      $Fine = *on
234200020211      *
234300020211     C  n21              do
234400020211     C                   movel     TbeSif        $Sif             10
234500020211     C                   movel     TbeApl        $Apl              2
234600020211     C                   movel     TbeLin        $Lin              1
234700020215     C                   move      '3NN'         TbeCod
234800020211     C                   enddo
234900020211      *
235000020211     C                   EndSr
235100020211      *--------------------------------------------------------------------------------------------*
235200020208      *
235300020208      * chiavi di lettura
235400020208      *
235500020208     C     k02tbe4       KLIST                                                  *tntbe01l
235600020208     C                   KFLD                    tbecod                          -tabella
235700020208     C                   KFLD                    tbelin                          -chiave uno
235800020208     C                   KFLD                    tbesif                          -chiave due
235900020208     C                   KFLD                    tbeke1                          -lingua
236000020208      *
236100020208     C     k02tbe5       KLIST                                                  *tntbe01l
236200020208     C                   KFLD                    tbecod                          -tabella
236300020208     C                   KFLD                    tbelin                          -chiave uno
236400020208     C                   KFLD                    tbesif                          -chiave due
236500020208     C                   KFLD                    tbeke1                          -lingua
236600020208     C                   KFLD                    tbeke2                          -lingua
236700020208      *
236800020211     C     k01tbe2       KLIST                                                  *tntbe01l
236900020211     C                   KFLD                    tbecod                          -tabella
237000020211     C                   KFLD                    tbeke1                          -chiave uno
237100020212      *
237200020212     C     k01tbe5       KLIST                                                  *tntbe01l
237300020212     C                   KFLD                    TbeCod                          -tabella
237400020212     C                   KFLD                    tbeke1                          -chiave uno
237500020212     C                   KFLD                    tbeKe2                          -tabella
237600020212     C                   KFLD                    $lin                            -chiave uno
237700020212     C                   KFLD                    $sif                            -tabella
237800020212      *
237900020212     C     ktabel        KLIST                                                  *tntbe01l
238000020212     C                   KFLD                    $Kut                            -tabella
238100020212     C                   KFLD                    $Cod1                           -chiave uno
238200020212     C                   KFLD                    $key                            -tabella
238300091117      /free
238400091117
238500091117       //--------------------------------------------------------------
238600091117       //?Reperimento Dati del job (Utente/Operativi).                 ?
238700091117       //--------------------------------------------------------------
238800091117       BEGSR  sr_DatiJob;
238900091117
239000091117         in(e) §AzUte;
239100091117         if NOT %error;
239200091117           in(e) §DatiUte;
239300091117         endif;
239400091117         if %error or RSut = *blank;
239500091117           tibs34r ( tibs34ds );
239600091117           in §AzUte;
239700091117           in §DatiUte;
239800091117         endif;
239900091117
240000091117       ENDSR;
240100091117
240200091117      /end-free
240300091117
240400091117       //--------------------------------------------------------------
240500091117       //?Schiere a tempo di compilazione.                             ?
240600091117       //--------------------------------------------------------------
240700091117
240800091117** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con entrambi i file?
240900091127SETRAS  GAITRAGRU FILTRAGRU
241000091117AS888   GAITRAGRPSFILTRAGRPF
241100020212** Msg
241200020212Immettere almeno un punto operativo che puo' confermare le bolle del cliente
241300020212Punto operativo inesistente o errato
241400020212P.O. già abilitato per cliente
241500020212P.O. 999 non valido se cliente mantiene Num. spedizione
241600020215Il P.O. del cliente e i P.O scelti devono essere POSTE o NON POSTE
241700020320P.O. 999 non valido se cliente ha codice trattamento tipo Disk 'B'
