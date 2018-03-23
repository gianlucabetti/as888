000100050201      *---------------------------------------------------------------*
000200050201      *    RISTAMPA PROGETTO DI LIQUIDAZIONE                          *
000300050201      *---------------------------------------------------------------*
000400980831
000500050201      *****************************************************************
000600050201      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)     *
000700050201      *****************************************************************
000800980715
000900050201      *****************************************************************
001000050201      *  RIEPILOGO INDICATORI                                         *
001100050201      *****************************************************************
001200990108      * 01 - VISUALIZZO evento
001300050222     *** * 02 - VISUALIZZO numero mandato
001400990126      * 03 - RISTAMPA numero mandato
001500990126      * 04 - VISUALIZZO rivalsa
001600990325      * 05 - STAMPO numero serie spedizione
001700990415      * 06 - NON VISUALIZZO mandato assicurativo
001800050523      *--
001900050523      * 25 - Stampa Numero polizza
002000050523      * 26 - Stampa Riferimento mittente alfanumerico
002100050523      *--
002200990614      * 30 - COMODO
002300060621      *--
002400060621      * 35 - Stampo frase relative alle sole spedizioni assicurate
002500050201      *****************************************************************
002600980618
002700951009     H DECEDIT('0,') DATEDIT(*DMY.)
002800050201      *---------------------------------------------------------------*
002900981005     fTABEL00F  IF   E           K DISK
003000120427     fFNCLS01L  if   e           k disk
003100990615     fFNDCT01L  IF   E           K DISK
003200981005     fFNDET01L  IF   E           K DISK
003300050207     fFNDCL01L  IF   E           K DISK
003400050207     fFNDKA01L  IF   E           K DISK
003500050207     fFNDCS01L  IF   E           K DISK
003600990415     fFNDPT01L  IF   E           K DISK
003700990909     fTITAS30C  IF   E           K DISK
003800050523     fTITA430C  IF   E           K DISK
003900120427     fFIDN29P   o    e             printer usropn
004000120427     fFIDN29P2  o    e             printer usropn
004100130920     fFIDN29PG  o    e             printer usropn
004200130920     fFIDN29PG2 o    e             printer usropn
004300050201      *---------------------------------------------------------------*
004400990203     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
004500060316     D VLR             S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Video
004600050222     D*SLR***          S             56    DIM(7) CTDATA PERRCD(1)              Limite Risar. Stampa
004700060316     D SLR             S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Stampa
004800120502     D SLR2            S             78    DIM(9) CTDATA PERRCD(1)              Limite Risar. Stampa
004900990126     D W35             S             35    DIM(10)
005000050201      * ------------------------------------------------------------- *
005100050207     D KPJBA         E DS
005200050207      *
005300981020     D FIDN00DS      E DS
005400990909     d  ds_numca               6     19
005500050207      *
005600050207      * - Parametri x Controllo profilo utenti
005700050207     d TIBS34ds      e ds
005800050207      * - Ds di riferimento al file esterno AZUTE00F
005900050207     d AZUTEds       e ds                  extname(AZUTE00F)
006000050207      * - Ds per dati organigramma
006100050207     d dDATIUTE      e ds
006200050207      *
006300981020     D TIBS02DS      E DS
006400981020     D DTAD          E DS
006500050523     d DTA4A         e ds                  inz
006600981020     D DSTB          E DS
006700981020     D DSCV          E DS
006800010910     D DGEDDN        E DS
006900010417     D DGEI          E DS
007000130920     D DSTD          E DS                  INZ
007100990125     D DS15          E DS
007200981228     D DDCT01        E DS
007300020301     d ddpz          e ds
007400010907      * Dati per gestione Euro
007500010907     D YEURCODS      E DS                  inz
007600010907     D   YECTAR      e                     inz('H')
007700010907      * Dati per editazione di un numero
007800010907     D XEDITDS       E DS                  inz
007900010907     D   OedFlgErr   e                     inz(*off)
008000980615
008100990108     d                 ds                  INZ
008200990108     d  DS_datasp              1      8  0
008300990108     d  TASaas                 1      4  0
008400990108     d  TASmgs                 5      8  0
008500990108
008600971016     D WLBDAT          DS                  INZ
008700971016     D  G02DAT                 1      8  0
008800971016     D  G02INV                 9     16  0
008900971016     D  G02ERR                17     17
009000971016     D  G02TGI                18     22  0
009100980615
009200990126     D Wdataeur        S               D   DATFMT(*eur)
009300990126     D Wdataoggi       S               D   DATFMT(*eur)
009400010907     D W§GEcmr         S             13  3 inz
009500010907     D W§GElrp         S             13  3 inz
009600990203     D Wprv            S                   LIKE(DKAprv)
009700981020     D Wtrc            S                   LIKE(DKAtrc)
009800990909     D Kcod            S                   LIKE(TBLcod)
009900990909     D Kkey            S                   LIKE(TBLkey)
010000990202     D Ktpd            S                   LIKE(DCStpd)
010100990202     D Knkt            S                   LIKE(DCSnkt)
010200990202     D Kstd            S                   LIKE(DCSstd)
010300990202     D Kdim            S                   LIKE(DCSdim)
010400990202     D Khim            S                   LIKE(DCShim)
010500990202     D Knks            S                   LIKE(DCSnks)
010600990202     D Ktrc            S                   LIKE(DCStrc)
010700050202     d SavDSCV         s                   like(DScv)   inz
010800981124
010900990204     D Wlri            S            100
011000990203     D Wnaz            S             30
011100000204     D wrivalsa        s              1
011200980625     D Wkey1           s             15
011300990205     D xcodice         s             31
011400990205     D xtipo           s              1
011500981104     D w003a           s              3
011600990125     D w004a           s              4
011700990204     D w037a           s             37
011800980720     D w0033           s              3  3
011900990126     D w0133           s             13  3
012000980720     D w0140           s             14  0
012100990126     D JJ              s              3  0
012200980626     D XX              s              2  0
012300990205     D xlen            s              5  0
012400980611     D wdtgio          s              8  0
012500980611     D dateu           s              8  0
012600130920     D wDataGLD        s              8  0
012700990909
012800990909     D V1Cipl          s             12  3
012900000105     D W035a           s             35
013000000105     D W035b           s             35
013100010907      *-------------
013200010907     D wImporto        S             30S 5 inz
013300010907     D wNrDec          S              2S 0 inz
013400120427
013500120427     d wInglese        s               n   inz(*off)
013600050203
013700050203     d C_Text_A        c                   const('quale risarcimento per -
013800050203     d                                     il danno più sopra indicato.')
013900120427     d C_Text_A2       c                   const('as compensation for the -
014000150605     d                                     a.m. claim.')
014100990909
014200050201      *---------------------------------------------------------------*
014300990615      *  AGGANCIO ARCHIVI
014400990615     C                   EXSR      AGG_files
014500990614
014600990614      *  NUMERO POLIZZA
014700990614      *     Per determinala utilizzo l'azienda e l'anno del Numero Pratica Assicurativa
014800990614     c                   clear                   TIBS02DS
014900990614     C                   MOVEL     'C'           t02mod
015000990614     C                   MOVEL     knsif         t02sif
015100990614     C                   MOVEL     'DPZ'         t02cod
015200990614     C                   MOVEL     DCTpra        W004a
015300990614     C                   EVAL      t02ke1 = DCTdit + W004a
015400990614      *
015500990614     C                   CALL      'TIBS02R'
015600990614     C                   PARM                    KPJBA
015700990614     C                   PARM                    TIBS02DS
015800990614      *
015900990614     C                   If        t02err = *BLANKS
016000020301     c                   movel     t02uni        ddpz
016100050201     ***c                   eval      v1cnpl = §dpznpla
016200990614     C                   Endif
016300050523      * se ca su spedizione maggiore del 17042005 stampo anche il numero polizza
016400050523     c                   If        ds_datasp > 20050417
016500050523     c                   seton                                        25
016600050524     c                   eval      P1npz = §dpznpla
016700050523     c                   endif
016800050201
016900050201      * Numero danno
017000050201     c                   eval      P1prd  = %trim(%editc(DCTpra:'Z'))
017100050201     c                                    + '/'
017200050201     c                                    + %trim(%editc(DCTprn:'Z'))
017300990614
017400990614      * DATA SPEDIZIONE
017500990614     c     *iso          move      DS_datasp     Wdataeur
017600050201     c                   move      Wdataeur      P1dsp
017700990614
017800990614      * SERIE SPEDIZIONE
017900050201     ***c                   eval      *in05 = (DCTnrs > *zeros)
018000050201if  1c                   if        DCTnrs > *zeros
018100050201     c                   eval      P1sesp = %trim(%editc(DCTnrs:'Z'))
018200050201     c                                    + ' '
018300050201     c                                    + %trim(%editc(DCTnsp:'Z'))
018400050201x   1c                   else
018500050201     c                   eval      P1sesp = %trim(%editc(DCTnsp:'Z'))
018600050201e   1c                   endif
018700990614
018800990614      * DESCRIZIONE ANOMALIA     Evento o C.A.
018900990614     c                   IF        DCTnev > *zeros
019000990614     C     KDET          chain     FNDET000                           30
019100990614     C                   movel(P)  DETtad        WKEY1
019200990614     c                   ELSE
019300990614     C                   movel(P)  DCTtad        WKEY1
019400990614     c                   ENDIF
019500990614      *
019600990614     C                   exsr      CHTAD
019700990614     c                   If        §TADdest <> *blanks
019800050201     C                   movel     §TADdest      P1tad
019900990614     c                   Else
020000050201     C                   movel     §TADdesc      P1tad
020100990614     c                   Endif
020200120502     c                   IF        wInglese and §TADdesi <> *blanks
020300120502     c                   eval      P1tad = §TADdesi
020400120502     c                   ENDIF
020500990614
020600990909      * Compongo "Risarcibile con il limite"
020700990615     c                   EXSR      SUB_tipris
020800990615      *
020900120502     c                   IF        not wInglese
021000050201     c                   eval      P1lri1 = §gedDBA + ' '
021100050203     c                                    + %trim(OedNUMAN)
021200050201     c                                    + SLR(xx)
021300120502     c                   ELSE
021400120502     c                   eval      P1lri1 = §gedDBA  +   ' '
021500120502     c                                    +  %TRIM(OedNUMAN)
021600120502     c                                    +  SLR2(xx)
021700120502     c                   ENDIF
021800050222     ***c                   SELECT
021900050222     ***c                   WHEN      XX = 2  or  XX = 5
022000050222     ***c                   eval      P1lri2 = ' assicurativo numero ' + DCLnma
022100050222     ***c                   WHEN      XX = 6  or  XX = 7
022200050222     ***c                   eval      P1lri2 = ' numero ' + DCLnma
022300050222     ***c                   ENDSL
022400990909
022500990909      * Descrizione calcolo del risarcimento
022600990909     c                   exsr      descris
022700990614
022800990910      * Imposto la data del giorno nella data stampa
022900990910     c     *iso          move      DCLdds        Wdataeur
023000050201     c                   move      Wdataeur      P1dds
023100990909
023200050203      * Valuta & Importo da Liquidare (con 2 decimali)
023300050202     c                   eval      wImporto  = DCLipl
023400050207     c***                eval      wNrDec    = §cvDEC
023500050203     c                   eval      wNrDec    = 2
023600050202     c                   exsr      Edit
023700050202     c                   eval      P1ipl =  %trim(DCLvpl) + ' '
023800050202     c                                   +  %trim(OedNumAN)
023900050202     c                   movel     SavDsCV       dsCV
024000120427     c                   IF        not wInglese
024100050203     c                   eval      %subst(P1txt :
024200050203     c                                    %len(%trim(P1ipl)) + 2) =
024300050203     c                                    C_Text_A
024400120427     c                   ELSE
024500120427     c                   eval      %subst(P1txt :
024600120427     c                                    %len(%trim(P1ipl)) + 2) =
024700120427     c                                    C_Text_A2
024800120427     c                   ENDIF
024900050201      * Dati Bartolini
025000050201      * => "chiodati" nel prtf
025100130920      *
025200990614      * Dati Intestatario
025300050201     c                   movel(p)  DKArag        P1rsc
025400050201     c                   movel(p)  DKArag        P1rsc2
025500050201     c                   movel(p)  DKAvia        P1ind
025600050201     c                   eval      P1clp =  %trim(DKAcap)  +  ' '
025700050201     c                                   +  %trim(DKAloc)  +  ' '
025800050201     c                                   +  Wprv
025900130920      *
026000990614      *
026100130920if  1c                   If        DCLdds >= wDataGLD
026200120427     c                   IF        not wInglese
026300050201     c                   WRITE     DN29T1
026400130911     c                   WRITE     DN29P1
026500120427     c                   ELSE
026600120427     c                   WRITE     DN29T12
026700130911     c                   WRITE     DN29P12
026800120427     c                   ENDIF
026900130920x   1c                   Else
027000130920     c                   IF        not wInglese
027100130920     c                   WRITE     DN29T1G
027200130920     c                   ELSE
027300130920     c                   WRITE     DN29T1G2
027400130920     c                   ENDIF
027500130920e   1c                   EndIf
027600980717
027700980623      * Chiudo pgm di servizo
027800981029     c                   clear                   tibs02ds
027900981029     c                   movel     'C'           t02tla
028000981029     c                   call      'TIBS02R'
028100981029     c                   parm                    kpjba
028200981029     c                   parm                    tibs02ds
028300981029      *
028400981029     c                   movel     FIDN00DS      kpjbu
028500990909
028600990909      *
028700981005     C                   EVAL      *INLR = *ON
028800990909      ****************************************************************
028900990909      *  AGGANCIO FILES
029000990909      ****************************************************************
029100990909     C     AGG_files     BEGSR
029200990909
029300990909      * Testata C.A.
029400990909     c     knumca        CHAIN     fndct000                           30
029500990909     c                   movel     dctflo        DDCT01
029600120427
029700120427      * Importi C.A.
029800120427     c     knumca        CHAIN     fndcl000                           30
029900120427      *
030000120427      *  INTESTATARIO
030100120427     c                   movel     'I'           Wtrc
030200120427     c     Kdka          chain     FNDKA000                           30
030300120427
030400120427      * se la data della stampa del progetto di liquidazione è inferiore all'attivazione
030500120427      * della stampa in inglese stampo sempre in italiano
030600120604     c                   IF        DCLdds < 20120601
030700120427     c                   eval      wInglese = *off
030800120427     c                   ENDIF
030900120427      * se la data della stampa del progetto di liquidazione è uguale o maggiore
031000120427      * all'attivazione della stampa in inglese, con il codice del beneficiario vado
031100120427      * in anagrafica clienti a recuperare se stampa in inglese o no
031200120604     c                   IF        DCLdds >= 20120601
031300120427     c     DKAksc        chain     FNCLS01L
031400120427     c                   IF        not %found(FNCLS01L)
031500120427     c                   eval      wInglese = *off
031600120427     c                   ELSE
031700120427     c                   eval      wInglese = (%subst(CLSflo:7:1) = 'S')
031800120427     c                   ENDIF
031900120427     c                   ENDIF
032000120427      *  Apro il file di stampa italiano o inglese
032100130920     c                   If        DCLdds >= wDataGLD
032200120427     c                   IF        wInglese
032300120427     c                   open      FIDN29P2
032400120427     c                   ELSE
032500120427     c                   open      FIDN29P
032600120427     c                   ENDIF
032700130920     c                   Else
032800130920     c                   IF        wInglese
032900130920     c                   open      FIDN29PG2
033000130920     c                   ELSE
033100130920     c                   open      FIDN29PG
033200130920     c                   ENDIF
033300130920     c                   EndIf
033400990909
033500990909      * Spedizione
033600990909     c     kbol          chain     titas30c                           30
033700990909      *  se tipo bolla non è valido cerco altro rcd
033800990909     C                   movel     'TB'          kCOD
033900990909     C                   movel(P)  TAStbl        kKEY
034000990909     C     ktab          CHAIN     TABEL                              30
034100990909     C                   movel     TBLUNI        DSTB
034200990909     c                   if        §TBrbl = 'R'
034300990909     c     kbol          reade     titas30c                               30
034400990909     c                   endif
034500050523      * Riferimenti
034600050523     c                   z-add     TASrmn        P1rmn
034700050523      * il riferimento mittente alfanum. per la sede si trova in TITA4
034800050523     c     ktas4         chain     TITA430C
034900050523     c                   if        %found(TITA430C)
035000050523     c                   movel     TA4not        DTA4A
035100050523     c                   else
035200050523     c                   clear                   DTA4A
035300050523     c                   endif
035400050523     c                   movel     §TA4Arma      P1rma
035500050523     c     §TA4Arma      comp      *blanks                            2626
035600990909
035700990909      * Importi C.A.
035800120427     c**!!!knumca        CHAIN     fndcl000                           30
035900990909
036000990909      * Mandato Assicurativo
036100990909     c     knumca        CHAIN     fndpt000                           06
036200990909     c                   IF        *IN06 = *OFF  and  DPTatb = 'A'
036300990909     c                   eval      *in06 = *on
036400990909     c                   ENDIF
036500000204      *
036600000204      * Controllo se ho rivalsa
036700000204     C                   movel     'R'           Wtrc
036800000204     c     Kdka          chain     FNDKA000                           30
036900000204     c                   IF        *in30 = *off  and  DKAatb = *blanks
037000000204     c                   eval      Wrivalsa = 'S'
037100000204     c                   ENDIF
037200990909      *
037300990909      *  INTESTATARIO
037400120427     c**!!!              movel     'I'           Wtrc
037500120427     c**!!!Kdka          chain     FNDKA000                           30
037600990909      *
037700990909     c                   If         DKAprv <> *blanks
037800050201     c                   movel     DKAprv        Wprv
037900990909     c                   Else
038000050201     c                   movel     DKAnaz        Wnaz
038100990909     c                   exsr      ctrnaz
038200050201     c                   movel     §15des        Wnaz
038300050201     c                   movel(p)  §15des        P1naz
038400990909     c                   Endif
038500990909      *
038600990909     C                   ENDSR
038700990615      *****************************************************************
038800990615      *   DETERMINO IL TIPO RISARCIMENTO
038900990615      *****************************************************************
039000990615     C     SUB_tipris    BEGSR
039100990615
039200990615     c                   clear                   xx
039300990615
039400021104      * Se PA EXPORT o PF IMPORT, non dpd, ho tipo risarcimento Estero CMR
039500010907    1c                   IF        (§DCTtisp = 'E'  and  §DCTport = 'A')  or
039600021104     c                             (§DCTtisp = 'I'  and  §DCTport = 'F'
039700021104     c                                              and  §DCTeuro <> 'D')
039800990615     c                   eval      XX = 4
039900010907     c                   z-add     W§GEcmr       wImporto
040000011026     c                   z-add     3             wNrDec
040100990615     c                   ELSE
040200990615
040300990615      * Esposto il valore in bolla
040400010907    2c                   IF        TASias > *zeros  and  TASfcm <> 'F'
040500990615     c                   eval      XX = 3
040600011114     c                   if        TASias <> *zeros and TASvas = *blanks
040700011114     c                   movel     'ITL'         TASvas
040800011114     c                   endif
040900990615      *
041000990615     c                   z-add     TASias        W0133
041100990615     c                   movel     TAsvas        W003a
041200010910    4C                   if        TASvas <> §gedDBA
041300010907     C                   exsr      srCMB
041400010907   -4c                   endif
041500010907     c                   z-add     W0133         wImporto
041600010907     c                   z-add     §cvDEC        wNrDec
041700990615     c                   ELSE
041800990615      *
041900990615      * Se il cliente è assicurato imposto dati assicurazione
042000010907    3c                   IF        *IN06 = *OFF
042100990615      *  Importo Assicurato
042200990615     c                   z-add     DPTvlm        W0133
042300990615     c                   movel     DPTvvm        W003a
042400020418    4c                   IF        DPTvlm > *zeros   and
042500020418    5c                             DPTvvm <> §gedDBA
042600010907     C                   exsr      srCMB
042700010907   -4c                   ENDIF
042800010907     c                   z-add     W0133         WImporto
042900010907     c                   z-add     §cvDEC        wNrDec
043000990615      *  Imposto costante in base al tipo valore
043100010907    4c                   SELECT
043200990615     c                   WHEN      DPTftc = 'R'  or  DPTftc = '9'                 Limite Risarcibile
043300990615     c                   eval      XX = 2
043400990615     c                   WHEN        DPTfvm = '3'                                 Kg merce
043500990615     c                   eval      XX = 5
043600990615     c                   WHEN        DPTfvm = '1'                                 colli
043700990615     c                   eval      XX = 6
043800990615     c                   WHEN        DPTfvm = '2'                                 spedizione
043900990615     c                   eval      XX = 7
044000010907   -4c                   ENDSL
044100990615      *
044200010907   -3c                   ENDIF
044300990615
044400020418      * Se non è assicurato oppure c'è RIVALSA si applica LEGGE 450 o CMR
044500021111      *    se spedizione EEX o EUP o export DPD o export FedEx
044600010907    3c                   SELECT
044700990615     c                   WHEN        XX <> *zeros
044800021111     c                   WHEN           §DCTeuro =  'X'
044900021111     c                             or   §DCTeuro =  'P'
045000020422     c                             or   §DCTeuro =  'D'
045100020422     c                             and  §DCTtisp =  'E'
045200021111     c                             or   §DCTeuro =  'F'
045300021111     c                             and  §DCTtisp =  'E'
045400990615     c                   eval      XX = 4
045500010907     c                   z-add     W§GEcmr       wImporto
045600011026     c                   z-add     3             wNrDec
045700990615     c                   OTHER
045800060316     c****               eval      XX = 1
045900060316     c****               z-add     W§GElrp       wImporto
046000060316      * Verifico il tipo di limite risarcibile in base al flag in dctflo
046100060316     c                   If        §dcttlrp = ' '
046200060316     c                   eval      XX = 1                                       * legge 450
046300060316     c                   z-add     6,2           WImporto
046400060316     c                   endif
046500060316     c                   If        §dcttlrp = '6'
046600060316     c                   eval      XX = 9                                       * rcv transitoria
046700060316     c                   z-add     6,2           WImporto
046800060316     c                   endif
046900060316     c                   If        §dcttlrp = '1'
047000060316     c                   eval      XX = 8                                       * legge 286
047100060316     c                   z-add     1             WImporto
047200060316     c                   endif
047300060316
047400010914     c                   z-add     §cvDEC        wNrDec
047500010907   -3c                   ENDSL
047600990615      *
047700010907    2c                   ENDIF
047800010907    1c                   ENDIF
047900000204      * Se esiste la Rivalsa il tipo risarcimanto è secondo la legge 450
048000000204     c                   IF        Wrivalsa = 'S'
048100010907     c                   clear                   wImporto
048200010907     c                   clear                   wNrDec
048300021111      * se spedizione EEX o EUP, oppure export DPD, o export FedEx
048400021111     c                   if             §DCTeuro =  'X'
048500021111     c                             or   §DCTeuro =  'P'
048600020422     c                             or   §DCTeuro =  'D'
048700020422     c                             and  §DCTtisp =  'E'
048800021111     c                             or   §DCTeuro =  'F'
048900021111     c                             and  §DCTtisp =  'E'
049000000204     c                   eval      XX = 4
049100010907     c                   z-add     W§GEcmr       wImporto
049200011026     c                   z-add     3             wNrDec
049300000204     c                   else
049400000204     c                   eval      XX = 1
049500060411     c*****              z-add     W§GElrp       wImporto
049600060411      * Verifico il tipo di limite risarcibile in base al flag in dctflo
049700060411     c                   If        §dcttlrp = ' '
049800060411     c                   eval      XX = 1                                       * legge 450
049900060411     c                   z-add     6,2           WImporto
050000060411     c                   endif
050100060411     c                   If        §dcttlrp = '6'
050200060411     c                   eval      XX = 9                                       * rcv transitoria
050300060411     c                   z-add     6,2           WImporto
050400060411     c                   endif
050500060411     c                   If        §dcttlrp = '1'
050600060411     c                   eval      XX = 8                                       * legge 286
050700060411     c                   z-add     1             WImporto
050800060411     c                   endif
050900060411
051000010914     c                   z-add     §cvDEC        wNrDec
051100000204     c                   ENDIF
051200000204     c                   ENDIF
051300050202      *
051400050202      * Imposto costante per video
051500050202     c                   exsr      Edit
051600050202     c                   eval      Wlri = §gedDBA + ' '
051700050203     c                                  + %TRIM(OedNUMAN)
051800050202     c                                  +  VLR(xx)
051900990615      *
052000050222     *** * Richiesta numero mandato
052100050222     *** * => MAI !
052200050222     ***c                   EVAL      *IN02 = (XX = 2  or  XX >= 5)
052300060621
052400060621      * Verifico se spedizione assicurata per pilotare la stampa di una frase
052500060621      *
052600060621     c                   eval      *in35 = (xx = 2 or xx = 3 or xx>= 5)
052700060621
052800990615      *
052900990615     C                   ENDSR
053000990909      *****************************************************************
053100990909      *  REPERIMENTO DESCRIZIONE CALCOLO RISARCIMENTO
053200990909      *****************************************************************
053300990909     C     DESCRIS       BEGSR
053400990909      *
053500990909     c                   movel     'C'           ktpd
053600990909     c                   movel(P)  DS_numca      knkt
053700990909     c                   movel     'C'           kstd
053800990909     c                   clear                   kdim
053900990909     c                   clear                   khim
054000990909     c                   clear                   knks
054100990909     c                   movel     'D'           ktrc
054200990909      *
054300990909      * Cancello note esistenti
054400990909     c     kdcs          setll     fndcs000
054500990909     c     kdcs          reade     fndcs000
054600990909    2c                   clear                   JJ
054700990909      *
054800990909     c                   dow       %EOF = *off
054900990909    2c                   add       1             JJ
055000990909      *
055100990909     c                   SELECT
055200990909     c                   WHEN      JJ = 1
055300050201     c                   movel     DCSnot        P1cr1
055400990909     c                   WHEN      JJ = 2
055500050201     c                   move      DCSnot        P1cr1
055600990909     c                   WHEN      JJ = 3
055700050201     c                   movel     DCSnot        P1cr2
055800990909     c                   WHEN      JJ = 4
055900050201     c                   move      DCSnot        P1cr2
056000990909     c                   ENDSL
056100990909      *
056200990909     c     kdcs          reade     fndcs000
056300990909     c                   enddo
056400990909      *
056500990909     C                   ENDSR
056600990126      *****************************************************************
056700990126      *  REPERIMENTO DATI TIPO ANOMALIA
056800990126      *****************************************************************
056900990126     C     CHTAD         BEGSR
057000990126      *
057100990126     c                   clear                   DTAD
057200990126      *
057300990126     c                   clear                   TIBS02DS
057400990126     C                   MOVEL     'C'           t02mod
057500990126     C                   MOVEL     knsif         t02sif
057600990126     C                   MOVEL     'TAD'         t02cod
057700990126     C                   MOVEL(P)  Wkey1         t02ke1
057800990126      *
057900990126     C                   CALL      'TIBS02R'
058000990126     C                   PARM                    KPJBA
058100990126     C                   PARM                    TIBS02DS
058200990126      *
058300990126     C                   if        t02err = *BLANKS
058400990126     C                   MOVEL     T02UNI        DTAD
058500990126     C                   endif
058600990126      *
058700990126     C                   ENDSR
058800990125      *****************************************************************
058900990125      * CONTROLLO DIVISA
059000990125      *****************************************************************
059100011026     C     CTRVLT        BEGSR
059200990125      *
059300011026     C                   MOVEL     'CV'          KCOD
059400011026     C                   MOVEL(P)  W003a         KKEY
059500011026     C     KTAB          CHAIN     TABEL00F                           30
059600990125      *
059700011026     C                   IF        *IN30 = *ON  or
059800011026     C                                (*IN30 = *OFF  and  TBLFLG <> *blanks)
059900011026     C                   clear                   DSCV
060000011026     C                   ELSE
060100011026     C                   movel     TBLuni        DSCV
060200011026     C                   ENDIF
060300990125      *
060400011026     C                   ENDSR
060500990125      *****************************************************************
060600990125      * DECODIFICO NAZIONE
060700990125      *****************************************************************
060800990125     C     CTRNAZ        BEGSR
060900990125      *
061000990125     C                   clear                   DS15
061100990125     C                   movel     '15'          KCOD
061200990125     C                   movel(P)  WNAZ          KKEY
061300990125     C     KTAB          CHAIN     TABEL00f                           30
061400990125      *
061500990125     C                   if        *IN30 = *OFF  and  TBLFLG = *blanks
061600990125     C                   movel     TBLUNI        DS15
061700990125     C                   endif
061800990125      *
061900990125     C                   ENDSR
062000010907
062100010907      *****************************************************************
062200010907      * CONVERSIONE IMPORTI IN MONETA CORRENTE
062300010907      *****************************************************************
062400010907     C     srCMB         BEGSR
062500010907      *
062600010907     C                   reset                   YEURCODS
062700010907      *
062800010907     C                   movel     W003a         yecDVI
062900010907     C                   z-add     W0133         yecIMI
063000010910     C                   movel     §gedDBA       yecDVO
063100010907      *
063200010907     C                   call      'YEURCO'
063300010907     C                   parm                    YEURCODS
063400010907      *
063500010907     C                   IF        yecESI  = ' '
063600010907     C                   z-add     yecIMO        W0133
063700011002     C                   movel     yecDVO        W003a
063800010907     C                   ENDIF
063900010907      *
064000010907     C                   ENDSR
064100010907
064200010907      ************************************************************
064300010907      * Editazione di un numero
064400010907      ************************************************************
064500010907     C     Edit          Begsr
064600010907     C*
064700010907     C                   reset                   XEDITDS
064800010907     C* numero da elaborare
064900010907     C* (WImporto ha 5 decimali, $SrcVar ha 0 decimali)
065000010907     C                   Select
065100010907     C                   When      wNrDec    = 0
065200010907     C                   eval      IedNUMERO = wImporto
065300010907     C                   When      wNrDec    = 1
065400010907     C                   eval      IedNUMERO = wImporto * 10
065500010907     C                   When      wNrDec    = 2
065600010907     C                   eval      IedNUMERO = wImporto * 100
065700010907     C                   When      wNrDec    = 3
065800010907     C                   eval      IedNUMERO = wImporto * 1000
065900010907     C                   When      wNrDec    = 4
066000010907     C                   eval      IedNUMERO = wImporto * 10000
066100010907     C                   When      wNrDec    = 5
066200010907     C                   eval      IedNUMERO = wImporto * 100000
066300010907     C                   Endsl
066400010907     C* numero decimali
066500010907     C                   eval      IedNRDEC  = wNrDec
066600010907     C* codice di editazione
066700050203     C                   eval      IedEDTCOD = '3'
066800010907     C* max lunghezza campo in output
066900050203     C                   eval      IedLENFLD = 16
067000010907     C*
067100010907     C                   call      'XEDIT'
067200010907     C                   parm                    XEDITDS
067300010907      *
067400010907     C                   ENDSR
067500010907
067600980722      *****************************************************************
067700980623      *  ROUTINE INIZIALE
067800980722      *****************************************************************
067900951009     C     *INZSR        BEGSR
068000980722
068100951009     C     *ENTRY        PLIST
068200951009     C                   PARM                    KPJBA
068300980722
068400981020     C                   MOVEL     KPJBU         FIDN00DS
068500980625
068600981126      * Dati società
068700050207     c     *dtaara       define    §azute        AZUTEds
068800050207     c     *dtaara       define    §datiute      dDATIUTE
068900050207     c                   in(E)     *dtaara
069000050207     c                   if        %ERROR or RSUT = *blanks
069100050207     c                   clear                   Tibs34Ds
069200050207     c                   call      'TIBS34R'
069300050207     c                   parm                    Tibs34Ds
069400050207     c                   in        *dtaara
069500050207     c                   endif
069600050207      *
069700050207     c                   eval      TBLkut = 1
069800050207      *
069900010417      ***
070000010417      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
070100010417      ***
070200010417      * recupero la moneta corrente
070300010417     C                   CLEAR                   tibs02ds
070400010417      *
070500010417     C                   MOVEL     'C'           T02MOD
070600010417     C                   MOVEL     KNSIF         T02SIF
070700010417     C                   MOVEL     'GED'         T02COD
070800010907     C                   MOVEL     'DANNI'       T02KE1
070900010417      *
071000010417     C                   CALL      'TIBS02R'
071100010417     C                   PARM                    KPJBA
071200010417     C                   PARM                    tibs02ds
071300010417      *
071400010910     c                   movel     T02UNI        DGEDDN
071500011026      *
071600011026      * ...ed i relativi dati
071700011026     C                   movel     §gedDBA       W003a
071800011026     C                   exsr      ctrvlt
071900130920      ***
072000130920      * Aggancio tabella con dati Consuldanni/Gld/Setras
072100130920      ***
072200130920     c                   clear                   TIBS02DS
072300130920     C                   MOVEL     'C'           t02mod
072400130920     C                   MOVEL     knsif         t02sif
072500130920     C                   MOVEL     'STD'         t02cod
072600130920     C                   MOVEL(P)  '1'           t02ke1
072700130920     C                   MOVEL(P)  §gedDBA       t02ke2
072800130920      *
072900130920     C                   CALL      'TIBS02R'
073000130920     C                   PARM                    KPJBA
073100130920     C                   PARM                    TIBS02DS
073200130920      *
073300130920     C                   clear                   DSTD
073400130920     C                   If        t02err = *BLANKS
073500130920     C                   MOVEL     T02UNI        DSTD
073600130920     C                   Endif
073700130920     C                   clear                   WLBDAT
073800130920     C                   eval      G02inv = §STDgld
073900130920     C                   eval      G02err = '3'
074000130920     C                   call      'XSRDA8'
074100130920     C                   parm                    WLBDAT
074200130920     C                   z-add     G02inv        wDataGLD
074300011026      *
074400010417      * recupero i valori nella moneta corrente
074500010417     C                   CLEAR                   tibs02ds
074600010417      *
074700010417     C                   MOVEL     'L'           T02TLA
074800010417     C                   MOVEL     'C'           T02MOD
074900010417     C                   MOVEL     KNSIF         T02SIF
075000010417     C                   MOVEL     'GEI'         T02COD
075100010910     C                   MOVEL     §GEDDBA       T02KE1
075200010417      *
075300010417     C                   CALL      'TIBS02R'
075400010417     C                   PARM                    KPJBA
075500010417     C                   PARM                    tibs02ds
075600010417      *
075700010417     c                   movel     t02uni        DGEI
075800010417      * VALORIZZO il comapo del controvalore CMR
075900010417     c                   z-add     §geccm        W§GEcmr
076000010417      * VALORIZZO il campo del Limite risarcibile
076100010417     c                   z-add     §gelrp        W§GElrp
076200981009
076300980611      * ORA
076400980611     C                   TIME                    W0140
076500980619      * UDATE IN GGMMAAAA
076600980611     C                   MOVE      W0140         WDTGIO
076700980619      * UDATE IN AAAAMMGG
076800971016     C                   Z-ADD     WDTGIO        G02DAT
076900971016     C                   MOVEL     *BLANK        G02ERR
077000971016     C                   CALL      'XSRDA8'
077100971016     C                   PARM                    WLBDAT
077200980611     C                   MOVEL     G02INV        DATEU
077300980623     C     *iso          MOVEL     DATEU         Wdataoggi
077400980625
077500971020     C     KTAB          KLIST
077600050207     C                   KFLD                    TBLKUT
077700971020     C                   KFLD                    KCOD
077800971020     C                   KFLD                    KKEY
077900980625      *
078000990108     c     kbol          klist
078100990909     c                   kfld                    dctaas
078200990909     c                   kfld                    dctlnp
078300990909     c                   kfld                    dctnrs
078400990909     c                   kfld                    dctnsp
078500050523      *
078600050523     C     Ktas4         klist
078700050523     C                   kfld                    DCTaas
078800050523     C                   kfld                    DCTlnp
078900050523     C                   kfld                    DCTnrs
079000050523     C                   kfld                    DCTnsp
079100050523     C                   kfld                    TA4trc
079200050523     c                   eval      TA4trc = 'A'
079300050523      *
079400980625      *
079500971021     C     KDET          KLIST
079600990108     C                   KFLD                    DCTaae
079700990108     C                   KFLD                    DCTnev
079800980625      *
079900971021     C     KDCS          KLIST
080000990202     C                   KFLD                    kTPD
080100990202     C                   KFLD                    kNKT
080200990202     C                   KFLD                    kSTD
080300990202     C                   KFLD                    kDIM
080400990202     C                   KFLD                    kHIM
080500990202     C                   KFLD                    kNKS
080600990202     C                   KFLD                    kTRC
080700980625      *
080800980520     c     knumca        klist
080900980520     c                   kfld                    i00aac
081000980520     c                   kfld                    i00fil
081100980520     c                   kfld                    i00nca
081200981020      *
081300981020     c     KDKA          klist
081400981020     c                   kfld                    i00aac
081500981020     c                   kfld                    i00fil
081600981020     c                   kfld                    i00nca
081700981020     c                   kfld                    Wtrc
081800981224      *
081900951009     C                   ENDSR
082000050201      *---------------------------------------------------------------*
082100981014** MSG  L78                                                                  *
082200990203Immetere Data Stampa corretta.                                                 1
082300990126Il Numero Mandato è obbligatorio                                               2
082400990203Inserire la modalità di calcolo.                                               3
082500990126ATTENZIONE: manca la tabella Polizza avvertire l'ufficio EDP di Bologna !!     4
082600990203Inserire l'importo da liquidare.                                               5
082700990204** VLR  L78                                                          *
082800990203 a Kg merce perduta o danneggiata (legge 450/85).                              1  Legge 450
082900990126 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          2  Elevazione
083000990126 in base all'assicurazione stipulata sul valore merce.                         3  Valore in bolla
083100990126 a Kg merce perduta o danneggiata (C.M.R.).                                    4  CMR
083200990204 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo          5  Mandato a Kg
083300990204 a collo in base al Vostro mandato assicurativo                                6  Mandato a collo
083400990204 a spedizione in base al Vostro mandato assicurativo                           7  Mandato a sped.
083500060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)           8  Decreto legge 286
083600060316 a Kg merce perduta o danneggiata                                              9  rcv transitoria
083700050222** SLR  L56                                            *------------>*
083800990204 a Kg merce perduta o danneggiata (legge 450/85).
083900050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
084000990204 In base all'assicurazione stipulata sul valore merce.
084100990204 a Kg merce perduta o danneggiata (C.M.R.).
084200050222 a Kg merce perduta o danneggiata in base al Vs. mandato assicurativo
084300990204 a collo in base al Vostro mandato assicurativo
084400990204 a spedizione in base al Vostro mandato assicurativo
084500060316 a Kg merce perduta o danneggiata (Art 1696 c.c. mod. Dlgs 286/2005)
084600060316 a Kg merce perduta o danneggiata
084700120502** SLR2 L78                                            *------------>*
084800120502 per each kilo of missing or damaged goods (lgs decree 450/85)
084900120507 per each kilo of missing or damaged goods, according to the insurance writ
085000120507 according to the insurance writ, based to value of goods
085100120502 per each kilo of missing or damaged goods (C.M.R.)
085200120507 per each kilo of missing or damaged goods, according to the insurance writ
085300120502 per each parcel, accordig to the insurance writ
085400120507 according to the insurance writ, each shipment
085500120507 per each kilo of missing or damaged goods (art 1696 m.lgs d.286/2005)
085600120502 per each kilo of missing or damaged goods
