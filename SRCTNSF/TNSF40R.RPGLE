000100060504      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200060504      *===============================================================*
000300060504      *?TNSF40R * Ritassazione spedizioni fatturate                  ?*
000400060504      *===============================================================*
000500060504
000600060504     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060504
000800060504      *---------------------------------------------------------------*
000900060504
001000060801     fTitas40c  if   e           k disk    usropn
001100060801     fTita430c  if   e           k disk    usropn
001200060801     fTita730c  if   e           k disk    usropn
001300061010     fTntam04l  if   e           k disk
001400060504      *
001500060508     fTNSF40D   cf   e             workstn sfile(sf40s02:nrr)
001600060516      *
001700060801     fWFrft00f  uf a e             disk    usropn
001800060504
001900060504      *---------------------------------------------------------------*
002000060504
002100060504      *
002200060504      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002300060504      *
002400060504      * - Messaggi di errore
002500081013     d $Msg            s             78    dim(15) ctdata  perrcd(1)
002600060801     d cmdt            s             60    dim(05) ctdata perrcd(1)
002700060512      * P.O. gestibili dall'utente
002800060512     d fig             s              3    dim(250) inz(*Zeros)
002900060512      * P.O.
003000060512     d com             s              3    dim(250) inz(*Zeros)
003100060504      *
003200060504      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003300060504      *
003400060504     d KPJBA         e ds
003500060518      * - Parametri x il lancio del pgm batch
003600060518     d TNSF40DS      e ds                  inz
003700060504      * - Parametri x Controllo profilo utenti
003800060504     d TIBS34DS      e ds                  inz
003900060504      * - Ds di riferimento al file esterno AZUTE00F
004000060504     d AZUTEds       e ds                  extname(AZUTE00F)
004100060504      * - Ds per dati organigramma
004200060504     d DDatiUte      e ds
004300060512      * - Ds per dati autorizzazioni
004400060512     d dLat          e ds
004500060512     d dute01        e ds
004600060512     d Trul31ds      e ds
004700060512      * - Ds per dati in tabella
004800060512     d tibs02ds      e ds
004900060518      * - Ds per riferimento alfanumerico
005000060518     d Dta4a         e ds                  inz
005100060509      * - Ds per dati anagrafici
005200060509     d TIBS69DS      e ds
005300060509     d DS_cnaco      e ds                  extname(CNACO00F)
005400060509     d DS_cnind      e ds                  extname(CNIND00F)
005500060509     d DS_cnclp      e ds                  extname(CNCLP00F)
005600060509     d DS_fncls      e ds                  extname(FNCLS00F)
005700060518      ** DS Calcolo tassazione  - Varie
005800060518     d Dtasv         e ds
005900060518     d  sv                     1     20
006000060518     d                                     Dim(20)                              Sigle varie
006100060518     d  va                    21    140P 3
006200060518     d                                     Dim(20)                              Importi varie
006300060512      * - Ds per ricerca tariffe
006400060512     d parmta36        ds
006500060512     d  ta36ksc                6     12  0
006600060512     d  ta36ctr               13     15
006700060512     d  ta36flg               16     16
006800060512     d  ta36vpr               17     17
006900060512     d  ta36kcc               18     21  0
007000060512     d  ta36prg               37     39
007100060518      * - Ds per scrittura file di lavoro
007200060518     d                 ds
007300060518     d  RFTsv1f                1      1
007400060518     d  RFTsv2f                2      2
007500060518     d  RFTsv3f                3      3
007600060518     d  RFTsv4f                4      4
007700060518     d  RFTsv5f                5      5
007800060518     d  RFTsv6f                6      6
007900060518     d  RFTsv7f                7      7
008000060518     d  RFTsv8f                8      8
008100060518     d  RFTsv9f                9      9
008200060518     d  RFTs10f               10     10
008300060518     d  RFTs11f               11     11
008400060518     d  RFTs12f               12     12
008500060518     d  RFTs13f               13     13
008600060518     d  RFTs14f               14     14
008700060518     d  RFTs15f               15     15
008800060518     d  RFTs16f               16     16
008900060518     d  RFTs17f               17     17
009000060518     d  RFTs18f               18     18
009100060518     d  RFTs19f               19     19
009200060518     d  RFTs20f               20     20
009300060518     d  RFT_svf                1     20    Dim(20)                              Sigle varie
009400060512
009500060518      * - Ds per scrittura file di lavoro
009600060518     d                 ds
009700060518     d  RFTva1f                1     11  3
009800060518     d  RFTva2f               12     22  3
009900060518     d  RFTva3f               23     33  3
010000060518     d  RFTva4f               34     44  3
010100060518     d  RFTva5f               45     55  3
010200060518     d  RFTva6f               56     66  3
010300060518     d  RFTva7f               67     77  3
010400060518     d  RFTva8f               78     88  3
010500060518     d  RFTva9f               89     99  3
010600060518     d  RFTv10f              100    110  3
010700060518     d  RFTv11f              111    121  3
010800060518     d  RFTv12f              122    132  3
010900060518     d  RFTv13f              133    143  3
011000060518     d  RFTv14f              144    154  3
011100060518     d  RFTv15f              155    165  3
011200060518     d  RFTv16f              166    176  3
011300060518     d  RFTv17f              177    187  3
011400060518     d  RFTv18f              188    198  3
011500060518     d  RFTv19f              199    209  3
011600060518     d  RFTv20f              210    220  3
011700060518     d  RFT_vaf                1    220  3 Dim(20)                              Importo varie
011800060504      *
011900060504      * - Controllo/Inversione date
012000060504     d WLBdat          ds                  inz
012100060504     d   G08dat                       8  0 inz
012200060504     d   G08inv                       8  0 inz
012300060504     d   G08err                       1    inz('3')
012400060504     d   G08tgi                       5  0 inz
012500060504      *
012600060504      * - Variabili definite a programma
012700060504     d Wdate           s              8  0 inz
012800060516     d Wora            s              4  0 inz
012900060516     d W0140           s             14  0 inz
013000060505     d wdataeur        s               d   datfmt(*eur)
013100060505     d wdataiso        s               d   datfmt(*iso)
013200060505     d wdtlimite       s              8  0
013300060508     d tasdsp          s              8  0
013400060508     d nrr             s              4  0 inz
013500060510     d nrrmax          s              4  0
013600060509     d §write          s              1    inz(*off)
013700060509     d wtibs69r        s              1
013800060509     d wksc            s                   like(tasksc)
013900060515     d Kctr            s                   LIKE(Tamctr)
014000060512     d wabi            s                   like(UteAut)
014100060517     d §4trc           s                   like(ta4trc)
014200081013     d Ksc_ctr         s             10
014300081013     d WKsc_ctr        s             10
014400081013     d w_onectr        s              1
014500060518     d xx              s              4  0
014600060518     d k               s              4  0
014700060523     d kk              s              4  0
014800060515     d codut           s              1  0 Inz(1)
014900060515     d W_v1cdft        s                   like(v1cdft)
015000060515     d W_v1cdsa        s                   like(v1cdsa)
015100060515     d W_v1cdsd        s                   like(v1cdsd)
015200060512
015300060512     d paxdut          s             30
015400060512     d paxdmt          s                   LIKE(ACOrag)
015500060512     d paxccc          s                   LIKE(ACOkcc)
015600060512     d paxsta          s              1  0
015700060512     d paxflr          s             90
015800060512     d paxdit          s              3
015900060512     d paxnum          s              2  0
016000060512     d paxkcm          s             80
016100060512     d paxksm          s            140
016200060512     d paxkdm          s             60
016300060801
016400060801     d wlib            s             10
016500060801     d wlib1           s             10
016600060801     d comman          s             80
016700060801     d lenght          s             15  5 inz(80)
016800060801
016900060512
017000060504      *
017100060504      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
017200060504      *
017300060504      * - TITAS40C
017400060504     c     K03TAS40      klist
017500060515     c                   kfld                    v1cfiv
017600060515     c                   kfld                    v1cnft
017700060515     c                   kfld                    W_v1cdft
017800060515
017900060515     c     K04TAS40      klist
018000060515     c                   kfld                    v1cfiv
018100060515     c                   kfld                    v1cnft
018200060515     c                   kfld                    W_v1cdft
018300060515     c                   kfld                    v1cksc
018400060515
018500060517     c     K05TAS40      klist
018600060517     c                   kfld                    v1cfiv
018700060517     c                   kfld                    v1cnft
018800060517     c                   kfld                    W_v1cdft
018900060517     c                   kfld                    v2sksc
019000060517     c                   kfld                    v2sctr
019100060517
019200060517      * - TITA430C
019300060518     c     KTA4          Klist
019400060518     c                   Kfld                    TASaas
019500060518     c                   Kfld                    TASlnp
019600060518     c                   Kfld                    TASnrs
019700060518     c                   Kfld                    TASnsp
019800060518     c                   Kfld                    §4trc
019900060518      * - TITA730C
020000060518     c     KTA7          Klist
020100060518     c                   Kfld                    TASaas
020200060518     c                   Kfld                    TASlnp
020300060518     c                   Kfld                    TASnrs
020400060518     c                   Kfld                    TASnsp
020500060518     c                   Kfld                    TAStbl
020600061010      * - TNTAM04L
020700061010     c     k02tam04      klist
020800060515     c                   kfld                    v2cksc
020900060515     c                   kfld                    kctr
021000060504
021100060504      *---------------------------------------------------------------*
021200060504      *  RIEPILOGO INDICATORI                                         *
021300060504      *---------------------------------------------------------------*
021400060504      * 10    - Comodo                                                *
021500060504      * 28    - Emette il messaggio di errore a video                 *
021600060515      * 32    - Indicatore risuktato lettura subfile                  *
021700060504      * 41/45 - Errori del video                                      *
021800060504      * 90    - Generico di errore                                    *
021900060504      *---------------------------------------------------------------*
022000060504
022100060504     c     *Entry        plist
022200060504     c                   parm                    KPJBA
022300060504      *
022400060504      * Operazioni Iniziali
022500060504     c                   exsr      RoutInz
022600060504      *
022700060504      * Gestione Video
022800060504do  1c                   DO        *hival
022900060504      * 1° videata
023000060504     c                   exsr      GESD01
023100060515      * KC  vado a fine
023200060515     c                   if        *inkc
023300060515     c                   leave
023400060515     c                   endif
023500060504      * 2° videata
023600060504     c                   exsr      GESD02
023700060504      * KF Conferma vado a fine
023800060504     c                   if        *inkf
023900060504     c                   leave
024000060504     c                   endif
024100060504
024200060504e   1c                   ENDDO
024300060504      *
024400060504      * Fine
024500060509      *
024600060509     c                   If        Wtibs69r  <>  *blanks
024700060509     c                   eval      I69tla  = 'C'
024800060509     c                   call      'TIBS69R'
024900060509     c                   Parm                    Tibs69ds
025000060509     c                   Endif
025100060509      *
025200060512     c     fine          tag
025300060801     c                   If        Dutlpo <> 'S'
025400060801     c                   Clear                   comman
025500060801     c                   Movel(p)  cmdt(5)       comman
025600060801     c                   Call      'QCMDEXC'                            99
025700060801     c                   Parm                    comman
025800060801     c                   Parm                    lenght
025900060801     c                   endif
026000060801
026100060504     c                   movel     *on           *inLR
026200060504      *
026300060504      *---------------------------------------------------------------*
026400060504      * Operazioni Iniziali                                           *
026500060504      *---------------------------------------------------------------*
026600060504     c     RoutInz       BEGSR
026700060504      *
026800060504      * Reperisco dati job
026900060504     c                   exsr      DatiJob
027000060504      *
027100060504     c                   clear                   WLBdat
027200060504     c                   eval      G08dat   = *date
027300060504     c                   call      'XSRDA8'
027400060504     c                   parm                    WLBdat
027500060504     c                   eval      wDate    = G08inv
027600060505
027700060505     c     *eur          Movel     *date         wdataeur
027800060505
027900060505      * alla data odierna devo togliere 13 mesi
028000060505     c                   subdur    13:*m         wdataeur
028100060505     c                   Movel     wdataeur      wdataiso
028200060505     c     *iso          Movel     wdataiso      wdtlimite
028300060505
028400060801      * verifico se sono in filiale faccio le ovrdbf per i file di sede
028500060801     c                   If        dutlpo <> 'S'
028600060801      * librerie
028700060801
028800060801      * Se sono in ambiente buono - GAITRAGRU
028900060801     c                   If        knsif = 'FILTRA201 '
029000060801     c                   Eval      wlib = 'GAITRAGRU '
029100060801     c                   Eval      wlib1 = 'GAITRAAZM '
029200060801      *  Se sono in ambiente di prova - GAITRAGRPS
029300060801     c                   Else
029400060801     c                   Eval      wlib = 'GAITRAGRPS'
029500060801     c                   Eval      wlib1 = 'GAITRAAZM '
029600060801     c                   EndIf
029700060801      * TITAS40C
029800060801     c                   Clear                   comman
029900060801     c                   Movel(p)  cmdt(1)       comman
030000060801     c                   Eval      %Subst(comman:30:10) = wlib
030100060801     c                   Eval      comman =
030200060801     c                             %trim(comman) + '/TITAS40C)'
030300060801     c                   Call      'QCMDEXC'                            99
030400060801     c                   Parm                    comman
030500060801     c                   Parm                    lenght
030600060801      * TITA430C
030700060801     c                   Clear                   comman
030800060801     c                   Movel(p)  cmdt(2)       comman
030900060801     c                   Eval      %Subst(comman:30:10) = wlib
031000060801     c                   Eval      comman =
031100060801     c                             %trim(comman) + '/TITA430C)'
031200060801     c                   Call      'QCMDEXC'                            99
031300060801     c                   Parm                    comman
031400060801     c                   Parm                    lenght
031500060801      * TITA730C
031600060801     c                   Clear                   comman
031700060801     c                   Movel(p)  cmdt(3)       comman
031800060801     c                   Eval      %Subst(comman:30:10) = wlib
031900060801     c                   Eval      comman =
032000060801     c                             %trim(comman) + '/TITA730C)'
032100060801     c                   Call      'QCMDEXC'                            99
032200060801     c                   Parm                    comman
032300060801     c                   Parm                    lenght
032400060801      * WFRFT00F
032500060801     c                   Clear                   comman
032600060801     c                   Movel(p)  cmdt(4)       comman
032700060801     c                   Eval      %Subst(comman:30:10) = wlib1
032800060801     c                   Eval      comman =
032900060801     c                             %trim(comman) + '/WFRFT00F)'
033000060801     c                   Call      'QCMDEXC'                            99
033100060801     c                   Parm                    comman
033200060801     c                   Parm                    lenght
033300060801
033400060801     c                   endif
033500060504      *
033600060801     c                   open      titas40c
033700060801     c                   open      tita430c
033800060801     c                   open      tita730c
033900060801     c                   open      wfrft00f
034000060801
034100060504     c                   ENDSR
034200060504      *
034300060504      *---------------------------------------------------------------*
034400060504      * Reperimento Dati del job (Utente/Operativi)                   *
034500060504      *---------------------------------------------------------------*
034600060504     c     DatiJob       BEGSR
034700060504      *
034800060504     c     *dtaara       define    §azute        azuteds
034900060504     c     *dtaara       define    §datiute      ddatiute
035000060504      *
035100060504     c                   in(E)     *dtaara
035200060504     c                   IF        %ERROR or RSUT = *blanks
035300060504     c                   clear                   Tibs34Ds
035400060504     c                   call      'TIBS34R'
035500060504     c                   parm                    Tibs34Ds
035600060504     c                   in        *dtaara
035700060512     c                   Endif
035800060512
035900060512     c                   Clear                   wabi
036000060512     c                   Clear                   dLat
036100060512
036200060512      * Verifica errori e autorità profilo
036300060512s   1c                   Select
036400060512      * se ho errori nei dati utente esco dal pgm
036500060512w   1c                   When      DutErr = 'E'
036600060512     c                   GoTo      Fine
036700060512      * se non c'è l'abilitazione
036800060512      * --> se 1° livello, abilitazioni al terminal
036900060512      *     se 2° livello, abilitazioni al punto operativo
037000060512      *     se sede è impossibile ma se capita mando a fine il pgm
037100060512w   1c                   When      UteAut = *Blanks
037200060512if  2c                   If        DutLpo = '1'
037300060512     c                   Eval      wabi   = 'TP'
037400060512e   2c                   EndIf
037500060512if  2c                   If        DutLpo = '2'
037600060512     c                   Eval      wabi   = 'PO'
037700060512e   2c                   EndIf
037800060512if  2c                   If        DutLpo = 'S'
037900060512     c                   GoTo      Fine
038000060512e   2c                   EndIf
038100060512      * carica le abilitazioni del profilo
038200060512x   1c                   Other
038300060512     c                   Movel     UteFaf        Dute01
038400060512     c                   If        §UteGtc <> *Blanks
038500060512     c                   Eval      wabi = §UteGtc
038600060512     c                   Else
038700060512     c                   Eval      wabi = UteAut
038800060512     c                   EndIf
038900060512e   1c                   EndSl
039000060512
039100060512      * controllo se ok l'abilitazione dell'utente
039200060512     c                   Clear                   Tibs02ds
039300060512     c                   Eval      T02Mod = 'C'
039400060512     c                   Eval      T02Sif = knsif
039500060512     c                   Eval      T02Cod = 'LAT'
039600060512     c                   Movel(p)  wabi          T02Ke1
039700060512     c                   Call      'TIBS02R'
039800060512     c                   Parm                    kpjba
039900060512     c                   Parm                    Tibs02ds
040000060512if  1c                   If        T02Err = *Blanks
040100060512     c                   Eval      dLat = T02Uni
040200060512e   1c                   EndIf
040300060512      * errore o non abilitato
040400060512if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
040500060512     c                   GoTo      Fine
040600060512e   1c                   EndIf
040700060512
040800060512      * Se l'utente ha autorizzazione 'AZ' è di sede
040900060512      *
041000060512      * Reperimento dei P.O. gestibili dall'utente
041100060512     c                   clear                   TRUL31DS
041200060512     c                   eval      I31abi = wabi
041300060512     c                   eval      I31cdi = DUTdis
041400060512     c                   eval      I31car = DUTare
041500060512     c                   eval      I31cpo = DUTpou
041600060512     c                   call      'TRUL31R'
041700060512     c                   parm                    KPJBA
041800060512     c                   parm                    TRUL31DS
041900060512if  2c                   if        O31pog > *zeros
042000060512     c                   movea     O31pog        fig
042100060512x   2c                   else
042200060512     c                   GoTo      Fine
042300060512e   2c                   endif
042400060512      *
042500060504      *
042600060504     c                   ENDSR
042700060504      *
042800060504      *---------------------------------------------------------------*
042900060504      * Gestione videata D01                                          *
043000060504      *---------------------------------------------------------------*
043100060504     c     GesD01        BEGSR
043200060504      *
043300060504      * Scrivo la testata e la riga tasti funzionali abilitati
043400060504if  1c                   if        NOT *in90
043500060515     c                   write     sf40T01
043600060515     c                   write     sf40Z01
043700060504e   1c                   endif
043800060504
043900060504     c                   do        *hival
044000060504      * Emetto la videata
044100060515     c                   exfmt     sf40D01
044200060504     c                   setoff                                       28  90
044300060515     c                   clear                   v1cmsg
044400060504      *
044500060504      * F3=Fine
044600060504w   1c                   If        *inKC
044700060504     c                   leave
044800060504     c                   endif
044900060504      *
045000060504      * Controllo dati immessi a video
045100060504     c                   exsr      CtrD01
045200060504      * se ci sono errori riemetto la videata
045300060504     c                   if        *in90
045400060504     c                   iter
045500060504
045600060504     c                   else
045700060504      * carico il subfile della videata successiva
045800060504     c                   exsr      CarSbf
045900060504
046000060523      * se non ho caricato neppure un record nel subfile emetto un msg di errore
046100060523     c                   If        nrr =  0
046200060523     c                   seton                                        289041
046300060523     c                   eval      v1cmsg   = $Msg(14)
046400060523     c                   iter
046500060523
046600060523     c                   else
046700060523
046800060504     c                   leave
046900060523     c                   endif
047000060504     c                   endif
047100060504
047200060504     c                   enddo
047300060504      *
047400060504     c                   ENDSR
047500060504      *
047600060504      *---------------------------------------------------------------*
047700060504      * Controllo dati immessi in videata D01                         *
047800060504      *---------------------------------------------------------------*
047900060504     c     CtrD01        BEGSR
048000060504      *
048100060504     c                   movea     *zeros        *in(41)
048200060504      * Fattura da ritassare
048300060505
048400060505      * Libro IVA
048500060505     c                   If        V1Cfiv   = *zeros
048600060504     c                   seton                                        432890
048700060515     c                   eval      v1cmsg   = $Msg(1)
048800060505     c                   leavesr
048900060505     c                   endif
049000060505      * Numero fattuta
049100060505     c                   If        V1Cnft   = *zeros
049200060504     c                   seton                                        442890
049300060515     c                   eval      v1cmsg   = $Msg(2)
049400060504     c                   leavesr
049500060505     c                   endif
049600060505      * Data   fattuta
049700060515     c                   If        V1Cdft   = *zeros
049800060504     c                   seton                                        452890
049900060515     c                   eval      v1cmsg   = $Msg(3)
050000060504     c                   leavesr
050100060505     c                   endif
050200060505      * verifico se corretta
050300060505     c                   clear                   W_v1cdft
050400060515     c                   move      V1Cdft        g08dat
050500060515     c                   eval      g08err = *blanks
050600060505     c                   Call      'XSRDA8'
050700060505     c                   parm                    Wlbdat
050800060515     c                   If        g08err = '1'
050900060505     c                   seton                                        289045
051000060515     c                   eval      v1cmsg   = $Msg(3)
051100060505     c                   leavesr
051200060505     c                   else
051300060515     c                   movel     g08dat        V1Cdft
051400060515     c                   move      g08inv        W_v1cdft
051500060505     c                   endif
051600060505
051700060505      * verifico che la data non sia antecedente 13 mesi
051800060505     c                   If        W_v1cdft < wdtlimite
051900060505     c                   seton                                        289045
052000060515     c                   eval      v1cmsg   = $Msg(6)
052100060505     c                   leavesr
052200060505     c                   endif
052300060515      * in caso di distinta è obbligatorio inserire il codice del cliente
052400060515     c                   If        v1cnft =  999999 and v1cksc = *zeros
052500060515     c                   seton                                        289046
052600060515     c                   eval      v1cmsg   = $Msg(12)
052700060515     c                   leavesr
052800060515     c                   endif
052900060505
053000060505      * verifico se esiste la fattura in Titas
053100060515     c                   If        v1cnft <> 999999
053200060505     c     k03tas40      chain     titas40c
053300060515     c                   else
053400060515     c     k04tas40      chain     titas40c
053500060515     c                   endif
053600060515
053700060505     c                   If        not %found(titas40c)
053800060505     c                   seton                                        432890
053900060515     c                   eval      v1cmsg   = $Msg(7)
054000060505     c                   leavesr
054100060505     c                   endif
054200060524
054300060524      * Data spedizioni da ritassare
054400060524
054500060524      * Controllo data spedizione  "dal"
054600060524
054700060524     c                   clear                   W_v1cdsd
054800060524     c                   If        V1Cdsd  <> 0
054900060524     c                   move      V1Cdsd        g08dat
055000060524     c                   eval      g08err = *blanks
055100060524     c                   Call      'XSRDA8'
055200060524     c                   parm                    Wlbdat
055300060524     c                   If        g08err = '1'
055400060524     c                   seton                                        289041
055500060524     c                   eval      v1cmsg   = $Msg(4)
055600060524     c                   leavesr
055700060524     c                   else
055800060524     c                   movel     g08dat        V1Cdsd
055900060524     c                   move      g08inv        W_v1cdsd
056000060524     c                   endif
056100060524     c                   endif
056200060524
056300060524      * Controllo data spedizione  "dal"
056400060524
056500060524     c                   clear                   W_v1cdsa
056600060524     c                   If        V1Cdsa  <> 0
056700060524     c                   move      V1Cdsa        g08dat
056800060524     c                   eval      g08err = *blanks
056900060524     c                   Call      'XSRDA8'
057000060524     c                   parm                    Wlbdat
057100060524     c                   If        g08err = '1'
057200060524     c                   seton                                        289042
057300060524     c                   eval      v1cmsg   = $Msg(4)
057400060524     c                   leavesr
057500060524     c                   else
057600060524     c                   movel     g08dat        V1Cdsa
057700060524     c                   move      g08inv        W_v1cdsa
057800060524     c                   endif
057900060524     c                   endif
058000060524
058100060524      * data  "dal" maggiore di data  "al"
058200060524     c                   if        W_v1cdsa  < W_v1cdsd
058300060524     c                   seton                                        289041
058400060524     c                   eval      v1cmsg   = $Msg(6)
058500060524     c                   leavesr
058600060524     c                   endif
058700060524      * Se data sped. "al" è vuota, la imposto come la data sped. "dal"
058800060524     c                   if        W_v1cdsa  = 0  AND  W_v1cdsd  <> 0
058900060524     c                   z-add     W_v1cdsd      W_v1cdsa
059000060524     c                   endif
059100060524
059200060504      *
059300060504     c                   ENDSR
059400060505      *
059500060505      *---------------------------------------------------------------*
059600060505      * Caricamento subfile con i codici clienti e codici tariffe    *
059700060505      *---------------------------------------------------------------*
059800060505     c     Carsbf        BEGSR
059900060508
060000060508      * pulisco  subfile
060100060508     c                   seton                                        22
060200060508     c                   setoff                                       2021
060300060508     c                   write     sf40c02
060400060508     c                   setoff                                       22
060500060726      * pulisco  record subfile
060600060726     c                   clear                   sf40s02
060700060509
060800060509     c                   z-add     *zeros        nrr
060900060508      * lettura spedizioni legate alla fattura indicata
061000060515     c                   If        v1cnft <> 999999
061100060508     c     k03tas40      setll     titas40c
061200060515     c                   else
061300060515     c     k04tas40      setll     titas40c
061400060515     c                   endif
061500060508
061600060508     c                   do        *hival
061700060508
061800060515     c                   If        v1cnft <> 999999
061900060508     c     k03tas40      reade     titas40c
062000060515     c                   else
062100060524     c     k04tas40      reade     titas40c
062200060515     c                   endif
062300060508      * fine lettura
062400060508     c                   If        %eof(titas40c)
062500060508     c                   leave
062600060508     c                   endif
062700060524      * verifico data spedizione se richieste a video
062800060508     c                   clear                   tasdsp
062900060508     c                   movel     tasaas        tasdsp
063000060508     c                   move      tasmgs        tasdsp
063100060508      * data spedizione  fuori dal periodo selezionato leggo un'altra bolla
063200060524     c                   If        W_v1cdsd <> 0 and W_v1cdsa <> 0  and
063300060524     c                             (tasdsp < W_v1cdsd or tasdsp > W_v1cdsa)
063400060508     c                   iter
063500060508     c                   endif
063600060509      * controllo se rottura di codice cliente
063700060509     c                   If        tasksc <> v2sksc
063800060509     c                   eval      §write = *on
063900060509     c                   eval      v2sksc = tasksc
064000060509     c                   eval      v2sctr = tasctr
064100060509     c                   endif
064200060509      * controllo se rottura codice tariffa
064300060509     c                   If        tasctr <> v2sctr
064400060509     c                   eval      §write = *on
064500060509     c                   eval      v2sctr = tasctr
064600060509     c                   endif
064700060509      * scrittura del records di subfile
064800060509     c                   If        §write = *on
064900060509      * decodifico
065000060509     c                   eval      Wksc = v2sksc
065100060509     c                   exsr      SR_anagra
065200060509     c                   movel     acorag        v2srag
065300060509
065400060509     c                   add       1             nrr
065500060509     c                   write     sf40s02
065600060509     c                   eval      §write = *off
065700060509     c                   endif
065800060509
065900060509     c                   enddo
066000060517
066100060517     c                   z-add     nrr           nrrmax
066200081006
066300081006     c                   eval      v1csgl = 'N'
066400060509      *
066500060505     c                   ENDSR
066600060509      *
066700060509      *---------------------------------------------------------------*
066800060509      * Gestione videata D02                                          *
066900060509      *---------------------------------------------------------------*
067000060509     c     GesD02        BEGSR
067100060509      *
067200060509      * verifco se è stato scritto almeno un record
067300060509
067400060509     c                   if        nrr > 0
067500060509     c                   seton                                        2021
067600060509     c                   endif
067700060509
067800060509     c                   z-add     1             v2cnrr
067900060509      * emetto il subfile
068000060509
068100060509
068200060509     c                   do        *hival
068300060516     c                   write     sf40t01
068400060516     c                   write     sf40z02
068500060509     c                   exfmt     sf40c02
068600060515
068700060515     c                   setoff                                       289050
068800060515     c                   setoff                                       515253
068900081013     c                   setoff                                       25
069000060510      * cmd 12
069100060509     c                   If        *inkl
069200060509     c                   leave
069300060509     c                   endif
069400060510      * controllo la videata
069500060510     c                   exsr      Ctrs02
069600060515      * se ci sono degli errori riemetto il subfile
069700060515     c                   If        *in28 and *in90
069800111116     c                   z-add     nrr           v2cnrr
069900060515     c                   iter
070000060515     c                   endif
070100081013      * verifico se richiesto il riepilogo ma è stata richiesta la ritassazione con + tariffe
070200081013     c                   If        v1csgl = 'S' and W_onectr = 'N'
070300081013     c                   seton                                        289025
070400081015     c                   eval      v2cmsg   = $Msg(15)
070500081013     c                   iter
070600081013     c                   endif
070700060515      * conferma F6
070800060515     c                   If        *inkf
070900060515     c                   exsr      SR_confe
071000060515     c                   leave
071100060515     c                   endif
071200060515
071300060515     c                   enddo
071400060515
071500060515     c                   Endsr
071600060510      *---------------------------------------------------------------*
071700060510      *  Controllo subfile
071800060510      *---------------------------------------------------------------*
071900060510     c     CTRs02        Begsr
072000060510
072100060510      * lettura
072200081013     c                   clear                   Ksc_ctr
072300081013     c                   clear                   Wksc_ctr
072400060510     c                   z-add     1             nrr
072500060510     c                   dow       nrr <= nrrmax
072600060515     c     nrr           chain     sf40s02                            32
072700060510      * fine file
072800060515     c                   if        *in32
072900060510     c                   leave
073000060510     c                   endif
073100060510      * se cliente non selezionato ma inseriti gli altri dati emetto errore
073200060512     c                   If        v2sopz = ' ' and (v2cksc <> *zeros or
073300060512     c                             v2cctr <> *blanks or v2cdta <> *zeros or
073400060512     c                             v2crag <> *blanks )
073500060510     c                   seton                                        502890
073600060510     c                   eval      V2cmsg   = $Msg(8)
073700060510     c                   update    sf40s02
073800060510     c                   leave
073900060510     c                   endif
074000060510      * se cliente selezionato ma non inseriti gli altri dati emetto errore
074100060512     c                   If        v2sopz <> ' ' and (v2cksc = *zeros and
074200060512     c                             v2cctr = *blanks and v2cdta = *zeros and
074300060512     c                             v2crag = *blanks )
074400060510     c                   seton                                        512890
074500060510     c                   eval      V2cmsg   = $Msg(9)
074600060510     c                   update    sf40s02
074700060510     c                   leave
074800060510     c                   endif
074900060512
075000060510      * verifico il codice cliente
075100060512
075200060512     c                   If        v2cksc =  *zeros and v2crag <> *blanks
075300060512      * richiamo il programma di interrogazione anagraficva clienti
075400060512     c                   Z-add     1             xx
075500060512     c                   Clear                   com
075600060512     c     *zeros        Lookup    fig(xx)                                30
075700060512     c                   If        *In30 and xx <= 30
075800060512     c                   Do        30            XX
075900060512     c                   Move      Fig(XX)       Com(XX)
076000060512     c                   enddo
076100060512     c                   EndIf
076200060512     c                   movea     com           paxflr
076300060512
076400060512     c                   movel     rsut          paxdut
076500060512     c                   z-add     9             paxsta
076600060512     c                   z-add     dutkci        paxccc
076700060512     c                   movel(P)  v2crag        paxdmt
076800060512     c                   z-add     1             paxnum
076900060512
077000060512     c                   call      'XALFA3BR'
077100060512     c                   parm                    paxdut
077200060512     c                   parm                    codut
077300060512     c                   parm                    paxdmt
077400060512     c                   parm                    paxccc
077500060512     c                   parm                    paxsta
077600060512     c                   parm                    paxflr
077700060512     c                   parm      *blanks       paxdit
077800060512     c                   parm                    paxnum
077900060512     c                   parm                    paxkcm
078000060512     c                   parm                    paxksm
078100060512     c                   parm                    paxkdm
078200060512
078300060512      * Non trovato o premuto F12
078400060512    2c                   if        paxsta > -1
078500060512     c                   movel     paxksm        v2cksc
078600060516     c                   movel     paxdmt        v2crag
078700060516     c                   update    sf40s02
078800060516     c                   leave
078900060512    2c                   endif
079000060512
079100060512     c                   endif
079200060512      * controllo il codice cliente
079300060516     c                   if        v2cksc <> *zeros
079400060512     c                   z-add     v2cksc        wksc
079500060512
079600060512     c                   Exsr      SR_anagra
079700060512
079800060512     c                   if        *in30 or Acoflg = '*'
079900060512     c                   seton                                        512890
080000060512     c                   eval      V2cmsg   = $Msg(10)
080100060512     c                   update    sf40s02
080200060512     c                   leave
080300060515     c                   endif
080400060516      * se ragione sociale a video diversa da anagrafica emetto il subfile
080500060516     c                   If        v2crag <> %subst(acorag:1:20)
080600060515     c                   movel     acorag        v2crag
080700060516     c                   endif
080800060515
080900060515     c                   endif
081000060512
081100060512      * verifico la tariffa
081200060515
081300060515      * se codice cliente a zero e tariffa non valorizzata richiamo il pgm di ricerca
081400060515     c                   If        v2cctr = *blanks and v2cksc > 0
081500060515
081600060512     c                   Eval      ta36ksc = v2cksc
081700060512     c                   Clear                   ta36ctr
081800060515     c                   Eval      ta36flg = '1'
081900060512     c                   Eval      ta36vpr = '2'
082000060512     c                   Eval      ta36kcc = dutkci
082100060512     c                   Eval      kpjbu = parmta36
082200060512     c                   Call      'TNTA36R'
082300060512     c                   Parm                    kpjba
082400060512     c                   Eval      parmta36 = kpjbu
082500060726     c                   If        ta36ctr <> *blanks
082600060512     c                   Eval      v2cctr = ta36ctr
082700060515     c                   update    sf40s02
082800060726     c                   leavesr
082900060726     c                   endif
083000060512     c                   endif
083100060515      * controllo la tariffa se esiste
083200060515
083300060515     c                   If        V2Cctr  = *blanks  and v2sopz = 'X'
083400060515     c                   seton                                        289052
083500060515     c                   eval      V2cmsg   = $Msg(13)
083600060516     c                   update    sf40s02
083700060726     c                   leave
083800060515     c                   endif
083900060515
084000060515     c                   If        v2cctr <> *blanks
084100060515     c                   move      v2cctr        kctr
084200061010     c     K02tam04      chain     Tntam04l
084300061010     c                   if        not %found(tntam04l)
084400060515     c                   seton                                        522890
084500060515     c                   eval      V2cmsg   = $Msg(11)
084600060515     c                   update    sf40s02
084700060726     c                   leave
084800060515     c                   endif
084900060515     c                   endif
085000060515
085100060515      * verifico la data tassazione
085200060515
085300060515
085400060515     c                   If        V2Cdta  <> 0
085500060515     c                   move      V2cdta        g08dat
085600060515     c                   eval      g08err = *blanks
085700060515     c                   Call      'XSRDA8'
085800060515     c                   parm                    Wlbdat
085900060515     c                   If        g08err = '1'
086000060515     c                   seton                                        289053
086100060515     c                   eval      V2cmsg   = $Msg(4)
086200060726     c                   update    sf40s02
086300060726     c                   leave
086400060515     c                   else
086500060515     c                   movel     g08dat        V2Cdta
086600060515     c                   move      g08inv        V2hdta
086700090603     c                   endif
086800060515     c                   endif
086900081013      *
087000081013     c                   If        v2sopz = 'X'
087100081013     c                   movel(p)  v2cksc        wksc_ctr
087200081013     c                   move      v2cctr        wksc_ctr
087300081013     c                   if        ksc_ctr = *blank
087400081013     c                   eval      ksc_ctr = Wksc_ctr
087500081013     c                   endif
087600081013
087700081013      * verifico se si tratta della stessa tariffa
087800081013     c                   If        ksc_ctr <> wksc_ctr
087900081013     c                   eval      w_onectr = 'N'
088000081013     c                   else
088100081013     c                   eval      w_onectr = 'Y'
088200081013     c                   endif
088300081013     c                   endif
088400081013
088500060515
088600060523     c                   update    sf40s02
088700060515     c                   add       1             nrr
088800060515
088900060515     c                   enddo
089000060510      *
089100060510     c                   Endsr
089200060510
089300060515      *---------------------------------------------------------------*
089400060515      *  Routine di preparazione archivio da ritassare
089500060515      *---------------------------------------------------------------*
089600060515     c     SR_confe      Begsr
089700060516
089800060516      * scrittura del file da ritassare
089900060516
090000060516     c                   time                    w0140
090100060516     c                   movel     w0140         wora
090200060516
090300060516     c                   clear                   Wfrft000
090400060516      * imposto i campi uguali per tutti
090500060516     c                   eval      RFTnoj = knmeb                               nome del job
090600060516     c                   eval      RFTpru = knmus
090700060516     c                   eval      RFTdta = wdate
090800060516     c                   eval      RFTora = wora
090900060516     c                   eval      RFTfiv = v1cfiv
091000060516     c                   eval      RFTnft = v1cnft
091100060516     c                   eval      RFTdft = W_v1cdft
091200060516      *
091300060516      * lettura del subfile per i clienti selezionati e titas per le bolle selezionate
091400060517     c                   z-add     1             nrr
091500060517     c                   dow       nrr <= nrrmax
091600060517     c     nrr           chain     sf40s02                            32
091700060517      * fine file
091800060517     c                   if        *in32
091900060517     c                   leave
092000060517     c                   endif
092100060517      * se cliente selezionato
092200060517     c                   If        V2sopz = 'X'
092300060517      * leggo le spedizioni da ritassare
092400060517     c                   exsr      SR_wrtrft
092500060517     c                   endif
092600060517
092700060518     c                   add       1             nrr
092800060517     c                   enddo
092900060518
093000060518      * lancio programma tassazione
093100060518
093200060518     c                   eval      I40noj = knmeb                               nome del job
093300060518     c                   eval      I40pru = knmus
093400060518     c                   eval      I40dta = wdate
093500060518     c                   eval      I40ora = wora
093600081006     c                   eval      I40sgl = v1csgl
093700120921     c                   eval      I40sva = V1Csva
093800060518
093900060518     c                   movel     tnsf40ds      kpjbu
094000060524     c                   movel     'SF41'        Kcoaz
094100070411     c******             call      'BCH10'
094200070411     c                   call      'TNSF41R'
094300060518     c                   parm                    kpjba
094400060516
094500060515     c                   Endsr
094600060517
094700060517      *---------------------------------------------------------------*
094800060517      *  Scrittura File di lavoro per ritassazione
094900060517      *---------------------------------------------------------------*
095000060517     c     SR_WRTRFT     Begsr
095100060517      *
095200060517     c     k05tas40      setll     titas40c
095300060517     c                   do        *hival
095400060517
095500060518     c                   clear                   dtasv
095600060518     c                   clear                   Rft_svf
095700060518     c                   clear                   RFT_vaf
095800060518
095900060518     c     k05tas40      reade     titas40c
096000060517      * fine lettura
096100060517     c                   If        %eof(titas40c)
096200060517     c                   leave
096300060517     c                   endif
096400060517      * verifico data spedizione
096500060517     c                   clear                   tasdsp
096600060517     c                   movel     tasaas        tasdsp
096700060517     c                   move      tasmgs        tasdsp
096800060525      * data spedizione  fuori dal periodo selezionato leggo un'altra bolla
096900060525     c                   If        W_v1cdsd <> 0 and W_v1cdsa <> 0  and
097000060525     c                             (tasdsp < W_v1cdsd or tasdsp > W_v1cdsa)
097100060517     c                   iter
097200060517     c                   endif
097300060517      * scrivo il record
097400060517     c                   eval      RFTcbo = tasksc
097500060517     c                   eval      RFTtbo = tasctr
097600060517     c                   eval      RFTaas = tasaas
097700060517     c                   eval      RFTmgs = tasmgs
097800060517     c                   eval      RFTlnp = taslnp
097900060517     c                   eval      RFTnrs = tasnrs
098000060517     c                   eval      RFTnsp = tasnsp
098100060518     c                   eval      RFTtbl = tastbl
098200060517     c                   eval      RFTrmn = tasrmn
098300060517      * cerco il riferimento mittente alfanumerico
098400060517     c                   eval      §4trc = 'A'
098500060517     c     kta4          chain     tita430c
098600060517     c                   If        %found(tita430c)
098700060517     c                   movel     ta4not        dta4a
098800060517     c                   else
098900060517     c                   clear                   dta4a
099000060517     c                   endif
099100060517
099200060517     c                   If        §ta4arma <> *blanks
099300060517     c                   eval      RFTrma = §ta4arma
099400060517     c                   endif
099500060517      * tassazione
099600060517     c                   eval      RFTdivf = tasdiv
099700060517     c                   eval      RFTporf = taspor
099800060518      * carico eventuali varie presenti in TITA7
099900060518     c                   Exsr      Carta7
100000060523      * verifico se esiste la varia "D"
100100060523     c                   z-add     1             kk
100200060523     c     'D'           lookup    sv(kk)                                 05
100300060523     c                   if        *in05
100400060523     c                   clear                   sv(kk)
100500060523     c                   sub       va(kk)        tasimv
100600060523     c                   clear                   va(kk)
100700060523     c                   endif
100800060523
100900060518     c                   movea     sv            rft_svf
101000060518     c                   movea     va            rft_vaf
101100060518      *
101200060518      * valorizzo i dati della nuova tassazione
101300060518     c                   eval      RFTkscr = V2cksc
101400060518     c                   movel     v2cctr        RFTctrr
101500060518      * se valorizzata data tassazione a video la metto altrimenti data spedizione
101600060518     c                   if        v2hdta > 0
101700060518     c                   eval      RFTdspr = v2hdta
101800060518     c                   else
101900060518     c                   eval      RFTdspr = Tasdsp
102000060518     c                   endif
102100060518
102200060518     c                   eval      RFTimvf = tasimv
102300060517
102400060518     c                   write     WFRFT000
102500060518
102600060517     c                   enddo
102700060518
102800060518     c                   Endsr
102900060515
103000060518      *---------------------------------------------------------------*
103100060518      ** Carica importi varie da tita7000
103200060518      *---------------------------------------------------------------*
103300060518     c     Carta7        Begsr
103400060518      *
103500060518      * leggo tita7 solo se ho la 3° varia di titas maggiore di zero
103600060518     c                   If        tassv3 <> *blanks
103700060518      * mi posiziono su tita7
103800060518     c     Kta7          setll     tita730c
103900060518     c                   if        %equal(tita730c)
104000060518     c                   z-add     3             k
104100060518      **
104200060518     c                   do        *hival
104300060518     c     Kta7          reade     tita730c
104400060518      * fine file
104500060518     c                   if        %eof(tita730c)
104600060518     c                   leave
104700060518     c                   endif
104800060518      * carico le varie
104900060518     c                   add       1             k
105000060518     c                   movel     ta7svn        sv(k)
105100060518     c                   z-add     ta7van        va(k)
105200060518     c                   enddo
105300060518
105400060518     c                   endif
105500060518
105600060518     c                   endif
105700060518
105800060518     c                   Endsr
105900060509      *---------------------------------------------------------------*
106000060509      *  AGGANCIO ANAGRAFICO CLIENTE
106100060509      *---------------------------------------------------------------*
106200060509     c     SR_anagra     Begsr
106300060509      *
106400060509     c                   clear                   Tibs69ds
106500060509     c                   z-add     Wksc          I69kac
106600060509      *
106700060509     c                   call      'TIBS69R'
106800060509     c                   parm                    tibs69DS
106900060509     c                   parm                    DS_cnaco
107000060509     c                   parm                    DS_cnind
107100060509     c                   parm                    DS_cnclp
107200060509     c                   parm                    DS_fncls
107300060509      *
107400060509IF  2c                   eval      *IN30 = (O69ERR =  '1')
107500060509     c                   eval      wtibs69r = '1'
107600060509      *
107700060509     c                   ENDSR
107800060509      *
107900060504      *
108000060504      *---------------------------------------------------------------*
108100060504
108200060504**   $MSG >------------------------------------------------------------------*
108300060504Immettere il libro IVA                                                         1
108400060504Immettere il numero fattura                                                    2
108500060504Immettere la data fattura                                                      3
108600060505Data errata                                                                    4
108700060505Data spedizione Dal non può essere minore di data spedizione Al                5
108800060505Attenzione Fattura troppo vecchia impossibile recuperare i dati                6
108900060515Documento inesistente in archivio                                              7
109000060510Errore non è stato selezionato il cliente da ritassare                         8
109100060510Errore impostare dati per la ritassazione                                      9
109200060512Cliente errato                                                                 10
109300060515Tariffa Cliente inesistente                                                    11
109400060515In caso di ritassazione distinta è obbligatorio il codice cliente              12
109500060515Immettere codice tariffa                                                       13
109600060523Non esistono spedizioni da tassare nel periodo indicato                        14
109700081013Riepilogo per scaglioni solo se richiesta 1 sola tariffa cliente               15
109800060801** cmdt
109900060801OVRDBF FILE(TITAS40C) TOFILE(
110000060801OVRDBF FILE(TITA430C) TOFILE(
110100060801OVRDBF FILE(TITA730C) TOFILE(
110200060801OVRDBF FILE(WFRFT00F) TOFILE(
110300060801DLTOVR FILE(*ALL)
