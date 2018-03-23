000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200000406      ***************************************************************************
000300151218      *  AGGIORNA EVENTI FISSI PER IMPORT DPD    - nuovo programma              *
000400971216      ***************************************************************************
000500060608     FtIDP302L  UF   E           K DISK
000600151222     FtIDP301L  uF   E           K DISK    rename(tidp3000:tidp3001)
000700151218     f                                     prefix(S)
000800151222     FFiapd01L  IF   E           K DISK
000900151222     FFNBLP01L  IF   E           K DISK
001000151218     FFNBLt01L  IF   E           K DISK
001100991216     FFNARB01L  IF   E           K DISK
001200151218     FFNbrv07l  IF   E           K DISK
001300991213     FTNTBE01L  IF   E           K DISK
001400020919     Fazorg01l  IF   E           K DISK
001500151222     Ffnevb04l  IF   E           K DISK
001700940321      *----------------------------------------------------*
001800000103     D KEVD            S             15    DIM(30)  inz                         CODICE NAZIONE
001900991213     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
002000991213      *-------------------
002100991213     D XX              S              3  0 inz
002200991213     D w0080           S              8  0 inz
002300000103     D w015A           S             15
002400991213     D wdata           S              8  0
002500991213     D wora            S              4  0
002600151221     D Sdata           S              8  0
002700151221     D Sora            S              4  0
002800160112     D Rdata           S              8  0
002900160112     D Rora            S              4  0
003000151223     D Sora6           S              6  0
003100991213     D wfil            S              4  0
003200151218     D wdfv            S              8  0
003300151222     D wscan           S              2
003400151222     D wtrov           S              1
003500151221     d Dataiso         s               d   datfmt(*iso)
003600151223     d timeiso         s               t   timfmt(*iso)
003700020919     D kpjbus          S                   LIKE(kpjbu)
003800151222     D ktip            S                   LIKE(apdtip)
003900151222     D kcev            S                   LIKE(evbcev)
004000971212      *-------------------
004100991130     D KPJBA         E DS
004200151218     D fnlv53ds      E DS
004300991213     D DEVD          E DS
004400151222     D DEVb01        E DS
004500991216     D CNCR80        E DS
004600991216     D UT§DSE0F      E DS
004700020919     D og143         E DS
004800151221     D xgiolavds     E DS
004900040907     D*
005000040907     D WLBDAT          DS
005100040907     D  G02DAT                 1      8  0
005200040907     D  G02INV                 9     16  0
005300040907     D  G02ERR                17     17
005400040907     D  G02TGI                18     22  0
005500040907     D  data_Oggi      s                   like(G02INV) INZ(0)
005600991216      *----------------------------------------------------*
005700130910      * legge quelle con DATA = 0
005800060608     C     w0080         setll     tidp3000
005900060608     C     w0080         reade     tidp3000
006000991213      *
006100060608     C                   DOW       NOT %eof(tIDP302L)
006200991210      *
006300991213      * Controllo che si tratti di un evento fisso da tradurre
006400991213     c                   eval      xx = 1
006500000103     c                   eval      W015A = DP3evd
006600000103     c     W015A         lookup    KEVD(xx)                               31
006700991213      *
006800991214     C                   IF        *IN31 = *on
006900080604      ** se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
007000080604     c                             or Dp3CEV = 'ORM'
007100991213      *
007200991215     C                   If        BLPaas <> DP3aas or BLPlnp <> Dp3lnp  or
007300991215     C                             BLPnrs <> DP3nrs  or  BLPnsp <> DP3nsp
007400130910      *
007500151221     c                   clear                   blplna
007600151221     c                   clear                   blptfa
007700130910     c     KSPE          CHAIN     FNBLP01L
007800151221      *
007900151221      *   chain di ARB per info di arrivo merce
008000130912     c                   if        %Found(FNBLP01L)
008100130912      *
008200130910     c     KSPE          CHAIN     FNARB01L
008300130910      *  se c'è ARB sostituisce i 3 campi DTI/HTI/DAM del BLP
008400130910     c                   if        %Found(FNARB01L)
008500130910     c                   eval      BLPDTI = ARBDTI
008600130910     c                   eval      BLPHTI = ARBHTI
008700130910     c                   eval      BLPDAM = ARBDAM
008800140728     c*m                 eval      BLPDCM = ARBDCM
008900131106     c                   eval      BLPCCA = ARBCCA
009000140728     c*m                 eval      BLPHMC = ARBHMC
009100130910     c                   End
009200130912      *
009300130912     c                   Endif
009400130910      *
009500991216     c                   Endif
009600991213      *
009700991216     C                   clear                   wora
009800991216     C                   clear                   wfil
009900991216     C                   clear                   wdata
010000991216
010100991210     c                   SELECT
010200991216
010300130910      * se non è riuscito ad agganciare la bolla in PARTENZA
010400991213     c                   WHEN      NOT %found(FNBLP01L)
010500151218     c
010600151218     c* escludo tutte le bolle con consegna anomala 5 e 7
010700151218     c                   WHEN      blpcca='7' or blpcca ='5'
010800991216
010900151218      * Entrata all'Hub   SCAN-10
011000000103     C                   WHEN      DP3evd = '10 '
011100151218     c                   exsr      SCAN10
011200151218     c*
011300151218      * Data arrivo al P.O. di consegna  SCAN-02
011400151218     C                   WHEN      DP3evd = '02 '
011500151218     c                   exsr      SCAN02
011600130912      **
011700151222      * Data consegna reale (Delivery) SCAN-13
011800151222      *   da non dare nemmeno per RESO 2 e 6
011900151222     c                   WHEN      DP3evd = '13 '   and
012000151222     c                             BLPdcm > *zeros  and  BLPcca <> '2'
012100130912     c                              and BLPcca <> '6'
012200130912     c                              and blpdcm <= data_oggi
012300151223     c                   eval      wscan='13'
012400151222      *
012500151222      * Devo distinguere se la distinta è fittizia opure NO
012600160114      *  lo faccio solo se non ha consegna anomala
012700160114     c                   if        blpcca=' '
012800151222     c     kapd          chain     fiapd01l
012900151222     c                   if        %found(fiapd01l) and apdpdd='S'
013000151222     c                   exsr      DistFitt
013100151222     c                   endif
013200160114     c                   endif
013300151222
013400151222     c                   if        wdata=0
013500031125     c                   eval      wdata = blpdcm
013600031125     c                   If        blphmc = *zeros
013700031125     c                   eval      wora  = 1200
013800031125     c                   Else
013900031125     c                   eval      wora  = blphmc
014000031125     c                   Endif
014100151222     c                   Endif
014200031125
014300130910      * Data arrivo al transito   SCAN-01
014400061030      *  allo stesso modo degli altri SCAN non deve inviare l'informazione
014500061030      *  in presenza di un'anomalia 7 (non fatta xchè non è mai arrivato il collo)
014600151222     C**                 WHEN      DP3evd = '01 '  and  BLPdet > *zeros
014700151222     C**                                           and  BLPflp > *zeros
014800151222     c**                            and blpdet <= data_oggi
014900151222     c**                            and  BLPcca <> '7'
015000130910
015100151222     c**                 eval      wdata = blpdet
015200151222     c**                 eval      wora  = 0300
015300151222     c**                 exsr      elabora
015400151221     C                   ENDSL
015500130910
015600991216      *
015700991216      * Se ho impostato data e ora scrivo su file eventi
015800991216     c                   If        Wdata > *zeros  and  Wora > *zeros
015900160118     c                             and wdata>20160111
016000991216     c                   exsr      elabora
016100151221
016200160105     c* Riempo eventuale SCAN precedente
016300160105     c                   if        wscan='13'
016400160105     c                   eval      wscan='02'
016500160105     c                   exsr      Riempi
016600160105     c                   endif
016700160105
016800160105     c                   if        wscan='02'
016900151222     c                   eval      wscan='10'
017000151222     c                   exsr      Riempi
017100160105     c                   endif
017400160114     c*
017800160114     c                   Endif
017900130912      *
018000991213     c                   ENDIF
018100991210      *
018200060608     C     w0080         reade     tidp3000
018300991210     c                   ENDDO
018400971215      *
018500020918     c     fine          tag
018600151218     C                   MOVEL     'C'           D53TLA
018700151218     C                   CALL      'FNLV53R'
018800151218     C                   PARM                    fnlv53ds
018900151218     C*
019000991129     C                   eval      *INLR = *ON
019100991213      *----------------------------------------------------*
019200991214      *   AGGIORNO FILE EVENTI DPD                         *
019300991213      *----------------------------------------------------*
019400991213     C     ELABORA       BEGSR
019500991213      *
019600991213     c                   movel     evd(xx)       Devd
019700991213      *
019800991213     c                   SELECT
019900991214     c                   WHEN      §EVDipo = 'T'
020000991213     c                   z-add     blpflp        wfil
020100991214     c                   WHEN      §EVDipo = 'A'
020200991213     c                   z-add     blplna        wfil
020300991213     c                   OTHER
020400991213     c                   z-add     blplnp        wfil
020500991213     C                   ENDSL
020600991213      *
020700991213     c                   eval      DP3fle = wfil
020800991213     c                   eval      DP3dev = wdata
020900991213     c                   eval      DP3hev = wora
021000991213     c                   clear                   DP3ftr
021100991213      *
021200060608     c                   update    tidp3000
021300991213      *
021400991213     C                   ENDSR
021500930126      *----------------------------------------------------*
021600930126      *   DEFINIZIONE CHIAVI                               *
021700930126      *----------------------------------------------------*
021800940321     C     *INZSR        BEGSR
021900940321      *
022000991129     c     *ENTRY        PLIST
022100991130     c                   PARM                    KPJBA
022200991129      *
022300991129     C     KSPE          KLIST
022400991210     C                   KFLD                    dp3AAS
022500991210     C                   KFLD                    dp3LNP
022600991210     C                   KFLD                    dp3NRS
022700991210     C                   KFLD                    dp3NSP
022800151222     c     KEVB          KLIST
022900151222     C                   KFLD                    dp3AAS
023000151222     C                   KFLD                    dp3LNP
023100151222     C                   KFLD                    dp3NRS
023200151222     C                   KFLD                    dp3NSP
023300151222     C                   KFLD                    KCEV
023400151218     c     kbrv          klist
023500151218     c                   kfld                    bltfls
023600151218     c                   kfld                    bltlna
023700151218     c                   kfld                    bltnrs
023800151218     c                   kfld                    bltnsc
023900151222     c     kapd          klist
024000151222     c                   kfld                    ktip
024100151222     c                   kfld                    arbpdc
024200151222     c                   eval      ktip='A'
024300151222     c                   eval      kCEV='MIC'
024400991216      * Dati società
024500991216     C                   Z-ADD     1             CODUT
024600991216     C                   CALL      'X§PARUT'
024700991216     C                   PARM                    UT§DSE0F
024800991216     C                   MOVEL     REC80         CNCR80
024900020919     C                   MOVEL     kpjbu         KPJBUs
025000040907      *
025100040907      *  imposta UDATE di elaborazione
025200040907     C                   move      *date         G02DAT
025300040907     C                   MOVE      *ZEROS        G02INV
025400040907     C                   MOVE      *BLANKS       G02ERR
025500040907     C                   CALL      'XSRDA8'
025600040907     C                   PARM                    WLBDAT
025700040907     C                   Z-ADD     G02INV        data_oggi
025800991213      *
025900991213      * Carico Tabella Eventi DPD
026000991213     c     'EVD'         chain     tntbe000                           31
026100991213      *
026200991213     c                   DOW       *in31 = *off
026300991213      * Se il S.I. è indicato deve essere uguale al mio
026400991213     c                   IF        TBEatb = *BLANKS  and
026500991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
026600991213     c                   movel     TBEuni        Devd
026700991213      * Carico solo eventi fissi da tradurre in import
026800991213      * la chiave di ricerca è formata dai primi 2 char dell'evento DPD
026900991213      * più il nostro cod evento
027000991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
027100991213     c                   add       1             xx
027200000103     c                   movel     TBEke1        KEVD(XX)
027300991213     c                   movel     TBEuni        EVD(XX)
027400991213     c                   ENDIF
027500991213     c                   ENDIF
027600991213      *
027700991213     c     'EVD'         reade     tntbe000                               31
027800991213     c                   ENDDO
027900991213      *
028000020918     C     endinz        ENDSR
028100941230      *-------------------------------------------------------------------------
028200151218     c     SCAN10        BEGSR
028300151221     c                   eval      wscan='10'
028400151218     c* Leggo dettaglio colli
028500151221     c                   exsr      cercaspu
028600151221     c
028700151221     c                   if        wtrov='S'
028800151221     c                   eval      wdata=Sdata
028900151221     c                   eval      wora =Sora
029000151221     c                   endif
029100151221
029200151218     c* prima di aggiornare lo scan, verifico se già pieni gli scan successivi,
029300151218     c*  che la data e ora siano <
029400151218     c* ?? non sono certa che mi serva questo controllo: aspetto
029500151218     c**                 clear                   sdp3imm
029600151218     c**   kspe          setll     tidp301l
029700151218     c**   kspe          reade     tidp301l
029800151218     c**                 dow       not %eof(tidp301l)
029900151218     c**                 if        sdp3dev>0
030000151218     c**                 movel     sdp3dev       sdp3imm          14 0
030100151218     c**                 move      sdp3hev       sdp3imm
030200151218     c**
030300151218     c**------------------------------------------------------------------------
030400151218     c** calcolo SCAN 10 a modo vecchio
030500151218     c**                 Select
030600151218     c**                 When      blpdse > *zeros and BLPcca <> '5'
030700151218     c*+                            and BLPcca <> '7'
030800151218     c**                            and blpdse <= data_oggi
030900151218     c**                 eval      wdata = blpdse
031000151218     c**                 When      BLPdcm > *zeros and BLPcca <> '5'
031100151218     c**                            and BLPcca <> '7'
031200151218     c**                            and blpdcm <= data_oggi
031300151218     c**                 eval      wdata = blpduc
031400151218     c**                 Endsl
031500151218      *
031600151218     c**                 If        wdata > *zeros
031700151218     c**                 eval      wora  = 0900
031800151218     c**                 Endif
031900151218     c
032000151218     c                   EndSR
032100151221      *-------------------------------------------------------------------------
032200151221     c     SCAN02        BEGSR
032300151221     c                   eval      wscan='02'
032400151221     c
032500160107    1c                   if        blpdti>0 and blpdti<=data_oggi
032600151221     c                   eval      wdata = blpdti
032700151221     c                   eval      wora  = blphti
032800151221     c
032900151221     c* se ora non presente, devo prenderla dalla spunta
033000160107    2c                   if        blphti=0
033100151221     c                   exsr      Cercaspu
033200151221     c                   eval      wdata=Sdata
033300151221     c                   eval      wora =Sora
033400160107    2c                   endif
033500160107    1c                   endif
033600151221     c*
033700151222     c* se data e ora ancora vuoti e la bolla è stata resa devo impostarla lo stesso
033800160107     c* Perchè lo scam 13 - consegna non viene dato per le spedizioni di reso
033900160107    1c                   if        wdata=0 and (blpcca='2' or blpcca='6')
034000160107     c                             and blpdcm>0
034100151222     c                   eval      wdata=blpdcm
034200160107    2c                   if        0700<blphmc
034300160107     c                   eval      wora =0700
034400160107     c                   else
034500160107     c                   eval      timeiso=%time(blphmc*100)
034600160107     c                   subdur    1:*h          timeiso
034700160107     c                   eval      sora6=%dec(timeiso)
034800160107     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
034900160107    2c                   endif
035000160107    1c                   endif
035100151222     c
035200151221     c
035300151221     c**                 Select
035400151221     c**                 When      blpdti > *zeros
035500151221     c**                            and blpdti <= data_oggi
035600151221     c**                            and  BLPcca <> '7'
035700151221     c**                 eval      wdata = blpdti
035800151221     c**                 eval      wora  = blphti
035900151221     c**                 When      blpdam > *zeros
036000151221     c**                            and blpdam <= data_oggi
036100151221     c**                            and  BLPcca <> '7'
036200151221     c**                 eval      wdata = blpdam
036300151221     c**                 When      blpdcm > *zeros  and  BLPcca <> '5'
036400151221     c**                            and BLPcca <> '7'
036500151221     c**                            and blpdcm <= data_oggi
036600151221     c**                 eval      wdata = blpdcm
036700151221     c**                 Endsl
036800151221      *
036900151221     c**                 If        wdata > *zeros  and  Wora = *zeros
037000151221     c**                 eval      wora  = 0700
037100151221     c**                 Endif
037200151221
037300151221     c                   ENDSR
037400151221      *-------------------------------------------------------------------------
037500151221     c     CercaSPU      BEGSR
037600151221     C                   CLEAR                   Wimm
037700151221     C                   CLEAR                   WDFV
037800151222     C                   CLEAR                   Wtrov
037900151221     C                   CLEAR                   Sdata
038000151221     C                   CLEAR                   Sora
038100151221     c                   if        blpncl=1
038200151221     c                   eval      bltfls=blpfls
038300151221     c                   eval      bltlna=blplna
038400151221     c                   eval      bltnrs=blpnrs
038500151221     c                   eval      bltnsc=blpncd
038600151221     c                   if        wscan='10'
038700151221     c                   exsr      leggispuP
038800151221     c                   else
038900151221     c                   exsr      leggispuA
039000151221     c                   endif
039100151221     c
039200151221     c                   else
039300151221     c     kspe          setll     fnblt01l
039400151221     c     kspe          reade     fnblt01l
039500151221     c                   dow       not %eof(fnblt01l)
039600151221     c                   if        wscan='10'
039700151221     c                   exsr      leggispuP
039800151221     c                   else
039900151221     c                   exsr      leggispuA
040000151221     c                   endif
040100151221     c
040200151221     c     kspe          reade     fnblt01l
040300151221     c                   enddo
040400151221     c                   endif
040500151221     c
040600151221     c* Trovata spunta reale
040700151221     c                   if        wtrov='S' and wimm>0
040800151221     c                   movel     wimm          sdata
040900151221     c                   move      wimm          whhmmss           6 0
041000151221     c                   eval      Sora=whhmmss/100
041100151221     c                   endif
041200151221     c
041300151221     c* e l'ora è maggiore di oggi --> clear
041400151221     c                   if        sdata>data_oggi
041500151221     c                   clear                   D53DFV
041600151221     c                   clear                   Sdata
041700151221     c                   clear                   Sora
041800151221     c                   clear                   wtrov
041900151221     c                   endif
042000151221     c                   ENDSR
042100151218      *-------------------------------------------------------------------------
042200151221     c     LeggispuP     BEGSR
042300151218     C*
042400151218     c* Cerco la data e ora spunta reale di categoria 5 o 1
042500151218     c     kbrv          setll     fnbrv07l
042600151218     c     kbrv          reade     fnbrv07l
042700151221    1c                   dow       not %eof(fnbrv07l)
042800151218     c
042900151221    2c                   if        brvnpg=5 or brvnpg=1 and (brvdcs>0  or
043000151221     c                             brvnps=00)
043100160113   2ac**                 if        (brvatr=' ' or brvdcs>20160101)
043200160113     c**                           and brvfgs<>bltlna
043300160107     c
043400151218     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
043500151218     c                   clear                   fnlv53ds
043600151218     C                   z-add     brvnFV        D53NFV
043700151218     C                   MOVEL     brvNPG        D53NPG
043800151218     C                   MOVEL     brvFGS        D53FGS
043900151218     C                   CALL      'FNLV53R'
044000151218     C                   PARM                    fnlv53ds
044100160107     C* spunta non errata e non di defluenza --> ok
044200151221    3C     D53ERR        IFeq      ' '
044300160107     c     d53def        andeq     ' '
044400151218     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
044500151222    4c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
044600151222     c                             or  brvdcs=0
044700151218     c                   movel     brvdfs        brvimm           14 0
044800151218     c                   move      brvhms        brvimm
044900151218     c                   else
045000151218     c                   movel     brvdcs        brvimm           14 0
045100151218     c                   move      brvhcs        brvimm
045200151221    4c                   endif
045300151218     c
045400151221    4c                   select
045500151221     c                   when      wdfv=0
045600151221     C                   MOVEL     brvimm        Wimm             14 0
045700151221     C                   MOVEL     d53DFV        WDFV
045800151221     c                   eval      wtrov='S'
045900151221     c* Se minore  memorizzo
046000151221     c                   when      d53dfv<wdfv
046100151218     C                   MOVEL     brvimm        Wimm             14 0
046200151218     C                   MOVEL     d53DFV        WDFV
046300151218     c                   eval      wtrov='S'
046400151218     c
046500151221     c* Se uguale tengo la data ora immissione + bassa
046600151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
046700151221     C                   MOVEL     brvimm        Wimm             14 0
046800151218     C                   MOVEL     d53DFV        WDFV
046900151218     c                   eval      wtrov='S'
047000151221    4c                   endsl
047100151218     c
047200151221    3c                   endif
047300160113    2c***                endif
047400151228    2c                   endif
047500151218     c
047600151222RM*  C     kbrv          reade     FNBRV07l                               33
047700151218    1c                   enddo
047800151218     c
047900151218     c                   endsr
048000151221      *-------------------------------------------------------------------------
048100151221     c     LeggispuA     BEGSR
048200151221     C*
048300151221     c* Cerco la data e ora spunta reale di categoria arrivi
048400151221     c     kbrv          setll     fnbrv07l
048500151221     c     kbrv          reade     fnbrv07l
048600151221    1c                   dow       not %eof(fnbrv07l)
048700151221     c
048800151221    2c                   if        brvnpg<>5 and brvnpg<>1 and (brvdcs>0
048900151221     c                             or brvnps=00)
049000151221    3c                   if        blptfa=0  or
049100151221     c                             brvfgs=blptfa or brvfgs=blplna or
049200151221     c                             brvpes=blplna
049300151221     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
049400151221     c                   clear                   fnlv53ds
049500151221     C                   z-add     brvnFV        D53NFV
049600151221     C                   MOVEL     brvNPG        D53NPG
049700151221     C                   MOVEL     brvFGS        D53FGS
049800151221     C                   CALL      'FNLV53R'
049900151221     C                   PARM                    fnlv53ds
050000151221     C* ERRORE
050100151221    4C     D53ERR        IFeq      ' '
050200151221     c     d53spg        andne     'P'
050300151221     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
050400151222    5c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
050500151222     c                             or  brvdcs=0
050600151221     c                   movel     brvdfs        brvimm           14 0
050700151221     c                   move      brvhms        brvimm
050800151221     c                   else
050900151221     c                   movel     brvdcs        brvimm           14 0
051000151221     c                   move      brvhcs        brvimm
051100151221    5c                   endif
051200151221     c
051300151221    5c                   select
051400151221     c                   when      wdfv=0
051500151221     C                   MOVEL     brvimm        Wimm             14 0
051600151221     C                   MOVEL     d53DFV        WDFV
051700151221     c                   eval      wtrov='S'
051800151221     c* Se minore  memorizzo
051900151221     c                   when      d53dfv<wdfv
052000151221     C                   MOVEL     brvimm        Wimm             14 0
052100151221     C                   MOVEL     d53DFV        WDFV
052200151221     c                   eval      wtrov='S'
052300151221     c
052400151221     c* Se uguale tengo la data ora immissione + bassa
052500151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
052600151221     C                   MOVEL     brvimm        Wimm             14 0
052700151221     C                   MOVEL     d53DFV        WDFV
052800151221     c                   eval      wtrov='S'
052900151221    5c                   endsl
053000151221     c
053100151221    4c                   endif
053200151221    3c                   endif
053300151221    2c                   endif
053400151221     c
053500151222     C     kbrv          reade     FNBRV07l                               33
053600151221    1c                   enddo
053700151221     c
053800151221     c                   endsr
053900151221      *-------------------------------------------------------------------------
054000151222     c     Riempi        BEGSR
054100151221     c                   exsr      LeggiSCAN
054200151222     c
054300160112     c                   if        Rdata>0
054400151222     c                   SELECT
054500151222     c                   WHEN      §EVDipo = 'T'
054600151222     c                   z-add     blpflp        wfil
054700151222     c                   WHEN      §EVDipo = 'A'
054800151222     c                   z-add     blplna        wfil
054900151222     c                   OTHER
055000151222     c                   z-add     blplnp        wfil
055100151222     C                   ENDSL
055200151222      *
055300151222     c                   eval      sDP3fle = wfil
055400160112     c                   eval      sDP3dev = Rdata
055500160112     c                   eval      sDP3hev = Rora
055600151222     c                   clear                   sDP3ftr
055700151222      *
055800151222     c                   update    tidp3001
055900151222     c                   endif
056000160105
056100160105     c* imposto i campi W = ai campi S se per caso devo impostare
056200160105     c*  anche lo SCAN 10
056300160112     c                   eval      wdata=Rdata
056400160112     c                   eval      wora =Rora
056500151222     c                   ENDSR
056600151221
056700151222      *-------------------------------------------------------------------------
056800151221     c     LeggiSCAN     BEGSR
056900160112     c                   clear                   Rdata
057000160112     c                   clear                   Rora
057100151222     c* Verifico lo SCAN  --> se vuoto lo imposto
057200151221     c     kspe          setll     tidp301l
057300151221     c     kspe          reade     tidp301l
057400151221    1c                   dow       not %eof(tidp301l)
057500151221    2c                   if        sdp3evd=wscan  and wscan='10'
057600151221
057700151221    3c                   if        sdp3dev=0
057800160112     c* Prima ricerco nelle spunte, se non trovo uso data part ultimo collo
057900160112     c                   exsr      cercaspu
058000160113
058100160112     c                   select
058200160112     c                   when      wtrov='S'
058300160112     c                   eval      Rdata=Sdata
058400160112     c                   eval      Rora =Sora
058500160112     c
058600160112    4c                   when      blpduc<wdata
058700160112     c                   eval      Rdata=blpduc
058800160112     c                   eval      Rora=1400
058900160112   x4c                   other
059000151221     c* data arrivo -1 lavorativo
059100151221     c                   clear                   xgiolavds
059200151221     c                   eval      IXGLDATA=wdata
059300151221     c                   eval      IXGLfil=blplnp
059400151221     c                   eval      IXGLpa ='P'
059500151221     c                   eval      IXGLsub ='S'
059600151221     c                   eval      IXGLgg  =1
059700151221     c                   eval      IXGLlav ='S'
059800151222     c                   call      'XGIOLAV'
059900151221     c                   parm                    xgiolavds
060000151221    5c                   if        oxglerr=' '
060100160112     c                   eval      Rdata=oxgldata
060200160112     c                   eval      Rora=1400
060300151221    5c                   endif
060400151221     c
060500160112    4c                   endsl
060600151221    3c                   endif
060700151221     c
060800151221     c                   leave
060900151221    2c                   endif
061000151222     c
061100151222    2c                   if        sdp3evd=wscan  and wscan='02'
061200151222
061300151222    3c                   if        sdp3dev=0
061400160112     c                   eval      Rdata=wdata
061500151223
061600151223    4c                   if        0700<wora
061700160112     c                   eval      Rora =0700
061800151223     c                   else
061900151223     c                   eval      timeiso=%time(wora*100)
062000151223     c                   subdur    1:*h          timeiso
062100160114     c                   eval      Sora6=%dec(timeiso)
062200160114     c                   eval      Rora =%int(%subst(%editc(Sora6:'X'):1:4))
062300151223    4c                   endif
062400151223    3c                   endif
062500151222     c
062600151222     c                   leave
062700151222    2c                   endif
062800151221     c
062900151221     c     kspe          reade     tidp301l
063000151221    1c                   enddo
063100151221     c
063200151222     c                   eval      W015A = sDP3evd
063300151221     c     W015A         lookup    KEVD(xx)                               31
063400151222     c                   movel     evd(xx)       Devd
063500151221     c                   endsr
063600151222      *-------------------------------------------------------------------------
063700151222     c     DistFitt      BEGSR
063800151222     c* Se distinta dittiza, la consegna merce la dò con orario inserimento ultimo MIC
063900151222     c     kevb          setgt     fnevb04l
064000151222     c     kevb          readpe    fnevb04l
064100151222     c                   if        not%eof(fnevb04l) and evbdev=blpddc
064200151222     c                   eval      devb01=evbnot
064300151222     c                   if        §NOTNDC=%editc(blpndc:'X')
064400160111     c
064500160111     c                   if        evbdev=evbdtv
064600160111     c
064700151222     c                   eval      wdata=EVBDTV
064800151223     c                   eval      wora =%int(%subst(%editc(EVBorv:'X'):1:4))
064900151222     c                   endif
065000151222     c                   endif
065100160111     c
065200160111     c                   endif
065300151222     c
065400151222     c                   endsr
