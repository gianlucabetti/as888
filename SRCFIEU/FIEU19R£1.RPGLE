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
001600940321      *----------------------------------------------------*
001700000103     D KEVD            S             15    DIM(30)  inz                         CODICE NAZIONE
001800991213     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
001900991213      *-------------------
002000991213     D XX              S              3  0 inz
002100991213     D w0080           S              8  0 inz
002200000103     D w015A           S             15
002300991213     D wdata           S              8  0
002400991213     D wora            S              4  0
002500151221     D Sdata           S              8  0
002600151221     D Sora            S              4  0
002700151223     D Sora6           S              6  0
002800991213     D wfil            S              4  0
002900151218     D wdfv            S              8  0
003000151222     D wscan           S              2
003100151222     D wtrov           S              1
003200151221     d Dataiso         s               d   datfmt(*iso)
003300151223     d timeiso         s               t   timfmt(*iso)
003400020919     D kpjbus          S                   LIKE(kpjbu)
003500151222     D ktip            S                   LIKE(apdtip)
003600151222     D kcev            S                   LIKE(evbcev)
003700971212      *-------------------
003800991130     D KPJBA         E DS
003900151218     D fnlv53ds      E DS
004000991213     D DEVD          E DS
004100151222     D DEVb01        E DS
004200991216     D CNCR80        E DS
004300991216     D UT§DSE0F      E DS
004400020919     D og143         E DS
004500151221     D xgiolavds     E DS
004600040907     D*
004700040907     D WLBDAT          DS
004800040907     D  G02DAT                 1      8  0
004900040907     D  G02INV                 9     16  0
005000040907     D  G02ERR                17     17
005100040907     D  G02TGI                18     22  0
005200040907     D  data_Oggi      s                   like(G02INV) INZ(0)
005300991216      *----------------------------------------------------*
005400130910      * legge quelle con DATA = 0
005500060608     C     w0080         setll     tidp3000
005600060608     C     w0080         reade     tidp3000
005700991213      *
005800060608     C                   DOW       NOT %eof(tIDP302L)
005900991210      *
006000991213      * Controllo che si tratti di un evento fisso da tradurre
006100991213     c                   eval      xx = 1
006200000103     c                   eval      W015A = DP3evd
006300000103     c     W015A         lookup    KEVD(xx)                               31
006400991213      *
006500991214     C                   IF        *IN31 = *on
006600080604      ** se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
006700080604     c                             or Dp3CEV = 'ORM'
006800991213      *
006900991215     C                   If        BLPaas <> DP3aas or BLPlnp <> Dp3lnp  or
007000991215     C                             BLPnrs <> DP3nrs  or  BLPnsp <> DP3nsp
007100130910      *
007200151221     c                   clear                   blplna
007300151221     c                   clear                   blptfa
007400130910     c     KSPE          CHAIN     FNBLP01L
007500151221      *
007600151221      *   chain di ARB per info di arrivo merce
007700130912     c                   if        %Found(FNBLP01L)
007800130912      *
007900130910     c     KSPE          CHAIN     FNARB01L
008000130910      *  se c'è ARB sostituisce i 3 campi DTI/HTI/DAM del BLP
008100130910     c                   if        %Found(FNARB01L)
008200130910     c                   eval      BLPDTI = ARBDTI
008300130910     c                   eval      BLPHTI = ARBHTI
008400130910     c                   eval      BLPDAM = ARBDAM
008500140728     c*m                 eval      BLPDCM = ARBDCM
008600131106     c                   eval      BLPCCA = ARBCCA
008700140728     c*m                 eval      BLPHMC = ARBHMC
008800130910     c                   End
008900130912      *
009000130912     c                   Endif
009100130910      *
009200991216     c                   Endif
009300991213      *
009400991216     C                   clear                   wora
009500991216     C                   clear                   wfil
009600991216     C                   clear                   wdata
009700991216
009800991210     c                   SELECT
009900991216
010000130910      * se non è riuscito ad agganciare la bolla in PARTENZA
010100991213     c                   WHEN      NOT %found(FNBLP01L)
010200151218     c
010300151218     c* escludo tutte le bolle con consegna anomala 5 e 7
010400151218     c                   WHEN      blpcca='7' or blpcca ='5'
010500991216
010600151218      * Entrata all'Hub   SCAN-10
010700000103     C                   WHEN      DP3evd = '10 '
010800151218     c                   exsr      SCAN10
010900151218     c*
011000151218      * Data arrivo al P.O. di consegna  SCAN-02
011100151218     C                   WHEN      DP3evd = '02 '
011200151218     c                   exsr      SCAN02
011300130912      **
011400151222      * Data consegna reale (Delivery) SCAN-13
011500151222      *   da non dare nemmeno per RESO 2 e 6
011600151222     c                   WHEN      DP3evd = '13 '   and
011700151222     c                             BLPdcm > *zeros  and  BLPcca <> '2'
011800130912     c                              and BLPcca <> '6'
011900130912     c                              and blpdcm <= data_oggi
012000151223     c                   eval      wscan='13'
012100151222      *
012200151222      * Devo distinguere se la distinta è fittizia opure NO
012300151222     c     kapd          chain     fiapd01l
012400151222     c                   if        %found(fiapd01l) and apdpdd='S'
012500151222     c                   exsr      DistFitt
012600151222     c                   endif
012700151222
012800151222     c                   if        wdata=0
012900031125     c                   eval      wdata = blpdcm
013000031125     c                   If        blphmc = *zeros
013100031125     c                   eval      wora  = 1200
013200031125     c                   Else
013300031125     c                   eval      wora  = blphmc
013400031125     c                   Endif
013500151222     c                   Endif
013600031125
013700130910      * Data arrivo al transito   SCAN-01
013800061030      *  allo stesso modo degli altri SCAN non deve inviare l'informazione
013900061030      *  in presenza di un'anomalia 7 (non fatta xchè non è mai arrivato il collo)
014000151222     C**                 WHEN      DP3evd = '01 '  and  BLPdet > *zeros
014100151222     C**                                           and  BLPflp > *zeros
014200151222     c**                            and blpdet <= data_oggi
014300151222     c**                            and  BLPcca <> '7'
014400130910
014500151222     c**                 eval      wdata = blpdet
014600151222     c**                 eval      wora  = 0300
014700151222     c**                 exsr      elabora
014800151221     C                   ENDSL
014900130910
015000991216      *
015100991216      * Se ho impostato data e ora scrivo su file eventi
015200991216     c                   If        Wdata > *zeros  and  Wora > *zeros
015300991216     c                   exsr      elabora
015400151221
015500160105     c* Riempo eventuale SCAN precedente
015600160105     c                   if        wscan='13'
015700160105     c                   eval      wscan='02'
015800160105     c                   exsr      Riempi
015900160105     c                   endif
016000160105
016100160105     c                   if        wscan='02'
016200151222     c                   eval      wscan='10'
016300151222     c                   exsr      Riempi
016400160105     c                   endif
016500151221     c
016600991216     c                   Endif
016700130912      *
016800991213     c                   ENDIF
016900991210      *
017000060608     C     w0080         reade     tidp3000
017100991210     c                   ENDDO
017200971215      *
017300020918     c     fine          tag
017400151218     C                   MOVEL     'C'           D53TLA
017500151218     C                   CALL      'FNLV53R'
017600151218     C                   PARM                    fnlv53ds
017700151218     C*
017800991129     C                   eval      *INLR = *ON
017900991213      *----------------------------------------------------*
018000991214      *   AGGIORNO FILE EVENTI DPD                         *
018100991213      *----------------------------------------------------*
018200991213     C     ELABORA       BEGSR
018300991213      *
018400991213     c                   movel     evd(xx)       Devd
018500991213      *
018600991213     c                   SELECT
018700991214     c                   WHEN      §EVDipo = 'T'
018800991213     c                   z-add     blpflp        wfil
018900991214     c                   WHEN      §EVDipo = 'A'
019000991213     c                   z-add     blplna        wfil
019100991213     c                   OTHER
019200991213     c                   z-add     blplnp        wfil
019300991213     C                   ENDSL
019400991213      *
019500991213     c                   eval      DP3fle = wfil
019600991213     c                   eval      DP3dev = wdata
019700991213     c                   eval      DP3hev = wora
019800991213     c                   clear                   DP3ftr
019900991213      *
020000060608     c                   update    tidp3000
020100991213      *
020200991213     C                   ENDSR
020300930126      *----------------------------------------------------*
020400930126      *   DEFINIZIONE CHIAVI                               *
020500930126      *----------------------------------------------------*
020600940321     C     *INZSR        BEGSR
020700940321      *
020800991129     c     *ENTRY        PLIST
020900991130     c                   PARM                    KPJBA
021000991129      *
021100991129     C     KSPE          KLIST
021200991210     C                   KFLD                    dp3AAS
021300991210     C                   KFLD                    dp3LNP
021400991210     C                   KFLD                    dp3NRS
021500991210     C                   KFLD                    dp3NSP
021600151222     c     KEVB          KLIST
021700151222     C                   KFLD                    dp3AAS
021800151222     C                   KFLD                    dp3LNP
021900151222     C                   KFLD                    dp3NRS
022000151222     C                   KFLD                    dp3NSP
022100151222     C                   KFLD                    KCEV
022200151218     c     kbrv          klist
022300151218     c                   kfld                    bltfls
022400151218     c                   kfld                    bltlna
022500151218     c                   kfld                    bltnrs
022600151218     c                   kfld                    bltnsc
022700151222     c     kapd          klist
022800151222     c                   kfld                    ktip
022900151222     c                   kfld                    arbpdc
023000151222     c                   eval      ktip='A'
023100151222     c                   eval      kCEV='MIC'
023200991216      * Dati società
023300991216     C                   Z-ADD     1             CODUT
023400991216     C                   CALL      'X§PARUT'
023500991216     C                   PARM                    UT§DSE0F
023600991216     C                   MOVEL     REC80         CNCR80
023700020919     C                   MOVEL     kpjbu         KPJBUs
023800040907      *
023900040907      *  imposta UDATE di elaborazione
024000040907     C                   move      *date         G02DAT
024100040907     C                   MOVE      *ZEROS        G02INV
024200040907     C                   MOVE      *BLANKS       G02ERR
024300040907     C                   CALL      'XSRDA8'
024400040907     C                   PARM                    WLBDAT
024500040907     C                   Z-ADD     G02INV        data_oggi
024600991213      *
024700991213      * Carico Tabella Eventi DPD
024800991213     c     'EVD'         chain     tntbe000                           31
024900991213      *
025000991213     c                   DOW       *in31 = *off
025100991213      * Se il S.I. è indicato deve essere uguale al mio
025200991213     c                   IF        TBEatb = *BLANKS  and
025300991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
025400991213     c                   movel     TBEuni        Devd
025500991213      * Carico solo eventi fissi da tradurre in import
025600991213      * la chiave di ricerca è formata dai primi 2 char dell'evento DPD
025700991213      * più il nostro cod evento
025800991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
025900991213     c                   add       1             xx
026000000103     c                   movel     TBEke1        KEVD(XX)
026100991213     c                   movel     TBEuni        EVD(XX)
026200991213     c                   ENDIF
026300991213     c                   ENDIF
026400991213      *
026500991213     c     'EVD'         reade     tntbe000                               31
026600991213     c                   ENDDO
026700991213      *
026800020918     C     endinz        ENDSR
026900941230      *-------------------------------------------------------------------------
027000151218     c     SCAN10        BEGSR
027100151221     c                   eval      wscan='10'
027200151218     c* Leggo dettaglio colli
027300151221     c                   exsr      cercaspu
027400151221     c
027500151221     c                   if        wtrov='S'
027600151221     c                   eval      wdata=Sdata
027700151221     c                   eval      wora =Sora
027800151221     c                   endif
027900151221
028000151218     c* prima di aggiornare lo scan, verifico se già pieni gli scan successivi,
028100151218     c*  che la data e ora siano <
028200151218     c* ?? non sono certa che mi serva questo controllo: aspetto
028300151218     c**                 clear                   sdp3imm
028400151218     c**   kspe          setll     tidp301l
028500151218     c**   kspe          reade     tidp301l
028600151218     c**                 dow       not %eof(tidp301l)
028700151218     c**                 if        sdp3dev>0
028800151218     c**                 movel     sdp3dev       sdp3imm          14 0
028900151218     c**                 move      sdp3hev       sdp3imm
029000151218     c**
029100151218     c**------------------------------------------------------------------------
029200151218     c** calcolo SCAN 10 a modo vecchio
029300151218     c**                 Select
029400151218     c**                 When      blpdse > *zeros and BLPcca <> '5'
029500151218     c*+                            and BLPcca <> '7'
029600151218     c**                            and blpdse <= data_oggi
029700151218     c**                 eval      wdata = blpdse
029800151218     c**                 When      BLPdcm > *zeros and BLPcca <> '5'
029900151218     c**                            and BLPcca <> '7'
030000151218     c**                            and blpdcm <= data_oggi
030100151218     c**                 eval      wdata = blpduc
030200151218     c**                 Endsl
030300151218      *
030400151218     c**                 If        wdata > *zeros
030500151218     c**                 eval      wora  = 0900
030600151218     c**                 Endif
030700151218     c
030800151218     c                   EndSR
030900151221      *-------------------------------------------------------------------------
031000151221     c     SCAN02        BEGSR
031100151221     c                   eval      wscan='02'
031200151221     c
031300160107    1c                   if        blpdti>0 and blpdti<=data_oggi
031400151221     c                   eval      wdata = blpdti
031500151221     c                   eval      wora  = blphti
031600151221     c
031700151221     c* se ora non presente, devo prenderla dalla spunta
031800160107    2c                   if        blphti=0
031900151221     c                   exsr      Cercaspu
032000151221     c                   eval      wdata=Sdata
032100151221     c                   eval      wora =Sora
032200160107    2c                   endif
032300160107    1c                   endif
032400151221     c*
032500151222     c* se data e ora ancora vuoti e la bolla è stata resa devo impostarla lo stesso
032600160107     c* Perchè lo scam 13 - consegna non viene dato per le spedizioni di reso
032700160107    1c                   if        wdata=0 and (blpcca='2' or blpcca='6')
032800160107     c                             and blpdcm>0
032900151222     c                   eval      wdata=blpdcm
033000160107    2c                   if        0700<blphmc
033100160107     c                   eval      wora =0700
033200160107     c                   else
033300160107     c                   eval      timeiso=%time(blphmc*100)
033400160107     c                   subdur    1:*h          timeiso
033500160107     c                   eval      sora6=%dec(timeiso)
033600160107     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
033700160107    2c                   endif
033800160107    1c                   endif
033900151222     c
034000151221     c
034100151221     c**                 Select
034200151221     c**                 When      blpdti > *zeros
034300151221     c**                            and blpdti <= data_oggi
034400151221     c**                            and  BLPcca <> '7'
034500151221     c**                 eval      wdata = blpdti
034600151221     c**                 eval      wora  = blphti
034700151221     c**                 When      blpdam > *zeros
034800151221     c**                            and blpdam <= data_oggi
034900151221     c**                            and  BLPcca <> '7'
035000151221     c**                 eval      wdata = blpdam
035100151221     c**                 When      blpdcm > *zeros  and  BLPcca <> '5'
035200151221     c**                            and BLPcca <> '7'
035300151221     c**                            and blpdcm <= data_oggi
035400151221     c**                 eval      wdata = blpdcm
035500151221     c**                 Endsl
035600151221      *
035700151221     c**                 If        wdata > *zeros  and  Wora = *zeros
035800151221     c**                 eval      wora  = 0700
035900151221     c**                 Endif
036000151221
036100151221     c                   ENDSR
036200151221      *-------------------------------------------------------------------------
036300151221     c     CercaSPU      BEGSR
036400151221     C                   CLEAR                   Wimm
036500151221     C                   CLEAR                   WDFV
036600151222     C                   CLEAR                   Wtrov
036700151221     C                   CLEAR                   Sdata
036800151221     C                   CLEAR                   Sora
036900151221     c                   if        blpncl=1
037000151221     c                   eval      bltfls=blpfls
037100151221     c                   eval      bltlna=blplna
037200151221     c                   eval      bltnrs=blpnrs
037300151221     c                   eval      bltnsc=blpncd
037400151221     c                   if        wscan='10'
037500151221     c                   exsr      leggispuP
037600151221     c                   else
037700151221     c                   exsr      leggispuA
037800151221     c                   endif
037900151221     c
038000151221     c                   else
038100151221     c     kspe          setll     fnblt01l
038200151221     c     kspe          reade     fnblt01l
038300151221     c                   dow       not %eof(fnblt01l)
038400151221     c                   if        wscan='10'
038500151221     c                   exsr      leggispuP
038600151221     c                   else
038700151221     c                   exsr      leggispuA
038800151221     c                   endif
038900151221     c
039000151221     c     kspe          reade     fnblt01l
039100151221     c                   enddo
039200151221     c                   endif
039300151221     c
039400151221     c* Trovata spunta reale
039500151221     c                   if        wtrov='S' and wimm>0
039600151221     c                   movel     wimm          sdata
039700151221     c                   move      wimm          whhmmss           6 0
039800151221     c                   eval      Sora=whhmmss/100
039900151221     c                   endif
040000151221     c
040100151221     c* e l'ora è maggiore di oggi --> clear
040200151221     c                   if        sdata>data_oggi
040300151221     c                   clear                   D53DFV
040400151221     c                   clear                   Sdata
040500151221     c                   clear                   Sora
040600151221     c                   clear                   wtrov
040700151221     c                   endif
040800151221     c                   ENDSR
040900151218      *-------------------------------------------------------------------------
041000151221     c     LeggispuP     BEGSR
041100151218     C*
041200151218     c* Cerco la data e ora spunta reale di categoria 5 o 1
041300151218     c     kbrv          setll     fnbrv07l
041400151218     c     kbrv          reade     fnbrv07l
041500151221    1c                   dow       not %eof(fnbrv07l)
041600151218     c
041700151221    2c                   if        brvnpg=5 or brvnpg=1 and (brvdcs>0  or
041800151221     c                             brvnps=00)
041900160107   2ac                   if        (brvatr=' ' or brvdcs>20160101)
042000160107     c                             and brvfgs<>bltlna
042100160107     c
042200151218     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
042300151218     c                   clear                   fnlv53ds
042400151218     C                   z-add     brvnFV        D53NFV
042500151218     C                   MOVEL     brvNPG        D53NPG
042600151218     C                   MOVEL     brvFGS        D53FGS
042700151218     C                   CALL      'FNLV53R'
042800151218     C                   PARM                    fnlv53ds
042900160107     C* spunta non errata e non di defluenza --> ok
043000151221    3C     D53ERR        IFeq      ' '
043100160107     c     d53def        andeq     ' '
043200151218     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
043300151222    4c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
043400151222     c                             or  brvdcs=0
043500151218     c                   movel     brvdfs        brvimm           14 0
043600151218     c                   move      brvhms        brvimm
043700151218     c                   else
043800151218     c                   movel     brvdcs        brvimm           14 0
043900151218     c                   move      brvhcs        brvimm
044000151221    4c                   endif
044100151218     c
044200151221    4c                   select
044300151221     c                   when      wdfv=0
044400151221     C                   MOVEL     brvimm        Wimm             14 0
044500151221     C                   MOVEL     d53DFV        WDFV
044600151221     c                   eval      wtrov='S'
044700151221     c* Se minore  memorizzo
044800151221     c                   when      d53dfv<wdfv
044900151218     C                   MOVEL     brvimm        Wimm             14 0
045000151218     C                   MOVEL     d53DFV        WDFV
045100151218     c                   eval      wtrov='S'
045200151218     c
045300151221     c* Se uguale tengo la data ora immissione + bassa
045400151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
045500151221     C                   MOVEL     brvimm        Wimm             14 0
045600151218     C                   MOVEL     d53DFV        WDFV
045700151218     c                   eval      wtrov='S'
045800151221    4c                   endsl
045900151218     c
046000151221    3c                   endif
046100151221    2c                   endif
046200151228    2c                   endif
046300151218     c
046400151222RM*  C     kbrv          reade     FNBRV07l                               33
046500151218    1c                   enddo
046600151218     c
046700151218     c                   endsr
046800151221      *-------------------------------------------------------------------------
046900151221     c     LeggispuA     BEGSR
047000151221     C*
047100151221     c* Cerco la data e ora spunta reale di categoria arrivi
047200151221     c     kbrv          setll     fnbrv07l
047300151221     c     kbrv          reade     fnbrv07l
047400151221    1c                   dow       not %eof(fnbrv07l)
047500151221     c
047600151221    2c                   if        brvnpg<>5 and brvnpg<>1 and (brvdcs>0
047700151221     c                             or brvnps=00)
047800151221    3c                   if        blptfa=0  or
047900151221     c                             brvfgs=blptfa or brvfgs=blplna or
048000151221     c                             brvpes=blplna
048100151221     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
048200151221     c                   clear                   fnlv53ds
048300151221     C                   z-add     brvnFV        D53NFV
048400151221     C                   MOVEL     brvNPG        D53NPG
048500151221     C                   MOVEL     brvFGS        D53FGS
048600151221     C                   CALL      'FNLV53R'
048700151221     C                   PARM                    fnlv53ds
048800151221     C* ERRORE
048900151221    4C     D53ERR        IFeq      ' '
049000151221     c     d53spg        andne     'P'
049100151221     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
049200151222    5c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
049300151222     c                             or  brvdcs=0
049400151221     c                   movel     brvdfs        brvimm           14 0
049500151221     c                   move      brvhms        brvimm
049600151221     c                   else
049700151221     c                   movel     brvdcs        brvimm           14 0
049800151221     c                   move      brvhcs        brvimm
049900151221    5c                   endif
050000151221     c
050100151221    5c                   select
050200151221     c                   when      wdfv=0
050300151221     C                   MOVEL     brvimm        Wimm             14 0
050400151221     C                   MOVEL     d53DFV        WDFV
050500151221     c                   eval      wtrov='S'
050600151221     c* Se minore  memorizzo
050700151221     c                   when      d53dfv<wdfv
050800151221     C                   MOVEL     brvimm        Wimm             14 0
050900151221     C                   MOVEL     d53DFV        WDFV
051000151221     c                   eval      wtrov='S'
051100151221     c
051200151221     c* Se uguale tengo la data ora immissione + bassa
051300151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
051400151221     C                   MOVEL     brvimm        Wimm             14 0
051500151221     C                   MOVEL     d53DFV        WDFV
051600151221     c                   eval      wtrov='S'
051700151221    5c                   endsl
051800151221     c
051900151221    4c                   endif
052000151221    3c                   endif
052100151221    2c                   endif
052200151221     c
052300151222     C     kbrv          reade     FNBRV07l                               33
052400151221    1c                   enddo
052500151221     c
052600151221     c                   endsr
052700151221      *-------------------------------------------------------------------------
052800151222     c     Riempi        BEGSR
052900151221     c                   exsr      LeggiSCAN
053000151222     c
053100151222     c                   if        sdata>0
053200151222     c                   SELECT
053300151222     c                   WHEN      §EVDipo = 'T'
053400151222     c                   z-add     blpflp        wfil
053500151222     c                   WHEN      §EVDipo = 'A'
053600151222     c                   z-add     blplna        wfil
053700151222     c                   OTHER
053800151222     c                   z-add     blplnp        wfil
053900151222     C                   ENDSL
054000151222      *
054100151222     c                   eval      sDP3fle = wfil
054200151222     c                   eval      sDP3dev = sdata
054300151222     c                   eval      sDP3hev = sora
054400151222     c                   clear                   sDP3ftr
054500151222      *
054600151222     c                   update    tidp3001
054700151222     c                   endif
054800160105
054900160105     c* imposto i campi W = ai campi S se per caso devo impostare
055000160105     c*  anche lo SCAN 10
055100160105     c                   eval      wdata=sdata
055200160105     c                   eval      wora =sora
055300151222     c                   ENDSR
055400151221
055500151222      *-------------------------------------------------------------------------
055600151221     c     LeggiSCAN     BEGSR
055700151221     c                   clear                   Sdata
055800151221     c                   clear                   Sora
055900151222     c* Verifico lo SCAN  --> se vuoto lo imposto
056000151221     c     kspe          setll     tidp301l
056100151221     c     kspe          reade     tidp301l
056200151221    1c                   dow       not %eof(tidp301l)
056300151221    2c                   if        sdp3evd=wscan  and wscan='10'
056400151221
056500151221    3c                   if        sdp3dev=0
056600151221    4c                   if        blpduc<wdata
056700151221     c                   eval      Sdata=blpduc
056800151221     c                   eval      Sora=1400
056900151221   x4c                   else
057000151221     c* data arrivo -1 lavorativo
057100151221     c                   clear                   xgiolavds
057200151221     c                   eval      IXGLDATA=wdata
057300151221     c                   eval      IXGLfil=blplnp
057400151221     c                   eval      IXGLpa ='P'
057500151221     c                   eval      IXGLsub ='S'
057600151221     c                   eval      IXGLgg  =1
057700151221     c                   eval      IXGLlav ='S'
057800151222     c                   call      'XGIOLAV'
057900151221     c                   parm                    xgiolavds
058000151221    5c                   if        oxglerr=' '
058100151221     c                   eval      Sdata=oxgldata
058200151221     c                   eval      Sora=1400
058300151221    5c                   endif
058400151221     c
058500151221    4c                   endif
058600151221    3c                   endif
058700151221     c
058800151221     c                   leave
058900151221    2c                   endif
059000151222     c
059100151222    2c                   if        sdp3evd=wscan  and wscan='02'
059200151222
059300151222    3c                   if        sdp3dev=0
059400151222     c                   eval      sdata=wdata
059500151223
059600151223    4c                   if        0700<wora
059700151222     c                   eval      sora =0700
059800151223     c                   else
059900151223     c                   eval      timeiso=%time(wora*100)
060000151223     c                   subdur    1:*h          timeiso
060100151223     c                   eval      sora6=%dec(timeiso)
060200151223     c                   eval      sora =%int(%subst(%editc(sora6:'X'):1:4))
060300151223    4c                   endif
060400151223    3c                   endif
060500151222     c
060600151222     c                   leave
060700151222    2c                   endif
060800151221     c
060900151221     c     kspe          reade     tidp301l
061000151221    1c                   enddo
061100151221     c
061200151222     c                   eval      W015A = sDP3evd
061300151221     c     W015A         lookup    KEVD(xx)                               31
061400151222     c                   movel     evd(xx)       Devd
061500151221     c                   endsr
061600151222      *-------------------------------------------------------------------------
061700151222     c     DistFitt      BEGSR
061800151222     c* Se distinta dittiza, la consegna merce la dò con orario inserimento ultimo MIC
061900151222     c     kevb          setgt     fnevb04l
062000151222     c     kevb          readpe    fnevb04l
062100151222     c                   if        not%eof(fnevb04l) and evbdev=blpddc
062200151222     c                   eval      devb01=evbnot
062300151222     c                   if        §NOTNDC=%editc(blpndc:'X')
062400160111     c
062500160111     c                   if        evbdev=evbdtv
062600160111     c
062700151222     c                   eval      wdata=EVBDTV
062800151223     c                   eval      wora =%int(%subst(%editc(EVBorv:'X'):1:4))
062900151222     c                   endif
063000151222     c                   endif
063100160111     c
063200160111     c                   endif
063300151222     c
063400151222     c                   endsr
