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
002700160112     D Rdata           S              8  0
002800160112     D Rora            S              4  0
002900151223     D Sora6           S              6  0
003000991213     D wfil            S              4  0
003100151218     D wdfv            S              8  0
003200151222     D wscan           S              2
003300151222     D wtrov           S              1
003400151221     d Dataiso         s               d   datfmt(*iso)
003500151223     d timeiso         s               t   timfmt(*iso)
003600020919     D kpjbus          S                   LIKE(kpjbu)
003700151222     D ktip            S                   LIKE(apdtip)
003800151222     D kcev            S                   LIKE(evbcev)
003900971212      *-------------------
004000991130     D KPJBA         E DS
004100151218     D fnlv53ds      E DS
004200991213     D DEVD          E DS
004300151222     D DEVb01        E DS
004400991216     D CNCR80        E DS
004500991216     D UT§DSE0F      E DS
004600020919     D og143         E DS
004700151221     D xgiolavds     E DS
004800040907     D*
004900040907     D WLBDAT          DS
005000040907     D  G02DAT                 1      8  0
005100040907     D  G02INV                 9     16  0
005200040907     D  G02ERR                17     17
005300040907     D  G02TGI                18     22  0
005400040907     D  data_Oggi      s                   like(G02INV) INZ(0)
005500991216      *----------------------------------------------------*
005600130910      * legge quelle con DATA = 0
005700060608     C     w0080         setll     tidp3000
005800060608     C     w0080         reade     tidp3000
005900991213      *
006000060608     C                   DOW       NOT %eof(tIDP302L)
006100991210      *
006200991213      * Controllo che si tratti di un evento fisso da tradurre
006300991213     c                   eval      xx = 1
006400000103     c                   eval      W015A = DP3evd
006500000103     c     W015A         lookup    KEVD(xx)                               31
006600991213      *
006700991214     C                   IF        *IN31 = *on
006800080604      ** se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
006900080604     c                             or Dp3CEV = 'ORM'
007000991213      *
007100991215     C                   If        BLPaas <> DP3aas or BLPlnp <> Dp3lnp  or
007200991215     C                             BLPnrs <> DP3nrs  or  BLPnsp <> DP3nsp
007300130910      *
007400151221     c                   clear                   blplna
007500151221     c                   clear                   blptfa
007600130910     c     KSPE          CHAIN     FNBLP01L
007700151221      *
007800151221      *   chain di ARB per info di arrivo merce
007900130912     c                   if        %Found(FNBLP01L)
008000130912      *
008100130910     c     KSPE          CHAIN     FNARB01L
008200130910      *  se c'è ARB sostituisce i 3 campi DTI/HTI/DAM del BLP
008300130910     c                   if        %Found(FNARB01L)
008400130910     c                   eval      BLPDTI = ARBDTI
008500130910     c                   eval      BLPHTI = ARBHTI
008600130910     c                   eval      BLPDAM = ARBDAM
008700140728     c*m                 eval      BLPDCM = ARBDCM
008800131106     c                   eval      BLPCCA = ARBCCA
008900140728     c*m                 eval      BLPHMC = ARBHMC
009000130910     c                   End
009100130912      *
009200130912     c                   Endif
009300130910      *
009400991216     c                   Endif
009500991213      *
009600991216     C                   clear                   wora
009700991216     C                   clear                   wfil
009800991216     C                   clear                   wdata
009900991216
010000991210     c                   SELECT
010100991216
010200130910      * se non è riuscito ad agganciare la bolla in PARTENZA
010300991213     c                   WHEN      NOT %found(FNBLP01L)
010400151218     c
010500151218     c* escludo tutte le bolle con consegna anomala 5 e 7
010600151218     c                   WHEN      blpcca='7' or blpcca ='5'
010700991216
010800151218      * Entrata all'Hub   SCAN-10
010900000103     C                   WHEN      DP3evd = '10 '
011000151218     c                   exsr      SCAN10
011100151218     c*
011200151218      * Data arrivo al P.O. di consegna  SCAN-02
011300151218     C                   WHEN      DP3evd = '02 '
011400151218     c                   exsr      SCAN02
011500130912      **
011600151222      * Data consegna reale (Delivery) SCAN-13
011700151222      *   da non dare nemmeno per RESO 2 e 6
011800151222     c                   WHEN      DP3evd = '13 '   and
011900151222     c                             BLPdcm > *zeros  and  BLPcca <> '2'
012000130912     c                              and BLPcca <> '6'
012100130912     c                              and blpdcm <= data_oggi
012200151223     c                   eval      wscan='13'
012300151222      *
012400151222      * Devo distinguere se la distinta è fittizia opure NO
012500160114      *  lo faccio solo se non ha consegna anomala
012600160114     c                   if        blpcca=' '
012700151222     c     kapd          chain     fiapd01l
012800151222     c                   if        %found(fiapd01l) and apdpdd='S'
012900151222     c                   exsr      DistFitt
013000151222     c                   endif
013100160114     c                   endif
013200151222
013300151222     c                   if        wdata=0
013400031125     c                   eval      wdata = blpdcm
013500031125     c                   If        blphmc = *zeros
013600031125     c                   eval      wora  = 1200
013700031125     c                   Else
013800031125     c                   eval      wora  = blphmc
013900031125     c                   Endif
014000151222     c                   Endif
014100031125
014200130910      * Data arrivo al transito   SCAN-01
014300061030      *  allo stesso modo degli altri SCAN non deve inviare l'informazione
014400061030      *  in presenza di un'anomalia 7 (non fatta xchè non è mai arrivato il collo)
014500151222     C**                 WHEN      DP3evd = '01 '  and  BLPdet > *zeros
014600151222     C**                                           and  BLPflp > *zeros
014700151222     c**                            and blpdet <= data_oggi
014800151222     c**                            and  BLPcca <> '7'
014900130910
015000151222     c**                 eval      wdata = blpdet
015100151222     c**                 eval      wora  = 0300
015200151222     c**                 exsr      elabora
015300151221     C                   ENDSL
015400130910
015500991216      *
015600991216      * Se ho impostato data e ora scrivo su file eventi
015700991216     c                   If        Wdata > *zeros  and  Wora > *zeros
015800160118     c                             and wdata>20160111
015900991216     c                   exsr      elabora
016000151221
016100160105     c* Riempo eventuale SCAN precedente
016200160105     c                   if        wscan='13'
016300160105     c                   eval      wscan='02'
016400160105     c                   exsr      Riempi
016500160105     c                   endif
016600160105
016700160105     c                   if        wscan='02'
016800151222     c                   eval      wscan='10'
016900151222     c                   exsr      Riempi
017000160105     c                   endif
017100160114     c*
017200160114     c                   Endif
017300130912      *
017400991213     c                   ENDIF
017500991210      *
017600060608     C     w0080         reade     tidp3000
017700991210     c                   ENDDO
017800971215      *
017900020918     c     fine          tag
018000151218     C                   MOVEL     'C'           D53TLA
018100151218     C                   CALL      'FNLV53R'
018200151218     C                   PARM                    fnlv53ds
018300151218     C*
018400991129     C                   eval      *INLR = *ON
018500991213      *----------------------------------------------------*
018600991214      *   AGGIORNO FILE EVENTI DPD                         *
018700991213      *----------------------------------------------------*
018800991213     C     ELABORA       BEGSR
018900991213      *
019000991213     c                   movel     evd(xx)       Devd
019100991213      *
019200991213     c                   SELECT
019300991214     c                   WHEN      §EVDipo = 'T'
019400991213     c                   z-add     blpflp        wfil
019500991214     c                   WHEN      §EVDipo = 'A'
019600991213     c                   z-add     blplna        wfil
019700991213     c                   OTHER
019800991213     c                   z-add     blplnp        wfil
019900991213     C                   ENDSL
020000991213      *
020100991213     c                   eval      DP3fle = wfil
020200991213     c                   eval      DP3dev = wdata
020300991213     c                   eval      DP3hev = wora
020400991213     c                   clear                   DP3ftr
020500991213      *
020600060608     c                   update    tidp3000
020700991213      *
020800160518      *  Se aggiornato uno SCAN 13 di DELIVERY
020900160518     c                   if        DP3EVD = '13 '
021000160518      * controlla se c'è un ALERT in attesa di essere inviato
021100160518      ***  x SCAN 18 E25/S25 mail o SMS
021200160519     c                   call      'FIEU17C'
021300160518     C                   parm                    dp3AAS
021400160518     C                   parm                    dp3LNP
021500160518     C                   parm                    dp3NRS
021600160518     C                   parm                    dp3NSP
021700160518     C                   parm                    dp3DEV
021800160518     C                   parm                    dp3HEV
021900160519     C                   parm                    dp3FLE
022000160518     C                   parm                    dp3PRN
022100160519     C                   parm                    dp3EVD
022200160518     c                   end
022300160518      *
022400991213     C                   ENDSR
022500930126      *----------------------------------------------------*
022600930126      *   DEFINIZIONE CHIAVI                               *
022700930126      *----------------------------------------------------*
022800940321     C     *INZSR        BEGSR
022900940321      *
023000991129     c     *ENTRY        PLIST
023100991130     c                   PARM                    KPJBA
023200991129      *
023300991129     C     KSPE          KLIST
023400991210     C                   KFLD                    dp3AAS
023500991210     C                   KFLD                    dp3LNP
023600991210     C                   KFLD                    dp3NRS
023700991210     C                   KFLD                    dp3NSP
023800151222     c     KEVB          KLIST
023900151222     C                   KFLD                    dp3AAS
024000151222     C                   KFLD                    dp3LNP
024100151222     C                   KFLD                    dp3NRS
024200151222     C                   KFLD                    dp3NSP
024300151222     C                   KFLD                    KCEV
024400151218     c     kbrv          klist
024500151218     c                   kfld                    bltfls
024600151218     c                   kfld                    bltlna
024700151218     c                   kfld                    bltnrs
024800151218     c                   kfld                    bltnsc
024900151222     c     kapd          klist
025000151222     c                   kfld                    ktip
025100151222     c                   kfld                    arbpdc
025200151222     c                   eval      ktip='A'
025300151222     c                   eval      kCEV='MIC'
025400991216      * Dati società
025500991216     C                   Z-ADD     1             CODUT
025600991216     C                   CALL      'X§PARUT'
025700991216     C                   PARM                    UT§DSE0F
025800991216     C                   MOVEL     REC80         CNCR80
025900020919     C                   MOVEL     kpjbu         KPJBUs
026000040907      *
026100040907      *  imposta UDATE di elaborazione
026200040907     C                   move      *date         G02DAT
026300040907     C                   MOVE      *ZEROS        G02INV
026400040907     C                   MOVE      *BLANKS       G02ERR
026500040907     C                   CALL      'XSRDA8'
026600040907     C                   PARM                    WLBDAT
026700040907     C                   Z-ADD     G02INV        data_oggi
026800991213      *
026900991213      * Carico Tabella Eventi DPD
027000991213     c     'EVD'         chain     tntbe000                           31
027100991213      *
027200991213     c                   DOW       *in31 = *off
027300991213      * Se il S.I. è indicato deve essere uguale al mio
027400991213     c                   IF        TBEatb = *BLANKS  and
027500991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
027600991213     c                   movel     TBEuni        Devd
027700991213      * Carico solo eventi fissi da tradurre in import
027800991213      * la chiave di ricerca è formata dai primi 2 char dell'evento DPD
027900991213      * più il nostro cod evento
028000991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
028100991213     c                   add       1             xx
028200000103     c                   movel     TBEke1        KEVD(XX)
028300991213     c                   movel     TBEuni        EVD(XX)
028400991213     c                   ENDIF
028500991213     c                   ENDIF
028600991213      *
028700991213     c     'EVD'         reade     tntbe000                               31
028800991213     c                   ENDDO
028900991213      *
029000020918     C     endinz        ENDSR
029100941230      *-------------------------------------------------------------------------
029200151218     c     SCAN10        BEGSR
029300151221     c                   eval      wscan='10'
029400151218     c* Leggo dettaglio colli
029500151221     c                   exsr      cercaspu
029600151221     c
029700151221     c                   if        wtrov='S'
029800151221     c                   eval      wdata=Sdata
029900151221     c                   eval      wora =Sora
030000151221     c                   endif
030100151221
030200151218     c* prima di aggiornare lo scan, verifico se già pieni gli scan successivi,
030300151218     c*  che la data e ora siano <
030400151218     c* ?? non sono certa che mi serva questo controllo: aspetto
030500151218     c**                 clear                   sdp3imm
030600151218     c**   kspe          setll     tidp301l
030700151218     c**   kspe          reade     tidp301l
030800151218     c**                 dow       not %eof(tidp301l)
030900151218     c**                 if        sdp3dev>0
031000151218     c**                 movel     sdp3dev       sdp3imm          14 0
031100151218     c**                 move      sdp3hev       sdp3imm
031200151218     c**
031300151218     c**------------------------------------------------------------------------
031400151218     c** calcolo SCAN 10 a modo vecchio
031500151218     c**                 Select
031600151218     c**                 When      blpdse > *zeros and BLPcca <> '5'
031700151218     c*+                            and BLPcca <> '7'
031800151218     c**                            and blpdse <= data_oggi
031900151218     c**                 eval      wdata = blpdse
032000151218     c**                 When      BLPdcm > *zeros and BLPcca <> '5'
032100151218     c**                            and BLPcca <> '7'
032200151218     c**                            and blpdcm <= data_oggi
032300151218     c**                 eval      wdata = blpduc
032400151218     c**                 Endsl
032500151218      *
032600151218     c**                 If        wdata > *zeros
032700151218     c**                 eval      wora  = 0900
032800151218     c**                 Endif
032900151218     c
033000151218     c                   EndSR
033100151221      *-------------------------------------------------------------------------
033200151221     c     SCAN02        BEGSR
033300151221     c                   eval      wscan='02'
033400151221     c
033500160107    1c                   if        blpdti>0 and blpdti<=data_oggi
033600151221     c                   eval      wdata = blpdti
033700151221     c                   eval      wora  = blphti
033800151221     c
033900151221     c* se ora non presente, devo prenderla dalla spunta
034000160107    2c                   if        blphti=0
034100151221     c                   exsr      Cercaspu
034200151221     c                   eval      wdata=Sdata
034300151221     c                   eval      wora =Sora
034400160107    2c                   endif
034500160107    1c                   endif
034600151221     c*
034700151222     c* se data e ora ancora vuoti e la bolla è stata resa devo impostarla lo stesso
034800160107     c* Perchè lo scam 13 - consegna non viene dato per le spedizioni di reso
034900160107    1c                   if        wdata=0 and (blpcca='2' or blpcca='6')
035000160107     c                             and blpdcm>0
035100151222     c                   eval      wdata=blpdcm
035200160107    2c                   if        0700<blphmc
035300160107     c                   eval      wora =0700
035400160107     c                   else
035500160107     c                   eval      timeiso=%time(blphmc*100)
035600160107     c                   subdur    1:*h          timeiso
035700160107     c                   eval      sora6=%dec(timeiso)
035800160107     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
035900160107    2c                   endif
036000160107    1c                   endif
036100151222     c
036200151221     c
036300151221     c**                 Select
036400151221     c**                 When      blpdti > *zeros
036500151221     c**                            and blpdti <= data_oggi
036600151221     c**                            and  BLPcca <> '7'
036700151221     c**                 eval      wdata = blpdti
036800151221     c**                 eval      wora  = blphti
036900151221     c**                 When      blpdam > *zeros
037000151221     c**                            and blpdam <= data_oggi
037100151221     c**                            and  BLPcca <> '7'
037200151221     c**                 eval      wdata = blpdam
037300151221     c**                 When      blpdcm > *zeros  and  BLPcca <> '5'
037400151221     c**                            and BLPcca <> '7'
037500151221     c**                            and blpdcm <= data_oggi
037600151221     c**                 eval      wdata = blpdcm
037700151221     c**                 Endsl
037800151221      *
037900151221     c**                 If        wdata > *zeros  and  Wora = *zeros
038000151221     c**                 eval      wora  = 0700
038100151221     c**                 Endif
038200151221
038300151221     c                   ENDSR
038400151221      *-------------------------------------------------------------------------
038500151221     c     CercaSPU      BEGSR
038600151221     C                   CLEAR                   Wimm
038700151221     C                   CLEAR                   WDFV
038800151222     C                   CLEAR                   Wtrov
038900151221     C                   CLEAR                   Sdata
039000151221     C                   CLEAR                   Sora
039100151221     c                   if        blpncl=1
039200151221     c                   eval      bltfls=blpfls
039300151221     c                   eval      bltlna=blplna
039400151221     c                   eval      bltnrs=blpnrs
039500151221     c                   eval      bltnsc=blpncd
039600151221     c                   if        wscan='10'
039700151221     c                   exsr      leggispuP
039800151221     c                   else
039900151221     c                   exsr      leggispuA
040000151221     c                   endif
040100151221     c
040200151221     c                   else
040300151221     c     kspe          setll     fnblt01l
040400151221     c     kspe          reade     fnblt01l
040500151221     c                   dow       not %eof(fnblt01l)
040600151221     c                   if        wscan='10'
040700151221     c                   exsr      leggispuP
040800151221     c                   else
040900151221     c                   exsr      leggispuA
041000151221     c                   endif
041100151221     c
041200151221     c     kspe          reade     fnblt01l
041300151221     c                   enddo
041400151221     c                   endif
041500151221     c
041600151221     c* Trovata spunta reale
041700151221     c                   if        wtrov='S' and wimm>0
041800151221     c                   movel     wimm          sdata
041900151221     c                   move      wimm          whhmmss           6 0
042000151221     c                   eval      Sora=whhmmss/100
042100151221     c                   endif
042200151221     c
042300151221     c* e l'ora è maggiore di oggi --> clear
042400151221     c                   if        sdata>data_oggi
042500151221     c                   clear                   D53DFV
042600151221     c                   clear                   Sdata
042700151221     c                   clear                   Sora
042800151221     c                   clear                   wtrov
042900151221     c                   endif
043000151221     c                   ENDSR
043100151218      *-------------------------------------------------------------------------
043200151221     c     LeggispuP     BEGSR
043300151218     C*
043400151218     c* Cerco la data e ora spunta reale di categoria 5 o 1
043500151218     c     kbrv          setll     fnbrv07l
043600151218     c     kbrv          reade     fnbrv07l
043700151221    1c                   dow       not %eof(fnbrv07l)
043800151218     c
043900151221    2c                   if        brvnpg=5 or brvnpg=1 and (brvdcs>0  or
044000151221     c                             brvnps=00)
044100160113   2ac**                 if        (brvatr=' ' or brvdcs>20160101)
044200160113     c**                           and brvfgs<>bltlna
044300160107     c
044400151218     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
044500151218     c                   clear                   fnlv53ds
044600151218     C                   z-add     brvnFV        D53NFV
044700151218     C                   MOVEL     brvNPG        D53NPG
044800151218     C                   MOVEL     brvFGS        D53FGS
044900151218     C                   CALL      'FNLV53R'
045000151218     C                   PARM                    fnlv53ds
045100160107     C* spunta non errata e non di defluenza --> ok
045200151221    3C     D53ERR        IFeq      ' '
045300160107     c     d53def        andeq     ' '
045400151218     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
045500151222    4c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
045600151222     c                             or  brvdcs=0
045700151218     c                   movel     brvdfs        brvimm           14 0
045800151218     c                   move      brvhms        brvimm
045900151218     c                   else
046000151218     c                   movel     brvdcs        brvimm           14 0
046100151218     c                   move      brvhcs        brvimm
046200151221    4c                   endif
046300151218     c
046400151221    4c                   select
046500151221     c                   when      wdfv=0
046600151221     C                   MOVEL     brvimm        Wimm             14 0
046700151221     C                   MOVEL     d53DFV        WDFV
046800151221     c                   eval      wtrov='S'
046900151221     c* Se minore  memorizzo
047000151221     c                   when      d53dfv<wdfv
047100151218     C                   MOVEL     brvimm        Wimm             14 0
047200151218     C                   MOVEL     d53DFV        WDFV
047300151218     c                   eval      wtrov='S'
047400151218     c
047500151221     c* Se uguale tengo la data ora immissione + bassa
047600151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
047700151221     C                   MOVEL     brvimm        Wimm             14 0
047800151218     C                   MOVEL     d53DFV        WDFV
047900151218     c                   eval      wtrov='S'
048000151221    4c                   endsl
048100151218     c
048200151221    3c                   endif
048300160113    2c***                endif
048400151228    2c                   endif
048500151218     c
048600151222RM*  C     kbrv          reade     FNBRV07l                               33
048700151218    1c                   enddo
048800151218     c
048900151218     c                   endsr
049000151221      *-------------------------------------------------------------------------
049100151221     c     LeggispuA     BEGSR
049200151221     C*
049300151221     c* Cerco la data e ora spunta reale di categoria arrivi
049400151221     c     kbrv          setll     fnbrv07l
049500151221     c     kbrv          reade     fnbrv07l
049600151221    1c                   dow       not %eof(fnbrv07l)
049700151221     c
049800151221    2c                   if        brvnpg<>5 and brvnpg<>1 and (brvdcs>0
049900151221     c                             or brvnps=00)
050000151221    3c                   if        blptfa=0  or
050100151221     c                             brvfgs=blptfa or brvfgs=blplna or
050200151221     c                             brvpes=blplna
050300151221     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
050400151221     c                   clear                   fnlv53ds
050500151221     C                   z-add     brvnFV        D53NFV
050600151221     C                   MOVEL     brvNPG        D53NPG
050700151221     C                   MOVEL     brvFGS        D53FGS
050800151221     C                   CALL      'FNLV53R'
050900151221     C                   PARM                    fnlv53ds
051000151221     C* ERRORE
051100151221    4C     D53ERR        IFeq      ' '
051200151221     c     d53spg        andne     'P'
051300151221     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
051400151222    5c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
051500151222     c                             or  brvdcs=0
051600151221     c                   movel     brvdfs        brvimm           14 0
051700151221     c                   move      brvhms        brvimm
051800151221     c                   else
051900151221     c                   movel     brvdcs        brvimm           14 0
052000151221     c                   move      brvhcs        brvimm
052100151221    5c                   endif
052200151221     c
052300151221    5c                   select
052400151221     c                   when      wdfv=0
052500151221     C                   MOVEL     brvimm        Wimm             14 0
052600151221     C                   MOVEL     d53DFV        WDFV
052700151221     c                   eval      wtrov='S'
052800151221     c* Se minore  memorizzo
052900151221     c                   when      d53dfv<wdfv
053000151221     C                   MOVEL     brvimm        Wimm             14 0
053100151221     C                   MOVEL     d53DFV        WDFV
053200151221     c                   eval      wtrov='S'
053300151221     c
053400151221     c* Se uguale tengo la data ora immissione + bassa
053500151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
053600151221     C                   MOVEL     brvimm        Wimm             14 0
053700151221     C                   MOVEL     d53DFV        WDFV
053800151221     c                   eval      wtrov='S'
053900151221    5c                   endsl
054000151221     c
054100151221    4c                   endif
054200151221    3c                   endif
054300151221    2c                   endif
054400151221     c
054500151222     C     kbrv          reade     FNBRV07l                               33
054600151221    1c                   enddo
054700151221     c
054800151221     c                   endsr
054900151221      *-------------------------------------------------------------------------
055000151222     c     Riempi        BEGSR
055100151221     c                   exsr      LeggiSCAN
055200151222     c
055300160112     c                   if        Rdata>0
055400151222     c                   SELECT
055500151222     c                   WHEN      §EVDipo = 'T'
055600151222     c                   z-add     blpflp        wfil
055700151222     c                   WHEN      §EVDipo = 'A'
055800151222     c                   z-add     blplna        wfil
055900151222     c                   OTHER
056000151222     c                   z-add     blplnp        wfil
056100151222     C                   ENDSL
056200151222      *
056300151222     c                   eval      sDP3fle = wfil
056400160112     c                   eval      sDP3dev = Rdata
056500160112     c                   eval      sDP3hev = Rora
056600151222     c                   clear                   sDP3ftr
056700151222      *
056800151222     c                   update    tidp3001
056900151222     c                   endif
057000160105
057100160105     c* imposto i campi W = ai campi S se per caso devo impostare
057200160105     c*  anche lo SCAN 10
057300160112     c                   eval      wdata=Rdata
057400160112     c                   eval      wora =Rora
057500151222     c                   ENDSR
057600151221
057700151222      *-------------------------------------------------------------------------
057800151221     c     LeggiSCAN     BEGSR
057900160112     c                   clear                   Rdata
058000160112     c                   clear                   Rora
058100151222     c* Verifico lo SCAN  --> se vuoto lo imposto
058200151221     c     kspe          setll     tidp301l
058300151221     c     kspe          reade     tidp301l
058400151221    1c                   dow       not %eof(tidp301l)
058500151221    2c                   if        sdp3evd=wscan  and wscan='10'
058600151221
058700151221    3c                   if        sdp3dev=0
058800160112     c* Prima ricerco nelle spunte, se non trovo uso data part ultimo collo
058900160112     c                   exsr      cercaspu
059000160113
059100160112     c                   select
059200160112     c                   when      wtrov='S'
059300160112     c                   eval      Rdata=Sdata
059400160112     c                   eval      Rora =Sora
059500160112     c
059600160112    4c                   when      blpduc<wdata
059700160112     c                   eval      Rdata=blpduc
059800160112     c                   eval      Rora=1400
059900160112   x4c                   other
060000151221     c* data arrivo -1 lavorativo
060100151221     c                   clear                   xgiolavds
060200151221     c                   eval      IXGLDATA=wdata
060300151221     c                   eval      IXGLfil=blplnp
060400151221     c                   eval      IXGLpa ='P'
060500151221     c                   eval      IXGLsub ='S'
060600151221     c                   eval      IXGLgg  =1
060700151221     c                   eval      IXGLlav ='S'
060800151222     c                   call      'XGIOLAV'
060900151221     c                   parm                    xgiolavds
061000151221    5c                   if        oxglerr=' '
061100160112     c                   eval      Rdata=oxgldata
061200160112     c                   eval      Rora=1400
061300151221    5c                   endif
061400151221     c
061500160112    4c                   endsl
061600151221    3c                   endif
061700151221     c
061800151221     c                   leave
061900151221    2c                   endif
062000151222     c
062100151222    2c                   if        sdp3evd=wscan  and wscan='02'
062200151222
062300151222    3c                   if        sdp3dev=0
062400160112     c                   eval      Rdata=wdata
062500151223
062600151223    4c                   if        0700<wora
062700160112     c                   eval      Rora =0700
062800151223     c                   else
062900151223     c                   eval      timeiso=%time(wora*100)
063000151223     c                   subdur    1:*h          timeiso
063100160114     c                   eval      Sora6=%dec(timeiso)
063200160114     c                   eval      Rora =%int(%subst(%editc(Sora6:'X'):1:4))
063300151223    4c                   endif
063400151223    3c                   endif
063500151222     c
063600151222     c                   leave
063700151222    2c                   endif
063800151221     c
063900151221     c     kspe          reade     tidp301l
064000151221    1c                   enddo
064100151221     c
064200151222     c                   eval      W015A = sDP3evd
064300151221     c     W015A         lookup    KEVD(xx)                               31
064400151222     c                   movel     evd(xx)       Devd
064500151221     c                   endsr
064600151222      *-------------------------------------------------------------------------
064700151222     c     DistFitt      BEGSR
064800151222     c* Se distinta dittiza, la consegna merce la dò con orario inserimento ultimo MIC
064900160906     c*           + 5 minuti per problemi di visualizzazione che ha la DPD
065000151222     c     kevb          setgt     fnevb04l
065100151222     c     kevb          readpe    fnevb04l
065200151222     c                   if        not%eof(fnevb04l) and evbdev=blpddc
065300151222     c                   eval      devb01=evbnot
065400151222     c                   if        §NOTNDC=%editc(blpndc:'X')
065500160111     c
065600160111     c                   if        evbdev=evbdtv
065700160111     c
065800151222     c                   eval      wdata=EVBDTV
065900160906     c                   eval      timeiso=%time(evborv)
066000160906     c                   adddur    5:*mn         timeiso
066100160906     c                   eval      sora6=%dec(timeiso)
066200160906     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
066300151222     c                   endif
066400151222     c                   endif
066500160111     c
066600160111     c                   endif
066700151222     c
066800151222     c                   endsr
