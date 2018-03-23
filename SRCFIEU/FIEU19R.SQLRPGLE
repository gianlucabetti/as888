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
003000161007     D wfil            S              3  0
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
012300161007      *
012400161007      *  cerca se c'è stato un dirottamento QUINDI BOLLA LEGATA
012500161007      *   di impostare la FILIALE che consegna realmente E NON
012600161007      *   come faceva prima la FILIALE originale di consegna.
012700151222      *
012800161007     C/EXEC SQL
012900161007     C+   SELECT LBLLAN INTO :wfil
013000161007     C+     FROM FNLBL00F WHERE
013100161007     C+   LBLAAO=:dp3aas  and
013200161007     C+   LBLLPO=:dp3lnp  and
013300161007     C+   LBLNRO=:dp3nrs  and
013400161007     C+   LBLNSO=:dp3nsp
013500161007     c+ ORDER BY RRN(FNLBL00F) desc
013600161007     c+ FETCH FIRST 1 ROW ONLY
013700161007     C/END-EXEC
013800161007      *  Deve testare che l'ultimo della catena non sia un RESO a DPD
013900161007      *   ossia che non sia una linea DPD per far rendere la merce
014000161007      *   in questo caso NON deve impostare il WFIL.
014100161007      *  Altrimenti è l'ultimo della catena dei dirottamenti.
014200161010     c                   if        wfil > 0
014300161007     c     wfil          chain     azorg01l
014400161007     c                   if            %Found(azorg01l)
014500161007     c                   movel     orgDE3        og143
014600161007     c                   if        §OGNTW ='DPD'
014700161007      *  lo pulisce come se fosse tutto come prima di questa modifica
014800161007      *  Lo scan 13 avrà il suo Depot della bolla originale.
014900161007     c                   clear                   wfil
015000161007     c                   end
015100161007     c                   end
015200161010     c                   end
015300161007      *
015400151222      * Devo distinguere se la distinta è fittizia opure NO
015500160114      *  lo faccio solo se non ha consegna anomala
015600160114     c                   if        blpcca=' '
015700151222     c     kapd          chain     fiapd01l
015800151222     c                   if        %found(fiapd01l) and apdpdd='S'
015900151222     c                   exsr      DistFitt
016000151222     c                   endif
016100160114     c                   endif
016200151222
016300151222     c                   if        wdata=0
016400031125     c                   eval      wdata = blpdcm
016500031125     c                   If        blphmc = *zeros
016600031125     c                   eval      wora  = 1200
016700031125     c                   Else
016800031125     c                   eval      wora  = blphmc
016900031125     c                   Endif
017000151222     c                   Endif
017100031125
017200130910      * Data arrivo al transito   SCAN-01
017300061030      *  allo stesso modo degli altri SCAN non deve inviare l'informazione
017400061030      *  in presenza di un'anomalia 7 (non fatta xchè non è mai arrivato il collo)
017500151222     C**                 WHEN      DP3evd = '01 '  and  BLPdet > *zeros
017600151222     C**                                           and  BLPflp > *zeros
017700151222     c**                            and blpdet <= data_oggi
017800151222     c**                            and  BLPcca <> '7'
017900130910
018000151222     c**                 eval      wdata = blpdet
018100151222     c**                 eval      wora  = 0300
018200151222     c**                 exsr      elabora
018300151221     C                   ENDSL
018400130910
018500991216      *
018600991216      * Se ho impostato data e ora scrivo su file eventi
018700991216     c                   If        Wdata > *zeros  and  Wora > *zeros
018800160118     c                             and wdata>20160111
018900991216     c                   exsr      elabora
019000151221
019100160105     c* Riempo eventuale SCAN precedente
019200160105     c                   if        wscan='13'
019300160105     c                   eval      wscan='02'
019400160105     c                   exsr      Riempi
019500160105     c                   endif
019600160105
019700160105     c                   if        wscan='02'
019800151222     c                   eval      wscan='10'
019900151222     c                   exsr      Riempi
020000160105     c                   endif
020100160114     c*
020200160114     c                   Endif
020300130912      *
020400991213     c                   ENDIF
020500991210      *
020600060608     C     w0080         reade     tidp3000
020700991210     c                   ENDDO
020800971215      *
020900020918     c     fine          tag
021000151218     C                   MOVEL     'C'           D53TLA
021100151218     C                   CALL      'FNLV53R'
021200151218     C                   PARM                    fnlv53ds
021300151218     C*
021400991129     C                   eval      *INLR = *ON
021500991213      *----------------------------------------------------*
021600991214      *   AGGIORNO FILE EVENTI DPD                         *
021700991213      *----------------------------------------------------*
021800991213     C     ELABORA       BEGSR
021900991213      *
022000991213     c                   movel     evd(xx)       Devd
022100991213      *
022200991213     c                   SELECT
022300161007     c                   WHEN      wfil >0
022400161007      *** se già stato impostato precedentemente NON fa nulla
022500161007      *
022600991214     c                   WHEN      §EVDipo = 'T'
022700991213     c                   z-add     blpflp        wfil
022800991214     c                   WHEN      §EVDipo = 'A'
022900991213     c                   z-add     blplna        wfil
023000991213     c                   OTHER
023100991213     c                   z-add     blplnp        wfil
023200991213     C                   ENDSL
023300991213      *
023400991213     c                   eval      DP3fle = wfil
023500991213     c                   eval      DP3dev = wdata
023600991213     c                   eval      DP3hev = wora
023700991213     c                   clear                   DP3ftr
023800991213      *
023900060608     c                   update    tidp3000
024000991213      *
024100160518      *  Se aggiornato uno SCAN 13 di DELIVERY
024200160518     c                   if        DP3EVD = '13 '
024300160518      * controlla se c'è un ALERT in attesa di essere inviato
024400160518      ***  x SCAN 18 E25/S25 mail o SMS
024500160519     c                   call      'FIEU17C'
024600160518     C                   parm                    dp3AAS
024700160518     C                   parm                    dp3LNP
024800160518     C                   parm                    dp3NRS
024900160518     C                   parm                    dp3NSP
025000160518     C                   parm                    dp3DEV
025100160518     C                   parm                    dp3HEV
025200160519     C                   parm                    dp3FLE
025300160518     C                   parm                    dp3PRN
025400160519     C                   parm                    dp3EVD
025500160518     c                   end
025600160518      *
025700991213     C                   ENDSR
025800930126      *----------------------------------------------------*
025900930126      *   DEFINIZIONE CHIAVI                               *
026000930126      *----------------------------------------------------*
026100940321     C     *INZSR        BEGSR
026200940321      *
026300991129     c     *ENTRY        PLIST
026400991130     c                   PARM                    KPJBA
026500991129      *
026600991129     C     KSPE          KLIST
026700991210     C                   KFLD                    dp3AAS
026800991210     C                   KFLD                    dp3LNP
026900991210     C                   KFLD                    dp3NRS
027000991210     C                   KFLD                    dp3NSP
027100151222     c     KEVB          KLIST
027200151222     C                   KFLD                    dp3AAS
027300151222     C                   KFLD                    dp3LNP
027400151222     C                   KFLD                    dp3NRS
027500151222     C                   KFLD                    dp3NSP
027600151222     C                   KFLD                    KCEV
027700151218     c     kbrv          klist
027800151218     c                   kfld                    bltfls
027900151218     c                   kfld                    bltlna
028000151218     c                   kfld                    bltnrs
028100151218     c                   kfld                    bltnsc
028200151222     c     kapd          klist
028300151222     c                   kfld                    ktip
028400151222     c                   kfld                    arbpdc
028500151222     c                   eval      ktip='A'
028600151222     c                   eval      kCEV='MIC'
028700991216      * Dati società
028800991216     C                   Z-ADD     1             CODUT
028900991216     C                   CALL      'X§PARUT'
029000991216     C                   PARM                    UT§DSE0F
029100991216     C                   MOVEL     REC80         CNCR80
029200020919     C                   MOVEL     kpjbu         KPJBUs
029300040907      *
029400040907      *  imposta UDATE di elaborazione
029500040907     C                   move      *date         G02DAT
029600040907     C                   MOVE      *ZEROS        G02INV
029700040907     C                   MOVE      *BLANKS       G02ERR
029800040907     C                   CALL      'XSRDA8'
029900040907     C                   PARM                    WLBDAT
030000040907     C                   Z-ADD     G02INV        data_oggi
030100991213      *
030200991213      * Carico Tabella Eventi DPD
030300991213     c     'EVD'         chain     tntbe000                           31
030400991213      *
030500991213     c                   DOW       *in31 = *off
030600991213      * Se il S.I. è indicato deve essere uguale al mio
030700991213     c                   IF        TBEatb = *BLANKS  and
030800991213     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
030900991213     c                   movel     TBEuni        Devd
031000991213      * Carico solo eventi fissi da tradurre in import
031100991213      * la chiave di ricerca è formata dai primi 2 char dell'evento DPD
031200991213      * più il nostro cod evento
031300991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
031400991213     c                   add       1             xx
031500000103     c                   movel     TBEke1        KEVD(XX)
031600991213     c                   movel     TBEuni        EVD(XX)
031700991213     c                   ENDIF
031800991213     c                   ENDIF
031900991213      *
032000991213     c     'EVD'         reade     tntbe000                               31
032100991213     c                   ENDDO
032200991213      *
032300020918     C     endinz        ENDSR
032400941230      *-------------------------------------------------------------------------
032500151218     c     SCAN10        BEGSR
032600151221     c                   eval      wscan='10'
032700151218     c* Leggo dettaglio colli
032800151221     c                   exsr      cercaspu
032900151221     c
033000151221     c                   if        wtrov='S'
033100151221     c                   eval      wdata=Sdata
033200151221     c                   eval      wora =Sora
033300151221     c                   endif
033400151221
033500151218     c* prima di aggiornare lo scan, verifico se già pieni gli scan successivi,
033600151218     c*  che la data e ora siano <
033700151218     c* ?? non sono certa che mi serva questo controllo: aspetto
033800151218     c**                 clear                   sdp3imm
033900151218     c**   kspe          setll     tidp301l
034000151218     c**   kspe          reade     tidp301l
034100151218     c**                 dow       not %eof(tidp301l)
034200151218     c**                 if        sdp3dev>0
034300151218     c**                 movel     sdp3dev       sdp3imm          14 0
034400151218     c**                 move      sdp3hev       sdp3imm
034500151218     c**
034600151218     c**------------------------------------------------------------------------
034700151218     c** calcolo SCAN 10 a modo vecchio
034800151218     c**                 Select
034900151218     c**                 When      blpdse > *zeros and BLPcca <> '5'
035000151218     c*+                            and BLPcca <> '7'
035100151218     c**                            and blpdse <= data_oggi
035200151218     c**                 eval      wdata = blpdse
035300151218     c**                 When      BLPdcm > *zeros and BLPcca <> '5'
035400151218     c**                            and BLPcca <> '7'
035500151218     c**                            and blpdcm <= data_oggi
035600151218     c**                 eval      wdata = blpduc
035700151218     c**                 Endsl
035800151218      *
035900151218     c**                 If        wdata > *zeros
036000151218     c**                 eval      wora  = 0900
036100151218     c**                 Endif
036200151218     c
036300151218     c                   EndSR
036400151221      *-------------------------------------------------------------------------
036500151221     c     SCAN02        BEGSR
036600151221     c                   eval      wscan='02'
036700151221     c
036800160107    1c                   if        blpdti>0 and blpdti<=data_oggi
036900151221     c                   eval      wdata = blpdti
037000151221     c                   eval      wora  = blphti
037100151221     c
037200151221     c* se ora non presente, devo prenderla dalla spunta
037300160107    2c                   if        blphti=0
037400151221     c                   exsr      Cercaspu
037500151221     c                   eval      wdata=Sdata
037600151221     c                   eval      wora =Sora
037700160107    2c                   endif
037800160107    1c                   endif
037900151221     c*
038000151222     c* se data e ora ancora vuoti e la bolla è stata resa devo impostarla lo stesso
038100160107     c* Perchè lo scam 13 - consegna non viene dato per le spedizioni di reso
038200160107    1c                   if        wdata=0 and (blpcca='2' or blpcca='6')
038300160107     c                             and blpdcm>0
038400151222     c                   eval      wdata=blpdcm
038500160107    2c                   if        0700<blphmc
038600160107     c                   eval      wora =0700
038700160107     c                   else
038800160107     c                   eval      timeiso=%time(blphmc*100)
038900160107     c                   subdur    1:*h          timeiso
039000160107     c                   eval      sora6=%dec(timeiso)
039100160107     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
039200160107    2c                   endif
039300160107    1c                   endif
039400151222     c
039500151221     c
039600151221     c**                 Select
039700151221     c**                 When      blpdti > *zeros
039800151221     c**                            and blpdti <= data_oggi
039900151221     c**                            and  BLPcca <> '7'
040000151221     c**                 eval      wdata = blpdti
040100151221     c**                 eval      wora  = blphti
040200151221     c**                 When      blpdam > *zeros
040300151221     c**                            and blpdam <= data_oggi
040400151221     c**                            and  BLPcca <> '7'
040500151221     c**                 eval      wdata = blpdam
040600151221     c**                 When      blpdcm > *zeros  and  BLPcca <> '5'
040700151221     c**                            and BLPcca <> '7'
040800151221     c**                            and blpdcm <= data_oggi
040900151221     c**                 eval      wdata = blpdcm
041000151221     c**                 Endsl
041100151221      *
041200151221     c**                 If        wdata > *zeros  and  Wora = *zeros
041300151221     c**                 eval      wora  = 0700
041400151221     c**                 Endif
041500151221
041600151221     c                   ENDSR
041700151221      *-------------------------------------------------------------------------
041800151221     c     CercaSPU      BEGSR
041900151221     C                   CLEAR                   Wimm
042000151221     C                   CLEAR                   WDFV
042100151222     C                   CLEAR                   Wtrov
042200151221     C                   CLEAR                   Sdata
042300151221     C                   CLEAR                   Sora
042400151221     c                   if        blpncl=1
042500151221     c                   eval      bltfls=blpfls
042600151221     c                   eval      bltlna=blplna
042700151221     c                   eval      bltnrs=blpnrs
042800151221     c                   eval      bltnsc=blpncd
042900151221     c                   if        wscan='10'
043000151221     c                   exsr      leggispuP
043100151221     c                   else
043200151221     c                   exsr      leggispuA
043300151221     c                   endif
043400151221     c
043500151221     c                   else
043600151221     c     kspe          setll     fnblt01l
043700151221     c     kspe          reade     fnblt01l
043800151221     c                   dow       not %eof(fnblt01l)
043900151221     c                   if        wscan='10'
044000151221     c                   exsr      leggispuP
044100151221     c                   else
044200151221     c                   exsr      leggispuA
044300151221     c                   endif
044400151221     c
044500151221     c     kspe          reade     fnblt01l
044600151221     c                   enddo
044700151221     c                   endif
044800151221     c
044900151221     c* Trovata spunta reale
045000151221     c                   if        wtrov='S' and wimm>0
045100151221     c                   movel     wimm          sdata
045200151221     c                   move      wimm          whhmmss           6 0
045300151221     c                   eval      Sora=whhmmss/100
045400151221     c                   endif
045500151221     c
045600151221     c* e l'ora è maggiore di oggi --> clear
045700151221     c                   if        sdata>data_oggi
045800151221     c                   clear                   D53DFV
045900151221     c                   clear                   Sdata
046000151221     c                   clear                   Sora
046100151221     c                   clear                   wtrov
046200151221     c                   endif
046300151221     c                   ENDSR
046400151218      *-------------------------------------------------------------------------
046500151221     c     LeggispuP     BEGSR
046600151218     C*
046700151218     c* Cerco la data e ora spunta reale di categoria 5 o 1
046800151218     c     kbrv          setll     fnbrv07l
046900151218     c     kbrv          reade     fnbrv07l
047000151221    1c                   dow       not %eof(fnbrv07l)
047100151218     c
047200151221    2c                   if        brvnpg=5 or brvnpg=1 and (brvdcs>0  or
047300151221     c                             brvnps=00)
047400160113   2ac**                 if        (brvatr=' ' or brvdcs>20160101)
047500160113     c**                           and brvfgs<>bltlna
047600160107     c
047700151218     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
047800151218     c                   clear                   fnlv53ds
047900151218     C                   z-add     brvnFV        D53NFV
048000151218     C                   MOVEL     brvNPG        D53NPG
048100151218     C                   MOVEL     brvFGS        D53FGS
048200151218     C                   CALL      'FNLV53R'
048300151218     C                   PARM                    fnlv53ds
048400160107     C* spunta non errata e non di defluenza --> ok
048500151221    3C     D53ERR        IFeq      ' '
048600160107     c     d53def        andeq     ' '
048700151218     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
048800151222    4c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
048900151222     c                             or  brvdcs=0
049000151218     c                   movel     brvdfs        brvimm           14 0
049100151218     c                   move      brvhms        brvimm
049200151218     c                   else
049300151218     c                   movel     brvdcs        brvimm           14 0
049400151218     c                   move      brvhcs        brvimm
049500151221    4c                   endif
049600151218     c
049700151221    4c                   select
049800151221     c                   when      wdfv=0
049900151221     C                   MOVEL     brvimm        Wimm             14 0
050000151221     C                   MOVEL     d53DFV        WDFV
050100151221     c                   eval      wtrov='S'
050200151221     c* Se minore  memorizzo
050300151221     c                   when      d53dfv<wdfv
050400151218     C                   MOVEL     brvimm        Wimm             14 0
050500151218     C                   MOVEL     d53DFV        WDFV
050600151218     c                   eval      wtrov='S'
050700151218     c
050800151221     c* Se uguale tengo la data ora immissione + bassa
050900151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
051000151221     C                   MOVEL     brvimm        Wimm             14 0
051100151218     C                   MOVEL     d53DFV        WDFV
051200151218     c                   eval      wtrov='S'
051300151221    4c                   endsl
051400151218     c
051500151221    3c                   endif
051600160113    2c***                endif
051700151228    2c                   endif
051800151218     c
051900151222RM*  C     kbrv          reade     FNBRV07l                               33
052000151218    1c                   enddo
052100151218     c
052200151218     c                   endsr
052300151221      *-------------------------------------------------------------------------
052400151221     c     LeggispuA     BEGSR
052500151221     C*
052600151221     c* Cerco la data e ora spunta reale di categoria arrivi
052700151221     c     kbrv          setll     fnbrv07l
052800151221     c     kbrv          reade     fnbrv07l
052900151221    1c                   dow       not %eof(fnbrv07l)
053000151221     c
053100151221    2c                   if        brvnpg<>5 and brvnpg<>1 and (brvdcs>0
053200151221     c                             or brvnps=00)
053300151221    3c                   if        blptfa=0  or
053400151221     c                             brvfgs=blptfa or brvfgs=blplna or
053500151221     c                             brvpes=blplna
053600151221     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
053700151221     c                   clear                   fnlv53ds
053800151221     C                   z-add     brvnFV        D53NFV
053900151221     C                   MOVEL     brvNPG        D53NPG
054000151221     C                   MOVEL     brvFGS        D53FGS
054100151221     C                   CALL      'FNLV53R'
054200151221     C                   PARM                    fnlv53ds
054300151221     C* ERRORE
054400151221    4C     D53ERR        IFeq      ' '
054500151221     c     d53spg        andne     'P'
054600151221     c* se la data immisione è maggiore della data di scarico --> uso data di scarico
054700151222    5c                   if        (brvdfs<=brvdcs and brvdfs>20000101)
054800151222     c                             or  brvdcs=0
054900151221     c                   movel     brvdfs        brvimm           14 0
055000151221     c                   move      brvhms        brvimm
055100151221     c                   else
055200151221     c                   movel     brvdcs        brvimm           14 0
055300151221     c                   move      brvhcs        brvimm
055400151221    5c                   endif
055500151221     c
055600151221    5c                   select
055700151221     c                   when      wdfv=0
055800151221     C                   MOVEL     brvimm        Wimm             14 0
055900151221     C                   MOVEL     d53DFV        WDFV
056000151221     c                   eval      wtrov='S'
056100151221     c* Se minore  memorizzo
056200151221     c                   when      d53dfv<wdfv
056300151221     C                   MOVEL     brvimm        Wimm             14 0
056400151221     C                   MOVEL     d53DFV        WDFV
056500151221     c                   eval      wtrov='S'
056600151221     c
056700151221     c* Se uguale tengo la data ora immissione + bassa
056800151221     c                   when      (d53dfv=wdfv  and brvimm<wimm)
056900151221     C                   MOVEL     brvimm        Wimm             14 0
057000151221     C                   MOVEL     d53DFV        WDFV
057100151221     c                   eval      wtrov='S'
057200151221    5c                   endsl
057300151221     c
057400151221    4c                   endif
057500151221    3c                   endif
057600151221    2c                   endif
057700151221     c
057800151222     C     kbrv          reade     FNBRV07l                               33
057900151221    1c                   enddo
058000151221     c
058100151221     c                   endsr
058200151221      *-------------------------------------------------------------------------
058300151222     c     Riempi        BEGSR
058400151221     c                   exsr      LeggiSCAN
058500151222     c
058600160112     c                   if        Rdata>0
058700151222     c                   SELECT
058800151222     c                   WHEN      §EVDipo = 'T'
058900151222     c                   z-add     blpflp        wfil
059000151222     c                   WHEN      §EVDipo = 'A'
059100151222     c                   z-add     blplna        wfil
059200151222     c                   OTHER
059300151222     c                   z-add     blplnp        wfil
059400151222     C                   ENDSL
059500151222      *
059600151222     c                   eval      sDP3fle = wfil
059700160112     c                   eval      sDP3dev = Rdata
059800160112     c                   eval      sDP3hev = Rora
059900151222     c                   clear                   sDP3ftr
060000151222      *
060100151222     c                   update    tidp3001
060200151222     c                   endif
060300160105
060400160105     c* imposto i campi W = ai campi S se per caso devo impostare
060500160105     c*  anche lo SCAN 10
060600160112     c                   eval      wdata=Rdata
060700160112     c                   eval      wora =Rora
060800151222     c                   ENDSR
060900151221
061000151222      *-------------------------------------------------------------------------
061100151221     c     LeggiSCAN     BEGSR
061200160112     c                   clear                   Rdata
061300160112     c                   clear                   Rora
061400151222     c* Verifico lo SCAN  --> se vuoto lo imposto
061500151221     c     kspe          setll     tidp301l
061600151221     c     kspe          reade     tidp301l
061700151221    1c                   dow       not %eof(tidp301l)
061800151221    2c                   if        sdp3evd=wscan  and wscan='10'
061900151221
062000151221    3c                   if        sdp3dev=0
062100160112     c* Prima ricerco nelle spunte, se non trovo uso data part ultimo collo
062200160112     c                   exsr      cercaspu
062300160113
062400160112     c                   select
062500160112     c                   when      wtrov='S'
062600160112     c                   eval      Rdata=Sdata
062700160112     c                   eval      Rora =Sora
062800160112     c
062900160112    4c                   when      blpduc<wdata
063000160112     c                   eval      Rdata=blpduc
063100160112     c                   eval      Rora=1400
063200160112   x4c                   other
063300151221     c* data arrivo -1 lavorativo
063400151221     c                   clear                   xgiolavds
063500151221     c                   eval      IXGLDATA=wdata
063600151221     c                   eval      IXGLfil=blplnp
063700151221     c                   eval      IXGLpa ='P'
063800151221     c                   eval      IXGLsub ='S'
063900151221     c                   eval      IXGLgg  =1
064000151221     c                   eval      IXGLlav ='S'
064100151222     c                   call      'XGIOLAV'
064200151221     c                   parm                    xgiolavds
064300151221    5c                   if        oxglerr=' '
064400160112     c                   eval      Rdata=oxgldata
064500160112     c                   eval      Rora=1400
064600151221    5c                   endif
064700151221     c
064800160112    4c                   endsl
064900151221    3c                   endif
065000151221     c
065100151221     c                   leave
065200151221    2c                   endif
065300151222     c
065400151222    2c                   if        sdp3evd=wscan  and wscan='02'
065500151222
065600151222    3c                   if        sdp3dev=0
065700160112     c                   eval      Rdata=wdata
065800151223
065900151223    4c                   if        0700<wora
066000160112     c                   eval      Rora =0700
066100151223     c                   else
066200151223     c                   eval      timeiso=%time(wora*100)
066300151223     c                   subdur    1:*h          timeiso
066400160114     c                   eval      Sora6=%dec(timeiso)
066500160114     c                   eval      Rora =%int(%subst(%editc(Sora6:'X'):1:4))
066600151223    4c                   endif
066700151223    3c                   endif
066800151222     c
066900151222     c                   leave
067000151222    2c                   endif
067100151221     c
067200151221     c     kspe          reade     tidp301l
067300151221    1c                   enddo
067400151221     c
067500151222     c                   eval      W015A = sDP3evd
067600151221     c     W015A         lookup    KEVD(xx)                               31
067700151222     c                   movel     evd(xx)       Devd
067800151221     c                   endsr
067900151222      *-------------------------------------------------------------------------
068000151222     c     DistFitt      BEGSR
068100151222     c* Se distinta dittiza, la consegna merce la dò con orario inserimento ultimo MIC
068200160906     c*           + 5 minuti per problemi di visualizzazione che ha la DPD
068300151222     c     kevb          setgt     fnevb04l
068400151222     c     kevb          readpe    fnevb04l
068500151222     c                   if        not%eof(fnevb04l) and evbdev=blpddc
068600151222     c                   eval      devb01=evbnot
068700151222     c                   if        §NOTNDC=%editc(blpndc:'X')
068800160111     c
068900160111     c                   if        evbdev=evbdtv
069000160111     c
069100151222     c                   eval      wdata=EVBDTV
069200160906     c                   eval      timeiso=%time(evborv)
069300160906     c                   adddur    5:*mn         timeiso
069400160906     c                   eval      sora6=%dec(timeiso)
069500160906     c                   eval      wora =%int(%subst(%editc(sora6:'X'):1:4))
069600151222     c                   endif
069700151222     c                   endif
069800160111     c
069900160111     c                   endif
070000151222     c
070100151222     c                   endsr
