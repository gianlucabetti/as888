000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140130     H* FNLRG2R *----------------------------------------------------*
000300140130     H* - Gestione contatti destinatari
000400140130     H*--------------------------------------------------------------*
000500070207      *
000600070320     Ffnlrg2d   CF   E             WORKSTN
000700070621     Ffnarb01l  if   E           k disk
000800070621     F                                     INFDS(FNarb1)
000900070621     F                                     RENAME(FNarb000:FNarb001)
001000070621     FFNarb00F  UF   E             DISK
001100070328     Ffiar401l  If   E           K disk
001200070322     F                                     INFDS(Fiar41)
001300070322     F                                     RENAME(Fiar4000:Fiar4001)
001400070329     Ffiar400f  uf a E             disk    prefix(F_)
001500070419     FFnevb22l  uf   E           k disk
001600070621     FFnevb00f  O    E             disk
001700070621     F                                     RENAME(Fnevb000:Fnevbf00)
001800091217     Ffiar501l  uf a E           k disk
001900070322     Ffiar901l  uf   E           k disk
002000070201     Ftabel00f  if   E           k disk
002100070320     Fazorg01l  if   E           k disk
002200070321     Ffiarn11l  if   E           k disk
002300070621     Ffnlbl01l  if   E           k disk
002400070411      *.......... T E L E F O N A T E   /   R E C L A M I
002500160404     fFITGD32L  if   e           k disk
002600011228      *
002700070621     D fnarb1          DS
002800070621     D  arbNRR               397    400B 0
002900011228     D Kpjba         E DS
003000011228      *
003100070328     D DSARBD        E DS
003200070328     D DSARBG        E DS
003300131008     D DSARBN        E DS
003400070328     D DARBT         E DS
003500070328     D DSARBK        E DS
003600070621     D DSarb0        E DS                  EXTNAME(FNarb00F)
003700070621     D SAVarb        E DS                  EXTNAME(FNarb00F)
003800070621     D                                     prefix(x_)
003900140508      * - Flag operativi IDA06FLO per richiamo FIDNA6R
004000140508     d DTGD01        e ds
004100070328     D fnlr48ds      E DS
004200070703     D fnlv55ds      E DS
004300070703     D fnlr66ds      E DS
004400070328     D trul90ds      E DS
004500130515     D DS7PQRS       E DS
004600070329     d* controllo tipo incasso e divisa C/Assegno
004700070329     d trulticds     e ds
004800070329     D* DS PER TRUL21R - controllo importi C/A
004900070329     d trul21ds      e ds
005000070328     D fnlrg2ds      E DS
005100140130     D fnlrn6ds      E DS
005200140130     D fnlrg7ds      E DS
005300070320     D fnlv80ds      E DS
005400070329     d ILV80NWPROC     s              1
005500131008     d
005600131008     D** fnlv19ds      E DS
005700131008     d
005800070201     D fidn12ds      E DS
005900070410     D fidna2ds      E DS
006000070410     d  IA2mod       e                     inz('M')
006100070410     d  IA2tor       e                     inz('S')
006200140508     D fidna6ds      E DS
006300140409     D fnlrj6ds      E DS
006400070503     D fidg34ds      E DS
006500141024     D fnlry09ds2    E DS
006600070926     d TRTB2Ads      e ds                  inz
006700070926     d   TB2Aopz     e                     inz('1')
006800070926     d   TB2Atip     e                     inz('G')
006900070315     d azuteds       e ds                  extname(azute00f)
007000070315     d ddatiute      e ds
007100070315     d tibs34ds      e ds                  inz
007200070131     D trul06ds      E DS
007300070131     D  lin                    1     90  0 dim(30)
007400091216     D trulintds     E DS
007500070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
007600070206     D PARAM1          DS                  INZ
007700070206     D  PA1AAS                14     17  0
007800070206     D  PA1LNP                18     20  0
007900070206     D  PA1NRS                21     22  0
008000070206     D  PA1NSP                23     29  0
008100070206     D  PA1F03                30     30
008200070206     D  PA1FLG                31     31
008300070322     D  PA1PGM                32     41
008400070322     D  PA1DSA                42     61
008500070322     D  PA1CML                62     62
008600070322     D  PA1DSP                63     70  0
008700070322     D  PA1RMN                71     85  0
008800070322     D  PA1LNA                86     88  0
008900070322     D  PA1LN1                89    108
009000070322     D  PA1RSD               109    143
009100070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
009200070206     D  PA1GIA               144    144
009300070322     D  PA1FEL               145    147  0
009400070322     D  PA1TBO               148    148
009500070329     D DS1A          E DS
009600070201     d ds3a          e ds
009700070618     d ds2a          e ds
009800070320     d ds5e          e ds
009900070316     d dsbl4l        e ds
010000080909     d dsbl4a        e ds
010100070321     d dar5Gen       e ds
010200091217     d dar5          e ds
010300091216     d dVpodeco      e ds
010400091216     D* DS PER TIBS02R - GESTIONE TNTBE00F
010500091216     D DSBS02        E DS                  EXTNAME(TIBS02DS)
010600011228      *
010700070621     D*                DS
010800070621     D* ARBAAS                 1      4  0
010900070621     D* ARBMGS                 5      8  0
011000070621     D* ARBDSP                 1      8  0
011100070329
011200030613     D WLBDA8          DS
011300030613     D  G02DAT                 1      8  0
011400030613     D  G02INV                 9     16  0
011500030613     D  G02ERR                17     17
011600030613     D  G02TGI                18     22  0
011700070322     D Fiar41          DS
011800070322     D  ar4nrr               397    400B 0
011900070328     d v2cnra          ds
012000070328     d  v2cnt1
012100070328     d  v2cnt2
012200070328     d  v2cnt3
012300070328     d  v2cnt4
012400070327     D                SDS
012500070327     D  N_pgm                  1      8
012600070621
012700070329     d forzacas        s              1
012800070328     D wtit2           S                   like(v2ctit2)
012900070328     d dataeur         s               d   datfmt(*eur)
013000070703     D wrkoggiiso      s               d
013100070321     d KAr5Trd         s                   like(Ar5Trd)
013200070321     d KArnCdn         s                   like(ArnCdn)inz('LDV')
013300070321     d KArngst         s                   like(Arngst)inz('S')
013400070321     d wtrc            s                   like(ar4trc)
013500070321     d wblk            s              1
013600070322     d sav4nrr         s                   like(ar4nrr)
013700070621     d savnrr          s                   like(arbnrr)
013800070328     d v2cdcr_i        s                   like(v2cdcr)
013900070329     d s_v2ctic        s                   like(v2ctic)
014000070329     d w_dtcr          s                   like(olv80tcrd)
014100070329     d pre_tic         s                   like(v2ctic)
014200070703     d*sav_kf          s                   like(*inkf)
014300070621     d kaas            s                   like(arbaas)
014400070621     d klnp            s                   like(arblnp)
014500070621     d knrs            s                   like(arbnrs)
014600070621     d knsp            s                   like(arbnsp)
014700070621     d vh2aap          s                   like(arbaas)
014800070621     d vh2lpp          s                   like(arblnp)
014900070621     d vh2nrp          s                   like(arbnrs)
015000070621     d vh2nsp          s                   like(arbnsp)
015100091127     d s_ar5cdi        s                   like(§ar5cdi)
015200140409     d wncl            s                   like(arbncl)
015300070924      *
015400070924     d Cod             s                   like(TBLcod)  inz
015500070924     d Key             s                   like(TBLkey)  inz
015600070924     d Ordina          s              1                  inz
015700070924     d Comando         s              1                  inz
015800070319
015900070316      *---------------------------------------------------------------------
016000070321     D coscas          C                   CONST('           Contrasse-
016100070321     D                                     gno           ')
016200070321     D cosapp          C                   CONST('           Appuntame-
016300070321     D                                     nto           ')
016400070321     D cossup          C                   CONST('           Supermerc-
016500070321     D                                     ato           ')
016600070321     D cosffd          C                   CONST('         Fermo Depos-
016700070321     D                                     ito           ')
016800070321     D coslav          C                   CONST('          Lasciato A-
016900070321     D                                     vviso         ')
017000070321     D cossuptas       C                   CONST('      Supermercato:t-
017100070321     D                                     assativo      ')
017200070321     D cosapptas       C                   CONST('      Appuntamento:t-
017300070321     D                                     assativo      ')
017400141030     D msg             S             78    DIM(15) CTDATA PERRCD(1)
017500070320      *---------------------------------------------------------------------
017600070320     C     *ENTRY        PLIST
017700070320     C                   PARM                    Kpjba
017800070320     C                   PARM                    fnlrg2ds
017900070320     c*
018000070320     c                   clear                   olrg2err
018100070320     c                   clear                   olrg2msg
018200070326     c                   clear                   olrg2no1
018300070326     c                   clear                   olrg2no2
018400070330     c                   clear                   olrg2f12
018500070320     c                   if        ilrg2tla='C'
018600070320     c                   goto      fine
018700070320     c                   endif
018800130827
018900130827     c* pulizia campo per errore fozabile
019000130827     c                   clear                   forzacas
019100130827
019200070327     C                   TIME                    W0140            14 0
019300070327
019400070327      * UDATE IN AAAAMMGG
019500070327     C                   MOVE      W0140         g02dat            8 0
019600070327     C                   MOVEL     *BLANK        G02ERR
019700070327     C                   CALL      'XSRDA8'
019800070327     C                   PARM                    WLBDA8
019900070327     C                   MOVEL     G02INV        DATEU             8 0
020000070321     c*
020100070621     c     karb          chain     fnarb01l
020200070621     c                   z-add     arbnrr        savnrr
020300070321     C                   MOVEL     'L'           WTRC
020400070328     C     Kar4          CHAIN     Fiar401l
020500070321     C                   if        %found(fiar401l)
020600070321     c                   movel     ar4not        dsbl4l
020700070322     c                   z-add     ar4nrr        sav4nrr
020800070328     c                   else
020900070328     c                   clear                   sav4nrr
021000070328     c                   clear                   dsbl4l
021100070321     c                   endif
021200070321     c                   if        ilrg2cau='L' and arbfbc='A'
021300070419     c     karb          setgt     fnevb22l
021400070419     c     karb          readpe(N) fnevb22l
021500070321     c                   endif
021600070320     c*
021700070321     C                   EXSR      RieD01
021800000608      *
021900070315     c                   do        *hival
022000070412     c                   if        ilrg2tss<>'S'and ilrg2tch<>'R'
022100070410     c                   exsr      sr_AbilF10
022200070412     c                   endif
022300140922     c                   exsr      sr_abilf19
022400070411     c  N13              exsr      sr_abilf16
022500070619     c                   write     lrg2t01
022600070320     C                   EXFMT     lrg2d01
022700070315     c                   setoff                                       9028
022800070315     c                   clear                   vidmsg
022900070326      * F12=Ritorno
023000070326     C                   if        *INKL
023100070330     c                   eval      olrg2f12='1'
023200070315     C                   leave
023300070315     c                   endif
023400070315      * F7=Interrogazione bolle arrivi
023500070315     c                   if        *inkg
023600070315     c                   clear                   kpjbu
023700070315     c                   clear                   param1
023800070322     C                   MOVE      v2haas        PA1AAS
023900070322     C                   MOVE      v2clnp        PA1LNP
024000070322     C                   MOVE      v2cnrs        PA1NRS
024100070322     C                   MOVE      v2cnsp        PA1NSP
024200070322     C                   MOVE      'N'           PA1FLG
024300070322     C                   MOVE      *blanks       PA1CML
024400070327     C                   MOVEL     N_pgm         PA1PGM
024500070315     c                   movel     param1        kpjbu
024600070315     c                   call      'FNLR36R'
024700070315     c                   parm                    kpjba
024800070315     c                   iter
024900070315     c                   endif
025000141105     c* Contatto non eseguibile per presenza dispo di dirottamento: permetto solo l'F12
025100141105     c                   if        not *inkl and OLRY09ESDI='1'
025200141105     c                   iter
025300141105     c                   endif
025400070410      * F16=Gestione Richieste (R.A.)
025500070405     c                   if        *inkq
025600070410     c                   reset                   FIDNA2ds
025700070410     c                   eval      ia2ogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
025800070410     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
025900070410     c                   call      'FIDNA2R'
026000070410     c                   parm                    KPJBA
026100070410     c                   parm                    FIDNA2ds
026200070405     c                   iter
026300070405     c                   endif
026400070412      *?F17 - Note LDV
026500070412if  1c                   if        *inKR = *on
026600140130     c                   clear                   fnlrn6ds
026700140130     c                   eval      in6mod='V'
026800140130     c                   eval      in6aas=arbaas
026900140130     c                   eval      in6lnp=arblnp
027000140130     c                   eval      in6nrs=arbnrs
027100140130     c                   eval      in6nsp=arbnsp
027200140131     c                   eval      in6pgm='FNLRG2R'
027300140130     c                   call      'FNLRN6R'
027400070412     c                   parm                    kpjba
027500140130     c                   parm                    fnlrn6ds
027600070412     c                   iter
027700070412e   1c                   endif
027800140922      *?F19 - Indicazioni di Consegna da WEB
027900140922if  1c                   if        *inKT = *on
028000141024     c                   clear                   fnlry09ds2
028100140922     c                   eval      ilry09tch='E'
028200141024     c                   eval      ILRY09TDIS='E'
028300140922     c                   eval      ILRY09AAS=ilrg2aas
028400140922     c                   eval      ILRY09LNP=ILRG2LNP
028500140922     c                   eval      ILRY09NRS=ILRG2NRS
028600140922     c                   eval      ILRY09NSP=ILRG2NSP
028700141024     c                   eval      ILRY09EF12='S'
028800141024     c                   eval      ILRY09EF21='S'
028900141024     c                   eval      ILRY09EF06='S'
029000141024
029100141024     c                   eval      kpjbu=fnlry09ds2
029200140922     c                   call      'FNLRY09R'
029300141013     c                   parm                    kpjba
029400140922     c                   iter
029500140922e   1c                   endif
029600070319      * controlli videata
029700070315     C                   EXSR      Ctrd01
029800070315     c   90              iter
029900070614     c* se immesso esito "E"=Esclusa da gestione-attesa giacenza emetto
030000070614     c* finestra per eventuale immissione codice giacenza
030100070703     c*                  if        v2cesi='E'
030200070703     c*                  eval      sav_kf=*inkf
030300070703     c*                  exsr      gesw01
030400070703     c*                  eval      *inkf=sav_kf
030500070703     c*                  endif
030600070320     c* f6=Conferma
030700070320     c   kf              exsr      confe
030800070322     c   90              iter
030900070329     c                   if        *inkf='1' and *in90='0'
031000070329     c                   leave
031100070329     c                   endif
031200070320     c* F10=Abilitazione Rapida
031300070320     c   kj              exsr      rapida
031400070326     c   90              iter
031500070329     c                   if        *inkj='1' and *in90='0'
031600070329     c                   leave
031700070329     c                   endif
031800011227      *
031900011227     C                   ENDDO
032000070124      *
032100011227      *
032200070320     c     fine          tag
032300070320     c                   if        ilrg2tla=' '
032400070320     c                   seton                                        rt
032500070320     c                   else
032600070328     c                   exsr      chiudi
032700070315     C                   seton                                        lr
032800070320     c                   endif
032900070315     c**********************************************************************
033000070320     c* Caricamento d01
033100070315     c**********************************************************************
033200070321     C     RieD01        BEGSR
033300070321     c                   setoff                                       90
033400070321     c* CAMPI DI OUTPUT
033500070320     c                   z-add     arblnp        v2clnp
033600070320     c                   z-add     arbnrs        v2cnrs
033700070320     c                   z-add     arbnsp        v2cnsp
033800070320     c                   movel     arbaas        wdata             8 0
033900070320     c                   move      arbmgs        wdata
034000070320     c     *iso          move      wdata         dataeur
034100070320     c                   move      dataeur       v2cdsp
034200070322     c                   move      arbaas        v2haas
034300070320     c                   movel     arbcbo        v2ccbo
034400070320     c                   clear                   v2dcbo
034500070320     c                   move      '3A'          tblcod
034600070320     c                   movel(p)  arbcbo        tblkey
034700070320     c                   exsr      ctabel
034800070320     c                   movel     tbluni        ds3a
034900070320     c                   movel     §3ades        v2dcbo
035000070320     c* tipo spedizione
035100070320     c                   clear                   v2dtsp
035200070320     c                   move      '5E'          tblcod
035300070320     c                   movel(p)  arbtsp        tblkey
035400070320     c                   exsr      ctabel
035500070320     c                   movel     tbluni        ds5e
035600090604     c                   movel     §5ed08        v2dtsp
035700070320     c* mittente
035800070320     c                   clear                   v2cmit
035900070320     c                   if        %subst(§3atb1:1:1)='F'
036000070320     c                   move      arbksc        v2cmit
036100070320     c                   else
036200070320     c                   move      arbccm        wccm4             4 0
036300070320     c                   if        wccm4<>9999
036400070320     c                   move      arbccm        v2cmit
036500070320     c                   endif
036600070320     c                   endif
036700070320     c* ragione sociale mittente
036800070320     c                   movel(P)  arbrsm        v2crsm
036900070320     c* ragione sociale mittente originale
037000070320     c                   movel(P)  arbrmo        v2crmo
037100070320     c* destinatario
037200070320     c                   movel(P)  arbrsd        v2crsd
037300070320     c* estensione ragione sociale destinatario da fiar4 record "D"
037400070320     C                   MOVEL     'D'           WTRC
037500070328     C     KAR4          CHAIN     Fiar401l
037600070320     C                   IF        %found(fiar401l)
037700070320     C                   MOVE      AR4NOT        v2crsd
037800070320     C                   ENDIF
037900070321     c                   movel     arbind        v2cind
038000070321     c                   movel     arbcad        v2ccad
038100070321     c                   movel     arblod        v2clod
038200070321     c                   eval      v2clod=%trim(v2clod)+ '  ' + arbprd
038300070321     c* referente consegna
038400070321     c                   Clear                   DAr5Gen
038500070321     c                   Eval      kAr5Trd = 'GEN'
038600091217     c     KFiar5        Chain(n)  Fiar501l
038700070321     c                   If        %Found(Fiar501l)
038800070321     c                   Movel     Ar5Uni        DAr5Gen
038900070321     c                   EndIf
039000070327    2c                   if        %subst(§ar5ref:1:1) = '#'
039100070327     c                   eval      v2cref=%subst(§ar5ref:2:34)
039200070327     c                   else
039300070321     c                   Eval      V2cRef = §Ar5Ref
039400070327     c                   endif
039500070327    2c                   if        %subst(§ar5teld:1:1) = '#'
039600070327     c                   eval      v2ctel=%subst(§ar5teld:2:15)
039700070327     c                   else
039800070321     c                   Eval      V2cTel = §Ar5Teld
039900070327     c                   endif
040000091127     c* memorizzo anche eventuale codice intestazione assegno
040100091127     c                   eval      s_ar5cdi=§ar5cdi
040200070321     c* consegna richiesta
040300070321     c                   clear                   fnlv80ds
040400070321     c                   eval      ilv80aas=arbaas
040500070321     c                   eval      ilv80lnp=arblnp
040600070321     c                   eval      ilv80nrs=arbnrs
040700070321     c                   eval      ilv80nsp=arbnsp
040800070321     c                   eval      ilv80mgs=arbmgs
040900070321     c                   eval      ilv80tcr=arbtcr
041000070321     c                   eval      ilv80dcr=arbdcr
041100070321     c                   eval      ilv80hcr=arbhcr
041200070321     c                   eval      ilv80tc1=arbtc1
041300070321     c                   eval      ilv80tc2=arbtc2
041400070321     c                   call      'FNLV80R'
041500070321     c                   parm                    kpjba
041600070321     c                   parm                    fnlv80ds
041700070329     c                   parm      'S'           ILV80NWPROC
041800070321     c                   eval      v2ctcro=olv80tcrd
041900070330     c                   if        arbdcr>0
042000070329     c     *iso          move      arbdcr        dataeur
042100070329     c                   move      dataeur       v2cdcro
042200070330     c                   else
042300070330     c                   clear                   v2cdcro
042400070330     c                   endif
042500070321     c                   eval      v2chcro=arbhcr
042600070321     c* colli e peso bollettato
042700070321     c                   z-add     arbncl        v2cncl
042800070321     c                   z-add     arbpkb        v2cpkb
042900070326     c* altre gestioni
043000070321     c                   clear                   v2dges
043100070321     c                   clear                   v2cges
043200140130     c                   clear                   fnlrg7ds
043300140130     c                   z-add     v2haas        ILRG7AAS
043400140130     c                   z-add     v2clnp        ILRG7LNP
043500140130     c                   z-add     v2cnrs        ILRG7NRS
043600140130     c                   z-add     v2cnsp        ILRG7NSP
043700140130     c                   movel     ilrg2cau      ILRG7NB1
043800140130     c                   call      'FNLRG7R'
043900140130     c                   parm                    fnlrg7ds
044000140130     c                   movel     OLRG7VSSIN    v2dges
044100140409     c* visualizzo se tutti i colli arrivati alla fil di arrivo
044200140409     c                   clear                   fnlrj6ds
044300140409     c                   eval      iLRJ6TRIC = 'A'
044400140409     c                   eval      iLRJ6AAS  = v2haas
044500140409     c                   eval      iLRJ6LNP  = v2cLNP
044600140409     c                   eval      iLRJ6NRS  = v2cNRS
044700140409     c                   eval      iLRJ6NSP  = v2cNSP
044800140409     c                   call      'FNLRJ6R'
044900140409     c                   parm                    fnlrj6ds
045000140409     c
045100140409     c                   clear                   v2carr
045200140409     c                   if        OLRJ6FALL='S'
045300140409     c                   eval      v2carr='TUTTI'
045400140409     c                   else
045500140409     c                   eval      wncl=OLRJ6CDCM+OLRJ6CSPU
045600140409     c                   eval      v2carr=%editc(wncl:'Z')
045700140409     c                   if        v2carr=*blanks
045800140409     c                   move      '0'           v2carr
045900140409     c                   endif
046000140409     c                   endif
046100070702     c*
046200070702     c* causale per variazione bolla
046300070702     c                   clear                   v2ccvb
046400130515     c* particolarità
046500130515     c                   setoff                                       444547
046600130515     c                   clear                   v2igga
046700130515     c                   clear                   v2igma
046800130515     c                   clear                   v2igva
046900130515     c                   eval      v2cgga=arbgga
047000130515     c                   if        v2cgga<>*blanks
047100130515     c                   seton                                        44
047200130515     c                   endif
047300130515     c                   eval      v2cgma=arbgma
047400130515     c                   if        v2cgma<>*blanks
047500130515     c                   seton                                        45
047600130515     c                   endif
047700130515     c                   eval      v2cgva=arbgva
047800130515     c                   if        v2cgva<>*blanks
047900130515     c                   seton                                        47
048000130515     c                   endif
048100070326
048200070321     c* abilitazione merce
048300140131     C**   ARBAMA        IFGT      0
048400140131     C**                 MOVEL     'SI'          V2CAMA
048500140131     C**                 ELSE
048600140131     C**                 MOVEL     'NO'          V2CAMA
048700140131     C**                 ENDIF
048800140131     C**                 MOVEL     '     '       V2DAMA
048900140131     C**   ARBAMA        IFEQ      3
049000140131     C**                 MOVEL     'FORZ.'       V2dAMA
049100140131     C**                 ELSE
049200140131     C**   ARBAMA        IFEQ      1
049300140131     C**                 MOVEL     'PART.'       V2DAMA
049400140131     C**                 ENDIF
049500140131     C**                 ENDIF
049600080909     c* SI DDT
049700080909     c                   setoff                                       14
049800080909     c                   movel     'A'           wtrc
049900080909     c     kar4          chain     fiar401l
050000080909     c                   if        %found(fiar401l)
050100080909     c                   movel     ar4not        dsbl4a
050200080909     c                   if        §b4abm='S' or §b4abm='C' or
050300080909     c                             §b4abm='P' or §b4abm='Q'
050400080909     c                   seton                                        14
050500080909     c                   endif
050600080909     c                   endif
050700070321     c* flag fermo deposito
050800070321     c                   if        arbffd<>*blanks
050900070321     c                   seton                                        10
051000070321     c                   else
051100070321     c                   setoff                                       10
051200070321     c                   endif
051300070321     c* abilitazione tasto funzionale F10-Abilitazione Rapida
051400070412     c                   setoff                                       1213
051500070329     c                   if        ilrg2tss='S' or ilrg2tch='R'
051600070321     c                   seton                                        12
051700070405     c                   if        ilrg2tch='R'
051800070405     c                   seton                                        13
051900070321     c                   endif
052000070410     c                   endif
052100070412     c  N12              exsr      sr_AbilF10
052200070321     c* Sottotitolo videata + situazione contatto in corso
052300070321     c                   clear                   v2ctit2
052400070321     c                   clear                   v2dblk
052500070402     c                   clear                   wblk
052600070321     c                   select
052700070321     c* contrassegno
052800070321     c                   when      ilrg2cau='C'
052900070321     c                   eval      v2ctit2=coscas
053000070321     c                   if        arbacc<>'S'
053100070321     c                   move      §b4ltca       wblk
053200070321     c* se il c/a non è abilitato e §b4ltca=blaks c'è qualcosa che non va..
053300070321     c                   if        wblk=*blanks
053400070321     c                   move      *hival        wblk
053500070321     c                   endif
053600070321     c                   endif
053700070321     c* appuntamento
053800070321     c                   when      ilrg2cau='A'
053900070321     c                   if        ilrg2tss=' '
054000070321     c                   eval      v2ctit2=cosapp
054100070321     c                   else
054200070321     c                   eval      v2ctit2=cosapptas
054300070321     c                   endif
054400070321     c                   move      §b4lbas       wblk
054500070321     c* supermercato
054600070321     c                   when      ilrg2cau='S'
054700070321     c                   if        ilrg2tss=' '
054800070321     c                   eval      v2ctit2=cossup
054900070321     c                   else
055000070321     c                   eval      v2ctit2=cossuptas
055100070321     c                   endif
055200070321     c                   move      §b4lbas       wblk
055300070321     c* fermo deposito
055400070321     c                   when      ilrg2cau='F'
055500070321     c                   eval      v2ctit2=cosffd
055600070321     c                   move      §b4ltfd       wblk
055700070321     c* lasciato avviso
055800070321     c                   when      ilrg2cau='L'
055900070321     c                   eval      v2ctit2=coslav
056000070321     c                   if        arbfbc='A'
056100070321     c                   move      evb2tel       wblk
056200070321     c* se il lavv non è abilitato e evb2tel=blank c'è qualcosa che non va..
056300070321     c                   if        wblk=*blanks
056400070321     c                   move      *hival        wblk
056500070321     c                   endif
056600070321     c                   endif
056700070321     c                   endsl
056800070321
056900070321     c                   select
057000070321     c                   when      wblk=*blanks
057100070328     c                   if        ilrg2cau='F'
057200070328     c                   eval      v2dblk='CONTATTATO'
057300070328     c                   else
057400070328     c                   eval      v2dblk='ABILITATO'
057500070328     c                   endif
057600070321     c                   when      wblk='S'
057700070328     c                   eval      v2dblk='DA CONTATTARE'
057800070321     c                   when      wblk='T'
057900070328     c                   eval      v2dblk='DA RICONTATTARE'
058000070321     c                   when      wblk='N'
058100070328     c                   eval      v2dblk='ESCLUSO DA GESTIONE--
058200070328     c                             ATTESA GIACENZA'
058300070321     c                   other
058400070328     c                   eval      v2dblk=*all'?'
058500070321     c                   endsl
058600070503     c* giro di consegna
058700070503     c                   clear                   v2ccgi
058800070504     c***                clear                   v2ccos
058900070503     c                   clear                   v2dcgi
059000070503     c                   clear                   fidg34ds
059100070503     c                   eval      id34tip='2'
059200070503     c                   eval      id34aas=arbaas
059300070503     c                   eval      id34lnp=arblnp
059400070503     c                   eval      id34nrs=arbnrs
059500070503     c                   eval      id34nsp=arbnsp
059600070503     c                   call      'FIDG34R'
059700070503     c                   parm                    FIDG34DS
059800070503     c                   movel     od34cgi       v2ccgi
059900070503     c                   eval      v2dcgi=od34cgides
060000070504     c**                 if        od34esito = *blanks
060100070504     c**                 eval      v2ccos='-'
060200070504     c**                 endif
060300070503     c
060400070321     c*
060500070321     c* CAMPI DI INPUT/OUTPUT
060600070321     c*
060700070321     c* note partenza
060800070321     c                   clear                   v2cnot8
060900070321     c                   clear                   v2cnot9
061000070322     c                   clear                   v2hnot8
061100070322     c                   clear                   v2hnot9
061200070321     C                   MOVEL     '8'           WTRC
061300070328     C     Kar4          CHAIN     Fiar401l
061400070321     C                   if        %found(fiar401l)
061500070321     C                   MOVEL     ar4NOT        V2cnot8
061600070322     C                   MOVEL     ar4NOT        V2hnot8
061700070321     C                   ENDIF
061800070321     C*
061900070321     C                   MOVEL     '9'           WTRC
062000070328     C     Kar4          CHAIN     Fiar401l
062100070321     C                   if        %found(fiar401l)
062200070321     C                   MOVEL     ar4NOT        V2cnot9
062300070322     C                   MOVEL     ar4NOT        V2hnot9
062400070321     C                   ENDIF
062500070321     c* Contrassegno
062600070321     c                   if        ilrg2cau='C'
062700070321     c                   setoff                                       11
062800070321     c                   eval      v2cflc='?'
062900070321     c                   else
063000070321     c                   seton                                        11
063100070321     c                   eval      v2cflc=':'
063200070321     c                   endif
063300070321     C                   MOVEL     *BLANKS       V2cTIC
063400070329     C                   MOVEL     *BLANKS       S_V2cTIC
063500070329     C                   MOVEL     *BLANKS       pre_TIC
063600070321     C                   MOVEL     *ZERO         V2cCAS
063700070321     C                   MOVEL     *BLANKS       V2cVCA
063800091127     c                   setoff                                       15
063900070321     C     §3AFCA        IFGT      *ZERO
064000070322     C     Karb          CHAIN(N)  FiAR901L
064100070321     C                   if        %found(fiar901l)
064200070321     C                   MOVEL     AR9TIC        V2cTIC
064300070329     C                   MOVEL     AR9TIC        s_V2cTIC
064400070329     C                   MOVEL     AR9TIC        pre_TIC
064500070321     C                   MOVEL     AR9CAS        V2cCAS
064600070321     C                   MOVEL     AR9VCA        V2cVCA
064700070321     C                   ENDif
064800091216     c* richiamo trul per determinare l'intestatario del c/a
064900091216     c                   if        dateu>=§VPOCAS
065000091216     c                   clear                   trulintds
065100091216     c                   eval      iinttic=v2ctic
065200091216     c                   eval      iinttip='I'
065300091216     c                   eval      IINTRSCM=arbrsm
065400091216     c                   eval      IINTRSCO=arbrmo
065500091216     c                   eval      IINTcdi=s_ar5cdi
065600091216     c                   call      'TRULINTR'
065700091216     c                   parm                    trulintds
065800091216     c                   if        ointerr=*blanks
065900091127     c                   seton                                        15
066000091216     c                   eval      v2ccdi=ointrsci
066100091216     c                   endif
066200091216     c                   endif
066300070321     C                   ENDIF
066400070328     c* DESCRIZIONE ESITO
066500070328     c                   clear                   v2desa
066600070328     c                   clear                   v2desi
066700070328     c* PRIMA RIGA
066800070328     c                   select
066900070328     c* Tassative
067000070328     c                   when      ilrg2tss='S'
067100070328     c                   eval      v2desa='(D=Da contattare)'
067200070328     c* Fermo deposito
067300070328     c                   when      ilrg2cau='F'
067400070328     c                   eval      v2desa='(C=Contattato; R=Ricontattare;'
067500070328     c* Appuntamenti
067600070412     c***                when      ilrg2cau='A'
067700070412     c***                eval      v2desa='(A=Abilitato; R=Ricontattare)'
067800070328     c* Altri
067900070328     c                   other
068000070328     c                   eval      v2desa='(A=Abilitato; R=Ricontattare;'
068100070328     c                   endsl
068200070328     c* SECONDA RIGA
068300070328     c                   if        (ilrg2cau='S' and ilrg2tss=' ') or
068400070412     c                             ilrg2cau='C' or ilrg2cau='A' or
068500070328     c                             ilrg2cau='L' or ilrg2cau='F'
068600070712     c                   eval      v2desi='E=Esclusa da gestione-apertura giace-
068700070712     c                             nza)'
068800070328     c                   endif
068900070321     c
069000070321     c                   clear                   v2ctcr
069100070321     c                   clear                   v2cdcr
069200070321     c                   clear                   v2chcr
069300070321     c                   clear                   v2cesi
069400070330     c                   if        ilrg2tch='R'
069500070328     c                   eval      v2cnra=ilrg2no1
069600070328     c                   else
069700070321     c                   clear                   v2cnt1
069800070321     c                   clear                   v2cnt2
069900070321     c                   clear                   v2cnt3
070000070321     c                   clear                   v2cnt4
070100070328     c                   endif
070200070614     c* campi della window per l'inserimento del codice giacenza
070300070614     c                   clear                   w2ccmc
070400070614     c                   clear                   w2dcmc
070500070321     c*
070600070321     c
070700070315     C                   ENDSR
070800070321     c**********************************************************************
070900070321     c* Verifica abilitazione tasto F10-Abilitazione Rapida
071000070321     c**********************************************************************
071100070321     C     SR_AbilF10    BEGSR
071200070321     c* L'F10 non è da abilitare quando:
071300070321     c* - Sono in gestione delle tassative
071400070321     c* - Sono in gestione di spedizioni per le quali sono già presenti Note
071500070321     c*   AUT
071600070321     c* Il Primo caso è sufficiente controllarlo una volta nel pgm mentre il
071700070321     c* secondo caso occorre controllarlo che viene emesso il video in
071800070321     c* quanto le note AUT possono essere inserite anche tramite l'interro-
071900070321     c* gazione bolle e si tratta coumunque di una condizione dinamica al
072000070321     c* contrario della prima che dipende dalla causale contatto richiesta
072100070321     c* QUINDI: Controllo qui solo il secondo caso
072200070412     c                   setoff                                       65
072300070321     c     karn          setll     fiarn11l
072400070321     c                   if        %equal(fiarn11l)
072500070412     c                   seton                                        65
072600070321     c                   endif
072700070321     c                   endsr
072800070315     c**********************************************************************
072900070315     c* Controlli video D01.
073000070315     c**********************************************************************
073100070315     C     Ctrd01        BEGSR
073200070315      *
073300070329     c                   setoff                                       404142
073400141030     c                   setoff                                       4348
073500070329     c* ? su altre gestioni
073600070329     c                   if        v2cges='?'
073700140130     c                   clear                   fnlrg7ds
073800140130     c                   eval      ilrg7aas=ilrg2aas
073900140130     c                   eval      ilrg7lnp=ilrg2lnp
074000140130     c                   eval      ilrg7nrs=ilrg2nrs
074100140130     c                   eval      ilrg7nsp=ilrg2nsp
074200140130     c                   eval      ilrg7vsdet='?'
074300140130     c                   call      'FNLRG7R'
074400140130     c                   parm                    fnlrg7ds
074500070329     c                   clear                   v2cges
074600070329     c                   seton                                        90
074700070329     c                   goto      endct8
074800070329     c                   endif
074900130515     c* ? su particolarità
075000130515     c                   if        v2igga='?'
075100130515     C                   CLEAR                   DS7PQRS
075200130515     C                   MOVEL     'S'           DS7RIC
075300130515     C                   MOVEL     'V'           DS7GES
075400130515     C                   MOVEL     V2CGGA        DS7COD
075500130515     C                   MOVEL     DS7PQRS       KPJBU
075600130515     C                   CALL      'TRTB69R'
075700130515     C                   PARM                    KPJBA
075800130515     c                   clear                   v2igga
075900130515     c                   seton                                        90
076000130515     c                   goto      endct8
076100130515     c                   endif
076200130515
076300130515     c                   if        v2igma='?'
076400130515     C                   CLEAR                   DS7PQRS
076500130515     C                   MOVEL     'S'           DS7RIC
076600130515     C                   MOVEL     'V'           DS7GES
076700130515     C                   MOVEL     V2cgma        DS7COD
076800130515     C                   MOVEL     DS7PQRS       KPJBU
076900130515     C                   CALL      'TRTB70R'
077000130515     C                   PARM                    KPJBA
077100130515     c                   clear                   v2igma
077200130515     c                   seton                                        90
077300130515     c                   goto      endct8
077400130515     c                   endif
077500130515
077600130515     c                   if        v2igva='?'
077700130515     C                   CLEAR                   DS7PQRS
077800130515     C                   MOVEL     'S'           DS7RIC
077900130515     C                   MOVEL     'V'           DS7GES
078000130515     C                   MOVEL     V2cgva        DS7COD
078100130515     C                   MOVEL     DS7PQRS       KPJBU
078200130515     C                   CALL      'TRTB73R'
078300130515     C                   PARM                    KPJBA
078400130515     c                   clear                   v2igva
078500130515     c                   seton                                        90
078600130515     c                   goto      endct8
078700130515     c                   endif
078800070703     c* Causale per variazione bolla
078900070703    1C     v2ccvb        IFNE      *BLANKS
079000070703     C*
079100070703     C     '?'           SCAN      v2ccvb                                 80
079200070703     C* . . . Ricerca
079300070703    2C     *IN80         IFEQ      *ON
079400070703     c                   clear                   fnlr66ds
079500070703     C                   MOVEL     ilrg2nsp      D66NSP
079600070703     C                   MOVEL     ilrg2aas      D66AAS
079700070703     C                   MOVEL     ilrg2lnp      D66LNP
079800070703     C                   MOVEL     ilrg2nrs      D66NRS
079900070703     C                   MOVEL     'V'           D66GES
080000070703     C                   MOVEL     'A'           D66TBO
080100070703     C*
080200070703     C                   MOVEL     fnlr66ds      KPJBU
080300070703     C                   CALL      'FNLR66R'
080400070703     C                   PARM                    KPJBA
080500070703     C                   MOVEL     KPJBU         fnlr66ds
080600070703     C                   MOVEL     D66CVB        V2ccvb
080700070703     c                   seton                                        90
080800070703     C                   GOTO      ENDCT8
080900070703   X2C                   ELSE
081000070703     C* . . . Controllo
081100070703     C                   CLEAR                   fnlr66ds
081200070703     C                   MOVEL     'C'           D66GES
081300070703     C                   MOVEL     'A'           D66TBO
081400070703     C                   MOVEL     V2ccvb        D66CVB
081500070703     C*
081600070703     C                   MOVEL     fnlr66ds      KPJBU
081700070703     C                   CALL      'FNLR66R'
081800070703     C                   PARM                    KPJBA
081900070703     C                   MOVEL     KPJBU         fnlr66ds
082000070703     C                   MOVEL     D66CVB        V2ccvb
082100070703     C*
082200070703     C* . . . Errore
082300070703    3C     D66MSG        IFNE      *BLANKS
082400070703     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
082500070703     C                   SETON                                        284390
082600070703     C                   GOTO      ENDCT8
082700070703    3C                   ENDIF
082800070703    2C                   ENDIF
082900070703     C*
083000070703     C* Richiamo pgm di variazione se impostato il codice
083100070703     C*
083200070703     C                   CLEAR                   fnlr48ds
083300070703     C                   z-add     ilrg2aas      D48AAS
083400070703     C                   z-add     ilrg2lnp      D48LNP
083500070703     C                   z-add     ilrg2nrs      D48NRS
083600070703     C                   z-add     ilrg2nsp      D48NSP
083700070703     c* come p.o. gestione passo il gestore della lna
083800070703     c                   clear                   fnlv55ds
083900070703     c                   z-add     arblna        d55lin
084000070703     c                   movel     '6'           d55tpt
084100070703     C                   time                    wrkoggiiso
084200070703     C     *iso          move      wrkoggiiso    d55drf
084300070703     c                   call      'FNLV55R'
084400070703     c                   parm                    fnlv55ds
084500070703     c                   movel     d55tfa        d48fgs
084600070703     C                   MOVEL     ARBCBO        D48CBO
084700070703     C                   MOVEL     V2ccvb        D48CVB
084800070703     C                   MOVEL     'A'           D48TBO
084900070703     C                   MOVEL     'S'           D48F12
085000070703     C                   MOVEL     'V'           D48FFR
085100070703     C                   MOVEL     fnlr48ds      KPJBU
085200070703     C                   CALL      'FNLR48R'
085300070703     C                   PARM                    KPJBA
085400070703     C                   PARM                    DSARBD
085500070703     C                   PARM                    DSARBK
085600070703     C                   PARM                    DARBT
085700070703     C                   PARM                    DSARBG
085800070703     C                   PARM                    TRUL90DS
085900070703     c                   clear                   v2ccvb
086000070709     c                   seton                                        90
086100070709     c                   goto      endct8
086200070703     C*
086300070703     c                   endif
086400070702     c
086500070329     c* TIPO INCASSO solo se non è protetto (causale contrassegno)
086501160705     c*              e se richiesta abilitazione
086600160705    1c                   if        *in11='0' and
086601160705    1c                               (v2cESI = 'A' or
086602160705    1c                               (*inkj='1' and v2cesi = *blanks))
086700070329     c* Richiamo pgm per controllo tipo incasso e divisa C/A
086800070329     c                   clear                   trulticds
086900070329     c                   clear                   ds1a
087000070329     c                   eval      iticTBO='A'
087100070329     c                   eval      iticlnp=arblnp
087200070329     c                   eval      iticlna=arblna
087300070329     c                   eval      iticnzd=arbnzd
087400090707     c                   eval      iticprd=arbprd
087500090707     c                   eval      iticcad=arbcad
087600090707     c                   eval      iticlod=arblod
087700090707     c                   eval      iticind=arbind
087800090707     c                   eval      iticrsd=arbrsd
087900070621     c                   movel     arbaas        iticdsp
088000070621     c                   move      arbmgs        iticdsp
088100091216     c                   if        s_ar5cdi<>*blanks
088200091216     C                   MOVEL     s_ar5cdi      iticccm
088300091216     c                   else
088400130827    2C***  ARBCCM        IFGT      0
088500130827     C***                MOVEL     ARBCCM        iticccm
088600130827     C***                ELSE
088700130827     C***                MOVEL     ARBKSC        iticccm
088800130827    2C***                END
088900130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
089000130827     c                   if        %subst(§3atb1:1:1)='F'
089100130827     c                   z-add     arbksc        iticccm
089200130827     c                   else
089300130827     c                   z-add     arbccm        iticccm
089400130827     c                   if        iticccm=0
089500130827     c                   z-add     8888          iticccm
089600130827     c                   movel     arblnp        iticccm
089700130827     c                   endif
089800130827     c                   endif
089900091216     c                   endif
090000070329     c                   eval      itictic=v2ctic
090100070329     c                   eval      iticticbol=ar9tic
090200070329     c                   eval      iticdiv=ar9vca
090300070329     c                   eval      iticcas=v2ccas
090400160510     c* il nome del pgm è importante perchè esegue un controllo sui contanti  SOLO QUI!!
090500160510     c*  quindi non lo prendo da SDS ma lo riempo con EVAL
090600160510     c                   eval      iticpgm='FNLRG2R '
090700070329     c                   call      'TRULTICR'
090800070329     c                   parm                    TRULTICDS
090900070329     c                   parm                    ds1a
091000070329     c
091100070329    2c                   if        oticric='T'
091200070329     c                   movel     otictic       v2ctic
091300070329     C                   SETON                                        4190
091400070329     C                   GOTO      ENDCT8
091500070329    2c                   endif
091600070329     c* Errore tipo incasso
091700091211    2c                   if        oticerr='T' or oticerr='C'
091800070329     C                   MOVEL     oticmsg       VIDMSG
091900070329     C                   SETON                                        419028
092000070329     C                   GOTO      ENDCT8
092100070329    2C                   ENDIF
092200070329     c* Errore divisa
092300070329    2c                   if        oticerr='D'
092400070329     C                   MOVEL     oticmsg       VIDMSG
092500070329     C                   SETON                                        419028
092600070329     C                   GOTO      ENDCT8
092700070329    2C                   ENDIF
092800070329     c*
092900070329    2c                   if        v2ctic<>pre_tic
093000070329     c                   clear                   forzacas
093100070329     c                   eval      pre_tic=v2ctic
093200091216     c                   setoff                                       15
093300091216     c* aggiorno "intestare a" a video nel caso fosse cambiato il tipo incasso C/A
093400091216    3c                   if        dateu>=§VPOCAS
093500091216     c                   clear                   trulintds
093600091216     c                   eval      iinttic=v2ctic
093700091216     c                   eval      iinttip='I'
093800091216     c                   eval      IINTRSCM=arbrsm
093900091216     c                   eval      IINTRSCO=arbrmo
094000091216     c                   eval      IINTcdi=s_ar5cdi
094100091216     c                   call      'TRULINTR'
094200091216     c                   parm                    trulintds
094300091216    4c                   if        ointerr=*blanks
094400091216     c                   seton                                        15
094500091216     c                   eval      v2ccdi=ointrsci
094600091216    4C                   endif
094700091216     c                   seton                                        90
094800091216     c                   goto      endct8
094900091216    3c                   endif
095000070329    2c                   endif
095100070329     c
095200070329      * Controllo il c/assegno tramite il trul21r
095300070329     c                   clear                   trul21ds
095400070329     c                   eval      i21cbo = arbcbo
095500070329     c                   eval      i21tsp = arbtsp
095600070329     c                   eval      i21lnp = arblnp
095700070329     c                   eval      i21nzm = arbnzm
095800070329     c                   eval      i21lna = arblna
095900070329     c                   eval      i21nzd = arbnzd
096000130827     c******             eval      i21ksc = arbksc
096100070329     c                   movel     arbctr        i21ctr
096200070329     c                   eval      i21tic = v2ctic
096300070329     c                   eval      i21imp = v2ccas
096400070329     c                   eval      i21div = v2cvca
096500070329     c                   eval      i21ute = knmus
096600070329     c                   eval      i21pgm = n_pgm
096700130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
096800130827     c                   if        %subst(§3atb1:1:1)='F'
096900130827     c                   z-add     arbksc        i21ksc
097000130827     c                   else
097100130827     c                   z-add     arbccm        i21ksc
097200130827     c                   if        i21ksc=0
097300130827     c                   z-add     8888          i21ksc
097400130827     c                   movel     arblnp        i21ksc
097500130827     c                   endif
097600130827     c                   endif
097700070329     c                   call      'TRUL21R'
097800070329     c                   parm                    trul21ds
097900070329      * controllo che non sia minore del limite previsto
098000070329    2c                   if        o21fmn <> *blanks
098100070329     c                   eval      vidmsg = msg(7)
098200070329     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
098300070329     c                   eval      *in41 = *on
098400070329     c                   eval      *in90 = *on
098500070329     c                   eval      *in28 = *on
098600070329     c                   goto      endct8
098700070329    2c                   endif
098800070329      * controllo che non sia maggiore del limite previsto
098900070329      * non forzabile
099000070329    2c                   if        o21fx2 <> *blanks
099100070329     c                   eval      vidmsg = msg(9)
099200070329     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
099300070329     c                   eval      *in41 = *on
099400070329     c                   eval      *in90 = *on
099500070329     c                   eval      *in28 = *on
099600070329     c                   goto      endct8
099700070329    2c                   endif
099800070329      * controllo che non sia maggiore del limite per bancanri bartolini no
099900070329      * abilitati da sede, non forzabile
100000070329    2c                   if        o21fb <> *blanks
100100070329     c                   eval      vidmsg = msg(10)
100200070329     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
100300070329     c                   eval      *in41 = *on
100400070329     c                   eval      *in90 = *on
100500070329     c                   eval      *in28 = *on
100600070329     c                   goto      endct8
100700070329    2c                   endif
100800070329      * controllo che non sia maggiore del limite previsto
100900070329      * forzabile
101000070329    2c                   if        o21fx1 <> *blanks and forzacas = *blanks
101100070329     c                   eval      forzacas = '1'
101200070329     c                   eval      vidmsg = msg(8)
101300070329     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
101400070329     c                   eval      *in41 = *on
101500070329     c                   eval      *in90 = *on
101600070329     c                   eval      *in28 = *on
101700070329     c                   goto      endct8
101800070329    2c                   endif
101900070329    1c                   endif
102000070329     c* DATA CONSEGNA RICHIESTA:richiamo fnlr48r
102100141030    1c                   if        v2cdcr>0 or v2chcr>0
102200070329     c                   clear                   v2cdcr_i
102300070329     c* controllo formale data
102400141030     c                   if        v2cdcr>0
102500070329     c                   clear                   wlbda8
102600070329     C                   Z-ADD     v2cdcr        G02DAT
102700070329     C                   MOVEL     *BLANK        G02ERR
102800070329     C                   CALL      'XSRDA8'
102900070329     C                   PARM                    WLBDA8
103000070329    2c     g02err        ifeq      '1'
103100070329     c                   seton                                        409028
103200070329     c                   eval      vidmsg=msg(2)
103300070329     c                   goto      endct8
103400070329    2c                   endif
103500070329     c                   z-add     g02inv        v2cdcr_i
103600070329     c                   z-add     g02dat        v2cdcr
103700141030     c                   endif
103800070329     c                   clear                   fnlr48ds
103900070329     c                   eval      d48cvb='CR'
104000070329     c                   eval      d48ffr='F'
104100070329     C                   z-add     ilrg2aas      D48AAS
104200070329     C                   z-add     ilrg2lnp      D48LNP
104300070329     C                   z-add     ilrg2nrs      D48NRS
104400070329     C                   z-add     ilrg2nsp      D48NSP
104500070329     C                   MOVEL     ARBCBO        D48CBO
104600070329     C                   MOVEL     'A'           D48TBO
104700070329     C                   MOVEL     'G'           D48trc
104800070329     C                   MOVEL     fnlr48ds      KPJBU
104900070329     c                   movel     v2ctcr        §BGTCR
105000070329     c                   z-add     v2chcr        §BGHCR
105100070329     c                   z-add     v2cdcr_I      §BGDCR
105200070329     c                   movel     arbgc1        §BGGC1
105300070329     c                   movel     arbgc2        §BGGC2
105400070329     c                   movel     arbtc1        §BGTC1
105500070329     c                   movel     arbtc2        §BGTC2
105600070329     c                   call      'FNLR48R'
105700070329     C                   PARM                    KPJBA
105800070329     C                   PARM                    DSARBD
105900070329     C                   PARM                    DSARBK
106000070329     C                   PARM                    DARBT
106100070329     C                   PARM                    DSARBG
106200070329     C                   PARM                    trul90ds
106300070329     C                   MOVEL     KPJBU         fnlr48ds
106400070329    2c                   if        d48err<>*blanks
106500070329     c                   movel     d48msg        vidmsg
106600070329     c                   seton                                        409028
106700070329     c                   goto      endct8
106800070329    2c                   endif
106900070329    1c                   endif
107000070329     c* ESITO - Se f10=abilitazione rapida l'f10 deve essere = *blanks
107100070329    1c                   if        *inkj='1' and v2cesi<>*blanks
107200070329     c                   eval      vidmsg=msg(3)
107300070329     c                   seton                                        429028
107400070329     c                   goto      endct8
107500070329     c                   else
107600070329     c* vicecersa esito obbligatorio per no f10
107700070329    2c                   if        *inkj='0' and v2cesi=*blanks
107800070329     c                   eval      vidmsg=msg(4)
107900070329     c                   seton                                        429028
108000070329     c                   goto      endct8
108100070329    2c                   endif
108200070329    1c                   endif
108300070329     c*
108400070329    1c                   if        v2cesi<>*blanks
108500070329     c                   clear                   werr              1
108600070329    2c                   select
108700070329     c                   when      ilrg2tss='S'
108800070329    3c                   if        v2cesi<>'D'
108900070329     c                   eval      werr='1'
109000070329    3c                   endif
109100070329     c                   when      ilrg2cau='S' or ilrg2cau='C' or
109200070410     c                             ilrg2cau='L' or ilrg2cau='A'
109300070329    3c                   if        v2cesi<>'A' and v2cesi<>'R' and v2cesi<>
109400070329     C                             'E'
109500070329     c                   eval      werr='1'
109600070329    3c                   endif
109700070329     c* per il lasciato avviso se inserita data consegna richiesta l'esito
109800070329     c* deve essere per forza 'A'
109900070329    3c                   if        ilrg2cau='L' and v2cesi<>'A'
110000070403     c                             and v2cdcr>0
110100070329     c                   seton                                        429028
110200070329     c                   eval      vidmsg=msg(6)
110300070329     c                   goto      endct8
110400070329    3c                   endif
110500070410     c*                  when      ilrg2cau='A'
110600070410    3c*                  if        v2cesi<>'A' and v2cesi<>'R'
110700070410     c*                  eval      werr='1'
110800070410    3c*                  endif
110900070329     c                   when      ilrg2cau='F'
111000070329   3 c                   if        v2cesi<>'C' and v2cesi<>'R' and v2cesi<>
111100070329     c                             'E'
111200070329     c                   eval      werr='1'
111300070329   3 c                   endif
111400070329   2 c                   endsl
111500070329   2 c                   if        werr='1'
111600070329     c                   seton                                        429028
111700070329     c                   eval      vidmsg=msg(5)
111800070329     c                   goto      endct8
111900070329   2 c                   endif
112000070329   1 c                   endif
112100070329     c*
112200070329     c* se richiesta abilitazione rapida errore se immessi o variati dati
112300070329    1c                   if        *inkj='1' and
112400070329     c                             (v2cnot8<>v2hnot8 or v2cnot9<>v2hnot9
112500070330     c                             or v2cdcr>0 or v2ctic<>s_v2ctic)
112600070329     c                   seton                                        429028
112700070329     c                   eval      vidmsg=msg(11)
112800070329     c                   goto      endct8
112900070329    1c                   endif
113000141013     c* se richiesta conferma (F6 o f10) e presenti dispo da caricare --> errore
113100141013     c                   if        (*inkj='1' or *inkf='1') and OLRY09ESIT='1'
113200141013     c                   seton                                        429028
113300141013     c                   eval      vidmsg=msg(15)
113400141013     c                   goto      endct8
113500141013     c                   endif
113600070315      *
113700070329     C     endct8        ENDSR
113800070316     C*--- CHAIN TABEL00F --------------------------------------------*
113900070316     C     CTABEL        BEGSR
114000070316     C     KTAB          CHAIN     TABEL                              30
114100070316     c                   if        %found(tabel00f) and tblflg<> ' '
114200070316     C                   SETON                                        30
114300070316     C                   END
114400070316     C                   ENDSR
114500070614     c**********************************************************************
114600070614     c* Gestione window per immissione codice giacenza
114700070614     c**********************************************************************
114800070614     C     gesw01        BEGSR
114900070614     c     emw01         tag
115000070614     c                   exfmt     lrg2w01
115100070614     c                   setoff                                       2846
115200070614     c                   clear                   w2dcmc
115300070614     c                   clear                   widmsg
115400070712     c                   if        *inkl='1'
115500070712     c                   goto      e_emw01
115600070712     c                   endif
115700070614     C     '?'           SCAN      w2ccmc:1                               80
115800070712    1C     *IN80         IFEQ      *ON
115900070926     c                   clear                   TRTB2Ads
116000070926     c                   eval      TB2Aopz = '1'
116100070926     c                   eval      TB2Atip = 'G'
116200070926     c                   movel(p)  TRTB2Ads      KPJBU
116300070926     c                   call      'TRTB2AR'
116400070926     c                   parm                    KPJBA
116500070926     c                   movel     KPJBU         TRTB2Ads
116600070926     c                   movel     TB2Acod       W2Ccmc
116700070926     c                   movel     TB2Auni       ds2A
116800070926     c                   movel     §2Ades        W2Dcmc
116900070924     c                   write     lrg2t01
117000070924     c                   write     lrg2d01
117100070924     c                   goto      emw01
117200070712    1C                   endif
117300070614     c*
117400070712    1c                   if        w2ccmc=*blanks  and
117500070712     c                              (ilrg2cau <> 'L' and
117600070712     c                               ilrg2cau <> 'F')
117700070704     C                   SETON                                        2846
117800070704     c                   movel     msg(14)       widmsg
117900070704     c                   goto      emw01
118000070712    1c                   endif
118100070712      *
118200070712    1c                   if        w2ccmc <> *blanks
118300070712      *
118400070614     c                   clear                   ds2a
118500070614     c                   move      '2A'          tblcod
118600070614     c                   movel(p)  w2ccmc        tblkey
118700070614     C                   exsr      ctabel
118800070614     c* errore record non trovato
118900070614    2C                   if        *in30
119000070614     C                   SETON                                        2846
119100070614     c                   movel     msg(12)       widmsg
119200070614     c                   goto      emw01
119300070614    2c                   endif
119400070614     C                   MOVEL     tbluni        ds2a
119500070619     c                   movel     §2ades        w2dcmc
119600070614     c* errore codice giacenza non utilizzabile
119700081103    2c                   if        §2aftc='G'  and §2atip='G'  and
119800081103     c                             §2aflg=' '
119900081103    2c                   else
120000070614     C                   SETON                                        2846
120100070614     c                   movel     msg(13)       widmsg
120200070614     c                   goto      emw01
120300070614    2c                   endif
120400070712      *
120500070712    1c                   endif
120600070712      *
120700070703     c  nkf              goto      emw01
120800070712     c     e_emw01       endsr
120900070321     c**********************************************************************
121000070321     c* F6=Conferma
121100070321     c**********************************************************************
121200070321     C     Confe         BEGSR
121300070703     c* se immesso esito "E"=Esclusa da gestione-attesa giacenza emetto
121400070703     c* finestra per eventuale immissione codice giacenza
121500070704     c                   if        v2cesi='E'
121600070703     c                   exsr      gesw01
121700070712     c                   if        *inkl
121800070712     c                   seton                                        90
121900070712     c                   goto      f_confe
122000070712     c                   endif
122100070703     c                   endif
122200070328     c* DATA CONSEGNA RICHIESTA: E' il primo aggiornamento che faccio
122300070328     c* perchè il richiamo con "CS" deve essere fatto con emissione videata
122400070328     c* della manutenzione bolle dalla quale è possibile uscire con f12 ed
122500070328     c* in questo caso non devo fare nessun aggiornamento
122600070329    1C                   IF        V2CDCR>0
122700070328     c                   clear                   fnlr48ds
122800070330    2c                   if        arbtc1<>'S' and arbtc2<>'S'
122900070328     c                   eval      d48cvb='CR'
123000070328     c                   eval      d48ffr='E'
123100070328     c                   else
123200070328     c                   eval      d48cvb='CS'
123300070328     c                   eval      d48ffr='V'
123400070328     c                   eval      d48f12='S'
123500070329    2c                   endif
123600070328     C                   z-add     ilrg2aas      D48AAS
123700070328     C                   z-add     ilrg2lnp      D48LNP
123800070328     C                   z-add     ilrg2nrs      D48NRS
123900070328     C                   z-add     ilrg2nsp      D48NSP
124000070328     C                   MOVEL     ARBCBO        D48CBO
124100070328     C                   MOVEL     'A'           D48TBO
124200070328     C                   MOVEL     'S'           D48F12
124300070328     C                   MOVEL     'G'           D48trc
124400070328     C                   MOVEL     fnlr48ds      KPJBU
124500070328     c                   movel     v2ctcr        §BGTCR
124600070328     c                   z-add     v2chcr        §BGHCR
124700070328     c                   z-add     v2cdcr_I      §BGDCR
124800070328     c                   movel     arbgc1        §BGGC1
124900070328     c                   movel     arbgc2        §BGGC2
125000070328     c                   movel     arbtc1        §BGTC1
125100070328     c                   movel     arbtc2        §BGTC2
125200070328     c                   call      'FNLR48R'
125300070328     C                   PARM                    KPJBA
125400070328     C                   PARM                    DSARBD
125500070328     C                   PARM                    DSARBK
125600070328     C                   PARM                    DARBT
125700070328     C                   PARM                    DSARBG
125800070328     C                   PARM                    trul90ds
125900070328     C                   MOVEL     KPJBU         fnlr48ds
126000070328     c* Ritorno con errore o premuto f12
126100070329    2c                   if        d48err<>*blanks or d48f12='1'
126200070328     c                   seton                                        90
126300070329    3c                   if        d48err<>*blanks
126400070328     c                   seton                                        28
126500070328     c                   movel     d48msg        vidmsg
126600070329    3c                   endif
126700070328     c                   goto      f_confe
126800070329    2c                   endif
126900070329    1c                   endif
127000070322
127100070327     c*  N O T E ************
127200070322     c* Note di ar4 trk 8 e 9
127300070420    1c                   if        v2hnot8<>v2cnot8 or v2hnot9<>v2cnot9
127400131008     c* richaimo pgm di manutenzione bolle per aggiornamento delle note
127500131008     c                   clear                   fnlr48ds
127600131008     c                   eval      d48cvb='NT'
127700131008     c                   eval      d48ffr='E'
127800131008     C                   z-add     ilrg2aas      D48AAS
127900131008     C                   z-add     ilrg2lnp      D48LNP
128000131008     C                   z-add     ilrg2nrs      D48NRS
128100131008     C                   z-add     ilrg2nsp      D48NSP
128200131008     C                   MOVEL     ARBCBO        D48CBO
128300131008     C                   MOVEL     'A'           D48TBO
128400131008     C                   MOVEL     'S'           D48F12
128500131008     C                   MOVEL     'N'           D48trc
128600131008     C                   MOVEL     fnlr48ds      KPJBU
128700131008     c                   eval      §bnu_sn='S'
128800131008     c                   eval      §bnuni70=v2cnot8+v2cnot9
128900131008     c                   eval      §bntrd='NOT'
129000131008     c                   call      'FNLR48R'
129100131008     C                   PARM                    KPJBA
129200131008     C                   PARM                    DSARBD
129300131008     C                   PARM                    DSARBK
129400131008     C                   PARM                    DARBT
129500131008     C                   PARM                    DSARBG
129600131008     C                   PARM                    trul90ds
129700131008     C                   PARM                    DSARBN
129800131008     C                   MOVEL     KPJBU         fnlr48ds
129900131008
130000131008     c* Ritorno con errore o premuto f12
130100131008    2c                   if        d48err<>*blanks
130200131008     c                   seton                                        90
130300131008     c                   seton                                        28
130400131008     c                   movel     d48msg        vidmsg
130500131008     c                   goto      f_confe
130600131008    2c                   endif
130700131008    1c                   endif
130800131008     c
130900070322     c* verifico se presente ar4 trk "a"
131000131008     c**
131100131008     C**                 MOVEL     'a'           WTRC
131200131008     c**   kar4          setll     fiar401l
131300131008    2c**                 if        not %equal(fiar401l)
131400131008     c**                 clear                   fnlv19ds
131500131008     C**                 MOVEL     'T'           D19ftr
131600131008     C**                 MOVEL     'a'           D19trc
131700131008     C**                 MOVEL     arbAAS        D19AAS
131800131008     C**                 MOVEL     arbLNP        D19LNP
131900131008     C**                 MOVEL     arbNRS        D19NRS
132000131008     C**                 MOVEL     arbNSP        D19NSP
132100131008     C**                 MOVEL     V2hnot8       D19NT1
132200131008     C**                 clear                   D19NT2
132300131008     c**                 call      'FNLV19R'
132400131008     c**                 parm                    fnlv19ds
132500131008     c**                 parm      'S'           p19vuo            1
132600131008     c**
132700131008    3c**                 if        v2hnot9<>*blanks
132800131008     C**                 MOVEL     'b'           D19trc
132900131008     C**                 MOVEL(P)  V2hnot9       D19NT1
133000131008     C**                 clear                   D19NT2
133100131008     c**                 call      'FNLV19R'
133200131008     c**                 parm                    fnlv19ds
133300131008    3c**                 endif
133400131008    2c**                 endif
133500070327     c* aggiorno/deleto trk 8 e 9 se variati
133600131008     c**                 clear                   fnlv19ds
133700131008     C**                 MOVEL     'T'           D19ftr
133800131008     C**                 MOVEL     arbAAS        D19AAS
133900131008     C**                 MOVEL     arbLNP        D19LNP
134000131008     C**                 MOVEL     arbNRS        D19NRS
134100131008     C**                 MOVEL     arbNSP        D19NSP
134200131008     C**                 MOVEL     V2cnot8       D19NT1
134300131008     C**                 MOVEL     V2cnot9       D19NT2
134400131008    2C**   V2cnot8       IFNE      *BLANKS
134500131008     C**   V2cnot9       ORNE      *BLANKS
134600131008     C**                 MOVEL     '8'           D19TRC
134700131008     C**                 CALL      'FNLV19R'
134800131008     C**                 PARM                    fnlv19ds
134900131008    2C**                 ENDIF
135000131008     c**Se il pgm mi restituisce la seconda riga vuota la deleto perchè
135100131008     c**prima poteva esistere
135200131008    2C**   D19ERR        IFEQ      *BLANKS
135300131008     C**   D19NT1        ANDNE     *BLANKS
135400131008     C**   D19NT2        ANDEQ     *BLANKS
135500131008     C**                 MOVEL     '9'           D19TRC
135600131008     C**                 CLEAR                   D19NT1
135700131008     C**                 CLEAR                   D19NT2
135800131008     C**                 CALL      'FNLV19R'
135900131008     C**                 PARM                    fnlv19ds
136000131008   x2C**                 else
136100131008     C**Se tornato codice di err richiamo il pgm 2 volte:  una per nota
136200131008    3C**   D19ERR        IFNE      *BLANKS
136300131008     C**   v2cnot8       oreq      *BLANKS
136400131008     C**   v2cnot9       ANDEQ     *BLANKS
136500131008     C**Nota 1
136600131008     C**                 MOVEL     '8'           D19TRC
136700131008     C**                 MOVEL     v2cnot8       D19NT1
136800131008     C**                 CLEAR                   D19NT2
136900131008     C**                 CALL      'FNLV19R'
137000131008     C**                 PARM                    fnlv19ds
137100131008     C**Nota 2
137200131008     C**                 MOVEL     '9'           D19TRC
137300131008     C**                 MOVEL     v2cnot9       D19NT1
137400131008     C**                 CLEAR                   D19NT2
137500131008     C**                 CALL      'FNLV19R'
137600131008     C**                 PARM                    fnlv19ds
137700131008    3C**                 ENDIF
137800131008    2C**                 ENDIF
137900131008    1c**                 endif
138000131008     c
138100131008     c* ALLOCO PRINCIPALI RECORD
138200131008     c                   exsr      alloc
138300131008    1c                   if        *in90='1'
138400131008     c                   goto      f_confe
138500131008    1c                   endif
138600131008     c
138700070328     c* TIPO INCASSO C/ASSEGNO
138800070330     c                   if        ilrg2cau='C' and v2ctic<>s_v2ctic
138900070328     c                   movel     v2ctic        ar9tic
139000070328     C                   z-add     dateu         AR9duv
139100070328     C                   UPDATE    FiAR9000
139200091217     c* Aggiorno FIAR5 recrord GEN
139300091217     c                   exsr      RegisAr5GEN
139400070328     c                   endif
139500070327     c* SCRITTURA NOTE R.A.
139600070330     c                   exsr      sr_noteRa
139700070328     c* ESITO
139800070328     c                   select
139900070328     c* abilitato
140000070328     c                   when      v2cesi='A'
140100070328     c                   select
140200070328     c                   when      ilrg2cau='S' or ilrg2cau='A'
140300070328     c                   clear                   §b4lbas
140400070328     c                   when      ilrg2cau='C'
140500070328     c                   clear                   §b4ltca
140600070328     C                   MOVEL     'S'           ARBACC
140700070328     c                   when      ilrg2cau='L'
140800070419     c     karb          setgt     fnevb22l
140900070419     c     karb          readpe    fnevb22l
141000070419     c                   if        not %eof(fnevb22l)
141100070328     c                   clear                   evb2tel
141200070328     c                   update    fnevb200
141300070328     c                   endif
141400070402     c* Devo cliearare anche arbfbc se non ho richiamato fnlr48r
141500070402     c                   if        v2cdcr=0
141600070402     c                   clear                   arbfbc
141700070419     c                   clear                   arbcmc
141800070402     c                   endif
141900070328     c                   endsl
142000070328     c
142100070328     c* contattato (per fermo deposito)
142200070328     c                   when      v2cesi='C'
142300070328     c                   clear                   §b4ltfd
142400070328     c* da ricontattare
142500070328     c                   when      v2cesi='R'
142600070328     c                   select
142700070328     c                   when      ilrg2cau='S' or ilrg2cau='A'
142800070328     c                   eval      §b4lbas='T'
142900070328     c                   when      ilrg2cau='C'
143000070328     c                   eval      §b4ltca='T'
143100070328     c                   when      ilrg2cau='L'
143200070419     c     karb          setgt     fnevb22l
143300070419     c     karb          readpe    fnevb22l
143400070419     c                   if        not %eof(fnevb22l)
143500070328     c                   eval      evb2tel='T'
143600070328     c                   update    fnevb200
143700070328     c                   endif
143800070328     c                   when      ilrg2cau='F'
143900070328     c                   eval      §b4ltfd='T'
144000070328     c                   endsl
144100070328     c* esclusa da gestione-attesa giacenza
144200070328     c                   when      v2cesi='E'
144300070328     c                   select
144400070410     c                   when      ilrg2cau='S' or ilrg2cau='A'
144500070328     c                   eval      §b4lbas='N'
144600070328     c                   when      ilrg2cau='C'
144700070328     c                   eval      §b4ltca='N'
144800070328     c                   when      ilrg2cau='L'
144900070419     c     karb          setgt     fnevb22l
145000070419     c     karb          readpe    fnevb22l
145100070419     c                   if        not %eof(fnevb22l)
145200070328     c                   eval      evb2tel='N'
145300070328     c                   update    fnevb200
145400070328     c                   endif
145500070328     c                   when      ilrg2cau='F'
145600070328     c                   eval      §b4ltfd='N'
145700070328     c                   endsl
145800070615     c                   if        w2ccmc<>*blanks
145900070615     c                   movel     'G'           arbfbc
146000070615     c                   movel     w2ccmc        arbcmc
146100070615     c* scrivo evento
146200070615     c                   exsr      sr_evb
146300070615     c                   endif
146400070328     c* da contattare
146500070328     c                   when      v2cesi='D'
146600070328     c                   eval      §b4lbas='S'
146700070328     c                   endsl
146800070329     c* Rilascio e aggiorno ar4 trk "L"
146900070329     c                   exsr      aggar4l
147000070328     c* Rilascio e aggiorno il record bolla
147100070402     c* Abilitazione contrassegno (e lasciato avviso se non richiamato fnlr48R)
147200070615     c                   if        (v2cesi='A' and (ilrg2cau='C' or (
147300070615     c                             ilrg2cau='L' and v2cdcr=0))) or
147400070615     c                             (v2cesi='E' and w2ccmc<>*blanks)
147500070328     C                   update    fnarb000
147600070328     c                   else
147700070621     c                   unlock    fnarb00f
147800070328     c                   endif
147900070328     c* GESTIONE NOTE LDV (Tengo alla fine questo aggiornamento:è meno
148000070328     c* importante e siccome potrebbe potenzialmente dare errore di allocaz.
148100070328     c* in fase di update di fiarn per trasformare i record come storici
148200070328     c* lo tengo alla fine)
148300070328     c****               eval      v2cnra=v2cnt1+v2cnt2+v2cnt3+v2cnt4
148400070328     c                   if        v2cnra<>*blanks
148500140130     c                   clear                   fnlrn6ds
148600140130     c                   movel     'I'           IN6MOD
148700140130     c                   z-add     arbaas        IN6AAS
148800140130     c                   z-add     arblnp        IN6LNP
148900140130     c                   z-add     arbnrs        IN6NRS
149000140130     c                   z-add     arbnsp        IN6NSP
149100140130     c                   movel     N_pgm         IN6PGM
149200140130     c                   movel     v2cnt1        IN6NOt1
149300140130     c                   movel     v2cnt2        IN6NOt2
149400140130     c                   movel     v2cnt3        IN6NOt3
149500140130     c                   movel     v2cnt4        IN6NOt4
149600140130     c                   call      'FNLRN6R'
149700070329     c                   parm                    kpjba
149800140130     c                   parm                    fnlrn6ds
149900070328     c                   endif
150000070327     c
150100070322     c
150200070322     c
150300070322     c     f_confe       endsr
150400070321     c**********************************************************************
150500070321     c* F10=Abilitazione rapida
150600070321     c**********************************************************************
150700070321     C     Rapida        BEGSR
150800070329     c* ALLOCO I PRINCIPALI  RECORD IN AGGIORNAMENTO
150900070329     c                   exsr      alloc
151000070329     c                   if        *in90='1'
151100070329     c                   goto      frapida
151200070329     c                   endif
151300070330     c* NOTE R.A.
151400070330     c                   exsr      sr_noteRa
151500070329     c* ESITO
151600070329     c* abilitato/contattato
151700070329     c                   select
151800070329     c                   when      ilrg2cau='S' or ilrg2cau='A'
151900070329     c                   clear                   §b4lbas
152000070329     c                   when      ilrg2cau='C'
152100070329     c                   clear                   §b4ltca
152200070329     C                   MOVEL     'S'           ARBACC
152300070329     c                   when      ilrg2cau='L'
152400070419     c     karb          setgt     fnevb22l
152500070419     c     karb          readpe    fnevb22l
152600070419     c                   if        not %eof(fnevb22l)
152700070329     c                   clear                   evb2tel
152800070329     c                   update    fnevb200
152900070329     c                   endif
153000070329     c                   clear                   arbfbc
153100070419     c                   clear                   arbcmc
153200070329     c                   when      ilrg2cau='F'
153300070329     c                   clear                   §b4ltfd
153400070329     c                   endsl
153500070329     c                   exsr      aggar4l
153600070329     c                   if        ilrg2cau='C' or ilrg2cau='L'
153700070329     c                   update    fnarb000
153800070329     c                   else
153900070621     c                   unlock    fnarb00f
154000070329     c                   endif
154100070329     c     frapida       endsr
154200070330     c**********************************************************************
154300070330     c* Scrittura Note R.A.
154400070330     c**********************************************************************
154500070330     C     sr_noteRa     BEGSR
154600140508     c                   clear                   fidna6ds
154700140508     C                   CLEAR                   DTGD01
154800070330    1c                   select
154900070330     c                   when      ilrg2cau='L'
155000140508     c                   eval      ida06mad='  2'
155100070330     c                   when      ilrg2cau='C'
155200140508     c                   eval      ida06mad=' 10'
155300070330     c                   when      ilrg2cau='S'
155400140508     c                   eval      ida06mad=' 14'
155500070330     c                   when      ilrg2cau='A'
155600140508     c                   eval      ida06mad=' 15'
155700070330     c                   when      ilrg2cau='F'
155800140508     c                   eval      ida06mad=' 16'
155900070330    1c                   endsl
156000140508     c                   eval      ida06tor='S'
156100140508     c                   eval      ida06ogg=%editc(arblnp:'X')+
156200140508     c                             %editc(arbnrs:'X')+
156300070330     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
156400140508     c                   eval      ida06no1=v2cnt1+v2cnt2+v2cnt3+v2cnt4
156500140508     c* Impostazione campo ida06no2:elenco delle operazioni effettuate
156600070330     c* esito
156700070330    1c                   select
156800070417     c                   when      v2cesi='A'
156900140508     c                   eval      ida06no2='Esito:abilitata.'
157000070417     c                   when      *inkj
157100140508     c                   eval      ida06no2='Esito:abilitazione rapida'
157200070330     c                   when      v2cesi='C'
157300140508     c                   eval      ida06no2='Esito:contattato.'
157400070330     c                   when      v2cesi='R'
157500140508     c                   eval      ida06no2='Esito:da ricontattare.'
157600070330     c                   when      v2cesi='D'
157700140508     c                   eval      ida06no2='Esito:da contattare.'
157800070712     c                   when      v2cesi='E' and w2ccmc<>*blanks
157900140508     c                   eval      ida06no2=
158000140508     c                             'Esito:Esclusa gest.- apertura giac.'
158100070712     c                   when      v2cesi='E'
158200140508     c                   eval      ida06no2=
158300140508     c                             'Esito:Esclusa da gest.-attesa giac.'
158400070330    1c                   endsl
158500070330     c* dcr
158600070330    1c                   if        v2cdcr>0
158700070330     c                   clear                   fnlv80ds
158800070330     c                   eval      ilv80aas=arbaas
158900070330     c                   eval      ilv80lnp=arblnp
159000070330     c                   eval      ilv80nrs=arbnrs
159100070330     c                   eval      ilv80nsp=arbnsp
159200070330     c                   eval      ilv80mgs=arbmgs
159300070330     c                   eval      ilv80tcr=v2ctcr
159400070330     c                   eval      ilv80dcr=v2cdcr_i
159500070410     c                   eval      ilv80hcr=v2chcr
159600070330     c                   eval      ilv80tc1=arbtc1
159700070330     c                   eval      ilv80tc2=arbtc2
159800070330     c                   call      'FNLV80R'
159900070330     c                   parm                    kpjba
160000070330     c                   parm                    fnlv80ds
160100070330     c                   eval      W_dtcr=olv80tcrd
160200140508     c                   eval      %subst(ida06no2:36:10)='Cons.Rich:'
160300140508     c                   eval      ida06no2=%trim(ida06no2)+%trim(w_dtcr)
160400140508     c                   eval      ida06no2=%trim(ida06no2)+' '+
160500070330     c                             %editw(v2cdcr:'  /  /    ')
160600070330    2c                   if        v2chcr>0
160700140508     c                   eval      ida06no2=%trim(ida06no2)+' ore'
160800070330    3c                   if        v2ctcr=*blanks
160900140508     c                   eval      ida06no2=%trim(ida06no2)+' '+
161000070330     c                             %editw(v2chcr:'  :  ')
161100070330     c                   else
161200140508     c                   eval      %subst(ida06no2:71:5)=
161300070330     c                             %editw(v2chcr:'  :  ')
161400070330    3c                   endif
161500070330    2c                   endif
161600070330    1c                   endif
161700070330     c* Tipo incasso
161800070330    1c                   if        v2ctic<>s_v2ctic
161900070615     c                   exsr      sr_cerpos
162000070615     c                   if        n<>999
162100140508     c                   eval      %subst(ida06no2:N:23)=
162200140508     c                             'Variato Tp.inc. C/A da '
162300140508     c                   eval      ida06no2=%trim(ida06no2)+
162400070330     c                             '"'+S_v2ctic+'" in "'+v2ctic+'"'
162500070615     c                   endif
162600070330    1c                   endif
162700070615     c* codice giacenza
162800070620     c                   if        v2cesi='E' and w2ccmc<>*blanks
162900070615     c                   exsr      sr_cerpos
163000070615     c                   if        n<>999
163100070619     c                   movel     w2dcmc        w2dcmc22         22
163200140508     c                   eval      %subst(ida06no2:N:9)='Cd.giac.:'
163300140508     c                   eval      ida06no2=%trim(ida06no2)+
163400070620     c                             w2ccmc+'-'+w2dcmc22
163500070615     c                   endif
163600070615     c                   endif
163700090918     c* flag per segnalazione abilitazione rapida
163800140508     c   kj              eval      §tgdarap='S'
163900140508     c  nkj              eval      §tgdarap='C'
164000141003     c****               eval      dtgd01= Ida06flo
164100141003     c                   eval      Ida06flo = dtgd01
164200070330     c
164300140508     c                   eval      ida06ute=knmus
164400140508     c                   eval      ida06fil=dutpou
164500140508
164600070330    1c                   if        ilrg2tch<>'R'
164700140508     c                   call      'FIDNA6R'
164800140508     c                   parm                    fidna6ds
164900070330    1c                   endif
165000140508     c                   movel     ida06no1      olrg2no1
165100140508     c                   movel     ida06no2      olrg2no2
165200070330     c                   endsr
165300070615     c**********************************************************************
165400070615     C     SR_CERPOS     BEGSR
165500070615     c**********************************************************************
165600140508     c     ' '           checkr    ida06no2      N                 4 0
165700070615    2c                   select
165800070615     c                   when      N<=35
165900070615     c                   z-add     36            n
166000070615     c                   when      N<=70
166100070615     c                   z-add     71            n
166200070615     c                   when      N<=105
166300070615     c                   z-add     106           n
166400070615     c                   other
166500070615     c                   z-add     999           n
166600070615    2c                   endsl
166700070615     C                   ENDSR
166800070329     c**********************************************************************
166900070329     C     ALLOC         BEGSR
167000070329     c**********************************************************************
167100070329     c* ALLOCO I PRINCIPALI  RECORD IN AGGIORNAMENTO
167200070621     c     savnrr        chain(E)  fnarb00f
167300070329     c                   if        %error
167400070329     c                   seton                                        9028
167500070329     c                   eval      vidmsg=msg(1)
167600070329     c                   goto      f_alloc
167700070329     c                   endif
167800070329     c                   if        ilrg2cau<>'L'
167900070329     c                   if        sav4nrr>0
168000070329     c     sav4nrr       chain(E)  fiar400f
168100070329     c                   if        %error
168200070329     c                   eval      vidmsg=msg(1)
168300070329     c                   seton                                        9028
168400070329     c                   goto      f_alloc
168500070329     c                   endif
168600070329     c                   movel     F_ar4not      dsbl4l
168700070329     c                   else
168800070329     c                   clear                   dsbl4l
168900070329     c                   endif
169000070329     c                   endif
169100070330     c                   if        ilrg2cau='C'and v2ctic<>s_v2ctic
169200070329     c     karb          chain(E)  fiar901l
169300070329     c                   if        %error
169400070329     c                   eval      vidmsg=msg(1)
169500070329     c                   seton                                        9028
169600070329     c                   goto      f_alloc
169700070329     c                   endif
169800070329     c                   endif
169900070329     c     f_alloc       endsr
170000070329     c**********************************************************************
170100070329     C     AGGAR4L       BEGSR
170200070329     c**********************************************************************
170300070329     c* Rilascio e aggiorno ar4 trk "L"
170400070329     c                   if        ilrg2cau<>'L'
170500070329     c                   if        sav4nrr=0
170600070329     c                   clear                   fiar4000
170700070329     c                   z-add     ilrg2aas      f_ar4aas
170800070329     c                   z-add     ilrg2lnp      f_ar4lnp
170900070329     c                   z-add     ilrg2nrs      f_ar4nrs
171000070329     c                   z-add     ilrg2nsp      f_ar4nsp
171100070329     c                   movel     'L'           f_ar4trc
171200070329     c                   movel     'R'           f_ar4ftr
171300070329     c                   z-add     dateu         f_ar4dtr
171400070329     c                   endif
171500070329     c                   movel     dsbl4l        F_ar4not
171600070329     c                   z-add     dateu         F_ar4duv
171700070329     c                   if        sav4nrr=0
171800070329     c                   write     fiar4000
171900070329     c                   else
172000070329     c                   update    fiar4000
172100070329     c                   endif
172200070329     c                   endif
172300070329     c                   endsr
172400070411     c**********************************************************************
172500070411     c* Verifica abilitazione tasto F16-Gestione richieste assistenza
172600070411     c**********************************************************************
172700070411     C     SR_AbilF16    BEGSR
172800070411     c                   eval      TGDtor  = 'S'
172900070411     c                   eval      tgdogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
173000070411     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
173100070411     c     K02TGD02      setll     FITGD000
173200160404     c                   eval      *in64   = %equal(FITGD32L)
173300070411     c                   endsr
173400140922     c**********************************************************************
173500140922     c* Verifica abilitazione tasto F19-Indicazioni di Consegna da WEB
173600140922     c**********************************************************************
173700140922     C     SR_AbilF19    BEGSR
173800140922     c                   eval      *in63 = *off
173900141024     c                   clear                   v2dtxt1
174000141024     c                   clear                   v2dtxt2
174100141024     c                   clear                   fnlry09ds2
174200140922     c                   eval      ilry09tch='C'
174300140922     c                   eval      ILRY09AAS=ilrg2aas
174400140922     c                   eval      ILRY09LNP=ILRG2LNP
174500140922     c                   eval      ILRY09NRS=ILRG2NRS
174600140922     c                   eval      ILRY09NSP=ILRG2NSP
174700141024     c                   eval      kpjbu=fnlry09ds2
174800140922     c                   call      'FNLRY09R'
174900141013     c                   parm                    kpjba
175000141024     c                   eval      fnlry09ds2=kpjbu
175100141024     c                   if        olry09esit='1'
175200141024     c                   if        olry09esdi='0'
175300140922     c                   eval      *in63 = *on
175400141024     c                   eval      v2dtxt2='F19=Dispo Consegna in sospeso'
175500141024     c                   else
175600141030     c                   eval      v2dtxt1='Presente Dispo Consegna Dirot'
175700160202     c                   eval      v2dtxt2='t/Reso :non eseguire Contatto'
175800141024     c                   endif
175900140922     c                   endif
176000140922     c                   endsr
176100070618     c**********************************************************************
176200070618     c* Scrittura evento
176300070618     c**********************************************************************
176400070618     C     sr_evb        BEGSR
176500070621     c                   clear                   fnevbf00
176600070621     c* data/ora immissione evento
176700070621     c* data/ora evento
176800070621     c                   time                    w0140
176900070621     C                   MOVE      W0140         g02dat
177000070621     C                   MOVEL     *BLANK        G02ERR
177100070621     C                   CALL      'XSRDA8'
177200070621     C                   PARM                    WLBDA8
177300070621     C                   Z-ADD     G02INV        evbdtv
177400070621     c                   movel     w0140         evborv
177500070621     C                   Z-ADD     G02INV        evbdev
177600070621     c                   movel     w0140         evbhev
177700070621     c* filiale immissione evento
177800070621     c                   z-add     dutpou        evbfle
177900070621     c* codice utente video
178000070621     C                   Z-ADD     dutcou        EVBCDU
178100070621     c* key spedizione
178200070621     C                   clear                   VH2AAP
178300070621     C                   clear                   VH2LPP
178400070621     C                   clear                   VH2NRP
178500070621     C                   clear                   VH2NSP
178600070621     c* se bolla figlia memorizzo la spedizione della mamma
178700070621     c     karb          chain     fnlbl01l
178800070621     c                   if        %found(fnlbl01l) and lbllan=lbllap
178900070621     c                   movel     dsarb0        savarb
179000070621     C                   Z-ADD     LBLAAP        KAAS
179100070621     C                   Z-ADD     LBLLPP        KLNP
179200070621     C                   Z-ADD     LBLNRP        KNRS
179300070621     C                   Z-ADD     LBLNSP        KNSP
179400070621     c     karbp         chain     fnarb01l
179500070621     c                   if        %found(fnarb01l) and arbcca<>'2'
179600070621     c                             and arbcca<>'6'
179700070621     C                   Z-ADD     LBLAAP        VH2AAP
179800070621     C                   Z-ADD     LBLLPP        VH2LPP
179900070621     C                   Z-ADD     LBLNRP        VH2NRP
180000070621     C                   Z-ADD     LBLNSP        VH2NSP
180100070621     c                   endif
180200070621     c                   move      savarb        dsarb0
180300070621     c                   endif
180400070621     c                   if        vh2nsp>0
180500070621     C                   Z-ADD     VH2LPP        EVBLNP
180600070621     C                   Z-ADD     VH2AAP        EVBAAS
180700070621     C                   Z-ADD     VH2NRP        EVBNRS
180800070621     C                   Z-ADD     VH2NSP        EVBNSP
180900070621     c                   else
181000070621     C                   z-add     ilrg2aas      evbAAS
181100070621     C                   z-add     ilrg2lnp      evbLNP
181200070621     C                   z-add     ilrg2nrs      evbNRS
181300070621     C                   z-add     ilrg2nsp      evbNSP
181400070621     c                   endif
181500070621     c* codice evento
181600070621     c                   movel     w2ccmc        evbcev
181700070621     c                   write     fnevbf00
181800070618     c                   endsr
181900091217     c**********************************************************************
182000091217     c* Aggiornamento per record GEN ddel fiar5
182100091217     c**********************************************************************
182200091217     c     RegisAR5GEN   BEGSR
182300091217     c                   clear                   dar5gen
182400091217     c                   Eval      kAr5Trd = 'GEN'
182500091217     c     kfiar5        Chain     Fiar501l
182600091217      * Aggiorno Fiar5
182700091217if  1c                   If        %Found(Fiar501l)
182800091217     c                   movel     ar5uni        DAr5Gen
182900091217     c*
183000091217     c                   Eval      §Ar5ticmb=v2ctic
183100100119     c                   Eval      §Ar5flgmb='S'
183200091217     c                   Movel(p)  DAr5Gen       Ar5Uni
183300091217     c
183400091217      * se devo trasmettere alla sede sfleggo per la trasmissione
183500091217    2c                   If        §Ar5Trse = 'S'
183600091217     c                   Clear                   Ar5Ft2
183700091217    2c                   EndIf
183800091217     c                   Clear                   Ar5Ft3
183900091217     c                   Update    Fiar5000
184000091217      * Scrivo Fiar5
184100091217   x1c                   Else
184200091217      * se immessi i dati
184300091217     c                   Clear                   Fiar5000
184400091217     c                   Eval      Ar5Aas = ArbAas
184500091217     c                   Eval      Ar5Lnp = ArbLnp
184600091217     c                   Eval      Ar5Nrs = ArbNrs
184700091217     c                   Eval      Ar5Nsp = ArbNsp
184800091217     c                   Eval      Ar5Trd = 'GEN'
184900091217     c                   Move      Dateu         Ar5Dac
185000091217     c                   Movel     W0140         Ar5Orc
185100091217     c                   Clear                   Dar5Gen
185200091217     c
185300100119     c                   Eval      §Ar5flgmb='S'
185400091217     c                   Eval      §Ar5ticmb=v2ctic
185500091217     c                   Movel(p)  DAr5Gen       Ar5Uni
185600091217     c
185700091217    2c                   if        ar5uni<>*blanks
185800091217     c                   Write     Fiar5000
185900091217    2c                   EndIf
186000091217    1c                   EndIf
186100091217     c
186200091217     c                   ENDSR
186300011227     c**********************************************************************
186400000607     C     *INZSR        BEGSR
186500011227     c**********************************************************************
186600000607      *
186700070320     c
186800070123      *
186900070315     c     *dtaara       define    §azute        azuteds
187000070315     c     *dtaara       define    §datiute      ddatiute
187100070315     c                   in(e)     *dtaara
187200070315     c                   if        %error  or rsut = *blanks
187300070315     c                   clear                   tibs34ds
187400070315     c                   call      'TIBS34R'
187500070315     c                   parm                    tibs34ds
187600070315     c                   in        *dtaara
187700070315     c                   endif
187800030702      *reperimento data
187900070319     C                   TIME                    W0140            14 0
188000070319     C                   MOVE      W0140         WDAT              8 0
188100030702     C*
188200030702     C                   Z-ADD     WDAT          G02DAT
188300030702     C                   MOVEL     *BLANK        G02ERR
188400030702     C                   CALL      'XSRDA8'
188500030702     C                   PARM                    WLBDA8
188600030702     C* UDATE A 8 IN AAAA/MM/GG
188700030717     C                   Z-ADD     G02INV        WUDAte            8 0
188800091216      * Aggancio tabella "VPODECO" per reperimento data attivazione
188900091216      * gestione prepagati
189000091216     C                   CLEAR                   dsbs02
189100091216     c                   clear                   dvpodeco
189200091216     C                   MOVEL     'C'           T02MOD
189300091216     C                   MOVEL     KNSIF         T02SIF
189400091216     C                   MOVEL     'VPO'         T02COD
189500091216     c                   movel(P)  'DECO'        t02ke1
189600091216     C                   CALL      'TIBS02R'
189700091216     C                   PARM                    KPJBA
189800091216     C                   PARM                    dsbs02
189900091216    2C                   IF        T02ERR = *BLANKS
190000091216     C                   MOVEL     T02uni        dvpodeco
190100091216    2C                   ENDIF
190200091217
190300091217      * Aggancio tabella AR5 per record 'GEN'
190400091217     c                   Clear                   dAr5
190500091217     c                   Clear                   dsbs02
190600091217     c                   Eval      T02Mod = 'C'
190700091217     c                   Eval      T02Sif = knsif
190800091217     c                   Eval      T02Cod = 'AR5'
190900091217     c                   Movel(p)  'GEN'         T02Ke1
191000091217     c                   Call      'TIBS02R'
191100091217     c                   Parm                    Kpjba
191200091217     c                   Parm                    dsbs02
191300091217     c                   If        T02Err = *Blanks
191400091217     c                   Movel     T02Uni        dAr5
191500091217     c                   EndIf
191600091217     c
191700000607      *
191800070123      *
191900070124     C     Karb          klist
192000070321     C                   kfld                    ilrg2aas
192100070321     C                   kfld                    ilrg2lnp
192200070321     C                   kfld                    ilrg2nrs
192300070321     C                   kfld                    ilrg2nsp
192400070621     C     Karbp         klist
192500070621     C                   kfld                    kaas
192600070621     C                   kfld                    klnp
192700070621     C                   kfld                    knrs
192800070621     C                   kfld                    knsp
192900070125      *
193000070201      *
193100070201     C     Kar4          klist
193200070201     C                   kfld                    arbaas
193300070201     C                   kfld                    arblnp
193400070201     C                   kfld                    arbnrs
193500070201     C                   kfld                    arbnsp
193600070321     c                   kfld                    wtrc
193700070321     c     KFiar5        Klist
193800070321     C                   kfld                    arbaas
193900070321     C                   kfld                    arblnp
194000070321     C                   kfld                    arbnrs
194100070321     C                   kfld                    arbnsp
194200070321     c                   Kfld                    KAr5Trd
194300070201      *
194400070201     C     Ktab          klist
194500070201     C                   kfld                    tblkut
194600070201     C                   kfld                    tblcod
194700070201     C                   kfld                    tblkey
194800070201     C                   z-add     1             tblkut
194900070321     c     KArn          Klist
195000070321     C                   kfld                    arbaas
195100070321     C                   kfld                    arblnp
195200070321     C                   kfld                    arbnrs
195300070321     C                   kfld                    arbnsp
195400070321     c                   Kfld                    KArncdn
195500070321     c                   Kfld                    KArngst
195600160404      * File FITGD32L
195700070411     c     K02TGD02      klist
195800070411     c                   kfld                    TGDtor
195900070411     c                   kfld                    TGDogg
196000070614     c*
196100070614     C     *LIKE         DEFINE    TBLKUT        §KUT
196200070614     C     *LIKE         DEFINE    TBLCOD        §COD
196300070614     C     *LIKE         DEFINE    TBLKEY        §KEY
196400030714
196500011227     C                   ENDSR
196600070328     c**********************************************************************
196700070328     C     chiudi        BEGSR
196800070328     c**********************************************************************
196900070328     c                   eval      d48tla='C'
197000070328     C                   MOVEL     fnlr48ds      KPJBU
197100070328     c                   call      'FNLR48R'
197200070328     C                   PARM                    KPJBA
197300070328     C                   PARM                    DSARBD
197400070328     C                   PARM                    DSARBK
197500070328     C                   PARM                    DARBT
197600070328     C                   PARM                    DSARBG
197700070328     C                   PARM                    trul90ds
197800070328
197900070328     c                   clear                   fnlv80ds
198000070328     c                   eval      ilv80tla='C'
198100070328     c                   call      'FNLV80R'
198200070328     c                   parm                    kpjba
198300070328     c                   parm                    fnlv80ds
198400070328
198500131008     c**                 eval      d19tla='C'
198600131008     c**                 call      'FNLV19R'
198700131008     c**                 parm                    fnlv19ds
198800070328
198900140130     c                   eval      ilrg7tla='C'
199000140130     c                   call      'FNLRG7R'
199100140130     c                   parm                    fnlrg7ds
199200070329
199300070329     C                   CLEAR                   trul21ds
199400070329     C                   MOVEL     'C'           i21tla
199500070329     C                   CALL      'TRUL21R'
199600070329     C                   PARM                    trul21ds
199700070329     c
199800070329     C                   CLEAR                   trulTICds
199900070329     C                   MOVEL     'C'           IticTLA
200000070329     C                   CALL      'TRULTICR'
200100070329     C                   PARM                    TRULTICDS
200200070329     C                   PARM                    DS1A
200300070503
200400070503     c                   eval      id34tla = 'C'
200500070503     c                   call      'FIDG34R'
200600070503     c                   parm                    fidg34ds
200700070704
200800070704     c                   eval      d66tla  = 'C'
200900120126     C                   MOVEL     fnlr66ds      KPJBU
201000070704     c                   call      'FNLR66R'
201100120126     c                   parm                    kpjba
201200070704
201300070704     c                   clear                   fnlv55ds
201400070704     C                   MOVEL     'C'           D55TLA
201500070704     C                   CALL      'FNLV55R'
201600070704     C                   PARM                    FNLv55DS
201700091216     c*
201800091216     c                   clear                   trulintds
201900091216     C                   MOVEL     'C'           iinttla
202000091216     C                   CALL      'TRULINTR'
202100091216     C                   PARM                    TRULINTDS
202200140922
202300140922     c                   eval      ilry09tla= 'C'
202400141024     c                   eval      kpjbu=fnlry09ds2
202500140922     c                   call      'FNLRY09R'
202600141013     c                   parm                    kpjba
202700140922
202800070328     c                   endsr
202900070315**
203000070329Variazione non eseguibile: bolla impegnata da altro utente.
203100070329Data errata
203200070330Se impostato l'esito premere F6=Conferma
203300070329Immettere l'esito
203400070329Esito errato
203500070403Lasciato avviso ed immessa data cons.richiesta:la sped. deve essere ABILITATA
203600070329Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              07
203700070329Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        08
203800070329Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              09
203900110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       10
204000070704Per l'abilitazione rapida non è possibile variare/immettere dati              11
204100070704Codice errato                                                                 12
204200070704Codice non utilizzabile                                                       13
204300070704Codice obbligatorio                                                           14
204400141013Presenti Disposizioni di Consegna in sospeso: F19 per gestirle                15
