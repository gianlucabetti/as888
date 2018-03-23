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
002500070411     fFITGD02L  if   e           k disk
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
003801140508      * - Flag operativi IDA06FLO per richiamo FIDNA6R
003802140508     d DTGD01        e ds
003900070328     D fnlr48ds      E DS
004000070703     D fnlv55ds      E DS
004100070703     D fnlr66ds      E DS
004200070328     D trul90ds      E DS
004300130515     D DS7PQRS       E DS
004400070329     d* controllo tipo incasso e divisa C/Assegno
004500070329     d trulticds     e ds
004600070329     D* DS PER TRUL21R - controllo importi C/A
004700070329     d trul21ds      e ds
004800070328     D fnlrg2ds      E DS
004900140130     D fnlrn6ds      E DS
005000140130     D fnlrg7ds      E DS
005100070320     D fnlv80ds      E DS
005200070329     d ILV80NWPROC     s              1
005300131008     d
005400131008     D** fnlv19ds      E DS
005500131008     d
005600070201     D fidn12ds      E DS
005700070410     D fidna2ds      E DS
005800070410     d  IA2mod       e                     inz('M')
005900070410     d  IA2tor       e                     inz('S')
006000140508     D fidna6ds      E DS
006100140409     D fnlrj6ds      E DS
006200070503     D fidg34ds      E DS
006201141024     D fnlry09ds2    E DS
006300070926     d TRTB2Ads      e ds                  inz
006400070926     d   TB2Aopz     e                     inz('1')
006500070926     d   TB2Atip     e                     inz('G')
006600070315     d azuteds       e ds                  extname(azute00f)
006700070315     d ddatiute      e ds
006800070315     d tibs34ds      e ds                  inz
006900070131     D trul06ds      E DS
007000070131     D  lin                    1     90  0 dim(30)
007100091216     D trulintds     E DS
007200070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
007300070206     D PARAM1          DS                  INZ
007400070206     D  PA1AAS                14     17  0
007500070206     D  PA1LNP                18     20  0
007600070206     D  PA1NRS                21     22  0
007700070206     D  PA1NSP                23     29  0
007800070206     D  PA1F03                30     30
007900070206     D  PA1FLG                31     31
008000070322     D  PA1PGM                32     41
008100070322     D  PA1DSA                42     61
008200070322     D  PA1CML                62     62
008300070322     D  PA1DSP                63     70  0
008400070322     D  PA1RMN                71     85  0
008500070322     D  PA1LNA                86     88  0
008600070322     D  PA1LN1                89    108
008700070322     D  PA1RSD               109    143
008800070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
008900070206     D  PA1GIA               144    144
009000070322     D  PA1FEL               145    147  0
009100070322     D  PA1TBO               148    148
009200070329     D DS1A          E DS
009300070201     d ds3a          e ds
009400070618     d ds2a          e ds
009500070320     d ds5e          e ds
009600070316     d dsbl4l        e ds
009700080909     d dsbl4a        e ds
009800070321     d dar5Gen       e ds
009900091217     d dar5          e ds
010000091216     d dVpodeco      e ds
010100091216     D* DS PER TIBS02R - GESTIONE TNTBE00F
010200091216     D DSBS02        E DS                  EXTNAME(TIBS02DS)
010300011228      *
010400070621     D*                DS
010500070621     D* ARBAAS                 1      4  0
010600070621     D* ARBMGS                 5      8  0
010700070621     D* ARBDSP                 1      8  0
010800070329
010900030613     D WLBDA8          DS
011000030613     D  G02DAT                 1      8  0
011100030613     D  G02INV                 9     16  0
011200030613     D  G02ERR                17     17
011300030613     D  G02TGI                18     22  0
011400070322     D Fiar41          DS
011500070322     D  ar4nrr               397    400B 0
011600070328     d v2cnra          ds
011700070328     d  v2cnt1
011800070328     d  v2cnt2
011900070328     d  v2cnt3
012000070328     d  v2cnt4
012100070327     D                SDS
012200070327     D  N_pgm                  1      8
012300070621
012400070329     d forzacas        s              1
012500070328     D wtit2           S                   like(v2ctit2)
012600070328     d dataeur         s               d   datfmt(*eur)
012700070703     D wrkoggiiso      s               d
012800070321     d KAr5Trd         s                   like(Ar5Trd)
012900070321     d KArnCdn         s                   like(ArnCdn)inz('LDV')
013000070321     d KArngst         s                   like(Arngst)inz('S')
013100070321     d wtrc            s                   like(ar4trc)
013200070321     d wblk            s              1
013300070322     d sav4nrr         s                   like(ar4nrr)
013400070621     d savnrr          s                   like(arbnrr)
013500070328     d v2cdcr_i        s                   like(v2cdcr)
013600070329     d s_v2ctic        s                   like(v2ctic)
013700070329     d w_dtcr          s                   like(olv80tcrd)
013800070329     d pre_tic         s                   like(v2ctic)
013900070703     d*sav_kf          s                   like(*inkf)
014000070621     d kaas            s                   like(arbaas)
014100070621     d klnp            s                   like(arblnp)
014200070621     d knrs            s                   like(arbnrs)
014300070621     d knsp            s                   like(arbnsp)
014400070621     d vh2aap          s                   like(arbaas)
014500070621     d vh2lpp          s                   like(arblnp)
014600070621     d vh2nrp          s                   like(arbnrs)
014700070621     d vh2nsp          s                   like(arbnsp)
014800091127     d s_ar5cdi        s                   like(§ar5cdi)
014900140409     d wncl            s                   like(arbncl)
015000070924      *
015100070924     d Cod             s                   like(TBLcod)  inz
015200070924     d Key             s                   like(TBLkey)  inz
015300070924     d Ordina          s              1                  inz
015400070924     d Comando         s              1                  inz
015500070319
015600070316      *---------------------------------------------------------------------
015700070321     D coscas          C                   CONST('           Contrasse-
015800070321     D                                     gno           ')
015900070321     D cosapp          C                   CONST('           Appuntame-
016000070321     D                                     nto           ')
016100070321     D cossup          C                   CONST('           Supermerc-
016200070321     D                                     ato           ')
016300070321     D cosffd          C                   CONST('         Fermo Depos-
016400070321     D                                     ito           ')
016500070321     D coslav          C                   CONST('          Lasciato A-
016600070321     D                                     vviso         ')
016700070321     D cossuptas       C                   CONST('      Supermercato:t-
016800070321     D                                     assativo      ')
016900070321     D cosapptas       C                   CONST('      Appuntamento:t-
017000070321     D                                     assativo      ')
017100141030     D msg             S             78    DIM(15) CTDATA PERRCD(1)
017200070320      *---------------------------------------------------------------------
017300070320     C     *ENTRY        PLIST
017400070320     C                   PARM                    Kpjba
017500070320     C                   PARM                    fnlrg2ds
017600070320     c*
017700070320     c                   clear                   olrg2err
017800070320     c                   clear                   olrg2msg
017900070326     c                   clear                   olrg2no1
018000070326     c                   clear                   olrg2no2
018100070330     c                   clear                   olrg2f12
018200070320     c                   if        ilrg2tla='C'
018300070320     c                   goto      fine
018400070320     c                   endif
018500130827
018600130827     c* pulizia campo per errore fozabile
018700130827     c                   clear                   forzacas
018800130827
018900070327     C                   TIME                    W0140            14 0
019000070327
019100070327      * UDATE IN AAAAMMGG
019200070327     C                   MOVE      W0140         g02dat            8 0
019300070327     C                   MOVEL     *BLANK        G02ERR
019400070327     C                   CALL      'XSRDA8'
019500070327     C                   PARM                    WLBDA8
019600070327     C                   MOVEL     G02INV        DATEU             8 0
019700070321     c*
019800070621     c     karb          chain     fnarb01l
019900070621     c                   z-add     arbnrr        savnrr
020000070321     C                   MOVEL     'L'           WTRC
020100070328     C     Kar4          CHAIN     Fiar401l
020200070321     C                   if        %found(fiar401l)
020300070321     c                   movel     ar4not        dsbl4l
020400070322     c                   z-add     ar4nrr        sav4nrr
020500070328     c                   else
020600070328     c                   clear                   sav4nrr
020700070328     c                   clear                   dsbl4l
020800070321     c                   endif
020900070321     c                   if        ilrg2cau='L' and arbfbc='A'
021000070419     c     karb          setgt     fnevb22l
021100070419     c     karb          readpe(N) fnevb22l
021200070321     c                   endif
021300070320     c*
021400070321     C                   EXSR      RieD01
021500000608      *
021600070315     c                   do        *hival
021700070412     c                   if        ilrg2tss<>'S'and ilrg2tch<>'R'
021800070410     c                   exsr      sr_AbilF10
021900070412     c                   endif
021901140922     c                   exsr      sr_abilf19
022000070411     c  N13              exsr      sr_abilf16
022100070619     c                   write     lrg2t01
022200070320     C                   EXFMT     lrg2d01
022300070315     c                   setoff                                       9028
022400070315     c                   clear                   vidmsg
022500070326      * F12=Ritorno
022600070326     C                   if        *INKL
022700070330     c                   eval      olrg2f12='1'
022800070315     C                   leave
022900070315     c                   endif
023000070315      * F7=Interrogazione bolle arrivi
023100070315     c                   if        *inkg
023200070315     c                   clear                   kpjbu
023300070315     c                   clear                   param1
023400070322     C                   MOVE      v2haas        PA1AAS
023500070322     C                   MOVE      v2clnp        PA1LNP
023600070322     C                   MOVE      v2cnrs        PA1NRS
023700070322     C                   MOVE      v2cnsp        PA1NSP
023800070322     C                   MOVE      'N'           PA1FLG
023900070322     C                   MOVE      *blanks       PA1CML
024000070327     C                   MOVEL     N_pgm         PA1PGM
024100070315     c                   movel     param1        kpjbu
024200070315     c                   call      'FNLR36R'
024300070315     c                   parm                    kpjba
024400070315     c                   iter
024500070315     c                   endif
024501141105     c* Contatto non eseguibile per presenza dispo di dirottamento: permetto solo l'F12
024502141105     c                   if        not *inkl and OLRY09ESDI='1'
024503141105     c                   iter
024504141105     c                   endif
024600070410      * F16=Gestione Richieste (R.A.)
024700070405     c                   if        *inkq
024800070410     c                   reset                   FIDNA2ds
024900070410     c                   eval      ia2ogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
025000070410     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
025100070410     c                   call      'FIDNA2R'
025200070410     c                   parm                    KPJBA
025300070410     c                   parm                    FIDNA2ds
025400070405     c                   iter
025500070405     c                   endif
025600070412      *?F17 - Note LDV
025700070412if  1c                   if        *inKR = *on
025800140130     c                   clear                   fnlrn6ds
025900140130     c                   eval      in6mod='V'
026000140130     c                   eval      in6aas=arbaas
026100140130     c                   eval      in6lnp=arblnp
026200140130     c                   eval      in6nrs=arbnrs
026300140130     c                   eval      in6nsp=arbnsp
026400140131     c                   eval      in6pgm='FNLRG2R'
026500140130     c                   call      'FNLRN6R'
026600070412     c                   parm                    kpjba
026700140130     c                   parm                    fnlrn6ds
026800070412     c                   iter
026900070412e   1c                   endif
026901140922      *?F19 - Indicazioni di Consegna da WEB
026902140922if  1c                   if        *inKT = *on
026903141024     c                   clear                   fnlry09ds2
026905140922     c                   eval      ilry09tch='E'
026906141024     c                   eval      ILRY09TDIS='E'
026908140922     c                   eval      ILRY09AAS=ilrg2aas
026909140922     c                   eval      ILRY09LNP=ILRG2LNP
026910140922     c                   eval      ILRY09NRS=ILRG2NRS
026911140922     c                   eval      ILRY09NSP=ILRG2NSP
026912141024     c                   eval      ILRY09EF12='S'
026913141024     c                   eval      ILRY09EF21='S'
026914141024     c                   eval      ILRY09EF06='S'
026915141024
026916141024     c                   eval      kpjbu=fnlry09ds2
026917140922     c                   call      'FNLRY09R'
026918141013     c                   parm                    kpjba
026919140922     c                   iter
026920140922e   1c                   endif
027000070319      * controlli videata
027100070315     C                   EXSR      Ctrd01
027200070315     c   90              iter
027300070614     c* se immesso esito "E"=Esclusa da gestione-attesa giacenza emetto
027400070614     c* finestra per eventuale immissione codice giacenza
027500070703     c*                  if        v2cesi='E'
027600070703     c*                  eval      sav_kf=*inkf
027700070703     c*                  exsr      gesw01
027800070703     c*                  eval      *inkf=sav_kf
027900070703     c*                  endif
028000070320     c* f6=Conferma
028100070320     c   kf              exsr      confe
028200070322     c   90              iter
028300070329     c                   if        *inkf='1' and *in90='0'
028400070329     c                   leave
028500070329     c                   endif
028600070320     c* F10=Abilitazione Rapida
028700070320     c   kj              exsr      rapida
028800070326     c   90              iter
028900070329     c                   if        *inkj='1' and *in90='0'
029000070329     c                   leave
029100070329     c                   endif
029200011227      *
029300011227     C                   ENDDO
029400070124      *
029500011227      *
029600070320     c     fine          tag
029700070320     c                   if        ilrg2tla=' '
029800070320     c                   seton                                        rt
029900070320     c                   else
030000070328     c                   exsr      chiudi
030100070315     C                   seton                                        lr
030200070320     c                   endif
030300070315     c**********************************************************************
030400070320     c* Caricamento d01
030500070315     c**********************************************************************
030600070321     C     RieD01        BEGSR
030700070321     c                   setoff                                       90
030800070321     c* CAMPI DI OUTPUT
030900070320     c                   z-add     arblnp        v2clnp
031000070320     c                   z-add     arbnrs        v2cnrs
031100070320     c                   z-add     arbnsp        v2cnsp
031200070320     c                   movel     arbaas        wdata             8 0
031300070320     c                   move      arbmgs        wdata
031400070320     c     *iso          move      wdata         dataeur
031500070320     c                   move      dataeur       v2cdsp
031600070322     c                   move      arbaas        v2haas
031700070320     c                   movel     arbcbo        v2ccbo
031800070320     c                   clear                   v2dcbo
031900070320     c                   move      '3A'          tblcod
032000070320     c                   movel(p)  arbcbo        tblkey
032100070320     c                   exsr      ctabel
032200070320     c                   movel     tbluni        ds3a
032300070320     c                   movel     §3ades        v2dcbo
032400070320     c* tipo spedizione
032500070320     c                   clear                   v2dtsp
032600070320     c                   move      '5E'          tblcod
032700070320     c                   movel(p)  arbtsp        tblkey
032800070320     c                   exsr      ctabel
032900070320     c                   movel     tbluni        ds5e
033000090604     c                   movel     §5ed08        v2dtsp
033100070320     c* mittente
033200070320     c                   clear                   v2cmit
033300070320     c                   if        %subst(§3atb1:1:1)='F'
033400070320     c                   move      arbksc        v2cmit
033500070320     c                   else
033600070320     c                   move      arbccm        wccm4             4 0
033700070320     c                   if        wccm4<>9999
033800070320     c                   move      arbccm        v2cmit
033900070320     c                   endif
034000070320     c                   endif
034100070320     c* ragione sociale mittente
034200070320     c                   movel(P)  arbrsm        v2crsm
034300070320     c* ragione sociale mittente originale
034400070320     c                   movel(P)  arbrmo        v2crmo
034500070320     c* destinatario
034600070320     c                   movel(P)  arbrsd        v2crsd
034700070320     c* estensione ragione sociale destinatario da fiar4 record "D"
034800070320     C                   MOVEL     'D'           WTRC
034900070328     C     KAR4          CHAIN     Fiar401l
035000070320     C                   IF        %found(fiar401l)
035100070320     C                   MOVE      AR4NOT        v2crsd
035200070320     C                   ENDIF
035300070321     c                   movel     arbind        v2cind
035400070321     c                   movel     arbcad        v2ccad
035500070321     c                   movel     arblod        v2clod
035600070321     c                   eval      v2clod=%trim(v2clod)+ '  ' + arbprd
035700070321     c* referente consegna
035800070321     c                   Clear                   DAr5Gen
035900070321     c                   Eval      kAr5Trd = 'GEN'
036000091217     c     KFiar5        Chain(n)  Fiar501l
036100070321     c                   If        %Found(Fiar501l)
036200070321     c                   Movel     Ar5Uni        DAr5Gen
036300070321     c                   EndIf
036400070327    2c                   if        %subst(§ar5ref:1:1) = '#'
036500070327     c                   eval      v2cref=%subst(§ar5ref:2:34)
036600070327     c                   else
036700070321     c                   Eval      V2cRef = §Ar5Ref
036800070327     c                   endif
036900070327    2c                   if        %subst(§ar5teld:1:1) = '#'
037000070327     c                   eval      v2ctel=%subst(§ar5teld:2:15)
037100070327     c                   else
037200070321     c                   Eval      V2cTel = §Ar5Teld
037300070327     c                   endif
037400091127     c* memorizzo anche eventuale codice intestazione assegno
037500091127     c                   eval      s_ar5cdi=§ar5cdi
037600070321     c* consegna richiesta
037700070321     c                   clear                   fnlv80ds
037800070321     c                   eval      ilv80aas=arbaas
037900070321     c                   eval      ilv80lnp=arblnp
038000070321     c                   eval      ilv80nrs=arbnrs
038100070321     c                   eval      ilv80nsp=arbnsp
038200070321     c                   eval      ilv80mgs=arbmgs
038300070321     c                   eval      ilv80tcr=arbtcr
038400070321     c                   eval      ilv80dcr=arbdcr
038500070321     c                   eval      ilv80hcr=arbhcr
038600070321     c                   eval      ilv80tc1=arbtc1
038700070321     c                   eval      ilv80tc2=arbtc2
038800070321     c                   call      'FNLV80R'
038900070321     c                   parm                    kpjba
039000070321     c                   parm                    fnlv80ds
039100070329     c                   parm      'S'           ILV80NWPROC
039200070321     c                   eval      v2ctcro=olv80tcrd
039300070330     c                   if        arbdcr>0
039400070329     c     *iso          move      arbdcr        dataeur
039500070329     c                   move      dataeur       v2cdcro
039600070330     c                   else
039700070330     c                   clear                   v2cdcro
039800070330     c                   endif
039900070321     c                   eval      v2chcro=arbhcr
040000070321     c* colli e peso bollettato
040100070321     c                   z-add     arbncl        v2cncl
040200070321     c                   z-add     arbpkb        v2cpkb
040300070326     c* altre gestioni
040400070321     c                   clear                   v2dges
040500070321     c                   clear                   v2cges
040600140130     c                   clear                   fnlrg7ds
040700140130     c                   z-add     v2haas        ILRG7AAS
040800140130     c                   z-add     v2clnp        ILRG7LNP
040900140130     c                   z-add     v2cnrs        ILRG7NRS
041000140130     c                   z-add     v2cnsp        ILRG7NSP
041100140130     c                   movel     ilrg2cau      ILRG7NB1
041200140130     c                   call      'FNLRG7R'
041300140130     c                   parm                    fnlrg7ds
041400140130     c                   movel     OLRG7VSSIN    v2dges
041500140409     c* visualizzo se tutti i colli arrivati alla fil di arrivo
041600140409     c                   clear                   fnlrj6ds
041700140409     c                   eval      iLRJ6TRIC = 'A'
041800140409     c                   eval      iLRJ6AAS  = v2haas
041900140409     c                   eval      iLRJ6LNP  = v2cLNP
042000140409     c                   eval      iLRJ6NRS  = v2cNRS
042100140409     c                   eval      iLRJ6NSP  = v2cNSP
042200140409     c                   call      'FNLRJ6R'
042300140409     c                   parm                    fnlrj6ds
042400140409     c
042500140409     c                   clear                   v2carr
042600140409     c                   if        OLRJ6FALL='S'
042700140409     c                   eval      v2carr='TUTTI'
042800140409     c                   else
042900140409     c                   eval      wncl=OLRJ6CDCM+OLRJ6CSPU
043000140409     c                   eval      v2carr=%editc(wncl:'Z')
043100140409     c                   if        v2carr=*blanks
043200140409     c                   move      '0'           v2carr
043300140409     c                   endif
043400140409     c                   endif
043500070702     c*
043600070702     c* causale per variazione bolla
043700070702     c                   clear                   v2ccvb
043800130515     c* particolarità
043900130515     c                   setoff                                       444547
044000130515     c                   clear                   v2igga
044100130515     c                   clear                   v2igma
044200130515     c                   clear                   v2igva
044300130515     c                   eval      v2cgga=arbgga
044400130515     c                   if        v2cgga<>*blanks
044500130515     c                   seton                                        44
044600130515     c                   endif
044700130515     c                   eval      v2cgma=arbgma
044800130515     c                   if        v2cgma<>*blanks
044900130515     c                   seton                                        45
045000130515     c                   endif
045100130515     c                   eval      v2cgva=arbgva
045200130515     c                   if        v2cgva<>*blanks
045300130515     c                   seton                                        47
045400130515     c                   endif
045500070326
045600070321     c* abilitazione merce
045700140131     C**   ARBAMA        IFGT      0
045800140131     C**                 MOVEL     'SI'          V2CAMA
045900140131     C**                 ELSE
046000140131     C**                 MOVEL     'NO'          V2CAMA
046100140131     C**                 ENDIF
046200140131     C**                 MOVEL     '     '       V2DAMA
046300140131     C**   ARBAMA        IFEQ      3
046400140131     C**                 MOVEL     'FORZ.'       V2dAMA
046500140131     C**                 ELSE
046600140131     C**   ARBAMA        IFEQ      1
046700140131     C**                 MOVEL     'PART.'       V2DAMA
046800140131     C**                 ENDIF
046900140131     C**                 ENDIF
047000080909     c* SI DDT
047100080909     c                   setoff                                       14
047200080909     c                   movel     'A'           wtrc
047300080909     c     kar4          chain     fiar401l
047400080909     c                   if        %found(fiar401l)
047500080909     c                   movel     ar4not        dsbl4a
047600080909     c                   if        §b4abm='S' or §b4abm='C' or
047700080909     c                             §b4abm='P' or §b4abm='Q'
047800080909     c                   seton                                        14
047900080909     c                   endif
048000080909     c                   endif
048100070321     c* flag fermo deposito
048200070321     c                   if        arbffd<>*blanks
048300070321     c                   seton                                        10
048400070321     c                   else
048500070321     c                   setoff                                       10
048600070321     c                   endif
048700070321     c* abilitazione tasto funzionale F10-Abilitazione Rapida
048800070412     c                   setoff                                       1213
048900070329     c                   if        ilrg2tss='S' or ilrg2tch='R'
049000070321     c                   seton                                        12
049100070405     c                   if        ilrg2tch='R'
049200070405     c                   seton                                        13
049300070321     c                   endif
049400070410     c                   endif
049500070412     c  N12              exsr      sr_AbilF10
049600070321     c* Sottotitolo videata + situazione contatto in corso
049700070321     c                   clear                   v2ctit2
049800070321     c                   clear                   v2dblk
049900070402     c                   clear                   wblk
050000070321     c                   select
050100070321     c* contrassegno
050200070321     c                   when      ilrg2cau='C'
050300070321     c                   eval      v2ctit2=coscas
050400070321     c                   if        arbacc<>'S'
050500070321     c                   move      §b4ltca       wblk
050600070321     c* se il c/a non è abilitato e §b4ltca=blaks c'è qualcosa che non va..
050700070321     c                   if        wblk=*blanks
050800070321     c                   move      *hival        wblk
050900070321     c                   endif
051000070321     c                   endif
051100070321     c* appuntamento
051200070321     c                   when      ilrg2cau='A'
051300070321     c                   if        ilrg2tss=' '
051400070321     c                   eval      v2ctit2=cosapp
051500070321     c                   else
051600070321     c                   eval      v2ctit2=cosapptas
051700070321     c                   endif
051800070321     c                   move      §b4lbas       wblk
051900070321     c* supermercato
052000070321     c                   when      ilrg2cau='S'
052100070321     c                   if        ilrg2tss=' '
052200070321     c                   eval      v2ctit2=cossup
052300070321     c                   else
052400070321     c                   eval      v2ctit2=cossuptas
052500070321     c                   endif
052600070321     c                   move      §b4lbas       wblk
052700070321     c* fermo deposito
052800070321     c                   when      ilrg2cau='F'
052900070321     c                   eval      v2ctit2=cosffd
053000070321     c                   move      §b4ltfd       wblk
053100070321     c* lasciato avviso
053200070321     c                   when      ilrg2cau='L'
053300070321     c                   eval      v2ctit2=coslav
053400070321     c                   if        arbfbc='A'
053500070321     c                   move      evb2tel       wblk
053600070321     c* se il lavv non è abilitato e evb2tel=blank c'è qualcosa che non va..
053700070321     c                   if        wblk=*blanks
053800070321     c                   move      *hival        wblk
053900070321     c                   endif
054000070321     c                   endif
054100070321     c                   endsl
054200070321
054300070321     c                   select
054400070321     c                   when      wblk=*blanks
054500070328     c                   if        ilrg2cau='F'
054600070328     c                   eval      v2dblk='CONTATTATO'
054700070328     c                   else
054800070328     c                   eval      v2dblk='ABILITATO'
054900070328     c                   endif
055000070321     c                   when      wblk='S'
055100070328     c                   eval      v2dblk='DA CONTATTARE'
055200070321     c                   when      wblk='T'
055300070328     c                   eval      v2dblk='DA RICONTATTARE'
055400070321     c                   when      wblk='N'
055500070328     c                   eval      v2dblk='ESCLUSO DA GESTIONE--
055600070328     c                             ATTESA GIACENZA'
055700070321     c                   other
055800070328     c                   eval      v2dblk=*all'?'
055900070321     c                   endsl
056000070503     c* giro di consegna
056100070503     c                   clear                   v2ccgi
056200070504     c***                clear                   v2ccos
056300070503     c                   clear                   v2dcgi
056400070503     c                   clear                   fidg34ds
056500070503     c                   eval      id34tip='2'
056600070503     c                   eval      id34aas=arbaas
056700070503     c                   eval      id34lnp=arblnp
056800070503     c                   eval      id34nrs=arbnrs
056900070503     c                   eval      id34nsp=arbnsp
057000070503     c                   call      'FIDG34R'
057100070503     c                   parm                    FIDG34DS
057200070503     c                   movel     od34cgi       v2ccgi
057300070503     c                   eval      v2dcgi=od34cgides
057400070504     c**                 if        od34esito = *blanks
057500070504     c**                 eval      v2ccos='-'
057600070504     c**                 endif
057700070503     c
057800070321     c*
057900070321     c* CAMPI DI INPUT/OUTPUT
058000070321     c*
058100070321     c* note partenza
058200070321     c                   clear                   v2cnot8
058300070321     c                   clear                   v2cnot9
058400070322     c                   clear                   v2hnot8
058500070322     c                   clear                   v2hnot9
058600070321     C                   MOVEL     '8'           WTRC
058700070328     C     Kar4          CHAIN     Fiar401l
058800070321     C                   if        %found(fiar401l)
058900070321     C                   MOVEL     ar4NOT        V2cnot8
059000070322     C                   MOVEL     ar4NOT        V2hnot8
059100070321     C                   ENDIF
059200070321     C*
059300070321     C                   MOVEL     '9'           WTRC
059400070328     C     Kar4          CHAIN     Fiar401l
059500070321     C                   if        %found(fiar401l)
059600070321     C                   MOVEL     ar4NOT        V2cnot9
059700070322     C                   MOVEL     ar4NOT        V2hnot9
059800070321     C                   ENDIF
059900070321     c* Contrassegno
060000070321     c                   if        ilrg2cau='C'
060100070321     c                   setoff                                       11
060200070321     c                   eval      v2cflc='?'
060300070321     c                   else
060400070321     c                   seton                                        11
060500070321     c                   eval      v2cflc=':'
060600070321     c                   endif
060700070321     C                   MOVEL     *BLANKS       V2cTIC
060800070329     C                   MOVEL     *BLANKS       S_V2cTIC
060900070329     C                   MOVEL     *BLANKS       pre_TIC
061000070321     C                   MOVEL     *ZERO         V2cCAS
061100070321     C                   MOVEL     *BLANKS       V2cVCA
061200091127     c                   setoff                                       15
061300070321     C     §3AFCA        IFGT      *ZERO
061400070322     C     Karb          CHAIN(N)  FiAR901L
061500070321     C                   if        %found(fiar901l)
061600070321     C                   MOVEL     AR9TIC        V2cTIC
061700070329     C                   MOVEL     AR9TIC        s_V2cTIC
061800070329     C                   MOVEL     AR9TIC        pre_TIC
061900070321     C                   MOVEL     AR9CAS        V2cCAS
062000070321     C                   MOVEL     AR9VCA        V2cVCA
062100070321     C                   ENDif
062200091216     c* richiamo trul per determinare l'intestatario del c/a
062300091216     c                   if        dateu>=§VPOCAS
062400091216     c                   clear                   trulintds
062500091216     c                   eval      iinttic=v2ctic
062600091216     c                   eval      iinttip='I'
062700091216     c                   eval      IINTRSCM=arbrsm
062800091216     c                   eval      IINTRSCO=arbrmo
062900091216     c                   eval      IINTcdi=s_ar5cdi
063000091216     c                   call      'TRULINTR'
063100091216     c                   parm                    trulintds
063200091216     c                   if        ointerr=*blanks
063300091127     c                   seton                                        15
063400091216     c                   eval      v2ccdi=ointrsci
063500091216     c                   endif
063600091216     c                   endif
063700070321     C                   ENDIF
063800070328     c* DESCRIZIONE ESITO
063900070328     c                   clear                   v2desa
064000070328     c                   clear                   v2desi
064100070328     c* PRIMA RIGA
064200070328     c                   select
064300070328     c* Tassative
064400070328     c                   when      ilrg2tss='S'
064500070328     c                   eval      v2desa='(D=Da contattare)'
064600070328     c* Fermo deposito
064700070328     c                   when      ilrg2cau='F'
064800070328     c                   eval      v2desa='(C=Contattato; R=Ricontattare;'
064900070328     c* Appuntamenti
065000070412     c***                when      ilrg2cau='A'
065100070412     c***                eval      v2desa='(A=Abilitato; R=Ricontattare)'
065200070328     c* Altri
065300070328     c                   other
065400070328     c                   eval      v2desa='(A=Abilitato; R=Ricontattare;'
065500070328     c                   endsl
065600070328     c* SECONDA RIGA
065700070328     c                   if        (ilrg2cau='S' and ilrg2tss=' ') or
065800070412     c                             ilrg2cau='C' or ilrg2cau='A' or
065900070328     c                             ilrg2cau='L' or ilrg2cau='F'
066000070712     c                   eval      v2desi='E=Esclusa da gestione-apertura giace-
066100070712     c                             nza)'
066200070328     c                   endif
066300070321     c
066400070321     c                   clear                   v2ctcr
066500070321     c                   clear                   v2cdcr
066600070321     c                   clear                   v2chcr
066700070321     c                   clear                   v2cesi
066800070330     c                   if        ilrg2tch='R'
066900070328     c                   eval      v2cnra=ilrg2no1
067000070328     c                   else
067100070321     c                   clear                   v2cnt1
067200070321     c                   clear                   v2cnt2
067300070321     c                   clear                   v2cnt3
067400070321     c                   clear                   v2cnt4
067500070328     c                   endif
067600070614     c* campi della window per l'inserimento del codice giacenza
067700070614     c                   clear                   w2ccmc
067800070614     c                   clear                   w2dcmc
067900070321     c*
068000070321     c
068100070315     C                   ENDSR
068200070321     c**********************************************************************
068300070321     c* Verifica abilitazione tasto F10-Abilitazione Rapida
068400070321     c**********************************************************************
068500070321     C     SR_AbilF10    BEGSR
068600070321     c* L'F10 non è da abilitare quando:
068700070321     c* - Sono in gestione delle tassative
068800070321     c* - Sono in gestione di spedizioni per le quali sono già presenti Note
068900070321     c*   AUT
069000070321     c* Il Primo caso è sufficiente controllarlo una volta nel pgm mentre il
069100070321     c* secondo caso occorre controllarlo che viene emesso il video in
069200070321     c* quanto le note AUT possono essere inserite anche tramite l'interro-
069300070321     c* gazione bolle e si tratta coumunque di una condizione dinamica al
069400070321     c* contrario della prima che dipende dalla causale contatto richiesta
069500070321     c* QUINDI: Controllo qui solo il secondo caso
069600070412     c                   setoff                                       65
069700070321     c     karn          setll     fiarn11l
069800070321     c                   if        %equal(fiarn11l)
069900070412     c                   seton                                        65
070000070321     c                   endif
070100070321     c                   endsr
070200070315     c**********************************************************************
070300070315     c* Controlli video D01.
070400070315     c**********************************************************************
070500070315     C     Ctrd01        BEGSR
070600070315      *
070700070329     c                   setoff                                       404142
070800141030     c                   setoff                                       4348
070900070329     c* ? su altre gestioni
071000070329     c                   if        v2cges='?'
071100140130     c                   clear                   fnlrg7ds
071200140130     c                   eval      ilrg7aas=ilrg2aas
071300140130     c                   eval      ilrg7lnp=ilrg2lnp
071400140130     c                   eval      ilrg7nrs=ilrg2nrs
071500140130     c                   eval      ilrg7nsp=ilrg2nsp
071600140130     c                   eval      ilrg7vsdet='?'
071700140130     c                   call      'FNLRG7R'
071800140130     c                   parm                    fnlrg7ds
071900070329     c                   clear                   v2cges
072000070329     c                   seton                                        90
072100070329     c                   goto      endct8
072200070329     c                   endif
072300130515     c* ? su particolarità
072400130515     c                   if        v2igga='?'
072500130515     C                   CLEAR                   DS7PQRS
072600130515     C                   MOVEL     'S'           DS7RIC
072700130515     C                   MOVEL     'V'           DS7GES
072800130515     C                   MOVEL     V2CGGA        DS7COD
072900130515     C                   MOVEL     DS7PQRS       KPJBU
073000130515     C                   CALL      'TRTB69R'
073100130515     C                   PARM                    KPJBA
073200130515     c                   clear                   v2igga
073300130515     c                   seton                                        90
073400130515     c                   goto      endct8
073500130515     c                   endif
073600130515
073700130515     c                   if        v2igma='?'
073800130515     C                   CLEAR                   DS7PQRS
073900130515     C                   MOVEL     'S'           DS7RIC
074000130515     C                   MOVEL     'V'           DS7GES
074100130515     C                   MOVEL     V2cgma        DS7COD
074200130515     C                   MOVEL     DS7PQRS       KPJBU
074300130515     C                   CALL      'TRTB70R'
074400130515     C                   PARM                    KPJBA
074500130515     c                   clear                   v2igma
074600130515     c                   seton                                        90
074700130515     c                   goto      endct8
074800130515     c                   endif
074900130515
075000130515     c                   if        v2igva='?'
075100130515     C                   CLEAR                   DS7PQRS
075200130515     C                   MOVEL     'S'           DS7RIC
075300130515     C                   MOVEL     'V'           DS7GES
075400130515     C                   MOVEL     V2cgva        DS7COD
075500130515     C                   MOVEL     DS7PQRS       KPJBU
075600130515     C                   CALL      'TRTB73R'
075700130515     C                   PARM                    KPJBA
075800130515     c                   clear                   v2igva
075900130515     c                   seton                                        90
076000130515     c                   goto      endct8
076100130515     c                   endif
076200070703     c* Causale per variazione bolla
076300070703    1C     v2ccvb        IFNE      *BLANKS
076400070703     C*
076500070703     C     '?'           SCAN      v2ccvb                                 80
076600070703     C* . . . Ricerca
076700070703    2C     *IN80         IFEQ      *ON
076800070703     c                   clear                   fnlr66ds
076900070703     C                   MOVEL     ilrg2nsp      D66NSP
077000070703     C                   MOVEL     ilrg2aas      D66AAS
077100070703     C                   MOVEL     ilrg2lnp      D66LNP
077200070703     C                   MOVEL     ilrg2nrs      D66NRS
077300070703     C                   MOVEL     'V'           D66GES
077400070703     C                   MOVEL     'A'           D66TBO
077500070703     C*
077600070703     C                   MOVEL     fnlr66ds      KPJBU
077700070703     C                   CALL      'FNLR66R'
077800070703     C                   PARM                    KPJBA
077900070703     C                   MOVEL     KPJBU         fnlr66ds
078000070703     C                   MOVEL     D66CVB        V2ccvb
078100070703     c                   seton                                        90
078200070703     C                   GOTO      ENDCT8
078300070703   X2C                   ELSE
078400070703     C* . . . Controllo
078500070703     C                   CLEAR                   fnlr66ds
078600070703     C                   MOVEL     'C'           D66GES
078700070703     C                   MOVEL     'A'           D66TBO
078800070703     C                   MOVEL     V2ccvb        D66CVB
078900070703     C*
079000070703     C                   MOVEL     fnlr66ds      KPJBU
079100070703     C                   CALL      'FNLR66R'
079200070703     C                   PARM                    KPJBA
079300070703     C                   MOVEL     KPJBU         fnlr66ds
079400070703     C                   MOVEL     D66CVB        V2ccvb
079500070703     C*
079600070703     C* . . . Errore
079700070703    3C     D66MSG        IFNE      *BLANKS
079800070703     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
079900070703     C                   SETON                                        284390
080000070703     C                   GOTO      ENDCT8
080100070703    3C                   ENDIF
080200070703    2C                   ENDIF
080300070703     C*
080400070703     C* Richiamo pgm di variazione se impostato il codice
080500070703     C*
080600070703     C                   CLEAR                   fnlr48ds
080700070703     C                   z-add     ilrg2aas      D48AAS
080800070703     C                   z-add     ilrg2lnp      D48LNP
080900070703     C                   z-add     ilrg2nrs      D48NRS
081000070703     C                   z-add     ilrg2nsp      D48NSP
081100070703     c* come p.o. gestione passo il gestore della lna
081200070703     c                   clear                   fnlv55ds
081300070703     c                   z-add     arblna        d55lin
081400070703     c                   movel     '6'           d55tpt
081500070703     C                   time                    wrkoggiiso
081600070703     C     *iso          move      wrkoggiiso    d55drf
081700070703     c                   call      'FNLV55R'
081800070703     c                   parm                    fnlv55ds
081900070703     c                   movel     d55tfa        d48fgs
082000070703     C                   MOVEL     ARBCBO        D48CBO
082100070703     C                   MOVEL     V2ccvb        D48CVB
082200070703     C                   MOVEL     'A'           D48TBO
082300070703     C                   MOVEL     'S'           D48F12
082400070703     C                   MOVEL     'V'           D48FFR
082500070703     C                   MOVEL     fnlr48ds      KPJBU
082600070703     C                   CALL      'FNLR48R'
082700070703     C                   PARM                    KPJBA
082800070703     C                   PARM                    DSARBD
082900070703     C                   PARM                    DSARBK
083000070703     C                   PARM                    DARBT
083100070703     C                   PARM                    DSARBG
083200070703     C                   PARM                    TRUL90DS
083300070703     c                   clear                   v2ccvb
083400070709     c                   seton                                        90
083500070709     c                   goto      endct8
083600070703     C*
083700070703     c                   endif
083800070702     c
083900070329     c* TIPO INCASSO solo se non è protetto (causale contrassegno)
084000070329    1c                   if        *in11='0'
084100070329     c* Richiamo pgm per controllo tipo incasso e divisa C/A
084200070329     c                   clear                   trulticds
084300070329     c                   clear                   ds1a
084400070329     c                   eval      iticTBO='A'
084500070329     c                   eval      iticlnp=arblnp
084600070329     c                   eval      iticlna=arblna
084700070329     c                   eval      iticnzd=arbnzd
084800090707     c                   eval      iticprd=arbprd
084900090707     c                   eval      iticcad=arbcad
085000090707     c                   eval      iticlod=arblod
085100090707     c                   eval      iticind=arbind
085200090707     c                   eval      iticrsd=arbrsd
085300070621     c                   movel     arbaas        iticdsp
085400070621     c                   move      arbmgs        iticdsp
085500091216     c                   if        s_ar5cdi<>*blanks
085600091216     C                   MOVEL     s_ar5cdi      iticccm
085700091216     c                   else
085800130827    2C***  ARBCCM        IFGT      0
085900130827     C***                MOVEL     ARBCCM        iticccm
086000130827     C***                ELSE
086100130827     C***                MOVEL     ARBKSC        iticccm
086200130827    2C***                END
086300130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
086400130827     c                   if        %subst(§3atb1:1:1)='F'
086500130827     c                   z-add     arbksc        iticccm
086600130827     c                   else
086700130827     c                   z-add     arbccm        iticccm
086800130827     c                   if        iticccm=0
086900130827     c                   z-add     8888          iticccm
087000130827     c                   movel     arblnp        iticccm
087100130827     c                   endif
087200130827     c                   endif
087300091216     c                   endif
087400070329     c                   eval      itictic=v2ctic
087500070329     c                   eval      iticticbol=ar9tic
087600070329     c                   eval      iticdiv=ar9vca
087700070329     c                   eval      iticcas=v2ccas
087800070329     c                   eval      iticpgm=n_pgm
087900070329     c                   call      'TRULTICR'
088000070329     c                   parm                    TRULTICDS
088100070329     c                   parm                    ds1a
088200070329     c
088300070329    2c                   if        oticric='T'
088400070329     c                   movel     otictic       v2ctic
088500070329     C                   SETON                                        4190
088600070329     C                   GOTO      ENDCT8
088700070329    2c                   endif
088800070329     c* Errore tipo incasso
088900091211    2c                   if        oticerr='T' or oticerr='C'
089000070329     C                   MOVEL     oticmsg       VIDMSG
089100070329     C                   SETON                                        419028
089200070329     C                   GOTO      ENDCT8
089300070329    2C                   ENDIF
089400070329     c* Errore divisa
089500070329    2c                   if        oticerr='D'
089600070329     C                   MOVEL     oticmsg       VIDMSG
089700070329     C                   SETON                                        419028
089800070329     C                   GOTO      ENDCT8
089900070329    2C                   ENDIF
090000070329     c*
090100070329    2c                   if        v2ctic<>pre_tic
090200070329     c                   clear                   forzacas
090300070329     c                   eval      pre_tic=v2ctic
090400091216     c                   setoff                                       15
090500091216     c* aggiorno "intestare a" a video nel caso fosse cambiato il tipo incasso C/A
090600091216    3c                   if        dateu>=§VPOCAS
090700091216     c                   clear                   trulintds
090800091216     c                   eval      iinttic=v2ctic
090900091216     c                   eval      iinttip='I'
091000091216     c                   eval      IINTRSCM=arbrsm
091100091216     c                   eval      IINTRSCO=arbrmo
091200091216     c                   eval      IINTcdi=s_ar5cdi
091300091216     c                   call      'TRULINTR'
091400091216     c                   parm                    trulintds
091500091216    4c                   if        ointerr=*blanks
091600091216     c                   seton                                        15
091700091216     c                   eval      v2ccdi=ointrsci
091800091216    4C                   endif
091900091216     c                   seton                                        90
092000091216     c                   goto      endct8
092100091216    3c                   endif
092200070329    2c                   endif
092300070329     c
092400070329      * Controllo il c/assegno tramite il trul21r
092500070329     c                   clear                   trul21ds
092600070329     c                   eval      i21cbo = arbcbo
092700070329     c                   eval      i21tsp = arbtsp
092800070329     c                   eval      i21lnp = arblnp
092900070329     c                   eval      i21nzm = arbnzm
093000070329     c                   eval      i21lna = arblna
093100070329     c                   eval      i21nzd = arbnzd
093200130827     c******             eval      i21ksc = arbksc
093300070329     c                   movel     arbctr        i21ctr
093400070329     c                   eval      i21tic = v2ctic
093500070329     c                   eval      i21imp = v2ccas
093600070329     c                   eval      i21div = v2cvca
093700070329     c                   eval      i21ute = knmus
093800070329     c                   eval      i21pgm = n_pgm
093900130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
094000130827     c                   if        %subst(§3atb1:1:1)='F'
094100130827     c                   z-add     arbksc        i21ksc
094200130827     c                   else
094300130827     c                   z-add     arbccm        i21ksc
094400130827     c                   if        i21ksc=0
094500130827     c                   z-add     8888          i21ksc
094600130827     c                   movel     arblnp        i21ksc
094700130827     c                   endif
094800130827     c                   endif
094900070329     c                   call      'TRUL21R'
095000070329     c                   parm                    trul21ds
095100070329      * controllo che non sia minore del limite previsto
095200070329    2c                   if        o21fmn <> *blanks
095300070329     c                   eval      vidmsg = msg(7)
095400070329     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
095500070329     c                   eval      *in41 = *on
095600070329     c                   eval      *in90 = *on
095700070329     c                   eval      *in28 = *on
095800070329     c                   goto      endct8
095900070329    2c                   endif
096000070329      * controllo che non sia maggiore del limite previsto
096100070329      * non forzabile
096200070329    2c                   if        o21fx2 <> *blanks
096300070329     c                   eval      vidmsg = msg(9)
096400070329     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
096500070329     c                   eval      *in41 = *on
096600070329     c                   eval      *in90 = *on
096700070329     c                   eval      *in28 = *on
096800070329     c                   goto      endct8
096900070329    2c                   endif
097000070329      * controllo che non sia maggiore del limite per bancanri bartolini no
097100070329      * abilitati da sede, non forzabile
097200070329    2c                   if        o21fb <> *blanks
097300070329     c                   eval      vidmsg = msg(10)
097400070329     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
097500070329     c                   eval      *in41 = *on
097600070329     c                   eval      *in90 = *on
097700070329     c                   eval      *in28 = *on
097800070329     c                   goto      endct8
097900070329    2c                   endif
098000070329      * controllo che non sia maggiore del limite previsto
098100070329      * forzabile
098200070329    2c                   if        o21fx1 <> *blanks and forzacas = *blanks
098300070329     c                   eval      forzacas = '1'
098400070329     c                   eval      vidmsg = msg(8)
098500070329     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
098600070329     c                   eval      *in41 = *on
098700070329     c                   eval      *in90 = *on
098800070329     c                   eval      *in28 = *on
098900070329     c                   goto      endct8
099000070329    2c                   endif
099100070329    1c                   endif
099200070329     c* DATA CONSEGNA RICHIESTA:richiamo fnlr48r
099300141030    1c                   if        v2cdcr>0 or v2chcr>0
099400070329     c                   clear                   v2cdcr_i
099500070329     c* controllo formale data
099501141030     c                   if        v2cdcr>0
099600070329     c                   clear                   wlbda8
099700070329     C                   Z-ADD     v2cdcr        G02DAT
099800070329     C                   MOVEL     *BLANK        G02ERR
099900070329     C                   CALL      'XSRDA8'
100000070329     C                   PARM                    WLBDA8
100100070329    2c     g02err        ifeq      '1'
100200070329     c                   seton                                        409028
100300070329     c                   eval      vidmsg=msg(2)
100400070329     c                   goto      endct8
100500070329    2c                   endif
100600070329     c                   z-add     g02inv        v2cdcr_i
100700070329     c                   z-add     g02dat        v2cdcr
100701141030     c                   endif
100800070329     c                   clear                   fnlr48ds
100900070329     c                   eval      d48cvb='CR'
101000070329     c                   eval      d48ffr='F'
101100070329     C                   z-add     ilrg2aas      D48AAS
101200070329     C                   z-add     ilrg2lnp      D48LNP
101300070329     C                   z-add     ilrg2nrs      D48NRS
101400070329     C                   z-add     ilrg2nsp      D48NSP
101500070329     C                   MOVEL     ARBCBO        D48CBO
101600070329     C                   MOVEL     'A'           D48TBO
101700070329     C                   MOVEL     'G'           D48trc
101800070329     C                   MOVEL     fnlr48ds      KPJBU
101900070329     c                   movel     v2ctcr        §BGTCR
102000070329     c                   z-add     v2chcr        §BGHCR
102100070329     c                   z-add     v2cdcr_I      §BGDCR
102200070329     c                   movel     arbgc1        §BGGC1
102300070329     c                   movel     arbgc2        §BGGC2
102400070329     c                   movel     arbtc1        §BGTC1
102500070329     c                   movel     arbtc2        §BGTC2
102600070329     c                   call      'FNLR48R'
102700070329     C                   PARM                    KPJBA
102800070329     C                   PARM                    DSARBD
102900070329     C                   PARM                    DSARBK
103000070329     C                   PARM                    DARBT
103100070329     C                   PARM                    DSARBG
103200070329     C                   PARM                    trul90ds
103300070329     C                   MOVEL     KPJBU         fnlr48ds
103400070329    2c                   if        d48err<>*blanks
103500070329     c                   movel     d48msg        vidmsg
103600070329     c                   seton                                        409028
103700070329     c                   goto      endct8
103800070329    2c                   endif
103900070329    1c                   endif
104000070329     c* ESITO - Se f10=abilitazione rapida l'f10 deve essere = *blanks
104100070329    1c                   if        *inkj='1' and v2cesi<>*blanks
104200070329     c                   eval      vidmsg=msg(3)
104300070329     c                   seton                                        429028
104400070329     c                   goto      endct8
104500070329     c                   else
104600070329     c* vicecersa esito obbligatorio per no f10
104700070329    2c                   if        *inkj='0' and v2cesi=*blanks
104800070329     c                   eval      vidmsg=msg(4)
104900070329     c                   seton                                        429028
105000070329     c                   goto      endct8
105100070329    2c                   endif
105200070329    1c                   endif
105300070329     c*
105400070329    1c                   if        v2cesi<>*blanks
105500070329     c                   clear                   werr              1
105600070329    2c                   select
105700070329     c                   when      ilrg2tss='S'
105800070329    3c                   if        v2cesi<>'D'
105900070329     c                   eval      werr='1'
106000070329    3c                   endif
106100070329     c                   when      ilrg2cau='S' or ilrg2cau='C' or
106200070410     c                             ilrg2cau='L' or ilrg2cau='A'
106300070329    3c                   if        v2cesi<>'A' and v2cesi<>'R' and v2cesi<>
106400070329     C                             'E'
106500070329     c                   eval      werr='1'
106600070329    3c                   endif
106700070329     c* per il lasciato avviso se inserita data consegna richiesta l'esito
106800070329     c* deve essere per forza 'A'
106900070329    3c                   if        ilrg2cau='L' and v2cesi<>'A'
107000070403     c                             and v2cdcr>0
107100070329     c                   seton                                        429028
107200070329     c                   eval      vidmsg=msg(6)
107300070329     c                   goto      endct8
107400070329    3c                   endif
107500070410     c*                  when      ilrg2cau='A'
107600070410    3c*                  if        v2cesi<>'A' and v2cesi<>'R'
107700070410     c*                  eval      werr='1'
107800070410    3c*                  endif
107900070329     c                   when      ilrg2cau='F'
108000070329   3 c                   if        v2cesi<>'C' and v2cesi<>'R' and v2cesi<>
108100070329     c                             'E'
108200070329     c                   eval      werr='1'
108300070329   3 c                   endif
108400070329   2 c                   endsl
108500070329   2 c                   if        werr='1'
108600070329     c                   seton                                        429028
108700070329     c                   eval      vidmsg=msg(5)
108800070329     c                   goto      endct8
108900070329   2 c                   endif
109000070329   1 c                   endif
109100070329     c*
109200070329     c* se richiesta abilitazione rapida errore se immessi o variati dati
109300070329    1c                   if        *inkj='1' and
109400070329     c                             (v2cnot8<>v2hnot8 or v2cnot9<>v2hnot9
109500070330     c                             or v2cdcr>0 or v2ctic<>s_v2ctic)
109600070329     c                   seton                                        429028
109700070329     c                   eval      vidmsg=msg(11)
109800070329     c                   goto      endct8
109900070329    1c                   endif
109901141013     c* se richiesta conferma (F6 o f10) e presenti dispo da caricare --> errore
109902141013     c                   if        (*inkj='1' or *inkf='1') and OLRY09ESIT='1'
109903141013     c                   seton                                        429028
109904141013     c                   eval      vidmsg=msg(15)
109905141013     c                   goto      endct8
109906141013     c                   endif
110000070315      *
110100070329     C     endct8        ENDSR
110200070316     C*--- CHAIN TABEL00F --------------------------------------------*
110300070316     C     CTABEL        BEGSR
110400070316     C     KTAB          CHAIN     TABEL                              30
110500070316     c                   if        %found(tabel00f) and tblflg<> ' '
110600070316     C                   SETON                                        30
110700070316     C                   END
110800070316     C                   ENDSR
110900070614     c**********************************************************************
111000070614     c* Gestione window per immissione codice giacenza
111100070614     c**********************************************************************
111200070614     C     gesw01        BEGSR
111300070614     c     emw01         tag
111400070614     c                   exfmt     lrg2w01
111500070614     c                   setoff                                       2846
111600070614     c                   clear                   w2dcmc
111700070614     c                   clear                   widmsg
111800070712     c                   if        *inkl='1'
111900070712     c                   goto      e_emw01
112000070712     c                   endif
112100070614     C     '?'           SCAN      w2ccmc:1                               80
112200070712    1C     *IN80         IFEQ      *ON
112300070926     c                   clear                   TRTB2Ads
112400070926     c                   eval      TB2Aopz = '1'
112500070926     c                   eval      TB2Atip = 'G'
112600070926     c                   movel(p)  TRTB2Ads      KPJBU
112700070926     c                   call      'TRTB2AR'
112800070926     c                   parm                    KPJBA
112900070926     c                   movel     KPJBU         TRTB2Ads
113000070926     c                   movel     TB2Acod       W2Ccmc
113100070926     c                   movel     TB2Auni       ds2A
113200070926     c                   movel     §2Ades        W2Dcmc
113300070924     c                   write     lrg2t01
113400070924     c                   write     lrg2d01
113500070924     c                   goto      emw01
113600070712    1C                   endif
113700070614     c*
113800070712    1c                   if        w2ccmc=*blanks  and
113900070712     c                              (ilrg2cau <> 'L' and
114000070712     c                               ilrg2cau <> 'F')
114100070704     C                   SETON                                        2846
114200070704     c                   movel     msg(14)       widmsg
114300070704     c                   goto      emw01
114400070712    1c                   endif
114500070712      *
114600070712    1c                   if        w2ccmc <> *blanks
114700070712      *
114800070614     c                   clear                   ds2a
114900070614     c                   move      '2A'          tblcod
115000070614     c                   movel(p)  w2ccmc        tblkey
115100070614     C                   exsr      ctabel
115200070614     c* errore record non trovato
115300070614    2C                   if        *in30
115400070614     C                   SETON                                        2846
115500070614     c                   movel     msg(12)       widmsg
115600070614     c                   goto      emw01
115700070614    2c                   endif
115800070614     C                   MOVEL     tbluni        ds2a
115900070619     c                   movel     §2ades        w2dcmc
116000070614     c* errore codice giacenza non utilizzabile
116100081103    2c                   if        §2aftc='G'  and §2atip='G'  and
116200081103     c                             §2aflg=' '
116300081103    2c                   else
116400070614     C                   SETON                                        2846
116500070614     c                   movel     msg(13)       widmsg
116600070614     c                   goto      emw01
116700070614    2c                   endif
116800070712      *
116900070712    1c                   endif
117000070712      *
117100070703     c  nkf              goto      emw01
117200070712     c     e_emw01       endsr
117300070321     c**********************************************************************
117400070321     c* F6=Conferma
117500070321     c**********************************************************************
117600070321     C     Confe         BEGSR
117700070703     c* se immesso esito "E"=Esclusa da gestione-attesa giacenza emetto
117800070703     c* finestra per eventuale immissione codice giacenza
117900070704     c                   if        v2cesi='E'
118000070703     c                   exsr      gesw01
118100070712     c                   if        *inkl
118200070712     c                   seton                                        90
118300070712     c                   goto      f_confe
118400070712     c                   endif
118500070703     c                   endif
118600070328     c* DATA CONSEGNA RICHIESTA: E' il primo aggiornamento che faccio
118700070328     c* perchè il richiamo con "CS" deve essere fatto con emissione videata
118800070328     c* della manutenzione bolle dalla quale è possibile uscire con f12 ed
118900070328     c* in questo caso non devo fare nessun aggiornamento
119000070329    1C                   IF        V2CDCR>0
119100070328     c                   clear                   fnlr48ds
119200070330    2c                   if        arbtc1<>'S' and arbtc2<>'S'
119300070328     c                   eval      d48cvb='CR'
119400070328     c                   eval      d48ffr='E'
119500070328     c                   else
119600070328     c                   eval      d48cvb='CS'
119700070328     c                   eval      d48ffr='V'
119800070328     c                   eval      d48f12='S'
119900070329    2c                   endif
120000070328     C                   z-add     ilrg2aas      D48AAS
120100070328     C                   z-add     ilrg2lnp      D48LNP
120200070328     C                   z-add     ilrg2nrs      D48NRS
120300070328     C                   z-add     ilrg2nsp      D48NSP
120400070328     C                   MOVEL     ARBCBO        D48CBO
120500070328     C                   MOVEL     'A'           D48TBO
120600070328     C                   MOVEL     'S'           D48F12
120700070328     C                   MOVEL     'G'           D48trc
120800070328     C                   MOVEL     fnlr48ds      KPJBU
120900070328     c                   movel     v2ctcr        §BGTCR
121000070328     c                   z-add     v2chcr        §BGHCR
121100070328     c                   z-add     v2cdcr_I      §BGDCR
121200070328     c                   movel     arbgc1        §BGGC1
121300070328     c                   movel     arbgc2        §BGGC2
121400070328     c                   movel     arbtc1        §BGTC1
121500070328     c                   movel     arbtc2        §BGTC2
121600070328     c                   call      'FNLR48R'
121700070328     C                   PARM                    KPJBA
121800070328     C                   PARM                    DSARBD
121900070328     C                   PARM                    DSARBK
122000070328     C                   PARM                    DARBT
122100070328     C                   PARM                    DSARBG
122200070328     C                   PARM                    trul90ds
122300070328     C                   MOVEL     KPJBU         fnlr48ds
122400070328     c* Ritorno con errore o premuto f12
122500070329    2c                   if        d48err<>*blanks or d48f12='1'
122600070328     c                   seton                                        90
122700070329    3c                   if        d48err<>*blanks
122800070328     c                   seton                                        28
122900070328     c                   movel     d48msg        vidmsg
123000070329    3c                   endif
123100070328     c                   goto      f_confe
123200070329    2c                   endif
123300070329    1c                   endif
123400070322
123500070327     c*  N O T E ************
123600070322     c* Note di ar4 trk 8 e 9
123700070420    1c                   if        v2hnot8<>v2cnot8 or v2hnot9<>v2cnot9
123800131008     c* richaimo pgm di manutenzione bolle per aggiornamento delle note
123900131008     c                   clear                   fnlr48ds
124000131008     c                   eval      d48cvb='NT'
124100131008     c                   eval      d48ffr='E'
124200131008     C                   z-add     ilrg2aas      D48AAS
124300131008     C                   z-add     ilrg2lnp      D48LNP
124400131008     C                   z-add     ilrg2nrs      D48NRS
124500131008     C                   z-add     ilrg2nsp      D48NSP
124600131008     C                   MOVEL     ARBCBO        D48CBO
124700131008     C                   MOVEL     'A'           D48TBO
124800131008     C                   MOVEL     'S'           D48F12
124900131008     C                   MOVEL     'N'           D48trc
125000131008     C                   MOVEL     fnlr48ds      KPJBU
125100131008     c                   eval      §bnu_sn='S'
125200131008     c                   eval      §bnuni70=v2cnot8+v2cnot9
125300131008     c                   eval      §bntrd='NOT'
125400131008     c                   call      'FNLR48R'
125500131008     C                   PARM                    KPJBA
125600131008     C                   PARM                    DSARBD
125700131008     C                   PARM                    DSARBK
125800131008     C                   PARM                    DARBT
125900131008     C                   PARM                    DSARBG
126000131008     C                   PARM                    trul90ds
126100131008     C                   PARM                    DSARBN
126200131008     C                   MOVEL     KPJBU         fnlr48ds
126300131008
126400131008     c* Ritorno con errore o premuto f12
126500131008    2c                   if        d48err<>*blanks
126600131008     c                   seton                                        90
126700131008     c                   seton                                        28
126800131008     c                   movel     d48msg        vidmsg
126900131008     c                   goto      f_confe
127000131008    2c                   endif
127100131008    1c                   endif
127200131008     c
127300070322     c* verifico se presente ar4 trk "a"
127400131008     c**
127500131008     C**                 MOVEL     'a'           WTRC
127600131008     c**   kar4          setll     fiar401l
127700131008    2c**                 if        not %equal(fiar401l)
127800131008     c**                 clear                   fnlv19ds
127900131008     C**                 MOVEL     'T'           D19ftr
128000131008     C**                 MOVEL     'a'           D19trc
128100131008     C**                 MOVEL     arbAAS        D19AAS
128200131008     C**                 MOVEL     arbLNP        D19LNP
128300131008     C**                 MOVEL     arbNRS        D19NRS
128400131008     C**                 MOVEL     arbNSP        D19NSP
128500131008     C**                 MOVEL     V2hnot8       D19NT1
128600131008     C**                 clear                   D19NT2
128700131008     c**                 call      'FNLV19R'
128800131008     c**                 parm                    fnlv19ds
128900131008     c**                 parm      'S'           p19vuo            1
129000131008     c**
129100131008    3c**                 if        v2hnot9<>*blanks
129200131008     C**                 MOVEL     'b'           D19trc
129300131008     C**                 MOVEL(P)  V2hnot9       D19NT1
129400131008     C**                 clear                   D19NT2
129500131008     c**                 call      'FNLV19R'
129600131008     c**                 parm                    fnlv19ds
129700131008    3c**                 endif
129800131008    2c**                 endif
129900070327     c* aggiorno/deleto trk 8 e 9 se variati
130000131008     c**                 clear                   fnlv19ds
130100131008     C**                 MOVEL     'T'           D19ftr
130200131008     C**                 MOVEL     arbAAS        D19AAS
130300131008     C**                 MOVEL     arbLNP        D19LNP
130400131008     C**                 MOVEL     arbNRS        D19NRS
130500131008     C**                 MOVEL     arbNSP        D19NSP
130600131008     C**                 MOVEL     V2cnot8       D19NT1
130700131008     C**                 MOVEL     V2cnot9       D19NT2
130800131008    2C**   V2cnot8       IFNE      *BLANKS
130900131008     C**   V2cnot9       ORNE      *BLANKS
131000131008     C**                 MOVEL     '8'           D19TRC
131100131008     C**                 CALL      'FNLV19R'
131200131008     C**                 PARM                    fnlv19ds
131300131008    2C**                 ENDIF
131400131008     c**Se il pgm mi restituisce la seconda riga vuota la deleto perchè
131500131008     c**prima poteva esistere
131600131008    2C**   D19ERR        IFEQ      *BLANKS
131700131008     C**   D19NT1        ANDNE     *BLANKS
131800131008     C**   D19NT2        ANDEQ     *BLANKS
131900131008     C**                 MOVEL     '9'           D19TRC
132000131008     C**                 CLEAR                   D19NT1
132100131008     C**                 CLEAR                   D19NT2
132200131008     C**                 CALL      'FNLV19R'
132300131008     C**                 PARM                    fnlv19ds
132400131008   x2C**                 else
132500131008     C**Se tornato codice di err richiamo il pgm 2 volte:  una per nota
132600131008    3C**   D19ERR        IFNE      *BLANKS
132700131008     C**   v2cnot8       oreq      *BLANKS
132800131008     C**   v2cnot9       ANDEQ     *BLANKS
132900131008     C**Nota 1
133000131008     C**                 MOVEL     '8'           D19TRC
133100131008     C**                 MOVEL     v2cnot8       D19NT1
133200131008     C**                 CLEAR                   D19NT2
133300131008     C**                 CALL      'FNLV19R'
133400131008     C**                 PARM                    fnlv19ds
133500131008     C**Nota 2
133600131008     C**                 MOVEL     '9'           D19TRC
133700131008     C**                 MOVEL     v2cnot9       D19NT1
133800131008     C**                 CLEAR                   D19NT2
133900131008     C**                 CALL      'FNLV19R'
134000131008     C**                 PARM                    fnlv19ds
134100131008    3C**                 ENDIF
134200131008    2C**                 ENDIF
134300131008    1c**                 endif
134400131008     c
134500131008     c* ALLOCO PRINCIPALI RECORD
134600131008     c                   exsr      alloc
134700131008    1c                   if        *in90='1'
134800131008     c                   goto      f_confe
134900131008    1c                   endif
135000131008     c
135100070328     c* TIPO INCASSO C/ASSEGNO
135200070330     c                   if        ilrg2cau='C' and v2ctic<>s_v2ctic
135300070328     c                   movel     v2ctic        ar9tic
135400070328     C                   z-add     dateu         AR9duv
135500070328     C                   UPDATE    FiAR9000
135600091217     c* Aggiorno FIAR5 recrord GEN
135700091217     c                   exsr      RegisAr5GEN
135800070328     c                   endif
135900070327     c* SCRITTURA NOTE R.A.
136000070330     c                   exsr      sr_noteRa
136100070328     c* ESITO
136200070328     c                   select
136300070328     c* abilitato
136400070328     c                   when      v2cesi='A'
136500070328     c                   select
136600070328     c                   when      ilrg2cau='S' or ilrg2cau='A'
136700070328     c                   clear                   §b4lbas
136800070328     c                   when      ilrg2cau='C'
136900070328     c                   clear                   §b4ltca
137000070328     C                   MOVEL     'S'           ARBACC
137100070328     c                   when      ilrg2cau='L'
137200070419     c     karb          setgt     fnevb22l
137300070419     c     karb          readpe    fnevb22l
137400070419     c                   if        not %eof(fnevb22l)
137500070328     c                   clear                   evb2tel
137600070328     c                   update    fnevb200
137700070328     c                   endif
137800070402     c* Devo cliearare anche arbfbc se non ho richiamato fnlr48r
137900070402     c                   if        v2cdcr=0
138000070402     c                   clear                   arbfbc
138100070419     c                   clear                   arbcmc
138200070402     c                   endif
138300070328     c                   endsl
138400070328     c
138500070328     c* contattato (per fermo deposito)
138600070328     c                   when      v2cesi='C'
138700070328     c                   clear                   §b4ltfd
138800070328     c* da ricontattare
138900070328     c                   when      v2cesi='R'
139000070328     c                   select
139100070328     c                   when      ilrg2cau='S' or ilrg2cau='A'
139200070328     c                   eval      §b4lbas='T'
139300070328     c                   when      ilrg2cau='C'
139400070328     c                   eval      §b4ltca='T'
139500070328     c                   when      ilrg2cau='L'
139600070419     c     karb          setgt     fnevb22l
139700070419     c     karb          readpe    fnevb22l
139800070419     c                   if        not %eof(fnevb22l)
139900070328     c                   eval      evb2tel='T'
140000070328     c                   update    fnevb200
140100070328     c                   endif
140200070328     c                   when      ilrg2cau='F'
140300070328     c                   eval      §b4ltfd='T'
140400070328     c                   endsl
140500070328     c* esclusa da gestione-attesa giacenza
140600070328     c                   when      v2cesi='E'
140700070328     c                   select
140800070410     c                   when      ilrg2cau='S' or ilrg2cau='A'
140900070328     c                   eval      §b4lbas='N'
141000070328     c                   when      ilrg2cau='C'
141100070328     c                   eval      §b4ltca='N'
141200070328     c                   when      ilrg2cau='L'
141300070419     c     karb          setgt     fnevb22l
141400070419     c     karb          readpe    fnevb22l
141500070419     c                   if        not %eof(fnevb22l)
141600070328     c                   eval      evb2tel='N'
141700070328     c                   update    fnevb200
141800070328     c                   endif
141900070328     c                   when      ilrg2cau='F'
142000070328     c                   eval      §b4ltfd='N'
142100070328     c                   endsl
142200070615     c                   if        w2ccmc<>*blanks
142300070615     c                   movel     'G'           arbfbc
142400070615     c                   movel     w2ccmc        arbcmc
142500070615     c* scrivo evento
142600070615     c                   exsr      sr_evb
142700070615     c                   endif
142800070328     c* da contattare
142900070328     c                   when      v2cesi='D'
143000070328     c                   eval      §b4lbas='S'
143100070328     c                   endsl
143200070329     c* Rilascio e aggiorno ar4 trk "L"
143300070329     c                   exsr      aggar4l
143400070328     c* Rilascio e aggiorno il record bolla
143500070402     c* Abilitazione contrassegno (e lasciato avviso se non richiamato fnlr48R)
143600070615     c                   if        (v2cesi='A' and (ilrg2cau='C' or (
143700070615     c                             ilrg2cau='L' and v2cdcr=0))) or
143800070615     c                             (v2cesi='E' and w2ccmc<>*blanks)
143900070328     C                   update    fnarb000
144000070328     c                   else
144100070621     c                   unlock    fnarb00f
144200070328     c                   endif
144300070328     c* GESTIONE NOTE LDV (Tengo alla fine questo aggiornamento:è meno
144400070328     c* importante e siccome potrebbe potenzialmente dare errore di allocaz.
144500070328     c* in fase di update di fiarn per trasformare i record come storici
144600070328     c* lo tengo alla fine)
144700070328     c****               eval      v2cnra=v2cnt1+v2cnt2+v2cnt3+v2cnt4
144800070328     c                   if        v2cnra<>*blanks
144900140130     c                   clear                   fnlrn6ds
145000140130     c                   movel     'I'           IN6MOD
145100140130     c                   z-add     arbaas        IN6AAS
145200140130     c                   z-add     arblnp        IN6LNP
145300140130     c                   z-add     arbnrs        IN6NRS
145400140130     c                   z-add     arbnsp        IN6NSP
145500140130     c                   movel     N_pgm         IN6PGM
145600140130     c                   movel     v2cnt1        IN6NOt1
145700140130     c                   movel     v2cnt2        IN6NOt2
145800140130     c                   movel     v2cnt3        IN6NOt3
145900140130     c                   movel     v2cnt4        IN6NOt4
146000140130     c                   call      'FNLRN6R'
146100070329     c                   parm                    kpjba
146200140130     c                   parm                    fnlrn6ds
146300070328     c                   endif
146400070327     c
146500070322     c
146600070322     c
146700070322     c     f_confe       endsr
146800070321     c**********************************************************************
146900070321     c* F10=Abilitazione rapida
147000070321     c**********************************************************************
147100070321     C     Rapida        BEGSR
147200070329     c* ALLOCO I PRINCIPALI  RECORD IN AGGIORNAMENTO
147300070329     c                   exsr      alloc
147400070329     c                   if        *in90='1'
147500070329     c                   goto      frapida
147600070329     c                   endif
147700070330     c* NOTE R.A.
147800070330     c                   exsr      sr_noteRa
147900070329     c* ESITO
148000070329     c* abilitato/contattato
148100070329     c                   select
148200070329     c                   when      ilrg2cau='S' or ilrg2cau='A'
148300070329     c                   clear                   §b4lbas
148400070329     c                   when      ilrg2cau='C'
148500070329     c                   clear                   §b4ltca
148600070329     C                   MOVEL     'S'           ARBACC
148700070329     c                   when      ilrg2cau='L'
148800070419     c     karb          setgt     fnevb22l
148900070419     c     karb          readpe    fnevb22l
149000070419     c                   if        not %eof(fnevb22l)
149100070329     c                   clear                   evb2tel
149200070329     c                   update    fnevb200
149300070329     c                   endif
149400070329     c                   clear                   arbfbc
149500070419     c                   clear                   arbcmc
149600070329     c                   when      ilrg2cau='F'
149700070329     c                   clear                   §b4ltfd
149800070329     c                   endsl
149900070329     c                   exsr      aggar4l
150000070329     c                   if        ilrg2cau='C' or ilrg2cau='L'
150100070329     c                   update    fnarb000
150200070329     c                   else
150300070621     c                   unlock    fnarb00f
150400070329     c                   endif
150500070329     c     frapida       endsr
150600070330     c**********************************************************************
150700070330     c* Scrittura Note R.A.
150800070330     c**********************************************************************
150900070330     C     sr_noteRa     BEGSR
151000140508     c                   clear                   fidna6ds
151001140508     C                   CLEAR                   DTGD01
151100070330    1c                   select
151200070330     c                   when      ilrg2cau='L'
151300140508     c                   eval      ida06mad='  2'
151400070330     c                   when      ilrg2cau='C'
151500140508     c                   eval      ida06mad=' 10'
151600070330     c                   when      ilrg2cau='S'
151700140508     c                   eval      ida06mad=' 14'
151800070330     c                   when      ilrg2cau='A'
151900140508     c                   eval      ida06mad=' 15'
152000070330     c                   when      ilrg2cau='F'
152100140508     c                   eval      ida06mad=' 16'
152200070330    1c                   endsl
152300140508     c                   eval      ida06tor='S'
152400140508     c                   eval      ida06ogg=%editc(arblnp:'X')+
152401140508     c                             %editc(arbnrs:'X')+
152500070330     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
152600140508     c                   eval      ida06no1=v2cnt1+v2cnt2+v2cnt3+v2cnt4
152700140508     c* Impostazione campo ida06no2:elenco delle operazioni effettuate
152800070330     c* esito
152900070330    1c                   select
153000070417     c                   when      v2cesi='A'
153100140508     c                   eval      ida06no2='Esito:abilitata.'
153200070417     c                   when      *inkj
153300140508     c                   eval      ida06no2='Esito:abilitazione rapida'
153400070330     c                   when      v2cesi='C'
153500140508     c                   eval      ida06no2='Esito:contattato.'
153600070330     c                   when      v2cesi='R'
153700140508     c                   eval      ida06no2='Esito:da ricontattare.'
153800070330     c                   when      v2cesi='D'
153900140508     c                   eval      ida06no2='Esito:da contattare.'
154000070712     c                   when      v2cesi='E' and w2ccmc<>*blanks
154100140508     c                   eval      ida06no2=
154101140508     c                             'Esito:Esclusa gest.- apertura giac.'
154200070712     c                   when      v2cesi='E'
154300140508     c                   eval      ida06no2=
154301140508     c                             'Esito:Esclusa da gest.-attesa giac.'
154400070330    1c                   endsl
154500070330     c* dcr
154600070330    1c                   if        v2cdcr>0
154700070330     c                   clear                   fnlv80ds
154800070330     c                   eval      ilv80aas=arbaas
154900070330     c                   eval      ilv80lnp=arblnp
155000070330     c                   eval      ilv80nrs=arbnrs
155100070330     c                   eval      ilv80nsp=arbnsp
155200070330     c                   eval      ilv80mgs=arbmgs
155300070330     c                   eval      ilv80tcr=v2ctcr
155400070330     c                   eval      ilv80dcr=v2cdcr_i
155500070410     c                   eval      ilv80hcr=v2chcr
155600070330     c                   eval      ilv80tc1=arbtc1
155700070330     c                   eval      ilv80tc2=arbtc2
155800070330     c                   call      'FNLV80R'
155900070330     c                   parm                    kpjba
156000070330     c                   parm                    fnlv80ds
156100070330     c                   eval      W_dtcr=olv80tcrd
156200140508     c                   eval      %subst(ida06no2:36:10)='Cons.Rich:'
156300140508     c                   eval      ida06no2=%trim(ida06no2)+%trim(w_dtcr)
156400140508     c                   eval      ida06no2=%trim(ida06no2)+' '+
156500070330     c                             %editw(v2cdcr:'  /  /    ')
156600070330    2c                   if        v2chcr>0
156700140508     c                   eval      ida06no2=%trim(ida06no2)+' ore'
156800070330    3c                   if        v2ctcr=*blanks
156900140508     c                   eval      ida06no2=%trim(ida06no2)+' '+
157000070330     c                             %editw(v2chcr:'  :  ')
157100070330     c                   else
157200140508     c                   eval      %subst(ida06no2:71:5)=
157300070330     c                             %editw(v2chcr:'  :  ')
157400070330    3c                   endif
157500070330    2c                   endif
157600070330    1c                   endif
157700070330     c* Tipo incasso
157800070330    1c                   if        v2ctic<>s_v2ctic
157900070615     c                   exsr      sr_cerpos
158000070615     c                   if        n<>999
158100140508     c                   eval      %subst(ida06no2:N:23)=
158101140508     c                             'Variato Tp.inc. C/A da '
158200140508     c                   eval      ida06no2=%trim(ida06no2)+
158300070330     c                             '"'+S_v2ctic+'" in "'+v2ctic+'"'
158400070615     c                   endif
158500070330    1c                   endif
158600070615     c* codice giacenza
158700070620     c                   if        v2cesi='E' and w2ccmc<>*blanks
158800070615     c                   exsr      sr_cerpos
158900070615     c                   if        n<>999
159000070619     c                   movel     w2dcmc        w2dcmc22         22
159100140508     c                   eval      %subst(ida06no2:N:9)='Cd.giac.:'
159200140508     c                   eval      ida06no2=%trim(ida06no2)+
159300070620     c                             w2ccmc+'-'+w2dcmc22
159400070615     c                   endif
159500070615     c                   endif
159600090918     c* flag per segnalazione abilitazione rapida
159700140508     c   kj              eval      §tgdarap='S'
159800140508     c  nkj              eval      §tgdarap='C'
159801141003     c****               eval      dtgd01= Ida06flo
159802141003     c                   eval      Ida06flo = dtgd01
159900070330     c
159901140508     c                   eval      ida06ute=knmus
159902140508     c                   eval      ida06fil=dutpou
159903140508
160000070330    1c                   if        ilrg2tch<>'R'
160100140508     c                   call      'FIDNA6R'
160300140508     c                   parm                    fidna6ds
160400070330    1c                   endif
160500140508     c                   movel     ida06no1      olrg2no1
160600140508     c                   movel     ida06no2      olrg2no2
160700070330     c                   endsr
160800070615     c**********************************************************************
160900070615     C     SR_CERPOS     BEGSR
161000070615     c**********************************************************************
161100140508     c     ' '           checkr    ida06no2      N                 4 0
161200070615    2c                   select
161300070615     c                   when      N<=35
161400070615     c                   z-add     36            n
161500070615     c                   when      N<=70
161600070615     c                   z-add     71            n
161700070615     c                   when      N<=105
161800070615     c                   z-add     106           n
161900070615     c                   other
162000070615     c                   z-add     999           n
162100070615    2c                   endsl
162200070615     C                   ENDSR
162300070329     c**********************************************************************
162400070329     C     ALLOC         BEGSR
162500070329     c**********************************************************************
162600070329     c* ALLOCO I PRINCIPALI  RECORD IN AGGIORNAMENTO
162700070621     c     savnrr        chain(E)  fnarb00f
162800070329     c                   if        %error
162900070329     c                   seton                                        9028
163000070329     c                   eval      vidmsg=msg(1)
163100070329     c                   goto      f_alloc
163200070329     c                   endif
163300070329     c                   if        ilrg2cau<>'L'
163400070329     c                   if        sav4nrr>0
163500070329     c     sav4nrr       chain(E)  fiar400f
163600070329     c                   if        %error
163700070329     c                   eval      vidmsg=msg(1)
163800070329     c                   seton                                        9028
163900070329     c                   goto      f_alloc
164000070329     c                   endif
164100070329     c                   movel     F_ar4not      dsbl4l
164200070329     c                   else
164300070329     c                   clear                   dsbl4l
164400070329     c                   endif
164500070329     c                   endif
164600070330     c                   if        ilrg2cau='C'and v2ctic<>s_v2ctic
164700070329     c     karb          chain(E)  fiar901l
164800070329     c                   if        %error
164900070329     c                   eval      vidmsg=msg(1)
165000070329     c                   seton                                        9028
165100070329     c                   goto      f_alloc
165200070329     c                   endif
165300070329     c                   endif
165400070329     c     f_alloc       endsr
165500070329     c**********************************************************************
165600070329     C     AGGAR4L       BEGSR
165700070329     c**********************************************************************
165800070329     c* Rilascio e aggiorno ar4 trk "L"
165900070329     c                   if        ilrg2cau<>'L'
166000070329     c                   if        sav4nrr=0
166100070329     c                   clear                   fiar4000
166200070329     c                   z-add     ilrg2aas      f_ar4aas
166300070329     c                   z-add     ilrg2lnp      f_ar4lnp
166400070329     c                   z-add     ilrg2nrs      f_ar4nrs
166500070329     c                   z-add     ilrg2nsp      f_ar4nsp
166600070329     c                   movel     'L'           f_ar4trc
166700070329     c                   movel     'R'           f_ar4ftr
166800070329     c                   z-add     dateu         f_ar4dtr
166900070329     c                   endif
167000070329     c                   movel     dsbl4l        F_ar4not
167100070329     c                   z-add     dateu         F_ar4duv
167200070329     c                   if        sav4nrr=0
167300070329     c                   write     fiar4000
167400070329     c                   else
167500070329     c                   update    fiar4000
167600070329     c                   endif
167700070329     c                   endif
167800070329     c                   endsr
167900070411     c**********************************************************************
168000070411     c* Verifica abilitazione tasto F16-Gestione richieste assistenza
168100070411     c**********************************************************************
168200070411     C     SR_AbilF16    BEGSR
168300070411     c                   eval      TGDtor  = 'S'
168400070411     c                   eval      tgdogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
168500070411     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
168600070411     c     K02TGD02      setll     FITGD000
168700070411     c                   eval      *in64   = %equal(FITGD02L)
168800070411     c                   endsr
168801140922     c**********************************************************************
168802140922     c* Verifica abilitazione tasto F19-Indicazioni di Consegna da WEB
168803140922     c**********************************************************************
168804140922     C     SR_AbilF19    BEGSR
168805140922     c                   eval      *in63 = *off
168806141024     c                   clear                   v2dtxt1
168807141024     c                   clear                   v2dtxt2
168808141024     c                   clear                   fnlry09ds2
168809140922     c                   eval      ilry09tch='C'
168811140922     c                   eval      ILRY09AAS=ilrg2aas
168812140922     c                   eval      ILRY09LNP=ILRG2LNP
168813140922     c                   eval      ILRY09NRS=ILRG2NRS
168814140922     c                   eval      ILRY09NSP=ILRG2NSP
168815141024     c                   eval      kpjbu=fnlry09ds2
168816140922     c                   call      'FNLRY09R'
168817141013     c                   parm                    kpjba
168818141024     c                   eval      fnlry09ds2=kpjbu
168819141024     c                   if        olry09esit='1'
168820141024     c                   if        olry09esdi='0'
168821140922     c                   eval      *in63 = *on
168822141024     c                   eval      v2dtxt2='F19=Dispo Consegna in sospeso'
168823141024     c                   else
168824141030     c                   eval      v2dtxt1='Presente Dispo Consegna Dirot'
168825141024     c                   eval      v2dtxt2='tamento:non eseguire Contatto'
168826141024     c                   endif
168827140922     c                   endif
168828140922     c                   endsr
168900070618     c**********************************************************************
169000070618     c* Scrittura evento
169100070618     c**********************************************************************
169200070618     C     sr_evb        BEGSR
169300070621     c                   clear                   fnevbf00
169400070621     c* data/ora immissione evento
169500070621     c* data/ora evento
169600070621     c                   time                    w0140
169700070621     C                   MOVE      W0140         g02dat
169800070621     C                   MOVEL     *BLANK        G02ERR
169900070621     C                   CALL      'XSRDA8'
170000070621     C                   PARM                    WLBDA8
170100070621     C                   Z-ADD     G02INV        evbdtv
170200070621     c                   movel     w0140         evborv
170300070621     C                   Z-ADD     G02INV        evbdev
170400070621     c                   movel     w0140         evbhev
170500070621     c* filiale immissione evento
170600070621     c                   z-add     dutpou        evbfle
170700070621     c* codice utente video
170800070621     C                   Z-ADD     dutcou        EVBCDU
170900070621     c* key spedizione
171000070621     C                   clear                   VH2AAP
171100070621     C                   clear                   VH2LPP
171200070621     C                   clear                   VH2NRP
171300070621     C                   clear                   VH2NSP
171400070621     c* se bolla figlia memorizzo la spedizione della mamma
171500070621     c     karb          chain     fnlbl01l
171600070621     c                   if        %found(fnlbl01l) and lbllan=lbllap
171700070621     c                   movel     dsarb0        savarb
171800070621     C                   Z-ADD     LBLAAP        KAAS
171900070621     C                   Z-ADD     LBLLPP        KLNP
172000070621     C                   Z-ADD     LBLNRP        KNRS
172100070621     C                   Z-ADD     LBLNSP        KNSP
172200070621     c     karbp         chain     fnarb01l
172300070621     c                   if        %found(fnarb01l) and arbcca<>'2'
172400070621     c                             and arbcca<>'6'
172500070621     C                   Z-ADD     LBLAAP        VH2AAP
172600070621     C                   Z-ADD     LBLLPP        VH2LPP
172700070621     C                   Z-ADD     LBLNRP        VH2NRP
172800070621     C                   Z-ADD     LBLNSP        VH2NSP
172900070621     c                   endif
173000070621     c                   move      savarb        dsarb0
173100070621     c                   endif
173200070621     c                   if        vh2nsp>0
173300070621     C                   Z-ADD     VH2LPP        EVBLNP
173400070621     C                   Z-ADD     VH2AAP        EVBAAS
173500070621     C                   Z-ADD     VH2NRP        EVBNRS
173600070621     C                   Z-ADD     VH2NSP        EVBNSP
173700070621     c                   else
173800070621     C                   z-add     ilrg2aas      evbAAS
173900070621     C                   z-add     ilrg2lnp      evbLNP
174000070621     C                   z-add     ilrg2nrs      evbNRS
174100070621     C                   z-add     ilrg2nsp      evbNSP
174200070621     c                   endif
174300070621     c* codice evento
174400070621     c                   movel     w2ccmc        evbcev
174500070621     c                   write     fnevbf00
174600070618     c                   endsr
174700091217     c**********************************************************************
174800091217     c* Aggiornamento per record GEN ddel fiar5
174900091217     c**********************************************************************
175000091217     c     RegisAR5GEN   BEGSR
175100091217     c                   clear                   dar5gen
175200091217     c                   Eval      kAr5Trd = 'GEN'
175300091217     c     kfiar5        Chain     Fiar501l
175400091217      * Aggiorno Fiar5
175500091217if  1c                   If        %Found(Fiar501l)
175600091217     c                   movel     ar5uni        DAr5Gen
175700091217     c*
175800091217     c                   Eval      §Ar5ticmb=v2ctic
175900100119     c                   Eval      §Ar5flgmb='S'
176000091217     c                   Movel(p)  DAr5Gen       Ar5Uni
176100091217     c
176200091217      * se devo trasmettere alla sede sfleggo per la trasmissione
176300091217    2c                   If        §Ar5Trse = 'S'
176400091217     c                   Clear                   Ar5Ft2
176500091217    2c                   EndIf
176600091217     c                   Clear                   Ar5Ft3
176700091217     c                   Update    Fiar5000
176800091217      * Scrivo Fiar5
176900091217   x1c                   Else
177000091217      * se immessi i dati
177100091217     c                   Clear                   Fiar5000
177200091217     c                   Eval      Ar5Aas = ArbAas
177300091217     c                   Eval      Ar5Lnp = ArbLnp
177400091217     c                   Eval      Ar5Nrs = ArbNrs
177500091217     c                   Eval      Ar5Nsp = ArbNsp
177600091217     c                   Eval      Ar5Trd = 'GEN'
177700091217     c                   Move      Dateu         Ar5Dac
177800091217     c                   Movel     W0140         Ar5Orc
177900091217     c                   Clear                   Dar5Gen
178000091217     c
178100100119     c                   Eval      §Ar5flgmb='S'
178200091217     c                   Eval      §Ar5ticmb=v2ctic
178300091217     c                   Movel(p)  DAr5Gen       Ar5Uni
178400091217     c
178500091217    2c                   if        ar5uni<>*blanks
178600091217     c                   Write     Fiar5000
178700091217    2c                   EndIf
178800091217    1c                   EndIf
178900091217     c
179000091217     c                   ENDSR
179100011227     c**********************************************************************
179200000607     C     *INZSR        BEGSR
179300011227     c**********************************************************************
179400000607      *
179500070320     c
179600070123      *
179700070315     c     *dtaara       define    §azute        azuteds
179800070315     c     *dtaara       define    §datiute      ddatiute
179900070315     c                   in(e)     *dtaara
180000070315     c                   if        %error  or rsut = *blanks
180100070315     c                   clear                   tibs34ds
180200070315     c                   call      'TIBS34R'
180300070315     c                   parm                    tibs34ds
180400070315     c                   in        *dtaara
180500070315     c                   endif
180600030702      *reperimento data
180700070319     C                   TIME                    W0140            14 0
180800070319     C                   MOVE      W0140         WDAT              8 0
180900030702     C*
181000030702     C                   Z-ADD     WDAT          G02DAT
181100030702     C                   MOVEL     *BLANK        G02ERR
181200030702     C                   CALL      'XSRDA8'
181300030702     C                   PARM                    WLBDA8
181400030702     C* UDATE A 8 IN AAAA/MM/GG
181500030717     C                   Z-ADD     G02INV        WUDAte            8 0
181600091216      * Aggancio tabella "VPODECO" per reperimento data attivazione
181700091216      * gestione prepagati
181800091216     C                   CLEAR                   dsbs02
181900091216     c                   clear                   dvpodeco
182000091216     C                   MOVEL     'C'           T02MOD
182100091216     C                   MOVEL     KNSIF         T02SIF
182200091216     C                   MOVEL     'VPO'         T02COD
182300091216     c                   movel(P)  'DECO'        t02ke1
182400091216     C                   CALL      'TIBS02R'
182500091216     C                   PARM                    KPJBA
182600091216     C                   PARM                    dsbs02
182700091216    2C                   IF        T02ERR = *BLANKS
182800091216     C                   MOVEL     T02uni        dvpodeco
182900091216    2C                   ENDIF
183000091217
183100091217      * Aggancio tabella AR5 per record 'GEN'
183200091217     c                   Clear                   dAr5
183300091217     c                   Clear                   dsbs02
183400091217     c                   Eval      T02Mod = 'C'
183500091217     c                   Eval      T02Sif = knsif
183600091217     c                   Eval      T02Cod = 'AR5'
183700091217     c                   Movel(p)  'GEN'         T02Ke1
183800091217     c                   Call      'TIBS02R'
183900091217     c                   Parm                    Kpjba
184000091217     c                   Parm                    dsbs02
184100091217     c                   If        T02Err = *Blanks
184200091217     c                   Movel     T02Uni        dAr5
184300091217     c                   EndIf
184400091217     c
184500000607      *
184600070123      *
184700070124     C     Karb          klist
184800070321     C                   kfld                    ilrg2aas
184900070321     C                   kfld                    ilrg2lnp
185000070321     C                   kfld                    ilrg2nrs
185100070321     C                   kfld                    ilrg2nsp
185200070621     C     Karbp         klist
185300070621     C                   kfld                    kaas
185400070621     C                   kfld                    klnp
185500070621     C                   kfld                    knrs
185600070621     C                   kfld                    knsp
185700070125      *
185800070201      *
185900070201     C     Kar4          klist
186000070201     C                   kfld                    arbaas
186100070201     C                   kfld                    arblnp
186200070201     C                   kfld                    arbnrs
186300070201     C                   kfld                    arbnsp
186400070321     c                   kfld                    wtrc
186500070321     c     KFiar5        Klist
186600070321     C                   kfld                    arbaas
186700070321     C                   kfld                    arblnp
186800070321     C                   kfld                    arbnrs
186900070321     C                   kfld                    arbnsp
187000070321     c                   Kfld                    KAr5Trd
187100070201      *
187200070201     C     Ktab          klist
187300070201     C                   kfld                    tblkut
187400070201     C                   kfld                    tblcod
187500070201     C                   kfld                    tblkey
187600070201     C                   z-add     1             tblkut
187700070321     c     KArn          Klist
187800070321     C                   kfld                    arbaas
187900070321     C                   kfld                    arblnp
188000070321     C                   kfld                    arbnrs
188100070321     C                   kfld                    arbnsp
188200070321     c                   Kfld                    KArncdn
188300070321     c                   Kfld                    KArngst
188400070411      * File FITGD02L
188500070411     c     K02TGD02      klist
188600070411     c                   kfld                    TGDtor
188700070411     c                   kfld                    TGDogg
188800070614     c*
188900070614     C     *LIKE         DEFINE    TBLKUT        §KUT
189000070614     C     *LIKE         DEFINE    TBLCOD        §COD
189100070614     C     *LIKE         DEFINE    TBLKEY        §KEY
189200030714
189300011227     C                   ENDSR
189400070328     c**********************************************************************
189500070328     C     chiudi        BEGSR
189600070328     c**********************************************************************
189700070328     c                   eval      d48tla='C'
189800070328     C                   MOVEL     fnlr48ds      KPJBU
189900070328     c                   call      'FNLR48R'
190000070328     C                   PARM                    KPJBA
190100070328     C                   PARM                    DSARBD
190200070328     C                   PARM                    DSARBK
190300070328     C                   PARM                    DARBT
190400070328     C                   PARM                    DSARBG
190500070328     C                   PARM                    trul90ds
190600070328
190700070328     c                   clear                   fnlv80ds
190800070328     c                   eval      ilv80tla='C'
190900070328     c                   call      'FNLV80R'
191000070328     c                   parm                    kpjba
191100070328     c                   parm                    fnlv80ds
191200070328
191300131008     c**                 eval      d19tla='C'
191400131008     c**                 call      'FNLV19R'
191500131008     c**                 parm                    fnlv19ds
191600070328
191700140130     c                   eval      ilrg7tla='C'
191800140130     c                   call      'FNLRG7R'
191900140130     c                   parm                    fnlrg7ds
192000070329
192100070329     C                   CLEAR                   trul21ds
192200070329     C                   MOVEL     'C'           i21tla
192300070329     C                   CALL      'TRUL21R'
192400070329     C                   PARM                    trul21ds
192500070329     c
192600070329     C                   CLEAR                   trulTICds
192700070329     C                   MOVEL     'C'           IticTLA
192800070329     C                   CALL      'TRULTICR'
192900070329     C                   PARM                    TRULTICDS
193000070329     C                   PARM                    DS1A
193100070503
193200070503     c                   eval      id34tla = 'C'
193300070503     c                   call      'FIDG34R'
193400070503     c                   parm                    fidg34ds
193500070704
193600070704     c                   eval      d66tla  = 'C'
193700120126     C                   MOVEL     fnlr66ds      KPJBU
193800070704     c                   call      'FNLR66R'
193900120126     c                   parm                    kpjba
194000070704
194100070704     c                   clear                   fnlv55ds
194200070704     C                   MOVEL     'C'           D55TLA
194300070704     C                   CALL      'FNLV55R'
194400070704     C                   PARM                    FNLv55DS
194500091216     c*
194600091216     c                   clear                   trulintds
194700091216     C                   MOVEL     'C'           iinttla
194800091216     C                   CALL      'TRULINTR'
194900091216     C                   PARM                    TRULINTDS
194901140922
194902140922     c                   eval      ilry09tla= 'C'
194903141024     c                   eval      kpjbu=fnlry09ds2
194904140922     c                   call      'FNLRY09R'
194905141013     c                   parm                    kpjba
194906140922
195000070328     c                   endsr
195100070315**
195200070329Variazione non eseguibile: bolla impegnata da altro utente.
195300070329Data errata
195400070330Se impostato l'esito premere F6=Conferma
195500070329Immettere l'esito
195600070329Esito errato
195700070403Lasciato avviso ed immessa data cons.richiesta:la sped. deve essere ABILITATA
195800070329Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              07
195900070329Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        08
196000070329Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              09
196100110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       10
196200070704Per l'abilitazione rapida non è possibile variare/immettere dati              11
196300070704Codice errato                                                                 12
196400070704Codice non utilizzabile                                                       13
196500070704Codice obbligatorio                                                           14
196600141013Presenti Disposizioni di Consegna in sospeso: F19 per gestirle                15
