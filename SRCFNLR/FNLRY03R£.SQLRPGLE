000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140911     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000300140813     H* FNLRY03R *---------------------------------------------------*
000400140813     H* - Gestione disposizioni di consegna
000500140130     H*--------------------------------------------------------------*
000600070207      *
000700140812     Ffnlry03d  CF   E             WORKSTN
000800070621     Ffnarb01l  if   E           k disk
000900070621     F                                     RENAME(FNarb000:FNarb001)
001000140909     Ftiidc01l  uf   E           k disk
001100140814     FFNarb00F  iF   E             DISK
001200070328     Ffiar401l  If   E           K disk
001300070322     F                                     RENAME(Fiar4000:Fiar4001)
001400140814     Ffiar501l  if   E           k disk
001500140814     Ffiar901l  if   E           k disk
001600070201     Ftabel00f  if   E           k disk
001700070320     Fazorg01l  if   E           k disk
001800070321     Ffiarn11l  if   E           k disk
001900141103     Ffiarg01l  uf   E           k disk
002000070411      *.......... T E L E F O N A T E   /   R E C L A M I
002100160404     fFITGD32L  if   e           k disk
002200011228      *
002300011228     D Kpjba         E DS
002400140813     d
002500140813     d* DS PASSAGGIO DATI
002600141024     d*  O L D
002700141024     D fnlry03ds     E DS                  prefix(OL_)
002800141024     d*  N E W
002900141024     D fnlry03ds2    E DS
003000011228      *
003100070328     D DSARBD        E DS
003200070328     D DSARBG        E DS
003300131008     D DSARBN        E DS
003400070328     D DARBT         E DS
003500070328     D DSARBK        E DS
003600140911     d tiidcds       e ds                  extname(tiidc00f)
003700140911     d fnlry00m0     e ds
003800140911     d fnlry00f0     e ds                  qualified inz(*extdft)
003900070328     D fnlr48ds      E DS
004000070703     D fnlv55ds      E DS
004100070703     D fnlr66ds      E DS
004200070328     D trul90ds      E DS
004300130515     D DS7PQRS       E DS
004400140130     D fnlrn6ds      E DS
004500140130     D fnlrg7ds      E DS
004600070320     D fnlv80ds      E DS
004700070329     d ILV80NWPROC     s              1
004800131008     d
004900070201     D fidn12ds      E DS
005000070410     D fidna2ds      E DS
005100070410     d  IA2mod       e                     inz('M')
005200070410     d  IA2tor       e                     inz('S')
005300141001
005400141001     D fidna7ds      E DS
005500141006
005600141006     D fnlg82ds      E DS
005700141007     D fnlg20ds      E DS
005800141001
005900140409     D fnlrj6ds      E DS
006000070926     d TRTB2Ads      e ds                  inz
006100070926     d   TB2Aopz     e                     inz('1')
006200070926     d   TB2Atip     e                     inz('G')
006300070315     d azuteds       e ds                  extname(azute00f)
006400070315     d ddatiute      e ds
006500070315     d tibs34ds      e ds                  inz
006600070131     D trul06ds      E DS
006700070131     D  lin                    1     90  0 dim(30)
006800091216     D trulintds     E DS
006900140911     d dsemail       e ds
007000141002
007100141002      // - Parametri per ricerca Programmi chiamanti
007200141002     d dsStack         ds
007300141002     d  skStack                      10    Dim(100)
007400141003     d
007500141002      *
007600070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
007700070206     D PARAM1          DS                  INZ
007800070206     D  PA1AAS                14     17  0
007900070206     D  PA1LNP                18     20  0
008000070206     D  PA1NRS                21     22  0
008100070206     D  PA1NSP                23     29  0
008200070206     D  PA1F03                30     30
008300070206     D  PA1FLG                31     31
008400070322     D  PA1PGM                32     41
008500070322     D  PA1DSA                42     61
008600070322     D  PA1CML                62     62
008700070322     D  PA1DSP                63     70  0
008800070322     D  PA1RMN                71     85  0
008900070322     D  PA1LNA                86     88  0
009000070322     D  PA1LN1                89    108
009100070322     D  PA1RSD               109    143
009200070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
009300070206     D  PA1GIA               144    144
009400070322     D  PA1FEL               145    147  0
009500070322     D  PA1TBO               148    148
009600070201     d ds3a          e ds
009700070320     d ds5e          e ds
009800080909     d dsbl4a        e ds
009900070321     d dar5Gen       e ds
010000140813     d dar5Emd       e ds
010100151119     d****** dVpodeco      e ds
010200091216     D* DS PER TIBS02R - GESTIONE TNTBE00F
010300091216     D DSBS02        E DS                  EXTNAME(TIBS02DS)
010400011228      *
010500070329
010600030613     D WLBDA8          DS
010700030613     D  G02DAT                 1      8  0
010800030613     D  G02INV                 9     16  0
010900030613     D  G02ERR                17     17
011000030613     D  G02TGI                18     22  0
011100070327     D                SDS
011200070327     D  N_pgm                  1      8
011300070621
011400070328     d dataeur         s               d   datfmt(*eur)
011500140910     d dataiso         s               d   datfmt(*iso)
011600070703     D wrkoggiiso      s               d
011700070321     d KAr5Trd         s                   like(Ar5Trd)
011800070321     d KArnCdn         s                   like(ArnCdn)inz('LDV')
011900070321     d KArngst         s                   like(Arngst)inz('S')
012000070321     d wtrc            s                   like(ar4trc)
012100141008     D wrichtrul90     S              1
012200070321     d wblk            s              1
012300141006     d wnot            s            300
012400091127     d s_ar5cdi        s                   like(§ar5cdi)
012500140409     d wncl            s                   like(arbncl)
012600141001     d k_eladata       s                   like(idceladata) inz
012700140910     d wrsd2           s             35
012800140910     d wggc            s                   like(idcprcr)
012900140910     d wgg             s              2  0
013000140910     d wmm             s              2  0
013100140910     d waa             s              4  0
013200140910     d dw              s              1s 0
013300140911     d werrchk         s              1
013400141008     d wforzaF6        s              1
013500141013     d
013600141013     d forzacell       s              1
013700141013     d forzamail       s              1
013800070924      *
013900070924     d Cod             s                   like(TBLcod)  inz
014000070924     d Key             s                   like(TBLkey)  inz
014100070924     d Ordina          s              1                  inz
014200070924     d Comando         s              1                  inz
014300140911
014400140911     D prmRqsOpCode    s             10I 0
014500140911     D prmRpyOpCode    s             10I 0
014600140911     D prmRpyIdMsg     s             10I 0
014700140911     D prmRqsFormato   s             10A
014800140911     D prmRqsData      s          32767A
014900140911     D prmRqsDataSize  s             10I 0
015000140911     D prmRpyFormato   s             10a
015100140911     D prmRpyData      s          32767
015200140911     D prmRpyDataSize  s             10I 0
015300140911     D prmRpyFormMsg   s             10A
015400140911     D prmRpyMessage   s          32767
015500140911     D prmRpyMsgSize   s             10I 0
015600140911     D prmRpyFormForz  s             10A
015700140911     D prmRpyForzatu   s          32767
015800140911     D prmRpyForSize   s             10I 0
015900141002      // - Campi per procedura esterna
016000141002     d  pStack         s           1000
016100141002     d  pStartPoint    s              3  0
016200141002     d  pEntries       s              3  0
016300070319
016400070316      *---------------------------------------------------------------------
016500141001     D cosapp          C                   CONST(' CONSEGNA  RICHIESTA  ')
016600141001     D cosffd          C                   CONST('   FERMO  DEPOSITO    ')
016700141001     D cosind          C                   CONST('INDIRIZZO  ALTERNATIVO')
016800141001     D cosaltro        C                   CONST('  ALTRE  INDICAZIONI  ')
016900141001     D msg             S             78    DIM(10) CTDATA PERRCD(1)
017000141008      *
017100141008     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
017200141008     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
017300141008     D CMETf6          S              1    DIM(130) CTDATA PERRCD(65)
017400140911      // ? PROTOTIPI ?
017500140911      /copy gaitrasrc/srcprotopr,ubchkcel
017600140911      /copy gaitrasrc/srcprotopi,ubchkcel
017700140911      /copy gaitrasrc/srcprotopr,fnlr48r
017800140911      /copy gaitrasrc/srcprotopr,Fnlry00r
017900140911      /copy gaitrasrc/srcconst,FNLRY00R
018000141002
018100141002       // - Pgm per ricerca Programmi chiamati
018200141002     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
018300141002     d  pStack                     1000
018400141002     d  pStartPoint                   3  0
018500141002     d  pEntries                      3  0
018600141002
018700070320      *---------------------------------------------------------------------
018800141027      * INDICATORI
018900141027      *
019000141027      * 70 - abilitazione F21
019100141027      * 71 - dirottamento
019200141027      *---------------------------------------------------------------------
019300070320     C     *ENTRY        PLIST
019400070320     C                   PARM                    Kpjba
019500140813     C                   PARM                    fnlry03ds
019600141024     c* verifico se passata nuova o vecchia DS
019700141024     c                   movel     kpjbu         fnlry03ds2
019800141024
019900141024     c                   monitor
020000141024     c                   if        ILRY03NSP>0
020100141024     c                   endif
020200141024     c                   on-error
020300141024     c                   clear                   ILRY03nsp
020400141024     c                   endmon
020500141024
020600141024     c                   select
020700141024     c* passata solo DS vecchi --> sposto i campi
020800141024     c                   when      %parms=2
020900141024     c                   clear                   fnlry03ds2
021000141024     c                   eval      ILRY03TLA  =ol_ILRY03TLA
021100141027     c                   if        ilry03tla<>'C'
021200141024     c                   eval      ILRY03Tch  =ol_ILRY03Tch
021300141024     c                   eval      ILRY03AAS  =ol_ILRY03AAS
021400141024     c                   eval      ILRY03lnp  =ol_ILRY03lnp
021500141024     c                   eval      ILRY03nrs  =ol_ILRY03nrs
021600141024     c                   eval      ILRY03nsp  =ol_ILRY03nsp
021700141024     c                   eval      ILRY03fgs  =ol_ILRY03fgs
021800141024     c                   eval      ILRY03prg  =ol_ILRY03prg
021900141024     c                   eval      ILRY03tipd =ol_ILRY03tipd
022000141027     c                   endif
022100141024     c                   clear                   ol_OLry03err
022200141024     c                   clear                   ol_OLry03msg
022300141024     c                   clear                   ol_olry03no1
022400141024     c                   clear                   ol_olry03no2
022500141024     c                   clear                   ol_olry03f12
022600141024     c* non passata nessuna DS --> pulisco la ds nuova e darà errore
022700141024     c                   when      %parms=1 and ILRY03nsp =0
022800141024     c                   clear                   fnlry03ds2
022900141024     c                   endsl
023000141024     c
023100070320     c*
023200140813     c                   clear                   olry03err
023300140813     c                   clear                   olry03msg
023400140813     c                   clear                   olry03f12
023500140813     c                   if        ilry03tla='C'
023600070320     c                   goto      fine
023700070320     c                   endif
023800130827
023900070327     C                   TIME                    W0140            14 0
024000070327
024100070327      * UDATE IN AAAAMMGG
024200070327     C                   MOVE      W0140         g02dat            8 0
024300070327     C                   MOVEL     *BLANK        G02ERR
024400070327     C                   CALL      'XSRDA8'
024500070327     C                   PARM                    WLBDA8
024600070327     C                   MOVEL     G02INV        DATEU             8 0
024700070321     c*
024800070621     c     karb          chain     fnarb01l
024900141024     c                   if        not %found(fnarb01l)
025000141024     c                   eval      olry03err='1'
025100141024     c                   eval      olry03msg=msg(7)
025200141024     c                   goto      fine
025300141024     c                   endif
025400070320     c*
025500070321     C                   EXSR      RieD01
025600140813     c                   if        olry03err<>' '
025700140813     c                   goto      fine
025800140813     c                   endif
025900141028     c* chiamata cieca
026000141028    0c                   if        ILRY03FFR='N'
026100141028     c                   exsr      Ges_F21
026200141028     c
026300141028     c* Se ci sono stati errori o premuto F12 riemetto videata altrimenti uscita
026400141028    1c                   if        not *in66 or *in90 =*on or *in91
026500141028     c
026600141028    2c                   if        vidmsg<>*blanks
026700141028     c                   eval      olry03msg=vidmsg
026800141028     c                   eval      olry03err='1'
026900141028    2c                   endif
027000141028     c
027100141028     c                   eval      olry03f12='1'
027200141028     c                   unlock    tiidc01l
027300141028   x2c                   else
027400141028     c                   exsr      conf_cieca
027500141028    2c                   endif
027600000608      *
027700141028   x0c                   else
027800141028     c* con videata
027900070315     c                   do        *hival
028000141007     c* In realtà ora è sempre abilitato
028100140813     c                   exsr      sr_AbilF17
028200141007     c
028300141007     c* Per ora sempre NON abilitato F16
028400141007     c                   exsr      sr_abilf16
028500141007     c
028600140813     c                   write     lry03t01
028700140813     C                   EXFMT     lry03d01
028800070315     c                   setoff                                       9028
028900070315     c                   clear                   vidmsg
029000070326      * F12=Ritorno
029100070326     C                   if        *INKL
029200140813     c                   eval      olry03f12='1'
029300141001     c                   unlock    tiidc01l
029400070315     C                   leave
029500070315     c                   endif
029600070315      * F7=Interrogazione bolle arrivi
029700070315     c                   if        *inkg
029800070315     c                   clear                   kpjbu
029900070315     c                   clear                   param1
030000070322     C                   MOVE      v2haas        PA1AAS
030100070322     C                   MOVE      v2clnp        PA1LNP
030200070322     C                   MOVE      v2cnrs        PA1NRS
030300070322     C                   MOVE      v2cnsp        PA1NSP
030400070322     C                   MOVE      'N'           PA1FLG
030500070322     C                   MOVE      *blanks       PA1CML
030600070327     C                   MOVEL     N_pgm         PA1PGM
030700070315     c                   movel     param1        kpjbu
030800070315     c                   call      'FNLR36R'
030900070315     c                   parm                    kpjba
031000070315     c                   iter
031100070315     c                   endif
031200070410      * F16=Gestione Richieste (R.A.)
031300070405     c                   if        *inkq
031400070410     c                   reset                   FIDNA2ds
031500070410     c                   eval      ia2ogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
031600070410     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
031700070410     c                   call      'FIDNA2R'
031800070410     c                   parm                    KPJBA
031900070410     c                   parm                    FIDNA2ds
032000070405     c                   iter
032100070405     c                   endif
032200140912      * F10=Inserimento Richieste (R.A.)
032300140912     c                   if        *inkj
032400141003     c                   exsr      Win_RA
032500141003     c**                 call      'FIDNA1R'
032600141003     c**                 parm                    KPJBA
032700140912     c                   iter
032800140912     c                   endif
032900070412      *?F17 - Note LDV
033000070412if  1c                   if        *inKR = *on
033100140130     c                   clear                   fnlrn6ds
033200140130     c                   eval      in6mod='V'
033300140130     c                   eval      in6aas=arbaas
033400140130     c                   eval      in6lnp=arblnp
033500140130     c                   eval      in6nrs=arbnrs
033600140130     c                   eval      in6nsp=arbnsp
033700140131     c                   eval      in6pgm='FNLRG2R'
033800140130     c                   call      'FNLRN6R'
033900070412     c                   parm                    kpjba
034000140130     c                   parm                    fnlrn6ds
034100070412     c                   iter
034200070412e   1c                   endif
034300141010c
034400141010      * controlli videata
034500141010     C                   EXSR      Ctrd01
034600141010     c   90              iter
034700141010c
034800141008    1c                   if        *inkv
034900141028     c                   exsr      Ges_F21
035000141013     c
035100141013     c* Se ci sono stati errori o premuto F12 riemetto videata altrimenti uscita
035200141028    2c                   if        not *in66 or *in90 =*on or *in91
035300141010     c                   iter
035400141013    2c                   endif
035500141013    1c                   endif
035600141007     c
035700141008     c* F6=Conferma --> Presa Visione
035800141007     c* solo se fatta una variazione o una nota
035900141027     c* errore forzabile; obbligatorio se dirottamento
036000141013     c                   if        *inkf and
036100141013     c                             not *in66 and wnot=*blanks
036200141008     c                             and wforzaF6=' '
036300141007     c                   seton                                        9028
036400141027     c                   if        idcfla=' '
036500141007     c                   eval      vidmsg=msg(5)
036600141008     c                   eval      wforzaf6='S'
036700141027     c                   else
036800141027     c                   eval      vidmsg=msg(8)
036900141027     c                   endif
037000141007     c                   iter
037100141007     c                   endif
037200141007     c
037300141013     c* per F6 o F21 - conferma
037400141013     c                   if        *inkf or *inkv
037500141007     c                   exsr      confe
037600070322     c   90              iter
037700141013     c  n90              leave
037800141007     c                   endif
037900011227      *
038000011227     C                   ENDDO
038100070124      *
038200141028    0c                   endif
038300141028     c
038400141024     c                   if        %parms=2
038500141024     c                   eval      ol_olry03f12 =olry03f12
038600141024     c                   eval      ol_olry03err =olry03err
038700141024     c                   eval      ol_olry03msg =olry03msg
038800141027     c                   else
038900141027     c                   movel     fnlry03ds2    kpjbu
039000141024     c                   endif
039100070320     c     fine          tag
039200140813     c                   if        ilry03tla=' '
039300070320     c                   seton                                        rt
039400070320     c                   else
039500070328     c                   exsr      chiudi
039600070315     C                   seton                                        lr
039700070320     c                   endif
039800070315     c**********************************************************************
039900070320     c* Caricamento d01
040000070315     c**********************************************************************
040100070321     C     RieD01        BEGSR
040200141010     c                   setoff                                       9071
040300141001     c                   setoff                                       666711
040400141006     c                   setoff                                       686470
040500141027     c                   setoff                                       2223
040600141028     c                   setoff                                       2890
040700141006     c                   clear                   LRY03W1
040800141006     c                   clear                   LRY03W2
040900141006     c                   clear                   LRY03W3
041000141006     c                   clear                   LRY03W4
041100141006     c                   clear                   wnot
041200141008     c                   clear                   wforzaF6
041300141013     c                   clear                   forzacell
041400141013     c                   clear                   forzamail
041500141028     c                   clear                   vidmsg
041600140917     c* pos. cusr su causale variazione bolle
041700140917     c                   seton                                        46
041800141006     c* Accendo indicatore per la int bolle
041900141006     c                   seton                                        63
042000141006      /free
042100141006         XRTVSTKR (pStack:pStartPoint:pEntries);
042200141006         dsStack = pStack;
042300141006         IF  %lookup('FNLR36R   ':skStack) > 0;
042400141006          *in63=*off ;
042500141006         endif      ;
042600141006      /end-free
042700141027     c
042800070321     c* CAMPI DI OUTPUT
042900070320     c                   z-add     arblnp        v2clnp
043000070320     c                   z-add     arbnrs        v2cnrs
043100070320     c                   z-add     arbnsp        v2cnsp
043200070320     c                   movel     arbaas        wdata             8 0
043300070320     c                   move      arbmgs        wdata
043400070320     c     *iso          move      wdata         dataeur
043500070320     c                   move      dataeur       v2cdsp
043600070322     c                   move      arbaas        v2haas
043700141001     c**                 movel     arbcbo        v2ccbo
043800070320     c                   clear                   v2dcbo
043900070320     c                   move      '3A'          tblcod
044000070320     c                   movel(p)  arbcbo        tblkey
044100070320     c                   exsr      ctabel
044200070320     c                   movel     tbluni        ds3a
044300141001     c                   eval      v2dcbo=arbcbo+'-'+§3ades
044400070320     c* tipo spedizione
044500070320     c                   clear                   v2dtsp
044600070320     c                   move      '5E'          tblcod
044700070320     c                   movel(p)  arbtsp        tblkey
044800070320     c                   exsr      ctabel
044900070320     c                   movel     tbluni        ds5e
045000090604     c                   movel     §5ed08        v2dtsp
045100070320     c* mittente
045200070320     c                   clear                   v2cmit
045300070320     c                   if        %subst(§3atb1:1:1)='F'
045400070320     c                   move      arbksc        v2cmit
045500070320     c                   else
045600070320     c                   move      arbccm        wccm4             4 0
045700070320     c                   if        wccm4<>9999
045800070320     c                   move      arbccm        v2cmit
045900070320     c                   endif
046000070320     c                   endif
046100070320     c* ragione sociale mittente
046200070320     c                   movel(P)  arbrsm        v2crsm
046300070320     c* ragione sociale mittente originale
046400070320     c                   movel(P)  arbrmo        v2crmo
046500070320     c* destinatario
046600070320     c                   movel(P)  arbrsd        v2crsd
046700140910     c                   clear                   wrsd2
046800070320     c* estensione ragione sociale destinatario da fiar4 record "D"
046900070320     C                   MOVEL     'D'           WTRC
047000070328     C     KAR4          CHAIN     Fiar401l
047100070320     C                   IF        %found(fiar401l)
047200070320     C                   MOVE      AR4NOT        v2crsd
047300140910     C                   MOVE      AR4NOT        wrsd2
047400070320     C                   ENDIF
047500070321     c                   movel     arbind        v2cind
047600070321     c                   movel     arbcad        v2ccad
047700070321     c                   movel     arblod        v2clod
047800070321     c                   eval      v2clod=%trim(v2clod)+ '  ' + arbprd
047900070321     c* referente consegna
048000070321     c                   Clear                   DAr5Gen
048100070321     c                   Eval      kAr5Trd = 'GEN'
048200091217     c     KFiar5        Chain(n)  Fiar501l
048300070321     c                   If        %Found(Fiar501l)
048400070321     c                   Movel     Ar5Uni        DAr5Gen
048500070321     c                   EndIf
048600070327    2c                   if        %subst(§ar5ref:1:1) = '#'
048700140910     c                   eval      v2crefb=%subst(§ar5ref:2:34)
048800070327     c                   else
048900140910     c                   Eval      V2cRefb = §Ar5Ref
049000070327     c                   endif
049100140910     c                   eval      v2cref=v2crefb
049200070327    2c                   if        %subst(§ar5teld:1:1) = '#'
049300140910     c                   eval      v2ctelb=%subst(§ar5teld:2:15)
049400070327     c                   else
049500140910     c                   Eval      V2cTelb = §Ar5Teld
049600070327     c                   endif
049700140910     c                   eval      v2ctel=v2ctelb
049800140813     c* memorizzo anche eventuale codice intestazione assegno
049900140813     c                   eval      s_ar5cdi=§ar5cdi
050000140813     c* Mail e cell per alert
050100140813     c                   Clear                   DAr5emd
050200140813     c                   Eval      kAr5Trd = 'EMD'
050300140813     c     KFiar5        Chain(n)  Fiar501l
050400140813     c                   If        %Found(Fiar501l)
050500140813     c                   Movel     Ar5Uni        DAr5emd
050600140813     c                   EndIf
050700140910     c                   eval      v2cmailb=§ar5eml
050800140910     c                   eval      v2cmail =v2cmailb
050900140813    2c                   if        %subst(§ar5tel:1:1) = '#'
051000140910     c                   eval      v2ccellb=%subst(§ar5tel:2:15)
051100140813     c                   else
051200140910     c                   Eval      V2ccellb = §Ar5Tel
051300140813     c                   endif
051400140910     c                   eval      v2ccell =v2ccellb
051500140813     c
051600070321     c* consegna richiesta
051700070321     c                   clear                   fnlv80ds
051800070321     c                   eval      ilv80aas=arbaas
051900070321     c                   eval      ilv80lnp=arblnp
052000070321     c                   eval      ilv80nrs=arbnrs
052100070321     c                   eval      ilv80nsp=arbnsp
052200070321     c                   eval      ilv80mgs=arbmgs
052300070321     c                   eval      ilv80tcr=arbtcr
052400070321     c                   eval      ilv80dcr=arbdcr
052500070321     c                   eval      ilv80hcr=arbhcr
052600070321     c                   eval      ilv80tc1=arbtc1
052700070321     c                   eval      ilv80tc2=arbtc2
052800070321     c                   call      'FNLV80R'
052900070321     c                   parm                    kpjba
053000070321     c                   parm                    fnlv80ds
053100070329     c                   parm      'S'           ILV80NWPROC
053200070321     c                   eval      v2ctcro=olv80tcrd
053300070330     c                   if        arbdcr>0
053400070329     c     *iso          move      arbdcr        dataeur
053500070329     c                   move      dataeur       v2cdcro
053600070330     c                   else
053700070330     c                   clear                   v2cdcro
053800070330     c                   endif
053900070321     c                   eval      v2chcro=arbhcr
054000070321     c* colli e peso bollettato
054100070321     c                   z-add     arbncl        v2cncl
054200070321     c                   z-add     arbpkb        v2cpkb
054300070326     c* altre gestioni
054400070321     c                   clear                   v2dges
054500070321     c                   clear                   v2cges
054600140130     c                   clear                   fnlrg7ds
054700140130     c                   z-add     v2haas        ILRG7AAS
054800140130     c                   z-add     v2clnp        ILRG7LNP
054900140130     c                   z-add     v2cnrs        ILRG7NRS
055000140130     c                   z-add     v2cnsp        ILRG7NSP
055100140130     c                   call      'FNLRG7R'
055200140130     c                   parm                    fnlrg7ds
055300140130     c                   movel     OLRG7VSSIN    v2dges
055400140409     c* visualizzo se tutti i colli arrivati alla fil di arrivo
055500140409     c                   clear                   fnlrj6ds
055600140409     c                   eval      iLRJ6TRIC = 'A'
055700140409     c                   eval      iLRJ6AAS  = v2haas
055800140409     c                   eval      iLRJ6LNP  = v2cLNP
055900140409     c                   eval      iLRJ6NRS  = v2cNRS
056000140409     c                   eval      iLRJ6NSP  = v2cNSP
056100140409     c                   call      'FNLRJ6R'
056200140409     c                   parm                    fnlrj6ds
056300140409     c
056400140409     c                   clear                   v2carr
056500140409     c                   if        OLRJ6FALL='S'
056600140409     c                   eval      v2carr='TUTTI'
056700140409     c                   else
056800140409     c                   eval      wncl=OLRJ6CDCM+OLRJ6CSPU
056900140409     c                   eval      v2carr=%editc(wncl:'Z')
057000140409     c                   if        v2carr=*blanks
057100140409     c                   move      '0'           v2carr
057200140409     c                   endif
057300140409     c                   endif
057400070702     c*
057500070702     c* causale per variazione bolla
057600070702     c                   clear                   v2ccvb
057700130515     c* particolarità
057800130515     c                   setoff                                       444547
057900130515     c                   clear                   v2igga
058000130515     c                   clear                   v2igma
058100130515     c                   clear                   v2igva
058200130515     c                   eval      v2cgga=arbgga
058300130515     c                   if        v2cgga<>*blanks
058400130515     c                   seton                                        44
058500130515     c                   endif
058600130515     c                   eval      v2cgma=arbgma
058700130515     c                   if        v2cgma<>*blanks
058800130515     c                   seton                                        45
058900130515     c                   endif
059000130515     c                   eval      v2cgva=arbgva
059100130515     c                   if        v2cgva<>*blanks
059200130515     c                   seton                                        47
059300130515     c                   endif
059400080909     c* SI DDT
059500080909     c                   setoff                                       14
059600080909     c                   movel     'A'           wtrc
059700080909     c     kar4          chain     fiar401l
059800080909     c                   if        %found(fiar401l)
059900080909     c                   movel     ar4not        dsbl4a
060000080909     c                   if        §b4abm='S' or §b4abm='C' or
060100080909     c                             §b4abm='P' or §b4abm='Q'
060200080909     c                   seton                                        14
060300080909     c                   endif
060400080909     c                   endif
060500070321     c* flag fermo deposito
060600070321     c                   if        arbffd<>*blanks
060700070321     c                   seton                                        10
060800070321     c                   else
060900070321     c                   setoff                                       10
061000070321     c                   endif
061100140813     c* abilitazione tasto funzionale F17-note aut
061200140813     c                   exsr      sr_AbilF17
061300140813     c
061400140813     c* Lettura record di disposizione di consegna da gestire
061500140909     c     kidc          chain     tiidc01l
061600140909     c                   if        not %found(tiidc01l)
061700140813     c                   eval      olry03err='1'
061800140813     c                   eval      olry03msg=msg(1)
061900140813     c                   leavesr
062000140813     c                   endif
062100141001     c
062200141001     c* Se non è ancora stata elaborata --> errore
062300141001     c                   if        idcelaflg<>'E'
062400141001     c                   eval      olry03err='1'
062500141001     c                   eval      olry03msg=msg(4)
062600141001     c                   leavesr
062700141001     c                   endif
062800141001     c
062900140911     c* richiamo chkistruco per controlli di eseguibilità della variazione richiesta
063000141008     c**                 exsr      sr_chkistruco
063100141008     c**                 if        werrchk='1'
063200141008     c**                 eval      idcerr='1'
063300141008     c**                 eval      IDCLOG=%subst(textmsg:1:200)
063400141008     c**                 endif
063500140813     c
063600141001     c                   clear                   v2clog
063700141001     c                   clear                   v2clog1
063800141001     c                   if        idclog<>*blanks
063900141001     c                   seton                                        11
064000141006     c                   eval      v2clog=%subst(idclog:1:65)+'  '
064100141006     c                   eval      v2clog1='  '+%subst(idclog:66:74)+'  '
064200141006     c                   endif
064300141001     c
064400140813     c                   if        idcref<>*blanks
064500140813     c                   eval      v2cref=idcref
064600140813     c                   endif
064700140813     c                   if        idcteld<>*blanks
064800140813     c                   eval      v2ctel =idcteld
064900140813     c                   endif
065000140813     c                   if        idctelavv<>*blanks
065100140813     c                   eval      v2ccell=idctelavv
065200140813     c                   endif
065300140813     c                   if        idcmailavv<>*blanks
065400140813     c                   eval      v2cmail=idcmailavv
065500140813     c                   endif
065600140813     c* data e ora variazione
065700140813     c                   movel     idcinsdata    w0080             8 0
065800140813     c                   move      idcinsdata    w0060             6 0
065900140813     c                   movel     3             g02err
066000140813     C                   Z-ADD     w0080         G02inv
066100140813     C                   MOVEL     *zeros        G02dat
066200140813     C                   CALL      'XSRDA8'
066300140813     C                   PARM                    WLBDA8
066400141001     c                   eval      v2dvar=' D I S P O S I Z I O N I  dal'
066500140814     c                   if        idctipocli='M'
066600141001     c                   eval      v2dvar=%trimr(v2dvar)+ '  M i t t e n t e'
066700140814     c                   else
066800141001     c                   eval      v2dvar=%trimr(v2dvar)+ '  D e s t i n a t.'
066900140814     c                   endif
067000141001     c                   eval      v2dvar= %trimr(v2dvar) + '  il  '+
067100140814     c                               %editw(g02dat: '  /  /    ')+
067200140814     c                             '  '  +
067300141001     c                             %editw(w0060: '  :  :  ')
067400141001     c***                          ' ------------------------------------------'
067500140813     c
067600140813     c
067700070321     c* Sottotitolo videata + situazione contatto in corso
067800070321     c                   clear                   v2ctit2
067900140813     c                   clear                   v2cidc1
068000140813     c                   clear                   v2cidc2
068100140813     c                   clear                   v2cidc3
068200141006     c                   move      ' '           v2cidc3
068300141006     c                   movel     ' '           v2cidc3
068400070402     c                   clear                   wblk
068500151112     c                   clear                   v2hfla
068600141010     c*
068700141010     c                   seton                                        70
068800141010     c                   eval      v1cf21='F21=Applica Variazioni'
068900141010     c
069000070321     c                   select
069100140813     c* Tipo "4" --> disposizioni come note
069200140813     c                   when      ilry03tipd='4'
069300140813     c                   eval      v2ctit2=cosaltro
069400140813     c                   move      '4'           wblk
069500141010     c
069600141010     c* non devo accendere F21: non c'è nulla da applicare in automatico
069700141010     c                   setoff                                       70
069800141010     c
069900140813     c* Imposto i dati nella videata
070000141001     c                   eval      v2cidc1=%subst(idcnote:1:72)
070100141001     c                   eval      v2cidc2=%subst(idcnote:73:72)
070200141006     c                   eval      v2cidc3='   '+%subst(idcnote:145:56)
070300141006     c                   move      ' '           v2cidc3
070400140813     c
070500140813     c* Tipo "1" --> Appuntamento
070600140813     c                   when      ilry03tipd='1'
070700070321     c                   eval      v2ctit2=cosapp
070800140813     c                   move      '1'           wblk
070900140813     c* Imposto i dati richiesti
071000140813     c                   movel     idcdcr        w0080             8 0
071100140813     c                   movel     3             g02err
071200140813     C                   Z-ADD     w0080         G02inv
071300140813     C                   MOVEL     *zeros        G02dat
071400140813     C                   CALL      'XSRDA8'
071500140813     C                   PARM                    WLBDA8
071600140813     c                   eval      v2CIDC1='CONSEGNA RICHIESTA: IL'
071700140813     c                   eval      v2Cidc1=%TRIM(V2CIDC1) +'  '+
071800140813     c                                           %editw(g02dat:'  /  /    ')
071900140813     c                   if        idchcr>0
072000140813     c                   eval      v2cidc1=%trim(v2cidc1)+'  '+
072100140813     c                                    %editw(idchcr: '  :  ')
072200140813     c                   endif
072300140813     c
072400140813     C                   IF        idcprcr='M'
072500141008     c                   eval      v2cidc2= 'Preferibilmente il MATTINO   '
072600140813     c                   endif
072700140813     C                   IF        idcprcr='P'
072800141008     c                   eval      v2cidc2= 'Preferibilmente il POMERIGGIO'
072900140813     c                   endif
073000140813     c
073100140813     c* Tipo "2" --> fermo deposito
073200140813     c                   when      ilry03tipd='2'
073300070321     c                   eval      v2ctit2=cosffd
073400140813     c                   move      '2'           wblk
073500140813     c                   eval      v2cidc1= 'Fermo Deposito'
073600151112
073700151112     c* Tipo "5" --> RESO
073800151112     c                   when      ilry03tipd='5'
073900151112     c                   seton                                        7071
074000151112     c                   eval      v1cf21='F21=Reso/Dirott.'
074100151112     c                   eval      v2hfla=idcfla
074200151112     c                   eval      v2cidc2='R  E  S  O  '
074300141001     c
074400140813     c* Tipo "3" --> cambio indirizzo
074500140813     c                   when      ilry03tipd='3'
074600140813     c                   eval      v2ctit2=cosind
074700140813     c                   move      '3'           wblk
074800140813     c                   eval      v2cidc1= idcrsd
074900141002
075000141002     c                   clear                   v2cidc2
075100141002     c                   clear                   v2cidc3
075200141002     c* senza cons richiesta
075300141002     c                   if        idcdcr=0
075400141002     c* Bolla da dirottare
075500151112     c                   if        idcfla<>' '
075600141010     c                   seton                                        7071
075700151112     c                   eval      v1cf21='F21=Reso/Dirott.'
075800151112     c                   eval      v2hfla=idcfla
075900141008     c
076000141002     c                   eval      v2cidc2= %trim(idcIND)+' '+%trim(idccad)+
076100141002     c                             ' ' +idcprd+' '+idclod
076200141007     c                   eval      v2CIDC3='   D I R O T T A R E  alla  +
076300141007     c                               F i l i a l e  : ' + %editc(idcnewlna:'X')
076400151112     c
076500141002     c                   else
076600141002     c                   eval      v2cidc2= idcIND
076700141002
076800141006     c                   eval      v2cidc3= '   '+idccad
076900140911     c                   eval      v2cidc3= %trim(v2cidc3) + ' ' + idclod
077000140813     c                   if        %subst(v2cidc3:67:4)<>*blanks
077100140813     c                   eval      %subst(v2cidc3:67:4)='  '+idcprd
077200140813     c                   else
077300140813     c                   eval      v2cidc3=%trim(v2cidc3)+ '  '+idcprd
077400140813     c                   endif
077500141002     c                   endif
077600141002     c
077700141006     c                   move      ' '           v2cidc3
077800141002     c                   else
077900141002     c* con cons richiesta i dati devono essere + stretti
078000141003     c                   movel     idcdcr        w0080             8 0
078100141003     c                   movel     3             g02err
078200141003     C                   Z-ADD     w0080         G02inv
078300141003     C                   MOVEL     *zeros        G02dat
078400141003     C                   CALL      'XSRDA8'
078500141003     C                   PARM                    WLBDA8
078600141003     c
078700141002     c                   eval      v2cidc2= %trim(idcIND)+' '+%trim(idccad)+
078800141002     c                             ' ' +idcprd+' '+idclod
078900141006     c                   eval      v2CIDC3='   Consegna Richiesta IL'
079000141002     c                   eval      v2Cidc3=%TRIM(V2CIDC3) +'  '+
079100141002     c                                           %editw(g02dat:'  /  /    ')
079200141002     c                   if        idchcr>0
079300141002     c                   eval      v2cidc3=%trim(v2cidc3)+' '+
079400141002     c                                    %editw(idchcr: '  :  ')
079500141002     c                   endif
079600141002     C                   IF        idcprcr='M'
079700141003     c                   eval      v2cidc3= %trimr(v2cidc3)+
079800141003     c                                 ' Pref.Consegna MATTINO'
079900141002     c                   endif
080000141002     C                   IF        idcprcr='P'
080100141003     c                   eval      v2cidc3= %trimr(v2cidc3)+
080200141003     c                                      ' Pref.Consegna POMERIGGIO'
080300141002     c                   endif
080400141006     c                   move      ' '           v2cidc3
080500141002     c
080600141002     c                   endif
080700140813     c
080800070321     c                   endsl
080900140813     c
081000140813     c                   if        wblk=*blanks
081100140813     c                   move      *hival        wblk
081200140813     c                   endif
081300070321
081400070503     c* giro di consegna
081500140922     c***                clear                   v2ccgi
081600140922     c***                clear                   v2dcgi
081700140922     c***                clear                   fidg34ds
081800140922     c***                eval      id34tip='2'
081900140922     c***                eval      id34aas=arbaas
082000140922     c***                eval      id34lnp=arblnp
082100140922     c***                eval      id34nrs=arbnrs
082200140922     c***                eval      id34nsp=arbnsp
082300140922     c***                call      'FIDG34R'
082400140922     c***                parm                    FIDG34DS
082500140922     c***                movel     od34cgi       v2ccgi
082600140922     c***                eval      v2dcgi=od34cgides
082700070321     c*
082800070321     c* note partenza
082900070321     c                   clear                   v2cnot8
083000070321     c                   clear                   v2cnot9
083100070322     c                   clear                   v2hnot8
083200070322     c                   clear                   v2hnot9
083300070321     C                   MOVEL     '8'           WTRC
083400070328     C     Kar4          CHAIN     Fiar401l
083500070321     C                   if        %found(fiar401l)
083600070321     C                   MOVEL     ar4NOT        V2cnot8
083700070322     C                   MOVEL     ar4NOT        V2hnot8
083800070321     C                   ENDIF
083900070321     C*
084000070321     C                   MOVEL     '9'           WTRC
084100070328     C     Kar4          CHAIN     Fiar401l
084200070321     C                   if        %found(fiar401l)
084300070321     C                   MOVEL     ar4NOT        V2cnot9
084400070322     C                   MOVEL     ar4NOT        V2hnot9
084500070321     C                   ENDIF
084600140813     c
084700070321     C                   MOVEL     *BLANKS       V2cTIC
084800070321     C                   MOVEL     *ZERO         V2cCAS
084900070321     C                   MOVEL     *BLANKS       V2cVCA
085000091127     c                   setoff                                       15
085100070321     C     §3AFCA        IFGT      *ZERO
085200070322     C     Karb          CHAIN(N)  FiAR901L
085300070321     C                   if        %found(fiar901l)
085400070321     C                   MOVEL     AR9TIC        V2cTIC
085500070321     C                   MOVEL     AR9CAS        V2cCAS
085600070321     C                   MOVEL     AR9VCA        V2cVCA
085700070321     C                   ENDif
085800091216     c* richiamo trul per determinare l'intestatario del c/a
085900141001     c**                 if        dateu>=§VPOCAS
086000141001     c**                 clear                   trulintds
086100141001     c**                 eval      iinttic=v2ctic
086200141001     c**                 eval      iinttip='I'
086300141001     c**                 eval      IINTRSCM=arbrsm
086400141001     c**                 eval      IINTRSCO=arbrmo
086500141001     c**                 eval      IINTcdi=s_ar5cdi
086600141001     c**                 call      'TRULINTR'
086700141001     c**                 parm                    trulintds
086800141001     c**                 if        ointerr=*blanks
086900141001     c**                 seton                                        15
087000141001     c**                 eval      v2ccdi=ointrsci
087100141001     c**                 endif
087200141001     c**                 endif
087300141001     C                   ENDIF
087400140922
087500140922     c* Se in distinta visualizzo
087600140922     c                   if        arbndc>0 and arbddc>0 and
087700140922     c                             arbdcm=0
087800140922     c                   eval      *in67=*on
087900140922     c                   endif
088000141027     c* Tasti funzionali da abilitare
088100141027     c* DIROTTAMENTO
088200141027     c                   if        *in71
088300141027     C                   if        ILRY03DF12='S' or ILRY03DF12=' '
088400141027     c                   seton                                        22
088500141027     c                   endif
088600141027     C                   if        ILRY03DF06='S'
088700141027     c                   seton                                        23
088800141027     c                   endif
088900141028     C                   if        ILRY03DF21='N' or ILRY03DF21=' '
089000141027     c                   setoff                                       70
089100141027     c                   endif
089200141027     c
089300141027     c                   else
089400141027     C                   if        ILRY03eF12='S' or ILRY03eF12=' '
089500141027     c                   seton                                        22
089600141027     c                   endif
089700141027     C                   if        ILRY03eF06='S' or ILRY03eF06=' '
089800141027     c                   seton                                        23
089900141027     c                   endif
090000141028     C                   if        ILRY03eF21='N'
090100141027     c                   setoff                                       70
090200141027     c                   endif
090300141027     c                   endif
090400141027     c* passate le stampanti: non devo richiederle...
090500141027     c                   if        ilry03pss<>*blanks and ilry03mdb4<>*blanks
090600141027     c                   eval      wrichtrul90='S'
090700141027     c                   endif
090800070321     c*
090900070321     c
091000070315     C                   ENDSR
091100070321     c**********************************************************************
091200140813     c* Verifica abilitazione tasto F17-note aut
091300070321     c**********************************************************************
091400140813     C     SR_AbilF17    BEGSR
091500070412     c                   setoff                                       65
091600070321     c     karn          setll     fiarn11l
091700070321     c                   if        %equal(fiarn11l)
091800070412     c                   seton                                        65
091900070321     c                   endif
092000070321     c                   endsr
092100070315     c**********************************************************************
092200070315     c* Controlli video D01.
092300070315     c**********************************************************************
092400070315     C     Ctrd01        BEGSR
092500070315      *
092600070329     c                   setoff                                       404142
092700141001     c                   setoff                                       43
092800070329     c* ? su altre gestioni
092900070329     c                   if        v2cges='?'
093000140130     c                   clear                   fnlrg7ds
093100140813     c                   eval      ilrg7aas=ilry03aas
093200140813     c                   eval      ilrg7lnp=ilry03lnp
093300140813     c                   eval      ilrg7nrs=ilry03nrs
093400140813     c                   eval      ilrg7nsp=ilry03nsp
093500140130     c                   eval      ilrg7vsdet='?'
093600140130     c                   call      'FNLRG7R'
093700140130     c                   parm                    fnlrg7ds
093800070329     c                   clear                   v2cges
093900070329     c                   seton                                        90
094000070329     c                   goto      endct8
094100070329     c                   endif
094200130515     c* ? su particolarità
094300130515     c                   if        v2igga='?'
094400130515     C                   CLEAR                   DS7PQRS
094500130515     C                   MOVEL     'S'           DS7RIC
094600130515     C                   MOVEL     'V'           DS7GES
094700130515     C                   MOVEL     V2CGGA        DS7COD
094800130515     C                   MOVEL     DS7PQRS       KPJBU
094900130515     C                   CALL      'TRTB69R'
095000130515     C                   PARM                    KPJBA
095100130515     c                   clear                   v2igga
095200130515     c                   seton                                        90
095300130515     c                   goto      endct8
095400130515     c                   endif
095500130515
095600130515     c                   if        v2igma='?'
095700130515     C                   CLEAR                   DS7PQRS
095800130515     C                   MOVEL     'S'           DS7RIC
095900130515     C                   MOVEL     'V'           DS7GES
096000130515     C                   MOVEL     V2cgma        DS7COD
096100130515     C                   MOVEL     DS7PQRS       KPJBU
096200130515     C                   CALL      'TRTB70R'
096300130515     C                   PARM                    KPJBA
096400130515     c                   clear                   v2igma
096500130515     c                   seton                                        90
096600130515     c                   goto      endct8
096700130515     c                   endif
096800130515
096900130515     c                   if        v2igva='?'
097000130515     C                   CLEAR                   DS7PQRS
097100130515     C                   MOVEL     'S'           DS7RIC
097200130515     C                   MOVEL     'V'           DS7GES
097300130515     C                   MOVEL     V2cgva        DS7COD
097400130515     C                   MOVEL     DS7PQRS       KPJBU
097500130515     C                   CALL      'TRTB73R'
097600141013     C                   PARM                    kpJBA
097700130515     c                   clear                   v2igva
097800130515     c                   seton                                        90
097900130515     c                   goto      endct8
098000130515     c                   endif
098100070703     c* Causale per variazione bolla
098200070703    1C     v2ccvb        IFNE      *BLANKS
098300070703     C*
098400070703     C     '?'           SCAN      v2ccvb                                 80
098500070703     C* . . . Ricerca
098600070703    2C     *IN80         IFEQ      *ON
098700070703     c                   clear                   fnlr66ds
098800140813     C                   MOVEL     ilry03nsp     D66NSP
098900140813     C                   MOVEL     ilry03aas     D66AAS
099000140813     C                   MOVEL     ilry03lnp     D66LNP
099100140813     C                   MOVEL     ilry03nrs     D66NRS
099200070703     C                   MOVEL     'V'           D66GES
099300070703     C                   MOVEL     'A'           D66TBO
099400070703     C*
099500070703     C                   MOVEL     fnlr66ds      KPJBU
099600070703     C                   CALL      'FNLR66R'
099700070703     C                   PARM                    KPJBA
099800070703     C                   MOVEL     KPJBU         fnlr66ds
099900070703     C                   MOVEL     D66CVB        V2ccvb
100000070703     c                   seton                                        90
100100070703     C                   GOTO      ENDCT8
100200070703   X2C                   ELSE
100300070703     C* . . . Controllo
100400070703     C                   CLEAR                   fnlr66ds
100500070703     C                   MOVEL     'C'           D66GES
100600070703     C                   MOVEL     'A'           D66TBO
100700070703     C                   MOVEL     V2ccvb        D66CVB
100800070703     C*
100900070703     C                   MOVEL     fnlr66ds      KPJBU
101000070703     C                   CALL      'FNLR66R'
101100070703     C                   PARM                    KPJBA
101200070703     C                   MOVEL     KPJBU         fnlr66ds
101300070703     C                   MOVEL     D66CVB        V2ccvb
101400070703     C*
101500070703     C* . . . Errore
101600070703    3C     D66MSG        IFNE      *BLANKS
101700070703     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
101800070703     C                   SETON                                        284390
101900070703     C                   GOTO      ENDCT8
102000070703    3C                   ENDIF
102100070703    2C                   ENDIF
102200070703     C*
102300070703     C* Richiamo pgm di variazione se impostato il codice
102400070703     C*
102500141010     c                   exsr      Pul_lr48
102600141014     C                   MOVEL     V2ccvb        D48CVB
102700140910      /free
102800140910        select;
102900140910       // Appuntamento
103000151112        when (IDCTIPODIS='1' or (IDCTIPODIS='3' and idcfla=' ')) and
103100141204                                             %subst(d48cvb:1:1)='C' ;
103200141002        // non forzo se supermercato perchè bisognerebbe usare CS
103300141001           if arbtc1<>'S' and arbtc2<> 'S'   ;
103400140910           d48cvb='CI';
103500141001           endif  ;
103600141010           exsr  Car_ARBG  ;
103700141010
103800140910       // Fermo Deposito o Altro Indirizzo
103900141002        when (IDCTIPODIS='2' or IDCTIPODIS='3') and d48cvb='I0' ;
104000140910           d48trc='D';
104100141010           if idctipodis='2'
104200141010             and idctipocli='M';
104300140910              d48cvb='I7';
104400140911            endif;
104500141010            exsr Car_ARBD  ;
104600140910        endsl;
104700141010            exsr Call_LR48 ;
104800140910      /end-free
104900141010    2c                   if        d48err= *blanks
105000140911     c                   clear                   v2ccvb
105100141010     c                   endif
105200141010     c
105300070709     c                   goto      endct8
105400140911     C*
105500070703     c                   endif
105600141010     c
105700140911     c* Controllo campi input/output:
105800141013     c* --> Cellulare se errore lo clearo
105900140911      /free
106000141013       if v2cCELL <>*blanks and forzacell=*blanks ;
106100140911       pInCell = %trim(v2ccell);
106200140911       pOutErr = *blanks;
106300140911       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
106400140911
106500140911           // Numero di cellulare KO
106600140911          *in90=*on;
106700140911          *in28=*on;
106800140911          *in40=*on;
106900140911          vidmsg=msg(2);
107000141013          forzacell='S'  ;
107100140911          leavesr    ;
107200140911       endif;
107300140911       endif;
107400140911      /end-free
107500140911     c* indirizzo email
107600141028     c                   if        v2cmail<>*blanks and forzamail=*blanks
107700140911     c                   Clear                   dsemail
107800140911     c                   Eval      §emlindi = v2cmail
107900140911     c                   Call      'XEMAIL'
108000140911     c                   Parm                    dsemail
108100140911      * Errore
108200140911if  3c                   If        §emlerro <> '0'
108300140911     c                   Eval      *In90 = *On
108400140911     C                   EVAL      *in28=*on
108500140911     C                   EVAL      *in41=*on
108600140911     c                   Eval      vidmsg = msg(3)
108700141013     c                   eval      forzamail='S'
108800140911     c                   goto      endct8
108900140911   x3c                   else
109000140911     c                   Eval      v2cmail = §emlindo
109100140911    3c                   EndIf
109200140911     c                   endif
109300070315      *
109400070329     C     endct8        ENDSR
109500141028     c**********************************************************************
109600141028     c* Gestione tasto funzionale F21
109700141028     c**********************************************************************
109800141028     c     Ges_f21       BEGSR
109900151112     c* F21 - Dirottamento/reos
110000141028   1ac                   if        *in71
110100141028     c*
110200141028    2c                   if        wrichtrul90=' '
110300141028     c* La prima volta devo proporre la richiesta stampanti
110400141028     c                   exsr      RICHSTAMPAN
110500141028     c
110600141028     c* Se premuto F3 esco senza fare nulla
110700141028    3c                   if        d90f3<>'1' and not *in91
110800141028     c                   eval      wrichtrul90='S'
110900141028    3c                   endif
111000141028     c
111100141028    3c                   if        *in91
111200141028     C                   MOVEL     MSG(6)        VidMSG
111300141028     C                   SETON                                        2890
111400141028     c*****              iter
111500141028    3c                   endif
111600141028    2c                   endif
111700141028     c
111800141028    2c                   if        wrichtrul90='S'
111900141028     c                   clear                   fnlg82ds
112000141028     c                   clear                   fnlg20ds
112100141028     c                   eval      d82gia='N'
112200141028     c                   eval      d82tri='C'
112300151112     c
112400141028     c                   movel     arbaas        d82aas
112500141028     c                   movel     arblnp        d82lnp
112600141028     c                   movel     arbnrs        d82nrs
112700141028     c                   movel     arbnsp        d82nsp
112800141028     c                   if        ilry03pss<>*blanks
112900141028     c                   eval      D82MDB4=ilry03mdb4
113000141028     c                   eval      D82MDB5=ilry03mdb5
113100141028     c                   eval      D82pss =ilry03pss
113200141028     c                   eval      D82psl =ilry03psl
113300141028     c                   eval      D82bsl =ilry03bsl
113400141028     c                   eval      D82PRB4=ilry03prb4
113500141028     c                   eval      D82PRB5=ilry03prb5
113600141028     c                   eval      D82moe =ilry03mde
113700141028     c                   eval      D82pre =ilry03pre
113800141028     c                   eval      D82mob =ilry03mdb
113900141028     c                   eval      D82prb =ilry03prb
114000141028     c                   else
114100141028     c                   eval      D82MDB4=d90mdb4
114200141028     c                   eval      D82MDB5=d90mdb5
114300141028     c                   eval      D82pss =d90pss
114400141028     c                   eval      D82psl =d90psl
114500141028     c                   eval      D82bsl =d90bsl
114600141028     c                   eval      D82PRB4=d90prb4
114700141028     c                   eval      D82PRB5=d90prb5
114800141028     c                   eval      D82moe =d90mde
114900141028     c                   eval      D82pre =d90pre
115000141028     c                   eval      D82mob =d90mdb
115100141028     c                   eval      D82prb =d90prb
115200141028     c                   endif
115300141028     c
115400141028     c                   exsr      REP_fgs
115500141028     c                   movel     wfgs          d82fgs
115600151112     c                   if        v2hfla='S'
115700151112     c                   eval      d82dis='D'
115800151112     c                   else
115900151112     c                   eval      d82dis='R'
116000151112     c                   endif
116100141028     c                   movel     fnlg82ds      kpjbu
116200141028     c* indirizzo dirottato
116300141028     c                   eval      d20rsd=idcrsd
116400141028     c                   eval      d20rs2=%subst(idcrsd:36:35)
116500141028     c                   eval      d20ind=idcind
116600141028     c                   eval      d20lod=idclod
116700141028     c                   eval      d20cad=idccad
116800141028     c                   eval      d20prd=idcprd
116900141028     c                   eval      d20nzd=idcnaz
117000141028     c                   eval      d20ref=idcref
117100141028     c                   eval      d20tel=idcteld
117200141028     c                   eval      d20lna=idcnewlna
117300141028     c                   eval      d20snp='N'
117400141028     c                   if        §3afca<>0
117500141028     c                   eval      d20snp='S'
117600141028     c
117700141028     c                   if        §3afca=1 and %subst(§3atb1:1:1)='F'
117800141028     c                   eval      d20mdp='M'
117900141028     c                   else
118000141028     c                   eval      d20mdp='D'
118100141028     c                   endif
118200141028     c                   endif
118300141028     c
118400141028     c                   call      'FNLG20R'
118500141028     c                   parm                    kpjba
118600141028     c                   parm                    fnlg20ds
118700141028     c* se restituito errore --> lo emetto
118800141028     c                   if        d20erro= 'E'
118900141028     c                   seton                                        2890
119000141028     c                   eval      vidmsg=d20errmo
119100141028     c******             iter
119200141028     c                   endif
119300141028     c
119400141028     c* non è stato premuto F12 --> quindi creata bolla
119500141028     c                   if        d20erro= ' '
119600141028     c                   seton                                        66
119700141028     c                   endif
119800141028     c
119900141028    2c                   endif
120000141028     c                   else
120100141028     c* F21-Applica variazioni
120200141028     c                   exsr      Applica
120300141028   1ac                   endif
120400141028     c                   ENDSR
120500141028     c**********************************************************************
120600141028     c* conferma cieca
120700141028     c**********************************************************************
120800141028     c     conf_cieca    BEGSR
120900141028     c
121000141028     c* se non ci sono errori chiudo la dispo consegna
121100141028     c* controllo mail e cellulare: se errati non li scrivo
121200141028      /free
121300141028       if v2cCELL <>*blanks and forzacell=*blanks ;
121400141028       pInCell = %trim(v2ccell);
121500141028       pOutErr = *blanks;
121600141028       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
121700141028          forzacell='S'  ;
121800141028       endif;
121900141028       endif;
122000141028      /end-free
122100141028     c* indirizzo email
122200141028    2c                   if        v2cmail<>*blanks and forzamail=*blanks
122300141028     c                   Clear                   dsemail
122400141028     c                   Eval      §emlindi = v2cmail
122500141028     c                   Call      'XEMAIL'
122600141028     c                   Parm                    dsemail
122700141028      * Errore
122800141028if  3c                   If        §emlerro <> '0'
122900141028     c                   eval      forzamail='S'
123000141028   x3c                   else
123100141028     c                   Eval      v2cmail = §emlindo
123200141028    3c                   endif
123300141028    2c                   endif
123400141028     c
123500141028     c                   exsr      confe
123600141028     c
123700141028     c                   ENDSR
123800141010      /free
123900141010       //**********************************************************************
124000141010           BEGSR    Car_ARBG  ;
124100141010       //**********************************************************************
124200141010
124300141010           d48trc='G';
124400141010           §BGTCR=IDCTCR;
124500141010           §BGHCR=idchcr;
124600141010           §BGDCR=idcdcr;
124700141010           §BGGC1=arbgc1;
124800141010           §BGGC2=arbgc2;
124900141010       //   Giorni di chiusura --> se richiesta preferenza M/P la traduco nel primo campo
125000141010       //                          libero dei giorni di chiusura
125100141010           clear wggc;
125200141010           if idcprcr<>' ';
125300141010           if idcprcr='M';
125400141010              wggc='P';
125500141010           else;
125600141010              wggc='M';
125700141010           endif;
125800141010           endif;
125900141010           if wggc<>*blanks;
126000141010       // Nel primo byte  del giorno di chiusura devo mettere il numero del giorno della
126100141010       // settimana che corrisponde alla data cons rich.
126200141010       // Nel secondo byte metto mattino o pomeriggio
126300141010               dataiso=%date(§bgdcr:*iso);
126400141010               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
126500141010              if §BGGC1=*blanks;
126600141010                 %subst(§BGGC1:2:1)=wggc;
126700141010                 %subst(§BGGC1:1:1)=%editc(dw:'X');
126800141010              else;
126900141010                 %subst(§BGGC2:2:1)=wggc;
127000141010                 %subst(§BGGC2:1:1)=%editc(dw:'X');
127100141010              endif;
127200141010              if §BGGC1=§BGGC2;
127300141010                 clear §BGGC2;
127400141010              endif;
127500141010           endif;
127600141010           §BGTC1=arbtc1;
127700141010           §BGTC2=arbtc2;
127800141010           ENDSR  ;
127900141010
128000141010       //**********************************************************************
128100141010            BEGSR   Car_ARBD  ;
128200141010       //**********************************************************************
128300141010
128400141010           if idctipodis='2';
128500141010              §BDRSD=arbrsd;
128600141010              §BDRD2=wrsd2 ;
128700141010              §BDIND=arbIND;
128800141010              §BDCAD=arbCAD;
128900141010              §BDLOD=arbLOD;
129000141010              §BDPRD=arbPRD;
129100141010              §BDNZD=arbNzd;
129200141010              §BDFIN=arbfin;
129300141010              §BDFFD=idcffd;
129400141017              if §bdffd='S';
129500141017                 §bdffd='Y';
129600141017              endif;
129700141010           else ;
129800141010              §BDRSD=%subst(idcrsd:1:35);
129900141010              §BDRD2=%subst(IDCRSD:36:35);
130000141010              §BDIND=IDCIND;
130100141010              §BDCAD=IDCCAD;
130200141010              §BDLOD=IDCLOD;
130300141010              §BDPRD=IDCPRD;
130400141010              §BDNZD=IDCNAZ;
130500141010       // Se CAP/LOCALITA/PROVINCIA sono invariati rispetto alla bolla passo
130600141010       // flag inoltro = alla bolla altrimenti lo passo vuoto in modo da farlo ricalcolare
130700141010       // a fnlr48r in base ai nuovi dati
130800141010              if arbcad=idccad and arblod=idclod and arbprd=idcprd;
130900141010                §BDFIN = arbfin;
131000141010              endif;
131100141010              §BDFFD=arbffd;
131200141010           endif;
131300141010       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
131400141010       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
131500141010           §BDPKF  = arbpkf;
131600141010           §BDVLF  = arbvlf;
131700141010           §BDFVF  = arbfvf;
131800141010           §BDCPI  = ARBCPI;
131900141010           §BDREF  = IDCREF;
132000141010           §BDTEL  = IDCTELD;
132100141010         ENDSR  ;
132200141010
132300141010      /end-free
132400141010
132500141010     c**********************************************************************
132600141010     c* Pulizie e impostazioni DS per richiamo fnlr48r
132700141010     c**********************************************************************
132800141010     c     Pul_LR48      BEGSR
132900141010     C                   CLEAR                   fnlr48ds
133000141010     C                   CLEAR                   dsarbg
133100141010     C                   CLEAR                   dsarbd
133200141010     C                   CLEAR                   dsarbk
133300141010     C                   CLEAR                   darbt
133400141010     C                   z-add     ilry03aas     D48AAS
133500141010     C                   z-add     ilry03lnp     D48LNP
133600141010     C                   z-add     ilry03nrs     D48NRS
133700141010     C                   z-add     ilry03nsp     D48NSP
133800141010     c                   exsr      rep_FGS
133900141010     c
134000141010     c                   movel     wfgs          d48fgs
134100141010     C                   MOVEL     ARBCBO        D48CBO
134200141010     C                   MOVEL     'A'           D48TBO
134300141010     C                   MOVEL     'S'           D48F12
134400141010     C                   MOVEL     'V'           D48FFR
134500141010     c                   ENDSR
134600141010     c**********************************************************************
134700141010     c* Applicazione automatica variaizoni
134800141010     c**********************************************************************
134900141010     c     Applica       BEGSR
135000141010      /free
135100141010
135200141010       // Fermo Deposito o Altro Indirizzo
135300141010        if   (IDCTIPODIS='2' or IDCTIPODIS='3') ;
135400141014           exsr  Pul_lr48 ;
135500141010           d48trc='D';
135600141010           d48cvb='I0';
135700141010           if idctipodis='2'
135800141010              and idctipocli='M';
135900141010              d48cvb='I7';
136000141010           endif;
136100141010            exsr Car_ARBD  ;
136200141010           exsr  Call_lr48 ;
136300141010         endif  ;
136400141010
136500141010       // Appuntamento
136600141010        if    IDCTIPODIS='1' or (IDCTIPODIS='3'and idcdcr>0) ;
136700141010        // non forzo se supermercato perchè bisognerebbe usare CS
136800141014           exsr  Pul_lr48 ;
136900141014
137000141010           if arbtc1<>'S' and arbtc2<> 'S'   ;
137100141010           d48cvb='CI';
137200141010           else  ;
137300141010           d48cvb='CS';
137400141010           endif  ;
137500141010           exsr  Car_ARBG  ;
137600141010
137700141010           exsr  Call_lr48 ;
137800141010
137900141010        if *in66  ;
138000141010         *in70=*off ;
138100141010        endif ;
138200141010
138300141010         endif  ;
138400141010      /end-free
138500141010     c                   ENDSR
138600141010     c**********************************************************************
138700141010     c     Call_lr48     BEGSR
138800141010     c**********************************************************************
138900141010     C                   MOVEL     fnlr48ds      KPJBU
139000141010     C                   CALL      'FNLR48R'
139100141010     C                   PARM                    KPJBA
139200141010     C                   PARM                    DSARBD
139300141010     C                   PARM                    DSARBK
139400141010     C                   PARM                    DARBT
139500141010     C                   PARM                    DSARBG
139600141010     C                   PARM                    TRUL90DS
139700141010     c                   movel     kpjbu         fnlr48ds
139800141010     c
139900141010     c* se non premuto f12, quindi confermato, disabilito F12=Ritorno per obbligare a dare F6
140000141217     c*  nobn lo faccio nel caso sia dispo dirottamento
140100141112     c                   if        d48f12<>'1' and d48err=*blanks
140200151112     c                             and idcfla=' '
140300141010     c                   seton                                        66
140400141010     c                   endif
140500141010     c* Errore
140600141010    2c                   if        d48err<>*blanks
140700141010     c                   seton                                        90
140800141010     c                   seton                                        2843
140900141010     c                   movel     d48msg        vidmsg
141000141010    2c                   endif
141100141010     c
141200141010     c                   ENDSR
141300141006     c**********************************************************************
141400141010     c* Reperisco filiale gestione se non passata
141500141006     c**********************************************************************
141600141006     c     Rep_FGS       BEGSR
141700141006     c                   clear                   WFGS              3 0
141800141006     c                   if        ILRY03FGS>0
141900141006     c                   eval      WFGS=ILRY03FGS
142000141006     c                   else
142100141006     c* come p.o. gestione passo il gestore della lna
142200141006     c                   clear                   fnlv55ds
142300141006     c                   z-add     arblna        d55lin
142400141006     c                   movel     '6'           d55tpt
142500141006     C                   time                    wrkoggiiso
142600141006     C     *iso          move      wrkoggiiso    d55drf
142700141006     c                   call      'FNLV55R'
142800141006     c                   parm                    fnlv55ds
142900141006     c                   movel     d55tfa        wfgs
143000141006     c                   endif
143100141006     c                   ENDSR
143200141006     c**********************************************************************
143300141006     C*--- CHAIN TABEL00F
143400141006     c**********************************************************************
143500070316     C     CTABEL        BEGSR
143600070316     C     KTAB          CHAIN     TABEL                              30
143700070316     c                   if        %found(tabel00f) and tblflg<> ' '
143800070316     C                   SETON                                        30
143900070316     C                   END
144000070316     C                   ENDSR
144100070321     c**********************************************************************
144200070321     c* F6=Conferma
144300070321     c**********************************************************************
144400070321     C     Confe         BEGSR
144500070322
144600070327     c*  N O T E ************
144700070322     c* Note di ar4 trk 8 e 9
144800070420    1c                   if        v2hnot8<>v2cnot8 or v2hnot9<>v2cnot9
144900131008     c* richaimo pgm di manutenzione bolle per aggiornamento delle note
145000131008     c                   clear                   fnlr48ds
145100131008     c                   eval      d48cvb='NT'
145200131008     c                   eval      d48ffr='E'
145300140813     C                   z-add     ilry03aas     D48AAS
145400140813     C                   z-add     ilry03lnp     D48LNP
145500140813     C                   z-add     ilry03nrs     D48NRS
145600140813     C                   z-add     ilry03nsp     D48NSP
145700131008     C                   MOVEL     ARBCBO        D48CBO
145800131008     C                   MOVEL     'A'           D48TBO
145900131008     C                   MOVEL     'S'           D48F12
146000131008     C                   MOVEL     'N'           D48trc
146100131008     C                   MOVEL     fnlr48ds      KPJBU
146200131008     c                   eval      §bnu_sn='S'
146300131008     c                   eval      §bnuni70=v2cnot8+v2cnot9
146400131008     c                   eval      §bntrd='NOT'
146500131008     c                   call      'FNLR48R'
146600131008     C                   PARM                    KPJBA
146700131008     C                   PARM                    DSARBD
146800131008     C                   PARM                    DSARBK
146900131008     C                   PARM                    DARBT
147000131008     C                   PARM                    DSARBG
147100131008     C                   PARM                    trul90ds
147200131008     C                   PARM                    DSARBN
147300131008     C                   MOVEL     KPJBU         fnlr48ds
147400131008
147500131008     c* Ritorno con errore o premuto f12
147600131008    2c                   if        d48err<>*blanks
147700131008     c                   seton                                        90
147800131008     c                   seton                                        28
147900131008     c                   movel     d48msg        vidmsg
148000131008     c                   goto      f_confe
148100131008    2c                   endif
148200131008    1c                   endif
148300131008     c
148400140911     c* richiamo pgm di manutenzione bolle per aggiornamento delle note
148500140911      /free
148600140911           clear fnlr48ds;
148700140911           d48aas=ilry03aas;
148800140911           d48lnp=ilry03lnp;
148900140911           d48nrs=ilry03nrs;
149000140911           d48nsp=ilry03nsp;
149100140911           d48ffr='E'   ;
149200140911           d48cvb='NE';
149300140911           d48trc='N';
149400140911           d48tbo='A'   ;
149500140911           d48cbo=arbcbo;
149600141013
149700140911           clear dsarbn;
149800141013           // non memorizzo se ha dato errore
149900141013           if forzamail=' '  ;
150000141013           §BNU_SN='S';
150100140911           §BNUNI70=v2cmail   ;
150200141013           endif  ;
150300140911           §BNR_SN='S';
150400140911           §BNREF=v2cref;
150500140911           §BNTEL=v2ctel ;
150600140911           §BNC_SN='S';
150700141013           // non memorizzo se ha dato errore
150800141013           if forzacell=' '  ;
150900140911           §BNCEL=v2ccell  ;
151000141013           endif  ;
151100140911           kpjbu=fnlr48ds;
151200140911           fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds:dsarbn);
151300140912           fnlr48ds=kpjbu;
151400140912    2      if d48err<>*blanks;
151500140912             *in90=*on;
151600140912             *in28=*on;
151700140912             vidmsg=d48msg;
151800140912             leavesr;
151900140912    2      endif;
152000151112       // Se premuto F6 senza aver dirottato/reso tolgo il giro DIR dalla bolla
152100151112           if idcfla<>' ' and *in66=*off;
152200141103              chain (idcaas:idclnp:idcnrs:idcnsp) fiarg01l;
152300141103              if %found(fiarg01l);
152400141103                 clear argcgi;
152500141103                 clear argtgi;
152600141103                 update fiarg000;
152700141103              endif;
152800141103           endif;
152900141001       //CHIUSURA NOTE R.A.
153000140911           exsr sr_noteRa;
153100140911       //Aggiorno il record TIIDC
153200140813              IDCELAUTE=KNMUS;
153300140813              IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
153400140813                                 :1 :14 ) );
153500140814              update tiidc000  ;
153600140813      /end-free
153700070327     c
153800070322     c     f_confe       endsr
153900070411     c**********************************************************************
154000070411     c* Verifica abilitazione tasto F16-Gestione richieste assistenza
154100070411     c**********************************************************************
154200070411     C     SR_AbilF16    BEGSR
154300070411     c                   eval      TGDtor  = 'S'
154400070411     c                   eval      tgdogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
154500070411     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
154600070411     c     K02TGD02      setll     FITGD000
154700160404     c                   eval      *in64   = %equal(FITGD32L)
154800141002      /free
154900141002         XRTVSTKR (pStack:pStartPoint:pEntries);
155000141002         dsStack = pStack;
155100141002         IF  %lookup('FIDNA2R   ':skStack) > 0;
155200141002          *in64=*off ;
155300141002         endif      ;
155400141002
155500141002       endsr   ;
155600140911       //-------------------------------------------------------------*
155700140911       //Richiamo pgm di controllo eseguibilità dell'indicazione      *
155800140911       //-------------------------------------------------------------*
155900140911        Begsr sr_ChkIstruCo;
156000140911
156100140911        werrchk='0';
156200140911
156300140911        prmrqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO    ;
156400140911        prmrqsformato='TIIDCDS  '  ;
156500140911        prmrpyformato='         '  ;
156600140911       prmRpyFormMsg='FNLRY00M0'  ;
156700140911
156800140911         prmrqsData = tiidcds  ;
156900140911         prmrqsDatasize = %SIZE(tiidcds);
157000140911         prmrpyDatasize = %SIZE(tiidcds);
157100140911         prmRpyMsgSize  = %SIZE(fnlry00m0);
157200140911         prmRpyForSize  = %SIZE(fnlry00f0);
157300140911         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
157400140911                         prmRqsFormato : prmRqsData : prmRqsDataSize:
157500140911                         prmRpyFormato : prmRpyData : prmRpyDataSize:
157600140911                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
157700140911         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
157800140911
157900140911 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
158000140911            werrchk='1';
158100140911            fnlry00m0=prmrpymessage;
158200140911         endif;
158300140911
158400140911        endsr;
158500140911      /end-free
158600141001     c**********************************************************************
158700141001     c* CHIUSURA  Note R.A.
158800141001     c**********************************************************************
158900141001     C     sr_noteRa     BEGSR
159000141001     c                   clear                   fidna7ds
159100141001     C                   EVAL      IDNA7ANT=IDCANT
159200141001     C                   EVAL      IDNA7NUT=IDCNUT
159300141001     C                   EVAL      IDNA7PDT=IDCPDT
159400141001     C                   EVAL      IDNA7PCT=DUTPOU
159500141001     C                   EVAL      IDNA7utc=knmus
159600141006     c                   eval      IDNA7NOT=wnot
159700141001     c                   call      'FIDNA7R'
159800141001     c                   parm                    fidna7ds
159900141001     c
160000141001     C                   ENDSR
160100141003     c**********************************************************************
160200141003     C* richiama finestra per inserimento nuova R.A.
160300141003     c**********************************************************************
160400141003     C     WIN_RA        BEGSR
160500141003     c     forw          tag
160600141006     c                   setoff                                       68
160700141003     c                   exfmt     LRY03W03
160800141003     c                   setoff                                       2890
160900141003     c
161000141008     c**                 if        *inkl  or *inkf
161100141006     c                   eval      wnot    =LRY03W1+LRY03W2+LRY03W3+LRY03W4
161200141006     c                   if        wnot <>*blanks
161300141006     c                   seton                                        68
161400141006     c                   endif
161500141003     c                   leavesr
161600141008     c**                 endif
161700141006
161800141006     c                   goto      forw
161900141003     c
162000141003     C                   ENDSR
162100011227     c**********************************************************************
162200000607     C     *INZSR        BEGSR
162300011227     c**********************************************************************
162400070320     c
162500070123      *
162600070315     c     *dtaara       define    §azute        azuteds
162700070315     c     *dtaara       define    §datiute      ddatiute
162800070315     c                   in(e)     *dtaara
162900070315     c                   if        %error  or rsut = *blanks
163000070315     c                   clear                   tibs34ds
163100070315     c                   call      'TIBS34R'
163200070315     c                   parm                    tibs34ds
163300070315     c                   in        *dtaara
163400070315     c                   endif
163500030702      *reperimento data
163600070319     C                   TIME                    W0140            14 0
163700070319     C                   MOVE      W0140         WDAT              8 0
163800030702     C*
163900030702     C                   Z-ADD     WDAT          G02DAT
164000030702     C                   MOVEL     *BLANK        G02ERR
164100030702     C                   CALL      'XSRDA8'
164200030702     C                   PARM                    WLBDA8
164300030702     C* UDATE A 8 IN AAAA/MM/GG
164400030717     C                   Z-ADD     G02INV        WUDAte            8 0
164500091216      * Aggancio tabella "VPODECO" per reperimento data attivazione
164600091216      * gestione prepagati
164700151119     C***                CLEAR                   dsbs02
164800151119     c***                clear                   dvpodeco
164900151119     C***                MOVEL     'C'           T02MOD
165000151119     C***                MOVEL     KNSIF         T02SIF
165100151119     C***                MOVEL     'VPO'         T02COD
165200151119     c***                movel(P)  'DECO'        t02ke1
165300151119     C***                CALL      'TIBS02R'
165400151119     C***                PARM                    KPJBA
165500151119     C***                PARM                    dsbs02
165600151119    2C***                IF        T02ERR = *BLANKS
165700151119     C***                MOVEL     T02uni        dvpodeco
165800151119    2C***                ENDIF
165900070123      *
166000070124     C     Karb          klist
166100140813     C                   kfld                    ilry03aas
166200140813     C                   kfld                    ilry03lnp
166300140813     C                   kfld                    ilry03nrs
166400140813     C                   kfld                    ilry03nsp
166500140813     C     Kidc          klist
166600140813     C                   kfld                    ilry03aas
166700140813     C                   kfld                    ilry03lnp
166800140813     C                   kfld                    ilry03nrs
166900140813     C                   kfld                    ilry03nsp
167000140909     C                   kfld                    k_eladata
167100140813     C                   kfld                    ilry03prg
167200070201      *
167300070201     C     Kar4          klist
167400070201     C                   kfld                    arbaas
167500070201     C                   kfld                    arblnp
167600070201     C                   kfld                    arbnrs
167700070201     C                   kfld                    arbnsp
167800070321     c                   kfld                    wtrc
167900070321     c     KFiar5        Klist
168000070321     C                   kfld                    arbaas
168100070321     C                   kfld                    arblnp
168200070321     C                   kfld                    arbnrs
168300070321     C                   kfld                    arbnsp
168400070321     c                   Kfld                    KAr5Trd
168500070201      *
168600070201     C     Ktab          klist
168700070201     C                   kfld                    tblkut
168800070201     C                   kfld                    tblcod
168900070201     C                   kfld                    tblkey
169000070201     C                   z-add     1             tblkut
169100070321     c     KArn          Klist
169200070321     C                   kfld                    arbaas
169300070321     C                   kfld                    arblnp
169400070321     C                   kfld                    arbnrs
169500070321     C                   kfld                    arbnsp
169600070321     c                   Kfld                    KArncdn
169700070321     c                   Kfld                    KArngst
169800160404      * File FITGD32L
169900070411     c     K02TGD02      klist
170000070411     c                   kfld                    TGDtor
170100070411     c                   kfld                    TGDogg
170200070614     c*
170300070614     C     *LIKE         DEFINE    TBLKUT        §KUT
170400070614     C     *LIKE         DEFINE    TBLCOD        §COD
170500070614     C     *LIKE         DEFINE    TBLKEY        §KEY
170600030714
170700011227     C                   ENDSR
170800070328     c**********************************************************************
170900070328     C     chiudi        BEGSR
171000070328     c**********************************************************************
171100070328     c                   eval      d48tla='C'
171200070328     C                   MOVEL     fnlr48ds      KPJBU
171300070328     c                   call      'FNLR48R'
171400070328     C                   PARM                    KPJBA
171500070328     C                   PARM                    DSARBD
171600070328     C                   PARM                    DSARBK
171700070328     C                   PARM                    DARBT
171800070328     C                   PARM                    DSARBG
171900070328     C                   PARM                    trul90ds
172000070328
172100070328     c                   clear                   fnlv80ds
172200070328     c                   eval      ilv80tla='C'
172300070328     c                   call      'FNLV80R'
172400070328     c                   parm                    kpjba
172500070328     c                   parm                    fnlv80ds
172600070328
172700140130     c                   eval      ilrg7tla='C'
172800140130     c                   call      'FNLRG7R'
172900140130     c                   parm                    fnlrg7ds
173000070503
173100140922     c**                 eval      id34tla = 'C'
173200140922     c***                call      'FIDG34R'
173300140922     c***                parm                    fidg34ds
173400070704
173500070704     c                   eval      d66tla  = 'C'
173600120126     C                   MOVEL     fnlr66ds      KPJBU
173700070704     c                   call      'FNLR66R'
173800120126     c                   parm                    kpjba
173900070704
174000070704     c                   clear                   fnlv55ds
174100070704     C                   MOVEL     'C'           D55TLA
174200070704     C                   CALL      'FNLV55R'
174300070704     C                   PARM                    FNLv55DS
174400091216     c*
174500091216     c                   clear                   trulintds
174600091216     C                   MOVEL     'C'           iinttla
174700091216     C                   CALL      'TRULINTR'
174800091216     C                   PARM                    TRULINTDS
174900070328     c                   endsr
175000141008     C**************************************************************************
175100141008      * RICHIEDO LE STAMPANTI per stampa LDV
175200141008     C**************************************************************************
175300141008     c     RICHSTAMPAN   BEGSR
175400141008     C                   CLEAR                   TRUL90DS
175500141008      *
175600141008     C                   MOVEL     'S'           d90RSB
175700141008     C                   MOVEL     'S'           d90RSE
175800141008     C                   CALL      'TRUL90R'
175900141008     C                   PARM                    KPJBA
176000141008     C                   PARM                    trul90DS
176100141008      *
176200141008     C* F3 - FINE
176300141008     c                   if        d90f3<>'1'
176400141008      *
176500141008      * OVRPRTF BOLLE A4
176600141008     C                   MOVEA     D90PRB4       CMA4f6(30)
176700141008     C                   MOVEA     D90MDB4       CMA4f6(52)
176800141008     C*
176900141008     C                   Z-ADD     130           LUNG             15 5
177000141008     C                   CLEAR                   COMMAN          130
177100141008     C                   MOVEA     CMA4f6        COMMAN
177200141008     C                   CALL      'QCMDEXC'                            91
177300141008     C                   PARM                    COMMAN
177400141008     C                   PARM                    LUNG
177500141008      *
177600141008     C                   if        not *in91
177700141008      *
177800141008      * OVRPRTF BOLLE A5
177900141008     C                   MOVEA     D90PRB5       CMA5f6(30)
178000141008     C                   MOVEA     D90MDB5       CMA5f6(52)
178100141008     C*
178200141008     C                   Z-ADD     130           LUNG
178300141008     C                   CLEAR                   COMMAN
178400141008     C                   MOVEA     CMA5f6        COMMAN
178500141008     C                   CALL      'QCMDEXC'                            91
178600141008     C                   PARM                    COMMAN
178700141008     C                   PARM                    LUNG
178800141008     c                   endif
178900141008     C                   if        not *in91
179000141008      *
179100141008      * OVRPRTF etichette
179200141008     C                   MOVEA     D90PRe        CMetf6(30)
179300141008     C                   MOVEA     D90MDE        CMetf6(52)
179400141008     C*
179500141008     C                   Z-ADD     130           LUNG
179600141008     C                   CLEAR                   COMMAN
179700141008     C                   MOVEA     CMetf6        COMMAN
179800141008     C                   CALL      'QCMDEXC'                            91
179900141008     C                   PARM                    COMMAN
180000141008     C                   PARM                    LUNG
180100141008     c                   endif
180200141008     c                   endif
180300141008      *
180400141008     c                   ENDSR
180500070315**
180600141003Non trovate disposizioni di consegna da elaborare                              1
180700141013Presenti errori nel numero di cellulare del destinatario.Enter X forzare       2
180800141013Presenti errori nell'indirizzo Email Destinatario. enter x forzare             3
180900141001Disposizione di consegna ancora da elaborare                                   4
181000141008Non è stata eseguita nessuna variazione bolla.Premi F6 per forzare l'uscita    5
181100141008Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede         6
181200141024Non trovata spedizione                                                         7
181300151116Uscita senza esecuzione del RESO/DIROTTAMENTO: inserire note R.A.              8
181400141008**  cmA4f6
181500141008OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
181600141008USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
181700141008**  cmA5f6
181800141008OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
181900141008             USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
182000141008**  cmETf6
182100141008OVRPRTF FILE(FNLV22P)   OUTQ(xxxxxxxxxx) FORMTYPE('XXXXXXXXXX')
182200141008USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
