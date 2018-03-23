000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140911     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000300140813     H* FNLRY03R *---------------------------------------------------*
000400140813     H* - Gestione disposizioni di consegna
000500140130     H*--------------------------------------------------------------*
000600070207      *
000700140812     Ffnlry03d  CF   E             WORKSTN
000800070621     Ffnarb01l  if   E           k disk
000900070621     F                                     RENAME(FNarb000:FNarb001)
001000170111     Ffnblp01l  if   E           k disk    prefix(arb:3)
001100140909     Ftiidc01l  uf   E           k disk
001200140814     FFNarb00F  iF   E             DISK
001300070328     Ffiar401l  If   E           K disk
001400070322     F                                     RENAME(Fiar4000:Fiar4001)
001500140814     Ffiar501l  if   E           k disk
001600140814     Ffiar901l  if   E           k disk
001700070201     Ftabel00f  if   E           k disk
001800070320     Fazorg01l  if   E           k disk
001900070321     Ffiarn11l  if   E           k disk
002000141103     Ffiarg01l  uf   E           k disk
002100070411      *.......... T E L E F O N A T E   /   R E C L A M I
002200160404     fFITGD32L  if   e           k disk
002300011228      *
002400011228     D Kpjba         E DS
002500140813     d
002600140813     d* DS PASSAGGIO DATI
002700141024     d*  O L D
002800141024     D fnlry03ds     E DS                  prefix(OL_)
002900141024     d*  N E W
003000141024     D fnlry03ds2    E DS
003100011228      *
003200070328     D DSARBD        E DS
003300070328     D DSARBG        E DS
003400131008     D DSARBN        E DS
003500070328     D DARBT         E DS
003600070328     D DSARBK        E DS
003700140911     d tiidcds       e ds                  extname(tiidc00f)
003800140911     d fnlry00m0     e ds
003900140911     d fnlry00f0     e ds                  qualified inz(*extdft)
004000070328     D fnlr48ds      E DS
004100070703     D fnlv55ds      E DS
004200070703     D fnlr66ds      E DS
004300070328     D trul90ds      E DS
004400130515     D DS7PQRS       E DS
004500140130     D fnlrn6ds      E DS
004600140130     D fnlrg7ds      E DS
004700070320     D fnlv80ds      E DS
004800070329     d ILV80NWPROC     s              1
004900131008     d
005000070201     D fidn12ds      E DS
005100070410     D fidna2ds      E DS
005200070410     d  IA2mod       e                     inz('M')
005300070410     d  IA2tor       e                     inz('S')
005400141001
005500141001     D fidna7ds      E DS
005600141006
005700141006     D fnlg82ds      E DS
005800141007     D fnlg20ds      E DS
005900141001
006000140409     D fnlrj6ds      E DS
006100070926     d TRTB2Ads      e ds                  inz
006200070926     d   TB2Aopz     e                     inz('1')
006300070926     d   TB2Atip     e                     inz('G')
006400070315     d azuteds       e ds                  extname(azute00f)
006500070315     d ddatiute      e ds
006600070315     d tibs34ds      e ds                  inz
006700070131     D trul06ds      E DS
006800070131     D  lin                    1     90  0 dim(30)
006900091216     D trulintds     E DS
007000140911     d dsemail       e ds
007100141002
007200141002      // - Parametri per ricerca Programmi chiamanti
007300141002     d dsStack         ds
007400141002     d  skStack                      10    Dim(100)
007500141003     d
007600141002      *
007700070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
007800070206     D PARAM1          DS                  INZ
007900070206     D  PA1AAS                14     17  0
008000070206     D  PA1LNP                18     20  0
008100070206     D  PA1NRS                21     22  0
008200070206     D  PA1NSP                23     29  0
008300070206     D  PA1F03                30     30
008400070206     D  PA1FLG                31     31
008500070322     D  PA1PGM                32     41
008600070322     D  PA1DSA                42     61
008700070322     D  PA1CML                62     62
008800070322     D  PA1DSP                63     70  0
008900070322     D  PA1RMN                71     85  0
009000070322     D  PA1LNA                86     88  0
009100070322     D  PA1LN1                89    108
009200070322     D  PA1RSD               109    143
009300070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
009400070206     D  PA1GIA               144    144
009500070322     D  PA1FEL               145    147  0
009600070322     D  PA1TBO               148    148
009700070201     d ds3a          e ds
009800070320     d ds5e          e ds
009900080909     d dsbl4a        e ds
010000070321     d dar5Gen       e ds
010100140813     d dar5Emd       e ds
010200151119     d****** dVpodeco      e ds
010300091216     D* DS PER TIBS02R - GESTIONE TNTBE00F
010400091216     D DSBS02        E DS                  EXTNAME(TIBS02DS)
010500011228      *
010600070329
010700030613     D WLBDA8          DS
010800030613     D  G02DAT                 1      8  0
010900030613     D  G02INV                 9     16  0
011000030613     D  G02ERR                17     17
011100030613     D  G02TGI                18     22  0
011200070327     D                SDS
011300070327     D  N_pgm                  1      8
011400070621
011500070328     d dataeur         s               d   datfmt(*eur)
011600140910     d dataiso         s               d   datfmt(*iso)
011700070703     D wrkoggiiso      s               d
011800070321     d KAr5Trd         s                   like(Ar5Trd)
011900070321     d KArnCdn         s                   like(ArnCdn)inz('LDV')
012000070321     d KArngst         s                   like(Arngst)inz('S')
012100070321     d wtrc            s                   like(ar4trc)
012200141008     D wrichtrul90     S              1
012300070321     d wblk            s              1
012400141006     d wnot            s            300
012500091127     d s_ar5cdi        s                   like(§ar5cdi)
012600140409     d wncl            s                   like(arbncl)
012700141001     d k_eladata       s                   like(idceladata) inz
012800140910     d wrsd2           s             35
012900140910     d wggc            s                   like(idcprcr)
013000140910     d wgg             s              2  0
013100140910     d wmm             s              2  0
013200140910     d waa             s              4  0
013300140910     d dw              s              1s 0
013400140911     d werrchk         s              1
013500141008     d wforzaF6        s              1
013600141013     d
013700141013     d forzacell       s              1
013800141013     d forzamail       s              1
013900070924      *
014000070924     d Cod             s                   like(TBLcod)  inz
014100070924     d Key             s                   like(TBLkey)  inz
014200070924     d Ordina          s              1                  inz
014300070924     d Comando         s              1                  inz
014400140911
014500140911     D prmRqsOpCode    s             10I 0
014600140911     D prmRpyOpCode    s             10I 0
014700140911     D prmRpyIdMsg     s             10I 0
014800140911     D prmRqsFormato   s             10A
014900140911     D prmRqsData      s          32767A
015000140911     D prmRqsDataSize  s             10I 0
015100140911     D prmRpyFormato   s             10a
015200140911     D prmRpyData      s          32767
015300140911     D prmRpyDataSize  s             10I 0
015400140911     D prmRpyFormMsg   s             10A
015500140911     D prmRpyMessage   s          32767
015600140911     D prmRpyMsgSize   s             10I 0
015700140911     D prmRpyFormForz  s             10A
015800140911     D prmRpyForzatu   s          32767
015900140911     D prmRpyForSize   s             10I 0
016000141002      // - Campi per procedura esterna
016100141002     d  pStack         s           1000
016200141002     d  pStartPoint    s              3  0
016300141002     d  pEntries       s              3  0
016400070319
016500170111     D red             C                   CONST(X'28')
016600070316      *---------------------------------------------------------------------
016700141001     D cosapp          C                   CONST(' CONSEGNA  RICHIESTA  ')
016800141001     D cosffd          C                   CONST('   FERMO  DEPOSITO    ')
016900141001     D cosind          C                   CONST('INDIRIZZO  ALTERNATIVO')
017000141001     D cosaltro        C                   CONST('  ALTRE  INDICAZIONI  ')
017100141001     D msg             S             78    DIM(10) CTDATA PERRCD(1)
017200141008      *
017300141008     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
017400141008     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
017500141008     D CMETf6          S              1    DIM(130) CTDATA PERRCD(65)
017600140911      // ? PROTOTIPI ?
017700140911      /copy gaitrasrc/srcprotopr,ubchkcel
017800140911      /copy gaitrasrc/srcprotopi,ubchkcel
017900140911      /copy gaitrasrc/srcprotopr,fnlr48r
018000140911      /copy gaitrasrc/srcprotopr,Fnlry00r
018100140911      /copy gaitrasrc/srcconst,FNLRY00R
018200141002
018300141002       // - Pgm per ricerca Programmi chiamati
018400141002     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
018500141002     d  pStack                     1000
018600141002     d  pStartPoint                   3  0
018700141002     d  pEntries                      3  0
018800141002
018900070320      *---------------------------------------------------------------------
019000141027      * INDICATORI
019100141027      *
019200141027      * 70 - abilitazione F21
019300141027      * 71 - dirottamento
019400141027      *---------------------------------------------------------------------
019500070320     C     *ENTRY        PLIST
019600070320     C                   PARM                    Kpjba
019700140813     C                   PARM                    fnlry03ds
019800141024     c* verifico se passata nuova o vecchia DS
019900141024     c                   movel     kpjbu         fnlry03ds2
020000141024
020100141024     c                   monitor
020200141024     c                   if        ILRY03NSP>0
020300141024     c                   endif
020400141024     c                   on-error
020500141024     c                   clear                   ILRY03nsp
020600141024     c                   endmon
020700141024
020800141024     c                   select
020900141024     c* passata solo DS vecchi --> sposto i campi
021000141024     c                   when      %parms=2
021100141024     c                   clear                   fnlry03ds2
021200141024     c                   eval      ILRY03TLA  =ol_ILRY03TLA
021300141027     c                   if        ilry03tla<>'C'
021400141024     c                   eval      ILRY03Tch  =ol_ILRY03Tch
021500141024     c                   eval      ILRY03AAS  =ol_ILRY03AAS
021600141024     c                   eval      ILRY03lnp  =ol_ILRY03lnp
021700141024     c                   eval      ILRY03nrs  =ol_ILRY03nrs
021800141024     c                   eval      ILRY03nsp  =ol_ILRY03nsp
021900141024     c                   eval      ILRY03fgs  =ol_ILRY03fgs
022000141024     c                   eval      ILRY03prg  =ol_ILRY03prg
022100141024     c                   eval      ILRY03tipd =ol_ILRY03tipd
022200141027     c                   endif
022300141024     c                   clear                   ol_OLry03err
022400141024     c                   clear                   ol_OLry03msg
022500141024     c                   clear                   ol_olry03no1
022600141024     c                   clear                   ol_olry03no2
022700141024     c                   clear                   ol_olry03f12
022800141024     c* non passata nessuna DS --> pulisco la ds nuova e darà errore
022900141024     c                   when      %parms=1 and ILRY03nsp =0
023000141024     c                   clear                   fnlry03ds2
023100141024     c                   endsl
023200141024     c
023300070320     c*
023400140813     c                   clear                   olry03err
023500140813     c                   clear                   olry03msg
023600140813     c                   clear                   olry03f12
023700170111     c                   clear                   wmaiaff           1
023800140813     c                   if        ilry03tla='C'
023900070320     c                   goto      fine
024000070320     c                   endif
024100130827
024200070327     C                   TIME                    W0140            14 0
024300070327
024400070327      * UDATE IN AAAAMMGG
024500070327     C                   MOVE      W0140         g02dat            8 0
024600070327     C                   MOVEL     *BLANK        G02ERR
024700070327     C                   CALL      'XSRDA8'
024800070327     C                   PARM                    WLBDA8
024900070327     C                   MOVEL     G02INV        DATEU             8 0
025000070321     c*
025100070621     c     karb          chain     fnarb01l
025200170111    1c                   if        not %found(fnarb01l)
025300170111     c     karb          chain     fnblp01l
025400170111    2c                   if        not %found(fnblp01l)
025500141024     c                   eval      olry03err='1'
025600141024     c                   eval      olry03msg=msg(7)
025700141024     c                   goto      fine
025800170111   x2C                   ELSE
025900170111     c* considero bolla solo in fnblp se chiusa in partenza
026000170111    3C                   if        arbcca<>' '  and arbft2='N'
026100170111     c                   eval      wmaiaff='S'
026200170111   x3c                   else
026300170111     c* altrimenti errore
026400170111     c                   eval      olry03err='1'
026500170111     c                   eval      olry03msg=msg(7)
026600170111     c                   goto      fine
026700170111    3c                   endif
026800170111     c
026900170111    2c                   endif
027000170111    1c                   endif
027100070320     c*
027200070321     C                   EXSR      RieD01
027300140813     c                   if        olry03err<>' '
027400140813     c                   goto      fine
027500140813     c                   endif
027600141028     c* chiamata cieca
027700141028    0c                   if        ILRY03FFR='N'
027800141028     c                   exsr      Ges_F21
027900141028     c
028000141028     c* Se ci sono stati errori o premuto F12 riemetto videata altrimenti uscita
028100141028    1c                   if        not *in66 or *in90 =*on or *in91
028200141028     c
028300141028    2c                   if        vidmsg<>*blanks
028400141028     c                   eval      olry03msg=vidmsg
028500141028     c                   eval      olry03err='1'
028600141028    2c                   endif
028700141028     c
028800141028     c                   eval      olry03f12='1'
028900141028     c                   unlock    tiidc01l
029000141028   x2c                   else
029100141028     c                   exsr      conf_cieca
029200141028    2c                   endif
029300000608      *
029400141028   x0c                   else
029500141028     c* con videata
029600070315     c                   do        *hival
029700141007     c* In realtà ora è sempre abilitato
029800140813     c                   exsr      sr_AbilF17
029900141007     c
030000141007     c* Per ora sempre NON abilitato F16
030100141007     c                   exsr      sr_abilf16
030200141007     c
030300140813     c                   write     lry03t01
030400140813     C                   EXFMT     lry03d01
030500070315     c                   setoff                                       9028
030600070315     c                   clear                   vidmsg
030700070326      * F12=Ritorno
030800070326     C                   if        *INKL
030900140813     c                   eval      olry03f12='1'
031000141001     c                   unlock    tiidc01l
031100070315     C                   leave
031200070315     c                   endif
031300070315      * F7=Interrogazione bolle arrivi
031400070315     c                   if        *inkg
031500070315     c                   clear                   kpjbu
031600070315     c                   clear                   param1
031700070322     C                   MOVE      v2haas        PA1AAS
031800070322     C                   MOVE      v2clnp        PA1LNP
031900070322     C                   MOVE      v2cnrs        PA1NRS
032000070322     C                   MOVE      v2cnsp        PA1NSP
032100070322     C                   MOVE      'N'           PA1FLG
032200070322     C                   MOVE      *blanks       PA1CML
032300070327     C                   MOVEL     N_pgm         PA1PGM
032400070315     c                   movel     param1        kpjbu
032500070315     c                   call      'FNLR36R'
032600070315     c                   parm                    kpjba
032700070315     c                   iter
032800070315     c                   endif
032900070410      * F16=Gestione Richieste (R.A.)
033000070405     c                   if        *inkq
033100070410     c                   reset                   FIDNA2ds
033200070410     c                   eval      ia2ogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
033300070410     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
033400070410     c                   call      'FIDNA2R'
033500070410     c                   parm                    KPJBA
033600070410     c                   parm                    FIDNA2ds
033700070405     c                   iter
033800070405     c                   endif
033900140912      * F10=Inserimento Richieste (R.A.)
034000140912     c                   if        *inkj
034100141003     c                   exsr      Win_RA
034200141003     c**                 call      'FIDNA1R'
034300141003     c**                 parm                    KPJBA
034400140912     c                   iter
034500140912     c                   endif
034600070412      *?F17 - Note LDV
034700070412if  1c                   if        *inKR = *on
034800140130     c                   clear                   fnlrn6ds
034900140130     c                   eval      in6mod='V'
035000140130     c                   eval      in6aas=arbaas
035100140130     c                   eval      in6lnp=arblnp
035200140130     c                   eval      in6nrs=arbnrs
035300140130     c                   eval      in6nsp=arbnsp
035400140131     c                   eval      in6pgm='FNLRG2R'
035500140130     c                   call      'FNLRN6R'
035600070412     c                   parm                    kpjba
035700140130     c                   parm                    fnlrn6ds
035800070412     c                   iter
035900070412e   1c                   endif
036000141010c
036100141010      * controlli videata
036200141010     C                   EXSR      Ctrd01
036300141010     c   90              iter
036400141010c
036500141008    1c                   if        *inkv
036600141028     c                   exsr      Ges_F21
036700141013     c
036800141013     c* Se ci sono stati errori o premuto F12 riemetto videata altrimenti uscita
036900141028    2c                   if        not *in66 or *in90 =*on or *in91
037000141010     c                   iter
037100141013    2c                   endif
037200141013    1c                   endif
037300141007     c
037400141008     c* F6=Conferma --> Presa Visione
037500141007     c* solo se fatta una variazione o una nota
037600141027     c* errore forzabile; obbligatorio se dirottamento
037700141013     c                   if        *inkf and
037800141013     c                             not *in66 and wnot=*blanks
037900141008     c                             and wforzaF6=' '
038000141007     c                   seton                                        9028
038100141027     c                   if        idcfla=' '
038200141007     c                   eval      vidmsg=msg(5)
038300141008     c                   eval      wforzaf6='S'
038400141027     c                   else
038500141027     c                   eval      vidmsg=msg(8)
038600141027     c                   endif
038700141007     c                   iter
038800141007     c                   endif
038900141007     c
039000141013     c* per F6 o F21 - conferma
039100141013     c                   if        *inkf or *inkv
039200141007     c                   exsr      confe
039300070322     c   90              iter
039400141013     c  n90              leave
039500141007     c                   endif
039600011227      *
039700011227     C                   ENDDO
039800070124      *
039900141028    0c                   endif
040000141028     c
040100141024     c                   if        %parms=2
040200141024     c                   eval      ol_olry03f12 =olry03f12
040300141024     c                   eval      ol_olry03err =olry03err
040400141024     c                   eval      ol_olry03msg =olry03msg
040500141027     c                   else
040600141027     c                   movel     fnlry03ds2    kpjbu
040700141024     c                   endif
040800070320     c     fine          tag
040900140813     c                   if        ilry03tla=' '
041000070320     c                   seton                                        rt
041100070320     c                   else
041200070328     c                   exsr      chiudi
041300070315     C                   seton                                        lr
041400070320     c                   endif
041500070315     c**********************************************************************
041600070320     c* Caricamento d01
041700070315     c**********************************************************************
041800070321     C     RieD01        BEGSR
041900141010     c                   setoff                                       9071
042000141001     c                   setoff                                       666711
042100141006     c                   setoff                                       686470
042200141027     c                   setoff                                       2223
042300141028     c                   setoff                                       2890
042400141006     c                   clear                   LRY03W1
042500141006     c                   clear                   LRY03W2
042600141006     c                   clear                   LRY03W3
042700141006     c                   clear                   LRY03W4
042800141006     c                   clear                   wnot
042900141008     c                   clear                   wforzaF6
043000141013     c                   clear                   forzacell
043100141013     c                   clear                   forzamail
043200141028     c                   clear                   vidmsg
043300140917     c* pos. cusr su causale variazione bolle
043400140917     c                   seton                                        46
043500141006     c* Accendo indicatore per la int bolle
043600141006     c                   seton                                        63
043700141006      /free
043800141006         XRTVSTKR (pStack:pStartPoint:pEntries);
043900141006         dsStack = pStack;
044000141006         IF  %lookup('FNLR36R   ':skStack) > 0;
044100141006          *in63=*off ;
044200141006         endif      ;
044300141006      /end-free
044400141027     c
044500070321     c* CAMPI DI OUTPUT
044600070320     c                   z-add     arblnp        v2clnp
044700070320     c                   z-add     arbnrs        v2cnrs
044800070320     c                   z-add     arbnsp        v2cnsp
044900070320     c                   movel     arbaas        wdata             8 0
045000070320     c                   move      arbmgs        wdata
045100070320     c     *iso          move      wdata         dataeur
045200070320     c                   move      dataeur       v2cdsp
045300070322     c                   move      arbaas        v2haas
045400141001     c**                 movel     arbcbo        v2ccbo
045500070320     c                   clear                   v2dcbo
045600070320     c                   move      '3A'          tblcod
045700070320     c                   movel(p)  arbcbo        tblkey
045800070320     c                   exsr      ctabel
045900070320     c                   movel     tbluni        ds3a
046000141001     c                   eval      v2dcbo=arbcbo+'-'+§3ades
046100070320     c* tipo spedizione
046200070320     c                   clear                   v2dtsp
046300070320     c                   move      '5E'          tblcod
046400070320     c                   movel(p)  arbtsp        tblkey
046500070320     c                   exsr      ctabel
046600070320     c                   movel     tbluni        ds5e
046700090604     c                   movel     §5ed08        v2dtsp
046800070320     c* mittente
046900070320     c                   clear                   v2cmit
047000070320     c                   if        %subst(§3atb1:1:1)='F'
047100070320     c                   move      arbksc        v2cmit
047200070320     c                   else
047300070320     c                   move      arbccm        wccm4             4 0
047400070320     c                   if        wccm4<>9999
047500070320     c                   move      arbccm        v2cmit
047600070320     c                   endif
047700070320     c                   endif
047800070320     c* ragione sociale mittente
047900070320     c                   movel(P)  arbrsm        v2crsm
048000070320     c* ragione sociale mittente originale
048100070320     c                   movel(P)  arbrmo        v2crmo
048200070320     c* destinatario
048300070320     c                   movel(P)  arbrsd        v2crsd
048400140910     c                   clear                   wrsd2
048500070320     c* estensione ragione sociale destinatario da fiar4 record "D"
048600070320     C                   MOVEL     'D'           WTRC
048700070328     C     KAR4          CHAIN     Fiar401l
048800070320     C                   IF        %found(fiar401l)
048900070320     C                   MOVE      AR4NOT        v2crsd
049000140910     C                   MOVE      AR4NOT        wrsd2
049100070320     C                   ENDIF
049200070321     c                   movel     arbind        v2cind
049300070321     c                   movel     arbcad        v2ccad
049400070321     c                   movel     arblod        v2clod
049500070321     c                   eval      v2clod=%trim(v2clod)+ '  ' + arbprd
049600070321     c* referente consegna
049700070321     c                   Clear                   DAr5Gen
049800070321     c                   Eval      kAr5Trd = 'GEN'
049900091217     c     KFiar5        Chain(n)  Fiar501l
050000070321     c                   If        %Found(Fiar501l)
050100070321     c                   Movel     Ar5Uni        DAr5Gen
050200070321     c                   EndIf
050300070327    2c                   if        %subst(§ar5ref:1:1) = '#'
050400140910     c                   eval      v2crefb=%subst(§ar5ref:2:34)
050500070327     c                   else
050600140910     c                   Eval      V2cRefb = §Ar5Ref
050700070327     c                   endif
050800140910     c                   eval      v2cref=v2crefb
050900070327    2c                   if        %subst(§ar5teld:1:1) = '#'
051000140910     c                   eval      v2ctelb=%subst(§ar5teld:2:15)
051100070327     c                   else
051200140910     c                   Eval      V2cTelb = §Ar5Teld
051300070327     c                   endif
051400140910     c                   eval      v2ctel=v2ctelb
051500140813     c* memorizzo anche eventuale codice intestazione assegno
051600140813     c                   eval      s_ar5cdi=§ar5cdi
051700140813     c* Mail e cell per alert
051800140813     c                   Clear                   DAr5emd
051900140813     c                   Eval      kAr5Trd = 'EMD'
052000140813     c     KFiar5        Chain(n)  Fiar501l
052100140813     c                   If        %Found(Fiar501l)
052200140813     c                   Movel     Ar5Uni        DAr5emd
052300140813     c                   EndIf
052400140910     c                   eval      v2cmailb=§ar5eml
052500140910     c                   eval      v2cmail =v2cmailb
052600140813    2c                   if        %subst(§ar5tel:1:1) = '#'
052700140910     c                   eval      v2ccellb=%subst(§ar5tel:2:15)
052800140813     c                   else
052900140910     c                   Eval      V2ccellb = §Ar5Tel
053000140813     c                   endif
053100140910     c                   eval      v2ccell =v2ccellb
053200140813     c
053300070321     c* consegna richiesta
053400070321     c                   clear                   fnlv80ds
053500070321     c                   eval      ilv80aas=arbaas
053600070321     c                   eval      ilv80lnp=arblnp
053700070321     c                   eval      ilv80nrs=arbnrs
053800070321     c                   eval      ilv80nsp=arbnsp
053900070321     c                   eval      ilv80mgs=arbmgs
054000070321     c                   eval      ilv80tcr=arbtcr
054100070321     c                   eval      ilv80dcr=arbdcr
054200070321     c                   eval      ilv80hcr=arbhcr
054300070321     c                   eval      ilv80tc1=arbtc1
054400070321     c                   eval      ilv80tc2=arbtc2
054500070321     c                   call      'FNLV80R'
054600070321     c                   parm                    kpjba
054700070321     c                   parm                    fnlv80ds
054800070329     c                   parm      'S'           ILV80NWPROC
054900070321     c                   eval      v2ctcro=olv80tcrd
055000070330     c                   if        arbdcr>0
055100070329     c     *iso          move      arbdcr        dataeur
055200070329     c                   move      dataeur       v2cdcro
055300070330     c                   else
055400070330     c                   clear                   v2cdcro
055500070330     c                   endif
055600070321     c                   eval      v2chcro=arbhcr
055700070321     c* colli e peso bollettato
055800070321     c                   z-add     arbncl        v2cncl
055900070321     c                   z-add     arbpkb        v2cpkb
056000070326     c* altre gestioni
056100070321     c                   clear                   v2dges
056200070321     c                   clear                   v2cges
056300140130     c                   clear                   fnlrg7ds
056400170111     C* SE LA SPEDIZIONE è solo in blp chiusa permerce mai affidata, non richiamo il pgm
056500170111    1c                   if        wmaiaff=' '
056600140130     c                   z-add     v2haas        ILRG7AAS
056700140130     c                   z-add     v2clnp        ILRG7LNP
056800140130     c                   z-add     v2cnrs        ILRG7NRS
056900140130     c                   z-add     v2cnsp        ILRG7NSP
057000140130     c                   call      'FNLRG7R'
057100140130     c                   parm                    fnlrg7ds
057200140130     c                   movel     OLRG7VSSIN    v2dges
057300170111     c                   else
057400170111     c* sped chiusa 7- mai affidata
057500170111     c                   eval      v2DGES =RED +'Spedizione chiusa in ' +
057600170111     c                             'partenza per MERCE MAI AFFIDATA'
057700170111     C                   ENDIF
057800170111
057900140409     c* visualizzo se tutti i colli arrivati alla fil di arrivo
058000140409     c                   clear                   fnlrj6ds
058100170111     c                   if        wmaiaff='S'
058200170111     c                   eval      iLRJ6TRIC = 'P'
058300170111     c                   else
058400170111     c                   eval      iLRJ6TRIC = 'A'
058500170111     c                   endif
058600140409     c                   eval      iLRJ6AAS  = v2haas
058700140409     c                   eval      iLRJ6LNP  = v2cLNP
058800140409     c                   eval      iLRJ6NRS  = v2cNRS
058900140409     c                   eval      iLRJ6NSP  = v2cNSP
059000140409     c                   call      'FNLRJ6R'
059100140409     c                   parm                    fnlrj6ds
059200140409     c
059300140409     c                   clear                   v2carr
059400140409     c                   if        OLRJ6FALL='S'
059500140409     c                   eval      v2carr='TUTTI'
059600140409     c                   else
059700140409     c                   eval      wncl=OLRJ6CDCM+OLRJ6CSPU
059800140409     c                   eval      v2carr=%editc(wncl:'Z')
059900140409     c                   if        v2carr=*blanks
060000140409     c                   move      '0'           v2carr
060100140409     c                   endif
060200140409     c                   endif
060300170111     c
060400070702     c*
060500070702     c* causale per variazione bolla
060600070702     c                   clear                   v2ccvb
060700130515     c* particolarità
060800130515     c                   setoff                                       444547
060900130515     c                   clear                   v2igga
061000130515     c                   clear                   v2igma
061100130515     c                   clear                   v2igva
061200130515     c                   eval      v2cgga=arbgga
061300130515     c                   if        v2cgga<>*blanks
061400130515     c                   seton                                        44
061500130515     c                   endif
061600130515     c                   eval      v2cgma=arbgma
061700130515     c                   if        v2cgma<>*blanks
061800130515     c                   seton                                        45
061900130515     c                   endif
062000130515     c                   eval      v2cgva=arbgva
062100130515     c                   if        v2cgva<>*blanks
062200130515     c                   seton                                        47
062300130515     c                   endif
062400080909     c* SI DDT
062500080909     c                   setoff                                       14
062600080909     c                   movel     'A'           wtrc
062700080909     c     kar4          chain     fiar401l
062800080909     c                   if        %found(fiar401l)
062900080909     c                   movel     ar4not        dsbl4a
063000080909     c                   if        §b4abm='S' or §b4abm='C' or
063100080909     c                             §b4abm='P' or §b4abm='Q'
063200080909     c                   seton                                        14
063300080909     c                   endif
063400080909     c                   endif
063500070321     c* flag fermo deposito
063600070321     c                   if        arbffd<>*blanks
063700070321     c                   seton                                        10
063800070321     c                   else
063900070321     c                   setoff                                       10
064000070321     c                   endif
064100140813     c* abilitazione tasto funzionale F17-note aut
064200140813     c                   exsr      sr_AbilF17
064300140813     c
064400140813     c* Lettura record di disposizione di consegna da gestire
064500140909     c     kidc          chain     tiidc01l
064600140909     c                   if        not %found(tiidc01l)
064700140813     c                   eval      olry03err='1'
064800140813     c                   eval      olry03msg=msg(1)
064900140813     c                   leavesr
065000140813     c                   endif
065100141001     c
065200141001     c* Se non è ancora stata elaborata --> errore
065300141001     c                   if        idcelaflg<>'E'
065400141001     c                   eval      olry03err='1'
065500141001     c                   eval      olry03msg=msg(4)
065600141001     c                   leavesr
065700141001     c                   endif
065800141001     c
065900140911     c* richiamo chkistruco per controlli di eseguibilità della variazione richiesta
066000141008     c**                 exsr      sr_chkistruco
066100141008     c**                 if        werrchk='1'
066200141008     c**                 eval      idcerr='1'
066300141008     c**                 eval      IDCLOG=%subst(textmsg:1:200)
066400141008     c**                 endif
066500140813     c
066600141001     c                   clear                   v2clog
066700141001     c                   clear                   v2clog1
066800141001     c                   if        idclog<>*blanks
066900141001     c                   seton                                        11
067000141006     c                   eval      v2clog=%subst(idclog:1:65)+'  '
067100141006     c                   eval      v2clog1='  '+%subst(idclog:66:74)+'  '
067200141006     c                   endif
067300141001     c
067400140813     c                   if        idcref<>*blanks
067500140813     c                   eval      v2cref=idcref
067600140813     c                   endif
067700140813     c                   if        idcteld<>*blanks
067800140813     c                   eval      v2ctel =idcteld
067900140813     c                   endif
068000140813     c                   if        idctelavv<>*blanks
068100140813     c                   eval      v2ccell=idctelavv
068200140813     c                   endif
068300140813     c                   if        idcmailavv<>*blanks
068400140813     c                   eval      v2cmail=idcmailavv
068500140813     c                   endif
068600140813     c* data e ora variazione
068700140813     c                   movel     idcinsdata    w0080             8 0
068800140813     c                   move      idcinsdata    w0060             6 0
068900140813     c                   movel     3             g02err
069000140813     C                   Z-ADD     w0080         G02inv
069100140813     C                   MOVEL     *zeros        G02dat
069200140813     C                   CALL      'XSRDA8'
069300140813     C                   PARM                    WLBDA8
069400141001     c                   eval      v2dvar=' D I S P O S I Z I O N I  dal'
069500140814     c                   if        idctipocli='M'
069600141001     c                   eval      v2dvar=%trimr(v2dvar)+ '  M i t t e n t e'
069700140814     c                   else
069800141001     c                   eval      v2dvar=%trimr(v2dvar)+ '  D e s t i n a t.'
069900140814     c                   endif
070000141001     c                   eval      v2dvar= %trimr(v2dvar) + '  il  '+
070100140814     c                               %editw(g02dat: '  /  /    ')+
070200140814     c                             '  '  +
070300141001     c                             %editw(w0060: '  :  :  ')
070400141001     c***                          ' ------------------------------------------'
070500140813     c
070600140813     c
070700070321     c* Sottotitolo videata + situazione contatto in corso
070800070321     c                   clear                   v2ctit2
070900140813     c                   clear                   v2cidc1
071000140813     c                   clear                   v2cidc2
071100140813     c                   clear                   v2cidc3
071200141006     c                   move      ' '           v2cidc3
071300141006     c                   movel     ' '           v2cidc3
071400070402     c                   clear                   wblk
071500151112     c                   clear                   v2hfla
071600141010     c*
071700141010     c                   seton                                        70
071800141010     c                   eval      v1cf21='F21=Applica Variazioni'
071900141010     c
072000070321     c                   select
072100140813     c* Tipo "4" --> disposizioni come note
072200140813     c                   when      ilry03tipd='4'
072300140813     c                   eval      v2ctit2=cosaltro
072400140813     c                   move      '4'           wblk
072500141010     c
072600141010     c* non devo accendere F21: non c'è nulla da applicare in automatico
072700141010     c                   setoff                                       70
072800141010     c
072900140813     c* Imposto i dati nella videata
073000141001     c                   eval      v2cidc1=%subst(idcnote:1:72)
073100141001     c                   eval      v2cidc2=%subst(idcnote:73:72)
073200141006     c                   eval      v2cidc3='   '+%subst(idcnote:145:56)
073300141006     c                   move      ' '           v2cidc3
073400140813     c
073500140813     c* Tipo "1" --> Appuntamento
073600140813     c                   when      ilry03tipd='1'
073700070321     c                   eval      v2ctit2=cosapp
073800140813     c                   move      '1'           wblk
073900140813     c* Imposto i dati richiesti
074000140813     c                   movel     idcdcr        w0080             8 0
074100140813     c                   movel     3             g02err
074200140813     C                   Z-ADD     w0080         G02inv
074300140813     C                   MOVEL     *zeros        G02dat
074400140813     C                   CALL      'XSRDA8'
074500140813     C                   PARM                    WLBDA8
074600140813     c                   eval      v2CIDC1='CONSEGNA RICHIESTA: IL'
074700140813     c                   eval      v2Cidc1=%TRIM(V2CIDC1) +'  '+
074800140813     c                                           %editw(g02dat:'  /  /    ')
074900140813     c                   if        idchcr>0
075000140813     c                   eval      v2cidc1=%trim(v2cidc1)+'  '+
075100140813     c                                    %editw(idchcr: '  :  ')
075200140813     c                   endif
075300140813     c
075400140813     C                   IF        idcprcr='M'
075500141008     c                   eval      v2cidc2= 'Preferibilmente il MATTINO   '
075600140813     c                   endif
075700140813     C                   IF        idcprcr='P'
075800141008     c                   eval      v2cidc2= 'Preferibilmente il POMERIGGIO'
075900140813     c                   endif
076000140813     c
076100140813     c* Tipo "2" --> fermo deposito
076200140813     c                   when      ilry03tipd='2'
076300070321     c                   eval      v2ctit2=cosffd
076400140813     c                   move      '2'           wblk
076500140813     c                   eval      v2cidc1= 'Fermo Deposito'
076600151112
076700151112     c* Tipo "5" --> RESO
076800151112     c                   when      ilry03tipd='5'
076900151112     c                   seton                                        7071
077000151112     c                   eval      v1cf21='F21=Reso/Dirott.'
077100151112     c                   eval      v2hfla=idcfla
077200151112     c                   eval      v2cidc2='R  E  S  O  '
077300141001     c
077400140813     c* Tipo "3" --> cambio indirizzo
077500140813     c                   when      ilry03tipd='3'
077600140813     c                   eval      v2ctit2=cosind
077700140813     c                   move      '3'           wblk
077800140813     c                   eval      v2cidc1= idcrsd
077900141002
078000141002     c                   clear                   v2cidc2
078100141002     c                   clear                   v2cidc3
078200141002     c* senza cons richiesta
078300141002     c                   if        idcdcr=0
078400141002     c* Bolla da dirottare
078500151112     c                   if        idcfla<>' '
078600141010     c                   seton                                        7071
078700151112     c                   eval      v1cf21='F21=Reso/Dirott.'
078800151112     c                   eval      v2hfla=idcfla
078900141008     c
079000141002     c                   eval      v2cidc2= %trim(idcIND)+' '+%trim(idccad)+
079100141002     c                             ' ' +idcprd+' '+idclod
079200141007     c                   eval      v2CIDC3='   D I R O T T A R E  alla  +
079300141007     c                               F i l i a l e  : ' + %editc(idcnewlna:'X')
079400151112     c
079500141002     c                   else
079600141002     c                   eval      v2cidc2= idcIND
079700141002
079800141006     c                   eval      v2cidc3= '   '+idccad
079900140911     c                   eval      v2cidc3= %trim(v2cidc3) + ' ' + idclod
080000140813     c                   if        %subst(v2cidc3:67:4)<>*blanks
080100140813     c                   eval      %subst(v2cidc3:67:4)='  '+idcprd
080200140813     c                   else
080300140813     c                   eval      v2cidc3=%trim(v2cidc3)+ '  '+idcprd
080400140813     c                   endif
080500141002     c                   endif
080600141002     c
080700141006     c                   move      ' '           v2cidc3
080800141002     c                   else
080900141002     c* con cons richiesta i dati devono essere + stretti
081000141003     c                   movel     idcdcr        w0080             8 0
081100141003     c                   movel     3             g02err
081200141003     C                   Z-ADD     w0080         G02inv
081300141003     C                   MOVEL     *zeros        G02dat
081400141003     C                   CALL      'XSRDA8'
081500141003     C                   PARM                    WLBDA8
081600141003     c
081700141002     c                   eval      v2cidc2= %trim(idcIND)+' '+%trim(idccad)+
081800141002     c                             ' ' +idcprd+' '+idclod
081900141006     c                   eval      v2CIDC3='   Consegna Richiesta IL'
082000141002     c                   eval      v2Cidc3=%TRIM(V2CIDC3) +'  '+
082100141002     c                                           %editw(g02dat:'  /  /    ')
082200141002     c                   if        idchcr>0
082300141002     c                   eval      v2cidc3=%trim(v2cidc3)+' '+
082400141002     c                                    %editw(idchcr: '  :  ')
082500141002     c                   endif
082600141002     C                   IF        idcprcr='M'
082700141003     c                   eval      v2cidc3= %trimr(v2cidc3)+
082800141003     c                                 ' Pref.Consegna MATTINO'
082900141002     c                   endif
083000141002     C                   IF        idcprcr='P'
083100141003     c                   eval      v2cidc3= %trimr(v2cidc3)+
083200141003     c                                      ' Pref.Consegna POMERIGGIO'
083300141002     c                   endif
083400141006     c                   move      ' '           v2cidc3
083500141002     c
083600141002     c                   endif
083700140813     c
083800070321     c                   endsl
083900140813     c
084000140813     c                   if        wblk=*blanks
084100140813     c                   move      *hival        wblk
084200140813     c                   endif
084300070321
084400070503     c* giro di consegna
084500140922     c***                clear                   v2ccgi
084600140922     c***                clear                   v2dcgi
084700140922     c***                clear                   fidg34ds
084800140922     c***                eval      id34tip='2'
084900140922     c***                eval      id34aas=arbaas
085000140922     c***                eval      id34lnp=arblnp
085100140922     c***                eval      id34nrs=arbnrs
085200140922     c***                eval      id34nsp=arbnsp
085300140922     c***                call      'FIDG34R'
085400140922     c***                parm                    FIDG34DS
085500140922     c***                movel     od34cgi       v2ccgi
085600140922     c***                eval      v2dcgi=od34cgides
085700070321     c*
085800070321     c* note partenza
085900070321     c                   clear                   v2cnot8
086000070321     c                   clear                   v2cnot9
086100070322     c                   clear                   v2hnot8
086200070322     c                   clear                   v2hnot9
086300070321     C                   MOVEL     '8'           WTRC
086400070328     C     Kar4          CHAIN     Fiar401l
086500070321     C                   if        %found(fiar401l)
086600070321     C                   MOVEL     ar4NOT        V2cnot8
086700070322     C                   MOVEL     ar4NOT        V2hnot8
086800070321     C                   ENDIF
086900070321     C*
087000070321     C                   MOVEL     '9'           WTRC
087100070328     C     Kar4          CHAIN     Fiar401l
087200070321     C                   if        %found(fiar401l)
087300070321     C                   MOVEL     ar4NOT        V2cnot9
087400070322     C                   MOVEL     ar4NOT        V2hnot9
087500070321     C                   ENDIF
087600140813     c
087700070321     C                   MOVEL     *BLANKS       V2cTIC
087800070321     C                   MOVEL     *ZERO         V2cCAS
087900070321     C                   MOVEL     *BLANKS       V2cVCA
088000091127     c                   setoff                                       15
088100070321     C     §3AFCA        IFGT      *ZERO
088200070322     C     Karb          CHAIN(N)  FiAR901L
088300070321     C                   if        %found(fiar901l)
088400070321     C                   MOVEL     AR9TIC        V2cTIC
088500070321     C                   MOVEL     AR9CAS        V2cCAS
088600070321     C                   MOVEL     AR9VCA        V2cVCA
088700070321     C                   ENDif
088800091216     c* richiamo trul per determinare l'intestatario del c/a
088900141001     c**                 if        dateu>=§VPOCAS
089000141001     c**                 clear                   trulintds
089100141001     c**                 eval      iinttic=v2ctic
089200141001     c**                 eval      iinttip='I'
089300141001     c**                 eval      IINTRSCM=arbrsm
089400141001     c**                 eval      IINTRSCO=arbrmo
089500141001     c**                 eval      IINTcdi=s_ar5cdi
089600141001     c**                 call      'TRULINTR'
089700141001     c**                 parm                    trulintds
089800141001     c**                 if        ointerr=*blanks
089900141001     c**                 seton                                        15
090000141001     c**                 eval      v2ccdi=ointrsci
090100141001     c**                 endif
090200141001     c**                 endif
090300141001     C                   ENDIF
090400140922
090500140922     c* Se in distinta visualizzo
090600140922     c                   if        arbndc>0 and arbddc>0 and
090700140922     c                             arbdcm=0
090800140922     c                   eval      *in67=*on
090900140922     c                   endif
091000141027     c* Tasti funzionali da abilitare
091100141027     c* DIROTTAMENTO
091200141027     c                   if        *in71
091300141027     C                   if        ILRY03DF12='S' or ILRY03DF12=' '
091400141027     c                   seton                                        22
091500141027     c                   endif
091600141027     C                   if        ILRY03DF06='S'
091700141027     c                   seton                                        23
091800141027     c                   endif
091900141028     C                   if        ILRY03DF21='N' or ILRY03DF21=' '
092000170111     C                             OR WMAIAFF='S'
092100141027     c                   setoff                                       70
092200141027     c                   endif
092300141027     c
092400141027     c                   else
092500141027     C                   if        ILRY03eF12='S' or ILRY03eF12=' '
092600141027     c                   seton                                        22
092700141027     c                   endif
092800141027     C                   if        ILRY03eF06='S' or ILRY03eF06=' '
092900141027     c                   seton                                        23
093000141027     c                   endif
093100141028     C                   if        ILRY03eF21='N'
093200141027     c                   setoff                                       70
093300141027     c                   endif
093400141027     c                   endif
093500141027     c* passate le stampanti: non devo richiederle...
093600141027     c                   if        ilry03pss<>*blanks and ilry03mdb4<>*blanks
093700141027     c                   eval      wrichtrul90='S'
093800141027     c                   endif
093900070321     c*
094000070321     c
094100070315     C                   ENDSR
094200070321     c**********************************************************************
094300140813     c* Verifica abilitazione tasto F17-note aut
094400070321     c**********************************************************************
094500140813     C     SR_AbilF17    BEGSR
094600070412     c                   setoff                                       65
094700070321     c     karn          setll     fiarn11l
094800070321     c                   if        %equal(fiarn11l)
094900070412     c                   seton                                        65
095000070321     c                   endif
095100070321     c                   endsr
095200070315     c**********************************************************************
095300070315     c* Controlli video D01.
095400070315     c**********************************************************************
095500070315     C     Ctrd01        BEGSR
095600070315      *
095700070329     c                   setoff                                       404142
095800141001     c                   setoff                                       43
095900070329     c* ? su altre gestioni
096000070329     c                   if        v2cges='?'
096100140130     c                   clear                   fnlrg7ds
096200140813     c                   eval      ilrg7aas=ilry03aas
096300140813     c                   eval      ilrg7lnp=ilry03lnp
096400140813     c                   eval      ilrg7nrs=ilry03nrs
096500140813     c                   eval      ilrg7nsp=ilry03nsp
096600140130     c                   eval      ilrg7vsdet='?'
096700140130     c                   call      'FNLRG7R'
096800140130     c                   parm                    fnlrg7ds
096900070329     c                   clear                   v2cges
097000070329     c                   seton                                        90
097100070329     c                   goto      endct8
097200070329     c                   endif
097300130515     c* ? su particolarità
097400130515     c                   if        v2igga='?'
097500130515     C                   CLEAR                   DS7PQRS
097600130515     C                   MOVEL     'S'           DS7RIC
097700130515     C                   MOVEL     'V'           DS7GES
097800130515     C                   MOVEL     V2CGGA        DS7COD
097900130515     C                   MOVEL     DS7PQRS       KPJBU
098000130515     C                   CALL      'TRTB69R'
098100130515     C                   PARM                    KPJBA
098200130515     c                   clear                   v2igga
098300130515     c                   seton                                        90
098400130515     c                   goto      endct8
098500130515     c                   endif
098600130515
098700130515     c                   if        v2igma='?'
098800130515     C                   CLEAR                   DS7PQRS
098900130515     C                   MOVEL     'S'           DS7RIC
099000130515     C                   MOVEL     'V'           DS7GES
099100130515     C                   MOVEL     V2cgma        DS7COD
099200130515     C                   MOVEL     DS7PQRS       KPJBU
099300130515     C                   CALL      'TRTB70R'
099400130515     C                   PARM                    KPJBA
099500130515     c                   clear                   v2igma
099600130515     c                   seton                                        90
099700130515     c                   goto      endct8
099800130515     c                   endif
099900130515
100000130515     c                   if        v2igva='?'
100100130515     C                   CLEAR                   DS7PQRS
100200130515     C                   MOVEL     'S'           DS7RIC
100300130515     C                   MOVEL     'V'           DS7GES
100400130515     C                   MOVEL     V2cgva        DS7COD
100500130515     C                   MOVEL     DS7PQRS       KPJBU
100600130515     C                   CALL      'TRTB73R'
100700141013     C                   PARM                    kpJBA
100800130515     c                   clear                   v2igva
100900130515     c                   seton                                        90
101000130515     c                   goto      endct8
101100130515     c                   endif
101200070703     c* Causale per variazione bolla
101300070703    1C     v2ccvb        IFNE      *BLANKS
101400070703     C*
101500070703     C     '?'           SCAN      v2ccvb                                 80
101600070703     C* . . . Ricerca
101700070703    2C     *IN80         IFEQ      *ON
101800070703     c                   clear                   fnlr66ds
101900140813     C                   MOVEL     ilry03nsp     D66NSP
102000140813     C                   MOVEL     ilry03aas     D66AAS
102100140813     C                   MOVEL     ilry03lnp     D66LNP
102200140813     C                   MOVEL     ilry03nrs     D66NRS
102300070703     C                   MOVEL     'V'           D66GES
102400070703     C                   MOVEL     'A'           D66TBO
102500070703     C*
102600070703     C                   MOVEL     fnlr66ds      KPJBU
102700070703     C                   CALL      'FNLR66R'
102800070703     C                   PARM                    KPJBA
102900070703     C                   MOVEL     KPJBU         fnlr66ds
103000070703     C                   MOVEL     D66CVB        V2ccvb
103100070703     c                   seton                                        90
103200070703     C                   GOTO      ENDCT8
103300070703   X2C                   ELSE
103400070703     C* . . . Controllo
103500070703     C                   CLEAR                   fnlr66ds
103600070703     C                   MOVEL     'C'           D66GES
103700070703     C                   MOVEL     'A'           D66TBO
103800070703     C                   MOVEL     V2ccvb        D66CVB
103900070703     C*
104000070703     C                   MOVEL     fnlr66ds      KPJBU
104100070703     C                   CALL      'FNLR66R'
104200070703     C                   PARM                    KPJBA
104300070703     C                   MOVEL     KPJBU         fnlr66ds
104400070703     C                   MOVEL     D66CVB        V2ccvb
104500070703     C*
104600070703     C* . . . Errore
104700070703    3C     D66MSG        IFNE      *BLANKS
104800070703     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
104900070703     C                   SETON                                        284390
105000070703     C                   GOTO      ENDCT8
105100070703    3C                   ENDIF
105200070703    2C                   ENDIF
105300070703     C*
105400070703     C* Richiamo pgm di variazione se impostato il codice
105500070703     C*
105600141010     c                   exsr      Pul_lr48
105700141014     C                   MOVEL     V2ccvb        D48CVB
105800140910      /free
105900140910        select;
106000140910       // Appuntamento
106100151112        when (IDCTIPODIS='1' or (IDCTIPODIS='3' and idcfla=' ')) and
106200141204                                             %subst(d48cvb:1:1)='C' ;
106300141002        // non forzo se supermercato perchè bisognerebbe usare CS
106400141001           if arbtc1<>'S' and arbtc2<> 'S'   ;
106500140910           d48cvb='CI';
106600141001           endif  ;
106700141010           exsr  Car_ARBG  ;
106800141010
106900140910       // Fermo Deposito o Altro Indirizzo
107000141002        when (IDCTIPODIS='2' or IDCTIPODIS='3') and d48cvb='I0' ;
107100140910           d48trc='D';
107200141010           if idctipodis='2'
107300141010             and idctipocli='M';
107400140910              d48cvb='I7';
107500140911            endif;
107600141010            exsr Car_ARBD  ;
107700140910        endsl;
107800141010            exsr Call_LR48 ;
107900140910      /end-free
108000141010    2c                   if        d48err= *blanks
108100140911     c                   clear                   v2ccvb
108200141010     c                   endif
108300141010     c
108400070709     c                   goto      endct8
108500140911     C*
108600070703     c                   endif
108700141010     c
108800140911     c* Controllo campi input/output:
108900141013     c* --> Cellulare se errore lo clearo
109000140911      /free
109100141013       if v2cCELL <>*blanks and forzacell=*blanks ;
109200140911       pInCell = %trim(v2ccell);
109300140911       pOutErr = *blanks;
109400140911       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
109500140911
109600140911           // Numero di cellulare KO
109700140911          *in90=*on;
109800140911          *in28=*on;
109900140911          *in40=*on;
110000140911          vidmsg=msg(2);
110100141013          forzacell='S'  ;
110200140911          leavesr    ;
110300140911       endif;
110400140911       endif;
110500140911      /end-free
110600140911     c* indirizzo email
110700141028     c                   if        v2cmail<>*blanks and forzamail=*blanks
110800140911     c                   Clear                   dsemail
110900140911     c                   Eval      §emlindi = v2cmail
111000140911     c                   Call      'XEMAIL'
111100140911     c                   Parm                    dsemail
111200140911      * Errore
111300140911if  3c                   If        §emlerro <> '0'
111400140911     c                   Eval      *In90 = *On
111500140911     C                   EVAL      *in28=*on
111600140911     C                   EVAL      *in41=*on
111700140911     c                   Eval      vidmsg = msg(3)
111800141013     c                   eval      forzamail='S'
111900140911     c                   goto      endct8
112000140911   x3c                   else
112100140911     c                   Eval      v2cmail = §emlindo
112200140911    3c                   EndIf
112300140911     c                   endif
112400070315      *
112500070329     C     endct8        ENDSR
112600141028     c**********************************************************************
112700141028     c* Gestione tasto funzionale F21
112800141028     c**********************************************************************
112900141028     c     Ges_f21       BEGSR
113000151112     c* F21 - Dirottamento/reos
113100141028   1ac                   if        *in71
113200141028     c*
113300141028    2c                   if        wrichtrul90=' '
113400141028     c* La prima volta devo proporre la richiesta stampanti
113500141028     c                   exsr      RICHSTAMPAN
113600141028     c
113700141028     c* Se premuto F3 esco senza fare nulla
113800141028    3c                   if        d90f3<>'1' and not *in91
113900141028     c                   eval      wrichtrul90='S'
114000141028    3c                   endif
114100141028     c
114200141028    3c                   if        *in91
114300141028     C                   MOVEL     MSG(6)        VidMSG
114400141028     C                   SETON                                        2890
114500141028     c*****              iter
114600141028    3c                   endif
114700141028    2c                   endif
114800141028     c
114900141028    2c                   if        wrichtrul90='S'
115000141028     c                   clear                   fnlg82ds
115100141028     c                   clear                   fnlg20ds
115200141028     c                   eval      d82gia='N'
115300141028     c                   eval      d82tri='C'
115400151112     c
115500141028     c                   movel     arbaas        d82aas
115600141028     c                   movel     arblnp        d82lnp
115700141028     c                   movel     arbnrs        d82nrs
115800141028     c                   movel     arbnsp        d82nsp
115900141028     c                   if        ilry03pss<>*blanks
116000141028     c                   eval      D82MDB4=ilry03mdb4
116100141028     c                   eval      D82MDB5=ilry03mdb5
116200141028     c                   eval      D82pss =ilry03pss
116300141028     c                   eval      D82psl =ilry03psl
116400141028     c                   eval      D82bsl =ilry03bsl
116500141028     c                   eval      D82PRB4=ilry03prb4
116600141028     c                   eval      D82PRB5=ilry03prb5
116700141028     c                   eval      D82moe =ilry03mde
116800141028     c                   eval      D82pre =ilry03pre
116900141028     c                   eval      D82mob =ilry03mdb
117000141028     c                   eval      D82prb =ilry03prb
117100141028     c                   else
117200141028     c                   eval      D82MDB4=d90mdb4
117300141028     c                   eval      D82MDB5=d90mdb5
117400141028     c                   eval      D82pss =d90pss
117500141028     c                   eval      D82psl =d90psl
117600141028     c                   eval      D82bsl =d90bsl
117700141028     c                   eval      D82PRB4=d90prb4
117800141028     c                   eval      D82PRB5=d90prb5
117900141028     c                   eval      D82moe =d90mde
118000141028     c                   eval      D82pre =d90pre
118100141028     c                   eval      D82mob =d90mdb
118200141028     c                   eval      D82prb =d90prb
118300141028     c                   endif
118400141028     c
118500141028     c                   exsr      REP_fgs
118600141028     c                   movel     wfgs          d82fgs
118700151112     c                   if        v2hfla='S'
118800151112     c                   eval      d82dis='D'
118900151112     c                   else
119000151112     c                   eval      d82dis='R'
119100151112     c                   endif
119200141028     c                   movel     fnlg82ds      kpjbu
119300141028     c* indirizzo dirottato
119400141028     c                   eval      d20rsd=idcrsd
119500141028     c                   eval      d20rs2=%subst(idcrsd:36:35)
119600141028     c                   eval      d20ind=idcind
119700141028     c                   eval      d20lod=idclod
119800141028     c                   eval      d20cad=idccad
119900141028     c                   eval      d20prd=idcprd
120000141028     c                   eval      d20nzd=idcnaz
120100141028     c                   eval      d20ref=idcref
120200141028     c                   eval      d20tel=idcteld
120300141028     c                   eval      d20lna=idcnewlna
120400141028     c                   eval      d20snp='N'
120500141028     c                   if        §3afca<>0
120600141028     c                   eval      d20snp='S'
120700141028     c
120800141028     c                   if        §3afca=1 and %subst(§3atb1:1:1)='F'
120900141028     c                   eval      d20mdp='M'
121000141028     c                   else
121100141028     c                   eval      d20mdp='D'
121200141028     c                   endif
121300141028     c                   endif
121400141028     c
121500141028     c                   call      'FNLG20R'
121600141028     c                   parm                    kpjba
121700141028     c                   parm                    fnlg20ds
121800141028     c* se restituito errore --> lo emetto
121900141028     c                   if        d20erro= 'E'
122000141028     c                   seton                                        2890
122100141028     c                   eval      vidmsg=d20errmo
122200141028     c******             iter
122300141028     c                   endif
122400141028     c
122500141028     c* non è stato premuto F12 --> quindi creata bolla
122600141028     c                   if        d20erro= ' '
122700141028     c                   seton                                        66
122800141028     c                   endif
122900141028     c
123000141028    2c                   endif
123100141028     c                   else
123200141028     c* F21-Applica variazioni
123300141028     c                   exsr      Applica
123400141028   1ac                   endif
123500141028     c                   ENDSR
123600141028     c**********************************************************************
123700141028     c* conferma cieca
123800141028     c**********************************************************************
123900141028     c     conf_cieca    BEGSR
124000141028     c
124100141028     c* se non ci sono errori chiudo la dispo consegna
124200141028     c* controllo mail e cellulare: se errati non li scrivo
124300141028      /free
124400141028       if v2cCELL <>*blanks and forzacell=*blanks ;
124500141028       pInCell = %trim(v2ccell);
124600141028       pOutErr = *blanks;
124700141028       if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
124800141028          forzacell='S'  ;
124900141028       endif;
125000141028       endif;
125100141028      /end-free
125200141028     c* indirizzo email
125300141028    2c                   if        v2cmail<>*blanks and forzamail=*blanks
125400141028     c                   Clear                   dsemail
125500141028     c                   Eval      §emlindi = v2cmail
125600141028     c                   Call      'XEMAIL'
125700141028     c                   Parm                    dsemail
125800141028      * Errore
125900141028if  3c                   If        §emlerro <> '0'
126000141028     c                   eval      forzamail='S'
126100141028   x3c                   else
126200141028     c                   Eval      v2cmail = §emlindo
126300141028    3c                   endif
126400141028    2c                   endif
126500141028     c
126600141028     c                   exsr      confe
126700141028     c
126800141028     c                   ENDSR
126900141010      /free
127000141010       //**********************************************************************
127100141010           BEGSR    Car_ARBG  ;
127200141010       //**********************************************************************
127300141010
127400141010           d48trc='G';
127500141010           §BGTCR=IDCTCR;
127600141010           §BGHCR=idchcr;
127700141010           §BGDCR=idcdcr;
127800141010           §BGGC1=arbgc1;
127900141010           §BGGC2=arbgc2;
128000141010       //   Giorni di chiusura --> se richiesta preferenza M/P la traduco nel primo campo
128100141010       //                          libero dei giorni di chiusura
128200141010           clear wggc;
128300141010           if idcprcr<>' ';
128400141010           if idcprcr='M';
128500141010              wggc='P';
128600141010           else;
128700141010              wggc='M';
128800141010           endif;
128900141010           endif;
129000141010           if wggc<>*blanks;
129100141010       // Nel primo byte  del giorno di chiusura devo mettere il numero del giorno della
129200141010       // settimana che corrisponde alla data cons rich.
129300141010       // Nel secondo byte metto mattino o pomeriggio
129400141010               dataiso=%date(§bgdcr:*iso);
129500141010               EXEC SQL SET :dw = DAYOFWEEK_ISO(:dataiso);
129600141010              if §BGGC1=*blanks;
129700141010                 %subst(§BGGC1:2:1)=wggc;
129800141010                 %subst(§BGGC1:1:1)=%editc(dw:'X');
129900141010              else;
130000141010                 %subst(§BGGC2:2:1)=wggc;
130100141010                 %subst(§BGGC2:1:1)=%editc(dw:'X');
130200141010              endif;
130300141010              if §BGGC1=§BGGC2;
130400141010                 clear §BGGC2;
130500141010              endif;
130600141010           endif;
130700141010           §BGTC1=arbtc1;
130800141010           §BGTC2=arbtc2;
130900141010           ENDSR  ;
131000141010
131100141010       //**********************************************************************
131200141010            BEGSR   Car_ARBD  ;
131300141010       //**********************************************************************
131400141010
131500141010           if idctipodis='2';
131600141010              §BDRSD=arbrsd;
131700141010              §BDRD2=wrsd2 ;
131800141010              §BDIND=arbIND;
131900141010              §BDCAD=arbCAD;
132000141010              §BDLOD=arbLOD;
132100141010              §BDPRD=arbPRD;
132200141010              §BDNZD=arbNzd;
132300141010              §BDFIN=arbfin;
132400141010              §BDFFD=idcffd;
132500141017              if §bdffd='S';
132600141017                 §bdffd='Y';
132700141017              endif;
132800141010           else ;
132900141010              §BDRSD=%subst(idcrsd:1:35);
133000141010              §BDRD2=%subst(IDCRSD:36:35);
133100141010              §BDIND=IDCIND;
133200141010              §BDCAD=IDCCAD;
133300141010              §BDLOD=IDCLOD;
133400141010              §BDPRD=IDCPRD;
133500141010              §BDNZD=IDCNAZ;
133600141010       // Se CAP/LOCALITA/PROVINCIA sono invariati rispetto alla bolla passo
133700141010       // flag inoltro = alla bolla altrimenti lo passo vuoto in modo da farlo ricalcolare
133800141010       // a fnlr48r in base ai nuovi dati
133900141010              if arbcad=idccad and arblod=idclod and arbprd=idcprd;
134000141010                §BDFIN = arbfin;
134100141010              endif;
134200141010              §BDFFD=arbffd;
134300141010           endif;
134400141010       // P.IVA: PASSO SEMPRE ARBCPI PERCHÈ FNLR48R usa il campo solo se sped assegnato
134500141010       // e in questo caso in arbcpi c'è proprio la p.iva del destinatario
134600141010           §BDPKF  = arbpkf;
134700141010           §BDVLF  = arbvlf;
134800141010           §BDFVF  = arbfvf;
134900141010           §BDCPI  = ARBCPI;
135000141010           §BDREF  = IDCREF;
135100141010           §BDTEL  = IDCTELD;
135200141010         ENDSR  ;
135300141010
135400141010      /end-free
135500141010
135600141010     c**********************************************************************
135700141010     c* Pulizie e impostazioni DS per richiamo fnlr48r
135800141010     c**********************************************************************
135900141010     c     Pul_LR48      BEGSR
136000141010     C                   CLEAR                   fnlr48ds
136100141010     C                   CLEAR                   dsarbg
136200141010     C                   CLEAR                   dsarbd
136300141010     C                   CLEAR                   dsarbk
136400141010     C                   CLEAR                   darbt
136500141010     C                   z-add     ilry03aas     D48AAS
136600141010     C                   z-add     ilry03lnp     D48LNP
136700141010     C                   z-add     ilry03nrs     D48NRS
136800141010     C                   z-add     ilry03nsp     D48NSP
136900141010     c                   exsr      rep_FGS
137000141010     c
137100141010     c                   movel     wfgs          d48fgs
137200141010     C                   MOVEL     ARBCBO        D48CBO
137300141010     C                   MOVEL     'A'           D48TBO
137400141010     C                   MOVEL     'S'           D48F12
137500141010     C                   MOVEL     'V'           D48FFR
137600141010     c                   ENDSR
137700141010     c**********************************************************************
137800141010     c* Applicazione automatica variaizoni
137900141010     c**********************************************************************
138000141010     c     Applica       BEGSR
138100141010      /free
138200141010
138300141010       // Fermo Deposito o Altro Indirizzo
138400141010        if   (IDCTIPODIS='2' or IDCTIPODIS='3') ;
138500141014           exsr  Pul_lr48 ;
138600141010           d48trc='D';
138700141010           d48cvb='I0';
138800141010           if idctipodis='2'
138900141010              and idctipocli='M';
139000141010              d48cvb='I7';
139100141010           endif;
139200141010            exsr Car_ARBD  ;
139300141010           exsr  Call_lr48 ;
139400141010         endif  ;
139500141010
139600141010       // Appuntamento
139700141010        if    IDCTIPODIS='1' or (IDCTIPODIS='3'and idcdcr>0) ;
139800141010        // non forzo se supermercato perchè bisognerebbe usare CS
139900141014           exsr  Pul_lr48 ;
140000141014
140100141010           if arbtc1<>'S' and arbtc2<> 'S'   ;
140200141010           d48cvb='CI';
140300141010           else  ;
140400141010           d48cvb='CS';
140500141010           endif  ;
140600141010           exsr  Car_ARBG  ;
140700141010
140800141010           exsr  Call_lr48 ;
140900141010
141000141010        if *in66  ;
141100141010         *in70=*off ;
141200141010        endif ;
141300141010
141400141010         endif  ;
141500141010      /end-free
141600141010     c                   ENDSR
141700141010     c**********************************************************************
141800141010     c     Call_lr48     BEGSR
141900141010     c**********************************************************************
142000141010     C                   MOVEL     fnlr48ds      KPJBU
142100141010     C                   CALL      'FNLR48R'
142200141010     C                   PARM                    KPJBA
142300141010     C                   PARM                    DSARBD
142400141010     C                   PARM                    DSARBK
142500141010     C                   PARM                    DARBT
142600141010     C                   PARM                    DSARBG
142700141010     C                   PARM                    TRUL90DS
142800141010     c                   movel     kpjbu         fnlr48ds
142900141010     c
143000141010     c* se non premuto f12, quindi confermato, disabilito F12=Ritorno per obbligare a dare F6
143100141217     c*  nobn lo faccio nel caso sia dispo dirottamento
143200141112     c                   if        d48f12<>'1' and d48err=*blanks
143300151112     c                             and idcfla=' '
143400141010     c                   seton                                        66
143500141010     c                   endif
143600141010     c* Errore
143700141010    2c                   if        d48err<>*blanks
143800141010     c                   seton                                        90
143900141010     c                   seton                                        2843
144000141010     c                   movel     d48msg        vidmsg
144100141010    2c                   endif
144200141010     c
144300141010     c                   ENDSR
144400141006     c**********************************************************************
144500141010     c* Reperisco filiale gestione se non passata
144600141006     c**********************************************************************
144700141006     c     Rep_FGS       BEGSR
144800141006     c                   clear                   WFGS              3 0
144900141006     c                   if        ILRY03FGS>0
145000141006     c                   eval      WFGS=ILRY03FGS
145100141006     c                   else
145200141006     c* come p.o. gestione passo il gestore della lna
145300141006     c                   clear                   fnlv55ds
145400141006     c                   z-add     arblna        d55lin
145500141006     c                   movel     '6'           d55tpt
145600141006     C                   time                    wrkoggiiso
145700141006     C     *iso          move      wrkoggiiso    d55drf
145800141006     c                   call      'FNLV55R'
145900141006     c                   parm                    fnlv55ds
146000141006     c                   movel     d55tfa        wfgs
146100141006     c                   endif
146200141006     c                   ENDSR
146300141006     c**********************************************************************
146400141006     C*--- CHAIN TABEL00F
146500141006     c**********************************************************************
146600070316     C     CTABEL        BEGSR
146700070316     C     KTAB          CHAIN     TABEL                              30
146800070316     c                   if        %found(tabel00f) and tblflg<> ' '
146900070316     C                   SETON                                        30
147000070316     C                   END
147100070316     C                   ENDSR
147200070321     c**********************************************************************
147300070321     c* F6=Conferma
147400070321     c**********************************************************************
147500070321     C     Confe         BEGSR
147600070322
147700070327     c*  N O T E ************
147800070322     c* Note di ar4 trk 8 e 9
147900070420    1c                   if        v2hnot8<>v2cnot8 or v2hnot9<>v2cnot9
148000131008     c* richaimo pgm di manutenzione bolle per aggiornamento delle note
148100131008     c                   clear                   fnlr48ds
148200131008     c                   eval      d48cvb='NT'
148300131008     c                   eval      d48ffr='E'
148400140813     C                   z-add     ilry03aas     D48AAS
148500140813     C                   z-add     ilry03lnp     D48LNP
148600140813     C                   z-add     ilry03nrs     D48NRS
148700140813     C                   z-add     ilry03nsp     D48NSP
148800131008     C                   MOVEL     ARBCBO        D48CBO
148900131008     C                   MOVEL     'A'           D48TBO
149000131008     C                   MOVEL     'S'           D48F12
149100131008     C                   MOVEL     'N'           D48trc
149200131008     C                   MOVEL     fnlr48ds      KPJBU
149300131008     c                   eval      §bnu_sn='S'
149400131008     c                   eval      §bnuni70=v2cnot8+v2cnot9
149500131008     c                   eval      §bntrd='NOT'
149600131008     c                   call      'FNLR48R'
149700131008     C                   PARM                    KPJBA
149800131008     C                   PARM                    DSARBD
149900131008     C                   PARM                    DSARBK
150000131008     C                   PARM                    DARBT
150100131008     C                   PARM                    DSARBG
150200131008     C                   PARM                    trul90ds
150300131008     C                   PARM                    DSARBN
150400131008     C                   MOVEL     KPJBU         fnlr48ds
150500131008
150600131008     c* Ritorno con errore o premuto f12
150700131008    2c                   if        d48err<>*blanks
150800131008     c                   seton                                        90
150900131008     c                   seton                                        28
151000131008     c                   movel     d48msg        vidmsg
151100131008     c                   goto      f_confe
151200131008    2c                   endif
151300131008    1c                   endif
151400131008     c
151500140911     c* richiamo pgm di manutenzione bolle per aggiornamento delle note
151600140911      /free
151700140911           clear fnlr48ds;
151800140911           d48aas=ilry03aas;
151900140911           d48lnp=ilry03lnp;
152000140911           d48nrs=ilry03nrs;
152100140911           d48nsp=ilry03nsp;
152200140911           d48ffr='E'   ;
152300140911           d48cvb='NE';
152400140911           d48trc='N';
152500170111           if wmaiaff='S'  ;
152600170111           d48tbo='P'   ;
152700170111           else  ;
152800140911           d48tbo='A'   ;
152900170111           endif  ;
153000140911           d48cbo=arbcbo;
153100141013
153200140911           clear dsarbn;
153300141013           // non memorizzo se ha dato errore
153400141013           if forzamail=' '  ;
153500141013           §BNU_SN='S';
153600140911           §BNUNI70=v2cmail   ;
153700141013           endif  ;
153800140911           §BNR_SN='S';
153900140911           §BNREF=v2cref;
154000140911           §BNTEL=v2ctel ;
154100140911           §BNC_SN='S';
154200141013           // non memorizzo se ha dato errore
154300141013           if forzacell=' '  ;
154400140911           §BNCEL=v2ccell  ;
154500141013           endif  ;
154600140911           kpjbu=fnlr48ds;
154700140911           fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds:dsarbn);
154800140912           fnlr48ds=kpjbu;
154900140912    2      if d48err<>*blanks;
155000140912             *in90=*on;
155100140912             *in28=*on;
155200140912             vidmsg=d48msg;
155300140912             leavesr;
155400140912    2      endif;
155500151112       // Se premuto F6 senza aver dirottato/reso tolgo il giro DIR dalla bolla
155600151112           if idcfla<>' ' and *in66=*off;
155700141103              chain (idcaas:idclnp:idcnrs:idcnsp) fiarg01l;
155800141103              if %found(fiarg01l);
155900141103                 clear argcgi;
156000141103                 clear argtgi;
156100141103                 update fiarg000;
156200141103              endif;
156300141103           endif;
156400141001       //CHIUSURA NOTE R.A.
156500140911           exsr sr_noteRa;
156600140911       //Aggiorno il record TIIDC
156700140813              IDCELAUTE=KNMUS;
156800140813              IDCELADATA=%int( %subst( %char( %dec( %timestamp() ) )
156900140813                                 :1 :14 ) );
157000140814              update tiidc000  ;
157100140813      /end-free
157200070327     c
157300070322     c     f_confe       endsr
157400070411     c**********************************************************************
157500070411     c* Verifica abilitazione tasto F16-Gestione richieste assistenza
157600070411     c**********************************************************************
157700070411     C     SR_AbilF16    BEGSR
157800070411     c                   eval      TGDtor  = 'S'
157900070411     c                   eval      tgdogg=%editc(arblnp:'X')+%editc(arbnrs:'X')+
158000070411     c                             %editc(arbnsp:'X')+%editc(arbaas:'X')
158100070411     c     K02TGD02      setll     FITGD000
158200160404     c                   eval      *in64   = %equal(FITGD32L)
158300141002      /free
158400141002         XRTVSTKR (pStack:pStartPoint:pEntries);
158500141002         dsStack = pStack;
158600141002         IF  %lookup('FIDNA2R   ':skStack) > 0;
158700141002          *in64=*off ;
158800141002         endif      ;
158900141002
159000141002       endsr   ;
159100140911       //-------------------------------------------------------------*
159200140911       //Richiamo pgm di controllo eseguibilità dell'indicazione      *
159300140911       //-------------------------------------------------------------*
159400140911        Begsr sr_ChkIstruCo;
159500140911
159600140911        werrchk='0';
159700140911
159800140911        prmrqsOpCode = FNLRY00_RQSOPCODE_CHK_ISTRUCO    ;
159900140911        prmrqsformato='TIIDCDS  '  ;
160000140911        prmrpyformato='         '  ;
160100140911       prmRpyFormMsg='FNLRY00M0'  ;
160200140911
160300140911         prmrqsData = tiidcds  ;
160400140911         prmrqsDatasize = %SIZE(tiidcds);
160500140911         prmrpyDatasize = %SIZE(tiidcds);
160600140911         prmRpyMsgSize  = %SIZE(fnlry00m0);
160700140911         prmRpyForSize  = %SIZE(fnlry00f0);
160800140911         Fnlry00_Immetti(prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
160900140911                         prmRqsFormato : prmRqsData : prmRqsDataSize:
161000140911                         prmRpyFormato : prmRpyData : prmRpyDataSize:
161100140911                         prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
161200140911         //                prmRpyFormForz: prmRpyForzatu :prmRpyForSize) ;
161300140911
161400140911 5       if   prmrpyOpCode = FNLRY00_RPYOPCODE_ERROR  ;
161500140911            werrchk='1';
161600140911            fnlry00m0=prmrpymessage;
161700140911         endif;
161800140911
161900140911        endsr;
162000140911      /end-free
162100141001     c**********************************************************************
162200141001     c* CHIUSURA  Note R.A.
162300141001     c**********************************************************************
162400141001     C     sr_noteRa     BEGSR
162500141001     c                   clear                   fidna7ds
162600141001     C                   EVAL      IDNA7ANT=IDCANT
162700141001     C                   EVAL      IDNA7NUT=IDCNUT
162800141001     C                   EVAL      IDNA7PDT=IDCPDT
162900141001     C                   EVAL      IDNA7PCT=DUTPOU
163000141001     C                   EVAL      IDNA7utc=knmus
163100141006     c                   eval      IDNA7NOT=wnot
163200141001     c                   call      'FIDNA7R'
163300141001     c                   parm                    fidna7ds
163400141001     c
163500141001     C                   ENDSR
163600141003     c**********************************************************************
163700141003     C* richiama finestra per inserimento nuova R.A.
163800141003     c**********************************************************************
163900141003     C     WIN_RA        BEGSR
164000141003     c     forw          tag
164100141006     c                   setoff                                       68
164200141003     c                   exfmt     LRY03W03
164300141003     c                   setoff                                       2890
164400141003     c
164500141008     c**                 if        *inkl  or *inkf
164600141006     c                   eval      wnot    =LRY03W1+LRY03W2+LRY03W3+LRY03W4
164700141006     c                   if        wnot <>*blanks
164800141006     c                   seton                                        68
164900141006     c                   endif
165000141003     c                   leavesr
165100141008     c**                 endif
165200141006
165300141006     c                   goto      forw
165400141003     c
165500141003     C                   ENDSR
165600011227     c**********************************************************************
165700000607     C     *INZSR        BEGSR
165800011227     c**********************************************************************
165900070320     c
166000070123      *
166100070315     c     *dtaara       define    §azute        azuteds
166200070315     c     *dtaara       define    §datiute      ddatiute
166300070315     c                   in(e)     *dtaara
166400070315     c                   if        %error  or rsut = *blanks
166500070315     c                   clear                   tibs34ds
166600070315     c                   call      'TIBS34R'
166700070315     c                   parm                    tibs34ds
166800070315     c                   in        *dtaara
166900070315     c                   endif
167000030702      *reperimento data
167100070319     C                   TIME                    W0140            14 0
167200070319     C                   MOVE      W0140         WDAT              8 0
167300030702     C*
167400030702     C                   Z-ADD     WDAT          G02DAT
167500030702     C                   MOVEL     *BLANK        G02ERR
167600030702     C                   CALL      'XSRDA8'
167700030702     C                   PARM                    WLBDA8
167800030702     C* UDATE A 8 IN AAAA/MM/GG
167900030717     C                   Z-ADD     G02INV        WUDAte            8 0
168000091216      * Aggancio tabella "VPODECO" per reperimento data attivazione
168100091216      * gestione prepagati
168200151119     C***                CLEAR                   dsbs02
168300151119     c***                clear                   dvpodeco
168400151119     C***                MOVEL     'C'           T02MOD
168500151119     C***                MOVEL     KNSIF         T02SIF
168600151119     C***                MOVEL     'VPO'         T02COD
168700151119     c***                movel(P)  'DECO'        t02ke1
168800151119     C***                CALL      'TIBS02R'
168900151119     C***                PARM                    KPJBA
169000151119     C***                PARM                    dsbs02
169100151119    2C***                IF        T02ERR = *BLANKS
169200151119     C***                MOVEL     T02uni        dvpodeco
169300151119    2C***                ENDIF
169400070123      *
169500070124     C     Karb          klist
169600140813     C                   kfld                    ilry03aas
169700140813     C                   kfld                    ilry03lnp
169800140813     C                   kfld                    ilry03nrs
169900140813     C                   kfld                    ilry03nsp
170000140813     C     Kidc          klist
170100140813     C                   kfld                    ilry03aas
170200140813     C                   kfld                    ilry03lnp
170300140813     C                   kfld                    ilry03nrs
170400140813     C                   kfld                    ilry03nsp
170500140909     C                   kfld                    k_eladata
170600140813     C                   kfld                    ilry03prg
170700070201      *
170800070201     C     Kar4          klist
170900070201     C                   kfld                    arbaas
171000070201     C                   kfld                    arblnp
171100070201     C                   kfld                    arbnrs
171200070201     C                   kfld                    arbnsp
171300070321     c                   kfld                    wtrc
171400070321     c     KFiar5        Klist
171500070321     C                   kfld                    arbaas
171600070321     C                   kfld                    arblnp
171700070321     C                   kfld                    arbnrs
171800070321     C                   kfld                    arbnsp
171900070321     c                   Kfld                    KAr5Trd
172000070201      *
172100070201     C     Ktab          klist
172200070201     C                   kfld                    tblkut
172300070201     C                   kfld                    tblcod
172400070201     C                   kfld                    tblkey
172500070201     C                   z-add     1             tblkut
172600070321     c     KArn          Klist
172700070321     C                   kfld                    arbaas
172800070321     C                   kfld                    arblnp
172900070321     C                   kfld                    arbnrs
173000070321     C                   kfld                    arbnsp
173100070321     c                   Kfld                    KArncdn
173200070321     c                   Kfld                    KArngst
173300160404      * File FITGD32L
173400070411     c     K02TGD02      klist
173500070411     c                   kfld                    TGDtor
173600070411     c                   kfld                    TGDogg
173700070614     c*
173800070614     C     *LIKE         DEFINE    TBLKUT        §KUT
173900070614     C     *LIKE         DEFINE    TBLCOD        §COD
174000070614     C     *LIKE         DEFINE    TBLKEY        §KEY
174100030714
174200011227     C                   ENDSR
174300070328     c**********************************************************************
174400070328     C     chiudi        BEGSR
174500070328     c**********************************************************************
174600070328     c                   eval      d48tla='C'
174700070328     C                   MOVEL     fnlr48ds      KPJBU
174800070328     c                   call      'FNLR48R'
174900070328     C                   PARM                    KPJBA
175000070328     C                   PARM                    DSARBD
175100070328     C                   PARM                    DSARBK
175200070328     C                   PARM                    DARBT
175300070328     C                   PARM                    DSARBG
175400070328     C                   PARM                    trul90ds
175500070328
175600070328     c                   clear                   fnlv80ds
175700070328     c                   eval      ilv80tla='C'
175800070328     c                   call      'FNLV80R'
175900070328     c                   parm                    kpjba
176000070328     c                   parm                    fnlv80ds
176100070328
176200140130     c                   eval      ilrg7tla='C'
176300140130     c                   call      'FNLRG7R'
176400140130     c                   parm                    fnlrg7ds
176500070503
176600140922     c**                 eval      id34tla = 'C'
176700140922     c***                call      'FIDG34R'
176800140922     c***                parm                    fidg34ds
176900070704
177000070704     c                   eval      d66tla  = 'C'
177100120126     C                   MOVEL     fnlr66ds      KPJBU
177200070704     c                   call      'FNLR66R'
177300120126     c                   parm                    kpjba
177400070704
177500070704     c                   clear                   fnlv55ds
177600070704     C                   MOVEL     'C'           D55TLA
177700070704     C                   CALL      'FNLV55R'
177800070704     C                   PARM                    FNLv55DS
177900091216     c*
178000091216     c                   clear                   trulintds
178100091216     C                   MOVEL     'C'           iinttla
178200091216     C                   CALL      'TRULINTR'
178300091216     C                   PARM                    TRULINTDS
178400070328     c                   endsr
178500141008     C**************************************************************************
178600141008      * RICHIEDO LE STAMPANTI per stampa LDV
178700141008     C**************************************************************************
178800141008     c     RICHSTAMPAN   BEGSR
178900141008     C                   CLEAR                   TRUL90DS
179000141008      *
179100141008     C                   MOVEL     'S'           d90RSB
179200141008     C                   MOVEL     'S'           d90RSE
179300141008     C                   CALL      'TRUL90R'
179400141008     C                   PARM                    KPJBA
179500141008     C                   PARM                    trul90DS
179600141008      *
179700141008     C* F3 - FINE
179800141008     c                   if        d90f3<>'1'
179900141008      *
180000141008      * OVRPRTF BOLLE A4
180100141008     C                   MOVEA     D90PRB4       CMA4f6(30)
180200141008     C                   MOVEA     D90MDB4       CMA4f6(52)
180300141008     C*
180400141008     C                   Z-ADD     130           LUNG             15 5
180500141008     C                   CLEAR                   COMMAN          130
180600141008     C                   MOVEA     CMA4f6        COMMAN
180700141008     C                   CALL      'QCMDEXC'                            91
180800141008     C                   PARM                    COMMAN
180900141008     C                   PARM                    LUNG
181000141008      *
181100141008     C                   if        not *in91
181200141008      *
181300141008      * OVRPRTF BOLLE A5
181400141008     C                   MOVEA     D90PRB5       CMA5f6(30)
181500141008     C                   MOVEA     D90MDB5       CMA5f6(52)
181600141008     C*
181700141008     C                   Z-ADD     130           LUNG
181800141008     C                   CLEAR                   COMMAN
181900141008     C                   MOVEA     CMA5f6        COMMAN
182000141008     C                   CALL      'QCMDEXC'                            91
182100141008     C                   PARM                    COMMAN
182200141008     C                   PARM                    LUNG
182300141008     c                   endif
182400141008     C                   if        not *in91
182500141008      *
182600141008      * OVRPRTF etichette
182700141008     C                   MOVEA     D90PRe        CMetf6(30)
182800141008     C                   MOVEA     D90MDE        CMetf6(52)
182900141008     C*
183000141008     C                   Z-ADD     130           LUNG
183100141008     C                   CLEAR                   COMMAN
183200141008     C                   MOVEA     CMetf6        COMMAN
183300141008     C                   CALL      'QCMDEXC'                            91
183400141008     C                   PARM                    COMMAN
183500141008     C                   PARM                    LUNG
183600141008     c                   endif
183700141008     c                   endif
183800141008      *
183900141008     c                   ENDSR
184000070315**
184100141003Non trovate disposizioni di consegna da elaborare                              1
184200141013Presenti errori nel numero di cellulare del destinatario.Enter X forzare       2
184300141013Presenti errori nell'indirizzo Email Destinatario. enter x forzare             3
184400141001Disposizione di consegna ancora da elaborare                                   4
184500141008Non è stata eseguita nessuna variazione bolla.Premi F6 per forzare l'uscita    5
184600141008Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede         6
184700141024Non trovata spedizione                                                         7
184800151116Uscita senza esecuzione del RESO/DIROTTAMENTO: inserire note R.A.              8
184900141008**  cmA4f6
185000141008OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
185100141008USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
185200141008**  cmA5f6
185300141008OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
185400141008             USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
185500141008**  cmETf6
185600141008OVRPRTF FILE(FNLV22P)   OUTQ(xxxxxxxxxx) FORMTYPE('XXXXXXXXXX')
185700141008USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
