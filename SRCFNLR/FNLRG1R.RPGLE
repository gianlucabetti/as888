000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200070403     H* FNLRG1R *----------------------------------------------------*
000300070403     H* - Gestione contatti destinatari
000400070403     H*--------------------------------------------------------------*
000500070312     Ffnlrg1d   CF   E             WORKSTN
000600070312     f                                     sfile(lrg1s01:snrr1)
000700070131     Ffnarb05l  if   E           k disk    rename(fnarb000:fnarb5)
000800070402     Ffnarb01l  uf   E           k disk
000900070316     Fazorg01l  if   E           k disk
001000070123     Ffiar901l  if   E           k disk
001100070201     Ffiar401l  if   E           k disk
001200070330     Ffiar501l  if   E           k disk
001300070419     Ffnevb22l  if   E           k disk
001400070201     Ftabel00f  if   E           k disk
001500070412     Ffnfvv08l  if   E           k disk
001600070412     Fazcln01l  if   E           k disk
001700070522     Ffiarg01l  if   E           k disk
001800070504     d
001900070504     D sk8             S              1    DIM(8)
002000011228      *
002100011228     D Kpjba         E DS
002200011228      *
002300070321     D fnlrg2ds      E DS
002400070522     D fidg09ds      E DS
002500070430     D fnlrg6ds      E DS
002600070430     D  BLOV                  19     27    DIM(9)
002700070430     D dlrg6abi      E DS
002800070321     D fnlv80ds      E DS
002900140211     D fnlv82ds      E DS
003000070330     D fnlv53ds      E DS
003100070201     D fidn12ds      E DS
003200070315     d azuteds       e ds                  extname(azute00f)
003300070315     d ddatiute      e ds
003400070315     d tibs34ds      e ds                  inz
003500070131     D trul06ds      E DS
003600070131     D  lin                    1     90  0 dim(30)
003700141030     D fnlry09ds2    E DS
003800170620
003900170620      // - Reperimento filiali gestite dall'utente
004000170620     d TRUL31DS      e ds
004100170620     d  POG                   10    759    dim(250)
004200170620
004300070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
004400070206     D PARAM1          DS                  INZ
004500070206     D  PA1AAS                14     17  0
004600070206     D  PA1LNP                18     20  0
004700070206     D  PA1NRS                21     22  0
004800070206     D  PA1NSP                23     29  0
004900070206     D  PA1F03                30     30
005000070206     D  PA1FLG                31     31
005100070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
005200070206     D  PA1GIA               144    144
005300070330     d
005400070330     D                 DS
005500070330     D  ARBAAS                 1      4  0
005600070330     D  ARBMGS                 5      8  0
005700070330     D  ARBDSP                 1      8  0
005800070412     D                 DS
005900070412     D  kaaa                   1      4  0
006000070412     D  kmmm                   5      6  0
006100070412     D  kgg                    7      8  0
006200070412     D  kDATA                  1      8  0
006300070412     D clnmat          DS
006400070412     D  MAT                    1     31    dim(31)
006500070412     D clnpom          DS
006600070412     D  pom                    1     31    dim(31)
006700070330     d
006800070201     d ds3a          e ds
006900070316     d dsbl4l        e ds
007000080908     d dsbl4a        e ds
007100070330     d dar5gen       e ds
007200070319     d og148         e ds
007300071120     d og147         e ds
007400011228      *
007500070412     D KTFA            S                   LIKE(clntfa)
007600070412     D KTFP            S                   LIKE(clntfp)
007700070316     D wtit2           S                   like(v1ctit2)
007800070330     D KtrcK           S                   like(ar4trc)
007900070330     D Ktrd            S                   like(ar5trd) inz('GEN')
008000070327     d K_aas           s                   like(arbaas)
008100070327     d K_lnp           s                   like(arblnp)
008200070327     d K_nrs           s                   like(arbnrs)
008300070327     d K_nsp           s                   like(arbnsp)
008400070412     D knpg            S                   LIKE(fvvnpg) inz(2)
008500070430     D s_olrg2f12      S                   LIKE(olrg2f12)
008600070430     D wprima_v        S              1
008700030604     D snrr1           S              5i 0
008800070129     D lasnrr1         S              5i 0
008900070316     D I_v1cdcr        S                   like(v1cdcr)
009000071126     D I_v1cdta        S                   like(v1cdta)
009100030613     D WLBDA8          DS
009200030613     D  G02DAT                 1      8  0
009300030613     D  G02INV                 9     16  0
009400030613     D  G02ERR                17     17
009500030613     D  G02TGI                18     22  0
009600070319
009700070315     d  l6             s              3  0 dim(30)
009800070315     d  l1             s              3  0 dim(30)
009900070320     D nog             S              3  0 DIM(30)                              FIL.GESTITE
010000070330     d Dataiso         s               d   datfmt(*iso)
010100070406     d Datadmy         s               d   datfmt(*dmy)
010200070402     D wabb            S              1                                         FIL.GESTITE
010300070504     D wpos            S              2s 0
010400070508     D wpos2           S              2s 0
010500070504     D xx              S              1  0
010600071127     D soglia          S             12  0
010700071127     D arreffet        S             12  0
010800071128     D v1cdtaa         S              4s 0
010900070316      *---------------------------------------------------------------------
011000070321     D coscas          C                   CONST('           Contrasse-
011100070321     D                                     gno           ')
011200070321     D cosapp          C                   CONST('           Appuntame-
011300070321     D                                     nto           ')
011400070321     D cossup          C                   CONST('           Supermerc-
011500070321     D                                     ato           ')
011600070321     D cosffd          C                   CONST('         Fermo Depos-
011700070321     D                                     ito           ')
011800070321     D coslav          C                   CONST('          Lasciato A-
011900070321     D                                     vviso         ')
012000070321     D cossuptas       C                   CONST('      Supermercato:t-
012100070321     D                                     assativo      ')
012200070321     D cosapptas       C                   CONST('      Appuntamento:t-
012300070321     D                                     assativo      ')
012400070403     D TIPDEc          S             33    DIM(4) CTDATA PERRCD(1)
012500071126     D msg             S             78    DIM(25) CTDATA PERRCD(1)
012600071128     d weditora        c                   '  :  '
012700071128     d weditdta        c                   '  /  '
012800071204     d ord_ragprv      c                   'F11=Ord.xRagSoc/Prv'
012900071204     d ord_prvrag      c                   'F11=Ord.xPrv/RagSoc'
013000071204     d w_prvrsoc       C                   1
013100071204     d w_rsocprv       C                   2
013200071204     d W_ord           S              1  0
013300140307     d Wcau            S                   like(v1ccau)
013400070125      *---------------------------------------------------------------------
013500070125      * specifiche per ordinamento forzato sfl
013600070125     d NotUsed         S             16A
013700070125     d ReturnSize      S              9B 0
013800070125     d SizeList        S              9B 0
013900070125     d*---
014000070125     d MaxKey          C                   2
014100070125     d Ascendente      C                   1
014200070125     d Discendente     C                   2
014300070125     d Carattere       C                   6
014400070125     d Put             C                   1
014500070125     d EndPut          C                   2
014600070125     d Get             C                   3
014700070125     d Numerico        C                   8
014800070125     d*---
014900070125     d SflRcd          DS
015000070316     d  v1srsd
015100071203     d  v1sprd
015200070504     d  V1steld
015300070316     d  V1sAAS
015400080908     d**V1sLNP
015500080908     d**V1sNRS
015600080908     d**V1sNSP
015700080908     d  V1hLNP
015800080908     d  V1hNRS
015900080908     d  V1hNSP
016000080908     d  V1sddt
016100070330     d  V1sdsp
016200070504     d  V1sabi
016300071031     d  V1sast
016400070402     d  V1snt1
016500070330     d  V1snt2
016600070504     d  V1stIp_d
016700070125     d  Selected                      1A
016800070125
016900070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
017000070125     d QLGKL                         16    DIM(MaxKey)
017100070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
017200070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
017300070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
017400070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
017500170620
017600170620      //---------------------------------------------------------------
017700170620      //?Definizione Procedure usate.
017800170620      //---------------------------------------------------------------
017900170620       // - Caricamento Filiali in gestione
018000170620     d TRUL31R         pr                  extpgm('TRUL31R')
018100170620     d  kpjba                              likeds(kpjba)
018200170620     d  trul31ds                           likeds(trul31ds)
018300070125
018400070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
018500070125      /COPY QSYSINC/QRPGLESRC,QUSEC
018600070315      *---------------- Gestione prima videata -----------------------------
018700070502     c                   exsr      inzfgs
018800070315     C                   EXSR      Inzd01
018900000608      *
019000071119     c                   do        *hival
019100070315     c                   clear                   v1ctit2
019200070402     c                   clear                   v1ctipdec
019300070522     c                   clear                   v1cgircos
019400070522     c                   clear                   v1cgirt
019500071128     c                   clear                   v1clim
019600070315     c                   write     lrg1t01
019700070315     C                   EXFMT     lrg1d01
019800070315     c                   setoff                                       9028
019900070315     c                   clear                   vidmsg
020000170620     c                   exsr      sr_car£6
020100070315      * F3=Fine
020200070315     C                   if        *INKC
020300070315     C                   leave
020400070315     c                   endif
020500070319      * F18=cambio filiale - (Sproteggo cambio p.o gestione)
020600070315     c                   if        *inks
020700070315     c                   seton                                        1640
020800070315     c                   iter
020900070315     c                   endif
021000070315      * F7=Interrogazione bolle arrivi
021100070315     c                   if        *inkg
021200070315     c                   clear                   kpjbu
021300070315     c                   clear                   param1
021400070402     c                   movel     '2'           pa1flg
021500070315     c                   movel     param1        kpjbu
021600070315     c                   call      'FNLR36R'
021700070315     c                   parm                    kpjba
021800070402     C                   MOVEL     KPJBU         PARAM1
021900070402     C*
022000070402     C* SE PASSATA KEY LA IMPOSTO
022100070402     C     pa1nsp        IFGT      *ZEROS
022200070402     C                   MOVEL     pa1AAS        V1cAAS
022300070402     C                   MOVEL     pa1LNP        V1cLNP
022400070402     C                   MOVEL     pa1NRS        V1cNRS
022500070402     C                   MOVEL     pa1NSP        V1cNSP
022600070402     c                   endif
022700070315     c                   iter
022800070315     c                   endif
022900070522     c* F11=Interrogazione Giri
023000070522     c                   if        *inkk
023100070522     c                   clear                   fidg09ds
023200070522     c                   move      'P01'         d09iop0
023300070522     c                   movel     v1cfgs        d09ifgs
023400070522     c                   z-add     wudate        d09idat
023500070706     c                   eval      d09itug = 'C'
023600070522     c                   movel     fidg09ds      kpjbu
023700070522     c                   call      'FIDG09R'
023800070522     c                   parm                    kpjba
023900070522     c                   movel     kpjbu         fidg09ds
024000070522     c                   movel     d09ocgi       v1cgir
024100070522     c                   iter
024200070522     c                   endif
024300070402     c
024400070315      * controllo filiale in gestione
024500070315     c                   if        *in16
024600070315     c                   exsr      ctrfgs
024700070315     c   28              iter
024800070315     c                   endif
024900070319      * controlli videata
025000070315     C                   EXSR      Ctrd01
025100070315     c   90              iter
025200070316     c* se selezione per ragione sociale effettuo caricamento subfile
025300070316     c                   if        v1cnsp=0
025400070316     C                   EXSR      ges_s01
025500070402     c* Se esco con F3 - esco dal pgm
025600070402     c                   if        *inkc
025700070402     c                   leave
025800070402     c                   else
025900070402     c                   if        not *in28
026000070402     c                   exsr      INZd01
026100070402     c                   endif
026200070402     c
026300070315     c                   iter
026400070402     c                   endif
026500070402     c
026600070316     c                   else
026700070504     c* anche per spedizione, se non impostata la causale contatto
026800070504     c*  imposto il campo v1stip per il richiamo a fnlrg2r
026900070504     c                   clear                   wpos
027000070504     c                   exsr      CerABI
027100070504     c
027200070316     c* richiamo pgm gestione singola spedizione
027300070321     c                   exsr      chiamo
027400070402     c                   exsr      INZd01
027500070316     c                   iter
027600070316     c                   endif
027700011227      *
027800011227     C                   ENDDO
027900070321      * Chiusura con lr
028000070321     c                   eval      ilrg2tla='C'
028100070321     c                   call      'FNLRG2R'
028200070321     c                   parm                    kpjba
028300070321     c                   parm                    fnlrg2ds
028400070430     c*
028500070430     c                   eval      ilrg6tla='C'
028600070430     c                   call      'FNLRG6R'
028700070430     c                   parm                    fnlrg6ds
028800140211
028900140211     c                   eval      ilv82tla='C'
029000140211     c                   call      'FNLV82R'
029100140211     c                   parm                    fnlv82ds
029200141030
029300141030     c                   eval      ilry09tla= 'C'
029400141030     c                   eval      kpjbu=fnlry09ds2
029500141030     c                   call      'FNLRY09R'
029600141030     c                   parm                    kpjba
029700011227      *
029800070315     C                   seton                                        lr
029900070315     c**********************************************************************
030000070315     c* inizializza d01
030100070315     c**********************************************************************
030200070315     C     Inzd01        BEGSR
030300070403     c                   setoff                                       18
030400070315
030500070315     c                   clear                   v1ccau
030600070315     c                   clear                   v1crsd
030700070315     c                   clear                   v1crsa
030800070402     c                   eval      v1ctip='D'
030900070315     c                   clear                   v1ctss
031000070316     c                   clear                   v1cdcr
031100070319     c                   clear                   I_v1cdcr
031200070315     c                   clear                   v1clnp
031300070315     c                   clear                   v1cnrs
031400070315     c                   clear                   v1cnsp
031500070330     c                   movel     'S'           v1cabb
031600070319     c                   move      wdat          v1caas
031700070507     c                   clear                   wtit2
031800070521     c                   eval      v1cgir=*all'9'
031900071120     c                   clear                   v1cdta
032000071126     c                   clear                   I_v1cdta
032100071120     c                   clear                   v1cora
032200070315
032300070315     C                   ENDSR
032400070502     c**********************************************************************
032500070502     c* inizializza p.o. in gestione
032600070502     c**********************************************************************
032700070502     c     inzfgs        begsr
032800070502      * imposto il p.o. in gestione
032900070502     c                   eval      v1cfgs = dutpou
033000070502     c     v1cfgs        chain     azorg01L
033100070502     c                   if        %found(azorg01l) and orgfva = *blanks
033200070502     c                   eval      v1dfgs = orgdes
033300070502     c                   eval      og148=orgde8
033400071120     c                   eval      og147=orgde7
033500070502     c                   endif
033600070502     c*
033700070502     c* Controllo abilitazione del p.o. gestione alla gestione telefonate
033800070502     c                   movel     §oggeot       fgsgeot           1
033900071120     c                   eval      *in12=§ogdoada='S'
034000070502     c*
034100070502      * disabilito f18 se p.o. di secondo livello
034200070502     c                   eval      *in17 = (dutlpo = '2' or dutlpo = *blanks)
034300070502     c                   eval      *in16=*off
034400170620     c****               exsr      sr_car£6
034500070502     c                   endsr
034600070316     c**********************************************************************
034700070316     c* Caricamento £6 della filiale in gestione
034800070316     c**********************************************************************
034900070316     C     SR_car£6      BEGSR
035000070316      * carico filiali £6
035100070316     C                   clear                   trul06ds
035200070316     C                   movel     '£6'          d06cod
035300070316     C                   movel     v1cfgs        d06key
035400070316     C                   movel     trul06ds      kpjbu
035500070316     C                   CALL      'TRUL06R'
035600070316     C                   parm                    kpjba
035700070316     C                   movel     kpjbu         trul06ds
035800070316     C                   movea     lin           l6
035900070316     c                   endsr
036000070315     c**********************************************************************
036100070315     c* Controllo filiale in gestione
036200070315     c**********************************************************************
036300070315     C     Ctrfgs        BEGSR
036400070315     c*
036500070315     c                   setoff                                       40
036600070315     c                   clear                   v1dfgs
036700070320      * Il p.o. gestione non può essere un p.o. gestito
036800070320     c     V1cFgs        Lookup    Nog                                    30
036900070320     c                   If        *In30
037000070320     c                   Eval      *In40 = *On
037100070320     c                   Eval      *In28 = *On
037200070320     c                   Eval      VidMsg = Msg(1)
037300070320     c                   leavesr
037400070320     c                   EndIf
037500170620     c**** v1cfgs        lookup    l1                                     30
037600170620     c****               if        not *in30
037700170620       //?Filiale in gestione all'utente
037800170620         IF  %lookup(%editc(v1cfgs:'X'):pog) = 0;
037900070315     c                   seton                                        2840
038000070315     c                   eval      vidmsg = msg(01)
038100070315     c                   leavesr
038200070315     c                   endif
038300070315
038400070315      * decodifica p.o. in gestione
038500070319     c                   clear                   og148
038600071120     c                   clear                   og147
038700070315     c     v1cfgs        chain     azorg01L
038800070315     c                   if        %found(azorg01l) and orgfva = *blanks
038900070315     c                   eval      v1dfgs = orgdes
039000070319     c                   eval      og148=orgde8
039100071120     c                   eval      og147=orgde7
039200070315     c                   endif
039300070315     c                   setoff                                       16
039400071120     c                   eval      *in12=§ogdoada='S'
039500070319     c* Controllo abilitazione del p.o. gestione alla gestione telefonate
039600070319     c                   if        §oggeot<>'S'
039700070319     c                   seton                                        2840
039800070319     c                   eval      vidmsg = msg(11)
039900070319     c                   leavesr
040000070319     c                   endif
040100070417      *
040200070417     c                   movel     §oggeot       fgsgeot           1
040300070316     c                   exsr      sr_car£6
040400070417      *
040500070315     c                   endsr
040600070315     c**********************************************************************
040700070315     c* Controlli video D01.
040800070315     c**********************************************************************
040900070315     C     Ctrd01        BEGSR
041000070315      *
041100070316     c                   setoff                                       414243
041200070316     c                   setoff                                       444546
041300071120     c                   setoff                                       474849
041400070406     c
041500070406     c*  Se il p.o. in gestione non ha tellus attiva, non posso usare
041600070406     c*  il pgm
041700070406     c                   if        fgsgeot=' '
041800070406     c                   seton                                        429028
041900070406     c                   eval      vidmsg=msg(11)
042000070406     c                   goto      endd01
042100070406     c                   endif
042200070406     c
042300070315     c* causale contatto obbligatoria
042400070503    1c***                if        v1ccau=*blanks
042500070503     c***                seton                                        429028
042600070503     c***                eval      vidmsg=msg(2)
042700070503     c***                goto      endd01
042800070503    1c***                endif
042900070319     c* selezione per ragione sociale OPPURE per numero spedizione
043000070319    1c                   if        (v1crsd<>*blanks or v1crsa<>*blanks
043100070328     c                              or v1ctss='S')
043200070316     c                             and v1cnsp>0
043300070319     c                   seton                                        459028
043400070316     c                   eval      vidmsg=msg(3)
043500070316     c                   goto      endd01
043600070316    1c                   endif
043700070330     c
043800070319     c* GESTIONE SELEZIONE PER RAGIONE SOCIALE
043900070316    1c                   if        v1cnsp=0
044000070319     c* errore se immesso sia tipo spedizioni che tassative
044100070319     c                   if        v1ctip<>*blanks and v1ctss<>*blanks
044200070319     c                   seton                                        459028
044300070319     c                   eval      vidmsg=msg(10)
044400070319     c                   goto      endd01
044500070319     c                   endif
044600070319     c* Tipo spedizioni obbligatorio
044700070330    2c                   if        v1ctip=*blanks and v1ctss =*blanks
044800070330     c                   seton                                        459028
044900070330     c                   eval      vidmsg=msg(6)
045000070330     c                   goto      endd01
045100070330    2c                   endif
045200070402     c* Se richiesto appuntamento e le attensa di giacenza, errore
045300070402     c*  perchè non è possibile mettere la E come esito x appunt.
045400070406     c**                 if        v1ccau='A' and v1ctip='E'
045500070406     c**                 seton                                        459028
045600070406     c**                 eval      vidmsg=msg(17)
045700070406     c**                 goto      endd01
045800070406    2c**                 endif
045900070316      * se RSD 'DA'>*blanks e RSD 'A'a blank la inserisco uguale
046000070316    2c                   if        v1crsd <> *blank and v1crsa = *blank
046100070316     c                   eval      v1crsa = %trimr(v1crsd) + 'Z'
046200070316    2c                   endif
046300070316      * se RSD 'DA'>*blanks e RSD 'A'con solo iniziale metto nel
046400070316      * secondo byte la 'Z' x la selezione
046500070316    2c                   if        v1crsd <> *blank and v1crsa<> *blank and
046600070316     c                             %subst(v1crsa:2:1) = ' '
046700070316     c                   eval      v1crsa = %trimr(v1crsa) + 'Z'
046800070316    2c                   endif
046900070316      * vice versa RSD 'A'>*blank e RSD 'DA'a blank la inserisco uguale
047000070316    2c                   if        v1crsa <> *blank and v1crsd = *blank
047100070316     c                   movel     v1crsa        v1crsd
047200070316    2c                   endif
047300070316      * verifico che RSD 'DA' sia congruente con RSD 'A'
047400070316    2c                   if        v1crsd > v1crsa
047500070316     c                   seton                                        419028
047600070316     c                   eval      vidmsg=msg(5)
047700070316     c                   goto      endd01
047800070316    2c                   endif
047900070503     c* Tassative solo se richiesta causale per "S"
048000070503    2c***                if        v1ctss='S' and v1ccau<>'A' and
048100070503     c***                          v1ccau<>'S'
048200070503    2c                   if        v1ctss='S' and v1ccau<>'S' and
048300070503     c                             v1ccau<>' '
048400070316     c                   seton                                        469028
048500070316     c                   eval      vidmsg=msg(7)
048600070316     c                   goto      endd01
048700070316    2c                   endif
048800070330     c
048900070316     c* se richieste tassative controllo dcr
049000070319    2c                   if        v1ctss='S'
049100070319    3c                   if        v1cdcr=0
049200070319     c                   seton                                        479028
049300070319     c                   eval      vidmsg=msg(9)
049400070319     c                   goto      endd01
049500070319    3c                   endif
049600070316     c                   clear                   wlbda8
049700070316     C                   Z-ADD     v1cdcr        G02DAT
049800070316     C                   MOVEL     *BLANK        G02ERR
049900070316     C                   CALL      'XSRDA8'
050000070316     C                   PARM                    WLBDA8
050100070319    3c     g02err        ifeq      '1'
050200070316     c                   seton                                        479028
050300070316     c                   eval      vidmsg=msg(9)
050400070316     c                   goto      endd01
050500070319    3c                   endif
050600070319     c                   z-add     g02inv        I_v1cdcr
050700070402     c                   z-add     g02dat        v1cdcr
050800070319    2c                   endif
050900071126     c* data ora limite abbinamento
051000071126    2c                   if        *in12
051100071126    3c                   if        v1cdta>0
051200071126    4c                   if        v1cabb<>'S'
051300071126     c                   seton                                        489028
051400071126     c                   eval      vidmsg=msg(22)
051500071126     c                   goto      endd01
051600071126    4c                   endif
051700071126     c                   clear                   wlbda8
051800071126     C                   Z-ADD     v1cdta        G02DAT
051900071126     C                   MOVEL     *BLANK        G02ERR
052000071126     C                   CALL      'XSRDA8'
052100071126     C                   PARM                    WLBDA8
052200071126    4c     g02err        ifeq      '1'
052300071126     c                   seton                                        489028
052400071126     c                   eval      vidmsg=msg(23)
052500071126     c                   goto      endd01
052600071126    4c                   endif
052700071126     c                   z-add     g02inv        I_v1cdta
052800071126     c                   z-add     g02dat        v1cdta
052900071126    4c                   if        v1cora=0 or v1cora>2400
053000071126     c                   seton                                        499028
053100071126     c                   eval      vidmsg=msg(24)
053200071126     c                   goto      endd01
053300071126    4c                   endif
053400071128   x3c                   else
053500071128     c                   if        v1cora<>0
053600071128     c                   seton                                        499028
053700071128     c                   eval      vidmsg=msg(25)
053800071128     c                   goto      endd01
053900071128     c                   endif
054000071126    3c                   endif
054100071126    2c                   endif
054200070315     c* gestione selezione per numero spedizione
054300070316   x1c                   else
054400070327     c                   z-add     v1caas        k_aas
054500070327     c                   z-add     v1clnp        k_lnp
054600070327     c                   z-add     v1cnrs        k_nrs
054700070327     c                   z-add     v1cnsp        k_nsp
054800070402     c     karb          chain(N)  fnarb01l
054900070402    2c                   if        not %found(fnarb01l)
055000070316     c                   seton                                        449028
055100070316     c                   eval      vidmsg=msg(4)
055200070316     c                   goto      endd01
055300070402    2c                   endif
055400070316     c* la linea di arrivo deve essere in gestione
055500070316     c     arblna        lookup    l6                                     30
055600070402    2c                   if        *in30='0'
055700070316     c                   seton                                        449028
055800070402     c                   eval      vidmsg=msg(15)
055900070316     c                   goto      endd01
056000070402    2c                   endif
056100070402     c* Escludo se bloccata, in distinta o consegnata
056200070402    2c                   if        arbfbc='G' or arbfbc='*' or
056300070402     c                             arbfbc='B'
056400070402     c                   seton                                        449028
056500070402     c                   eval      vidmsg=msg(13)
056600070402     c                   eval      %subst(vidmsg:48:1)=arbfbc
056700070402     c                   goto      endd01
056800070402    2c                   endif
056900070402     c
057000080418    2c                   if        arbndc>0  and arbdcm=0
057100070402     c                   seton                                        449028
057200070402     c                   eval      vidmsg=msg(14)
057300070402     c                   goto      endd01
057400070402    2c                   endif
057500080418     c* Permetto solo i ritorni all'incasso non abilitati
057600080418    2c                   if        arbdcm>0 and arbicc<>'R'
057700080418     c                   seton                                        449028
057800080418     c                   eval      vidmsg=msg(14)
057900080418     c                   goto      endd01
058000080418    2c                   endif
058100070402     c
058200070316     c* controllo congruenza spedizione con causale contatto
058300070316     c                   exsr      ctrspecau
058400070504     c
058500070504    2c                   if        *in08
058600070504     c                   seton                                        449028
058700070316     c                   goto      endd01
058800070402   x2c                   else
058900070402     c
059000070402     c                   clear                   olv80tat
059100070504     c* se non c'e' errore di scelta posso  entrare solo se c'e'
059200070402     c*  blocco impostato (a S/T/N)
059300070402     c*  Se manca il blocco entro solo se tassativa
059400070504     c*  o con F13 per ribolccare spedizione per A/S/F
059500070504    3c                   if        wblk=*blanks  and v1ccau='S'
059600070402     c* Verifico se tassativa
059700070402     c                   exsr      CALLLV80
059800070403    3c                   endif
059900070403     c
060000070504    3c                   if        wblk=*blanks and olv80tat<>'S'
060100070504     c                   select
060200070504     c* Se C/L messaggio, bloccante
060300070504    4c                   when      v1ccau='C' or v1ccau='L'
060400070504     c                   seton                                        449028
060500070504     c                   eval      vidmsg=msg(16)
060600070504     c                   goto      endd01
060700070504     c
060800070504     c* non immessa causle contatto: errore bloccante
060900070504    4c                   when      v1ccau=' ' and not *inkm
061000070504     c                   seton                                        449028
061100070504     c                   eval      vidmsg=msg(19)
061200070504     c                   goto      endd01
061300070504   x4c                   other
061400070503     C* SE F13 - obbligatoria causale contatto
061500070504    5c                   if        *inkm and v1ccau=' '
061600070503     c                   seton                                        449028
061700070503     c                   eval      vidmsg=msg(20)
061800070503     c                   goto      endd01
061900070503    5c                   endif
062000070403     c
062100070403     c* se non forzato con F13, per A/S/F/tassative errore forzabile
062200070403    5c                   if        not *inkm
062300070403     c                   seton                                        449028
062400070403     c                   eval      vidmsg=msg(18)
062500070403     c                   seton                                        18
062600070403     c                   goto      endd01
062700070403    5c                   endif
062800070504    4c                   endsl
062900070402     c
063000070402    3c                   endif
063100140210
063200070402    2c                   endif
063300070402    1c                   endif
063400070315      *
063500070316     C     endd01        ENDSR
063600070316     c**********************************************************************
063700070316     c* Controllo congruenza fra causale contatto e spedizione
063800070316     c**********************************************************************
063900070316     C     Ctrspecau     BEGSR
064000070504     c                   clear                   wblk              5
064100070504     c                   setoff                                       08
064200070504     c
064300070402     c                   move      '3A'          tblcod
064400070402     c                   movel(p)  arbcbo        tblkey
064500070402     c                   exsr      ctabel
064600070402     c                   movel     tbluni        ds3a
064700070402     c
064800070504     c* Escludo bolle di recupero tranne che per lasciato avviso
064900070402     c                   if        §3arbl='R'
065000070402     c                   if        v1ccau='A' or v1ccau='S' or
065100070504     c                             v1ccau='F' or v1ccau='C'
065200070504     c                   seton                                        08
065300070402     c                   endif
065400070402     c                   endif
065500070402     c
065600070402     c
065700070402    1c                   select
065800070316     c* C=C/assegno
065900070402    1c                   when      v1ccau='C'
066000070402    2c                   if        §3afca=0
066100070504     c                   seton                                        08
066200070330     c* se bolla con contrassegno verifico che effettivamente l'impor-
066300070316     c* to sia > 0
066400070402   x2c                   else
066500070316     c     karb          chain     fiar901l
066600070402    3c                   if        not %found(fiar901l) or ar9cas=0
066700070504     c                   seton                                        08
066800070402     c* se bolla non abilitata, la abilito subito
066900070402    4c                   if        arbacc=' '
067000070402     c     karb          chain     fnarb01l
067100070402    5c                   if        not %error
067200070402     c                   eval      arbacc='S'
067300070402     c                   update    fnarb000
067400070402    5c                   endif
067500070402    4c                   endif
067600070402     c
067700070402    3c                   endif
067800070402    2c                   endif
067900070504     c
068000070504    2c                   if        *in08='0'
068100070316     c                   eval      wtit2=Coscas
068200070402    2c                   endif
068300070316     c
068400070316     c* A=Appuntamento
068500070316     c                   when      v1ccau='A'
068600070417     c                   if        arbtc1<>'A' and arbtc2<>'A'
068700070504     c                   seton                                        08
068800070316     c                   endif
068900070504     c                   if        *in08='0'
069000070316     c                   eval      wtit2=Cosapp
069100070316     c                   endif
069200070316     c* S=Supermercato
069300070316     c                   when      v1ccau='S'
069400070417     c                   if        arbtc1<>'S' and arbtc2<>'S'
069500070504     c                   seton                                        08
069600070316     c                   endif
069700070504     c                   if        *in08='0'
069800070316     c                   eval      wtit2=Cossup
069900070316     c                   endif
070000070316     c* F=Fermo Deposito
070100070316     c                   when      v1ccau='F'
070200070316     c                   if        arbffd=*blanks
070300070504     c                   seton                                        08
070400070316     c                   endif
070500070504     c                   if        *in08='0'
070600070316     c                   eval      wtit2=Cosffd
070700070316     c                   endif
070800070316     c* L=Lasciato Avviso
070900070316     c                   when      v1ccau='L'
071000070316     c                   if        arbfbc='A'
071100070419     c     karb          setgt     fnevb22l
071200070419     c     karb          readpe    fnevb22l
071300070402     c
071400070419     c                   if        %eof(fnevb22l) or (evb2tel<>'S'
071500070402     c                             and evb2tel<>'T' and evb2tel<>'N')
071600070504     c                   seton                                        08
071700070316     c                   endif
071800070316     c                   else
071900070504     c                   setoff                                       08
072000070316     c                   endif
072100070504     c                   if        *in08='0'
072200070316     c                   eval      wtit2=Coslav
072300070316     c                   endif
072400070316     c                   endsl
072500070316     c*
072600070504     c* 08 on -  spedizione che non prevede l'abilitazione richiesta
072700070504    1c                   if        *in08
072800070316     c                   eval      vidmsg=msg(8)
072900070402   x1c                   else
073000070402     c
073100070402     c* chaino record "L" e carico il tipo blocco che mi serve
073200070402     c*
073300070402     c* se non è lasciato avviso, chaino tipo record "L"
073400070503    3c**                 if        v1ccau<>'L'
073500070503     c**                 movel     'L'           Ktrck
073600070503     c**   kar4          chain     fiar401l
073700070503    4c**                 if        not %found(fiar401l)
073800070503     c**                 clear                   ar4not
073900070503    4c**                 endif
074000070503     c**
074100070503     c**                 movel     ar4not        dsbl4l
074200070503    3c**                 endif
074300070503     c
074400070503     c* richiamo fnlrg6r per avere la stringa delle abilitazioni
074500070503     c*
074600070503     c                   clear                   fnlrg6ds
074700070503     c                   eval      ilrg6aas=k_aas
074800070503     c                   eval      ilrg6lnp=K_lnp
074900070503     c                   eval      ilrg6nrs=K_nrs
075000070503     c                   eval      ilrg6nsp=K_nsp
075100070503     c* Escludo dalla visualizzazione Assegnati
075200070503     c                   movel     '9'           ilrg6nb1
075300070503     c                   call      'FNLRG6R'
075400070503     c                   parm                    fnlrg6ds
075500070503     c                   movel     olrg6abi      dlrg6abi
075600070402     c
075700070402     c* Memorizzo il blocco presente su ar4 in base alla causale contatto
075800070402    3c                   select
075900070504     c                   when      v1ccau='S'
076000070504     c                   eval      wblk=d§lrg6tsu
076100070402     c
076200070504     c                   when      v1ccau='A'
076300070504     c                   eval      wblk=d§lrg6tap
076400070504     c
076500070402     c                   when      v1ccau='C'
076600070402     c                   if        arbacc<>'S'
076700070503     c                   eval      wblk=d§lrg6tca
076800070402     c                   endif
076900070402
077000070402     c                   when      v1ccau='F'
077100070503     c                   eval      wblk=d§lrg6tfd
077200070402
077300070402     c                   when      v1ccau='L'
077400070503     c                   eval      wblk=d§lrg6tla
077500070503     c
077600070503     c* se non impostato metto se ha almeno una abilitazione
077700070503     c                   when      v1ccau=' '
077800070504     c                   eval      wblk=%subst(olrg6abi:3:5)
077900070504     c
078000070402    3c                   endsl
078100070402    1c                   endif
078200070402     c
078300070316     c                   endsr
078400070316     C*--- CHAIN TABEL00F --------------------------------------------*
078500070316     C     CTABEL        BEGSR
078600070316     C     KTAB          CHAIN     TABEL                              30
078700070316     c                   if        %found(tabel00f) and tblflg<> ' '
078800070316     C                   SETON                                        30
078900070316     C                   END
079000070316     C                   ENDSR
079100011227     c**********************************************************************
079200011227     c* gestione sfl
079300011227     c**********************************************************************
079400070316     C     ges_s01       BEGSR
079500000627      *
079600070402     c                   eval      *inke='1'
079700070402     c* looppo per F5 - rivisulizza
079800070402     c                   dow       *inke='1'
079900070402     c
080000070402     c                   eval      *inke='0'
080100070316     C                   EXSR      ries01
080200071204     c* ordino il subfile per provincia/ragione sociale dst.solo se caricati
080300070316     c* record
080400070316     c                   if        ok>0
080500071204     c                   eval      w_ord=w_prvrsoc
080600070316     C                   EXSR      ordina
080700070316     c                   else
080800070402     c
080900070402     c* Esco dal sfl emettendo msg di errore
081000070327     c                   setoff                                       20
081100070402     c                   seton                                        2890
081200070504ù    c                   movel     msg(12)       vidmsg
081300070402     c                   leave
081400070316     c                   endif
081500070316      * Imposto sottotitolo seconda videata
081600070316     c                   eval      v1ctit2=wtit2
081700070522     c                   if        v1cgir<>*all'9'
081800070522     c                   eval      v1cgircos='Giro'
081900070522     c                   eval      v1cgirt=v1cgir
082000070522     c                   if        v1cgirt=*blanks
082100070522     c                   eval      v1cgirt='Senza giro'
082200070522     c                   eval      v1cgircos=*blanks
082300070522     c                   endif
082400070522     c                   endif
082500070507     c* se specificata la causale contatto, scirvo nel sfl ALTRI contatti
082600070507     c                   if        wtit2<>*blanks
082700070507     c                   eval      v1caltri='Altri'
082800070507     c                   else
082900070507     c                   clear                   v1caltri
083000070507     c                   endif
083100070507     c
083200070316     c                   if        v1ctss='S'
083300070316     c                   if        v1ccau='S'
083400070316     c                   eval      v1ctit2=cossuptas
083500070316     c                   else
083600070316     c                   eval      v1ctit2=cosapptas
083700070316     c                   endif
083800070316     c                   endif
083900070330     c
084000070330      * Imposto tipo contatto in testata
084100070330     c                   select
084200070330     c                   when      v1ctip='D'
084300070330     c                   eval      v1ctipdec=tipdec(1)
084400070330     c                   when      v1ctip='R'
084500070330     c                   eval      v1ctipdec=tipdec(2)
084600070330     c                   when      v1ctip='E'
084700070330     c                   eval      v1ctipdec=tipdec(3)
084800070330     c                   when      v1ctip='T'
084900070330     c                   eval      v1ctipdec=tipdec(4)
085000070330     c                   ENDSL
085100071128      * Imposto data e ora limite abbinamento se presenti
085200071128     c                   if        *in12 and v1cdta>0
085300071128     c                   movel     v1cdta        v1cdtaa
085400071128     c                   eval      v1clim='Lim.Abbin. ' +
085500071128     c                             %editw (v1cdtaa:weditdta)+
085600071128     c                             ' ' +
085700071128     c                             %editw(v1cora:weditora)
085800071128     c                   endif
085900071204     c                   eval      v1tf11=ord_ragprv
086000070316     c*
086100070316     C                   DO        *hival
086200070316     C                   WRITE     LRG1T01
086300070316     C                   WRITE     LRG1Z01
086400070316     C                   EXFMT     lrg1c01
086500011227     C                   SELECT
086600070402     c* esco per F3-Fine  F12-Ritorno  F5-rivisualizza
086700070402     C                   WHEN      *INKL=*ON or *inkc  or *inke
086800070316     C                   leave
086900071204     c* f11=Cambio ordinamento sfl
087000071204     C                   WHEN      *INkk
087100071204     c                   select
087200071204     c                   when      w_ord=w_prvrsoc
087300071204     c                   eval      w_ord=w_rsocprv
087400071204     c                   eval      v1tf11=ord_prvrag
087500071204     c                   when      w_ord=w_rsocprv
087600071204     c                   eval      w_ord=w_prvrsoc
087700071204     c                   eval      v1tf11=ord_ragprv
087800071204     c                   endsl
087900071204     C                   exsr      ordina
088000071204     c                   iter
088100011227
088200011227     C                   OTHER
088300011227
088400070321     C                   EXSR      GestioneSFL
088500011227     C                   ENDSL
088600011228
088700011227     C                   ENDDO
088800070402     c
088900070402     C                   ENDDO
089000070402     C                   ENDSR
089100070125     c**********************************************************************
089200070316     c* carica sfl
089300070125     c**********************************************************************
089400070316     C     Ries01        BEGSR
089500070504     c* se ho richiesto le tassative e non ho impostato la causale
089600070504     c*  contatto--> la forzo
089700070504     c                   if        v1ctss='S' and v1ccau=' '
089800070504     c                   eval      v1ccau='S'
089900070504     c                   endif
090000070330     c* Pulizia SFL
090100070327     c                   setoff                                       21
090200070316     C                   WRITE     lrg1c01
090300070316     c                   seton                                        21
090400070125     C                   EVAL      snrr1   = 0
090500070316     C                   EVAL      nrr1    = 0
090600070330     c
090700070131      * gestione eventuali filiali in £6
090800070316    1c                   do        *hival        £6                3 0
090900070316    2c                   if        l6(£6)  = 0
091000070131     c                   leave
091100070316    2c                   endif
091200070330     c
091300070316     c     l6(£6)        setll     fnarb05l
091400070316    2c                   do        *hival
091500070316     c     l6(£6)        reade     fnarb05l
091600070316      *
091700070316    3c                   if        %eof(fnarb05l)
091800070316     c                   leave
091900070316    3c                   endif
092000070131
092100070131      * filtri
092200070131
092300070131      * bolle da escludere sempre
092400080418      * le spedizioni consegnate non le escludo perchè devo elaborare i
092500080418      *  ritorni all'incasso non abilitati dei C/ASS
092600070316    3c                   if        arbfbc = '*' or arbfbc = 'B' or
092700080418     c****                         arbfbc = 'G' or arbndc > 0 or arbdcm>0
092800080418     c                             arbfbc = 'G' or (arbndc > 0  and arbdcm=0)
092900070131     c                   iter
093000070316    3c                   endif
093100141030
093200141030     c* scarto se presente dispo di dirottamento
093300141030     c                   clear                   fnlry09ds2
093400141030     c                   eval      ilry09tch='C'
093500141030     c                   eval      ilry09tdis='D'
093600141030     c                   eval      ILRY09AAS=arbaas
093700141030     c                   eval      ILRY09LNP=arbLNP
093800141030     c                   eval      ILRY09NRS=arbNRS
093900141030     c                   eval      ILRY09NSP=arbNSP
094000141030     c                   eval      kpjbu=fnlry09ds2
094100141030     c                   call      'FNLRY09R'
094200141030     c                   parm                    kpjba
094300141030     c                   eval      fnlry09ds2=kpjbu
094400141030     c                   if        olry09esit='1'
094500141030     c                   iter
094600141030     c                   endif
094700070131
094800070201      * con C.A. aperta
094900070330     c***                clear                   fidn12ds
095000070330     c***                eval      i12aas = arbaas
095100070330     c***                eval      i12lnp = arblnp
095200070330     c***                eval      i12nrs = arbnrs
095300070330     c***                eval      i12nsp = arbnsp
095400070330     c***                eval      i12fel = 'E'
095500070330     c***                eval      i12fan = 'E'
095600070330     c***                eval      i12fch = 'E'
095700070330     c***                call      'FIDN12R'
095800070330     c***                parm                    fidn12ds
095900070201      **
096000070201      * se c'è almeno una C.A. aperta
096100070201      **
096200070330    3c***  o12nca        ifgt      0
096300070330     c***                iter
096400070330    3c***                end
096500070330     c
096600070522     c* scarto la spedizione se il codice giro richiesto è diverso
096700070522     c                   if        v1cgir<>*all'9'
096800070522     c     karg1         chain     fiarg01l
096900070522     C                   if        (%found(fiarg01l) and argcgi <> v1cgir)
097000070522     c                             or not %found(fiarg01l)
097100070522     c                   iter
097200070522     c                   endif
097300070522     c                   endif
097400070316      * verifico congurenza con causale contatto
097500070327     c                   z-add     arbaas        k_aas
097600070327     c                   z-add     arblnp        k_lnp
097700070327     c                   z-add     arbnrs        k_nrs
097800070327     c                   z-add     arbnsp        k_nsp
097900070316     c                   exsr      ctrspecau
098000070504     c
098100070504     c* Spedizione incongruente con la causale contatto immessa
098200070504    3c                   if        *in08
098300070316     c                   iter
098400070504     c                   else
098500070504     c* se non ho immesso la causale contatto e nessuna abilitazione
098600070504     c*  da fare, errore
098700070504    4c                   if        v1ccau=' ' and wblk=*blanks
098800070504     c                   iter
098900070504    4c                   endif
099000070504    3c                   endif
099100070330     c
099200070330      * parzializzazione per RSD richiesto range a video
099300070330    3c                   if        v1crsd <> *blank and v1crsa <> *blank
099400070330     c                             and (arbrsd < v1crsd or arbrsd > v1crsa)
099500070330     c                   iter
099600070330    3c                   endif
099700070330     c
099800070330     c* Parzializzazione in base all'abbinamento
099900070330     c                   clear                   wabb
100000070412     c                   clear                   wcallLV53         1
100100070330    3c                   if        v1cabb<>'N'
100200070411     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
100300070411    4C                   SELECT
100400070411     C     ARBAMA        WHENEQ    3
100500070411     C                   MOVEL     'S'           wABB
100600070411     C*
100700070411     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
100800070411    4C     ARBAMA        WHENEQ    1
100900070411     C     ARBAMP        ANDEQ     1
101000070411     C                   MOVEL     'S'           wABB
101100070411     C*
101200070411     C* 3) SE LOCALE SEMPRE ABBINATO
101300070411    4C                   WHEN      arbtfp=arbtfa or arblnp=arblna
101400070411     C                   MOVEL     'S'           wABB
101500070411     C*
101600070411   X4C                   OTHER
101700071127     c                   exsr      calllv53
101800070330     c*
101900070411    5c                   if        d53err=*blanks and d53nfa>0
102000070330     c                   eval      wabb='S'
102100070330     c                   else
102200070330     c                   eval      wabb='N'
102300070411    5c                   endif
102400070411    4c                   endsl
102500070330     c
102600070330     c* Solo bolle     abbinate
102700070330    4c                   if        v1cabb='S' and wabb<>'S'
102800070330     c                   iter
102900070330    4c                   endif
103000070330     c* Solo bolle non abbinate
103100070330    4c                   if        v1cabb='E' and wabb='S'
103200070330     c                   iter
103300070330    4c                   endif
103400070330    3c                   endif
103500071127     c* Parzializzazione in base alla data ora limite abbinamento
103600071127     c* (le bolle con abilitazione forzata e quelle locali le carico
103700071127     c* sempre a prescindere)
103800071127    3c                   if        *in12 and v1cdta>0 and arbama<>3 and
103900071127     c                             arbtfp<>arbtfa and arblnp<>arblna
104000071127    4c                   if        wcalllv53=' '
104100071127     c                   exsr      calllv53
104200071127    4c                   endif
104300071127    4c                   if        d53err=*blanks and d53nfa>0
104400071127     c                   movel     i_v1cdta      soglia
104500071127     c                   move      v1cora        soglia
104600071127     c                   movel     d53aed        ArrEffet
104700071127     c                   move      d53aeh        ArrEffet
104800071127    5c                   if        arreffet>soglia
104900071127     c                   iter
105000071127    5c                   endif
105100071127   x4c                   else
105200071127     c                   iter
105300071127    4c                   endif
105400071127    3c                   endif
105500070330
105600070330     c
105700070330     c* TIPO SPEDIZIONI -
105800070316     c* Verifico congruenza del blocco con la richiesta
105900070330     c*   del tipo spedizioni selezionato
106000070330    3c                   if        v1ctip<>*blanks
106100070504     c                   clear                   wpos
106200070330     c* Esito OK --> escludo
106300070330    4c                   select
106400070504    4c                   when      wblk =*blanks
106500070330     c                   iter
106600070330     c* solo le "Da contattare"
106700070504    4c****               when      v1ctip='D' and wblk<>'S'
106800070504    4c                   when      v1ctip='D'
106900070504     c                   eval      wpos=%scan('S':wblk)
107000070504     c                   if        wpos=0
107100070504     c                   iter
107200070504     c                   endif
107300070330     c* solo le "da Ricontattare"
107400070504    4c****               when      v1ctip='R' and wblk<>'T'
107500070504    4c                   when      v1ctip='R'
107600070504     c                   eval      wpos=%scan('T':wblk)
107700070504     c                   if        wpos=0
107800070504     c                   iter
107900070504     c                   endif
108000070330     c* solo le "Attesa giacenza"
108100070507    4c*****              when      v1ctip='E' and wblk<>'N'
108200070504    4c                   when      v1ctip='E'
108300070504     c                   eval      wpos=%scan('N':wblk)
108400070504     c                   if        wpos=0
108500070504     c                   iter
108600070504     c                   endif
108700070330     c* solo    "tutti contatti"
108800070504    4c***                when      v1ctip='T' and wblk='N'
108900070504    4c                   when      v1ctip='T'
109000070504     c                   eval      wpos=%scan('S':wblk)
109100070504     c                   if        wpos=0
109200070504     c                   eval      wpos=%scan('T':wblk)
109300070504     c                   if        wpos=0
109400070504     c                   iter
109500070504     c                   endif
109600070504     c                   endif
109700070504     c
109800070330    4c                   endsl
109900140210     c* se per la spedizione è previsto alert inviato/da inviare non
110000140211     c* la visualizzo nel sfl
110100140307     c                   select
110200140307     c                   when      v1ccau='L' or v1ccau='F'
110300140307     c                   eval      wcau=v1ccau
110400140211     c                   exsr      sr_callv82
110500140211     c                   if        olv82sts='0' or olv82sts='1'
110600140211     c                   iter
110700140211     c                   endif
110800140307     c                   when      v1ccau=*blanks
110900140307     c                   if        D§LRG6TCA=*blanks and
111000140307     c                             D§LRG6TSU=*blanks and
111100140307     c                             D§LRG6TAP=*blanks
111200140307     c                   select
111300140307     c* Solo lasciato avviso
111400140307     c                   when      D§LRG6tla<>*blanks and
111500140307     c                             D§LRG6Tfd=*blanks
111600140307     c                   eval      wcau='L'
111700140307     c                   exsr      sr_callv82
111800140307     c                   if        olv82sts='0' or olv82sts='1'
111900140307     c                   iter
112000140307     c                   endif
112100140307     c* Solo Fermo deposito
112200140307     c                   when      D§LRG6tfd<>*blanks and
112300140307     c                             D§LRG6Tla=*blanks
112400140307     c                   eval      wcau='F'
112500140307     c                   exsr      sr_callv82
112600140307     c                   if        olv82sts='0' or olv82sts='1'
112700140307     c                   iter
112800140307     c                   endif
112900140307     c* Sia lasciato avviso che fermo deposito
113000140307     c                   when      D§LRG6tfd<>*blanks and
113100140307     c                             D§LRG6Tla<>*blanks
113200140307     c* richiamo per l.avviso e se alert inviato o da inviare verifico anche
113300140307     c* il fermo deposito
113400140307     c                   eval      wcau='L'
113500140307     c                   exsr      sr_callv82
113600140307     c                   if        olv82sts='0' or olv82sts='1'
113700140307     c                   eval      wcau='F'
113800140307     c                   exsr      sr_callv82
113900140307     c* se entrambi inviati/da inviare non carico la spedizione nel sfl
114000140307     c                   if        olv82sts='0' or olv82sts='1'
114100140307     c                   iter
114200140307     c                   endif
114300140307     c                   endif
114400140307     c                   endsl
114500140307     c                   endif
114600140307     c                   endsl
114700140211     c
114800070330    3c                   endif
114900070330    c
115000070330     c* TASSATIVE
115100070330     c* escludo se blocco impostato (rientra nelle altre selezioni)
115200070330    3c                   if        v1ctss='S'
115300070330    4c                   if        wblk <> ' '
115400070330     c                   iter
115500070330    4c                   endif
115600070330     c
115700070504     c* Escludo se la data cons,richiesta non corrisponde a quella immes
115800070316    4c                   if        arbdcr<>I_v1cdcr
115900070316     c                   iter
116000070316    4c                   endif
116100070402     c
116200070402     c                   EXSR      CALLLV80
116300070402
116400070330     c
116500070330     c* Escludo se supermercato o appuntamento non tassativo
116600070316    4c                   if        olv80tat<>'S'
116700070316     c                   iter
116800070316    4c                   endif
116900070330     c
117000070412     c* se locale desumo la data di arrivo
117100070412    4c                   if        arbtfp=arbtfa
117200070412     c                   clear                   fnlv53ds
117300070412     c                   EXSR      BOLLALOC
117400070412   x4c                   else
117500070412     c
117600070330     c* Escludo se data cons.richiesta <> data arrivo BOLLA
117700070412    5c                   if        wcallLV53=' '
117800071127     c                   exsr      calllv53
117900070412    5c                   endif
118000070412    4c                   endif
118100070330     c*
118200070330    4c                   if        d53err<>*blanks or d53dfa<>I_v1cdcr
118300070412     c* Se la data è diversa, verifico se festiva, nel qual caso
118400070412     c*  determino la prima data superiore lavorativa e rifaccio il controll
118500070412    5c                   if        d53dfa>0
118600070412     c                   exsr      Festivo
118700070412    6c                   if        d53dfa<>i_v1cdcr
118800070412     c                   iter
118900070412    6c                   endif
119000070412     c
119100070412   x5c                   else
119200070412     c                   iter
119300070412    5c                   endif
119400070412     c
119500070330    4c                   endif
119600070330    3c                   endif
119700070330     c* ********************
119800070330     c* Carico record in SFL
119900070330     c* ********************
120000070316     C                   move      *blank        V1sopz
120100070316     c                   movel     arbrsd        v1srsd
120200071203     c                   movel     arbprd        v1sprd
120300070316     c                   z-add     arbaas        v1saas
120400080908     c**                 z-add     arblnp        v1slnp
120500080908     c**                 z-add     arbnrs        v1snrs
120600080908     c**                 z-add     arbnsp        v1snsp
120700080908     c                   z-add     arblnp        v1hlnp
120800080908     c                   z-add     arbnrs        v1hnrs
120900080908     c                   z-add     arbnsp        v1hnsp
121000080908     c* DDT
121100080908     c                   clear                   v1sddt
121200080908     c                   movel     'A'           Ktrck
121300080908     c     kar4          chain     fiar401l
121400080908     c                   if        %found(fiar401l)
121500080908     c                   movel     ar4not        dsbl4a
121600080908     c                   if        §b4abm='S' or §b4abm='C' or
121700080908     c                             §b4abm='P' or §b4abm='Q'
121800080909     c                   eval      v1sddt='SI DDT'
121900080908     c                   endif
122000080908     c                   endif
122100070330     c* Data spedizione
122200070330     c     *iso          movel     arbdsp        dataiso
122300070406     c                   movel     dataiso       datadmy
122400070406     c                   movel     datadmy       v1sdsp
122500070402     c                   clear                   v1stip_d
122600070330     c* Note bolla
122700070330     c                   clear                   v1snt1
122800070330     c                   clear                   v1snt2
122900070402     c                   movel     '8'           Ktrck
123000070330     c     kar4          chain     fiar401l
123100070330     c                   if        %found(fiar401l)
123200070330     c                   movel     ar4not        v1snt1
123300070402     c                   movel     '9'           Ktrck
123400070330     c     kar4          chain     fiar401l
123500070330     c                   if        %found(fiar401l)
123600070330     c                   movel     ar4not        v1snt2
123700070330     c                   endif
123800070330     c                   endif
123900070330     c* Telefono destinatario
124000070330     c     kar5          chain     fiar501l
124100070330     c                   if        %found(fiar501l)
124200070330     c                   movel     ar5uni        dar5gen
124300070330     c                   else
124400070330     c                   clear                   dar5gen
124500070330     c                   endif
124600070402     c
124700070504     c                   if        %subst(§ar5teld:1:1) = '#'
124800070402     c                   eval      v1steld=%subst(§ar5teld:2:15)
124900070402     c                   else
125000070402     c                   Eval      V1sTeld = §Ar5Teld
125100070402     c                   endif
125200070330     c
125300070508     c* Decodifica tipo contatto solo se richiesto  T-tutti contatti
125400070402     c                   clear                   v1stip_d
125500070330     c
125600070504    3c                   if        v1ctip='T'
125700070508     c                   clear                   wpos2
125800070508     c                   eval      wpos2=%scan('S':wblk)
125900070508     c                   if        wpos2>0
126000070330     c                   eval      v1stip_d='DaCont'
126100070508     c                   else
126200070330     c                   eval      v1stip_d='Ricont'
126300070504    3c                   endif
126400070508    3c                   endif
126500070504     c
126600070504     c                   exsr      CerABI
126700071120     c* visualizzo un asterisco se Spedizione da controllare per
126800071120     c* la messa in consegna automatica: si tratta dei seguenti casi:
126900071120     c*   Espresso + appuntamento
127000071120     c*   appuntamento con data cons.richiesta>0
127100071120     c*   spedizione da ricontattare con dcr>0
127200071120     c* Sono escluse le spedizioni dirette ai Supermercati
127300071120     c                   if        (arbtsp='E' and (arbtc1='A' or arbtc2='A'))
127400071120     c                             or (arbdcr>0 and (arbtc1='A' or arbtc2='A'))
127500071120     c                             or (arbdcr>0 and %scan('T':wblk)>0)
127600071120     c                   if        arbtc1<>'S' and arbtc2<>'S'
127700071120     c                   eval      v1sast='*'
127800071120     c                   endif
127900071120     c                   else
128000071120     c                   clear                   v1sast
128100071120     c                   endif
128200071120
128300070125      *esco per raggiunta massima capacità sfl
128400070316    3c                   if        snrr1 > 9990
128500070125     c                   leave
128600070316    3c                   endif
128700070125
128800070125     C                   EVAL      snrr1 = snrr1 + 1
128900070316     C                   WRITE     lrg1s01
129000070316    2C                   ENDDO
129100070316    1C                   enddo
129200070125
129300070316    1C                   IF        snrr1  > 0
129400070316     C                   EVAL      *IN20 = *ON
129500070125     C                   MOVE      1             OK                1 0
129600070125     C                   ELSE
129700070125     C                   MOVE      0             OK                1 0
129800070316    1C                   ENDIF
129900070125     C                   EVAL      nrr1  = 1
130000070316     C                   EVAL      lasnrr1  = snrr1
130100070316     C                   EVAL      snrr1  = 1
130200070125     C                   EVAL      csrrrn = 1
130300070402     c
130400070125     C                   ENDSR
130500070504     C**************************************************************************
130600070507     c* Visuliazzo le abilitazioni della bolla
130700070507     c*  escludo quella richeista in prima videata, se richeista
130800070504     C**************************************************************************
130900070504     c     CerABI        BEGSR
131000070504     c                   clear                   v1sabi
131100070504     c                   z-add     1             x
131200070504     c                   clear                   sk8
131300070507     c*
131400070507    1c                   if        v1ccau=' '
131500070504     c* Im wpos è contenuta la posizione della 1 abilitazione che
131600070504     c*  corrisponde al tipo spedizioni richiesto a video
131700070504     c*  poi di seguito visulizzo tutte le altre
131800070504     c                   if        wpos=1
131900070504     c                   movea     'Ca'          sk8(x)
132000070504     c                   add       2             x
132100070504     c                   clear                   d§lrg6tca
132200070504     c                   endif
132300070504     c                   if        wpos=2
132400070504     c                   movea     'Su'          sk8(x)
132500070504     c                   add       2             x
132600070504     c                   clear                   d§lrg6tsu
132700070504     c                   endif
132800070504     c                   if        wpos=3
132900070504     c                   movea     'Ap'          sk8(x)
133000070504     c                   add       2             x
133100070504     c                   clear                   d§lrg6tap
133200070504     c                   endif
133300070504     c                   if        wpos=4
133400070504     c                   movea     'Fd'          sk8(x)
133500070504     c                   add       2             x
133600070504     c                   clear                   d§lrg6tfd
133700070504     c                   endif
133800070504     c                   if        wpos=5
133900070504     c                   movea     'La'          sk8(x)
134000070504     c                   add       2             x
134100070504     c                   clear                   d§lrg6tla
134200070504     c                   endif
134300070507     c
134400070507    1c                   endif
134500070504     c                   clear                   wpos
134600070504     c
134700070504     c* C/Ass
134800070507     c                   if        d§lrg6tca<>' ' and v1ccau<>'C'
134900070504     c                   movea     'Ca'          sk8(x)
135000070504     c                   add       2             x
135100070504     c                   endif
135200070504     c* Supermercati
135300070507     c                   if        d§lrg6tsu<>' ' and v1ccau<>'S'
135400070504     c                   movea     'Su'          sk8(x)
135500070504     c                   add       2             x
135600070504     c                   endif
135700070504     c* Appuntamento
135800070507     c                   if        d§lrg6tap<>' ' and v1ccau<>'A'
135900070504     c                   movea     'Ap'          sk8(x)
136000070504     c                   add       2             x
136100070504     c                   endif
136200070504     c* Fermo Deposito
136300070507     c                   if        d§lrg6tfd<>' ' and v1ccau<>'F'
136400070504     c                   movea     'Fd'          sk8(x)
136500070504     c                   add       2             x
136600070504     c                   endif
136700070504     c* Lasciato Avviso
136800070507     c                   if        d§lrg6tla<>' ' and v1ccau<>'L'
136900070504     c                   movea     'La'          sk8(x)
137000070504     c                   add       2             x
137100070504     c                   endif
137200070504     c
137300070504     c                   movea     sk8           v1sabi
137400070504     c                   ENDSR
137500070412     C**************************************************************************
137600070412     C* Per bolla locale desumo la data dell'arrivo bolla
137700070412     C**************************************************************************
137800070412     c     BOLLALOC      begsr
137900070412     c     kfvv8         SETGT     FNFVV08L
138000070412     c     kfvv81        READe     FNFVV08L
138100070412     C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=ARBDBR
138200070412     C                   EVAL      d53dfa =FVVDFV
138300070412     C*
138400070412     C                   ELSE
138500070412     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA BORDERO + 1
138600070412     c     *iso          movel     arbDBR        dataiso
138700070412     c                   adddur    1:*d          dataiso
138800070412     c                   movel     dataiso       d53dfa
138900070412     C                   ENDIF
139000070412     c                   ENDSR
139100070412     C**************************************************************************
139200070412     C* Determino foglio arrivi in data festiva
139300070412     C**************************************************************************
139400070412     c     FESTIVO       begsr
139500070412     c                   setoff                                       34
139600070412     c     *iso          move      d53dfa        dataiso
139700070412     c
139800070412    6c                   dow       not *in34
139900070412     c* controllo se e' festivo
140000070412     C     *iso          move      dataiso       kdata
140100070412     C                   Z-ADD     68            KTFp
140200070412     C                   clear                   KTFa
140300070412     C     KCLN          CHAIN     AZCLN01L                           34
140400070412    7C     *IN34         IFEQ      *OFF
140500070412     C     MAT(KGG)      andne     ' '
140600070412     C     *IN34         oreq      *OFF
140700070412     C     POM(KGG)      andne     ' '
140800070412     c**
140900070412     c                   adddur    1:*D          dataiso
141000070412     c                   else
141100070412     c                   seton                                        34
141200070412    7c                   endif
141300070412    6c                   enddo
141400070412     c
141500070412     c     *iso          movel     dataiso       d53dfa
141600070412     c                   ENDSR
141700070402     c**********************************************************************
141800070402     c* Verifico se si tratta di tassativa
141900070402     c**********************************************************************
142000070402     c     CALLLV80      begsr
142100070402     c                   clear                   fnlv80ds
142200070402     c                   eval      ilv80aas=arbaas
142300070402     c                   eval      ilv80lnp=arblnp
142400070402     c                   eval      ilv80nrs=arbnrs
142500070402     c                   eval      ilv80nsp=arbnsp
142600070402     c                   eval      ilv80mgs=arbmgs
142700070402     c                   eval      ilv80tcr=arbtcr
142800070402     c                   eval      ilv80dcr=arbdcr
142900070402     c                   eval      ilv80hcr=arbhcr
143000070402     c                   eval      ilv80tc1=arbtc1
143100070402     c                   eval      ilv80tc2=arbtc2
143200070402     c                   call      'FNLV80R'
143300070402     c                   parm                    kpjba
143400070402     c                   parm                    fnlv80ds
143500070402     c                   parm      'S'           ilv80nwproc       1
143600070402     c                   endsr
143700070321     c**********************************************************************
143800070321     C     Gestionesfl   BEGSR
143900070321     c**********************************************************************
144000070321      * Elaborazione opzioni.
144100070321     c*
144200070321     c                   readc     lrg1s01
144300070402     c
144400070321     c                   dow       %eof=*oFF
144500070321     C                   IF        v1sopz <> *blanks
144600070321     c                   exsr      chiamo
144700070321     c                   move      *blank        v1sopz
144800070402     c
144900070402     c* Se non premuto F12-proteggo opzione
145000070507     c                   if        wf6='S'
145100070402     c                   seton                                        10
145200070402     c                   endif
145300070402     c
145400070321     c                   update    lrg1s01
145500070402     c                   setoff                                       10
145600070321     C                   ENDIF
145700070402     c
145800070321     c                   readc(E)  lrg1s01
145900070321     C                   enddo
146000070321     C                   ENDSR
146100070321     c**********************************************************************
146200070321     C     chiamo        BEGSR
146300070321     c**********************************************************************
146400070321     c                   clear                   fnlrg2ds
146500070430     c                   clear                   fnlrg6ds
146600070507     c                   clear                   wf6               1
146700070321     c                   if        v1cnsp>0
146800070321     c                   eval      ilrg2aas=v1caas
146900070321     c                   eval      ilrg2lnp=v1clnp
147000070321     c                   eval      ilrg2nrs=v1cnrs
147100070321     c                   eval      ilrg2nsp=v1cnsp
147200070402     c                   eval      ilrg2tss=olv80tat
147300070321     c                   else
147400070321     c                   eval      ilrg2aas=v1saas
147500080908     c                   eval      ilrg2lnp=v1hlnp
147600080908     c                   eval      ilrg2nrs=v1hnrs
147700080908     c                   eval      ilrg2nsp=v1hnsp
147800070321     c                   eval      ilrg2tss=v1ctss
147900070321     c                   endif
148000070504     c* Se richiesto causale contatto
148100070504     c                   if        v1ccau<>' '
148200070321     c                   eval      ilrg2cau=v1ccau
148300070504     c                   else
148400070504     c                   movel     v1sabi        ilrg2cau
148500070504     c                   endif
148600070504     c
148700070430     c                   clear                   wprima_v
148800070430     c                   clear                   xx
148900070430     c                   do        *hival
149000070430     c                   if        ilrg2cau=' '
149100070430     c                   leave
149200070430     c                   endif
149300070321     c                   call      'FNLRG2R'
149400070321     c                   parm                    kpjba
149500070321     c                   parm                    fnlrg2ds
149600070507     c                   if        olrg2f12=*blanks
149700070507     c                   eval      wf6='S'
149800070430     c                   endif
149900070502     c* verifico se presenti altre causali per le quali
150000070430     c* bisogna gestire il contatto
150100070502     c                   if        ilrg2tss='S'
150200070430     c                   leave
150300070430     c                   else
150400070430     c                   z-add     ilrg2aas      ILRG6AAS
150500070430     c                   z-add     ilrg2lnp      ILRG6LNP
150600070430     c                   z-add     ilrg2nrs      ILRG6NRS
150700070430     c                   z-add     ilrg2nsp      ILRG6NSP
150800070430     c                   add       1             xx
150900070430     c                   move      ilrg2cau      blov(xx)
151000070430     c                   call      'FNLRG6R'
151100070430     c                   parm                    fnlrg6ds
151200070430     c                   movel     olrg6abi      dlrg6abi
151300070504     c
151400070430     c                   select
151500070502     c                   when      d§lrg6tca<>' '
151600070430     c                   eval      ilrg2cau='C'
151700070504     c
151800070504     c                   when      d§lrg6tap<>' '
151900070430     c                   eval      ilrg2cau='A'
152000070504     c
152100070504     c                   when      d§lrg6tsu<>' '
152200070430     c                   eval      ilrg2cau='S'
152300070504     c
152400070502     c                   when      d§lrg6tfd<>' '
152500070430     c                   eval      ilrg2cau='F'
152600070504     c
152700070502     c                   when      d§lrg6tla<>' '
152800070430     c                   eval      ilrg2cau='L'
152900070504     c
153000070430     c                   other
153100070430     c                   clear                   ilrg2cau
153200070430     c                   endsl
153300070430     c                   endif
153400070430     c                   enddo
153500070321     C                   ENDSR
153600071127     c**********************************************************************
153700071127     C     calllv53      BEGSR
153800071127     c**********************************************************************
153900071127      *
154000071127     c                   clear                   fnlv53ds
154100071127     C                   MOVEL     'A'           D53TFO
154200071127     C                   MOVE      arbNFV        D53NFV
154300071127     C                   MOVE      arbtfp        D53FGS
154400071127     C                   MOVEL     'B'           D53ABB
154500071127     C                   MOVE      ARBLNA        D53LNA
154600071127     C                   MOVE      ARBTFA        D53TFA
154700071127     C                   CALL      'FNLV53R'
154800071127     C                   PARM                    fnlv53ds
154900071127     c                   eval      wcalllv53='S'
155000071127     C
155100071127     C                   ENDSR
155200011227     c**********************************************************************
155300000607     C     *INZSR        BEGSR
155400011227     c**********************************************************************
155500000607      *
155600000607     C     *ENTRY        PLIST
155700000607     C                   PARM                    Kpjba
155800070123      *
155900070315     c     *dtaara       define    §azute        azuteds
156000070315     c     *dtaara       define    §datiute      ddatiute
156100070315     c                   in(e)     *dtaara
156200070315     c                   if        %error  or rsut = *blanks
156300070315     c                   clear                   tibs34ds
156400070315     c                   call      'TIBS34R'
156500070315     c                   parm                    tibs34ds
156600070315     c                   in        *dtaara
156700070315     c                   endif
156800030702      *reperimento data
156900070319     C                   TIME                    W0140            14 0
157000070319     C                   MOVE      W0140         WDAT              8 0
157100030702     C*
157200030702     C                   Z-ADD     WDAT          G02DAT
157300030702     C                   MOVEL     *BLANK        G02ERR
157400030702     C                   CALL      'XSRDA8'
157500030702     C                   PARM                    WLBDA8
157600030702     C* UDATE A 8 IN AAAA/MM/GG
157700030717     C                   Z-ADD     G02INV        WUDAte            8 0
157800000607      *
157900170620        clear TRUL31DS;
158000170620        IF  DUTlpo = '2';
158100170620          I31abi = 'PO';
158200170620        ELSE;
158300170620          I31abi = 'RA';
158400170620        ENDIF;
158500170620        I31cdi = DUTdis;
158600170620        I31car = DUTare;
158700170620        I31cpo = DUTpou;
158800170620        trul31r (kpjba:trul31ds);
158900170620        IF  O31pog <= *zeros;
159000170620          clear POG;
159100170620        ENDIF;
159200070123      *
159300070124     C     Karb          klist
159400070327     C                   kfld                    K_aas
159500070327     C                   kfld                    K_lnp
159600070327     C                   kfld                    K_nrs
159700070327     C                   kfld                    K_nsp
159800070125      *
159900070201      *
160000070201     C     Kar4          klist
160100070201     C                   kfld                    arbaas
160200070201     C                   kfld                    arblnp
160300070201     C                   kfld                    arbnrs
160400070201     C                   kfld                    arbnsp
160500070330     c                   kfld                    ktrck
160600070330     C     Kar5          klist
160700070330     C                   kfld                    arbaas
160800070330     C                   kfld                    arblnp
160900070330     C                   kfld                    arbnrs
161000070330     C                   kfld                    arbnsp
161100070330     c                   kfld                    ktrd
161200070201      *
161300070201     C     Ktab          klist
161400070201     C                   kfld                    tblkut
161500070201     C                   kfld                    tblcod
161600070201     C                   kfld                    tblkey
161700070412     C     Kfvv8         KLIST
161800070412     C                   KFLD                    arbtfa
161900070412     C                   KFLD                    knpg
162000070412     C                   KFLD                    arbdbr
162100070412     C     Kfvv81        KLIST
162200070412     C                   KFLD                    arbtfa
162300070412     C                   KFLD                    knpg
162400070412     C     KCLN          KLIST
162500070412     C                   KFLD                    KTFP
162600070412     C                   KFLD                    KTFA
162700070412     C                   KFLD                    KAAA              4 0
162800070412     C                   KFLD                    KMMM              2 0
162900070522     c*
163000070522     C     Karg1         klist
163100070522     C                   kfld                    arbaas
163200070522     C                   kfld                    arblnp
163300070522     C                   kfld                    arbnrs
163400070522     C                   kfld                    arbnsp
163500070412     c
163600070201     C                   z-add     1             tblkut
163700070205      * carico filiali £1
163800070205     C                   clear                   trul06ds
163900070205     C                   movel     '£1'          d06cod
164000070320     C                   movel     simfel        d06key
164100070205     C                   movel     trul06ds      kpjbu
164200070205     C                   CALL      'TRUL06R'
164300070205     C                   parm                    kpjba
164400070315     c                   movel     kpjbu         trul06ds
164500070315     C                   movea     lin           l1
164600070320     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
164700070320     C                   CLEAR                   trul06ds
164800070320     C                   MOVE      '£6'          D06COD
164900070320     c                   eval      d06esc = 'G'
165000070320     c                   movel     simfel        d06key
165100070320     C                   MOVEL     trul06ds      KPJBU
165200070320     C                   CALL      'TRUL06R'
165300070320     C                   PARM                    KPJBA
165400070320     C                   MOVEL     KPJBU         trul06ds
165500070320     C                   MOVEl     LIN           nog
165600030714
165700011227     C                   ENDSR
165800070125      *--------------------------------------------------------------------------------------------*
165900070125      * ordina - riscrive il sfl ordinato per località
166000070125      *--------------------------------------------------------------------------------------------*
166100070125     C     ordina        BEGSR
166200070125     C                   setoff                                       72
166300070125      * IMPOSTO ENTRATA AD 'A'
166400070125     C                   EVAL      snrr1 = 0
166500070125      *
166600070125     C                   CLEAR                   QLGSCB
166700070125     C                   CLEAR                   QLGSCB00
166800070125
166900071203     ** 2 campi chiave.
167000071203     C                   EVAL      QLGNBRK    = 2
167100071204     c                   select
167200071204     c* ordinamento per prv/rag.sociale destinatario
167300071204     c                   when      w_ord=1
167400071204     ** provincia dopo rag. sociale destinatario 2 byte,
167500071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
167600071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
167700070125     C                   EVAL      QLGDT      = carattere
167800070125     C                   EVAL      QLGSO      = Ascendente
167900070125     C                   EVAL      QLGKL(1)   = QLGSKL
168000071204     ** poi ragione sociale destinatario in pos 1, 22 byte
168100071204     C                   EVAL      QLGSP      = 1
168200071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
168300071203     C                   EVAL      QLGDT      = carattere
168400071203     C                   EVAL      QLGSO      = Ascendente
168500071203     C                   EVAL      QLGKL(2)   = QLGSKL
168600071204     c* ordinamento per rag.sociale/prv destinatario
168700071204     c                   when      w_ord=2
168800071204     C                   EVAL      QLGSP      = 1
168900071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
169000071204     C                   EVAL      QLGDT      = carattere
169100071204     C                   EVAL      QLGSO      = Ascendente
169200071204     C                   EVAL      QLGKL(1)   = QLGSKL
169300071204     ** ragione sociale a posizione 1, carattere, 24 byte,
169400071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
169500071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
169600071204     C                   EVAL      QLGDT      = carattere
169700071204     C                   EVAL      QLGSO      = Ascendente
169800071204     C                   EVAL      QLGKL(2)   = QLGSKL
169900071204     c                   endsl
170000070125
170100070125      * Load other sort parameters.
170200070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
170300070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
170400070125     C                   EVAL      QLGRT     = 8
170500070125     C                   EVAL      QLGOKL    = 80
170600070125     C                   EVAL      QLGLKE    = 16
170700070125     C                   EVAL      QLGLSS    = 290
170800070125
170900070125      * Initialize Sort I/O API fields.
171000070125     C                   EVAL      QLGRL00  = QLGRL
171100070125     C                   EVAL      QLGRC00  = 1
171200070125     C                   CLEAR                   QUSEI
171300070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
171400070125
171500070125      * First step - Initialize the sort routine.
171600070125     C                   CALL      'QLGSORT'
171700070125     C                   PARM                    QLGSCB
171800070125     C                   PARM                    NotUsed
171900070125     C                   PARM                    NotUsed
172000070125     C                   PARM                    SizeList
172100070125     C                   PARM                    ReturnSize
172200070125     C                   PARM                    QUSEC
172300070125
172400070125      * Next step - Write records to I/O routine.
172500070125     C                   EVAL      QLGRT00 = Put
172600070125
172700070125     C                   DO        *hival        cx                4 0
172800070125
172900070316     C     cx            CHAIN     lrg1s01                            50
173000070125     c   50              leave
173100070125     ** Solo le righe con Selected = 'Y' sono riordinate,
173200070125     ** quindi per fare un ordinamento di tutte le righe
173300070125     ** metto 'Y' sempre.
173400070125     C                   EVAL      Selected = 'Y'
173500070125
173600070125     C                   CLEAR                   QUSEI
173700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
173800070125     C                   CALL      'QLGSRTIO'
173900070125     C                   PARM                    QLGSCB00
174000070125     C                   PARM                    SflRcd
174100070125     C                   PARM                    NotUsed
174200070125     C                   PARM                    SizeList
174300070125     C                   PARM                    NotUsed
174400070125     C                   PARM                    QUSEC
174500070125
174600070125     C                   ENDDO
174700070125
174800070125      * Next step - Signal end of input, clear subfile for reload.
174900070125     C                   EVAL      QLGRT00 = EndPut
175000070125     C                   CLEAR                   QUSEI
175100070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
175200070125
175300070125     C                   CALL      'QLGSRTIO'
175400070125     C                   PARM                    QLGSCB00
175500070125     C                   PARM                    SflRcd
175600070125     C                   PARM                    NotUsed
175700070125     C                   PARM                    SizeList
175800070125     C                   PARM                    NotUsed
175900070125     C                   PARM                    QUSEC
176000070125
176100070316     C                   SETOff                                       21        *
176200070316     C                   WRITE     lrg1c01                                      *
176300070316     C                   SETOn                                        21        *
176400070316     C                   WRITE     lrg1z01                                      *
176500070125
176600070125      * Final step - Write the records back to the subfile.
176700070125     C                   EVAL      QLGRT00 = Get
176800070316     C                   DO        lasnrr1       x                 4 0
176900070125     C                   CLEAR                   QUSEI
177000070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
177100070125     C                   CALL      'QLGSRTIO'
177200070125     C                   PARM                    QLGSCB00
177300070125     C                   PARM                    NotUsed
177400070125     C                   PARM                    SflRcd
177500070125     C                   PARM                    QLGRL00
177600070125     C                   PARM                    NotUsed
177700070125     C                   PARM                    QUSEC
177800070125      *
177900070125     c                   z-add     x             snrr1
178000070316     C                   WRITE     lrg1s01
178100070125     C                   ENDDO
178200070125
178300070125      *
178400070125     C                   ENDSR
178500140211      *--------------------------------------------------------------------------------------------*
178600140211      * Richiamo driver per verificare eventuale stato stato alert
178700140211      *--------------------------------------------------------------------------------------------*
178800140211     C     sr_callv82    BEGSR
178900140211     c                   clear                   fnlv82ds
179000140211     c                   eval      ILV82AAS=arbaas
179100140211     c                   eval      ILV82LNP=arblnp
179200140211     c                   eval      ILV82NRS=arbnrs
179300140211     c                   eval      ILV82NSP=arbnsp
179400140211     c                   select
179500140307     c                   when      wcau='L'
179600140211     c                   eval      ILV82TIPA='RIC'
179700140307     c                   when      wcau='F'
179800140211     c                   eval      ILV82TIPA='FD'
179900140211     c                   endsl
180000140211     c                   call      'FNLV82R'
180100140211     c                   parm                    fnlv82ds
180200140211     c                   endsr
180300070315**
180400070403           Da CONTATTARE
180500070403          Da RICONTATTARE
180600070403ESCLUSA GESTIONE: Attesa GIACENZA
180700070403      CONTATTI e RICONTATTI
180800070330**
180900070319Filiale in gestione errata
181000070316Causale contatto obbligatoria
181100070402Selezionare per Numero Spedizione OPPURE altre parzializzazioni
181200070402Spedizione inesistente
181300070316Parzializzazione per ragione sociale errata: inserire correttamente
181400070316Tipo Spedizioni obbligatorio
181500070503Gestione "Tassative" valida solo per spedizioni per Supermercato               07
181600070403La spedizione non prevede abilitazione in base alla causale contatto immessa
181700070319Data consegna richiesta mancante o errata
181800070319Selezionare per "Tipo Spedizioni" OPPURE per "Tassative"
181900070319Filiale in gestione non abilitata alla Gestione Contatti Destinatario
182000070402Nessun contatto da gestire per la selezione effettuata
182100070402Spedizione non gestibile: bloccata con blocco " "
182200070402Spedizione non gestibile: consegnata o in distinta                             14
182300070402Spedizione non gestibile: linea di arrivo non in gestione                      15
182400070403Spedizione già abilitata in base alla causale contatto immessa                 16
182500070402Impossibile selezionare "Appuntamenti" in "Attesa di Giacenza"                 17
182600070403Spedizione già abilitata :F13 per ribloccare la spedizione                     18
182700070503Spedizione già abilitata                                                       19
182800070503Per riabilitare una spedizione con F13, specificare causale contatto           20
182900070504La spedizione non prevede abilitazioni                                         21
183000071126Per selezionare per data limite abbin. richiedere solo le sped.di f.v.abbinati 22
183100071126Data errata                                                                    23
183200071126Ora errata                                                                     24
183300071128Immettere anche la data oppure lasciare vuota l'ora                            25
