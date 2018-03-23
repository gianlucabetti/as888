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
002801140211     D fnlv82ds      E DS
002900070330     D fnlv53ds      E DS
003000070201     D fidn12ds      E DS
003100070315     d azuteds       e ds                  extname(azute00f)
003200070315     d ddatiute      e ds
003300070315     d tibs34ds      e ds                  inz
003400070131     D trul06ds      E DS
003500070131     D  lin                    1     90  0 dim(30)
003501141030     D fnlry09ds2    E DS
003600070206     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
003700070206     D PARAM1          DS                  INZ
003800070206     D  PA1AAS                14     17  0
003900070206     D  PA1LNP                18     20  0
004000070206     D  PA1NRS                21     22  0
004100070206     D  PA1NSP                23     29  0
004200070206     D  PA1F03                30     30
004300070206     D  PA1FLG                31     31
004400070206     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004500070206     D  PA1GIA               144    144
004600070330     d
004700070330     D                 DS
004800070330     D  ARBAAS                 1      4  0
004900070330     D  ARBMGS                 5      8  0
005000070330     D  ARBDSP                 1      8  0
005100070412     D                 DS
005200070412     D  kaaa                   1      4  0
005300070412     D  kmmm                   5      6  0
005400070412     D  kgg                    7      8  0
005500070412     D  kDATA                  1      8  0
005600070412     D clnmat          DS
005700070412     D  MAT                    1     31    dim(31)
005800070412     D clnpom          DS
005900070412     D  pom                    1     31    dim(31)
006000070330     d
006100070201     d ds3a          e ds
006200070316     d dsbl4l        e ds
006201080908     d dsbl4a        e ds
006300070330     d dar5gen       e ds
006400070319     d og148         e ds
006500071120     d og147         e ds
006600011228      *
006700070412     D KTFA            S                   LIKE(clntfa)
006800070412     D KTFP            S                   LIKE(clntfp)
006900070316     D wtit2           S                   like(v1ctit2)
007000070330     D KtrcK           S                   like(ar4trc)
007100070330     D Ktrd            S                   like(ar5trd) inz('GEN')
007200070327     d K_aas           s                   like(arbaas)
007300070327     d K_lnp           s                   like(arblnp)
007400070327     d K_nrs           s                   like(arbnrs)
007500070327     d K_nsp           s                   like(arbnsp)
007600070412     D knpg            S                   LIKE(fvvnpg) inz(2)
007700070430     D s_olrg2f12      S                   LIKE(olrg2f12)
007800070430     D wprima_v        S              1
007900030604     D snrr1           S              5i 0
008000070129     D lasnrr1         S              5i 0
008100070316     D I_v1cdcr        S                   like(v1cdcr)
008200071126     D I_v1cdta        S                   like(v1cdta)
008300030613     D WLBDA8          DS
008400030613     D  G02DAT                 1      8  0
008500030613     D  G02INV                 9     16  0
008600030613     D  G02ERR                17     17
008700030613     D  G02TGI                18     22  0
008800070319
008900070315     d  l6             s              3  0 dim(30)
009000070315     d  l1             s              3  0 dim(30)
009100070320     D nog             S              3  0 DIM(30)                              FIL.GESTITE
009200070330     d Dataiso         s               d   datfmt(*iso)
009300070406     d Datadmy         s               d   datfmt(*dmy)
009400070402     D wabb            S              1                                         FIL.GESTITE
009500070504     D wpos            S              2s 0
009600070508     D wpos2           S              2s 0
009700070504     D xx              S              1  0
009800071127     D soglia          S             12  0
009900071127     D arreffet        S             12  0
010000071128     D v1cdtaa         S              4s 0
010100070316      *---------------------------------------------------------------------
010200070321     D coscas          C                   CONST('           Contrasse-
010300070321     D                                     gno           ')
010400070321     D cosapp          C                   CONST('           Appuntame-
010500070321     D                                     nto           ')
010600070321     D cossup          C                   CONST('           Supermerc-
010700070321     D                                     ato           ')
010800070321     D cosffd          C                   CONST('         Fermo Depos-
010900070321     D                                     ito           ')
011000070321     D coslav          C                   CONST('          Lasciato A-
011100070321     D                                     vviso         ')
011200070321     D cossuptas       C                   CONST('      Supermercato:t-
011300070321     D                                     assativo      ')
011400070321     D cosapptas       C                   CONST('      Appuntamento:t-
011500070321     D                                     assativo      ')
011600070403     D TIPDEc          S             33    DIM(4) CTDATA PERRCD(1)
011700071126     D msg             S             78    DIM(25) CTDATA PERRCD(1)
011800071128     d weditora        c                   '  :  '
011900071128     d weditdta        c                   '  /  '
012000071204     d ord_ragprv      c                   'F11=Ord.xRagSoc/Prv'
012100071204     d ord_prvrag      c                   'F11=Ord.xPrv/RagSoc'
012200071204     d w_prvrsoc       C                   1
012300071204     d w_rsocprv       C                   2
012400071204     d W_ord           S              1  0
012401140307     d Wcau            S                   like(v1ccau)
012500070125      *---------------------------------------------------------------------
012600070125      * specifiche per ordinamento forzato sfl
012700070125     d NotUsed         S             16A
012800070125     d ReturnSize      S              9B 0
012900070125     d SizeList        S              9B 0
013000070125     d*---
013100070125     d MaxKey          C                   2
013200070125     d Ascendente      C                   1
013300070125     d Discendente     C                   2
013400070125     d Carattere       C                   6
013500070125     d Put             C                   1
013600070125     d EndPut          C                   2
013700070125     d Get             C                   3
013800070125     d Numerico        C                   8
013900070125     d*---
014000070125     d SflRcd          DS
014100070316     d  v1srsd
014200071203     d  v1sprd
014300070504     d  V1steld
014400070316     d  V1sAAS
014500080908     d**V1sLNP
014600080908     d**V1sNRS
014700080908     d**V1sNSP
014701080908     d  V1hLNP
014702080908     d  V1hNRS
014703080908     d  V1hNSP
014704080908     d  V1sddt
014800070330     d  V1sdsp
014900070504     d  V1sabi
015000071031     d  V1sast
015100070402     d  V1snt1
015200070330     d  V1snt2
015300070504     d  V1stIp_d
015400070125     d  Selected                      1A
015500070125
015600070125      /COPY QSYSINC/QRPGLESRC,QLGSORT
015700070125     d QLGKL                         16    DIM(MaxKey)
015800070125     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
015900070125     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
016000070125     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
016100070125     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
016200070125
016300070125      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
016400070125      /COPY QSYSINC/QRPGLESRC,QUSEC
016500070315      *---------------- Gestione prima videata -----------------------------
016600070502     c                   exsr      inzfgs
016700070315     C                   EXSR      Inzd01
016800000608      *
016900071119     c                   do        *hival
017000070315     c                   clear                   v1ctit2
017100070402     c                   clear                   v1ctipdec
017200070522     c                   clear                   v1cgircos
017300070522     c                   clear                   v1cgirt
017400071128     c                   clear                   v1clim
017500070315     c                   write     lrg1t01
017600070315     C                   EXFMT     lrg1d01
017700070315     c                   setoff                                       9028
017800070315     c                   clear                   vidmsg
017900070315      * F3=Fine
018000070315     C                   if        *INKC
018100070315     C                   leave
018200070315     c                   endif
018300070319      * F18=cambio filiale - (Sproteggo cambio p.o gestione)
018400070315     c                   if        *inks
018500070315     c                   seton                                        1640
018600070315     c                   iter
018700070315     c                   endif
018800070315      * F7=Interrogazione bolle arrivi
018900070315     c                   if        *inkg
019000070315     c                   clear                   kpjbu
019100070315     c                   clear                   param1
019200070402     c                   movel     '2'           pa1flg
019300070315     c                   movel     param1        kpjbu
019400070315     c                   call      'FNLR36R'
019500070315     c                   parm                    kpjba
019600070402     C                   MOVEL     KPJBU         PARAM1
019700070402     C*
019800070402     C* SE PASSATA KEY LA IMPOSTO
019900070402     C     pa1nsp        IFGT      *ZEROS
020000070402     C                   MOVEL     pa1AAS        V1cAAS
020100070402     C                   MOVEL     pa1LNP        V1cLNP
020200070402     C                   MOVEL     pa1NRS        V1cNRS
020300070402     C                   MOVEL     pa1NSP        V1cNSP
020400070402     c                   endif
020500070315     c                   iter
020600070315     c                   endif
020700070522     c* F11=Interrogazione Giri
020800070522     c                   if        *inkk
020900070522     c                   clear                   fidg09ds
021000070522     c                   move      'P01'         d09iop0
021100070522     c                   movel     v1cfgs        d09ifgs
021200070522     c                   z-add     wudate        d09idat
021300070706     c                   eval      d09itug = 'C'
021400070522     c                   movel     fidg09ds      kpjbu
021500070522     c                   call      'FIDG09R'
021600070522     c                   parm                    kpjba
021700070522     c                   movel     kpjbu         fidg09ds
021800070522     c                   movel     d09ocgi       v1cgir
021900070522     c                   iter
022000070522     c                   endif
022100070402     c
022200070315      * controllo filiale in gestione
022300070315     c                   if        *in16
022400070315     c                   exsr      ctrfgs
022500070315     c   28              iter
022600070315     c                   endif
022700070319      * controlli videata
022800070315     C                   EXSR      Ctrd01
022900070315     c   90              iter
023000070316     c* se selezione per ragione sociale effettuo caricamento subfile
023100070316     c                   if        v1cnsp=0
023200070316     C                   EXSR      ges_s01
023300070402     c* Se esco con F3 - esco dal pgm
023400070402     c                   if        *inkc
023500070402     c                   leave
023600070402     c                   else
023700070402     c                   if        not *in28
023800070402     c                   exsr      INZd01
023900070402     c                   endif
024000070402     c
024100070315     c                   iter
024200070402     c                   endif
024300070402     c
024400070316     c                   else
024500070504     c* anche per spedizione, se non impostata la causale contatto
024600070504     c*  imposto il campo v1stip per il richiamo a fnlrg2r
024700070504     c                   clear                   wpos
024800070504     c                   exsr      CerABI
024900070504     c
025000070316     c* richiamo pgm gestione singola spedizione
025100070321     c                   exsr      chiamo
025200070402     c                   exsr      INZd01
025300070316     c                   iter
025400070316     c                   endif
025500011227      *
025600011227     C                   ENDDO
025700070321      * Chiusura con lr
025800070321     c                   eval      ilrg2tla='C'
025900070321     c                   call      'FNLRG2R'
026000070321     c                   parm                    kpjba
026100070321     c                   parm                    fnlrg2ds
026200070430     c*
026300070430     c                   eval      ilrg6tla='C'
026400070430     c                   call      'FNLRG6R'
026500070430     c                   parm                    fnlrg6ds
026501140211
026502140211     c                   eval      ilv82tla='C'
026503140211     c                   call      'FNLV82R'
026504140211     c                   parm                    fnlv82ds
026505141030
026506141030     c                   eval      ilry09tla= 'C'
026507141030     c                   eval      kpjbu=fnlry09ds2
026508141030     c                   call      'FNLRY09R'
026509141030     c                   parm                    kpjba
026600011227      *
026700070315     C                   seton                                        lr
026800070315     c**********************************************************************
026900070315     c* inizializza d01
027000070315     c**********************************************************************
027100070315     C     Inzd01        BEGSR
027200070403     c                   setoff                                       18
027300070315
027400070315     c                   clear                   v1ccau
027500070315     c                   clear                   v1crsd
027600070315     c                   clear                   v1crsa
027700070402     c                   eval      v1ctip='D'
027800070315     c                   clear                   v1ctss
027900070316     c                   clear                   v1cdcr
028000070319     c                   clear                   I_v1cdcr
028100070315     c                   clear                   v1clnp
028200070315     c                   clear                   v1cnrs
028300070315     c                   clear                   v1cnsp
028400070330     c                   movel     'S'           v1cabb
028500070319     c                   move      wdat          v1caas
028600070507     c                   clear                   wtit2
028700070521     c                   eval      v1cgir=*all'9'
028800071120     c                   clear                   v1cdta
028900071126     c                   clear                   I_v1cdta
029000071120     c                   clear                   v1cora
029100070315
029200070315     C                   ENDSR
029300070502     c**********************************************************************
029400070502     c* inizializza p.o. in gestione
029500070502     c**********************************************************************
029600070502     c     inzfgs        begsr
029700070502      * imposto il p.o. in gestione
029800070502     c                   eval      v1cfgs = dutpou
029900070502     c     v1cfgs        chain     azorg01L
030000070502     c                   if        %found(azorg01l) and orgfva = *blanks
030100070502     c                   eval      v1dfgs = orgdes
030200070502     c                   eval      og148=orgde8
030300071120     c                   eval      og147=orgde7
030400070502     c                   endif
030500070502     c*
030600070502     c* Controllo abilitazione del p.o. gestione alla gestione telefonate
030700070502     c                   movel     §oggeot       fgsgeot           1
030800071120     c                   eval      *in12=§ogdoada='S'
030900070502     c*
031000070502      * disabilito f18 se p.o. di secondo livello
031100070502     c                   eval      *in17 = (dutlpo = '2' or dutlpo = *blanks)
031200070502     c                   eval      *in16=*off
031300070502     c                   exsr      sr_car£6
031400070502     c                   endsr
031500070316     c**********************************************************************
031600070316     c* Caricamento £6 della filiale in gestione
031700070316     c**********************************************************************
031800070316     C     SR_car£6      BEGSR
031900070316      * carico filiali £6
032000070316     C                   clear                   trul06ds
032100070316     C                   movel     '£6'          d06cod
032200070316     C                   movel     v1cfgs        d06key
032300070316     C                   movel     trul06ds      kpjbu
032400070316     C                   CALL      'TRUL06R'
032500070316     C                   parm                    kpjba
032600070316     C                   movel     kpjbu         trul06ds
032700070316     C                   movea     lin           l6
032800070316     c                   endsr
032900070315     c**********************************************************************
033000070315     c* Controllo filiale in gestione
033100070315     c**********************************************************************
033200070315     C     Ctrfgs        BEGSR
033300070315     c*
033400070315     c                   setoff                                       40
033500070315     c                   clear                   v1dfgs
033600070320      * Il p.o. gestione non può essere un p.o. gestito
033700070320     c     V1cFgs        Lookup    Nog                                    30
033800070320     c                   If        *In30
033900070320     c                   Eval      *In40 = *On
034000070320     c                   Eval      *In28 = *On
034100070320     c                   Eval      VidMsg = Msg(1)
034200070320     c                   leavesr
034300070320     c                   EndIf
034400070315     c     v1cfgs        lookup    l1                                     30
034500070315     c                   if        not *in30
034600070315     c                   seton                                        2840
034700070315     c                   eval      vidmsg = msg(01)
034800070315     c                   leavesr
034900070315     c                   endif
035000070315
035100070315      * decodifica p.o. in gestione
035200070319     c                   clear                   og148
035300071120     c                   clear                   og147
035400070315     c     v1cfgs        chain     azorg01L
035500070315     c                   if        %found(azorg01l) and orgfva = *blanks
035600070315     c                   eval      v1dfgs = orgdes
035700070319     c                   eval      og148=orgde8
035800071120     c                   eval      og147=orgde7
035900070315     c                   endif
036000070315     c                   setoff                                       16
036100071120     c                   eval      *in12=§ogdoada='S'
036200070319     c* Controllo abilitazione del p.o. gestione alla gestione telefonate
036300070319     c                   if        §oggeot<>'S'
036400070319     c                   seton                                        2840
036500070319     c                   eval      vidmsg = msg(11)
036600070319     c                   leavesr
036700070319     c                   endif
036800070417      *
036900070417     c                   movel     §oggeot       fgsgeot           1
037000070316     c                   exsr      sr_car£6
037100070417      *
037200070315     c                   endsr
037300070315     c**********************************************************************
037400070315     c* Controlli video D01.
037500070315     c**********************************************************************
037600070315     C     Ctrd01        BEGSR
037700070315      *
037800070316     c                   setoff                                       414243
037900070316     c                   setoff                                       444546
038000071120     c                   setoff                                       474849
038100070406     c
038200070406     c*  Se il p.o. in gestione non ha tellus attiva, non posso usare
038300070406     c*  il pgm
038400070406     c                   if        fgsgeot=' '
038500070406     c                   seton                                        429028
038600070406     c                   eval      vidmsg=msg(11)
038700070406     c                   goto      endd01
038800070406     c                   endif
038900070406     c
039000070315     c* causale contatto obbligatoria
039100070503    1c***                if        v1ccau=*blanks
039200070503     c***                seton                                        429028
039300070503     c***                eval      vidmsg=msg(2)
039400070503     c***                goto      endd01
039500070503    1c***                endif
039600070319     c* selezione per ragione sociale OPPURE per numero spedizione
039700070319    1c                   if        (v1crsd<>*blanks or v1crsa<>*blanks
039800070328     c                              or v1ctss='S')
039900070316     c                             and v1cnsp>0
040000070319     c                   seton                                        459028
040100070316     c                   eval      vidmsg=msg(3)
040200070316     c                   goto      endd01
040300070316    1c                   endif
040400070330     c
040500070319     c* GESTIONE SELEZIONE PER RAGIONE SOCIALE
040600070316    1c                   if        v1cnsp=0
040700070319     c* errore se immesso sia tipo spedizioni che tassative
040800070319     c                   if        v1ctip<>*blanks and v1ctss<>*blanks
040900070319     c                   seton                                        459028
041000070319     c                   eval      vidmsg=msg(10)
041100070319     c                   goto      endd01
041200070319     c                   endif
041300070319     c* Tipo spedizioni obbligatorio
041400070330    2c                   if        v1ctip=*blanks and v1ctss =*blanks
041500070330     c                   seton                                        459028
041600070330     c                   eval      vidmsg=msg(6)
041700070330     c                   goto      endd01
041800070330    2c                   endif
041900070402     c* Se richiesto appuntamento e le attensa di giacenza, errore
042000070402     c*  perchè non è possibile mettere la E come esito x appunt.
042100070406     c**                 if        v1ccau='A' and v1ctip='E'
042200070406     c**                 seton                                        459028
042300070406     c**                 eval      vidmsg=msg(17)
042400070406     c**                 goto      endd01
042500070406    2c**                 endif
042600070316      * se RSD 'DA'>*blanks e RSD 'A'a blank la inserisco uguale
042700070316    2c                   if        v1crsd <> *blank and v1crsa = *blank
042800070316     c                   eval      v1crsa = %trimr(v1crsd) + 'Z'
042900070316    2c                   endif
043000070316      * se RSD 'DA'>*blanks e RSD 'A'con solo iniziale metto nel
043100070316      * secondo byte la 'Z' x la selezione
043200070316    2c                   if        v1crsd <> *blank and v1crsa<> *blank and
043300070316     c                             %subst(v1crsa:2:1) = ' '
043400070316     c                   eval      v1crsa = %trimr(v1crsa) + 'Z'
043500070316    2c                   endif
043600070316      * vice versa RSD 'A'>*blank e RSD 'DA'a blank la inserisco uguale
043700070316    2c                   if        v1crsa <> *blank and v1crsd = *blank
043800070316     c                   movel     v1crsa        v1crsd
043900070316    2c                   endif
044000070316      * verifico che RSD 'DA' sia congruente con RSD 'A'
044100070316    2c                   if        v1crsd > v1crsa
044200070316     c                   seton                                        419028
044300070316     c                   eval      vidmsg=msg(5)
044400070316     c                   goto      endd01
044500070316    2c                   endif
044600070503     c* Tassative solo se richiesta causale per "S"
044700070503    2c***                if        v1ctss='S' and v1ccau<>'A' and
044800070503     c***                          v1ccau<>'S'
044900070503    2c                   if        v1ctss='S' and v1ccau<>'S' and
045000070503     c                             v1ccau<>' '
045100070316     c                   seton                                        469028
045200070316     c                   eval      vidmsg=msg(7)
045300070316     c                   goto      endd01
045400070316    2c                   endif
045500070330     c
045600070316     c* se richieste tassative controllo dcr
045700070319    2c                   if        v1ctss='S'
045800070319    3c                   if        v1cdcr=0
045900070319     c                   seton                                        479028
046000070319     c                   eval      vidmsg=msg(9)
046100070319     c                   goto      endd01
046200070319    3c                   endif
046300070316     c                   clear                   wlbda8
046400070316     C                   Z-ADD     v1cdcr        G02DAT
046500070316     C                   MOVEL     *BLANK        G02ERR
046600070316     C                   CALL      'XSRDA8'
046700070316     C                   PARM                    WLBDA8
046800070319    3c     g02err        ifeq      '1'
046900070316     c                   seton                                        479028
047000070316     c                   eval      vidmsg=msg(9)
047100070316     c                   goto      endd01
047200070319    3c                   endif
047300070319     c                   z-add     g02inv        I_v1cdcr
047400070402     c                   z-add     g02dat        v1cdcr
047500070319    2c                   endif
047600071126     c* data ora limite abbinamento
047700071126    2c                   if        *in12
047800071126    3c                   if        v1cdta>0
047900071126    4c                   if        v1cabb<>'S'
048000071126     c                   seton                                        489028
048100071126     c                   eval      vidmsg=msg(22)
048200071126     c                   goto      endd01
048300071126    4c                   endif
048400071126     c                   clear                   wlbda8
048500071126     C                   Z-ADD     v1cdta        G02DAT
048600071126     C                   MOVEL     *BLANK        G02ERR
048700071126     C                   CALL      'XSRDA8'
048800071126     C                   PARM                    WLBDA8
048900071126    4c     g02err        ifeq      '1'
049000071126     c                   seton                                        489028
049100071126     c                   eval      vidmsg=msg(23)
049200071126     c                   goto      endd01
049300071126    4c                   endif
049400071126     c                   z-add     g02inv        I_v1cdta
049500071126     c                   z-add     g02dat        v1cdta
049600071126    4c                   if        v1cora=0 or v1cora>2400
049700071126     c                   seton                                        499028
049800071126     c                   eval      vidmsg=msg(24)
049900071126     c                   goto      endd01
050000071126    4c                   endif
050100071128   x3c                   else
050200071128     c                   if        v1cora<>0
050300071128     c                   seton                                        499028
050400071128     c                   eval      vidmsg=msg(25)
050500071128     c                   goto      endd01
050600071128     c                   endif
050700071126    3c                   endif
050800071126    2c                   endif
050900070315     c* gestione selezione per numero spedizione
051000070316   x1c                   else
051100070327     c                   z-add     v1caas        k_aas
051200070327     c                   z-add     v1clnp        k_lnp
051300070327     c                   z-add     v1cnrs        k_nrs
051400070327     c                   z-add     v1cnsp        k_nsp
051500070402     c     karb          chain(N)  fnarb01l
051600070402    2c                   if        not %found(fnarb01l)
051700070316     c                   seton                                        449028
051800070316     c                   eval      vidmsg=msg(4)
051900070316     c                   goto      endd01
052000070402    2c                   endif
052100070316     c* la linea di arrivo deve essere in gestione
052200070316     c     arblna        lookup    l6                                     30
052300070402    2c                   if        *in30='0'
052400070316     c                   seton                                        449028
052500070402     c                   eval      vidmsg=msg(15)
052600070316     c                   goto      endd01
052700070402    2c                   endif
052800070402     c* Escludo se bloccata, in distinta o consegnata
052900070402    2c                   if        arbfbc='G' or arbfbc='*' or
053000070402     c                             arbfbc='B'
053100070402     c                   seton                                        449028
053200070402     c                   eval      vidmsg=msg(13)
053300070402     c                   eval      %subst(vidmsg:48:1)=arbfbc
053400070402     c                   goto      endd01
053500070402    2c                   endif
053600070402     c
053700080418    2c                   if        arbndc>0  and arbdcm=0
053800070402     c                   seton                                        449028
053900070402     c                   eval      vidmsg=msg(14)
054000070402     c                   goto      endd01
054100070402    2c                   endif
054200080418     c* Permetto solo i ritorni all'incasso non abilitati
054300080418    2c                   if        arbdcm>0 and arbicc<>'R'
054400080418     c                   seton                                        449028
054500080418     c                   eval      vidmsg=msg(14)
054600080418     c                   goto      endd01
054700080418    2c                   endif
054800070402     c
054900070316     c* controllo congruenza spedizione con causale contatto
055000070316     c                   exsr      ctrspecau
055100070504     c
055200070504    2c                   if        *in08
055300070504     c                   seton                                        449028
055400070316     c                   goto      endd01
055500070402   x2c                   else
055600070402     c
055700070402     c                   clear                   olv80tat
055800070504     c* se non c'e' errore di scelta posso  entrare solo se c'e'
055900070402     c*  blocco impostato (a S/T/N)
056000070402     c*  Se manca il blocco entro solo se tassativa
056100070504     c*  o con F13 per ribolccare spedizione per A/S/F
056200070504    3c                   if        wblk=*blanks  and v1ccau='S'
056300070402     c* Verifico se tassativa
056400070402     c                   exsr      CALLLV80
056500070403    3c                   endif
056600070403     c
056700070504    3c                   if        wblk=*blanks and olv80tat<>'S'
056800070504     c                   select
056900070504     c* Se C/L messaggio, bloccante
057000070504    4c                   when      v1ccau='C' or v1ccau='L'
057100070504     c                   seton                                        449028
057200070504     c                   eval      vidmsg=msg(16)
057300070504     c                   goto      endd01
057400070504     c
057500070504     c* non immessa causle contatto: errore bloccante
057600070504    4c                   when      v1ccau=' ' and not *inkm
057700070504     c                   seton                                        449028
057800070504     c                   eval      vidmsg=msg(19)
057900070504     c                   goto      endd01
058000070504   x4c                   other
058100070503     C* SE F13 - obbligatoria causale contatto
058200070504    5c                   if        *inkm and v1ccau=' '
058300070503     c                   seton                                        449028
058400070503     c                   eval      vidmsg=msg(20)
058500070503     c                   goto      endd01
058600070503    5c                   endif
058700070403     c
058800070403     c* se non forzato con F13, per A/S/F/tassative errore forzabile
058900070403    5c                   if        not *inkm
059000070403     c                   seton                                        449028
059100070403     c                   eval      vidmsg=msg(18)
059200070403     c                   seton                                        18
059300070403     c                   goto      endd01
059400070403    5c                   endif
059500070504    4c                   endsl
059600070402     c
059700070402    3c                   endif
059710140210
059800070402    2c                   endif
059900070402    1c                   endif
060000070315      *
060100070316     C     endd01        ENDSR
060200070316     c**********************************************************************
060300070316     c* Controllo congruenza fra causale contatto e spedizione
060400070316     c**********************************************************************
060500070316     C     Ctrspecau     BEGSR
060600070504     c                   clear                   wblk              5
060700070504     c                   setoff                                       08
060800070504     c
060900070402     c                   move      '3A'          tblcod
061000070402     c                   movel(p)  arbcbo        tblkey
061100070402     c                   exsr      ctabel
061200070402     c                   movel     tbluni        ds3a
061300070402     c
061400070504     c* Escludo bolle di recupero tranne che per lasciato avviso
061500070402     c                   if        §3arbl='R'
061600070402     c                   if        v1ccau='A' or v1ccau='S' or
061700070504     c                             v1ccau='F' or v1ccau='C'
061800070504     c                   seton                                        08
061900070402     c                   endif
062000070402     c                   endif
062100070402     c
062200070402     c
062300070402    1c                   select
062400070316     c* C=C/assegno
062500070402    1c                   when      v1ccau='C'
062600070402    2c                   if        §3afca=0
062700070504     c                   seton                                        08
062800070330     c* se bolla con contrassegno verifico che effettivamente l'impor-
062900070316     c* to sia > 0
063000070402   x2c                   else
063100070316     c     karb          chain     fiar901l
063200070402    3c                   if        not %found(fiar901l) or ar9cas=0
063300070504     c                   seton                                        08
063400070402     c* se bolla non abilitata, la abilito subito
063500070402    4c                   if        arbacc=' '
063600070402     c     karb          chain     fnarb01l
063700070402    5c                   if        not %error
063800070402     c                   eval      arbacc='S'
063900070402     c                   update    fnarb000
064000070402    5c                   endif
064100070402    4c                   endif
064200070402     c
064300070402    3c                   endif
064400070402    2c                   endif
064500070504     c
064600070504    2c                   if        *in08='0'
064700070316     c                   eval      wtit2=Coscas
064800070402    2c                   endif
064900070316     c
065000070316     c* A=Appuntamento
065100070316     c                   when      v1ccau='A'
065200070417     c                   if        arbtc1<>'A' and arbtc2<>'A'
065300070504     c                   seton                                        08
065400070316     c                   endif
065500070504     c                   if        *in08='0'
065600070316     c                   eval      wtit2=Cosapp
065700070316     c                   endif
065800070316     c* S=Supermercato
065900070316     c                   when      v1ccau='S'
066000070417     c                   if        arbtc1<>'S' and arbtc2<>'S'
066100070504     c                   seton                                        08
066200070316     c                   endif
066300070504     c                   if        *in08='0'
066400070316     c                   eval      wtit2=Cossup
066500070316     c                   endif
066600070316     c* F=Fermo Deposito
066700070316     c                   when      v1ccau='F'
066800070316     c                   if        arbffd=*blanks
066900070504     c                   seton                                        08
067000070316     c                   endif
067100070504     c                   if        *in08='0'
067200070316     c                   eval      wtit2=Cosffd
067300070316     c                   endif
067400070316     c* L=Lasciato Avviso
067500070316     c                   when      v1ccau='L'
067600070316     c                   if        arbfbc='A'
067700070419     c     karb          setgt     fnevb22l
067800070419     c     karb          readpe    fnevb22l
067900070402     c
068000070419     c                   if        %eof(fnevb22l) or (evb2tel<>'S'
068100070402     c                             and evb2tel<>'T' and evb2tel<>'N')
068200070504     c                   seton                                        08
068300070316     c                   endif
068400070316     c                   else
068500070504     c                   setoff                                       08
068600070316     c                   endif
068700070504     c                   if        *in08='0'
068800070316     c                   eval      wtit2=Coslav
068900070316     c                   endif
069000070316     c                   endsl
069100070316     c*
069200070504     c* 08 on -  spedizione che non prevede l'abilitazione richiesta
069300070504    1c                   if        *in08
069400070316     c                   eval      vidmsg=msg(8)
069500070402   x1c                   else
069600070402     c
069700070402     c* chaino record "L" e carico il tipo blocco che mi serve
069800070402     c*
069900070402     c* se non è lasciato avviso, chaino tipo record "L"
070000070503    3c**                 if        v1ccau<>'L'
070100070503     c**                 movel     'L'           Ktrck
070200070503     c**   kar4          chain     fiar401l
070300070503    4c**                 if        not %found(fiar401l)
070400070503     c**                 clear                   ar4not
070500070503    4c**                 endif
070600070503     c**
070700070503     c**                 movel     ar4not        dsbl4l
070800070503    3c**                 endif
070900070503     c
071000070503     c* richiamo fnlrg6r per avere la stringa delle abilitazioni
071100070503     c*
071200070503     c                   clear                   fnlrg6ds
071300070503     c                   eval      ilrg6aas=k_aas
071400070503     c                   eval      ilrg6lnp=K_lnp
071500070503     c                   eval      ilrg6nrs=K_nrs
071600070503     c                   eval      ilrg6nsp=K_nsp
071700070503     c* Escludo dalla visualizzazione Assegnati
071800070503     c                   movel     '9'           ilrg6nb1
071900070503     c                   call      'FNLRG6R'
072000070503     c                   parm                    fnlrg6ds
072100070503     c                   movel     olrg6abi      dlrg6abi
072200070402     c
072300070402     c* Memorizzo il blocco presente su ar4 in base alla causale contatto
072400070402    3c                   select
072500070504     c                   when      v1ccau='S'
072600070504     c                   eval      wblk=d§lrg6tsu
072700070402     c
072800070504     c                   when      v1ccau='A'
072900070504     c                   eval      wblk=d§lrg6tap
073000070504     c
073100070402     c                   when      v1ccau='C'
073200070402     c                   if        arbacc<>'S'
073300070503     c                   eval      wblk=d§lrg6tca
073400070402     c                   endif
073500070402
073600070402     c                   when      v1ccau='F'
073700070503     c                   eval      wblk=d§lrg6tfd
073800070402
073900070402     c                   when      v1ccau='L'
074000070503     c                   eval      wblk=d§lrg6tla
074100070503     c
074200070503     c* se non impostato metto se ha almeno una abilitazione
074300070503     c                   when      v1ccau=' '
074400070504     c                   eval      wblk=%subst(olrg6abi:3:5)
074500070504     c
074600070402    3c                   endsl
074700070402    1c                   endif
074800070402     c
074900070316     c                   endsr
075000070316     C*--- CHAIN TABEL00F --------------------------------------------*
075100070316     C     CTABEL        BEGSR
075200070316     C     KTAB          CHAIN     TABEL                              30
075300070316     c                   if        %found(tabel00f) and tblflg<> ' '
075400070316     C                   SETON                                        30
075500070316     C                   END
075600070316     C                   ENDSR
075700011227     c**********************************************************************
075800011227     c* gestione sfl
075900011227     c**********************************************************************
076000070316     C     ges_s01       BEGSR
076100000627      *
076200070402     c                   eval      *inke='1'
076300070402     c* looppo per F5 - rivisulizza
076400070402     c                   dow       *inke='1'
076500070402     c
076600070402     c                   eval      *inke='0'
076700070316     C                   EXSR      ries01
076800071204     c* ordino il subfile per provincia/ragione sociale dst.solo se caricati
076900070316     c* record
077000070316     c                   if        ok>0
077100071204     c                   eval      w_ord=w_prvrsoc
077200070316     C                   EXSR      ordina
077300070316     c                   else
077400070402     c
077500070402     c* Esco dal sfl emettendo msg di errore
077600070327     c                   setoff                                       20
077700070402     c                   seton                                        2890
077800070504ù    c                   movel     msg(12)       vidmsg
077900070402     c                   leave
078000070316     c                   endif
078100070316      * Imposto sottotitolo seconda videata
078200070316     c                   eval      v1ctit2=wtit2
078300070522     c                   if        v1cgir<>*all'9'
078400070522     c                   eval      v1cgircos='Giro'
078500070522     c                   eval      v1cgirt=v1cgir
078600070522     c                   if        v1cgirt=*blanks
078700070522     c                   eval      v1cgirt='Senza giro'
078800070522     c                   eval      v1cgircos=*blanks
078900070522     c                   endif
079000070522     c                   endif
079100070507     c* se specificata la causale contatto, scirvo nel sfl ALTRI contatti
079200070507     c                   if        wtit2<>*blanks
079300070507     c                   eval      v1caltri='Altri'
079400070507     c                   else
079500070507     c                   clear                   v1caltri
079600070507     c                   endif
079700070507     c
079800070316     c                   if        v1ctss='S'
079900070316     c                   if        v1ccau='S'
080000070316     c                   eval      v1ctit2=cossuptas
080100070316     c                   else
080200070316     c                   eval      v1ctit2=cosapptas
080300070316     c                   endif
080400070316     c                   endif
080500070330     c
080600070330      * Imposto tipo contatto in testata
080700070330     c                   select
080800070330     c                   when      v1ctip='D'
080900070330     c                   eval      v1ctipdec=tipdec(1)
081000070330     c                   when      v1ctip='R'
081100070330     c                   eval      v1ctipdec=tipdec(2)
081200070330     c                   when      v1ctip='E'
081300070330     c                   eval      v1ctipdec=tipdec(3)
081400070330     c                   when      v1ctip='T'
081500070330     c                   eval      v1ctipdec=tipdec(4)
081600070330     c                   ENDSL
081700071128      * Imposto data e ora limite abbinamento se presenti
081800071128     c                   if        *in12 and v1cdta>0
081900071128     c                   movel     v1cdta        v1cdtaa
082000071128     c                   eval      v1clim='Lim.Abbin. ' +
082100071128     c                             %editw (v1cdtaa:weditdta)+
082200071128     c                             ' ' +
082300071128     c                             %editw(v1cora:weditora)
082400071128     c                   endif
082500071204     c                   eval      v1tf11=ord_ragprv
082600070316     c*
082700070316     C                   DO        *hival
082800070316     C                   WRITE     LRG1T01
082900070316     C                   WRITE     LRG1Z01
083000070316     C                   EXFMT     lrg1c01
083100011227     C                   SELECT
083200070402     c* esco per F3-Fine  F12-Ritorno  F5-rivisualizza
083300070402     C                   WHEN      *INKL=*ON or *inkc  or *inke
083400070316     C                   leave
083500071204     c* f11=Cambio ordinamento sfl
083600071204     C                   WHEN      *INkk
083700071204     c                   select
083800071204     c                   when      w_ord=w_prvrsoc
083900071204     c                   eval      w_ord=w_rsocprv
084000071204     c                   eval      v1tf11=ord_prvrag
084100071204     c                   when      w_ord=w_rsocprv
084200071204     c                   eval      w_ord=w_prvrsoc
084300071204     c                   eval      v1tf11=ord_ragprv
084400071204     c                   endsl
084500071204     C                   exsr      ordina
084600071204     c                   iter
084700011227
084800011227     C                   OTHER
084900011227
085000070321     C                   EXSR      GestioneSFL
085100011227     C                   ENDSL
085200011228
085300011227     C                   ENDDO
085400070402     c
085500070402     C                   ENDDO
085600070402     C                   ENDSR
085700070125     c**********************************************************************
085800070316     c* carica sfl
085900070125     c**********************************************************************
086000070316     C     Ries01        BEGSR
086100070504     c* se ho richiesto le tassative e non ho impostato la causale
086200070504     c*  contatto--> la forzo
086300070504     c                   if        v1ctss='S' and v1ccau=' '
086400070504     c                   eval      v1ccau='S'
086500070504     c                   endif
086600070330     c* Pulizia SFL
086700070327     c                   setoff                                       21
086800070316     C                   WRITE     lrg1c01
086900070316     c                   seton                                        21
087000070125     C                   EVAL      snrr1   = 0
087100070316     C                   EVAL      nrr1    = 0
087200070330     c
087300070131      * gestione eventuali filiali in £6
087400070316    1c                   do        *hival        £6                3 0
087500070316    2c                   if        l6(£6)  = 0
087600070131     c                   leave
087700070316    2c                   endif
087800070330     c
087900070316     c     l6(£6)        setll     fnarb05l
088000070316    2c                   do        *hival
088100070316     c     l6(£6)        reade     fnarb05l
088200070316      *
088300070316    3c                   if        %eof(fnarb05l)
088400070316     c                   leave
088500070316    3c                   endif
088600070131
088700070131      * filtri
088800070131
088900070131      * bolle da escludere sempre
089000080418      * le spedizioni consegnate non le escludo perchè devo elaborare i
089100080418      *  ritorni all'incasso non abilitati dei C/ASS
089200070316    3c                   if        arbfbc = '*' or arbfbc = 'B' or
089300080418     c****                         arbfbc = 'G' or arbndc > 0 or arbdcm>0
089400080418     c                             arbfbc = 'G' or (arbndc > 0  and arbdcm=0)
089500070131     c                   iter
089600070316    3c                   endif
089601141030
089602141030     c* scarto se presente dispo di dirottamento
089603141030     c                   clear                   fnlry09ds2
089604141030     c                   eval      ilry09tch='C'
089605141030     c                   eval      ilry09tdis='D'
089606141030     c                   eval      ILRY09AAS=arbaas
089607141030     c                   eval      ILRY09LNP=arbLNP
089608141030     c                   eval      ILRY09NRS=arbNRS
089609141030     c                   eval      ILRY09NSP=arbNSP
089610141030     c                   eval      kpjbu=fnlry09ds2
089611141030     c                   call      'FNLRY09R'
089612141030     c                   parm                    kpjba
089613141030     c                   eval      fnlry09ds2=kpjbu
089614141030     c                   if        olry09esit='1'
089615141030     c                   iter
089616141030     c                   endif
089700070131
089800070201      * con C.A. aperta
089900070330     c***                clear                   fidn12ds
090000070330     c***                eval      i12aas = arbaas
090100070330     c***                eval      i12lnp = arblnp
090200070330     c***                eval      i12nrs = arbnrs
090300070330     c***                eval      i12nsp = arbnsp
090400070330     c***                eval      i12fel = 'E'
090500070330     c***                eval      i12fan = 'E'
090600070330     c***                eval      i12fch = 'E'
090700070330     c***                call      'FIDN12R'
090800070330     c***                parm                    fidn12ds
090900070201      **
091000070201      * se c'è almeno una C.A. aperta
091100070201      **
091200070330    3c***  o12nca        ifgt      0
091300070330     c***                iter
091400070330    3c***                end
091500070330     c
091600070522     c* scarto la spedizione se il codice giro richiesto è diverso
091700070522     c                   if        v1cgir<>*all'9'
091800070522     c     karg1         chain     fiarg01l
091900070522     C                   if        (%found(fiarg01l) and argcgi <> v1cgir)
092000070522     c                             or not %found(fiarg01l)
092100070522     c                   iter
092200070522     c                   endif
092300070522     c                   endif
092400070316      * verifico congurenza con causale contatto
092500070327     c                   z-add     arbaas        k_aas
092600070327     c                   z-add     arblnp        k_lnp
092700070327     c                   z-add     arbnrs        k_nrs
092800070327     c                   z-add     arbnsp        k_nsp
092900070316     c                   exsr      ctrspecau
093000070504     c
093100070504     c* Spedizione incongruente con la causale contatto immessa
093200070504    3c                   if        *in08
093300070316     c                   iter
093400070504     c                   else
093500070504     c* se non ho immesso la causale contatto e nessuna abilitazione
093600070504     c*  da fare, errore
093700070504    4c                   if        v1ccau=' ' and wblk=*blanks
093800070504     c                   iter
093900070504    4c                   endif
094000070504    3c                   endif
094100070330     c
094200070330      * parzializzazione per RSD richiesto range a video
094300070330    3c                   if        v1crsd <> *blank and v1crsa <> *blank
094400070330     c                             and (arbrsd < v1crsd or arbrsd > v1crsa)
094500070330     c                   iter
094600070330    3c                   endif
094700070330     c
094800070330     c* Parzializzazione in base all'abbinamento
094900070330     c                   clear                   wabb
095000070412     c                   clear                   wcallLV53         1
095100070330    3c                   if        v1cabb<>'N'
095200070411     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
095300070411    4C                   SELECT
095400070411     C     ARBAMA        WHENEQ    3
095500070411     C                   MOVEL     'S'           wABB
095600070411     C*
095700070411     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
095800070411    4C     ARBAMA        WHENEQ    1
095900070411     C     ARBAMP        ANDEQ     1
096000070411     C                   MOVEL     'S'           wABB
096100070411     C*
096200070411     C* 3) SE LOCALE SEMPRE ABBINATO
096300070411    4C                   WHEN      arbtfp=arbtfa or arblnp=arblna
096400070411     C                   MOVEL     'S'           wABB
096500070411     C*
096600070411   X4C                   OTHER
096700071127     c                   exsr      calllv53
096800070330     c*
096900070411    5c                   if        d53err=*blanks and d53nfa>0
097000070330     c                   eval      wabb='S'
097100070330     c                   else
097200070330     c                   eval      wabb='N'
097300070411    5c                   endif
097400070411    4c                   endsl
097500070330     c
097600070330     c* Solo bolle     abbinate
097700070330    4c                   if        v1cabb='S' and wabb<>'S'
097800070330     c                   iter
097900070330    4c                   endif
098000070330     c* Solo bolle non abbinate
098100070330    4c                   if        v1cabb='E' and wabb='S'
098200070330     c                   iter
098300070330    4c                   endif
098400070330    3c                   endif
098500071127     c* Parzializzazione in base alla data ora limite abbinamento
098600071127     c* (le bolle con abilitazione forzata e quelle locali le carico
098700071127     c* sempre a prescindere)
098800071127    3c                   if        *in12 and v1cdta>0 and arbama<>3 and
098900071127     c                             arbtfp<>arbtfa and arblnp<>arblna
099000071127    4c                   if        wcalllv53=' '
099100071127     c                   exsr      calllv53
099200071127    4c                   endif
099300071127    4c                   if        d53err=*blanks and d53nfa>0
099400071127     c                   movel     i_v1cdta      soglia
099500071127     c                   move      v1cora        soglia
099600071127     c                   movel     d53aed        ArrEffet
099700071127     c                   move      d53aeh        ArrEffet
099800071127    5c                   if        arreffet>soglia
099900071127     c                   iter
100000071127    5c                   endif
100100071127   x4c                   else
100200071127     c                   iter
100300071127    4c                   endif
100400071127    3c                   endif
100500070330
100600070330     c
100700070330     c* TIPO SPEDIZIONI -
100800070316     c* Verifico congruenza del blocco con la richiesta
100900070330     c*   del tipo spedizioni selezionato
101000070330    3c                   if        v1ctip<>*blanks
101100070504     c                   clear                   wpos
101200070330     c* Esito OK --> escludo
101300070330    4c                   select
101400070504    4c                   when      wblk =*blanks
101500070330     c                   iter
101600070330     c* solo le "Da contattare"
101700070504    4c****               when      v1ctip='D' and wblk<>'S'
101800070504    4c                   when      v1ctip='D'
101900070504     c                   eval      wpos=%scan('S':wblk)
102000070504     c                   if        wpos=0
102100070504     c                   iter
102200070504     c                   endif
102300070330     c* solo le "da Ricontattare"
102400070504    4c****               when      v1ctip='R' and wblk<>'T'
102500070504    4c                   when      v1ctip='R'
102600070504     c                   eval      wpos=%scan('T':wblk)
102700070504     c                   if        wpos=0
102800070504     c                   iter
102900070504     c                   endif
103000070330     c* solo le "Attesa giacenza"
103100070507    4c*****              when      v1ctip='E' and wblk<>'N'
103200070504    4c                   when      v1ctip='E'
103300070504     c                   eval      wpos=%scan('N':wblk)
103400070504     c                   if        wpos=0
103500070504     c                   iter
103600070504     c                   endif
103700070330     c* solo    "tutti contatti"
103800070504    4c***                when      v1ctip='T' and wblk='N'
103900070504    4c                   when      v1ctip='T'
104000070504     c                   eval      wpos=%scan('S':wblk)
104100070504     c                   if        wpos=0
104200070504     c                   eval      wpos=%scan('T':wblk)
104300070504     c                   if        wpos=0
104400070504     c                   iter
104500070504     c                   endif
104600070504     c                   endif
104700070504     c
104800070330    4c                   endsl
104801140210     c* se per la spedizione è previsto alert inviato/da inviare non
104802140211     c* la visualizzo nel sfl
104803140307     c                   select
104804140307     c                   when      v1ccau='L' or v1ccau='F'
104805140307     c                   eval      wcau=v1ccau
104806140211     c                   exsr      sr_callv82
104807140211     c                   if        olv82sts='0' or olv82sts='1'
104808140211     c                   iter
104809140211     c                   endif
104810140307     c                   when      v1ccau=*blanks
104813140307     c                   if        D§LRG6TCA=*blanks and
104814140307     c                             D§LRG6TSU=*blanks and
104815140307     c                             D§LRG6TAP=*blanks
104819140307     c                   select
104820140307     c* Solo lasciato avviso
104821140307     c                   when      D§LRG6tla<>*blanks and
104822140307     c                             D§LRG6Tfd=*blanks
104823140307     c                   eval      wcau='L'
104824140307     c                   exsr      sr_callv82
104825140307     c                   if        olv82sts='0' or olv82sts='1'
104826140307     c                   iter
104827140307     c                   endif
104828140307     c* Solo Fermo deposito
104829140307     c                   when      D§LRG6tfd<>*blanks and
104830140307     c                             D§LRG6Tla=*blanks
104831140307     c                   eval      wcau='F'
104832140307     c                   exsr      sr_callv82
104833140307     c                   if        olv82sts='0' or olv82sts='1'
104834140307     c                   iter
104835140307     c                   endif
104836140307     c* Sia lasciato avviso che fermo deposito
104837140307     c                   when      D§LRG6tfd<>*blanks and
104838140307     c                             D§LRG6Tla<>*blanks
104839140307     c* richiamo per l.avviso e se alert inviato o da inviare verifico anche
104840140307     c* il fermo deposito
104841140307     c                   eval      wcau='L'
104842140307     c                   exsr      sr_callv82
104843140307     c                   if        olv82sts='0' or olv82sts='1'
104844140307     c                   eval      wcau='F'
104845140307     c                   exsr      sr_callv82
104846140307     c* se entrambi inviati/da inviare non carico la spedizione nel sfl
104847140307     c                   if        olv82sts='0' or olv82sts='1'
104848140307     c                   iter
104849140307     c                   endif
104850140307     c                   endif
104851140307     c                   endsl
104852140307     c                   endif
104853140307     c                   endsl
104854140211     c
104900070330    3c                   endif
105000070330    c
105100070330     c* TASSATIVE
105200070330     c* escludo se blocco impostato (rientra nelle altre selezioni)
105300070330    3c                   if        v1ctss='S'
105400070330    4c                   if        wblk <> ' '
105500070330     c                   iter
105600070330    4c                   endif
105700070330     c
105800070504     c* Escludo se la data cons,richiesta non corrisponde a quella immes
105900070316    4c                   if        arbdcr<>I_v1cdcr
106000070316     c                   iter
106100070316    4c                   endif
106200070402     c
106300070402     c                   EXSR      CALLLV80
106400070402
106500070330     c
106600070330     c* Escludo se supermercato o appuntamento non tassativo
106700070316    4c                   if        olv80tat<>'S'
106800070316     c                   iter
106900070316    4c                   endif
107000070330     c
107100070412     c* se locale desumo la data di arrivo
107200070412    4c                   if        arbtfp=arbtfa
107300070412     c                   clear                   fnlv53ds
107400070412     c                   EXSR      BOLLALOC
107500070412   x4c                   else
107600070412     c
107700070330     c* Escludo se data cons.richiesta <> data arrivo BOLLA
107800070412    5c                   if        wcallLV53=' '
107900071127     c                   exsr      calllv53
108000070412    5c                   endif
108100070412    4c                   endif
108200070330     c*
108300070330    4c                   if        d53err<>*blanks or d53dfa<>I_v1cdcr
108400070412     c* Se la data è diversa, verifico se festiva, nel qual caso
108500070412     c*  determino la prima data superiore lavorativa e rifaccio il controll
108600070412    5c                   if        d53dfa>0
108700070412     c                   exsr      Festivo
108800070412    6c                   if        d53dfa<>i_v1cdcr
108900070412     c                   iter
109000070412    6c                   endif
109100070412     c
109200070412   x5c                   else
109300070412     c                   iter
109400070412    5c                   endif
109500070412     c
109600070330    4c                   endif
109700070330    3c                   endif
109800070330     c* ********************
109900070330     c* Carico record in SFL
110000070330     c* ********************
110100070316     C                   move      *blank        V1sopz
110200070316     c                   movel     arbrsd        v1srsd
110300071203     c                   movel     arbprd        v1sprd
110400070316     c                   z-add     arbaas        v1saas
110500080908     c**                 z-add     arblnp        v1slnp
110600080908     c**                 z-add     arbnrs        v1snrs
110700080908     c**                 z-add     arbnsp        v1snsp
110701080908     c                   z-add     arblnp        v1hlnp
110702080908     c                   z-add     arbnrs        v1hnrs
110703080908     c                   z-add     arbnsp        v1hnsp
110704080908     c* DDT
110705080908     c                   clear                   v1sddt
110706080908     c                   movel     'A'           Ktrck
110707080908     c     kar4          chain     fiar401l
110708080908     c                   if        %found(fiar401l)
110709080908     c                   movel     ar4not        dsbl4a
110710080908     c                   if        §b4abm='S' or §b4abm='C' or
110711080908     c                             §b4abm='P' or §b4abm='Q'
110712080909     c                   eval      v1sddt='SI DDT'
110713080908     c                   endif
110714080908     c                   endif
110800070330     c* Data spedizione
110900070330     c     *iso          movel     arbdsp        dataiso
111000070406     c                   movel     dataiso       datadmy
111100070406     c                   movel     datadmy       v1sdsp
111200070402     c                   clear                   v1stip_d
111300070330     c* Note bolla
111400070330     c                   clear                   v1snt1
111500070330     c                   clear                   v1snt2
111600070402     c                   movel     '8'           Ktrck
111700070330     c     kar4          chain     fiar401l
111800070330     c                   if        %found(fiar401l)
111900070330     c                   movel     ar4not        v1snt1
112000070402     c                   movel     '9'           Ktrck
112100070330     c     kar4          chain     fiar401l
112200070330     c                   if        %found(fiar401l)
112300070330     c                   movel     ar4not        v1snt2
112400070330     c                   endif
112500070330     c                   endif
112600070330     c* Telefono destinatario
112700070330     c     kar5          chain     fiar501l
112800070330     c                   if        %found(fiar501l)
112900070330     c                   movel     ar5uni        dar5gen
113000070330     c                   else
113100070330     c                   clear                   dar5gen
113200070330     c                   endif
113300070402     c
113400070504     c                   if        %subst(§ar5teld:1:1) = '#'
113500070402     c                   eval      v1steld=%subst(§ar5teld:2:15)
113600070402     c                   else
113700070402     c                   Eval      V1sTeld = §Ar5Teld
113800070402     c                   endif
113900070330     c
114000070508     c* Decodifica tipo contatto solo se richiesto  T-tutti contatti
114100070402     c                   clear                   v1stip_d
114200070330     c
114300070504    3c                   if        v1ctip='T'
114400070508     c                   clear                   wpos2
114500070508     c                   eval      wpos2=%scan('S':wblk)
114600070508     c                   if        wpos2>0
114700070330     c                   eval      v1stip_d='DaCont'
114800070508     c                   else
114900070330     c                   eval      v1stip_d='Ricont'
115000070504    3c                   endif
115100070508    3c                   endif
115200070504     c
115300070504     c                   exsr      CerABI
115400071120     c* visualizzo un asterisco se Spedizione da controllare per
115500071120     c* la messa in consegna automatica: si tratta dei seguenti casi:
115600071120     c*   Espresso + appuntamento
115700071120     c*   appuntamento con data cons.richiesta>0
115800071120     c*   spedizione da ricontattare con dcr>0
115900071120     c* Sono escluse le spedizioni dirette ai Supermercati
116000071120     c                   if        (arbtsp='E' and (arbtc1='A' or arbtc2='A'))
116100071120     c                             or (arbdcr>0 and (arbtc1='A' or arbtc2='A'))
116200071120     c                             or (arbdcr>0 and %scan('T':wblk)>0)
116300071120     c                   if        arbtc1<>'S' and arbtc2<>'S'
116400071120     c                   eval      v1sast='*'
116500071120     c                   endif
116600071120     c                   else
116700071120     c                   clear                   v1sast
116800071120     c                   endif
116900071120
117000070125      *esco per raggiunta massima capacità sfl
117100070316    3c                   if        snrr1 > 9990
117200070125     c                   leave
117300070316    3c                   endif
117400070125
117500070125     C                   EVAL      snrr1 = snrr1 + 1
117600070316     C                   WRITE     lrg1s01
117700070316    2C                   ENDDO
117800070316    1C                   enddo
117900070125
118000070316    1C                   IF        snrr1  > 0
118100070316     C                   EVAL      *IN20 = *ON
118200070125     C                   MOVE      1             OK                1 0
118300070125     C                   ELSE
118400070125     C                   MOVE      0             OK                1 0
118500070316    1C                   ENDIF
118600070125     C                   EVAL      nrr1  = 1
118700070316     C                   EVAL      lasnrr1  = snrr1
118800070316     C                   EVAL      snrr1  = 1
118900070125     C                   EVAL      csrrrn = 1
119000070402     c
119100070125     C                   ENDSR
119200070504     C**************************************************************************
119300070507     c* Visuliazzo le abilitazioni della bolla
119400070507     c*  escludo quella richeista in prima videata, se richeista
119500070504     C**************************************************************************
119600070504     c     CerABI        BEGSR
119700070504     c                   clear                   v1sabi
119800070504     c                   z-add     1             x
119900070504     c                   clear                   sk8
120000070507     c*
120100070507    1c                   if        v1ccau=' '
120200070504     c* Im wpos è contenuta la posizione della 1 abilitazione che
120300070504     c*  corrisponde al tipo spedizioni richiesto a video
120400070504     c*  poi di seguito visulizzo tutte le altre
120500070504     c                   if        wpos=1
120600070504     c                   movea     'Ca'          sk8(x)
120700070504     c                   add       2             x
120800070504     c                   clear                   d§lrg6tca
120900070504     c                   endif
121000070504     c                   if        wpos=2
121100070504     c                   movea     'Su'          sk8(x)
121200070504     c                   add       2             x
121300070504     c                   clear                   d§lrg6tsu
121400070504     c                   endif
121500070504     c                   if        wpos=3
121600070504     c                   movea     'Ap'          sk8(x)
121700070504     c                   add       2             x
121800070504     c                   clear                   d§lrg6tap
121900070504     c                   endif
122000070504     c                   if        wpos=4
122100070504     c                   movea     'Fd'          sk8(x)
122200070504     c                   add       2             x
122300070504     c                   clear                   d§lrg6tfd
122400070504     c                   endif
122500070504     c                   if        wpos=5
122600070504     c                   movea     'La'          sk8(x)
122700070504     c                   add       2             x
122800070504     c                   clear                   d§lrg6tla
122900070504     c                   endif
123000070507     c
123100070507    1c                   endif
123200070504     c                   clear                   wpos
123300070504     c
123400070504     c* C/Ass
123500070507     c                   if        d§lrg6tca<>' ' and v1ccau<>'C'
123600070504     c                   movea     'Ca'          sk8(x)
123700070504     c                   add       2             x
123800070504     c                   endif
123900070504     c* Supermercati
124000070507     c                   if        d§lrg6tsu<>' ' and v1ccau<>'S'
124100070504     c                   movea     'Su'          sk8(x)
124200070504     c                   add       2             x
124300070504     c                   endif
124400070504     c* Appuntamento
124500070507     c                   if        d§lrg6tap<>' ' and v1ccau<>'A'
124600070504     c                   movea     'Ap'          sk8(x)
124700070504     c                   add       2             x
124800070504     c                   endif
124900070504     c* Fermo Deposito
125000070507     c                   if        d§lrg6tfd<>' ' and v1ccau<>'F'
125100070504     c                   movea     'Fd'          sk8(x)
125200070504     c                   add       2             x
125300070504     c                   endif
125400070504     c* Lasciato Avviso
125500070507     c                   if        d§lrg6tla<>' ' and v1ccau<>'L'
125600070504     c                   movea     'La'          sk8(x)
125700070504     c                   add       2             x
125800070504     c                   endif
125900070504     c
126000070504     c                   movea     sk8           v1sabi
126100070504     c                   ENDSR
126200070412     C**************************************************************************
126300070412     C* Per bolla locale desumo la data dell'arrivo bolla
126400070412     C**************************************************************************
126500070412     c     BOLLALOC      begsr
126600070412     c     kfvv8         SETGT     FNFVV08L
126700070412     c     kfvv81        READe     FNFVV08L
126800070412     C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=ARBDBR
126900070412     C                   EVAL      d53dfa =FVVDFV
127000070412     C*
127100070412     C                   ELSE
127200070412     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA BORDERO + 1
127300070412     c     *iso          movel     arbDBR        dataiso
127400070412     c                   adddur    1:*d          dataiso
127500070412     c                   movel     dataiso       d53dfa
127600070412     C                   ENDIF
127700070412     c                   ENDSR
127800070412     C**************************************************************************
127900070412     C* Determino foglio arrivi in data festiva
128000070412     C**************************************************************************
128100070412     c     FESTIVO       begsr
128200070412     c                   setoff                                       34
128300070412     c     *iso          move      d53dfa        dataiso
128400070412     c
128500070412    6c                   dow       not *in34
128600070412     c* controllo se e' festivo
128700070412     C     *iso          move      dataiso       kdata
128800070412     C                   Z-ADD     68            KTFp
128900070412     C                   clear                   KTFa
129000070412     C     KCLN          CHAIN     AZCLN01L                           34
129100070412    7C     *IN34         IFEQ      *OFF
129200070412     C     MAT(KGG)      andne     ' '
129300070412     C     *IN34         oreq      *OFF
129400070412     C     POM(KGG)      andne     ' '
129500070412     c**
129600070412     c                   adddur    1:*D          dataiso
129700070412     c                   else
129800070412     c                   seton                                        34
129900070412    7c                   endif
130000070412    6c                   enddo
130100070412     c
130200070412     c     *iso          movel     dataiso       d53dfa
130300070412     c                   ENDSR
130400070402     c**********************************************************************
130500070402     c* Verifico se si tratta di tassativa
130600070402     c**********************************************************************
130700070402     c     CALLLV80      begsr
130800070402     c                   clear                   fnlv80ds
130900070402     c                   eval      ilv80aas=arbaas
131000070402     c                   eval      ilv80lnp=arblnp
131100070402     c                   eval      ilv80nrs=arbnrs
131200070402     c                   eval      ilv80nsp=arbnsp
131300070402     c                   eval      ilv80mgs=arbmgs
131400070402     c                   eval      ilv80tcr=arbtcr
131500070402     c                   eval      ilv80dcr=arbdcr
131600070402     c                   eval      ilv80hcr=arbhcr
131700070402     c                   eval      ilv80tc1=arbtc1
131800070402     c                   eval      ilv80tc2=arbtc2
131900070402     c                   call      'FNLV80R'
132000070402     c                   parm                    kpjba
132100070402     c                   parm                    fnlv80ds
132200070402     c                   parm      'S'           ilv80nwproc       1
132300070402     c                   endsr
132400070321     c**********************************************************************
132500070321     C     Gestionesfl   BEGSR
132600070321     c**********************************************************************
132700070321      * Elaborazione opzioni.
132800070321     c*
132900070321     c                   readc     lrg1s01
133000070402     c
133100070321     c                   dow       %eof=*oFF
133200070321     C                   IF        v1sopz <> *blanks
133300070321     c                   exsr      chiamo
133400070321     c                   move      *blank        v1sopz
133500070402     c
133600070402     c* Se non premuto F12-proteggo opzione
133700070507     c                   if        wf6='S'
133800070402     c                   seton                                        10
133900070402     c                   endif
134000070402     c
134100070321     c                   update    lrg1s01
134200070402     c                   setoff                                       10
134300070321     C                   ENDIF
134400070402     c
134500070321     c                   readc(E)  lrg1s01
134600070321     C                   enddo
134700070321     C                   ENDSR
134800070321     c**********************************************************************
134900070321     C     chiamo        BEGSR
135000070321     c**********************************************************************
135100070321     c                   clear                   fnlrg2ds
135200070430     c                   clear                   fnlrg6ds
135300070507     c                   clear                   wf6               1
135400070321     c                   if        v1cnsp>0
135500070321     c                   eval      ilrg2aas=v1caas
135600070321     c                   eval      ilrg2lnp=v1clnp
135700070321     c                   eval      ilrg2nrs=v1cnrs
135800070321     c                   eval      ilrg2nsp=v1cnsp
135900070402     c                   eval      ilrg2tss=olv80tat
136000070321     c                   else
136100070321     c                   eval      ilrg2aas=v1saas
136200080908     c                   eval      ilrg2lnp=v1hlnp
136300080908     c                   eval      ilrg2nrs=v1hnrs
136400080908     c                   eval      ilrg2nsp=v1hnsp
136500070321     c                   eval      ilrg2tss=v1ctss
136600070321     c                   endif
136700070504     c* Se richiesto causale contatto
136800070504     c                   if        v1ccau<>' '
136900070321     c                   eval      ilrg2cau=v1ccau
137000070504     c                   else
137100070504     c                   movel     v1sabi        ilrg2cau
137200070504     c                   endif
137300070504     c
137400070430     c                   clear                   wprima_v
137500070430     c                   clear                   xx
137600070430     c                   do        *hival
137700070430     c                   if        ilrg2cau=' '
137800070430     c                   leave
137900070430     c                   endif
138000070321     c                   call      'FNLRG2R'
138100070321     c                   parm                    kpjba
138200070321     c                   parm                    fnlrg2ds
138300070507     c                   if        olrg2f12=*blanks
138400070507     c                   eval      wf6='S'
138500070430     c                   endif
138600070502     c* verifico se presenti altre causali per le quali
138700070430     c* bisogna gestire il contatto
138800070502     c                   if        ilrg2tss='S'
138900070430     c                   leave
139000070430     c                   else
139100070430     c                   z-add     ilrg2aas      ILRG6AAS
139200070430     c                   z-add     ilrg2lnp      ILRG6LNP
139300070430     c                   z-add     ilrg2nrs      ILRG6NRS
139400070430     c                   z-add     ilrg2nsp      ILRG6NSP
139500070430     c                   add       1             xx
139600070430     c                   move      ilrg2cau      blov(xx)
139700070430     c                   call      'FNLRG6R'
139800070430     c                   parm                    fnlrg6ds
139900070430     c                   movel     olrg6abi      dlrg6abi
140000070504     c
140100070430     c                   select
140200070502     c                   when      d§lrg6tca<>' '
140300070430     c                   eval      ilrg2cau='C'
140400070504     c
140500070504     c                   when      d§lrg6tap<>' '
140600070430     c                   eval      ilrg2cau='A'
140700070504     c
140800070504     c                   when      d§lrg6tsu<>' '
140900070430     c                   eval      ilrg2cau='S'
141000070504     c
141100070502     c                   when      d§lrg6tfd<>' '
141200070430     c                   eval      ilrg2cau='F'
141300070504     c
141400070502     c                   when      d§lrg6tla<>' '
141500070430     c                   eval      ilrg2cau='L'
141600070504     c
141700070430     c                   other
141800070430     c                   clear                   ilrg2cau
141900070430     c                   endsl
142000070430     c                   endif
142100070430     c                   enddo
142200070321     C                   ENDSR
142300071127     c**********************************************************************
142400071127     C     calllv53      BEGSR
142500071127     c**********************************************************************
142600071127      *
142700071127     c                   clear                   fnlv53ds
142800071127     C                   MOVEL     'A'           D53TFO
142900071127     C                   MOVE      arbNFV        D53NFV
143000071127     C                   MOVE      arbtfp        D53FGS
143100071127     C                   MOVEL     'B'           D53ABB
143200071127     C                   MOVE      ARBLNA        D53LNA
143300071127     C                   MOVE      ARBTFA        D53TFA
143400071127     C                   CALL      'FNLV53R'
143500071127     C                   PARM                    fnlv53ds
143600071127     c                   eval      wcalllv53='S'
143700071127     C
143800071127     C                   ENDSR
143900011227     c**********************************************************************
144000000607     C     *INZSR        BEGSR
144100011227     c**********************************************************************
144200000607      *
144300000607     C     *ENTRY        PLIST
144400000607     C                   PARM                    Kpjba
144500070123      *
144600070315     c     *dtaara       define    §azute        azuteds
144700070315     c     *dtaara       define    §datiute      ddatiute
144800070315     c                   in(e)     *dtaara
144900070315     c                   if        %error  or rsut = *blanks
145000070315     c                   clear                   tibs34ds
145100070315     c                   call      'TIBS34R'
145200070315     c                   parm                    tibs34ds
145300070315     c                   in        *dtaara
145400070315     c                   endif
145500030702      *reperimento data
145600070319     C                   TIME                    W0140            14 0
145700070319     C                   MOVE      W0140         WDAT              8 0
145800030702     C*
145900030702     C                   Z-ADD     WDAT          G02DAT
146000030702     C                   MOVEL     *BLANK        G02ERR
146100030702     C                   CALL      'XSRDA8'
146200030702     C                   PARM                    WLBDA8
146300030702     C* UDATE A 8 IN AAAA/MM/GG
146400030717     C                   Z-ADD     G02INV        WUDAte            8 0
146500000607      *
146600070123      *
146700070124     C     Karb          klist
146800070327     C                   kfld                    K_aas
146900070327     C                   kfld                    K_lnp
147000070327     C                   kfld                    K_nrs
147100070327     C                   kfld                    K_nsp
147200070125      *
147300070201      *
147400070201     C     Kar4          klist
147500070201     C                   kfld                    arbaas
147600070201     C                   kfld                    arblnp
147700070201     C                   kfld                    arbnrs
147800070201     C                   kfld                    arbnsp
147900070330     c                   kfld                    ktrck
148000070330     C     Kar5          klist
148100070330     C                   kfld                    arbaas
148200070330     C                   kfld                    arblnp
148300070330     C                   kfld                    arbnrs
148400070330     C                   kfld                    arbnsp
148500070330     c                   kfld                    ktrd
148600070201      *
148700070201     C     Ktab          klist
148800070201     C                   kfld                    tblkut
148900070201     C                   kfld                    tblcod
149000070201     C                   kfld                    tblkey
149100070412     C     Kfvv8         KLIST
149200070412     C                   KFLD                    arbtfa
149300070412     C                   KFLD                    knpg
149400070412     C                   KFLD                    arbdbr
149500070412     C     Kfvv81        KLIST
149600070412     C                   KFLD                    arbtfa
149700070412     C                   KFLD                    knpg
149800070412     C     KCLN          KLIST
149900070412     C                   KFLD                    KTFP
150000070412     C                   KFLD                    KTFA
150100070412     C                   KFLD                    KAAA              4 0
150200070412     C                   KFLD                    KMMM              2 0
150300070522     c*
150400070522     C     Karg1         klist
150500070522     C                   kfld                    arbaas
150600070522     C                   kfld                    arblnp
150700070522     C                   kfld                    arbnrs
150800070522     C                   kfld                    arbnsp
150900070412     c
151000070201     C                   z-add     1             tblkut
151100070205      * carico filiali £1
151200070205     C                   clear                   trul06ds
151300070205     C                   movel     '£1'          d06cod
151400070320     C                   movel     simfel        d06key
151500070205     C                   movel     trul06ds      kpjbu
151600070205     C                   CALL      'TRUL06R'
151700070205     C                   parm                    kpjba
151800070315     c                   movel     kpjbu         trul06ds
151900070315     C                   movea     lin           l1
152000070320     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
152100070320     C                   CLEAR                   trul06ds
152200070320     C                   MOVE      '£6'          D06COD
152300070320     c                   eval      d06esc = 'G'
152400070320     c                   movel     simfel        d06key
152500070320     C                   MOVEL     trul06ds      KPJBU
152600070320     C                   CALL      'TRUL06R'
152700070320     C                   PARM                    KPJBA
152800070320     C                   MOVEL     KPJBU         trul06ds
152900070320     C                   MOVEl     LIN           nog
153000030714
153100011227     C                   ENDSR
153200070125      *--------------------------------------------------------------------------------------------*
153300070125      * ordina - riscrive il sfl ordinato per località
153400070125      *--------------------------------------------------------------------------------------------*
153500070125     C     ordina        BEGSR
153600070125     C                   setoff                                       72
153700070125      * IMPOSTO ENTRATA AD 'A'
153800070125     C                   EVAL      snrr1 = 0
153900070125      *
154000070125     C                   CLEAR                   QLGSCB
154100070125     C                   CLEAR                   QLGSCB00
154200070125
154300071203     ** 2 campi chiave.
154400071203     C                   EVAL      QLGNBRK    = 2
154500071204     c                   select
154600071204     c* ordinamento per prv/rag.sociale destinatario
154700071204     c                   when      w_ord=1
154800071204     ** provincia dopo rag. sociale destinatario 2 byte,
154900071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
155000071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
155100070125     C                   EVAL      QLGDT      = carattere
155200070125     C                   EVAL      QLGSO      = Ascendente
155300070125     C                   EVAL      QLGKL(1)   = QLGSKL
155400071204     ** poi ragione sociale destinatario in pos 1, 22 byte
155500071204     C                   EVAL      QLGSP      = 1
155600071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
155700071203     C                   EVAL      QLGDT      = carattere
155800071203     C                   EVAL      QLGSO      = Ascendente
155900071203     C                   EVAL      QLGKL(2)   = QLGSKL
156000071204     c* ordinamento per rag.sociale/prv destinatario
156100071204     c                   when      w_ord=2
156200071204     C                   EVAL      QLGSP      = 1
156300071204     C                   EVAL      QLGSS      = %SIZE(v1srsd)
156400071204     C                   EVAL      QLGDT      = carattere
156500071204     C                   EVAL      QLGSO      = Ascendente
156600071204     C                   EVAL      QLGKL(1)   = QLGSKL
156700071204     ** ragione sociale a posizione 1, carattere, 24 byte,
156800071204     C                   EVAL      QLGSP      = 1 + %size(v1srsd)
156900071204     C                   EVAL      QLGSS      = %SIZE(v1sprd)
157000071204     C                   EVAL      QLGDT      = carattere
157100071204     C                   EVAL      QLGSO      = Ascendente
157200071204     C                   EVAL      QLGKL(2)   = QLGSKL
157300071204     c                   endsl
157400070125
157500070125      * Load other sort parameters.
157600070125     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
157700070125     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
157800070125     C                   EVAL      QLGRT     = 8
157900070125     C                   EVAL      QLGOKL    = 80
158000070125     C                   EVAL      QLGLKE    = 16
158100070125     C                   EVAL      QLGLSS    = 290
158200070125
158300070125      * Initialize Sort I/O API fields.
158400070125     C                   EVAL      QLGRL00  = QLGRL
158500070125     C                   EVAL      QLGRC00  = 1
158600070125     C                   CLEAR                   QUSEI
158700070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
158800070125
158900070125      * First step - Initialize the sort routine.
159000070125     C                   CALL      'QLGSORT'
159100070125     C                   PARM                    QLGSCB
159200070125     C                   PARM                    NotUsed
159300070125     C                   PARM                    NotUsed
159400070125     C                   PARM                    SizeList
159500070125     C                   PARM                    ReturnSize
159600070125     C                   PARM                    QUSEC
159700070125
159800070125      * Next step - Write records to I/O routine.
159900070125     C                   EVAL      QLGRT00 = Put
160000070125
160100070125     C                   DO        *hival        cx                4 0
160200070125
160300070316     C     cx            CHAIN     lrg1s01                            50
160400070125     c   50              leave
160500070125     ** Solo le righe con Selected = 'Y' sono riordinate,
160600070125     ** quindi per fare un ordinamento di tutte le righe
160700070125     ** metto 'Y' sempre.
160800070125     C                   EVAL      Selected = 'Y'
160900070125
161000070125     C                   CLEAR                   QUSEI
161100070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
161200070125     C                   CALL      'QLGSRTIO'
161300070125     C                   PARM                    QLGSCB00
161400070125     C                   PARM                    SflRcd
161500070125     C                   PARM                    NotUsed
161600070125     C                   PARM                    SizeList
161700070125     C                   PARM                    NotUsed
161800070125     C                   PARM                    QUSEC
161900070125
162000070125     C                   ENDDO
162100070125
162200070125      * Next step - Signal end of input, clear subfile for reload.
162300070125     C                   EVAL      QLGRT00 = EndPut
162400070125     C                   CLEAR                   QUSEI
162500070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
162600070125
162700070125     C                   CALL      'QLGSRTIO'
162800070125     C                   PARM                    QLGSCB00
162900070125     C                   PARM                    SflRcd
163000070125     C                   PARM                    NotUsed
163100070125     C                   PARM                    SizeList
163200070125     C                   PARM                    NotUsed
163300070125     C                   PARM                    QUSEC
163400070125
163500070316     C                   SETOff                                       21        *
163600070316     C                   WRITE     lrg1c01                                      *
163700070316     C                   SETOn                                        21        *
163800070316     C                   WRITE     lrg1z01                                      *
163900070125
164000070125      * Final step - Write the records back to the subfile.
164100070125     C                   EVAL      QLGRT00 = Get
164200070316     C                   DO        lasnrr1       x                 4 0
164300070125     C                   CLEAR                   QUSEI
164400070125     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
164500070125     C                   CALL      'QLGSRTIO'
164600070125     C                   PARM                    QLGSCB00
164700070125     C                   PARM                    NotUsed
164800070125     C                   PARM                    SflRcd
164900070125     C                   PARM                    QLGRL00
165000070125     C                   PARM                    NotUsed
165100070125     C                   PARM                    QUSEC
165200070125      *
165300070125     c                   z-add     x             snrr1
165400070316     C                   WRITE     lrg1s01
165500070125     C                   ENDDO
165600070125
165700070125      *
165800070125     C                   ENDSR
165801140211      *--------------------------------------------------------------------------------------------*
165802140211      * Richiamo driver per verificare eventuale stato stato alert
165803140211      *--------------------------------------------------------------------------------------------*
165804140211     C     sr_callv82    BEGSR
165805140211     c                   clear                   fnlv82ds
165806140211     c                   eval      ILV82AAS=arbaas
165807140211     c                   eval      ILV82LNP=arblnp
165808140211     c                   eval      ILV82NRS=arbnrs
165809140211     c                   eval      ILV82NSP=arbnsp
165810140211     c                   select
165811140307     c                   when      wcau='L'
165812140211     c                   eval      ILV82TIPA='RIC'
165813140307     c                   when      wcau='F'
165814140211     c                   eval      ILV82TIPA='FD'
165815140211     c                   endsl
165816140211     c                   call      'FNLV82R'
165817140211     c                   parm                    fnlv82ds
165818140211     c                   endsr
165900070315**
166000070403           Da CONTATTARE
166100070403          Da RICONTATTARE
166200070403ESCLUSA GESTIONE: Attesa GIACENZA
166300070403      CONTATTI e RICONTATTI
166400070330**
166500070319Filiale in gestione errata
166600070316Causale contatto obbligatoria
166700070402Selezionare per Numero Spedizione OPPURE altre parzializzazioni
166800070402Spedizione inesistente
166900070316Parzializzazione per ragione sociale errata: inserire correttamente
167000070316Tipo Spedizioni obbligatorio
167100070503Gestione "Tassative" valida solo per spedizioni per Supermercato               07
167200070403La spedizione non prevede abilitazione in base alla causale contatto immessa
167300070319Data consegna richiesta mancante o errata
167400070319Selezionare per "Tipo Spedizioni" OPPURE per "Tassative"
167500070319Filiale in gestione non abilitata alla Gestione Contatti Destinatario
167600070402Nessun contatto da gestire per la selezione effettuata
167700070402Spedizione non gestibile: bloccata con blocco " "
167800070402Spedizione non gestibile: consegnata o in distinta                             14
167900070402Spedizione non gestibile: linea di arrivo non in gestione                      15
168000070403Spedizione già abilitata in base alla causale contatto immessa                 16
168100070402Impossibile selezionare "Appuntamenti" in "Attesa di Giacenza"                 17
168200070403Spedizione già abilitata :F13 per ribloccare la spedizione                     18
168300070503Spedizione già abilitata                                                       19
168400070503Per riabilitare una spedizione con F13, specificare causale contatto           20
168500070504La spedizione non prevede abilitazioni                                         21
168600071126Per selezionare per data limite abbin. richiedere solo le sped.di f.v.abbinati 22
168700071126Data errata                                                                    23
168800071126Ora errata                                                                     24
168900071128Immettere anche la data oppure lasciare vuota l'ora                            25
