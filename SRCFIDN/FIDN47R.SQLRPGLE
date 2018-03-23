000100121105     /*CMD addlible lib(FILTRAPRD) position(*last)
000200121105     /*PRM dbgview(*source)
000300121105     /*END rmvlible lib(FILTRAPRD)
000400121105     /*END
000500121105
000600990212     H DECEDIT('0,') DATEDIT(*DMY.)
000700981204      *----------------------------------------------------*
000800990212      *  ELENCO C.A.
000900981204      *----------------------------------------------------*
001000981204
001100981204      *--------------------------------------------------------------*
001200981204      *  RIEPILOGO INDICATORI
001300981204      *--------------------------------------------------------------*
001400990216      * 01 - on - sede    off - filiale
001500990311      * 02/05 - of su file printer
001600990420      * 06 - ci sono della C.A. in attesa di ricalcolo
001700990429      * 07 - Stampo alcune selezioni solo per la stampa interna
001800000214      * 08 - Condiziona l'alta intensita' a stampa
001900000214      * 09 - Non stampo i dati della responsabilita'
002000040726      * 13 - stampo anche la descrizione pezzi danneggiati/mancanti
002100990311      * 30 - ERRORE comodo
002200981204      * 31 - ERRORE comodo
002300981204      * 32 - ERRORE comodo
002400981204      * 99 - Pilota la stampa della testata la prima volta
002500981204      *--------------------------------------------------------------*
002600981204
002700090320     Fazcae01l  IF   E           K DISK
002800090323     Fazcae02l  IF   E           K DISK    RENAME(azcae000:azcae002)
002900090320     FFNDCT01L  IF   E           K DISK
003000990215     FFNDCT11L  IF   E           K DISK    RENAME(fndct000:fndct011)
003100990215     FFNDKA01L  IF   E           K DISK
003200040803     FFNDCK01L  IF   E           K DISK
003300990215     FFNDCD01L  IF   E           K DISK
003400990218     FFNDCR02L  IF   E           K DISK
003500090319     FFNDCR04L  IF   E           K DISK    rename(fndcr000:fndcr004)
003600090319     FFNDCR05L  IF   E           K DISK    rename(fndcr000:fndcr005)
003700990503     FFNDRR01L  IF   E           K DISK
003800000215     FFNDCL01L  IF   E           K DISK
003900000215     FFNDCF01L  IF   E           K DISK
004000040726     fFNDCS01L  if   e           k disk
004100990406     FTNTBE01L  UF A E           K DISK
004200990512     FTNTBE02L  iF   E           K DISK
004300990512     F                                     RENAME(tntbe000:tntbe002)
004400990212     FAZORG01L  IF   E           K DISK
004500070111     FAZORG05L  IF   E           K DISK
004600990219     F                                     RENAME(AZORG:AZORG2)
004700990215     FFNBLP01L  IF   E           K DISK    USROPN
004800020821     f                                     prefix(ARB:3)
004900990215     FFNARB01L  IF   E           K DISK    USROPN
005000990215     FFNBLT01L  IF   E           K DISK    USROPN
005100020821     f                                     prefix(ART:3)
005200990215     FFNART01L  IF   E           K DISK    USROPN
005300991116     FFIAR601l  IF   E           K DISK    USROPN
005400040531     FFIAR531C  if   e           k disk    usropn
005500060124     FFIAR501l  if   e           k disk    usropn
005600060124     f                                     rename(fiar5000:fiar5fil)
005700981210     FTABEL00F  IF   E           K DISK
005800090319     FWFRDN51L  UF A E           K DISK    usropn
005900060601     FFNDET01L  IF   E           K DISK
006000990809     FTITAS30C  IF   E           K DISK    usropn
006100040206     fTITAA30C  if   e           k disk    usropn
006200000214     FTITA430C  IF   E           K DISK    usropn
006300990218     FTNTMD01L  IF   E           K DISK    usropn
006400020821     f                                     prefix(ARB:3)
006500040204      *
006600040212     fWFDDN01L  Uf   e           k disk    usropn
006700040212     f                                     rename(WFDDN000:WFDDN01)
006800040212     fWFDDN00F  o    e             disk    usropn
006900040204      *
007000990408     Fqsysprt   O    F  132        PRINTER oflind(*INOF)
007100121219     FFIDN44P   O    E             PRINTER oflind(*IN02) infds(PRTFBK)  usropn
007200990212      *------------------------------------------------------------------------*
007300990212      * SKIERE
007400990212      *--------------------------------------------
007500990324     D TES             S             35    DIM(2) CTDATA PERRCD(1)              TESTATA STAMPA
007600121219     D ovrprt          S              1    DIM(61) CTDATA PERRCD(61)
007700121219     d
007800121219     D L6              S              3  0 DIM(30)                              P.O.GESTITI
007900990503     D W38             S              1    DIM(38)                              DI WORK X QCMD
008000990503     D W65             S              1    DIM(75)                              DI WORK X QCMD
008100121106     D $ksb            s              7    dim(5)
008200050610     d* p.o. abilitati
008300050610     D $pog            s              3    dim(250)
008400050610     D $DIG            s              1    dim(20)
008500050610     D $ARG            s              3    dim(50)
008600990503     D*  Schiere di totale
008700060601     D fTDD            S              2    DIM(40)                              P.O.GESTITI
008800060601     D fTAS            S             10  0 DIM(40)                              P.O.GESTITI
008900060601     D fTAC            S             13  3 DIM(40)                              P.O.GESTITI
009000060601     D fCFL            S              3    DIM(45)                              P.O.GESTITI
009100060601     D fTFS            S             10  0 DIM(45)                              P.O.GESTITI
009200060601     D fTFC            S             13  3 DIM(45)                              P.O.GESTITI
009300990503     D
009400990513     D* Descrizione e raggruppamenti tipi anomalia
009500060601     D aTDS            S             25    DIM(40)                              P.O.GESTITI
009600060601     D aTDR            S              1    DIM(40)                              P.O.GESTITI
009700990513     D
009800060601     D aTDD            S              2    DIM(40)                              P.O.GESTITI
009900060601     D aTAS            S             10  0 DIM(40)                              P.O.GESTITI
010000060601     D aTAC            S             13  3 DIM(40)                              P.O.GESTITI
010100060601     D aCFL            S              3    DIM(45)                              P.O.GESTITI
010200060601     D aTFS            S             10  0 DIM(45)                              P.O.GESTITI
010300060601     D aTFC            S             13  3 DIM(45)                              P.O.GESTITI
010400990510     D
010500060601     D dTDD            S              2    DIM(40)                              P.O.GESTITI
010600060601     D dTAS            S             10  0 DIM(40)                              P.O.GESTITI
010700060601     D dTAC            S             13  3 DIM(40)                              P.O.GESTITI
010800060601     D dCFL            S              3    DIM(45)                              P.O.GESTITI
010900060601     D dTFS            S             10  0 DIM(45)                              P.O.GESTITI
011000060601     D dTFC            S             13  3 DIM(45)                              P.O.GESTITI
011100990503     D
011200060601     D gTDD            S              2    DIM(40)                              P.O.GESTITI
011300060601     D gTAS            S             10  0 DIM(40)                              P.O.GESTITI
011400060601     D gTAC            S             13  3 DIM(40)                              P.O.GESTITI
011500060601     D gCFL            S              3    DIM(45)                              P.O.GESTITI
011600060601     D gTFS            S             10  0 DIM(45)                              P.O.GESTITI
011700060601     D gTFC            S             13  3 DIM(45)                              P.O.GESTITI
011800990503     D
011900060601     D wTDD            S              2    DIM(40)                              P.O.GESTITI
012000060601     D wTAS            S             10  0 DIM(40)                              P.O.GESTITI
012100060601     D wTAC            S             13  3 DIM(40)                              P.O.GESTITI
012200060601     D wCFL            S              3    DIM(45)                              P.O.GESTITI
012300060601     D wTFS            S             10  0 DIM(45)                              P.O.GESTITI
012400060601     D wTFC            S             13  3 DIM(45)                              P.O.GESTITI
012500110801     d
012600121127     D ksc_EX          S              7  0 DIM(1100)
012700121127     D ksc_tr          S              3  0 DIM(1100)
012800130909     D ksc_are         S              3  0 DIM(1100)
012900990212      *------------------------------------------------------------------------*
013000990212      * DS ESTERNE
013100990212      *--------------------------------------------
013200070111     D OG140         E DS
013300070111     D OG143         E DS
013400010910     D DGEDDN        E DS
013500010413     D DGEI          E DS
013600990215     D DS3A          E DS
013700120713     D*** DS3E          E DS
013800990309     D DS05          E DS
013900990309     D DS17          E DS
014000990408     D DSTD          E DS
014100990212     D UT§DSE0F      E DS
014200990212     D FIDN43DS      E DS
014300050610     D TIBS34DS      E DS
014400050610     D TRUL31DS      E DS
014500050610     D TRUL31DS2     E DS
014600121126     D Dcxq          E DS
014700121126     D DTAD          E DS
014800000214     D DTA4A         E DS
014900070927     D DSbl4E        E DS
015000990216     D DCCH          E DS
015100990217     D DTRE          E DS
015200050610     d Azuteds       e ds                  extname(Azute00f)
015300050610     d dDatiute      e ds
015400050610     d dLat          e ds
015500050610     d dute01        e ds
015600990215     D TRUL06DS      E DS
015700990215     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
015800990212     D KPJBA         E DS
015900990215     D DDCT01        E DS
016000990216     D DDCR01        E DS
016100040204     d dAR5ban       e ds                  inz
016200060124     d dAR5gen       e ds                  inz
016300990215     d TIBS69DS      E DS
016400990215     d TIBS02DS      E DS
016500040206     d TIBS05DS      E DS                  prefix(TB)
016600990408     d FIDN48DS      E DS
016700121127     d* DS x controllo riempimento schiere
016800121127     d trul0sds      e ds
016900121127
017000040210     d DS_cnaco      e ds                  inz  extname(CNACO00F)
017100040210     d DS_cnind      e ds                  inz  extname(CNIND00F)
017200040210     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
017300040210     d DS_fncls      e ds                  inz  extname(FNCLS00F)
017400040204     d DS_fndfa      e ds                  inz  extname(FNDFA00F)
017500040210     d FIDN05ds      e ds                  inz
017600040210     d   I05mod      e                     inz('C')
017700990212      *--------------------------------------------
017800990212      * DS INTERNE
017900990212      *--------------------------------------------
018000990212     D WLBDAT          DS                  INZ
018100990212     D  G02DAT                 1      8  0
018200990212     D  G02INV                 9     16  0
018300990212     D  G02ERR                17     17
018400990212     D  G02TGI                18     22  0
018500990215     D                 DS
018600990218     D  dctaac
018700990218     D  dctmgc
018800990218     D   DCTDCA                1      8  0
018900990218     D  dctfil
019000990218     D  dctnca
019100990218     D   DCTSPE                1     18  0
019200040726      *
019300040726     d  ds_keyca       ds                     inz
019400040726     d   wDCTaac                              like(DCTaac) inz
019500040726     d   wDCTfil                              like(DCTfil) inz
019600040726     d   wDCTnca                              like(DCTnca) inz
019700000119     d*
019800990215     D                 DS
019900990215     D  arbaas
020000990215     D  arbmgs
020100990215     D  arbdsp                 1      8  0
020200101126     d
020300990218     D PRTFBK          ds
020400990218     D   CURLINE             367    368I 0
020500981204      *------------------------------------------------------------------------*
020600990212      * definizione campi
020700990212      *--------------------------------------------
020800040206     d w0040           S              4  0 inz
020900990212     D W0140           S             14  0
021000040211     d w0070           s              7  0 inz
021100990212     D Wdtgio          S              8  0
021200990212     D dateu           S              8  0
021300990217     D WORA            S              6  0
021400990322     D WPOSB           S              2  0
021500990322     D WPOSM           S              2  0
021600990322     D WPOSD           S              2  0
021700990503     D ZZ              S              3  0
021800990503     D XX              S              3  0
021900990513     D YY              S              3  0
022000010312     D PP              S              3  0
022100121127     D Indx            S              4  0
022200990212     d
022300990322     D WA43KSM         S              7
022400990322     D WA43KSD         S              7
022500990322     D Wrag            S             14
022600990322     D WBOL            S              1
022700990322     D WBENEF          S              1
022800990420     D WNODET          S              1
022900990420     D WMITDES         S              1
023000990217     D WCONFR          S              1
023100990217     D WSELBOL         S              1
023200990217     D WRECSTA         S              1
023300990309     D WRECSTAa        S              1
023400990309     D WRECSTAd        S              1
023500990312     D WRECSTAn        S              1
023600990217     D WTROVADET       S              1
023700990309     D wbrkd           S              1
023800990309     D wbrka           S              1
023900990312     D wbrkn           S              1
024000990310     D wnuovopo        S              1
024100990323     D wnuovo          S              1
024200990312     D wSTADIS         S              1
024300990312     D wSTAARE         S              1
024400990312     D wSTAspe         S              1
024500990312     D wSTAsped        S              1
024600990312     D wSTAspea        S              1
024700990312     D wSTAspen        S              1
024800990406     D wscrivi         S              1
024900990430     D westero         S              1
025000990503     D wtiptot         S              1
025100990407     D Wlimris         s             13  3
025200000323     D SAVcfl          s              3
025300050613     D Wcontrlnplna    s              1
025400050708     D Wabiute         s              1
025500090323     D Wabiter         s              1
025600090320     d Kepa            s                   like(caeepa)
025700090323     d Ktfpa           s                   like(caetfp)
025800090320     d wabi            s                   like(UteAut)
025900040210     D kcrc            s                   LIKE(dctcrc)
026000990504     D wvlt            s                   LIKE(DCTvpv)
026100990406     D Wragr           S                   LIKE(§tadragr)
026200990406     D Ktae            S                   LIKE(dcttad)
026300990406     D Kcch            S                   LIKE(dctcch)
026400990406     D Kfpr            S                   LIKE(dctfpr)
026500990408     D Kcrs            S                   LIKE(rdncrs)
026600990312     D SAVSPE          S                   LIKE(dctspe)
026700070111     D** KCAR            S                   LIKE(orgcar)
026800990218     D P3CTGC          S                   LIKE(p3ctpc)
026900990218     D P3CTGS          S                   LIKE(p3ctpS)
027000990310     D P3CTDC          S                   LIKE(p3ctpc)
027100990310     D P3CTDS          S                   LIKE(p3ctpS)
027200990310     D P3CTAC          S                   LIKE(p3ctpc)
027300990310     D P3CTAS          S                   LIKE(p3ctpS)
027400990312     D P3CTNC          S                   LIKE(p3ctpc)
027500990312     D P3CTNS          S                   LIKE(p3ctpS)
027600990218     D Kcod            S                   LIKE(tblcod)
027700990216     D Kkey            S                   LIKE(tblkey)
027800990216     D KTRC            S                   LIKE(dkatrc)
027900990216     D KSEG            S                   LIKE(dcrseg)
028000990217     D KKE1            S                   LIKE(tbeke1)
028100990217     D KKE2            S                   LIKE(tbeke2)
028200990217     D KSIF            S                   LIKE(tbesif)
028300990217     D KLIN            S                   LIKE(tbelin)
028400990217     D KCODt           S                   LIKE(tbecod)
028500990218     D SAVORD1         S                   LIKE(dctord1)
028600990312     D KFIL            S                   LIKE(dctfil)
028700120713     D wFIL            S                   LIKE(dctfil)
028800990406     D SAVTAD          S                   LIKE(dcttad)
028900990406     D WTAD            S                   LIKE(dcttad)
029000990406     D SAVCCH          S                   LIKE(dctcch)
029100990406     D WCCH            S                   LIKE(dctcch)
029200050711     D spedata         S                   LIKE(arbdsp)
029300050711     D spersm          S                   LIKE(arbrsm)
029400050711     D spersd          S                   LIKE(arbrsd)
029500050711     D speccm          S                   LIKE(arbccm)
029600050711     D speccd          S                   LIKE(arbccm)
029700060601     D p1drag          S                   LIKE(§tadragr)
029800121127     d savkpjbu        s                   Like(kpjbu)
029900050711     d
030000040210     D datadmy         S               d   datfmt(*dmy)
030100090319     D wdaliso         S               d   datfmt(*iso)
030200120713     D dataiso         S               d   datfmt(*iso)
030300090319     D waliso          S               d   datfmt(*iso)
030400090319     D wdal            S              8  0
030500090319     D wal             S              8  0
030600990218     D SAVORD2         S              3
030700990312     D SAVINT2         S              1
030800990312     D p3cint          S              1
030900121126     c* filiale a zero per chain per clienti particolari che vengono memorizzati solo per terminal
031000121126     D kfil_0          S              3  0 inz(0)
031100121126     D wben_n          S              7  0 inz(0)
031200121126     D kter            S              3  0
031300130909     D kter_p          S              3  0
031400090323     D kterpo          S              3  0
031500090319     D kttf            S              1
031600090320     D ktie            S              1
031700110912     D ktieAV          S              1
031800121126     D tipocli         S              1
031900090323     D Kteralfa        S              3
032000990218      *------------------------------------------------------------------------*
032100990218      * CONSTANTI
032200990218      *------------------------------------------------------------------------*
032300990218     DCOSGEN           C                   'TOTALE GENERALE :'
032400090326     DCOSPAR           C                   'TOTALE Parziale :'
032500990310     DCOSAREA          C                   'TOTALE AREA     :'
032600990310     DCOSDISTR         C                   'TOTALE DISTRETTO:'
032700110207     DCOSMAN           C                   '    MANUALE  '
032800110207     DCOSAUT           C                   '             '
032900110207     DCOSRIC           C                   'ATTESA RICALC'
033000990218     DCOSPOAP          C                   'P.O Apertura    '
033100990218     DCOSPORE          C                   'P.O Responsabile'
033200990218     DCOSNES           C                   'Nessun Respons.'
033300990430     DCOSESCL          C                   'escluso   Distretto '
033400990430     DCOSINTER         C                   'Internazionale      '
033500990511     DCOSFILTRO        C                   'per raggr.Respons.'
033600990504     DCOSTIPANOM       C                   'per tipo  anomalia'
033700990212      *------------------------------------------------------------------------*
033800990809     ITITAS000
033900990218     I              TASAAS                      ARBAAS
034000990218     I              TASLNP                      ARBLNP
034100990218     I              TASNRS                      ARBNRS
034200990218     I              TASNSP                      ARBNSP
034300990218     I              TASMGS                      ARBMGS
034400990218     I              TASNCL                      ARBNCL
034500990218     I              TASKSC                      ARBKSC
034600990408     I              TASCTR                      ARBCTR
034700990408     I              TASTSP                      ARBTSP
034800990218     I              TASCCM                      ARBCCM
034900990218     I              TASLNA                      ARBLNA
035000990408     I              TASIAS                      ARBIAS
035100990408     I              TASCCA                      ARBCCA
035200000214     I              TASRMN                      ARBRMN
035300060124     I              TASpkf                      ARBpkf
035400990809     ITITAS010
035500990218     I              TASAAS                      ARBAAS
035600990218     I              TASLNP                      ARBLNP
035700990218     I              TASNRS                      ARBNRS
035800990218     I              TASNSP                      ARBNSP
035900990218     I              TASMGS                      ARBMGS
036000990218     I              TASNCL                      ARBNCL
036100990218     I              TASKSC                      ARBKSC
036200990408     I              TASCTR                      ARBCTR
036300990408     I              TASTSP                      ARBTSP
036400990218     I              TASCCM                      ARBCCM
036500990218     I              TASLNA                      ARBLNA
036600990408     I              TASIAS                      ARBIAS
036700990408     I              TASCCA                      ARBCCA
036800000215     I              TASRMN                      ARBRMN
036900060124     I              TASpkf                      ARBpkf
037000990809     ITITASP00
037100990218     I              TASAAS                      ARBAAS
037200990218     I              TASLNP                      ARBLNP
037300990218     I              TASNRS                      ARBNRS
037400990218     I              TASNSP                      ARBNSP
037500990218     I              TASMGS                      ARBMGS
037600990218     I              TASNCL                      ARBNCL
037700990218     I              TASKSC                      ARBKSC
037800990408     I              TASCTR                      ARBCTR
037900990408     I              TASTSP                      ARBTSP
038000990218     I              TASCCM                      ARBCCM
038100990218     I              TASLNA                      ARBLNA
038200990408     I              TASIAS                      ARBIAS
038300990408     I              TASCCA                      ARBCCA
038400000215     I              TASRMN                      ARBRMN
038500060124     I              TASpkf                      ARBpkf
038600990218      *------------------------------------------------------------------------*
038700990218     c                   clear                   p3ctps
038800990218     c                   clear                   p3ctpc
038900990218     c                   clear                   p3ctgs
039000990218     c                   clear                   p3ctgc
039100040204      *
039200990212     c* Stampo la videata delle selezioni
039300990215     c                   exsr      PRESELEZ
039400990428     c* USO INTERNO
039500990429     c                   eval      *in07='1'
039600990309     c                   write     dn44t1
039700990309     c                   write     dn44ts
039800990309     c                   write     dn4402
039900990309     c                   eval      *in02='1'
040000990429     c                   eval      *in07='0'
040100990218     c
040200990219     c*
040300050613if  1c                   if        (simfel>0  AND i43ord='1'  and i43dst=' ' and
040400050613     c                             i43are=*blanks)   or
040500090402if  1c                             (simfel=0  AND i43ord='1'  and i43fil>0)
040600090402     c********                     or (simfel>0  and i43ord='2'  AND i43prs>0)
040700050614     c
040800990219    2c                   if        i43ord='1'
040900990219     c                   eval      kfil=i43fil
041000990219    2c                   endif
041100050614     c
041200990219    2c                   if        i43ord='2'
041300990219     c                   eval      kfil=i43prs
041400050614    2c                   endif
041500050614    c
041600050614     c* Per sede decodifico distretto e area
041700050614     c                   if        simfel=0
041800050614     c                   eval      p3cdst=i43dst
041900050614     C                   movel     p1ddst        p3ddst
042000050614     C                   movel     p1ddst        p3dds2
042100050614     c                   movel     i43are        p3care
042200050614     C                   movel     p1dare        p3dare
042300050614     C                   movel     p1dare        p3dar2
042400050614     c                   endif
042500050614     c
042600990219     c                   EXSR      LETTURE
042700990219     c
042800040202x   1c                   else
042900990219     c
043000990219     c* Verifico cosa stampare
043100040202sel 2c                   select
043200040202w   2c                   when      i43dst<>*blanks and i43are<>*blanks
043300990219     c
043400990219     c* dipende dall'ordinamento verificare il P.O.
043500040202if  3c                   if        i43ord='1' and i43fil>0
043600990219     c     i43fil        chain     azorg01l                           34
043700040202x   3c                   else
043800070111     c     korg2         chain     azorg05l                           34
043900040202e   3c                   endif
044000990219     c
044100050613w   2c                   when      i43dst<>*blanks  and i43dst<>'T'
044200070111     c     i43dst        chain     azorg05l                           34
044300040202x   2c                   other
044400070111     c     *loval        setll     azorg05l
044500070111     c                   read      azorg05l                               34
044600040202e   2c                   endsl
044700990219     c
044800990212     c
044900040202do  2c     *in34         doweq     *off
045000990219     C* ESCLUDO RECORD ANNULLATI E DISTRETTO SEDE
045100040202if  3C     ORGFAG        IFEQ      'F'
045200990219     C     ORGFVA        ANDEQ     *BLANKS
045300990219     C     ORGFL3        ANDNE     '9'
045400990219     C     ORGFAG        OREQ      'A'
045500990219     C     ORGFVA        ANDEQ     *BLANKS
045600990219     C     ORGFL3        ANDNE     '9'
045700990311     c
045800990219     c* Decodifico distretto e area
045900990219     c                   clear                   savord1
046000990219     c                   clear                   savord2
046100990219     c
046200050613if  4c                   if        *in01  or i43dst<>' ' or i43are<>*blanks
046300990219     c                   exsr      DECODTES2
046400040202e   4c                   endif
046500990309     c
046600990309     c
046700090323     c                   if        i43ord='1'
046800990219     c                   eval      kfil=orgfil
046900090323     c                   else
047000090323     c                   clear                   kfil
047100090323     c                   eval      kterpo=orgfil
047200090323     c                   eval      kteralfa=%editc(orgfil:'X')
047300090323     c                   endif
047400090323     c
047500990219     c                   EXSR      LETTURE
047600040202e   3c                   endif
047700990219     c
047800040202sel 3c                   select
047900040202w   3c                   when      i43dst<>*blanks and i43are<>*blanks
048000990219     c
048100990219     c* dipende dall'ordinamento verificare il P.O.
048200040202if  4c                   if        i43ord='1' and i43fil>0
048300990301     c                   seton                                            34
048400040202x   4c                   else
048500070111     c     korg2         reade     azorg05l                               34
048600040202e   4c                   endif
048700990219     c
048800050613w   3c                   when      i43dst<>*blanks  and i43dst<>'T'
048900070111     c     i43dst        reade     azorg05l                               34
049000040202x   3c                   other
049100070111     c                   read      azorg05l                               34
049200040202e   3c                   endsl
049300040202e   2c                   enddo
049400990310     c
049500040202      * STAMPO GLI ULTIMI TOTALI
049600050613if  3c                   if        *in01 or i43dst<>' ' or i43are<>*blanks
049700990312     c* Ultimo totale di area
049800990312     c                   EXSR      TOTAREA
049900990312     c
050000990312     c* Ultimo totale di distretto
050100990310     c* Non lo stampo se e' stata chiesta un'area specifica
050200040209      * né se non è stata richiesta la creazione del file dettaglio
050300040209     c                   if             i43are = *blanks
050400990310     c                   EXSR      TOTDISTR
050500990310     c                   endif
050600040202e   3c                   endif
050700990512     c
050800040202e   1c                   endif
050900981204     c*
051000050614     c* Se ho delle bolle stampo i totali generali
051100040209      * (se non è stata richiesta la creazione del file dettaglio)
051200050614    1c                   if        p3ctgs > 0
051300050614if  1c                   if        (i43dst=' ' and i43are='   ')  or
051400050614     c                             (i43dst='T') or
051500050614     c                             i43prs>0 or i43fil>0
051600990218     c                   eval      p3ctps=p3ctgs
051700050614     c                   eval      p3ctpc=p3ctgc
051800990218     c
051900990218     c                   movel     'G'           WTOT              1
052000990218     c                   clear                   p3dtotr
052100990218     c                   clear                   p3dtoan
052200990218     c                   clear                   p3dtopo
052300990218     c                   clear                   p3dtode
052400990312     c* non li stampo nella distretto e nell'area
052500990312     c                   eval      wstadis='N'
052600990312     c                   eval      wstaare='N'
052700990218     c                   exsr      EXCPTOUT
052800040202x   1c                   else
052900990312     c                   eval      wstadis=' '
053000990312     c                   eval      wstaare=' '
053100040202e   1c                   endif
053200050614x   1c                   else
053300050614     c                   eval      wstadis=' '
053400050614     c                   eval      wstaare=' '
053500050614e   1c                   endif
053600990309     c
053700990428     c* USO INTERNO
053800040209      * (se non è stata richiesta la creazione del file dettaglio)
053900040209if  1c                   if             i43usi = 'S'
054000990428     c                   if        wstaspe=' '
054100990312     c                   write     dn44z3
054200990312     c                   endif
054300990312     c                   write     dn44e2
054400040202e   1c                   endif
054500990309     c
054600990406     c* Se devo scrivere il file reporto, memorizzo in tabella la DS di
054700990406     c*  lancio
054800040202if  1c                   if        i43crp='S'
054900990406     c                   EVAL      KCODT='STD'
055000990406     c                   EVAL      Kke1='REP            '
055100990406     c                   clear                   kke2
055200990406     c                   clear                   klin
055300990406     c                   clear                   ksif
055400990406     c     ktbe          chain     tntbe01l                           32
055500990406     c                   if        *in32
055600990406     c                   clear                   tntbe000
055700990406     c                   eval      tbecod='STD'
055800990406     c                   eval      tbeke1='REP'
055900990406     c                   endif
056000990406     c                   movel(p)  fidn43ds      tbeuni
056100990406     c   32              write     tntbe000
056200990406     c  N32              update    tntbe000
056300040202e   1c                   endif
056400990406     c
056500040202if  1c                   if        i43crp='S'
056600990408     c                   clear                   fidn48ds
056700990408     c                   eval      i48tla='C'
056800990408     c                   call      'FIDN48R'
056900990408     c                   parm                    kpjba
057000990408     c                   parm                    fidn48ds
057100040202e   1c                   endif
057200990414     c*
057300050613     c     fine          tag
057400981204     c                   eval      *inlr = *on
057500990215      *------------------------------------------------------------------------*
057600990215      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
057700990215      *------------------------------------------------------------------------*
057800990215     C     PRESELEZ      BEGSR
057900050708     c                   eval      wabiute='N'
058000090320     c                   eval      wabiter='N'
058100990324     c*
058200990324     c* Testata
058300990324     c                   if        i43ftt='C'
058400990324     c                   eval      v1ctes=tes(2)
058500990324     c                   else
058600990324     c                   eval      v1ctes=tes(1)
058700990324     c                   endif
058800050614     c* Terminal responsabile
058900050614     c                   if        i43ter>0
059000050614     c     i43ter        chain     azorg01l                           30
059100050614     c  n30              eval      p1dter=orgdes
059200050614     c   30              clear                   p1dter
059300050614     c*
059400050614     c* riempo anche i campi dell'area e del distretto
059500070118     c**                 movel     orgcar        i43are
059600070118     c                   movel     orgde0        og140
059700070118     c                   movel     §ogara        i43are
059800050614     c                   movel     orgfl3        i43dst
059900050708     c
060000050708     c                   movel     i43ter        w003a
060100050708     c     w003a         lookup    $pog                                   31
060200050708     c                   if        *in31
060300050708     c                   eval      wabiute='S'
060400050614     c                   endif
060500050708     c                   endif
060600990219     c* Distretto
060700050613     c                   if        i43dst<>*blanks  and i43dst<>'T'
060800990219     C                   movel     '17'          kCOD
060900990219     C                   movel(P)  i43dst        kKEY
061000990219     C     ktab          CHAIN     TABEL                              31
061100990219     C  n31              movel     TBLUNI        p1ddst
061200070110     c* Se non è stato richiesto il terminal responsabile
061300070110     c                   if        i43ter=0
061400070110     c     i43dst        lookup    $dig                                   31
061500070110     c                   if        *in31
061600070110     c                   eval      wabiute='S'
061700070110     c                   endif
061800070110     c                   endif
061900070110     c
062000990219     c                   else
062100050613     c   01              movel     'TUTTI'       p1ddst
062200050613     c  n01              if        i43dst='T'
062300050614     c                   movel     '- TUTTI'     p1ddst
062400050613     c                   else
062500050613     c                   movel     *blanks       p1ddst
062600990219     c                   endif
062700050613     c                   endif
062800990219     c* Area
062900990219     c                   if        i43are<>*blanks
063000990219     C                   movel     '05'          kCOD
063100990219     C                   movel(P)  i43are        kKEY
063200990219     C     ktab          CHAIN     TABEL                              31
063300990219     C  n31              movel     TBLUNI        p1dare
063400070110     c
063500070110     c                   if        i43ter=0
063600070110     c     i43are        lookup    $arg                                   31
063700070110     c                   if        *in31
063800070110     c                   eval      wabiute='S'
063900070110     c                   endif
064000070110     c                   endif
064100070110     c
064200990219     c                   else
064300050614     c   01              movel     'TUTTE'       p1dare
064400050613     c  n01              if        i43dst<>' '
064500050614     c                   movel     'TUTTE'       p1dare
064600050613     c                   else
064700050613     c                   movel     *blanks       p1dare
064800990219     c                   endif
064900050613     c                   endif
065000990219     c
065100990212     c* Decodifiche P.O.
065200050610     c                   if        i43prs>0
065300990212     c     i43prs        chain     azorg01l                           30
065400990212     c  n30              eval      p1dprs=orgdes
065500990212     c   30              clear                   p1dprs
065600050708     c
065700050708     c                   movel     i43prs        w003a
065800050708     c     w003a         lookup    $pog                                   31
065900050708     c                   if        *in31
066000050708     c                   eval      wabiute='S'
066100090320     c                   endif
066200050610     c                   endif
066300990309     c
066400990212     c
066500990215     c                   if        i43psl>0
066600990212     c     i43psl        chain     azorg01l                           30
066700990212     c  n30              eval      p1dpsl=orgdes
066800990212     c   30              clear                   p1dpsl
066900990215     c* Carico la sua l6
067000990215     C                   CLEAR                   L6
067100990215     C                   CLEAR                   TRUL06DS
067200990215     C                   MOVE      '£6'          D06COD
067300990215     C                   MOVEL(P)  I43psl        D06key
067400990215     C                   MOVEL(P)  TRUL06DS      KPJBU
067500990215     C                   CALL      'TRUL06R'
067600990215     C                   PARM                    KPJBA
067700990215     C                   MOVEL     KPJBU         TRUL06DS
067800990215     C                   MOVEA     LIN           L6
067900050708     c
068000050708     c                   movel     i43psl        w003a
068100050708     c     w003a         lookup    $pog                                   31
068200050708     c                   if        *in31
068300050708     c                   eval      wabiute='S'
068400050708     c                   endif
068500990215     c                   endif
068600990215     c
068700050708     c                   if        i43fil>0
068800990212     c     i43fil        chain     azorg01l                           30
068900990212     c  n30              eval      p1dfil=orgdes
069000990212     c   30              clear                   p1dfil
069100050610     c
069200050708     c
069300050708     c                   movel     i43fil        w003a
069400050708     c     w003a         lookup    $pog                                   31
069500050708     c                   if        *in31
069600050708     c                   eval      wabiute='S'
069700050708     c                   endif
069800050610     c                   endif
069900070423     c* Beneficiario
070000110210     c                   clear                   $ksb
070100121106     c                   clear                   xx
070200070423     c                   if        i43ksb>0
070300070423     C                   clear                   TIBS69DS
070400070423     c                   movel     i43ksb        I69kac
070500070423      *
070600070423     C                   CALL      'TIBS69R'
070700070423     C                   PARM                    tibs69DS
070800070423     C                   PARM                    DS_cnaco
070900070423     C                   PARM                    DS_cnind
071000070423     C                   PARM                    DS_cnclp
071100070423     C                   PARM                    DS_fncls
071200070423    2c                   if        o69err=' '
071300070423     c                   movel     acorag        p1dksb
071400070423    2c                   endif
071500121106      *
071600121106     c                   add       1             xx
071700110210     c                   movel     i43ksb        $ksb(xx)
071800110210    2c                   endif
071900110210     c                   if        i43ksb2>0
072000110210     C                   clear                   TIBS69DS
072100110210     c                   movel     i43ksb2       I69kac
072200110210      *
072300110210     C                   CALL      'TIBS69R'
072400110210     C                   PARM                    tibs69DS
072500110210     C                   PARM                    DS_cnaco
072600110210     C                   PARM                    DS_cnind
072700110210     C                   PARM                    DS_cnclp
072800110210     C                   PARM                    DS_fncls
072900110210    2c                   if        o69err=' '
073000110210     c                   movel     acorag        p1dksb2
073100110210    2c                   endif
073200121106      *
073300121106     c                   add       1             xx
073400110210     c                   movel     i43ksb2       $ksb(xx)
073500110210    2c                   endif
073600110210     c                   if        i43ksb3>0
073700110210     C                   clear                   TIBS69DS
073800110210     c                   movel     i43ksb3       I69kac
073900110210      *
074000110210     C                   CALL      'TIBS69R'
074100110210     C                   PARM                    tibs69DS
074200110210     C                   PARM                    DS_cnaco
074300110210     C                   PARM                    DS_cnind
074400110210     C                   PARM                    DS_cnclp
074500110210     C                   PARM                    DS_fncls
074600110210    2c                   if        o69err=' '
074700110210     c                   movel     acorag        p1dksb3
074800110210    2c                   endif
074900121106      *
075000121106     c                   add       1             xx
075100110210     c                   movel     i43ksb3       $ksb(xx)
075200110210    2c                   endif
075300110210     c                   if        i43ksb4>0
075400110210     C                   clear                   TIBS69DS
075500110210     c                   movel     i43ksb4       I69kac
075600110210      *
075700110210     C                   CALL      'TIBS69R'
075800110210     C                   PARM                    tibs69DS
075900110210     C                   PARM                    DS_cnaco
076000110210     C                   PARM                    DS_cnind
076100110210     C                   PARM                    DS_cnclp
076200110210     C                   PARM                    DS_fncls
076300110210    2c                   if        o69err=' '
076400110210     c                   movel     acorag        p1dksb4
076500110210    2c                   endif
076600121106      *
076700121106     c                   add       1             xx
076800110210     c                   movel     i43ksb4       $ksb(xx)
076900110210    2c                   endif
077000110210     c                   if        i43ksb5>0
077100110210     C                   clear                   TIBS69DS
077200110210     c                   movel     i43ksb5       I69kac
077300110210      *
077400110210     C                   CALL      'TIBS69R'
077500110210     C                   PARM                    tibs69DS
077600110210     C                   PARM                    DS_cnaco
077700110210     C                   PARM                    DS_cnind
077800110210     C                   PARM                    DS_cnclp
077900110210     C                   PARM                    DS_fncls
078000110210    2c                   if        o69err=' '
078100110210     c                   movel     acorag        p1dksb5
078200110210    2c                   endif
078300110210     c
078400121106     c                   add       1             xx
078500110210     c                   movel     i43ksb5       $ksb(xx)
078600110210    2c                   endif
078700070423     c
078800110210     c* Verifico se utente abilitato ai p.o. del beneficiario
078900110211     c                   if        $ksb(1)>*zeros and wabiute<>'S'
079000110210     c                   eval      wabiute='S'
079100110210     c                   z-add     1             xx
079200121106     c                   dow       xx <= %elem($ksb)  and
079300121106     c                             $ksb(xx)<>*blanks
079400121106      *
079500110210     c                   movel     $ksb(xx)      w003a
079600070423     c     w003a         lookup    $pog                                   31
079700110210     c                   if        not *in31
079800110210     c                   eval      wabiute='N'
079900070423     c                   endif
080000110210     c                   add       1             xx
080100110210    2c                   enddo
080200110210    2c                   endif
080300121106      *
080400070423     c* Cerco l'ultima posizione piena della ragione sociale
080500070423     c                   if        i43rsb<>*blanks
080600070423     c     ' '           checkr    i43rsb        wPOSB
080700070423     c                   endif
080800050610     c
080900050708     c
081000090320     c* se non sono abilitato come utente e ho chiesto per p.o. responsab
081100090320     c*  e ordinamento '2' --> controllo se abilitato il terminal
081200090320     c*  altrimento è lnp/lna
081300090323    1c                   if        wabiute='N' and i43ord='2' and i43prs>0
081400090320     c* cerco per terminal di partenza
081500090320     c                   eval      kepa='P'
081600090323     c                   eval      ktfpa=i43prs
081700090320     c     kcae          setll     azcae01l
081800090320     c     kcae          reade     azcae01l
081900090323    2c                   dow       not %eof(azcae01l)  and wabiter='N'
082000090323    3c                   if        (caedde<=wal  and caedsc>=wdal)
082100090320     c                   movel     caetfe        w003a
082200090320     c     w003a         lookup    $pog                                   31
082300090323    4c                   if        *in31
082400090320     c                   eval      wabiter='S'
082500090323    4c                   endif
082600090323    3c                   endif
082700090320     c
082800090320     c     kcae          reade     azcae01l
082900090323    2c                   enddo
083000090323     c
083100110210     c* se non trovato come terminal di partenza cerco come arrivo
083200090323    2c                   if        wabiter='N'
083300090323     c                   eval      kepa='A'
083400090323     c                   eval      ktfpa=i43prs
083500090323     c     kcae          setll     azcae02l
083600090323     c     kcae          reade     azcae02l
083700090323    3c                   dow       not %eof(azcae02l)  and wabiter='N'
083800090323    4c                   if        (caedde<=wal  and caedsc>=wdal)
083900090323     c                   movel     caetfe        w003a
084000090323     c     w003a         lookup    $pog                                   31
084100090323    5c                   if        *in31
084200090323     c                   eval      wabiter='S'
084300090323    5c                   endif
084400090323    4c                   endif
084500090323     c
084600090323     c     kcae          reade     azcae02l
084700090323    3c                   enddo
084800090323    2c                   endif
084900090323    1c                   endif
085000090320     c
085100090320     c* Se nessun p.o. di delle richieste è gestibile dall'utente oppure
085200090320     c*  nessun p.o. è stato immesso, devo controllare lnp/lna bolla
085300090323     c                   if        wabiute='N' and wabiter='N'
085400050708     c                   eval      wcontrlnplna='S'
085500050708     c                   endif
085600050708     c
085700050708     c* Le parzializzazioni sulle bolle non valgono ai fini delle
085800050708     c*  abilitazioni utente
085900990212     c     i43lnp        chain     azorg01l                           30
086000990212     c  n30              eval      p1dlnp=orgdes
086100990212     c   30              clear                   p1dlnp
086200990212     c     i43lna        chain     azorg01l                           30
086300990212     c  n30              eval      p1dlna=orgdes
086400990212     c   30              clear                   p1dlna
086500990212     c
086600990212     c* Ordinamento
086700990212     c                   if        i43ord='1'
086800990218     c                   eval      p1dord='Per NUMERO C.A.      '
086900990212     c                   endif
087000990212     c                   if        i43ord='2'
087100990218     c                   eval      p1dord='Per P.O. RESPONSABILE'
087200990504     c
087300990504     c* Carico i codici filtro
087400990504     c     'A1'          setll     fndrr01l
087500990504     c     'A1'          reade     fndrr01l                               30
087600990504     c                   eval      zz=1
087700990504     c                   dow       not *in30
087800000323     c
087900990504     c                   if        drratb=' '  AND
088000000323     c                             drrcfl>savcfl
088100000323
088200000323     c                   movel(p)  drrcfl        w003a             3
088300000323     c** Per raggruppamento '03' memorizzo come 'P' e come 'A'
088400000323     c                   if        drrcfl='03'
088500000323     c                   move      'A'           w003a
088600000323     c                   endif
088700000323     c
088800000323     c                   eval      acfl(zz)=w003a
088900990504     c                   add       1             zz
089000000323
089100000323     c                   if        drrcfl='03'
089200000323     c                   move      'P'           w003a
089300000323     c                   eval      acfl(zz)=w003a
089400000323     c                   add       1             zz
089500000328     c                   move      'T'           w003a
089600000328     c                   eval      acfl(zz)=w003a
089700000328     c                   add       1             zz
089800000323     c                   endif
089900000323
090000000323     c                   eval      savcfl=drrcfl
090100990504     c                   endif
090200990504     c
090300990504     c     'A1'          reade     fndrr01l                               30
090400990504     c                   enddo
090500990504     c                   movea     acfl          wcfl
090600990504     c                   movea     acfl          Dcfl
090700990504     c                   movea     acfl          gcfl
090800990511     c                   movea     acfl          fcfl
090900990212     c                   endif
091000040206      *
091100990512     c* Carico i tipi anomalia
091200040206     c                   exsr      Intab_TAD
091300990512     c
091400990512     c                   movea     atdd          wtdd
091500990512     c                   movea     atdd          Dtdd
091600990512     c                   movea     atdd          gtdd
091700990512     c                   movea     atdd          ftdd
091800990512     c
091900990512     c
092000990512     c* tipo P.O. selezione
092100990212     c                   if        i43tsl='P'
092200990212     c                   eval      p1dtsl='PARTENZA'
092300990212     c                   endif
092400990212     c                   if        i43tsl='A'
092500990212     c                   eval      p1dtsl='ARRIVO  '
092600990212     c                   endif
092700060206     c
092800060206     c* tipo responsabile
092900060206     c                   select
093000060206     c                   when      i43tr1='P'
093100060206     c                   eval      p1dtr1='PARTENZA'
093200060206     c                   when      i43tr1='A'
093300060206     c                   eval      p1dtr1='ARRIVO  '
093400060206     c                   when      i43tr1='T'
093500060206     c                   eval      p1dtr1='TRANSITO'
093600060206     c                   endsl
093700060206     c                   select
093800060206     c                   when      i43tr2='P'
093900060206     c                   eval      p1dtr2='PARTENZA'
094000060206     c                   when      i43tr2='A'
094100060206     c                   eval      p1dtr2='ARRIVO  '
094200060206     c                   when      i43tr2='T'
094300060206     c                   eval      p1dtr2='TRANSITO'
094400060206     c                   endsl
094500060206     c                   select
094600060206     c                   when      i43tr3='P'
094700060206     c                   eval      p1dtr3='PARTENZA'
094800060206     c                   when      i43tr3='A'
094900060206     c                   eval      p1dtr3='ARRIVO  '
095000060206     c                   when      i43tr3='T'
095100060206     c                   eval      p1dtr3='TRANSITO'
095200060206     c                   endsl
095300990212     c
095400990212     c* tipo anomalia
095500060601     c                   clear                   p1drag
095600990212     c                   if        i43tad<>'  '
095700990406     c                   eval      wtad=i43tad
095800990406     c                   exsr      DECTAD
095900990406     c                   movel     §taddesc      p1dtad
096000060601     c                   movel     §tadragr      p1drag
096100990212     c                   endif
096200990212     c
096300990212     c* Raggruppamento anomalia
096400990212     c                   if        i43rgr='A'
096500990212     c                   eval      p1drgr='AMMANCHI'
096600990212     c                   endif
096700990212     c                   if        i43rgr='M'
096800990212     c                   eval      p1drgr='MANCANZE'
096900990212     c                   endif
097000990212     c                   if        i43rgr='V'
097100990212     c                   eval      p1drgr='AVARIE  '
097200990212     c                   endif
097300990916     c                   if        i43rg2='A'
097400990916     c                   eval      p1drgr='AMMANCHI'
097500990916     c                   endif
097600990916     c                   if        i43rg2='M'
097700990916     c                   eval      p1drg2='MANCANZE'
097800990916     c                   endif
097900990916     c                   if        i43rg2='V'
098000990916     c                   eval      p1drg2='AVARIE  '
098100990916     c                   endif
098200990916     c                   if        i43rg3='A'
098300990916     c                   eval      p1drg3='AMMANCHI'
098400990916     c                   endif
098500990916     c                   if        i43rg3='M'
098600990916     c                   eval      p1drg3='MANCANZE'
098700990916     c                   endif
098800990916     c                   if        i43rg3='V'
098900990916     c                   eval      p1drg3='AVARIE  '
099000990916     c                   endif
099100990212     c
099200990212     c* Tipo liquidazione /pratica
099300990212     c                   if        i43tpl='T'
099400990212     c                   eval      p1dtpl='LIQUIDAZIONI TRANSATTIVE'
099500990212     c                   endif
099600990212     c                   if        i43tpl='P'
099700990212     c                   eval      p1dtpl='PRATICHE ASSICURATIVE   '
099800990212     c                   endif
099900000215     c
100000000215     c* Stampa dati di liquidazione
100100000215     c                   if        i43sdl='S'
100200000215     c                   eval      p1csdl='Si'
100300000215     c                   seton                                        09
100400000215     c                   endif
100500170424
100600170424     c* solo c.a. aperte in ritardo
100700170424     c                   if        i43rit='S'
100800170424     c                   eval      p1crit='Si'
100900170424     c                   endif
101000040726     c
101100040726      * Stampa descrizione pezzi danneggiati/mancanti
101200040726     c                   if        I43sdm = 'S'
101300040726     c                   eval      P1Csdm = 'Si'
101400040726     c                   endif
101500990212     c
101600990212     c* Tipo C.A.
101700990212     c                   if        i43tca='A'
101800990212     c                   eval      p1dtca='APERTE         '
101900990212     c                   endif
102000990212     c                   if        i43tca='C'
102100990212     c                   eval      p1dtca='CHIUSE         '
102200990212     c                   endif
102300990212     c                   if        i43tca='E'
102400990212     c                   eval      p1dtca='APERTE E CHIUSE'
102500990316     c                   endif
102600990215     c*
102700990215     c* Memorizzo se sono state fatte selezioni sulla bolla
102800990215     c                   if        i43lnp>0 or i43lna>0 or
102900990316     c                             i43dsd>0 or i43dsa>0 or
103000990316     c                             i43ksm>0 or i43ksd>0 or
103100060125     c                             i43rsm<>*blanks or i43rsd<>*blanks  or
103200060125     c                             i43val<>*blanks
103300990215     c                   eval      WSELBOL='S'
103400060125     c
103500060125     c* Stampa richiesta spedizioni di valore
103600060125     c                   if        i43val='S'
103700060125     c                   eval      p1cval='Si'
103800060125     c                   endif
103900990316     c* Mittente
104000990316     c                   if        i43ksm>0
104100990322     c                   movel     i43ksm        wa43ksm
104200990316     C                   clear                   TIBS69DS
104300990316     c                   movel     i43ksm        I69kac
104400990316      *
104500990316     C                   CALL      'TIBS69R'
104600990316     C                   PARM                    tibs69DS
104700990316     C                   PARM                    DS_cnaco
104800990316     C                   PARM                    DS_cnind
104900990316     C                   PARM                    DS_cnclp
105000990316     C                   PARM                    DS_fncls
105100990316    2c                   if        o69err=' '
105200990316     c                   movel     acorag        p1dksm
105300990316    2c                   endif
105400990217     c                   endif
105500990322     c* Cerco l'ultima posizione piena della ragione sociale
105600990322     c                   if        i43rsm<>*blanks
105700990322     c     ' '           checkr    i43rsm        wPOSM
105800990322     c                   endif
105900990316     c* Destinatario
106000990316     c                   if        i43ksd>0
106100990322     c                   movel     i43ksd        wa43ksd
106200990316     C                   clear                   TIBS69DS
106300990316     c                   movel     i43ksd        I69kac
106400990316      *
106500990316     C                   CALL      'TIBS69R'
106600990316     C                   PARM                    tibs69DS
106700990316     C                   PARM                    DS_cnaco
106800990316     C                   PARM                    DS_cnind
106900990316     C                   PARM                    DS_cnclp
107000990316     C                   PARM                    DS_fncls
107100990316    2c                   if        o69err=' '
107200990316     c                   movel     acorag        p1dksd
107300990316    2c                   endif
107400990316     c                   endif
107500990322     c* Cerco l'ultima posizione piena della ragione sociale
107600990322     c                   if        i43rsd<>*blanks
107700990322     c     ' '           checkr    i43rsd        wPOSD
107800990322     c                   endif
107900990316     c
108000990316     c                   endif
108100990406     c
108200090320     c* impostio date di elaborazione con data apertuura o chiusura c.a.
108300090319     c                   move      i43amgdcd     wdal
108400000216     C                   IF        I43AMGDCA>0
108500090319     c                   move      i43amgdca     wal
108600000216     C                   ELSE
108700090319     c                   move      i43amgdcd     wal
108800000216     C                   endif
108900090320
109000090320     c                   if        wdal=0 and i43amgdhd>0
109100090320     c                   move      i43amgdhd     wdal
109200090320     C                   IF        I43AMGDhA>0
109300090320     c                   move      i43amgdha     wal
109400090320     C                   ELSE
109500090320     c                   move      i43amgdhd     wal
109600090320     C                   endif
109700090320     c                   endif
109800000216     c
109900090320     c* Se vengono creati i dati, li devo anche cancellare prima
110000090320     c                   if        i43crp='S'
110100090320     c* se elaborata una sola data cancello record "G"
110200090320     c* se elaborate più date cancello record "R"
110300090320     c                   if        wdal=wal
110400090320     c                   eval      ktie='G'
110500110912     c                   eval      ktieav='A'
110600090320     c                   else
110700090320     c                   eval      ktie='R'
110800110912     c                   eval      ktieav='B'
110900090320     c                   endif
111000110912     c
111100090320     c     ktie          setll     wfrdn000
111200090320     c     ktie          reade     wfrdn000                               32
111300990406     c                   dow       not *in32
111400990408     c                   delete    wfrdn000
111500090320     c     ktie          reade     wfrdn000                               32
111600090319     c                   enddo
111700110912     c
111800110912     c     ktieAV        setll     wfrdn000
111900110912     c     ktieAV        reade     wfrdn000                               32
112000110912     c                   dow       not *in32
112100110912     c                   delete    wfrdn000
112200110912     c     ktieAV        reade     wfrdn000                               32
112300110912     c                   enddo
112400090319     c                   endif
112500990429     c
112600990429     c* Uso interno
112700990429     c                   if        i43usi='S'
112800990429     c                   eval      p1cusi='Si'
112900990429     c                   else
113000990429     c                   eval      p1cusi='No'
113100990429     c                   endif
113200050708     c                   select
113300050708     c                   when      i43toi='S'
113400050708     c                   eval      p1ctoi='Si                    '
113500050708     c
113600050708     c                   when      i43toi='G'
113700050708     c                   eval      p1ctoi='Generali con dettaglio'
113800050708     c                   other
113900050708     c                   eval      i43toi=' '
114000050708     c                   endsl
114100050708     c
114200990429     c* crea dati per report
114300990429     c                   if        i43crp='S'
114400990429     c                   eval      p1ccrp='Si'
114500040726     c                   else
114600040726     c                   eval      p1ccrp='No'
114700990429     c                   endif
114800040906      * crea workfile con dettaglio
114900050906     c                   select
115000050906     c                   when      i43cfd='S'
115100040906     c                   eval      p1ccfd='Si'
115200050906     c                   when      i43cfd='P'
115300050906     c                   eval      p1ccfd='SI'
115400050906     c                   other
115500040906     c                   eval      p1ccfd='No'
115600050906     c                   endsl
115700990406     c
115800121219     c* file printer: se richiesto congelato  faccio ovrprtf
115900121219     C* ESEGUO OVRPRTF
116000121219     c                   if        i43psn='H'
116100121219     C                   Z-ADD     50            LUNG             15 5
116200121219     C                   MOVEL     *BLANKS       COMMAN           80
116300121219     C                   MOVEA     ovrprt        COMMAN
116400121219     C                   CALL      'QCMDEXC'                            H1
116500121219     C                   PARM                    COMMAN
116600121219     C                   PARM                    LUNG
116700121219     c                   endif
116800121219     C*
116900121219     C                   OPEN      FIDN44P
117000121219     C*
117100990212     C                   ENDSR
117200990219      *------------------------------------------------------------------------*
117300990219      * DECODIFICO TESTATA DISTRETTO E AREA
117400990219      *------------------------------------------------------------------------*
117500990219     C     DECODTES2     BEGSR
117600040204      *
117700990310     c* Area
117800040202if  2c                   if        orgcar<>p3care
117900990310     c                   eval      wbrka='1'
118000990312     c                   eval      wbrkn='1'
118100990323     c
118200990323     c                   if        p3care>0
118300990323     c                   eval      wnuovo='S'
118400990323     c                   endif
118500990323     c
118600990312     c                   EXSR      TOTAREA
118700040202e   2c                   endif
118800990310     c
118900990310     c* Distretto
119000040202if  2c                   if        orgfl3<>p3cdst
119100990309     c                   eval      wbrkd='1'
119200990323     c                   if        p3care>0
119300990323     c                   eval      wnuovo='S'
119400990323     c                   endif
119500990310     c                   EXSR      TOTDISTR
119600040202e   2c                   endif
119700990309     c
119800990219     c                   clear                   p3ddst
119900990219     c                   clear                   p3dare
120000990312     c                   clear                   p3dds2
120100990312     c                   clear                   p3dar2
120200990219     c                   movel     orgcar        p3care
120300990219     c                   movel     orgfl3        p3cdst
120400990219     c
120500990219     C                   movel     '17'          kCOD
120600990219     C                   movel(P)  orgfl3        kKEY
120700990219     C     ktab          CHAIN     TABEL                              31
120800990219     C  n31              movel     TBLUNI        p3ddst
120900990312     C  n31              movel     TBLUNI        p3dds2
121000990309     C  n31              movel     TBLUNI        ds17
121100990309     c   31              clear                   ds17
121200990219     c* Area
121300990219     C                   movel     '05'          kCOD
121400990219     C                   movel(P)  orgcar        kKEY
121500990219     C     ktab          CHAIN     TABEL                              31
121600990219     C  n31              movel     TBLUNI        p3dare
121700990312     C  n31              movel     TBLUNI        p3dar2
121800990309     C  n31              movel     TBLUNI        ds05
121900990309     c   31              clear                   ds05
122000990219     C                   ENDSR
122100990219      *------------------------------------------------------------------------*
122200990219      * LETTURA per stmapa
122300990219      *------------------------------------------------------------------------*
122400990219     c     LETTURE       BEGSR
122500990219     c* 2 ordinamenti :
122600990219     c* 1) per numero C.A.
122700990219     c                   select
122800990219     c                   when      i43ord='1'
122900990219     c                   exsr      LEGGICA
123000990219     c                   when      i43ord='2'
123100990219     c                   exsr      LEGGIRES
123200990219     c                   endsl
123300990219
123400990219     c* Stampo l'ultimo totale parziale
123500990219     c                   if        p3ctps>0
123600990219     c                   eval      dctfil=000
123700990219     c                   eval      dctaac=0000
123800990219     c                   eval      dcrpor=999
123900990219     c                   EXSR      ROTTURE
124000990219     c                   endif
124100990219     c                   endsr
124200981204      *------------------------------------------------------------------------*
124300990212      * LETTURA PER NUMERO C.A.
124400981204      *------------------------------------------------------------------------*
124500990212     C     LEGGICA       BEGSR
124600990212     c* Se c'e' anno e P.O. li uso altrimenti dall'inizio
124700990212     c                   select
124800990219     c                   when      i43aac>0 and kfil>0
124900990215     c     kdct1         chain     fndct11l                           30
125000990219     c                   when      i43aac>0 and kfil=0
125100990215     c     i43aac        chain     fndct01l                           30
125200990219     c                   when      kfil>0 and i43aac=0
125300990219     c     kfil          chain     fndct11l                           30
125400990212     c                   other
125500990215     c     *loval        setll     fndct11l
125600990215     c                   read      fndct11l                               30
125700990212     c                   endsl
125800990212     c**
125900040202do  1c                   DOW       not *in30
126000040202if  2c     dctatb        ifeq      ' '
126100990212     c                   EXSR      CONFRO
126200040202if  3c                   if        wconfr<>'N'
126300990215     c                   exsr      STAMPA
126400050906if  4c                   if           I43cfd = 'S'
126500050906     c                             or I43cfd = 'P'
126600040204     c                   exsr      Wrt_WFDDN
126700040204e   4c                   endif
126800040202e   3c                   endif
126900040202e   2c                   endif
127000990212     c
127100990212     c* Lettura fondo ciclo
127200990212     c                   select
127300990219     c                   when      i43aac>0 and kfil>0
127400990215     c     kdct1         reade     fndct11l                               30
127500990219     c                   when      i43aac>0 and kfil=0
127600990212     c     i43aac        reade     fndct01l                               30
127700990219     c                   when      kfil>0 and i43aac=0
127800990219     c     kfil          reade     fndct11l                               30
127900990212     c                   other
128000990215     c                   read      fndct11l                               30
128100990212     c                   endsl
128200990212     c
128300040202e   1c                   ENDDO
128400990212     c                   ENDSR
128500990217      *------------------------------------------------------------------------*
128600990218      * LETTURA PER P.O. reponsabile
128700990217      *------------------------------------------------------------------------*
128800990217     C     LEGGIRES      BEGSR
128900090402    0c                   dou       kfil=0
129000090323     c
129100090323     c* Se impostato il p.o. responsabile --> ok
129200090323     c*   altrimenti lo devo cercare
129300090402    1c                   if        i43prs>0
129400090323     c                   eval      kfil=i43prs
129500090323     c                   else
129600090323     c     kDCRcer       setgt     fndcr05l
129700090323     c     kteralfa      reade     fndcr05l
129800090402    2c                   if        not %eof(fndcr05l)
129900090323     c                   eval      kfil=dcrpor
130000090323     c                   else
130100090323     c                   clear                   kfil
130200090402    2c                   endif
130300090402    1c                   endif
130400090323     c
130500090323     c
130600090402    1c                   if        kfil>0
130700990503     c* mio faccio guidare dall'ordinamento dei tipi responsabilita'
130800990503     c     'A1'          setll     fndrr01l
130900990503     c     'A1'          reade     fndrr01l                               38
131000040202do  1c                   dow       not *in38
131100990504     c
131200090319     c     kdcr3         chain     fndcr05l                           30
131300990218     c
131400040202do  2c                   DOW       not *in30
131500040202if  3c     dcratb        ifeq      ' '
131600090319     c                   eval      ddcr01=dcrflo
131700090319     c
131800090319     c                   if        §DCRTER<=*zeros
131900090319     c                   eval      §dcrter=%editc(dcrpor:'X')
132000090319     c                   endif
132100090323     c                   eval      kter=%int(§dcrter)
132200090319     c
132300990310     c* Leggo le testate c.a.
132400990218     c     kdcrt         chain     fndct01l                           31
132500990218     c*
132600040202if  4c                   if        not *in31 and dctatb=' '
132700990218     c                   EXSR      CONFRO
132800040202if  5c                   if        wconfr<>'N'
132900990218     c                   exsr      STAMPA
133000050906if  6c                   if           I43cfd = 'S'
133100050906     c                             or I43cfd = 'P'
133200040204     c                   exsr      Wrt_WFDDN
133300040204e   6c                   endif
133400040202e   5c                   endif
133500990406     c
133600990406     c* Vedo se devo anche scrivere il file del report
133700990406     c                   if        simfel=0 and i43crp='S' and Wscrivi='S'
133800990406     c                   exsr      SCRIREP
133900990218     c                   endif
134000990406     c
134100040202e   4c                   endif
134200040202e   3c                   endif
134300990218     c
134400990218     c* Lettura fondo ciclo
134500090319     c     kdcr3         reade     fndcr05l                               30
134600040202e   2c                   enddo
134700990503     c
134800990503     c     'A1'          reade     fndrr01l                               38
134900040202e   1c                   enddo
135000090323     c
135100090402    1c                   endif
135200090402     c                   if        i43prs>0
135300090402     c                   clear                   kfil
135400090402     c                   endif
135500090323     c
135600090402e   0c                   enddo
135700990218     c
135800990217     c                   ENDSR
135900981204      *------------------------------------------------------------------------*
136000990212      * CONFRONTO SELEZIONI FATTE
136100981204      *------------------------------------------------------------------------*
136200990212     C     CONFRO        BEGSR
136300990212     c                   clear                   wconfr
136400990215     c                   clear                   wbol
136500990322     c                   clear                   wbenef
136600990322     c                   clear                   wmitdes
136700990406     c                   eval      wscrivi='N'
136800990215     c                   movel     dctflo        ddct01
136900990212     c
137000990212     c* Selezioni sulle C.A. :
137100990322     c*
137200020821     c* A) Livello (se non Sede)
137300020821    1c                   if        SIMfel <> 0
137400110210    c
137500110210      * controllo anche che la c.a. non abbia dctdt2 > 0
137600110210    2c                   If        dctdt2 > 2
137700110210     c                   eval      wconfr='N'
137800110210     c                   goto      endconfro
137900110210    2c                   EndIf
138000110211    2c                   EndIf
138100110210     c
138200990212     c* 1) Anno CA
138300990216    1c                   if        i43aac>0  and dctaac<>i43aac
138400990212     c                   eval      wconfr='N'
138500990212     c                   goto      endconfro
138600990216    1c                   endif
138700990212     c
138800990212     c* 2) P.O. CA
138900990216    1c                   if        i43fil>0  and dctfil<>i43fil
139000990212     c                   eval      wconfr='N'
139100990212     c                   goto      endconfro
139200990216    1c                   endif
139300990212     c* 3) Tipo anomalia
139400060601     c* Parzializzo tipo anomalia no evento
139500060601    1c                   if        i43tad>*blanks and p1drag<>'E'
139600060601    1c                   if        dcttad<>i43tad
139700990212     c                   eval      wconfr='N'
139800990212     c                   goto      endconfro
139900990216    1c                   endif
140000060601    1c                   endif
140100060601     c
140200060601     c* se evento chain su file eventi per vedere il tipo
140300060601    1c                   if        i43tad>*blanks and p1drag='E'
140400060601     c                   eval      wconfr='N'
140500060601     c                   if        dctnev>0
140600060601     c     kdet          chain     fndet01l                           32
140700060601     c                   if        not *in32  and dettad=i43tad
140800060601     c                   eval      wconfr='S'
140900060601     c                   endif
141000060601     c                   endif
141100060601     c                   if        wconfr='N'
141200060601     c                   goto      endconfro
141300060601    1c                   endif
141400060601    1c                   endif
141500060601     c
141600990212     c* 4) Raggruppamento tipi anomalia
141700990916    1c                   if        i43rgr<>*blanks  OR
141800990916     c                             i43rg2<>*blanks  OR
141900990916     c                             i43rg3<>*blanks
142000990406     c                   eval      wtad=dcttad
142100990406     c                   exsr      DECTAD
142200990219     c
142300990916    2c                   if        §tadragr<>i43rgr AND
142400990916    2c                             §tadragr<>i43rg2 AND
142500990916    2c                             §tadragr<>i43rg3
142600990219     c                   eval      wconfr='N'
142700990219     c                   goto      endconfro
142800990406    2c                   endif
142900990406    1c                   endif
143000990219     c
143100990215     c* 5) P.O. selezione
143200990216    1c                   if        i43psl>0 and i43tsl=' '
143300990215     c     dctlna        lookup    l6                                     31
143400990215     c
143500990215     c                   movel     §dctlnpc      w0030             3 0
143600990215     c     w0030         lookup    l6                                     32
143700990216    2c                   if        not *in31 and not *in32
143800990215     c                   eval      wconfr='N'
143900990215     c                   goto      endconfro
144000990303    2c                   endif
144100990216    1c                   endif
144200990216    1c                   if        i43psl>0 and i43tsl='A'
144300990215     c     dctlna        lookup    l6                                     31
144400990215     c  N31              eval      wconfr='N'
144500990303     c  N31              goto      endconfro
144600990216    1c                   endif
144700990216    1c                   if        i43psl>0 and i43tsl='P'
144800990215     c                   movel     §dctlnpc      w0030             3 0
144900990215     c     w0030         lookup    l6                                     32
145000990215     c  N32              eval      wconfr='N'
145100990303     c  N32              goto      endconfro
145200990216    1c                   endif
145300990215     c* 5) Data C.A.
145400990216    1c                   if        i43dcd>0
145500990216    2c                   if        i43dca=0
145600990215     c                   eval      i43amgdca=i43amgdcd
145700990216    2c                   endif
145800990216    2c                   if        dctdca<i43amgdcd   OR
145900990215     c                             dctdca>i43amgdca
146000990215     c                   eval      wconfr='N'
146100990215     c                   goto      endconfro
146200990216    2c                   endif
146300990216    1c                   endif
146400990218     c
146500990218     c* 6) Data Chiusura C.A.
146600990218    1c                   if        i43dhd>0
146700990218    2c                   if        i43dha=0
146800990218     c                   eval      i43amgdha=i43amgdhd
146900990218    2c                   endif
147000990218    2c                   if        dctdch<i43amgdhd   OR
147100990218     c                             dctdch>i43amgdha
147200990218     c                   eval      wconfr='N'
147300990218     c                   goto      endconfro
147400990218    2c                   endif
147500990218    1c                   endif
147600990218     c
147700990218     c* 7) Causale chiusura C.A.
147800990218    1c                   if        i43cch>*blanks and dctcch<>i43cch
147900990218     c                   eval      wconfr='N'
148000990218     c                   goto      endconfro
148100990218    1c                   endif
148200990218     c* 8) C.A. in Liqu.Trans o Pratica Ass
148300990216    1c                   if        i43tpl<>' ' and dctfpr<>i43tpl
148400990215     c                   eval      wconfr='N'
148500990215     c                   goto      endconfro
148600990216    1c                   endif
148700990218     c* 9) Tipo C.A. : aperte o chiuse
148800990216    1c                   if        i43tca='A' and dctdch>0  OR
148900990215     c                             i43tca='C' and dctdch=0
149000990215     c                   eval      wconfr='N'
149100990215     c                   goto      endconfro
149200990216    1c                   endif
149300990322     c* 10) P.O. responsabile o terminal responsabile
149400990322     c*                            per ordinamento 1- Num C.A.
149500990322    1c                   if        i43ord='1' and i43prs>0  OR
149600990322     c                             i43ord='1' and i43ter>0
149700990215     c                   eval      wconfr='N'
149800990322     c
149900990217     c     kdct2         chain     fndcr02l                           31
150000990216    2c                   dow       not *in31
150100990322   2ac                   if        i43prs>0
150200990322     c
150300990216    3c     dcratb        ifne      ' '
150400990215     c     dcrpor        orne      i43prs
150500990217     c     kdct2         reade     fndcr02l                               31
150600060206   x3c                   else
150700060206     c* Se non immesso, il tipo--> ok
150800060206     c                   if        i43tr1=' ' and i43tr2=' ' and i43tr3=' '
150900990215     c                   eval      wconfr='S'
151000990215     c                   seton                                        31
151100060206     c                   else
151200060206     c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
151300060206     c                             dcrtcr=i43tr3
151400060206     c                   eval      wconfr='S'
151500060206     c                   seton                                        31
151600060206     c                   else
151700060206     c     kdct2         reade     fndcr02l                               31
151800060206     c                   endif
151900060206     c                   endif
152000060206     c
152100990216    3c                   endif
152200990322   2ac                   endif
152300990322     c
152400990322   2ac                   if        i43ter>0
152500990322    3c     dcratb        ifeq      ' '
152600990322     c     dcrpor        chain     azorg01l                           31
152700070111     c**n31              movel     orgcar        w003a
152800070111     c  n31              movel     orgde0        og140
152900070111     c  n31              movel     §ogara        w003a
153000990322     c   31              movel     *blanks       w003a
153100990322    4c                   if        w003a =i43are
153200060206     c* Se non immesso, il tipo--> ok
153300060206    5c                   if        i43tr1=' ' and i43tr2=' ' and i43tr3=' '
153400990322     c                   eval      wconfr='S'
153500990322     c                   seton                                        31
153600060206   x5c                   else
153700060206    6c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
153800060206     c                             dcrtcr=i43tr3
153900060206     c                   eval      wconfr='S'
154000060206     c                   seton                                        31
154100060206   x6c                   else
154200060206     c     kdct2         reade     fndcr02l                               31
154300060206    6c                   endif
154400060206    5c                   endif
154500060206     c
154600990322   x4c                   else
154700990322     c     kdct2         reade     fndcr02l                               31
154800990322    4c                   endif
154900990322     c
155000990322   x3c                   else
155100990322     c     kdct2         reade     fndcr02l                               31
155200110210    3c                   endif
155300990322   2ac                   endif
155400990322     c
155500990216    2c                   enddo
155600990322     c
155700990216    2c                   if        wconfr='N'
155800990215     c                   goto      endconfro
155900990216    2c                   endif
156000990216    1c                   endif
156100060206     c* Se leggo per tipo p.o. responsabile devo comunque controllare
156200060206     c*  il tipo
156300060206    1c                   if        i43ord='2' and i43prs>0  OR
156400060206     c                             i43ord='2' and i43ter>0
156500060206     c                   if        i43tr1<>' ' or i43tr2<>' ' or
156600060206     c                             i43tr3<>' '
156700060206    6c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
156800060206     c                             dcrtcr=i43tr3
156900060206     c                   else
157000060206     c                   eval      wconfr='N'
157100060206     c                   goto      endconfro
157200060206     c                   endif
157300060206     c                   endif
157400060206     c                   endif
157500990322     c
157600110211     c* 11) Peso danneggiato / mancante
157700060125     c                   if        i43pkd>0  and dctpkd<i43pkd
157800060125     c                   eval      wconfr='N'
157900060125     c                   goto      endconfro
158000060125     c                   endif
158100170424     c
158200170424     c* 12) c.a. aperte in ritardo
158300170424     c                   if        i43rit='S'
158400170424     c
158500170424     c* per ordinam "1" devo chainare il record della responsabilità
158600170424     c*  ne basta uno perchè il flag è su tutti i record
158700170424     c                   if        i43ord='1'
158800170424     c     kdct2         chain     fndcr02l
158900170424     c                   clear                   ddcr01
159000170424    2c                   if        %found(fndcr02l)
159100170424     c                   eval      ddcr01=dcrflo
159200170424     c                   endif
159300170424     c                   endif
159400170424
159500170424     c                   if        §DCRF1<>'R'
159600170424     c                   eval      wconfr='N'
159700170424     c                   goto      endconfro
159800170424     c                   endif
159900170424
160000170424     c                   endif
160100110211     c
160200110211      * se richiesta la stampa x p.o. responsabile non faccio il controllo dei livelli
160300110211     c* A) Livello (se non Sede)
160400110211    1c                   if        SIMfel <> 0
160500110211     c                   select
160600110211    2c                   when      wcontrlnplna='S'
160700110211     c*  Adesso controllo il p.o. utente con la skiera dei p.o. abilitati
160800110211     c                   movel     dctlna        w003a             3
160900110211     c     w003a         lookup    $pog                                   31
161000110211     c  n31§dctlnpc      lookup    $pog                                   31
161100110211     c  n31              eval      wconfr='N'
161200110211     c
161300110211     c* Se devo controllare il terminal, verifico se abilitato
161400110211    2c                   when      wabiter='S'
161500110211     c     §dcrter       lookup    $pog                                   31
161600110211     c  n31              eval      wconfr='N'
161700110211    2c                   endsl
161800110211     c
161900110211    1c                   endif
162000110211     c*
162100110211     c* 12) Beneficiario
162200110211    1c**                 if        i43rsb<>*blanks OR i43ksb>0  or
162300110211     C**                           i43ksb2>0  or i43ksb3>0 or i43ksb4>0  or
162400110211     c**                           i43ksb5>0
162500110211    1c                   if        i43rsb<>*blanks OR $ksb(1)>*zeros
162600110211     c* Devo cercare il beneficario
162700110211     c                   EXSR      CERBOL
162800110211    2c                   if         wbol<>' '
162900110211     c                   exsr      BENEFIC
163000110211    2c                   endif
163100110211     c* - codice
163200110211    2c**                 if        i43ksb>0   AND
163300110211     c**                           p3cksc<>wa43ksb
163400110211     c**                 eval      wconfr='N'
163500110211     c**                 goto      endconfro
163600110211     C
163700110211     c                   if        $ksb(1)>*zeros
163800110211     c                   if        p3cksc<=*zeros
163900110211     c                   eval      wconfr='N'
164000110211     c                   goto      endconfro
164100110211    2c                   endif
164200110211     c
164300110211     c     p3cksc        lookup    $KSB                                   31
164400110211     C                   IF        NOT *IN31
164500110211     c                   eval      wconfr='N'
164600110211     c                   goto      endconfro
164700110211    2c                   endif
164800110211    2c                   endif
164900110211     c
165000110211     c* - ragione sociale
165100110211    2c                   if        i43rsb<>*blanks
165200110211     c                   eval      wrag=%subst(p3dksc:1:wposb)
165300110211    3c                   if        wrag<>i43rsb
165400110211     c                   eval      wconfr='N'
165500110211     c                   goto      endconfro
165600110211    3c                   endif
165700110211    2c                   endif
165800110211    1c                   endif
165900110211     c
166000110211     c* Ricontrollo le abilitazioni  con la filiale del beneficiario
166100110211     c                   if        $ksb(1)>*zeros and wconfr='N'
166200110211     c                   movel     p3cksc        w003a             3
166300110211     c     w003a         lookup    $pog                                   31
166400110211     c                   if        not *in31
166500110211     c                   goto      endconfro
166600110211     c                   else
166700110211     c                   clear                   wconfr
166800110211     c                   endif
166900110211     c                   endif
167000110211     c
167100110211     c* se il controllo delle abilitazioni non è andato a buon fine --> scarto la bolla
167200110211     c                   if        wconfr='N'
167300110211     c                   goto      endconfro
167400110211    1c                   endif
167500110211     c
167600990218     c
167700990215     c*
167800990216    1c                   if        wselbol='S'
167900050711     c* 1) lnp bolla  da DCT
168000050711    3c***                if         i43lnp>0 and i43lnp<>arblnp
168100050711     c                   if         i43lnp>0
168200050711     c                   movel     §dctlnpc      w0030             3 0
168300050711     c                   if         i43lnp<>w0030
168400990215     c                   eval      wconfr='N'
168500990215     c                   goto      endconfro
168600990216    3c                   endif
168700050711    3c                   endif
168800050711     c* 2) lna bolla  da DCT
168900050711    3c                   if        i43lna>0 and i43lna<>dctlna
169000990215     c                   eval      wconfr='N'
169100990215     c                   goto      endconfro
169200990216    3c                   endif
169300050711    c
169400050711     c                   EXSR      CERBOL
169500050711    c
169600050711    2c                   if         wbol<>' '
169700990215     c* 3) Data spedizione
169800990216    3c                   if        i43dsd>0
169900990216    4c                   if        i43dsa=0
170000990215     c                   eval      i43amgdsa=i43amgdsd
170100990216    4c                   endif
170200050711    4c                   if        spedata<i43amgdsd   OR
170300050711     c                             spedata>i43amgdsa
170400990215     c                   eval      wconfr='N'
170500990215     c                   goto      endconfro
170600990216    4c                   endif
170700990218    3c                   endif
170800990322     c
170900990322     c* 4) Mittente
171000990322    1c                   if        i43rsm<>*blanks OR i43ksm>0
171100990322     c                   exsr      MITDES
171200990322     c* - codice
171300990322    2c                   if        i43ksm>0   AND
171400990322     c                             p3cccm<>wa43ksm
171500990322     c                   eval      wconfr='N'
171600990322     c                   goto      endconfro
171700990322    2c                   endif
171800990322     c
171900990322     c* - ragione sociale
172000990322    2c                   if        i43rsm<>*blanks
172100990322     c                   eval      wrag=%subst(p3dccm:1:wposm)
172200990322    3c                   if        wrag<>i43rsm
172300990322     c                   eval      wconfr='N'
172400990322     c                   goto      endconfro
172500990322    3c                   endif
172600990322    2c                   endif
172700990322    1c                   endif
172800990322     c* 5) Destinatario
172900990322    1c                   if        i43rsd<>*blanks OR i43ksd>0
173000990322     c                   exsr      MITDES
173100990322     c* - codice
173200990322    2c                   if        i43ksd>0   AND
173300990322     c                             p3crsd<>wa43ksd
173400990322     c                   eval      wconfr='N'
173500990322     c                   goto      endconfro
173600990322    2c                   endif
173700990322     c
173800990322     c* - ragione sociale
173900990322    2c                   if        i43rsd<>*blanks
174000990322     c                   eval      wrag=%subst(p3drsd:1:wposd)
174100990322    3c                   if        wrag<>i43rsd
174200990322     c                   eval      wconfr='N'
174300990322     c                   goto      endconfro
174400990322    3c                   endif
174500990322    2c                   endif
174600990322    1c                   endif
174700060124     c* 5) Di valore
174800060124     c                   if        i43val='S'   and §ar5bva<>'V'
174900060124     c                   eval      wconfr='N'
175000060124     c                   goto      endconfro
175100060124     c                   endif
175200990215     c
175300990322   x2c                   else
175400990322     c* Se non trovo la bolla le selezioni
175500990216    2c                   endif
175600990216    1c                   endif
175700990406    c
175800990406     c* Se ordinamento 2:
175900990406    1c                   if        i43ord='2'
176000990406     c* 1) escludo gli esclusi dalla reportistica
176100990406    2c                   if        dcttcr='S'
176200990406     c                   eval      wconfr='N'
176300990406     c                   eval      wscrivi='S'
176400990406     c                   goto      endconfro
176500990406    2c                   endif
176600990406     c* 2) escludo le C.A.chiuse con causale da non considerare
176700990406    2c                   if        dctcch<>*blanks
176800990406     c                   eval      wcch=dctcch
176900990406     c                   exsr      DECCCH
177000990406    3c                   if        §cchnre='S'
177100990406     c                   eval      wconfr='N'
177200990406     c                   eval      wscrivi='S'
177300990406     c                   goto      endconfro
177400990406    3c                   endif
177500990406    2c                   endif
177600990406    1c                   endif
177700990407     c* Se il record e' ok --> e' ok anche per scrivere il file
177800990407     c                   eval      wscrivi='S'
177900990215     c*
178000990215     c     endconfro     ENDSR
178100990215      *------------------------------------------------------------------------*
178200990215      * CERCO LA BOLLA DELLA C.A.
178300990215      *------------------------------------------------------------------------*
178400990215     c     CERBOL        BEGSR
178500990322     c* Solo se non l'ho ancora cercata
178600050711    1c                   if        wbol=' '
178700990216     c                   clear                   WPORTO
178800050711     c                   clear                   spedata
178900050711     c                   clear                   spersm
179000050711     c                   clear                   spersd
179100050711     c                   clear                   speccm
179200050711     c                   clear                   speccd
179300050711     c                   clear                   dcknsp
179400990215     c* Cerco la bolla
179500050711    2c                   if        simfel>0
179600040803      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
179700050711    3c                   if        §dctlega = 'S'
179800040803     c     Kdct2         chain     fndck01l
179900050711     c
180000050711    4c                   if        not %found(fndck01l)
180100050711     c                   clear                   dckaas
180200050711     c                   clear                   dcklnp
180300050711     c                   clear                   dcknrs
180400050711     c                   clear                   dcknsp
180500050711    4c                   endif
180600050711    3c                   endif
180700050711     c
180800050711     c* Per bolla legata prendo mittente e data spedizione da mamma
180900050711     c*  destinatario da figlia
181000050711    3c                   if        dcknsp>0
181100050711     c     kbolla        chain     fnblp01l
181200050711    5c                   if        %found(FNBLP01L)
181300050711     c                   eval      WBOL='P'
181400050711     c                   exsr      CARDATIBOPAR
181500050711    5c                   endif
181600050711     c
181700050711     c     kbolladck     chain     fnarb01l
181800050711    5c                   if        %found(FNARB01L)
181900050711     c                   eval      WBOL='A'
182000050711     c
182100050711     c* Se non trovato blp della mamma, riempo i campi del mittente e
182200050711     c*  DSP da fnarb della figlia
182300050711    6c                   if        not %found(FNBLP01L)
182400050711     c                   exsr      CARDatibopar
182500050711    6c                   endif
182600050711     c
182700050711     c                   movel     arbrsd        spersd
182800050711    7c                   if        speccd=0 and wporto='A'
182900050711     c                   movel     arbksc        speccd
183000050711    7c                   endif
183100050711     c*
183200050711     c* Imposto numnero spedizione della figlia se:
183300050711     c* 1) non trovato fnblp
183400050711     c* 2) parzializzato per destinatario
183500050711     c* 3) parzializzato per p.o. selezione e lna=p.o. selezione
183600050711     c
183700050711     c                   if        i43psl>0
183800050711     c     dctlna        lookup    l6                                     31
183900050712     c                   else
184000050712     c                   movel     dctlna        w003a
184100050712     c     w003a         lookup    $pog                                   31
184200050711     c                   endif
184300050711     c
184400050711    6c                   if        not %found(FNBLP01L)  or
184500050712     c                             %found(fnarb01l)  and
184600050712     c                             (i43rsd<>*blanks OR i43ksd>0  or  *in31)
184700050711     c                   eval      dctaas = dckaas
184800050711     c                   eval      dctlnp = dcklnp
184900050711     c                   eval      dctnrs = dcknrs
185000050711     c                   eval      dctnsp = dcknsp
185100050711     c                   endif
185200050711     c
185300050711   x5c                   else
185400050711     c*
185500050711     c* se non trovato arb della figlia, riempo i campi del destinatari
185600050711     c*  da fnblp della mamma
185700050711     c                   movel     arbrsd        spersd
185800050711    5c                   endif
185900050711     c
186000050711    3c                   else
186100040803      *
186200050711     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
186300050711     c     kbolla        chain     fnarb01l
186400050711    4c                   if        %found(FNARB01L)
186500050711     c                   eval      WBOL='A'
186600050711     c                   else
186700020821     c     kbolla        chain     fnblp01l
186800020821     c                   if        %found(FNBLP01L)
186900020821     c                   eval      WBOL='P'
187000020821     c                   endif
187100050711    4c                   endif
187200990218     c
187300990216     c* Verifico se la bolla e' franco o assegnato
187400050711    4c                   if         wbol<> ' '
187500050711     c                   exsr      CARDAtibopar
187600050712     c                   movel     arbrsd        spersd
187700050711     c
187800050711    5c                   if        wbol='A' and wporto='A'
187900050711     c                   movel     arbksc        speccd
188000050711    5c                   endif
188100050711    4c                   endif
188200050711    3c                   endif
188300990215     c
188400050711   x2c                   else
188500990218     c
188600990218     c* SEDE
188700990809     c     kbolla        chain     TITAS30c                           32
188800990218     c     kbolla        chain     tntmd01l                           32
188900050711    3c                   if        not *in32
189000050711     c                   eval      WBOL='D'
189100050711     c                   movel     tastbl        WPORTO
189200050711     c                   move      arbdsp        spedata
189300050711     c                   movel     arbrsm        spersm
189400050711     c                   movel     arbrsd        spersd
189500050711     c
189600050711    4c                   if        wporto='F'
189700050711     c                   movel     arbksc        speccm
189800050711   x4c                   else
189900050711     c                   movel     arbccm        speccm
190000050711     c                   movel     arbksc        speccd
190100050711    4c                   endif
190200050711    3c                   endif
190300050711    2c                   endif
190400990322     c
190500050711    1c                   endif
190600060124     c
190700060124     c* Vedo se spedizione di valore
190800060125     c                   eval      ar5trd='GEN'
190900060308     c                   if        simfel>0
191000060124     c     k05ar530      chain     fiar501l
191100060124     c                   else
191200060124     c     k05ar530      chain     fiar531c
191300060124     c                   endif
191400060124     c
191500060124     c                   if        %found
191600060124     c                   movel     ar5uni        dar5gen
191700060124     c                   else
191800060124     c                   clear                   dar5gen
191900060124     c                   endif
192000990218     c
192100990215     c                   ENDSR
192200050711      *------------------------------------------------------------------------*
192300050711      * CARICO dati dalla bolla partenza
192400050711      *------------------------------------------------------------------------*
192500050711     c     CARDATiBOPar  BEGSR
192600050711     c                   movel     arbdsp        spedata
192700050711     c                   movel     arbrsm        spersm
192800050711     c
192900050711     c                   exsr      CERTBO
193000050711     c
193100050711    1c                   if        wporto='F'
193200050711     c                   movel     arbksc        speccm
193300050711    1c                   else
193400050711     c                   movel     arbccm        speccm
193500050711     c
193600050711     c* chain in fiar6 per prendere codice di tassazione
193700050711    2c                   if        WBOL='P'
193800050711     c                   eval      Ktrc     = '1'
193900050711     c     kBOLtrc       chain     FIAR6000
194000050711    3c                   if        %found(FIAR601L)
194100050711     c                   move      ar6ksc        w004a
194200050711    4c                   if        w004a<>'9999'
194300050711     c                   movel     ar6ksc        speccd
194400050711    4c                   endif
194500050711    3c                   endif
194600050711    2c                   endif
194700050711    1c                   endif
194800050711     c                   ENDSR
194900050711      *------------------------------------------------------------------------*
195000050711      * CERCO il tipo bolla della spedizione
195100050711      *------------------------------------------------------------------------*
195200050711     c     CERTBO        BEGSR
195300050711     C                   movel     '3A'          kCOD
195400050711     C                   movel(P)  ARBCBO        kKEY
195500050711     C     ktab          CHAIN     TABEL                              31
195600050711     C  n31              movel     TBLUNI        DS3A
195700050711     C  n31              movel     §3ATB1        WPORTO            1
195800050711     c   31              movel     'F'           WPORTO
195900050711     c                   endsr
196000990215      *------------------------------------------------------------------------*
196100990215      * STAMPO LA C.A.
196200990215      *------------------------------------------------------------------------*
196300990215     C     STAMPA        BEGSR
196400000214     c                   clear                   dta4a
196500990310     c*
196600990218     c                   EXSR      ROTTURE
196700990215     c*
196800990215     c* Se non ho ancora chainato la bolla la cerco
196900990215     c                   EXSR      CERBOL
197000990215     c
197100990215     c* Giro le date c.a.
197200000215     c     *iso          move      dctdca        datadmy
197300000215     c     *dmy          move      datadmy       p3dadc
197400990215    1c                   if        dctdch>0
197500000215     c     *iso          move      dctdch        datadmy
197600000215     c     *dmy          move      datadmy       p3dadh
197700990215     c                   else
197800990217     c                   clear                   p3dadh
197900990215    1c                   endif
198000990218     c
198100990215     c* Decodifica tipo anomalia
198200990406     c                   eval      wtad=dcttad
198300990406     c                   exsr      DECTAD
198400990215     c                   movel     §Taddesc      p3dtad
198500990215     c
198600990215     c* Decodifica causale chiusura
198700990218     c                   clear                   p3dcch
198800990215    1c                   if        dctcch<>*blanks
198900990406     c                   eval      wcch=dctcch
199000990406     c                   exsr      DECCCH
199100990215     c                   movel     §cchdesc      p3dcch
199200990215    1c                   endif
199300990218     c
199400990218     C* Tipo calcolo rensponsabilita'
199500990218     c                   clear                   P3CCAR
199600990218     c                   select
199700990218     c                   when      dctcar='M'
199800990218     c                   eval      p3ccar=cosman
199900990218     c                   when      dctcar='A'
200000990218     c                   eval      p3ccar=cosaut
200100990218     c                   other
200200990218     c                   if        dctcar<>*blanks
200300990218     c                   eval      p3ccar=cosric
200400990218     c                   endif
200500990218     c                   endsl
200600990218     c
200700990218     c* Transaz o pratica
200800990218     c                   clear                   p3dtpl
200900990218     c                   if        dctfpr='T'
201000990218     c                   eval      p3dtpl='LqTra'
201100990218     c                   endif
201200990218     c                   if        dctfpr='P'
201300990218     c                   eval      p3dtpl='PrAss'
201400990218     c                   endif
201500020130     c                   if        §dctpfra = 'F'
201600020130     c                   eval      p3dtpl='PrAsF'
201700020130     c                   endif
201800110204     c* decodifico evento  se c'e'
201900110204     c                   clear                   p3deve
202000110204     c                   if        dctnev>0
202100110204     c     kdet          chain     fndet01l                           32
202200110204     c                   if        not *in32
202300110204     c                   eval      wtad=dettad
202400110204     c                   exsr      DECTAD
202500110204     c                   eval      p3deve=dettad+'-'+§taddesc
202600110204     c                   endif
202700110204     c                   endif
202800990215     c
202900050711     c* Data spedizione bolla
203000990629     c                   clear                   p3dADS
203100990216    1c                   if        wbol<>*blanks
203200050711     c     *iso          move      spedata       datadmy
203300000215     c     *dmy          move      datadmy       p3dads
203400990629    1C                   endif
203500000214     c
203600000214     c* Natura Merce
203700000214     c                   movel     dctnas        p3dnas
203800060125     c
203900060125     c* Spedizione di valore
204000060125     c                   if        §ar5bva='V'
204100060125     c                   eval      p3dval='Valore'
204200060125     c                   else
204300060125     c                   clear                   p3dval
204400060125     c                   endif
204500060125     c
204600040726      * Descrizione pezzi danneggiati/mancanti
204700040726     c                   clear                   P3Dpdm
204800040726     c                   eval      *in13   = *off
204900040726if  1c                   if        I43sdm  = 'S'
205000040726     c                   eval      wDCTaac = DCTaac
205100040726     c                   eval      wDCTfil = DCTfil
205200040726     c                   eval      wDCTnca = DCTnca
205300040726     c                   eval      DCStpd  = 'C'
205400040726     c                   movel(p)  DS_keyca      DCSnkt
205500040726     c                   eval      DCSstd  = 'P'
205600040726     c                   clear                   DCSdim
205700040726     c                   clear                   DCShim
205800040726     c                   clear                   DCSnks
205900040726     c                   eval      DCStrc  = 'D'
206000040726     c     k07dcs01      setll     FNDCS000
206100040726     c     k07dcs01      reade     FNDCS000
206200040726do  2c                   dow       not %eof(FNDCS01L)
206300040726if  3c                   if        DCSnot <> *blanks
206400040726if  4c                   if        P3Dpdm  = *blanks
206500040726     c                   eval      P3Dpdm  = DCSnot
206600040726x   4c                   else
206700040726     c                   eval      P3Dpdm  = %trim(P3Dpdm) + ' '
206800040726     c                                     + %trim(DCSnot)
206900040726     c                   leave
207000040726e   4c                   endif
207100040726e   3c                   endif
207200040726     c     k07dcs01      reade     FNDCS000
207300040726e   2c                   enddo
207400040726     c                   eval      *in13  = (P3Dpdm <> *blanks)
207500040726e   1c                   endif
207600060124     c*
207700000214     c* Riferimenti mittente
207800000214     c                   movel     arbrmn        p3drmn
207900000214     c                   if        wbol='P' or wbol='A'
208000000214     c                   movel     arbrma        p3drma
208100000214     c                   endif
208200000214     c* Per la sede si trova in tita4
208300000214     c                   if        wbol='D'
208400000215     c                   eval      ktrc='A'
208500040206     c     kBOLtrc       chain     tita430c                           31
208600000214     c  n31              movel     ta4not        dta4a
208700000214     c                   movel     §ta4arma      p3drma
208800000214     c                   endif
208900990215     c
209000990216     c* Beneficiario
209100990216     c                   EXSR      BENEFIC
209200990216     c
209300990322     c* Mittente e destinatario
209400990322     c                   EXSR      MITDES
209500990322     c
209600000215     c
209700000215     c* Se stampo i DATI di LIQUIDAZIONE, chain su fndcl
209800000215     c                   if        *in09
209900000215     c                   EXSR      STADCL
210000000215     c
210100000215     c                   else
210200000215
210300000215     c* Se stampo i DATI della RESPOSANBILITA, leggo fndcd e fndcr
210400990420     c* Se ci sono della c.a. in attesa di ricalcolo, me lo memorizzo
210500990420     c                   if        dctcar<>'M' and dctcar<>'A' and dctcar<>' '
210600990420     c                   eval      *in06='1'
210700990420     c                   endif
210800990420     c
210900990218     c                   if        i43ord='1'
211000990218     c                   EXSR      STADET1
211100990218     c                   endif
211200990218     c                   if        i43ord='2'
211300990218     c                   EXSR      STADET2
211400990218     c                   endif
211500000215     c                   endif
211600990218     c
211700990215     c                   ENDSR
211800040204      *
211900040204      *---------------------------------------------------------------*
212000040206      * Intabellamento tab. "TAD" = Tipi Anomalia Danni               *
212100040204      *---------------------------------------------------------------*
212200040206     c     Intab_TAD     BEGSR
212300040204      *
212400040204      * Carico i tipi anomalia
212500040204     c                   clear                   ATDd
212600040204     c                   clear                   ATDs
212700040204     c                   clear                   ATDr
212800040206     c                   clear                   TIBS05ds
212900040206     c                   eval      TBI05sif =  KNSIF
213000040206     c                   eval      TBI05cod =  'TAD'
213100040206     c                   call      'TIBS05R'
213200040206     c                   parm                    TIBS05ds
213300040206      *
213400040301if  1c                   if        TBO05err <> 'N'
213500040206      *
213600040204      * - Carico i dati da tab. "TAD"
213700040206     c                   clear                   zz
213800040204     c                   movel     'TAD'         KcodT
213900040206     c                   movel     TBO05sif      Ksif
214000040204     c                   clear                   Klin
214100040206     c     Ktbe2         setll     TNTBE002
214200040206     c     Ktbe2         reade     TNTBE002
214300040204do  2c                   dow       NOT %eof(TNTBE02L)
214400040204if  3c                   if        TBEatb   =  *blanks
214500040204     c                   movel     TBEuni        dTAD
214600040206     c                   add       1             zz
214700040204     c                   movel     TBEke1        ATDd(zz)
214800040204     c                   movel     §TADdesc      ATDs(zz)
214900040204     c                   movel     §TADragr      ATDr(zz)
215000040204e   3c                   endif
215100040206     c     Ktbe2         reade     TNTBE002
215200040204e   2c                   enddo
215300040206      *
215400040206e   1c                   endif
215500040204      *
215600040204     c                   ENDSR
215700040209      *
215800040209      *---------------------------------------------------------------*
215900040209      * Cancellazione record precedentemente estratti dall'utente     *
216000040209      *---------------------------------------------------------------*
216100040209     c     Dlt_WFDDN     BEGSR
216200040209      *
216300040212     c                   open      WFDDN01L
216400040212      *
216500040212     c     KNMUS         chain     WFDDN01
216600040212      *
216700040212do  1c                   dow       %found(WFDDN01L)
216800040212     c     KNMUS         DELETE    WFDDN01
216900040209e   1c                   enddo
217000040212      *
217100040212     c                   close     WFDDN01L
217200040209      *
217300040209     c                   ENDSR
217400040204      *
217500040204      *---------------------------------------------------------------*
217600040204      * Impostazione campi e scrittura record del work-file WFDDN00F  *
217700040204      *---------------------------------------------------------------*
217800040204     c     Wrt_WFDDN     BEGSR
217900040204      *
218000040204      * Se non ho ancora chainato la bolla la cerco
218100040204     c                   exsr      CerBOL
218200040204      *
218300040205      * Decodifico i campi di output
218400040204     c                   exsr      Decod_DDN
218500040204      *
218600040205      * Imposto i campi del record da scrivere
218700040204     c                   exsr      Imp_Rec_DDN
218800040204      *
218900040211      * Reperisco il detaglio colli della C.A. e
219000040205      * Scrivo i records (1 per P.O. anche in minima parte responsabile)
219100040211cas 1c     I43ord        caseq     '1'           Rep_SC1
219200040211cas 1c     I43ord        caseq     '2'           Rep_SC2
219300040211e   1c                   endcs
219400040204      *
219500040204     c                   ENDSR
219600040204      *
219700040204      *---------------------------------------------------------------*
219800040205      * Decodifico i campi "comuni" del record da scrivere            *
219900040204      *---------------------------------------------------------------*
220000040204     c     Decod_DDN     BEGSR
220100040204      *
220200040204      * Reperimento altri dati / decodifiche
220300040204      *
220400040204      * - Fase c.a.
220500040204if  1c                   if        DCTfca <> I05fca
220600040204     c                   clear                   DS_fndfa
220700040204     c                   reset                   FIDN05ds
220800040204     c                   eval      I05fca = DCTfca
220900040204     c                   call      'FIDN05R'
221000040204     c                   parm                    KPJBA
221100040204     c                   parm                    FIDN05ds
221200040204if  2c                   if        O05rec <> *blanks
221300040204     c                   movel     O05rec        DS_fndfa
221400040204e   2c                   endif
221500040204e   1c                   endif
221600040210      *
221700040210      * - Data ultima fase C.A.
221800040210     c     Kdct2         setgt     FNDCF000
221900040210     c     Kdct2         readpe    FNDCF000
222000040210do  1c                   dow       NOT %eof(FNDCF01L)
222100040210     c                             and DCFatb <> *blanks
222200040210     c     Kdct2         readpe    FNDCF000
222300040210e   1c                   enddo
222400040210if  1c                   if            %eof(FNDCF01L)
222500040210     c                             or  DCFatb <> *blanks
222600040210     c                   clear                   DCFdfc
222700040210e   1c                   endif
222800040204      *
222900040204      * - Liquidazione c.a.
223000040204     c     Kdct2         chain     FNDCL000
223100040204if  1c                   if        NOT %found(FNDCL01L)
223200040204     c                             or  DCLatb <> *blanks
223300040204     c                   clear                   DCLipl
223400040204     c                   clear                   DCLvpl
223500040204     c                   clear                   DCLvlp
223600040204     c                   clear                   DCLasn
223700040204     c                   clear                   DCLdea
223800040204e   1c                   endif
223900040204      *
224000040204      * - Causale chiusura
224100040204if  1c                   if        DCTcch <> *blanks
224200040204     c                   eval      Wcch   =  DCTcch
224300040204     c                   exsr      DecCCH
224400040204e   1c                   endif
224500040204      *
224600040204      * - Tipo anomalia
224700040204     c                   eval      Wtad   =  DCTtad
224800040204     c                   exsr      DecTAD
224900040204      *
225000040204      * - Beneficiario generico => decodifico su FNDKA
225100150512     c                   clear                   DKArag
225200040204     c                   move      DCTksc        w0040
225300040204if  1c                   if        w0040  =  8888
225400040204     c                   eval      Ktrc   =  'C'
225500040204     c     KdctK         chain     FNDKA000
225600040204if  2c                   if        NOT %found(FNDKA01L)
225700040205     c                             or  DKAatb <> *blanks
225800040204     c                   clear                   DKArag
225900040204e   2c                   endif
226000040204e   1c                   endif
226100040204      *
226200040204      * Riferimenti mittente
226300040204     c                   clear                   dTA4a
226400040204     c                   eval      Ktrc='A'
226500040206     c     kBOLtrc       chain     TITA430C
226600040204if  1c                   if        %found(TITA430C)
226700040204     c                   movel     TA4not        dTA4a
226800040204e   1c                   endif
226900070927      * Riferimento partner
227000070927     c                   clear                   dsBL4E
227100070927     c                   eval      Ktrc='E'
227200070927     c     kBOLtrc       chain     TITA430C
227300070927if  1c                   if        %found(TITA430C)
227400070927     c                   movel     TA4not        dsbl4E
227500070927e   1c                   endif
227600040204      *
227700040204      * - Bancali
227800040204     c                   clear                   dAR5ban
227900040204     c                   eval      AR5trd = 'BAN'
228000040531     c     K05AR530      chain     FIAR531C
228100040531if  1c                   if            %found(FIAR531C)
228200040204     c                             and AR5atb = *blanks
228300040204     c                   movel     AR5uni        dAR5ban
228400040204     c                   endif
228500040204      *
228600040204     c                   ENDSR
228700040204      *
228800040204      *---------------------------------------------------------------*
228900040205      * Imposto i campi "comuni" del record da scrivere               *
229000040205      * (segnacollo e % responsabilità per P.O.: a dettaglio)         *
229100040204      *---------------------------------------------------------------*
229200040204     c     Imp_Rec_DDN   BEGSR
229300040204      *
229400040209     c                   clear                   WFDDN000
229500040209      *
229600040204      * Impostazione dei campi di output
229700040204      *
229800040204      * - Dati relativi all'estrazione
229900040204     c                   eval      DDNeut   = KNMUS
230000050613     c                   eval      DDNepo   = dutpou
230100040204     c                   eval      DDNedt   = DateU
230200040204      *
230300040204      * - Comunicazione di anomalia
230400040204     c                   eval      DDNaac   = DCTaac
230500040204     c                   eval      DDNfil   = DCTfil
230600040204     c                   eval      DDNnca   = DCTnca
230700040204     c                   eval      DDNdca   = (DCTaac * 10000) + DCTmgc
230800040204      *
230900040204      * - Fase c.a.
231000040210     c                   eval      DDNfca   = DCTfca
231100040204     c                   eval      DDNfcd   = %subst(DFAdes:1:%len(DDNfcd))
231200040210     c                   eval      DDNdfc   = DCFdfc
231300040204      *
231400040204      * - Chiusura c.a.: data e causale
231500040210if  1c                   if        DCTcch  <> *blanks
231600040204     c                   eval      DDNcch   = DCTcch
231700040204     c                   eval      DDNchd   = §CCHdesc
231800040204e   1c                   endif
231900040204      *
232000040204      * - Tipo anomalia danni: codice e descrizione
232100040204     c                   eval      DDNtad   = DCTtad
232200040204if  1c                   if        DCTtad  <> *blanks
232300040204     c                   eval      DDNdta   = §TADdesc
232400040204e   1c                   endif
232500040204      *
232600040204      * - Spedizione
232700040204     c                   eval      DDNaas   = DCTaas
232800040204     c                   eval      DDNlnp   = DCTlnp
232900040204     c                   eval      DDNnrs   = DCTnrs
233000040204     c                   eval      DDNnsp   = DCTnsp
233100040204     c                   eval      DDNdsp   = (ARBaas * 10000) + ARBmgs
233200040204      *
233300040204      * - Riferimento numerico del mittente
233400040204     c                   eval      DDNrmn   = ARBrmn
233500040204      * -      "      alfanumerico del mittente
233600040204     c                   eval      DDNrma   = §TA4Arma
233700040204      * -      "      partner
233800070927     c*********          eval      DDNrpt   = §TA4Arpt
233900070927     c                   movel     §b4ERP        DDNrpt
234000040204      *
234100040205if  1c                   if        Wbol    <> *blanks
234200040206      * - Mittente (da TIBS69R o TITAA30C)
234300040210if  2c                   if        Wporto   = 'F'
234400040210     c                   eval      DDNccm   = ARBksc
234500040210     c                   move      ARBksc        w0040
234600040210x   2c                   else
234700040210     c                   eval      DDNccm   = ARBccm
234800040210     c                   move      ARBccm        w0040
234900040210e   2c                   endif
235000040206sel 2c                   select
235100040206w   2c                   when          Wporto =  'F'
235200040210     c                             and w0040  <> 8888
235300040206     c                             OR  Wporto =  'A'
235400040210     c                             and w0040  <> *zeros
235500040210     c                             and w0040  <> 8888
235600040210     c                             and w0040  <> 9999
235700040206     c                   clear                   TIBS69ds
235800040206     c                   clear                   DS_cnaco
235900040206     c                   clear                   DS_cnind
236000040206     c                   clear                   DS_cnclp
236100040206     c                   clear                   DS_fncls
236200040206     c                   eval      I69kac   = DDNccm
236300040206     c                   eval      I69kin   = DDNccm
236400040206     c                   call      'TIBS69R'
236500040206     c                   parm                    TIBS69ds
236600040206     c                   parm                    DS_cnaco
236700040206     c                   parm                    DS_cnind
236800040206     c                   parm                    DS_cnclp
236900040206     c                   parm                    DS_fncls
237000040206if  3c                   if        O69err   = *blanks
237100040206     c                   movel     ACOrag        DDNrsm
237200040206     c                   movel     INDvia        DDNinm
237300040206     c                   movel     INDcap        DDNcam
237400040206     c                   movel     INDcit        DDNlom
237500040206     c                   movel     INDprv        DDNprm
237600040206     c                   movel     INDsta        DDNnam
237700040206e   3c                   endif
237800040206w   2c                   when           Wporto = 'F'
237900040210     c                             and  w0040  = 8888
238000040206     c                             OR   Wporto = 'A'
238100040210     c                             and (w0040  = *zeros
238200040210     c                              or  w0040  = 8888
238300040210     c                              or  w0040  = 9999)
238400040206     c                   eval      Ktrc     = 'M'
238500040206     c     kBOLtrc       chain     TITAA30C
238600040206if  3c                   if        %found(TITAA30C)
238700040206     c                   eval      DDNrsm   = TAArsc
238800040206     c                   eval      DDNinm   = TAAind
238900040206     c                   eval      DDNcam   = TAAcap
239000040206     c                   eval      DDNlom   = TAAloc
239100040206     c                   eval      DDNprm   = TAAprv
239200040206     c                   eval      DDNnad   = TAAnaz
239300040206e   3c                   endif
239400040206e   2c                   endsl
239500040206      * - Destinatario (da TITAS30C)
239600040210     c                   move      ARBksc        w0040
239700040206if  2c                   if            Wporto =  'A'
239800040206     c                             and (Wbol  =  'A'
239900040206     c                              or  Wbol  =  'D')
240000040210     c                             and w0040  <> 9999
240100040205     c                   eval      DDNccd   = ARBksc
240200040205e   2c                   endif
240300040209     c                   eval      DDNrsd   = TASrsd
240400040209     c                   eval      DDNind   = TASind
240500040209     c                   eval      DDNcad   = TAScad
240600040209     c                   eval      DDNlod   = TASlod
240700040209     c                   eval      DDNprd   = TASprd
240800040209     c                   eval      DDNnad   = TASnzd
240900040205      *
241000040205e   1c                   endif
241100040204      *
241200040204      * - Beneficiario: M=Mittente, D=Destinatario
241300040204     c                   eval      DDNptr   = DCTptr
241400040205      * - Beneficiario alternativo inserito (e già decodificato)
241500040204if  1c                   if        DCTksc   > *zeros
241600040204     c                   eval      DDNksc   = DCTksc
241700040204     c                   eval      DDNdsc   = DKArag
241800040204x   1c                   else
241900040204      * - Beneficiario NON alternativo => uso mitt o dest
242000040204if  2c                   if            Wbol   <> *blanks
242100040204     c                             and DCTptr <> *blanks
242200040204if  3c                   if        DCTptr   = 'M'
242300040204      * - - Per bolla Franco è KSC, per bolla Assegnato è CCM
242400040204sel 4c                   select
242500040204w   4c                   when      Wporto   = 'F'
242600040204     c                   eval      DDNksc   = ARBksc
242700040204w   4c                   when      ARBccm   > 0
242800040204     c                   eval      DDNksc   = ARBccm
242900040204e   4c                   endsl
243000040204     c                   eval      DDNdsc   = ARBrsm
243100040204x   3c                   else
243200040204sel 4c                   select
243300040204      * - - - bolla arrivi o bolla di sede
243400040206w   4c                   when          Wporto =  'A'
243500040206     c                             and (Wbol  =  'A' or Wbol = 'D')
243600040204     c                   move      ARBksc        w0040
243700040204if  5c                   if        w0040   <> 9999
243800040204     c                   eval      DDNksc   = ARBksc
243900040204e   5c                   endif
244000040204      * - - - bolla partenza
244100040206w   4c                   when          Wporto =  'A'
244200040206     c                             and Wbol   =  'P'
244300040206     c                   eval      Ktrc     = '1'
244400040206     c     kBOLtrc       chain     FIAR6000
244500040204     c                   if        NOT %found(FIAR601L)
244600040204     c                   clear                   AR6ksc
244700040204     c                   endif
244800040204     c                   move      AR6ksc        w004a
244900040206if  5c                   if            w004a  >  *zeros
245000040206     c                             and w004a  <> '9999'
245100040204     c                   eval      DDNksc   = AR6ksc
245200040204e   5c                   endif
245300040204e   4c                   endsl
245400040204     c                   eval      DDNdsc   = ARBrsd
245500040204e   3c                   endif
245600040204e   2c                   endif
245700040204e   1c                   endif
245800040205      * - Beneficiario non ancora decodificato
245900040204if  1c                   if            DDNksc <> *zeros
246000040204     c                             and DDNdsc =  *blanks
246100040204     c                   clear                   TIBS69ds
246200040204     c                   eval      I69kac   = DDNksc
246300040204     c                   call      'TIBS69R'
246400040204     c                   parm                    TIBS69ds
246500040204     c                   parm                    DS_cnaco
246600040204     c                   parm                    DS_cnind
246700040204     c                   parm                    DS_cnclp
246800040204     c                   parm                    DS_fncls
246900040204if  2c                   if        O69err   = *blanks
247000040204     c                   eval      DDNdsc   = ACOrag
247100040204e   2c                   endif
247200040204e   1c                   endif
247300040204      *
247400040204      * - Natura merce
247500040204     c                   eval      DDNnas   = DCTnas
247600040204      *
247700040204      * - Numero colli in bolla
247800040204     c                   eval      DDNncl   = ARBncl
247900040204      * - Peso kg in bolla
248000040204     c                   eval      DDNpkf   = ARBpkf
248100040204      * - Numero bancali in bolla
248200040204     c                   eval      DDNnba   = §AR5nba
248300040204      *
248400040204      * - Numero colli danneggiati o mancanti
248500040204     c                   eval      DDNncn   = DCTncn
248600040204      * - Peso kg danneggiati o mancanti
248700040204     c                   eval      DDNpkd   = DCTpkd
248800040204      *
248900040209      * - "Decodifica" pratica assicurativa / liquidazione transattiva
249000040204sel 1c                   select
249100040204w   1c                   when      DCTfpr   = 'T'
249200040206     c                   eval      DDNfpr   = 'LqTra'
249300040204w   1c                   when      §DCTpfra = 'P'
249400040206     c                   eval      DDNfpr   = 'PrAsF'
249500040204w   1c                   when      DCTfpr   = 'P'
249600040206     c                   eval      DDNfpr   = 'PrAss'
249700040204e   1c                   endsl
249800040204      *
249900040209      * - Pratica assicurativa: azienda, anno e numero
250000040204     c                   eval      DDNdit   = DCTdit
250100040204     c                   eval      DDNpra   = DCTpra
250200040204     c                   eval      DDNprn   = DCTprn
250300040209      * - Pratica assicurativa: importo liquidato e divisa
250400040206     c                   eval      DDNipl   = DCLipl
250500040206     c                   eval      DDNvpl   = DCLvpl
250600040209      * - Pratica assicurativa: data valuta pagamento
250700040206     c                   eval      DDNvlp   = DCLvlp
250800040204      *
250900040209      * - Assegno: numero e data
251000040206     c                   eval      DDNasn   = DCLasn
251100040204     c                   eval      DDNdea   = DCLdea
251200040204      *
251300040204     c                   ENDSR
251400040211      *
251500040211      *---------------------------------------------------------------*
251600040211      * Scrivo i records (1 x p.o. responsabile a segnacollo)         *
251700040211      *---------------------------------------------------------------*
251800040211     c     Rep_SC1       BEGSR
251900040211      *
252000040211      * ...Potrei non trovare la responsabilità senza segnacollo
252100040211     c                   eval      wTrovaDet = 'N'
252200040211      *
252300040211      * Reperisco il detaglio colli della C.A.
252400040211     c     Kdct2         setll     FNDCD000
252500040211     c     Kdct2         reade     FNDCD000
252600040211      *
252700040211do  1c                   dow       NOT %eof(FNDCD01L)
252800040211      *
252900040211      * - Seleziono il dettaglio valido e aperto
253000040211if  2c                   if             DCDatb =  *blanks
253100040211     c                             and (DCDdch =  DCTdch
253200040211     c                              or  DCDdch =  *zeros)
253300040211      * - - Imposto dati di output per segnacollo:
253400040211      *     LnA, P.O., nr. serie, nr. segnacollo
253500040211     c                   eval      DDNlna = DCDlna
253600040211     c                   eval      DDNfls = DCDfls
253700040211     c                   eval      DDNnss = DCDnrs
253800040211     c                   eval      DDNnsc = DCDnsc
253900040211      * - - Reperisco le responsabilità e ne scrivo i records nel WF
254000040211     c                   exsr      Rep_Respon
254100040211e   2c                   endif
254200040211      *
254300040211     c     Kdct2         reade     FNDCD000
254400040211e   1c                   enddo
254500040211      *
254600040211      *
254700040211      * SE non ho trovato segnacolli: registro la responsabilità
254800040211      *    senza il segnacollo (come se non ci fosse)
254900040211if  1c                   if        wTrovaDet  =  'N'
255000040211      *
255100040211     c                   eval      DCDlna = DCTlna
255200040211     c                   clear                   DCDfls
255300040211     c                   clear                   DCDnrs
255400040211     c                   clear                   DCDnsc
255500040211     c                   eval      DDNlna = DCTlna
255600040211     c                   clear                   DDNfls
255700050906     c                   clear                   DDNnss
255800040211     c                   clear                   DDNnsc
255900040211     c     Kdct2         setll     FNDCR000
256000040211     c     Kdct2         reade     FNDCR000
256100040211do  2c                   dow       NOT %eof(FNDCR02L)
256200040211     c                             and DCRatb <> *blanks
256300040211     c     Kdct2         reade     FNDCR000
256400040211e   2c                   enddo
256500040211      *
256600040211if  2c                   if        %eof(FNDCR02L)
256700040211      * - Se non esiste nemmeno un record registro casuale "NOCL"
256800040211     c                   eval      DCRflo = '       NOCL    '
256900040211     c                   clear                   DCRpor
257000040211     c                   clear                   DCRper
257100040211     c                   clear                   DCRflo
257200040211     c                   clear                   DCRtcr
257300040211     c                   exsr      Wrt_Rec_DDN
257400040211x   2c                   else
257500040211      * - Altrimenti registro la casuale reperita
257600040211do  3c                   dou       %eof(FNDCR02L)
257700040211if  4c                   if        DCRatb = *blanks
257800040211     c                   exsr      Wrt_Rec_DDN
257900040211e   4c                   endif
258000040211     c     Kdct2         reade     FNDCR000
258100040211e   3c                   enddo
258200040211e   2c                   endif
258300040211      *
258400040211e   1c                   endif
258500040211      *
258600040211     c                   ENDSR
258700040204      *
258800040204      *---------------------------------------------------------------*
258900040206      * Reperimento responsabilità per segnacollo                     *
259000040204      *---------------------------------------------------------------*
259100040205     c     Rep_Respon    BEGSR
259200040204      *
259300040204     c                   movel     DCDnsc        Kseg
259400040206     c     Kdcdr         setll     FNDCR000
259500040206     c     Kdcdr         reade     FNDCR000
259600040204      *
259700040206do  1c                   dow       NOT %eof(FNDCR02L)
259800040204      *
259900040206if  2c                   if        DCRatb    = *blanks
260000040205     c                   exsr      Wrt_Rec_DDN
260100040206e   2c                   endif
260200040205      *
260300040205     c     Kdcdr         reade     FNDCR000
260400040205      *
260500040206e   1c                   enddo
260600040204      *
260700040204     c                   ENDSR
260800040211      *
260900040211      *---------------------------------------------------------------*
261000040211      * Scrivo il record (uno) per il p.o. responsabile in esame      *
261100040211      *---------------------------------------------------------------*
261200040211     c     Rep_SC2       BEGSR
261300040211      *
261400040211      * ...Potrei non trovare la responsabilità senza segnacollo
261500040211     c                   eval      wNOdet   = 'S'
261600040211      *
261700040211      * Controllo se esiste il detaglio colli della C.A.
261800040211     c                   movel     DCRflo        dDCR01
261900040211if  1c                   if        §DCRnsc  >  *zeros
262000040211      *
262100040211      * - Reperisco il detaglio colli della C.A.
262200040211     c                   movel     §DCRnsc       w0070
262300040211     c     kDCRd         chain     FNDCD000
262400040211if  2c                   if            %found(FNDCD01L)
262500040211     c                             and  DCDatb = *blanks
262600040211     c                             and (DCDdch = DCTdch
262700040211     c                              or  DCDdch = *zeros)
262800040211      * - - Imposto dati di output per segnacollo:
262900040211      *     LnA, P.O., nr. serie, nr. segnacollo
263000040211     c                   eval      DDNlna = DCDlna
263100040211     c                   eval      DDNfls = DCDfls
263200040211     c                   eval      DDNnss = DCDnrs
263300040211     c                   eval      DDNnsc = DCDnsc
263400120713     c***                eval      DDNcan = DCDcan
263500120712     c* decodifica
263600120713     c***                if        ddncan<>' '
263700120713     C**+                movel     '3E'          kCOD
263800120713     C***                movel(P)  ddncan        kKEY
263900120713     C***  ktab          CHAIN     TABEL                              31
264000120713     C*+n31              movel     TBLUNI        ds3e
264100120713     c** 31              clear                   ds3e
264200120713     c**                 eval      ddndcan=§3Edes
264300120713     c****               endif
264400120712     c
264500040211      * - - Scrivo direttamente il record (relativo alla
264600040211      *     responsabilità in esame) nel WF
264700040211     c                   eval      wNoDet = 'N'
264800040211     c                   exsr      Wrt_Rec_DDN
264900040211e   2c                   endif
265000040211      *
265100040211e   1c                   endif
265200040211      *
265300040211      * SE non ho trovato segnacolli: registro la responsabilità
265400040211      *    senza il segnacollo (come se non ci fosse)
265500040211if  1c                   if        wNoDet = 'S'
265600040211     c                   eval      DCDlna = DCTlna
265700040211     c                   clear                   DCDfls
265800040211     c                   clear                   DCDnrs
265900040211     c                   clear                   DCDnsc
266000040211     c                   eval      DDNlna = DCTlna
266100040211     c                   clear                   DDNfls
266200050906     c                   clear                   DDNnss
266300040211     c                   clear                   DDNnsc
266400040211     c                   exsr      Wrt_Rec_DDN
266500040211e   1c                   endif
266600040211      *
266700040211     c                   ENDSR
266800040205      *
266900040205      *---------------------------------------------------------------*
267000040205      * Scrittura singolo record dwl workfile WFDDN00F                *
267100040205      *---------------------------------------------------------------*
267200040205     c     Wrt_Rec_DDN   BEGSR
267300040205      *
267400040206      * Decodifica ultimi campi
267500040205      *
267600040206      * - P.O. responsabile del danno e relativa %
267700040206     c                   eval      DDNpor   = DCRpor
267800040206     c                   eval      DDNper   = DCRper
267900040206      *
268000040206      * - Tipo attribuzione reponsabilità
268100040206     c                   movel     DCRflo        dDCR01
268200120713     c                   if        §DCRTER<=*zeros
268300120713     c                   eval      wfil   =dcrpor
268400120713     c                   else
268500120713     c                   eval      wfil=%int(§dcrter)
268600120713     c                   endif
268700120713     c
268800120713     c                   eval      ddnter=wfil
268900120713     c
269000120713     c     wfil          chain     azorg01l                           31
269100120713     c  n31              movel     orgcar        ddnare
269200120713     c   31              movel     *blanks       ddnare
269300120713     c
269400120713     c                   if        ddnare >*zeros
269500120713     C                   movel     '05'          kCOD
269600120713     C                   movel(P)  ddnare        kKEY
269700120713     C     ktab          CHAIN     TABEL                              31
269800120713     C  n31              movel     TBLUNI        ddndare
269900120713     c   31              clear                   ddndare
270000120713     c                   endif
270100120713     c
270200040206     c                   movel     §DCRtre       DDNtre
270300040206     c                   clear                   dTRE
270400040206if  1c                   if        DDNtre  <> *blanks
270500040206     c                   clear                   TIBS02ds
270600040206     c                   eval      T02mod   = 'C'
270700040206     c                   eval      T02sif   = KNSIF
270800040206     c                   eval      T02cod   = 'TRE'
270900040206     c                   movel     DDNtre        T02ke1
271000040206     c                   call      'TIBS02R'
271100040206     c                   parm                    KPJBA
271200040206     c                   parm                    TIBS02ds
271300040206if  2c                   if        T02err  =  *blanks
271400040206     c                   movel     T02uni        dTRE
271500040206e   2c                   endif
271600040206     c                   movel     §TREdesc      DDNtrd
271700040206e   1c                   endif
271800040206      *
271900040206      * - Tabella calcolo responsabilità
272000040206if  1c                   if        DCRtcr  <> *blanks
272100040205     c                   clear                   TIBS02ds
272200040205     c                   eval      T02mod   = 'C'
272300040205     c                   eval      T02sif   = KNSIF
272400040206     c                   eval      T02cod   = 'TCR'
272500040206     c                   movel     DCRtcr        T02ke1
272600040206     c                   call      'TIBS02R'
272700040206     c                   parm                    KPJBA
272800040206     c                   parm                    TIBS02ds
272900040206if  2c                   if        T02err = *BLANKS
273000040206     c                   movel     T02uni        DDNtcr
273100040206e   2c                   endif
273200040206e   1c                   endif
273300040205      *
273400040206     c                   write     WFDDN000
273500040205      *
273600040205     c                   eval      wTrovaDet = 'S'
273700040205      *
273800040205     c                   ENDSR
273900040204      *
274000990406      *------------------------------------------------------------------------*
274100990406      * Decodifico tipo anomalia
274200990406      *------------------------------------------------------------------------*
274300990406     c     DECTAD        begsr
274400990406     c                   if        savtad<>wtad
274500990513     c                   eval      YY=1
274600990513     c     wtad          lookup    atdd(yy)                               32
274700990406      *
274800990513    2C                   IF        *in32
274900990513     C                   movel     atds(yy)      §taddesc
275000990513     C                   movel     atdr(yy)      §tadragr
275100990406     c                   else
275200990406     c                   eval      §taddesc=*all'?'
275300990406     c                   eval      §tadragr=*all'?'
275400990406    2c                   endif
275500990406     c                   eval      savtad=wtad
275600990406    1c                   endif
275700990406     c                   endsr
275800990406      *------------------------------------------------------------------------*
275900990406      * Decodifico causale chiusura
276000990406      *------------------------------------------------------------------------*
276100990406     c     DECCCH        begsr
276200990406    1c                   if        wcch<>savcch
276300990406     C                   clear                   tibs02ds
276400990406     C                   MOVEL     'C'           t02mod
276500990406     C                   MOVEL     knsif         t02sif
276600990406     C                   MOVEL     'CCH'         t02cod
276700990406     C                   MOVEL     wcch          t02ke1
276800990406     C                   CALL      'TIBS02R'
276900990406     C                   PARM                    KPJBA
277000990406     C                   PARM                    TIBS02DS
277100990406      *
277200990406    2C                   IF        t02err = *BLANKS
277300990406     C                   movel     T02uni        DCCH
277400990406     c                   else
277500990406     c                   clear                   dcch
277600990406    2c                   endif
277700990406     c
277800990406     c                   eval      savcch=wcch
277900990406    1c                   endif
278000990406     c                   ENDSR
278100000215      *------------------------------------------------------------------------*
278200000215      * STAMPO DATI DI LIQUIDAZIONE se richiesto per  ordinamento 1
278300000215      *------------------------------------------------------------------------*
278400000215     c     STADCL        BEGSR
278500000215     c                   clear                   WRECSTA
278600000215     c                   clear                   WRECSTAa
278700000215     c                   clear                   WRECSTAd
278800000215     c                   clear                   WRECSTAn
278900000215     c                   clear                   dclipl
279000000215     c                   clear                   dclvpl
279100000215     c                   clear                   p3dvlp
279200000215     c                   clear                   p3ddea
279300000215     c                   clear                   dclasn
279400000215     c                   clear                   p3ddav
279500000215     c
279600000215     c* Addiziono alle bolle stampate
279700000215     c                   eval      wtiptot='S'
279800000215     c                   exsr      CALTOT
279900000215     c
280000000215     c* Leggo il det.colli della C.A. e poi la responsabilita'
280100000215     c     kdct2         chain     fndcl01l                           31
280200000215    1c                   if        not *in31
280300000215     c
280400000215     c* Data valuta pagamento
280500000215    1c                   if        dclvlp>0
280600000215     c     *iso          move      dclvlp        datadmy
280700000215     c     *dmy          move      datadmy       p3dvlp
280800000215    1c                   endif
280900000215     c* Data assegno
281000000215    1c                   if        dcldea>0
281100000215     c     *iso          move      dcldea        datadmy
281200000215     c     *dmy          move      datadmy       p3ddea
281300000215    1c                   endif
281400000215    1c                   endif
281500000215     c
281600000215     c* Cerco la data di avviso danno
281700000215     c                   if        dctprn>0
281800000215     c     kdct2         setgt     fndcf01l                           31
281900000215     c     kdct2         readpe    fndcf01l                               31
282000000215     c                   dow       not *in31 AND dcffca<>545
282100000215     c     kdct2         readpe    fndcf01l                               31
282200000215     c                   enddo
282300000215     c                   if        not *in31 and dcffca=545
282400000215     c     *iso          move      dcfdfc        datadmy
282500000215     c     *dmy          move      datadmy       p3ddav
282600000215     c                   endif
282700000215     c                   endif
282800000215     c
282900000215     c* Stampa testata e dettaglio
283000000215     c                   EXSR      EXCPTOUT
283100000215     c
283200000215     c                   ENDSR
283300990218      *------------------------------------------------------------------------*
283400990406      * STAMPO DETTAGLIO C.A. E REPONSABILITA' PER ordinamento 1
283500990218      *------------------------------------------------------------------------*
283600990218     c     STADET1       BEGSR
283700990218     c                   clear                   WRECSTA
283800990309     c                   clear                   WRECSTAa
283900990309     c                   clear                   WRECSTAd
284000990312     c                   clear                   WRECSTAn
284100990512     c
284200990512     c* Addiziono alle bolle stampate
284300990512     c                   eval      wtiptot='S'
284400990512     c                   exsr      CALTOT
284500990512     c
284600990421     c                   eval      WTROVADET='N'
284700990218     c
284800990218     c* Leggo il det.colli della C.A. e poi la responsabilita'
284900990218     c     kdct2         chain     fndcd01l                           31
285000990421    1c                   if        not *in31
285100990218     c
285200990218    2c                   dow       not *in31
285300990218     c* Dettaglio valido e aperto
285400990218    3c     dcdatb        ifeq      ' '
285500990218     c     dcddch        andeq     dctdch
285600990421    3c     dcdatb        oreq      ' '
285700990421     c     dcddch        andeq     0
285800990218     c* stampo--> leggo la responsabilita' e stampo
285900990218     c                   movel     dcdnsc        kseg
286000990218     c     kdcdr         chain     fndcr02l                           32
286100990218     c
286200990421     c* Posso non trovare la responsabilita' senza segnacollo
286300990421     c                   if        not *in32
286400990218     c
286500990218    4c                   dow       not *in32
286600990218     c                   if        dcratb=' '
286700990421     c                   eval      wtrovadet='S'
286800990218     c* Decodifica campi
286900990218     c                   EXSR      DECODDCR
287000990218     c* Stampa testata e dettaglio
287100990312     c                   EXSR      EXCPTOUT
287200990218     c                   endif
287300990218     c
287400990218     c     kdcdr         reade     fndcr02l                               32
287500990218    4c                   enddo
287600990218     c                   endif
287700990218     c
287800990218    3c                   endif
287900990218     c     kdct2         reade     fndcd01l                               31
288000990218    2c                   enddo
288100990315    1c                   endif
288200990218     c
288300990218     c* Stampo la responsabilita' senza il segnacollo(come se non ci fosse)
288400990218     c                   if        wtrovadet='N'
288500990218     c                   EXSR      STANODETT
288600990218     c                   endif
288700990218     c
288800990218     c                   ENDSR
288900990218      *------------------------------------------------------------------------*
289000990218      * STAMPO DETTAGLIO C.A. E REPONSABILITA' PER ordinamento 2
289100990218      *------------------------------------------------------------------------*
289200990218     c     STADET2       BEGSR
289300100114     c                   clear                   Wnodet
289400990218     c                   if        dctspe<>savspe
289500990218     c                   clear                   WRECSTA
289600990309     c                   clear                   WRECSTAa
289700990309     c                   clear                   WRECSTAd
289800990312     c                   clear                   WRECSTAn
289900990218     c
290000990503     c** Calcolo i totali : spedizioni
290100990503     c                   eval      wtiptot='S'
290200990503     c                   exsr      CALTOT
290300990218     c                   movel     dctspe        savspe
290400990218     c                   endif
290500990218     c
290600990218     c* Se c'e' il segnacollo nella responsabilita' leggo solo
290700990218     c*  il suo dcd altrimenti li leggo tutti
290800040211      * => FALSO !!!
290900040211      * => ...Altrimenti non leggo nulla ed imposto il flag WNODET='S'
291000990218     c                   movel     dcrflo        ddcr01
291100990218     c
291200990218     c                   if        §dcrnsc>*zeros
291300990218     c                   movel     §dcrnsc       w0070             7 0
291400990218     c     kdcrd         chain     fndcd01l                           32
291500990218     c
291600990420     c                   if        not *in32  AND
291700990421     c                             dcddch=dctdch and dcdatb=' '  OR
291800990421     c                             not *in32  AND
291900990421     c                             dcddch=0      and dcdatb=' '
292000990218     c* Decodifica campi
292100990218     c                   EXSR      DECODDCR
292200990218     c* Stampa testata e dettaglio
292300990218     c                   EXSR      EXCPTOUT
292400990420     c
292500990420     c                   else
292600080804     c                   eval      wnodet='S'
292700990218     c                   endif
292800990218     c
292900990218     c                   else
293000990420     c                   eval      wnodet='S'
293100990420     c                   endif
293200990420     c
293300990420     c                   if        wnodet='S'
293400990218     c                   clear                   dcdnsc
293500990218     c                   clear                   dcdfls
293600990218     c                   clear                   dcdnrs
293700990218     c                   eval      dcdlna=dctlna
293800990218     c* Decodifica campi
293900990218     c                   EXSR      DECODDCR
294000990218     c* Stampa testata e dettaglio
294100990218     c                   EXSR      EXCPTOUT
294200990218     c                   endif
294300990218     c
294400990218     c                   ENDSR
294500990503      *------------------------------------------------------------------------*
294600990503      * CALCOLO I TOTALI
294700990503      *------------------------------------------------------------------------*
294800990503     c     CALTOT        BEGSR
294900990503     c* Addiziono 1 alle bolle stampate
295000990503     c                   if        wtiptot='S'
295100990503     c                   add       1             p3ctps
295200990503     c                   else
295300990503     c                   add       dcrper        p3ctpc
295400990503     c                   endif
295500990503     c
295600990503     c* Addiziono  per tipo anomalia
295700990503     c                   eval      zz=1
295800990510     c     dcttad        lookup    ftdd(zz)                               32
295900990503     c
296000990513     c                   if        wtiptot='S'   AND *in32
296100990510     c                   add       1             ftas(zz)
296200990503     c                   else
296300990510     c                   add       dcrper        ftac(zz)
296400990503     c                   endif
296500990503     c
296600000215     c* Addiziono  per codice filtro : solo se richiesti i dati della respon
296700000215     c                   if        not *in09
296800990503     c                   eval      zz=1
296900000323     c                   movel(p)  drrcfl        w003a
297000000323     c     w003a         lookup    fcfl(zz)                               32
297100990503     c
297200000323     c* Se non lo trovo cerco col tipo P/A/T
297300000323     c                   if        not *in32
297400000323     c                   move      dcrtcr        w003a
297500000323     c     w003a         lookup    fcfl(zz)                               32
297600000323     c                   endif
297700000323     c
297800990513     c                   if        wtiptot='S'  AND *in32
297900990510     c                   add       1             ftfs(zz)
298000990503     c                   else
298100990510     c                   add       dcrper        ftfc(zz)
298200990503     c                   endif
298300000215     c                   endif
298400990503     c                   ENDSR
298500990310      *------------------------------------------------------------------------*
298600990310      * STAMPO I TOTALI DI AREA
298700990310      *------------------------------------------------------------------------*
298800990310     c     TOTAREA       BEGSR
298900990310     c                   if        p3ctas>0
299000990310     c                   eval      p3ctps=p3ctas
299100990310     c                   eval      p3ctpc=p3ctac
299200990310     c
299300990310     c                   movel     'A'           WTOT              1
299400990310     c                   clear                   p3dtoan
299500990310     c                   clear                   p3dtopo
299600990310     c                   clear                   p3dtode
299700990310     c                   move      p3care        p3dtopo
299800990310     c                   movel     p3dare        p3dtode
299900990310     c                   eval      p3dtotr='-'
300000990310     c                   exsr      EXCPTOUT
300100990310     c                   movel     ' '           WTOT
300200990310     c
300300990510     c                   EXSR      CALTOTDIS
300400990310     c
300500990310     c                   clear                   p3ctas
300600990310     c                   clear                   p3ctac
300700990310     c                   clear                   p3ctps
300800990310     c                   clear                   p3ctpc
300900990510     c                   clear                   ftas
301000990510     c                   clear                   ftac
301100990510     c                   clear                   ftfs
301200990510     c                   clear                   ftfc
301300990510     c                   clear                   atas
301400990510     c                   clear                   atac
301500990510     c                   clear                   atfs
301600990510     c                   clear                   atfc
301700990310     c                   endif
301800990310     c                   ENDSR
301900990310      *------------------------------------------------------------------------*
302000990310      * STAMPO I TOTALI DI DISTRETTO
302100990310      *------------------------------------------------------------------------*
302200990310     c     TOTDISTR      BEGSR
302300990310     c                   if        p3ctds>0
302400990310     c                   eval      p3ctps=p3ctds
302500990310     c                   eval      p3ctpc=p3ctdc
302600990310     c
302700990310     c                   movel     'D'           WTOT              1
302800990310     c                   clear                   p3dtoan
302900990310     c                   clear                   p3dtopo
303000990310     c                   clear                   p3dtode
303100990310     c                   move      p3cdst        p3dtopo
303200990310     c                   movel     p3ddst        p3dtode
303300990310     c                   eval      p3dtotr='-'
303400990312     c                   eval      wstaare='N'
303500990310     c                   exsr      EXCPTOUT
303600990312     c                   eval      wstaare=' '
303700990310     c                   movel     ' '           WTOT
303800990310     c
303900990310     c                   clear                   p3ctds
304000990310     c                   clear                   p3ctdc
304100990310     c                   clear                   p3ctps
304200990310     c                   clear                   p3ctpc
304300990510     c                   clear                   ftas
304400990510     c                   clear                   ftac
304500990510     c                   clear                   ftfs
304600990510     c                   clear                   ftfc
304700990510     c                   clear                   dtas
304800990510     c                   clear                   dtac
304900990510     c                   clear                   dtfs
305000990510     c                   clear                   dtfc
305100990310     c                   endif
305200990310     c                   ENDSR
305300990218      *------------------------------------------------------------------------*
305400990218      * ROTTURE E SALTI PAGINA
305500990218      *------------------------------------------------------------------------*
305600990218     c     ROTTURE       BEGSR
305700990218     c
305800990512     C* O R D I N A M E N T O 1
305900990218     c                   clear                   dctord1           7
306000990218     c                   movel     dctaac        dctord1
306100990218     c                   move      dctfil        dctord1
306200990218     c
306300990512     c
306400040202if  1c                   if        i43ord='1'  AND
306500990218     c                             dctord1<>savord1
306600990218     c* Al primo giro non stampo i totali
306700040202if  2c                   if        savord1<>*blanks
306800990218     c
306900990218     c* Stampo i campi parziali
307000990218     c                   clear                   p3dtoan
307100990218     c                   clear                   p3dtode
307200990218     c                   clear                   p3dtopo
307300990218     c                   clear                   p3dtotr
307400990218     c
307500990218     c                   movel     savord1       w004a
307600990218     c                   movel     'Anno'        p3dtoan
307700990218     c                   move      w004a         p3dtoan
307800990218     c* P.O.
307900990218     c                   move      savord1       w0030             3 0
308000990218     c     w0030         chain     azorg01l                           32
308100990218     c  N32              movel     orgdes        p3dtode
308200990218     c                   movel     '-'           p3dtotr
308300990218     c                   move      w0030         p3dtopo
308400990218     c                   movel     COSPOAP       p3dtopo
308500990512     c
308600990512     c                   EXSR      STAPO
308700990218     c
308800040202e   2c                   endif
308900040202     c
309000990218     c                   eval      savord1=dctord1
309100990218     c
309200040202e   1c                   endif
309300990218     c
309400990218     c
309500990312     C* O R D I N A M E N T O   2
309600990218     c                   movel     dcrpor        w003a             3
309700040202if  1c                   if        i43ord='2' AND
309800990310     c                             w003a<>savord2
309900990310     c
310000990218     c* Al primo giro non stampo i totali
310100040202if  2c                   if        savord2<>*blanks
310200990218     c
310300990218     c* Stampo i campi parziali
310400990218     c                   clear                   p3dtoan
310500990218     c                   clear                   p3dtode
310600990218     c                   clear                   p3dtopo
310700990218     c                   clear                   p3dtotr
310800990218     c
310900990218     c* P.O.
311000040202if  3c                   if        savord2>*zeros
311100990218     c                   movel     savord2       w0030
311200990218     c     w0030         chain     azorg01l                           32
311300990218     c  N32              movel     orgdes        p3dtode
311400990430     c   32              clear                   orgfl1
311500990218     c                   else
311600990218     c                   movel(p)  COSNES        p3dtode
311700040202e   3c                   endif
311800990218     c
311900990218     c                   movel     '-'           p3dtotr
312000990218     c                   move      w0030         p3dtopo
312100990218     c                   movel     COSPORE       p3dtopo
312200990218     c
312300990512     c                   EXSR      STAPO
312400040202e   2c                   endif
312500990310     c
312600990218     c                   movel     dcrpor        savord2
312700040202e   1c                   endif
312800990218     C                   ENDSR
312900990512      *------------------------------------------------------------------------*
313000990512      * Stampo i totali per P.O.
313100990512      *------------------------------------------------------------------------*
313200050708     c     STAPO         BEGSR
313300990512     c* TOTALI P.O.
313400050708     c                   if        not *in09 and i43toi<>'G'
313500990512     c                   movel     'P'           WTOT
313600990512     c* creo rottura di bolla in quanto c'e' nuova spedizione
313700990512     c                   clear                   savspe
313800990512     c                   EXSR      excptout
313900990512     c                   movel     ' '           WTOT
314000000215     c                   endif
314100000215     c
314200990512     c                   eval      Wnuovopo='S'
314300990512     c
314400990512     c* Addiziono nei totali generali
314500990512     c                   EXSR      CALTOTGEN
314600990512     c* Addiziono nei totali di area
314700990512     c                   EXSR      CALTOTARE
314800990512     c
314900990512     c                   clear                   p3ctpc
315000990512     c                   clear                   p3ctps
315100990512     c                   clear                   ftas
315200990512     c                   clear                   ftac
315300990512     c                   clear                   ftfs
315400990512     c                   clear                   ftfc
315500990512     c                   ENDSR
315600990510      *------------------------------------------------------------------------*
315700990510      * Calcolo totali generali
315800990510      *------------------------------------------------------------------------*
315900990510     c     CALTOTGEN     BEGSR
316000000216     c* Escludo dai totali generali il distretto internazionale
316100000216     c                   if        orgfl3<>'7' OR simfel>0
316200990510     c                   add       p3ctps        p3ctgs
316300990510     c                   add       p3ctpc        p3ctgc
316400990510     c
316500990510     c* totali per filtro
316600990510     c                   eval      zz=1
316700990510     c                   dow       fcfl(zz)<>*blanks
316800990510     c                   if        ftfs(zz)>0
316900990511     c                   eval      xx=1
317000990510     c     fcfl(zz)      lookup    gcfl(xx)                               32
317100990510     c                   add       ftfs(zz)      gtfs(xx)
317200990510     c                   add       ftfc(zz)      gtfc(xx)
317300990510     c                   endif
317400990510     c                   add       1             zz
317500990510     c                   enddo
317600990510     c
317700990510     c* totali per tipo anomalia
317800990510     c                   eval      zz=1
317900990510     c
318000990510    1c                   dow       ftdd(zz)<>'  '
318100990511     c                   eval      xx=1
318200990510     c     ftdd(zz)      lookup    gtdd(xx)                               32
318300990513     c                   if        *in32
318400990510     c                   add       ftas(zz)      gtas(xx)
318500990510     c                   add       ftac(zz)      gtac(xx)
318600990513     c                   endif
318700990513     c                   add       1             zz
318800990510     c                   enddo
318900990510     c
319000990510     c                   else
319100990510     c                   eval      westero='S'
319200990510     c                   endif
319300990510     c                   ENDSR
319400990510      *------------------------------------------------------------------------*
319500990510      * Calcolo totali distretto
319600990510      *------------------------------------------------------------------------*
319700990510     c     CALTOTDIS     BEGSR
319800990510     c                   add       p3ctas        p3ctds
319900990510     c                   add       p3ctac        p3ctdc
320000990510     c* totali per filtro
320100990510     c                   eval      zz=1
320200990510     c                   dow       acfl(zz)<>*blanks
320300990510     c                   if        atfs(zz)>0
320400990511     c                   eval      xx=1
320500990510     c     acfl(zz)      lookup    dcfl(xx)                               32
320600990510     c                   add       atfs(zz)      dtfs(xx)
320700990510     c                   add       atfc(zz)      dtfc(xx)
320800990510     c                   endif
320900990510     c                   add       1             zz
321000990510     c                   enddo
321100990510     c
321200990510     c* totali per tipo anomalia
321300990510     c                   eval      zz=1
321400990510     c
321500990510    1c                   dow       atdd(zz)<>'  '
321600990511     c                   eval      xx=1
321700990510     c     atdd(zz)      lookup    dtdd(xx)                               32
321800990513     c                   if        *in32
321900990510     c                   add       atas(zz)      dtas(xx)
322000990510     c                   add       atac(zz)      dtac(xx)
322100990513     c                   endif
322200990513     c                   add       1             zz
322300990510     c                   enddo
322400990510     c                   ENDSR
322500990510      *------------------------------------------------------------------------*
322600990510      * Calcolo totali area
322700990510      *------------------------------------------------------------------------*
322800990510     c     CALTOTARE     BEGSR
322900990510     c                   add       p3ctps        p3ctas
323000990510     c                   add       p3ctpc        p3ctac
323100990510     c
323200990510     c* totali per filtro
323300990510     c                   eval      zz=1
323400990510     c                   dow       fcfl(zz)<>*blanks
323500990510     c                   if        ftfs(zz)>0
323600990511     c                   eval      xx=1
323700990510     c     fcfl(zz)      lookup    acfl(xx)                               32
323800990510     c                   add       ftfs(zz)      atfs(xx)
323900990510     c                   add       ftfc(zz)      atfc(xx)
324000990510     c                   endif
324100990510     c                   add       1             zz
324200990510     c                   enddo
324300990510     c
324400990510     c* totali per tipo anomalia
324500990510     c                   eval      zz=1
324600990510     c
324700990510    1c                   dow       ftdd(zz)<>'  '
324800990511     c                   eval      xx=1
324900990510     c     ftdd(zz)      lookup    atdd(xx)                               32
325000990513     c                   if        *in32
325100990510     c                   add       ftas(zz)      atas(xx)
325200990510     c                   add       ftac(zz)      atac(xx)
325300990513     c                   endif
325400990510     c                   add       1             zz
325500990510     c                   enddo
325600990510     c
325700990510     c                   ENDSR
325800990310      *------------------------------------------------------------------------*
325900050908      * Salto riga o pagine a cambio p.o.
326000990310      *------------------------------------------------------------------------*
326100990310     c     SALPAG2       begsr
326200990310    3c                   if        i43sap='S'
326300990310     c                   eval      *in02='1'
326400990310     c                   eval      *in03='1'
326500990310     c                   eval      *in04='1'
326600990310   x3c                   else
326700990310     c
326800990310     c* Lascio solo 2 righe vuote
326900990428     c                   if        i43usi='S'
327000050708     c                   if        i43toi=' ' OR
327100050613     c                             (*in01 AND wnuovo='S') or
327200050613     c                             (wnuovo='S' and i43dst<>*blanks) or
327300050613     c                             (wnuovo='S' and i43are<>*blanks)
327400990310     c                   write     dn44z2
327500990428     c                   endif
327600990323     c* Se primo p.o. di area nuova, stampo anche la testata distretto/area
327700990323     c*
327800050613     c                   if        wnuovo='S'
327900050613     c                   if        *in01 or
328000050613     c                             (i43dst<>' ' or i43are<>*blanks)
328100990323     c                   write     dn44t3
328200990323     c                   write     dn44t4
328300990323     c                   write     dn44t3
328400990428     c                   endif
328500050613     c                   endif
328600990428     c                   endif
328700990323     c
328800990310    3c                   endif
328900990323     c
329000990323     c                   clear                   wnuovo
329100990310     c                   ENDSR
329200990216      *------------------------------------------------------------------------*
329300990216      * BENEFICIARIO
329400990216      *------------------------------------------------------------------------*
329500990216     c     BENEFIC       BEGSR
329600990322     c* Solo se non l'ho ancora cercato
329700990322     c                   if        wbenef=' '
329800990322     c                   eval      wbenef='S'
329900990322     c
330000990216     c                   clear                   p3dksc
330100990217     c                   clear                   p3cksc
330200990216     c*
330300990216     c* Beneficiario alternativo inserito
330400990216    1c                   if        dctksc>0
330500990216     c                   movel     dctksc        p3cksc
330600040206     c                   move      p3cksc        w0040
330700990216     c
330800990216     c*Cod.generico --> chain in FNDKA per prendere la ragione sociale
330900990216    2c                   if        w0040=8888
331000990216     c                   eval      ktrc='C'
331100990216     c     kdctk         chain     fndka01l                           31
331200990216    3c                   if        not *in31
331300990216     c                   movel     dkarag        p3dksc
331400990216    3c                   endif
331500990216    2c                   endif
331600990216     c
331700990216   x1c                   else
331800990216     c* non c'e' beneficiario alternativo--> uso mitt o dest
331900990216     c*
332000010706    2c                   if        wbol <> ' ' and dctptr <> ' '
332100990216     c
332200990217    3c                   if        dctptr='M'
332300990216     c* Per bolla franco e' ksc, per bolla assegnato e' ccm
332400050711    4c**                 select
332500050711     c**                 when      wporto='F'
332600050711     c**                 movel     arbksc        p3cksc
332700050711     c**                 when      arbccm>0
332800050711     c**                 movel     arbccm        p3cksc
332900050711     c**                 other
333000050711    4c**                 endsl
333100050711     c**                 movel     arbrsm        p3dksc
333200050711     c
333300050711     c                   movel     speccm        p3cksc
333400050711     c                   movel     spersm        p3dksc
333500990216     c
333600990217   x3c                   else
333700050711    4c**                 select
333800990218     c
333900990218     c* SE bolla arrivi o bolla di sede
334000050711     c**                 when      wporto='A' and wbol='A'  OR
334100050711     c**                           wporto='A' and wbol='D'
334200990218     c
334300050711     c**                 move      arbksc        w0040
334400050711    5c**                 if        w0040=9999
334500050711     c**                 else
334600050711     c**                 movel     arbksc        p3cksc
334700050711    5c**                 endif
334800990218     c* Se bolla Partenza
334900050711    4c**                 when      wporto='A' and wbol='P'
335000990216     c*
335100050711     c**                 eval      Ktrc     = '1'
335200050711     c**   kBOLtrc       chain     fiar601l                           31
335300050711     c** 31              clear                   ar6ksc
335400050711     c**                 move      ar6ksc        w004a             4
335500050711    5c**   w004a         ifgt      *zeros
335600050711     **    w004a         andne     '9999'
335700050711     c*                  movel     ar6ksc        p3cksc
335800050711   x5c**                 else
335900050711    5c**                 endif
336000990216     c
336100050711   x4c**                 other
336200050711    4c**                 endsl
336300050711     c**
336400050711     c**                 movel     arbrsd        p3dksc
336500050711     c
336600050711     c                   move      speccd        w004a             4
336700050711    4c                   if        w004a>*zeros and w004a<>'9999'
336800050711     c                   movel     speccd        p3cksc
336900050711    4c                   endif
337000050711     c                   movel     spersd        p3dksc
337100990217    3c                   endif
337200990216    c
337300990217    2c                   endif
337400990217    1c                   endif
337500990322     c
337600990322     c* Se non e' ancora decodificato lo faccio ora
337700990322    1c                   if        p3dksc=*blanks and p3cksc<>*blanks
337800990322     C                   clear                   TIBS69DS
337900990322     c                   movel     p3cksc        I69kac
338000990322      *
338100990322     C                   CALL      'TIBS69R'
338200990322     C                   PARM                    tibs69DS
338300990322     C                   PARM                    DS_cnaco
338400990322     C                   PARM                    DS_cnind
338500990322     C                   PARM                    DS_cnclp
338600990322     C                   PARM                    DS_fncls
338700990322    2c                   if        o69err=' '
338800990322     c                   movel     acorag        p3dksc
338900990322    2c                   endif
339000990322    1c                   endif
339100990322     c
339200990322    1c                   endif
339300990216     c                   ENDSR
339400990322      *------------------------------------------------------------------------*
339500990322      * CERCO CODICE E RAGIONE SOCIALE MITTENTE E DESTINATARIO
339600990322      *------------------------------------------------------------------------*
339700990322     c     MITDES        BEGSR
339800990322    0c                   if        wmitdes=' '
339900990322     c                   clear                   p3cccm
340000990322     c                   clear                   p3crsd
340100990322     c                   clear                   p3dccm
340200990322     c                   clear                   p3drsd
340300990322     c
340400990322     c                   eval      wmitdes='S'
340500990322     c* Mittente su blp/arb
340600010706    1c                   if        wbol <> *blanks
340700050711     c**                 select
340800050711    2c**                 when      wporto='F'
340900050711     c**                 movel     arbksc        p3cccm
341000050711     c**                 when      arbccm>0
341100050711     c**                 movel     arbccm        p3cccm
341200050711    2c**                 endsl
341300050711     c**                 movel     arbrsm        p3dccm
341400050711     c
341500050711     c                   movel     speccm        p3cccm
341600050712     c                   movel     spersm        p3dccm
341700990322     c* Destinatario
341800050711    2c**                 if        wporto='A' and wbol='A' OR
341900050711    2c**                           wporto='A' and wbol='D'
342000050711     c**                 move      arbksc        w0040
342100050711    3c**                 if        w0040<>9999
342200050711     c**                 movel     arbksc        p3crsd
342300050711    3c**                 endif
342400050711    2c**                 endif
342500050711     c**                 movel     arbrsd        p3drsd
342600050711     c
342700050711     c                   move      speccd        w004a
342800050711     c                   if        w004a>*zeros and w004a<>'9999'
342900050711     c                   movel     speccd        p3crsd
343000050712     c                   endif
343100050712     c                   movel     spersd        p3drsd
343200990322     c
343300990322    1c                   endif
343400990322    0c                   endif
343500990322     c
343600990322    1c                   ENDSR
343700990216      *------------------------------------------------------------------------*
343800990216      * STAMPA C.A. E REPONSABILITA' SENZA DETTAGLIO
343900990216      *------------------------------------------------------------------------*
344000990217     c     STANODETT     BEGSR
344100990216     c* riempo il campo lna con lna della c.a.
344200990216     c                   eval      dcdlna=dctlna
344300990216     c                   clear                   dcdfls
344400990216     c                   clear                   dcdnrs
344500990216     c                   clear                   dcdnsc
344600990216     c*
344700990216     c     kdct2         chain     fndcr02l                           31
344800990421     c                   dow       not *in31  AND
344900990421     c                             dcratb<>' '
345000990421     c     kdct2         reade     fndcr02l                               31
345100990421     c                   enddo
345200990217     c
345300990217     c* Se non esiste nemmeno un record stampo una riga con casuale NOCL
345400990217     c                   if        *in31
345500990217     c                   eval      dcrflo='       NOCL    '
345600990217     c                   clear                   dcrpor
345700990217     c                   clear                   dcrtcr
345800990217     c                   eval      dcrper=dctncn
345900990218     c                   eval      p3ccar=cosaut
346000990217     c
346100990217     c                   exsr      DECODDCR
346200990217     c                   EXSR      EXCPTOUT
346300990217     c
346400990217     c                   else
346500990216     c                   dow       not *in31
346600990216     c                   if        dcratb=' '
346700990216     c                   exsr      DECODDCR
346800990216     c* Stampo
346900990216     c                   EXSR      EXCPTOUT
347000990216     c
347100990216     c                   endif
347200990216     c
347300990216     c     kdct2         reade     fndcr02l                               31
347400990216     c                   enddo
347500990217     c                   endif
347600990216     c
347700990216     c                   ENDSR
347800990216      *------------------------------------------------------------------------*
347900990216      * DECODIFICA DCR
348000990216      *------------------------------------------------------------------------*
348100990216     c     DECODDCR      BEGSR
348200990217     c                   clear                   p3dtre1
348300990217     c                   clear                   p3dtcr
348400990217     c
348500990216     c                   z-add     dcrper        p3dper
348600990216     c                   movel     dcrflo        ddcr01
348700990217
348800990217     c* Tipo attribuzione reponsabilita'--> decodifico
348900990216     c                   movel     §dcrtre       p3ctre
349000990217     c                   if        p3ctre<>*blanks
349100990217     c                   clear                   tibs02ds
349200990217     C                   MOVEL     'C'           t02mod
349300990217     C                   MOVEL     knsif         t02sif
349400990217     C                   MOVEL     'TRE'         t02cod
349500990217     c                   movel     p3ctre        t02ke1
349600990217      *
349700990217     C                   CALL      'TIBS02R'
349800990217     C                   PARM                    KPJBA
349900990217     C                   PARM                    TIBS02DS
350000990217      *
350100990217     C                   IF        T02err = *BLANKS
350200990217     C                   movel     T02uni        DTRE
350300990217     c                   movel     §tredesc      p3dtre1
350400990217     c                   endif
350500990217     c                   endif
350600990217     c
350700990216     c     dcrpor        chain     azorg01l                           32
350800990216     c  N32              movel     orgdes        p3dpor
350900990216     c   32              clear                   p3dpor
351000990217     c
351100990217     c                   if        dcrtcr<>*blanks
351200990217     c                   clear                   tibs02ds
351300990217     C                   MOVEL     'C'           t02mod
351400990217     C                   MOVEL     knsif         t02sif
351500990217     C                   MOVEL     'TCR'         t02cod
351600990217     c                   movel     dcrtcr        t02ke1
351700990217      *
351800990217     C                   CALL      'TIBS02R'
351900990217     C                   PARM                    KPJBA
352000990217     C                   PARM                    TIBS02DS
352100990217      *
352200990217     C                   IF        T02err = *BLANKS
352300990217     C                   movel     T02uni        p3dtcr
352400990217     c                   endif
352500990217     c                   endif
352600170424     c
352700170424     c                   eval      p3drit=§dcrf1
352800990217     c
352900990216     c                   ENDSR
353000990216      *------------------------------------------------------------------------*
353100990216      * STAMPO TESTATA e dettaglio
353200990216      *------------------------------------------------------------------------*
353300990216     C     EXCPTOUT      BEGSR
353400050908     c* Faccio qui la stampa della testata/o salto
353500990310     c*  righe perche' altrimenti mi salta molte righe inutilmente
353600990310     c                   if        wtot=' ' AND
353700990310     c                             wnuovopo='S'
353800990310     c                   exsr      SALPAG2
353900990310     c                   clear                   wnuovopo
354000990310     c                   endif
354100990216     c* Stampo testata
354200990312     c                   EXSR      TESTATA
354300990216     c
354400990309     c                   if        wtot='G'
354500990218     c                   eval      p3dtota=cosGEN
354600990430     c* Solo se ho trovato almeno un p.o. estero, stampo nella riga
354700990430     c*  dei totali generali, "escluso distretto internazionale"
354800990430     c                   if        westero='S'
354900990430     c                   eval      p3dtopo=cosESCL
355000990430     c                   eval      p3dtode=cosINTER
355100990430     c                   eval      *in08='1'
355200990309     c                   endif
355300990430     c                   endif
355400990430     c
355500990309     c                   if        wtot='P'
355600990309     c                   eval      p3dtota=cosPAR
355700990309     c                   endif
355800990310     c                   if        wtot='A'
355900050613     c                   if        simfel=0  or i43ter=0
356000990310     c                   eval      p3dtota=cosAREA
356100990315     c                   else
356200070111     c* Visto che le area possono contenere una parte di terminal
356300070111     c*  non usiamo più la costante TOTALE TERMINAL ma sempre
356400070111     c*  TOTALE AREA
356500070111     c                   eval      p3dtota=cosarea
356600990310     c                   endif
356700990315     c                   endif
356800990310     c                   if        wtot='D'
356900990310     c                   eval      p3dtota=cosDISTR
357000990310     c                   endif
357100990309     c
357200040202if  1c                   if        wtot<>' '
357300990309     c
357400990428     c* USO INTERNO
357500040202if  2c                   if        i43usi='S'
357600050708     c                   if        wtot<>'D' and wtot<>'A' AND
357700050708     c                             i43toi<>'S' AND not *in09
357800990218     c                   write     dn44t3
357900990310     c                   endif
358000000215     c
358100000215     c* se stampo dati di liquidazione, stampo la riga perche' non ho
358200000215     c*  i totali di p.o.
358300050708     c                   if        wtot<>'D' and i43toi<>'S' and *in09
358400000215     c                   write     dn44t3
358500000215     c                   endif
358600000215     c
358700990218     c                   write     dn44z1
358800040202e   2c                   endif
358900990504     c                   eval      *in08='0'
359000990503     c
359100990503     c* Se sto stampando per distretto o area,devo stampare i totali filtr
359200990504     c                   EXSR      FILTRIAREA
359300990504     c                   EXSR      FILTRIDISTR
359400990504     c                   EXSR      FILTRIGEN
359500990503     c
359600990504     c* USO INTERNO
359700990504     c                   if        i43usi='S'
359800990218     c                   write     dn44t3
359900990428     c                   endif
360000990218     c
360100040202x   1c                   else
360200990309     c
360300990428     c* USO INTERNO
360400990428     c                   eval      wstaspe='S'
360500050708if  2c                   if        i43usi='S' and i43toi<>'S'
360600990428     c                   if        wrecsta = ' '
360700990218     c                   write     dn44d03
360800990216     c                   movel     '1'           WRECSTA
360900990216     c                   else
361000990218     c                   write     dn44d04
361100990216     c                   endif
361200040202e   2c                   endif
361300990218     c* Addiziono i colli
361400000215     c                   if        not *in09
361500990503     c                   eval      wtiptot='C'
361600990503     c                   exsr      CALTOT
361700990309     c                   endif
361800040202e   1c                   endif
361900990309     c
362000990216     c                   ENDSR
362100990504      *------------------------------------------------------------------------*
362200990504      *  FILTRI dei totali di area
362300990504      *------------------------------------------------------------------------*
362400990504     C     FILTRIAREA    BEGSR
362500990504     c                   if        wtot='A'
362600990504     c                   movea     atas          wtas
362700990504     c                   movea     atac          wtac
362800990504     c                   movea     atfs          wtfs
362900990504     c                   movea     atfc          wtfc
363000990504     c                   EXSR      STAFILTRI
363100990504     c                   endif
363200990504     c                   ENDSR
363300990504      *------------------------------------------------------------------------*
363400990504      *  FILTRI dei totali generali
363500990504      *------------------------------------------------------------------------*
363600990504     C     FILTRIGEN     BEGSR
363700990504     c                   if        wtot='G'
363800990504     c                   movea     gtas          wtas
363900990504     c                   movea     gtac          wtac
364000990504     c                   movea     gtfs          wtfs
364100990504     c                   movea     gtfc          wtfc
364200990504     c                   EXSR      STAFILTRI
364300990504     c                   endif
364400990504     c                   ENDSR
364500990504      *------------------------------------------------------------------------*
364600990504      *  FILTRI dei totali di distretto
364700990504      *------------------------------------------------------------------------*
364800990504     C     FILTRIDISTR   BEGSR
364900990504     c                   if        wtot='D'
365000990504     c                   movea     dtas          wtas
365100990504     c                   movea     dtac          wtac
365200990504     c                   movea     dtfs          wtfs
365300990504     c                   movea     dtfc          wtfc
365400990504     c                   EXSR      STAFILTRI
365500990504     c                   endif
365600990504     c                   ENDSR
365700990503      *------------------------------------------------------------------------*
365800990503      * STAMPO i TOTALI FILTRI
365900990503      *------------------------------------------------------------------------*
366000990503     c     STAFILTRI     BEGSR
366100990503     c* Prima i filtri
366200990513     c* solo per ordinamento 2
366300990513     c                   if        i43ord='2'
366400990513     c                   EXSR      LASCIO1RG
366500990513     c
366600990503     c                   eval      p4des1=cosFILTRO
366700990503     c                   eval      zz=1
366800990503     c
366900990503    1c                   dow       wcfl(zz)<>'  '
367000990510    2c                   if        wtfs(zz)>0
367100990503     c                   movea     wcfl(zz)      w38(1)
367200990503     c                   movea     '-'           w38(3)
367300990503     c
367400990503     C                   clear                   tibs02ds
367500990503     C                   MOVEL     'C'           t02mod
367600990503     C                   MOVEL     knsif         t02sif
367700990503     C                   MOVEL     'RRD'         t02cod
367800990503     C                   MOVEL     'A1'          t02ke1
367900000323     C                   MOVEL     wcfl(zz)      w002a             2
368000000323     C                   MOVEL     w002a         t02ke2
368100990503     C                   CALL      'TIBS02R'
368200990503     C                   PARM                    KPJBA
368300990503     C                   PARM                    TIBS02DS
368400990503      *
368500990510    3C                   IF        t02err = *BLANKS
368600990503     c                   movel     t02uni        w035a            35
368700990503     c                   else
368800990503     c                   eval      w035a=*all'?'
368900990510    3c                   endif
369000990503     c
369100990503     c                   movea     w035a         w38(4)
369200990503     c                   movea     w38           p4des2
369300000328     c
369400000328     C                   MOVE      wcfl(zz)      w001a             1
369500000328     c                   clear                   p4des3
369600000328     c                   if        w001a='A'
369700000328     c                   movel     'Arrivo'      p4des3
369800000328     c                   endif
369900000328     c                   if        w001a='P'
370000000328     c                   movel     'Partenza'    p4des3
370100000328     c                   endif
370200000328     c                   if        w001a='T'
370300000328     c                   movel     'Transito'    p4des3
370400000328     c                   endif
370500000328     c
370600990503     c                   eval      p4ctps=wtfs(zz)
370700990503     c                   eval      p4ctpc=wtfc(zz)
370800990503     c
370900990504     C* Stampo
371000990511     c                   if        wtot<>'N'
371100990503     c                   exsr      TESTATA
371200990511     c                   endif
371300990511     c
371400990504     c                   exsr      STAMPOFIL
371500990504     c                   clear                   p4des1
371600990510    "c                   ENDIF
371700990504     c
371800990503     c                   add       1             zz
371900990510    1c                   enddo
372000990513     c                   endif
372100990503     c
372200990503     c* Poi i tipi anomalia
372300990513     c                   EXSR      LASCIO1RG
372400990513     c
372500990503     c                   eval      p4des1=cosTIPANOM
372600990503     c                   eval      zz=1
372700990503     c
372800990513    1c                   dow       wtdd(zz)<>'  '
372900990513     c                   if        wtas(zz)>0
373000990513     c                   movea     wtdd(zz)      w38(1)
373100990503     c                   movea     '-'           w38(3)
373200990503     c
373300990503     c                   eval      wtad=wtdd(zz)
373400990503     c                   exsr      DECTAD
373500990503     c                   movel(p)  §taddesc      w035a
373600990503     c
373700990503     c                   movea     w035a         w38(4)
373800990503     c                   movea     w38           p4des2
373900990503     c                   eval      p4ctps=wtas(zz)
374000990503     c                   eval      p4ctpc=wtac(zz)
374100990503     c
374200990504     c* Stampo
374300990503     c                   exsr      TESTATA
374400990504     c                   exsr      STAMPOFIL
374500990504     c                   clear                   p4des1
374600990513     c                   endif
374700990503     c
374800990503     c                   add       1             zz
374900990503    1c                   enddo
375000990503     c
375100990503     c                   ENDSR
375200990504      *------------------------------------------------------------------------*
375300990504      * LASCIO 1 RIGA
375400990504      *------------------------------------------------------------------------*
375500990504     c     LASCIO1RG     BEGSR
375600990504     c* USO INTERNO
375700990504     c                   if        i43usi='S'
375800990504     c                   write     dn44z5
375900990504     c                   endif
376000990504     c                   ENDSR
376100990504      *------------------------------------------------------------------------*
376200990504      * STAMPO 1 RIGA DI TOTALE FILTRO
376300990504      *------------------------------------------------------------------------*
376400990504     c     STAMPOFIL     BEGSR
376500990504     c* USO INTERNO
376600990504     c                   if        i43usi='S'
376700990504     c                   write     dn44z4
376800990504     c                   endif
376900990511     c
377000990504     c                   ENDSR
377100990312      *------------------------------------------------------------------------*
377200990312      * STAMPO TESTATA
377300990312      *------------------------------------------------------------------------*
377400990312     c     TESTATA       BEGSR
377500990428     c* stampo solo se sto stampando i totali o se devo anche
377600990428     c*  stampare il dettaglio
377700990428     c* USO INTERNO
377800990428    1C                   if        i43usi='S'
377900050708    2C                   if        wtot<>' ' OR i43toi<>'S'
378000990428    3C                   if        CURLINE>60   OR *IN02
378100990312     c                   write     dn44t1
378200990312     c                   write     dn44ts
378300050708     c                   if        i43toi<>'S'
378400990312     c                   write     dn44t2
378500990428     c                   endif
378600990312     c                   write     dn44t3
378700050613     c                   if        *in01 or i43dst<>*blanks or i43are<>*blanks
378800050613     c                   write     dn44t4
378900050613     c                   write     dn44t3
379000050908     c
379100050908     c                   if        wtot=' '
379200050908     c                   clear                   wnuovo
379300050613     c                   endif
379400050908     c                   endif
379500050908     c
379600990312     c                   clear                   wrecsta
379700990312     c                   eval      *in02='0'
379800990428    3c                   endif
379900990428    2c                   endif
380000990428    1c                   endif
380100990312     c
380200990312     c                   ENDSR
380300990406      *------------------------------------------------------------------------*
380400990406      * SCRIVO DATI PER REPORT CONSUNTIVO
380500990406      *------------------------------------------------------------------------*
380600990406     c     SCRIREP       BEGSR
380700990406     c                   clear                   ktae
380800990406     c                   clear                   kfpr
380900990408     c                   clear                   kcrs
381000990406     c                   clear                   kcch
381100990917     c                   clear                   kcrc
381200090319     c* Terminal dal dcrflo
381300090319     c                   eval      ddcr01=dcrflo
381400090319     c                   if        dcrtcr='P'
381500090319     c                   eval      kttf='P'
381600090319     c                   else
381700090319     c                   eval      kttf='A'
381800090319     c                   endif
381900110801     c
382000110801     c* Verifico se scrivere anche file clienti particolari
382100110801     c                   exsr      cerbol
382200110801     c* Verifico se cod cliente mittente è tra quelli particolari
382300121126     c*  e solo AVARIE e solo per il terminal previsto
382400121126     c*  o Destinatario o Beneficiario
382500110801     c                   clear                   Indx
382600130909     c                   clear                   kter_p
382700121126    1c                   if        speccm>0
382800121126     c                   eval      Indx=%lookup(speccm:ksc_ex)
382900121126    1c                   endif
383000121126    1c                   if        Indx=0 and speccd>0
383100121126     c                   eval      Indx=%lookup(speccd:ksc_ex)
383200121126    1c                   endif
383300121126    1c                   if        Indx=0
383400121126     c                   exsr      BENEFIC
383500121126    2c                   if        p3cksc>*zeros
383600121126     c                   eval      Wben_n=%int(p3cksc)
383700121126     c                   eval      Indx=%lookup(wben_n:ksc_ex)
383800121126    2c                   endif
383900121126    1c                   endif
384000121126     c
384100121126    1c                   if        Indx>0
384200121126     c                   eval      wtad=dcttad
384300121126     c                   exsr      DECTAD
384400121126    2c                   if        §tadragr<>'V'
384500121126     c                   clear                   Indx
384600121126    2c                   endif
384700121126    1c                   endif
384800121126     c
384900121126     c* Se si tratta di avarie controllo il terminal
385000121126    1c                   if        Indx>0
385100121126     c                   select
385200121126     c*  cliente momentaneamente bloccato
385300130909    2c                   when      ksc_are(indx)=888
385400121126     c                   clear                   Indx
385500121126     c* abilitato a tutto:  memorizzo per qualsiasi terminal
385600130909    2c                   when      ksc_are(indx)=999
385700130909     c                   eval      kter_p= ksc_tr(indx)
385800121126   x2c                   other
385900121126     c
386000130909     c* Abilito solo i terminal della C.A. se previsto dall'area del term di riferimento della tab
386100130909     c     kter          chain     azorg01l
386200130909    3c                   if        %found(azorg01l)
386300121126     c
386400130909    4c                   if        orgcar<> ksc_are(indx)
386500121126     c                   clear                   indx
386600130909   x4c                   else
386700130909     c                   eval      kter_p=ksc_tr(indx)
386800130909    4c                   endif
386900130909
387000130909   x3c                   else
387100130909     c                   clear                   indx
387200130909    3c                   endif
387300121126    c
387400121126    2c                   endsl
387500121126    1c                   endif
387600121126     c*
387700090319     c
387800990406     c* decodifico tipo anomalia e causale chiusura
387900990408     c                   eval      wtad=dcttad
388000990406     c                   exsr      DECTAD
388100990408     c
388200990406     c                   if        dctcch<>*blanks
388300990406     c                   eval      wcch=dctcch
388400990406     c                   exsr      DECCCH
388500990406     c* se da non memorizzare sul reporto la causale chiusura, come se fosse
388600990406     c*  aperta
388700990406     c                   if        §cchmerp='S'
388800990406     c                   eval      kcch=dctcch
388900990406     c                   endif
389000990406     c                   endif
389100990406     c
389200990406     c* Prendo mese di elaborazione
389300990406     c                   movel     §tadragr      wragr
389400990406     c
389500990406     c* se manca Tipo gestione, la devo desumere   T / P
389600990406     c                   if        dctfpr<>*blanks
389700990406     c                   movel     dctfpr        kfpr
389800990406     c                   else
389900990407     c                   EXSR      DESUMI
390000990406     c                   endif
390100990406     c
390200990406     c* Se c'e' evento devo prendere il suo tipo anomalia
390300990406     c                   if        dctnev>0
390400990406     c     kdet          chain     fndet01l                           32
390500990406     c                   if        not *in32
390600990406     c                   eval      ktae=dettad
390700990406     c                   endif
390800990406     c                   endif
390900990408     c
391000990408     c                   if        dcttcr='S'  OR
391100990408     c                             dctcch<>*blanks and §cchnre='S'
391200990408     c                   eval      kcrs='N'
391300990408     c                   else
391400990408     c                   eval      kcrs='S'
391500990408     c                   endif
391600990917     c* riserva
391700990917     c                   eval      kcrc=dctcrc
391800990408     c
391900090319     c     krdn          chain     wfrdn51l                           32
392000110801     c
392100990406     c                   if        *in32
392200990406     c                   clear                   wfrdn000
392300121126     c                   eval      tipocli=' '
392400110801     c                   exsr      RiempiRDN
392500110801     c                   endif
392600110801     c
392700990406     c                   add       dcrper        rdnncn
392800990406     c   32              write     wfrdn000
392900990406     c  N32              update    wfrdn000
393000110801     c
393100110801     c* Scrivo file clienti particolari
393200110801     c                   if        Indx>0
393300110912     c     krdnAV        chain     wfrdn51l                           32
393400110801     c
393500110801     c                   if        *in32
393600110912     c                   clear                   wfrdn000
393700121126     c
393800121127     c                   eval      tipocli='P'
393900110801     c                   exsr      RiempiRDN
394000121126     c                   eval      tipocli=' '
394100121126     c
394200110912     c* Cambia solo il tipo elaborazione da G/R a A/B
394300110912     c                   eval      rdntie=ktieAV
394400110801     c                   endif
394500110801     c
394600110801     c                   add       dcrper        rdnncn
394700110912     c   32              write     wfrdn000
394800110912     c  N32              update    wfrdn000
394900110801     c                   endif
395000990406     c
395100990406     c                   ENDSR
395200110801      *------------------------------------------------------------------------*
395300110801      * Riempimento campi chiave file WFRDN
395400110801      *------------------------------------------------------------------------*
395500110801     c     RiempiRDN     BEGSR
395600110801     c                   eval      rdndal=wdal
395700110801     c                   eval      rdnall=wal
395800110801     c                   eval      rdndiv=p3cdst
395900110801     C                   movel     '17'          kCOD
396000110801     C                   movel(P)  rdndiv        kKEY
396100110801     C     ktab          CHAIN     TABEL                              31
396200110801     C  n31              movel     TBLUNI        rdnddiv
396300110801     c
396400110801     c                   eval      rdnare=p3care
396500110801     C                   movel     '05'          kCOD
396600110801     C                   movel(P)  rdnare        kKEY
396700110801     C     ktab          CHAIN     TABEL                              31
396800110801     C  n31              movel     TBLUNI        rdndare
396900110801     c
397000110801     c                   eval      rdnttf=kttf
397100121126     c
397200121126     c* Per i clienti particolari non imposto la filiale
397300121126     c                   if        tipocli=' '
397400110801     c                   eval      rdnfil=dcrpor
397500110801     c     rdnfil        chain     azorg01l
397600110801     c                   if        %found(azorg01l)
397700110801     c                   eval      rdndfil=orgdes
397800110801     c                   endif
397900121126     c                   else
398000121126     c                   clear                   rdnfil
398100121126     c                   clear                   rdndfil
398200121126     c                   endif
398300110801     c
398400130909     c* per i clienti particolari uso kter_p
398500130909     c                   if        tipocli=' '
398600110801     c                   eval      rdnter=kter
398700130909     c                   else
398800130909     c                   eval      rdnter=kter_p
398900130909     c                   endif
399000110801     c     rdnter        chain     azorg01l
399100110801     c                   if        %found(azorg01l)
399200110801     c                   eval      rdndter=orgdes
399300110801     c                   endif
399400110801     c
399500110801     c                   eval      rdntae=ktae
399600110801     c                   eval      rdnrga=wragr
399700110801     c                   eval      rdntad=dcttad
399800110801     c                   eval      rdncch=kcch
399900110801     c                   eval      rdnfpr=kfpr
400000110801     c                   eval      rdncrs=kcrs
400100110801     c                   eval      rdncrc=kcrc
400200110801     c                   eval      rdntie=ktie
400300110801     c                   ENDSR
400400990407      *------------------------------------------------------------------------*
400500990407      * Desumo tipo gestione se manca
400600990407      *------------------------------------------------------------------------*
400700990407     C     DESUMI        BEGSR
400800990407     c* Se c'e' numero evento --> "P"
400900990407    1c                   if        dctnev>0
401000990408     c                   eval      kfpr='P'
401100990407    1c                   endif
401200990407     c
401300990407     c* Se c'e' imp. da assicurare -->  "P"
401400990408    1c                   if        kfpr=' '
401500990408     c                   EXSR      CERBOL
401600990408    2c                   if        arbias>0
401700990408     c                   eval      kfpr='P'
401800990407    2c                   endif
401900990407    1c                   endif
402000990407     c
402100990407     c                   movel     dctflo        ddct01
402200990407     c
402300990407     c* Se e' franco import -->  "P"
402400990408    1c                   if        kfpr=' '  AND
402500990407     c                             §dcttisp='I' and §dctport='F'
402600990408     c                   eval      kfpr='P'
402700990407    1c                   endif
402800020422     c
402900020513     c* Se e' franco export DPD con responsabilità partner -->  "P"
403000020422    1c                   if        kfpr     = ' '  and
403100020513     c                             §dcteuro = 'D'  and
403200020513     c                             §dcttisp = 'E'  and
403300020422     c                             §dctport = 'F'  and
403400020422     c                             DCTres   = 'P'
403500020422     c                   eval      kfpr='P'
403600020422    1c                   endif
403700990407     c
403800990407     c* Se c'e' Mandato -->  "P"
403900990408     c                   if        kfpr=' '
404000990407     c* se si tratta di cambio di porto il mandato non c'e'
404100990407     c                   EXSR      CERBOL
404200110503     c**                 if        arbcca<>'9'
404300110503     c                   if        §DCTcca<>'9'
404400990407     c                   move      arbksc        w0040
404500990407     c                   if        w0040<>8888 and w0040<>9999
404600990407     c* Richiamo pgm che mi dice se c'e' mandato
404700990407     c                   clear                   fidn48ds
404800990407     c                   eval      i48ksc=arbksc
404900990407     c                   eval      i48ctr=arbctr
405000990407     c                   eval      i48tsp=arbtsp
405100050711     c                   eval      i48dsp=spedata
405200990408     c                   eval      i48por=§dctport
405300990407     c                   if        §dcttisp=' '
405400000126     c* Carico tariffe italia + dpd
405500000126     c                   eval      i48fie='J'
405600990407     c                   else
405700000202     c                   eval      i48fie='S'
405800990407     c                   endif
405900990408     c                   eval      i48mns='S'
406000990407     c*
406100990407     c                   call      'FIDN48R'
406200990407     c                   parm                    kpjba
406300990407     c                   parm                    fidn48ds
406400990408     c
406500010417     c                   if        not *in32 and *in32
406600010417     c                   except    DETT
406700010417     c                   endif
406800990408     c
406900990407     c                   if        o48mand='S'
407000990408     c                   eval      kfpr='P'
407100990407     c                   endif
407200990407     c
407300990407     c                   endif
407400990407     c                   endif
407500990407     c                   endif
407600990407     c
407700990408     c                   if        kfpr=' '  AND
407800990407     c                             dctipv>0 and dctipv<§STDllt
407900990408     c                   eval      kfpr='T'
408000990407     c                   endif
408100990407     c
408200990408     c                   if        kfpr=' '
408300990407      * Determino se limite risarcibile da utilizzare è "CMR" o "Legge 450"
408400990407     c                   EXSR      IMPlimris
408500990407     c                   if        (dctpkd * wlimris) > §STDllt
408600990408     c                   eval      kfpr='P'
408700990407     c                   else
408800990408     c                   eval      kfpr='T'
408900990407     c                   endif
409000990407     c                   endif
409100990407     c
409200990407     c                   ENDSR
409300990407      *****************************************************************
409400990407      *  DETERMINO LIMITE RISARCIBILE DA UTILIZZARE
409500990407      *****************************************************************
409600990407     C     IMPLIMRIS     BEGSR
409700990407      *
409800010413      * se spedizione non nazionale o EXPORT DPD
409900010413      *
410000020422     c                   IF            (§DCTeuro =  'X'
410100020422     c                              or  §DCTeuro =  'P')
410200020422     c                             and  §DCTtisp <> *blanks
410300020422     c                             or   §DCTeuro =  'D'
410400020422     c                             and  §DCTtisp =  'E'
410500010413      * controvalore CMR
410600010413     c                   z-add     §geccm        Wlimris
410700990407      *
410800990407      *    LEGGE 450
410900990407     c                   ELSE
411000010413     c                   z-add     §GElrp        wlimris
411100990407     c                   ENDIF
411200990407      *
411300990407     C                   ENDSR
411400990212      *------------------------------------------------------------------------*
411500990212      * ROUTINE INIZIALE
411600990212      *------------------------------------------------------------------------*
411700990212     C     *INZSR        BEGSR
411800981204      *
411900981204     C     *ENTRY        PLIST
412000981204     C                   PARM                    KPJBA
412100990212     c                   movel     kpjbu         fidn43ds
412200050610      *
412300050610     c     *dtaara       define    §azute        azuteds
412400050610     c     *dtaara       define    §datiute      ddatiute
412500050610     c                   in(E)     *dtaara
412600050610    1c                   If        %error  or RSUT = *blanks
412700050610     c                   Clear                   Tibs34ds
412800050610     c                   Call      'TIBS34R'
412900050610     c                   Parm                    Tibs34ds
413000050610     c                   In        *dtaara
413100050610    1c                   EndIf
413200050610
413300050610     c                   Clear                   wabi
413400050610     c                   Clear                   dLat
413500050610
413600050610      * Verifica errori e autorità profilo
413700050610s   1c                   Select
413800050610      * se ho errori nei dati utente esco dal pgm
413900050610w   1c                   When      DutErr = 'E'
414000050610     c                   GoTo      Fine
414100050610      * se non c'è l'abilitazione
414200050610      * --> se 1° livello, abilitazioni al terminal
414300050610      *     se 2° livello, abilitazioni al punto operativo
414400050610      *     se sede è impossibile ma se capita mando a fine il pgm
414500050610w   1c                   When      UteAut = *Blanks
414600050610if  2c                   If        DutLpo = '1'
414700050610     c                   Eval      wabi   = 'TP'
414800050610e   2c                   EndIf
414900050610if  2c                   If        DutLpo = '2'
415000050610     c                   Eval      wabi   = 'PO'
415100050610e   2c                   EndIf
415200050610if  2c                   If        DutLpo = 'S'
415300050610     c                   GoTo      Fine
415400050610e   2c                   EndIf
415500050610      * carica le abilitazioni del profilo
415600050610x   1c                   Other
415700050610     c                   Movel     UteFaf        Dute01
415800050610     c                   If        §UteEca <> *Blanks
415900050610     c                   Eval      wabi = §UteEca
416000050610     c                   Else
416100050610     c                   Eval      wabi = UteAut
416200050610     c                   EndIf
416300050610e   1c                   EndSl
416400050610
416500050610      * controllo se ok l'abilitazione dell'utente
416600050610     c                   Clear                   Tibs02ds
416700050610     c                   Eval      T02Mod = 'C'
416800050610     c                   Eval      T02Sif = knsif
416900050610     c                   Eval      T02Cod = 'LAT'
417000050610     c                   Movel(p)  wabi          T02Ke1
417100050610     c                   Call      'TIBS02R'
417200050610     c                   Parm                    kpjba
417300050610     c                   Parm                    Tibs02ds
417400050610if  1c                   If        T02Err = *Blanks
417500050610     c                   Eval      dLat = T02Uni
417600050610e   1c                   EndIf
417700050610      * errore o non abilitato (impossibile xchè controllato nel filtro)
417800050610if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
417900050610     c                   goto      fine
418000050610   x1c                   Else
418100050610      * Reperimento dei P.O. gestibili dall'utente
418200050610     c                   clear                   TRUL31DS
418300050610     c                   clear                   TRUL31DS2
418400050610     c                   eval      I31abi = wabi
418500050610     c                   eval      I31cdi = DUTdis
418600050610     c                   eval      I31car = DUTare
418700050610     c                   eval      I31cpo = DUTpou
418800050610     c                   call      'TRUL31R'
418900050610     c                   parm                    KPJBA
419000050610     c                   parm                    TRUL31DS
419100050610     c                   parm                    TRUL31DS2
419200050610if  2c                   if        O31pog > *zeros
419300050610     c                   movea     O31pog        $pog
419400050610     c                   movea     O31arg        $arg
419500050610     c                   movea     O31dig        $dig
419600050610x   2c                   else
419700050610     c* impossibile perchè controllato nel filtrp
419800050610     c                   goto      fine
419900050610e   2c                   endif
420000050610    1c                   endif
420100050610     c
420200981204      *
420300050610     C                   Z-ADD     1             CODUT
420400981204
420500981204      * reperisco data e ora
420600981204     C                   TIME                    W0140
420700981204      * UDATE IN GGMMAAAA
420800981204     C                   MOVE      W0140         WDTGIO
420900990217     C                   MOVE      W0140         WDATE
421000981209     C                   MOVEL     W0140         WORA
421100981204      * UDATE IN AAAAMMGG
421200981204     C                   Z-ADD     WDTGIO        G02DAT
421300981204     C                   MOVEL     *BLANK        G02ERR
421400981204     C                   CALL      'XSRDA8'
421500981204     C                   PARM                    WLBDAT
421600981204     C                   MOVEL     G02INV        DATEU
421700120713     c* udate e 1 mese
421800120713     c     *iso          movel     dateu         dataiso
421900120713     c                   subdur    1:*m          dataiso
422000120713     c                   movel     dataiso       meno1mes          8 0
422100981204      *
422200990215     c* Apertura file diversa tra sede e filiale
422300050613     c**                 clear                   L1user
422400050613     c**                 clear                   L6user
422500990215     c     simfel        ifgt      0
422600990215     c                   open      fnarb01l
422700990215     c                   open      fnblp01l
422800990215     c                   open      fnblt01l
422900990215     c                   open      fnart01l
423000991116     c                   open      fiar601l
423100060124     c                   open      fiar501l
423200990216     c     *loval        setll     fnart01l
423300990216     c     *loval        setll     fnblt01l
423400020821     c
423500990215     c                   else
423600990809     c                   open      TITAS30c
423700000214     c                   open      TITA430c
423800990218     c                   open      tntmd01l
423900090319     c                   open      wfrdn51l
424000060124     c                   open      FIAR531C
424100050906     c                   if           I43cfd = 'S'
424200050906     c                             or I43cfd = 'P'
424300040206     c                   open      TITAA30C
424400050906if  5c                   if        I43cfd = 'P'
424500040212     c                   exsr      Dlt_WFDDN
424600050906e   5c                   endif
424700120713
424800120713     c* pulisco sempre e comunque per vecchiaia
424900120713     C/EXEC SQL
425000120713     c+   delete from  wfddn00f where ddnedt< :meno1mes
425100120713     C/end-EXEC
425200120713     c
425300040206     c                   open      WFDDN00F
425400040206     c                   endif
425500990809     c     *loval        setll     TITAS30c
425600990216     c                   seton                                        01
425700990215     c                   endif
425800990407     c
425900010413      ***
426000010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
426100010413      ***
426200010413      *
426300010413      * recupero la moneta corrente
426400010413     C                   CLEAR                   tibs02ds
426500010413      *
426600010413     C                   MOVEL     'L'           T02TLA
426700010413     C                   MOVEL     'C'           T02MOD
426800010413     C                   MOVEL     KNSIF         T02SIF
426900010413     C                   MOVEL     'GED'         T02COD
427000010907     C                   MOVEL     'DANNI'       T02KE1
427100010413      *
427200010413     C                   CALL      'TIBS02R'
427300010413     C                   PARM                    KPJBA
427400010413     C                   PARM                    tibs02ds
427500010413      *
427600010910     c                   movel     T02UNI        DGEDDN
427700010413      * recupero i valori nella moneta corrente
427800010413     C                   CLEAR                   tibs02ds
427900010413      *
428000010413     C                   MOVEL     'L'           T02TLA
428100010413     C                   MOVEL     'C'           T02MOD
428200010413     C                   MOVEL     KNSIF         T02SIF
428300010413     C                   MOVEL     'GEI'         T02COD
428400010910     C                   MOVEL     §GEDDBA       T02KE1
428500010413      *
428600010413     C                   CALL      'TIBS02R'
428700010413     C                   PARM                    KPJBA
428800010413     C                   PARM                    tibs02ds
428900010413      *
429000010413     c                   movel     t02uni        DGEI
429100990408      ***
429200990408      * leggo tabella standards danni
429300990408      ***
429400990408     c                   clear                   TIBS02DS
429500990408     C                   MOVEL     'C'           t02mod
429600990408     C                   MOVEL     knsif         t02sif
429700990408     C                   MOVEL     'STD'         t02cod
429800990408     C                   MOVEL(P)  1             t02ke1
429900010910     C                   MOVEL(P)  §gedDBA       t02ke2
430000990408      *
430100990408     C                   CALL      'TIBS02R'
430200990408     C                   PARM                    KPJBA
430300990408     C                   PARM                    TIBS02DS
430400990408     c
430500990408     C                   if        t02err = *BLANKS
430600990408     C                   MOVEL     T02UNI        DSTD
430700990408     c                   else
430800990408     c                   clear                   dstd
430900990408     C                   endif
431000110801     c*
431100110801     c* Carico tabella clienti particolari
431200121127     c*  solo se scrivo il report
431300130909    1c                   if        i43crp='S'
431400121127     c
431500121127     C                   Z-ADD     0             K                 4 0
431600110801     C                   MOVEL     'CXQ'         TBecod
431700110801     C     tbecod        SETLL     tntbe01l
431800110801     C                   DO        *HIVAL
431900110801     C     tbecod        READE     tntbe01l
432000110801     c                   if        %eof(tntbe01l)
432100110801     c                   leave
432200110801     c                   endif
432300110801
432400110801     C     tbeatb        IFeq      ' '
432500110801     C                   ADD       1             K
432600110801     C                   MOVEL     TBeKE2        ksc_ex(k)
432700121126     c                   movel     tbeuni        dcxq
432800130909
432900130909     c* Terminal di riferimento in cui memorizzare i dati
433000130909    3c                   if        §cxqterm>*zeros
433100130909     c                   movel     §cxqterm      ksc_tr(k)
433200130909     c* Area
433300130909    4c                   if        §cxqarea='T'
433400130909     c                   movel     999           ksc_are(k)
433500130909   x4c                   else
433600130909     c
433700130909     c     ksc_tr(k)     chain     azorg01l
433800130909    5c                   if        %found(azorg01l)
433900130909     c                   movel     orgcar        ksc_are(k)
434000130909   x5c                   else
434100130909     c                   movel     888           ksc_tr(K)
434200130909     c                   movel     888           ksc_are(k)
434300130909    5c                   endif
434400130909    4c                   endif
434500130909    c
434600130909   x3c                   else
434700130909     c* non riesco a memorizzare --> importo 888
434800130909     c                   movel     888           ksc_tr(k)
434900130909     c                   movel     888           ksc_are(k)
435000130909    3c                   endif
435100130909     c
435200110801     C                   ENDIF
435300110801     C                   ENDDO
435400121127     c
435500121127     c* Controllo riempimento schiera
435600121127     c                   movel     kpjbu         savkpjbu
435700121127     c                   clear                   trul0sds
435800121127     c                   eval      i0stla='L'
435900121127     c                   eval      i0sski='KSC_EX'
436000121127     c                   eval      i0sele=%elem(ksc_ex)
436100121127     c                   eval      i0spie=K
436200121127     c                   eval      i0sfile='Cienti tab CXQ'
436300121127     c                   eval      i0ssif=knsif
436400121127     c                   eval      i0spgm='FIDN47R'
436500121127     c                   movel     trul0sds      kpjbu
436600121127     c                   call      'TRUL0SR'
436700121127     c                   parm                    kpjba
436800121127
436900121127     c                   movel     savkpjbu      kpjbu
437000121127     c
437100121127     C                   ENDIF
437200981204
437300981204     C*----------------------------------------------------*
437400981204     C* KEY   KEY    KEY   KEY   KEY   KEY   KEY   KEY  KEY
437500981204     C*----------------------------------------------------*
437600981204      *
437700981204     c* ACCESSO tntbe
437800990406     C     ktbe          KLIST                                                  *
437900990217     C                   KFLD                    kcodt                          *
438000981204     C                   KFLD                    kke1                           *
438100981204     C                   KFLD                    kke2                           *
438200981204     C                   KFLD                    klin                           *
438300981204     C                   KFLD                    ksif                           *
438400990512     C     ktbe2         KLIST                                                  *
438500990512     C                   KFLD                    kcodt                          *
438600990512     C                   KFLD                    klin                           *
438700990512     C                   KFLD                    ksif                           *
438800090320     C     kcae          KLIST                                                  *
438900090320     C                   KFLD                    kepa                           *
439000090323     C                   KFLD                    ktfpa                          *
439100981204      *
439200981204     c* ACCESSO fndct
439300990215     C     kdct1         KLIST                                                  *
439400990219     C                   KFLD                    kfil                           *
439500990215     C                   KFLD                    i43aac                         *
439600981204      *
439700981204     c* ACCESSO fndcd
439800990215     C     kdct2         KLIST                                                  *
439900990215     C                   KFLD                    dctaac                         *
440000981210     C                   KFLD                    dctfil                         *
440100981210     C                   KFLD                    dctnca                         *
440200990216     C     kdcdr         KLIST                                                  *
440300990216     C                   KFLD                    dctaac                         *
440400990216     C                   KFLD                    dctfil                         *
440500990216     C                   KFLD                    dctnca                         *
440600990216     C                   KFLD                    KSEG                           *
440700990215     c* ACCESSO fndcd
440800990216     C     kdctk         KLIST                                                  *
440900990215     C                   KFLD                    dctaac                         *
441000990215     C                   KFLD                    dctfil                         *
441100990215     C                   KFLD                    dctnca                         *
441200990215     C                   KFLD                    KTRC                           *
441300981204      *
441400991116     c* ACCESSO FNARB/FNBLP
441500990216     C     kbolla        KLIST                                                  *
441600981209     C                   KFLD                    dctaas                         *
441700981209     C                   KFLD                    dctlnp                         *
441800981209     C                   KFLD                    dctnrs                         *
441900981204     C                   KFLD                    dctnsp                         *
442000050711     C     kbolladck     KLIST                                                  *
442100050711     C                   KFLD                    dckaas                         *
442200050711     C                   KFLD                    dcklnp                         *
442300050711     C                   KFLD                    dcknrs                         *
442400050711     C                   KFLD                    dcknsp                         *
442500040726      * Accesso FNDCS01L
442600040726     c     k07dcs01      klist
442700040726     c                   kfld                    DCStpd
442800040726     c                   kfld                    DCSnkt
442900040726     c                   kfld                    DCSstd
443000040726     c                   kfld                    DCSdim
443100040726     c                   kfld                    DCShim
443200040726     c                   kfld                    DCSnks
443300040726     c                   kfld                    DCStrc
443400040206      * Accesso TITA430C / TITAA30C / FIAR601L
443500040206     c     kBOLtrc       klist
443600040206     c                   kfld                    DCTaas
443700040206     c                   kfld                    DCTlnp
443800040206     c                   kfld                    DCTnrs
443900040206     c                   kfld                    DCTnsp
444000040206     c                   kfld                    Ktrc
444100040531      * Accesso FIAR531C
444200040206     c     K05AR530      klist
444300040206     c                   kfld                    DCTaas
444400040206     c                   kfld                    DCTlnp
444500040206     c                   kfld                    DCTnrs
444600040206     c                   kfld                    DCTnsp
444700040206     c                   kfld                    AR5trd
444800981210     c* ACCESSO TABEL
444900981210     C     ktab          KLIST                                                  *
445000040210     C                   KFLD                    CODUT                          *
445100990216     C                   KFLD                    kcod                           *
445200981210     C                   KFLD                    kkey                           *
445300010907     c*
445400990503     C     kDCR3         KLIST                                                  *
445500090323     C                   KFLD                    kteralfa                       *
445600090323     C                   KFLD                    kfil                           *
445700090323     C                   KFLD                    drrtre                         *
445800090323     C     kDCRcer       KLIST                                                  *
445900090323     C                   KFLD                    kteralfa                       *
446000090323     C                   KFLD                    kfil                           *
446100990218     C     kDCRT         KLIST                                                  *
446200990218     C                   KFLD                    dcraac                         *
446300990218     C                   KFLD                    dcrfil                         *
446400990218     C                   KFLD                    dcrnca                         *
446500990218     C     kdcrd         KLIST                                                  *
446600990218     C                   KFLD                    dctaac                         *
446700990218     C                   KFLD                    dctfil                         *
446800990218     C                   KFLD                    dctnca                         *
446900990218     C                   KFLD                    w0070                          *
447000990219     C     korg2         KLIST                                                  *
447100990219     C                   KFLD                    i43dst                         *
447200070111     C**                 KFLD                    kcar                           *
447300070111     C                   KFLD                    i43are                         *
447400990406     C     kRDN          KLIST                                                  *
447500090319     C                   KFLD                    ktie                           *
447600090319     C                   KFLD                    wdal                           *
447700090319     C                   KFLD                    wal                            *
447800990406     C                   KFLD                    p3cdst                         *
447900990406     C                   KFLD                    p3care                         *
448000090319     C                   KFLD                    kter                           *
448100090319     C                   KFLD                    kttf                           *
448200090319     C                   KFLD                    dcrpor                         *
448300990406     C                   KFLD                    ktae                           *
448400990406     C                   KFLD                    dcttad                         *
448500990917     C                   KFLD                    kcrc                           *
448600990917     C                   KFLD                    kcch                           *
448700990408     C                   KFLD                    kcrs                           *
448800990408     C                   KFLD                    kfpr                           *
448900110912     C     kRDNav        KLIST                                                  *
449000110912     C                   KFLD                    ktieav                         *
449100110912     C                   KFLD                    wdal                           *
449200110912     C                   KFLD                    wal                            *
449300110912     C                   KFLD                    p3cdst                         *
449400110912     C                   KFLD                    p3care                         *
449500130909     C                   KFLD                    kter_p                         *
449600110912     C                   KFLD                    kttf                           *
449700121126     C                   KFLD                    kfil_0                         *
449800110912     C                   KFLD                    ktae                           *
449900110912     C                   KFLD                    dcttad                         *
450000110912     C                   KFLD                    kcrc                           *
450100110912     C                   KFLD                    kcch                           *
450200110912     C                   KFLD                    kcrs                           *
450300110912     C                   KFLD                    kfpr                           *
450400990406     C     KDET          KLIST                                                  *
450500990406     C                   KFLD                    dctaae                         *
450600990406     C                   KFLD                    dctnev                         *
450700981209     c*
450800981204     C                   ENDSR
450900990219     c*
451000990408     OQsysprt   E            dett
451100990408     O                       dctaac
451200990408     O                       dctfil        z   +  1
451300990408     O                       dctnca        z   +  1
451400990408     O                       dctaas        z   +  1
451500990408     O                       dctlnp        z   +  1
451600990408     O                       dctnrs        z   +  1
451700990408     O                       dctnsp        z   +  1
451800990408     O                       i48ksc        z   +  1
451900990408     O                       i48ctr        z   +  1
452000990408     O                       i48por            +  1
452100990408     O                       i48fie            +  1
452200990408     O                       i48tsp            +  1
452300990408     O                       o48mand           +  1
452400990408     O                       o48FTC            +  1
452500990408     O                       kfpr              +  1
452600990324** TES - testata
452700990324ELENCO  COMUNICAZIONI  DI  ANOMALIA
452800990324   ELENCO  C.A.  DA  ARCHIVIARE
452900121219** OVRPRT
453000121219OVRPRTF FILE(FIDN44P) HOLD(*YES)
